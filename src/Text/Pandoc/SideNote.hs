{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.SideNote (usingSideNotes) where

import           Data.List           (intercalate)
import           Data.Text           (append, pack)

import           Control.Monad.State

import           Text.Pandoc.JSON
import           Text.Pandoc.Walk    (walk, walkM)

data NoteType
  = SideNote
  | MarginNote
  | FootNote
  deriving (Show, Eq)

getFirstStr :: [Block] -> (NoteType, [Block])
getFirstStr blocks@(block:blocks') =
  case block of
    Plain ((Str "{-}"):Space:rest) -> (MarginNote, (Plain rest):blocks')
    Plain ((Str "{.}"):Space:rest) -> (FootNote, (Plain rest):blocks')
    Para ((Str "{-}"):Space:rest) -> (MarginNote, (Para rest):blocks')
    Para ((Str "{.}"):Space:rest) -> (FootNote, (Para rest):blocks')
    LineBlock (((Str "{-}"):Space:rest):rest') -> (MarginNote, (LineBlock (rest:rest')):blocks')
    LineBlock (((Str "{.}"):Space:rest):rest') -> (FootNote, (LineBlock (rest:rest')):blocks')
    _ -> (SideNote, blocks)
getFirstStr blocks = (SideNote, blocks)

newline :: [Inline]
newline = [LineBreak, LineBreak]

-- This could be implemented more concisely, but I think this is more clear.
getThenIncr :: State Int Int
getThenIncr = do
  i <- get
  put (i + 1)
  return i

-- Extract inlines from blocks. Note has a [Block], but Span needs [Inline].
coerceToInline :: [Block] -> [Inline]
coerceToInline = concatMap deBlock . walk deNote
 where
  deBlock :: Block -> [Inline]
  deBlock (Plain     ls    ) = ls
  -- Simulate paragraphs with double LineBreak
  deBlock (Para      ls    ) = ls ++ newline
  -- See extension: line_blocks
  deBlock (LineBlock lss   ) = intercalate [LineBreak] lss ++ newline
  -- Pretend RawBlock is RawInline (might not work!)
  -- Consider: raw <div> now inside RawInline... what happens?
  deBlock (RawBlock fmt str) = [RawInline fmt str]
  -- lists, blockquotes, headers, hrs, and tables are all omitted
  -- Think they shouldn't be? I'm open to sensible PR's.
  deBlock _                  = []

  deNote (Note _) = Str ""
  deNote x        = x

filterNote :: Bool -> [Inline] -> State Int Inline
filterNote nonu content = do
  -- Generate a unique number for the 'for=' attribute
  i <- getThenIncr

  let labelCls = "margin-toggle" `append`
                 (if nonu then "" else " sidenote-number")
  let labelSym = if nonu then "&#8853;" else ""
  let labelHTML = mconcat
         [ "<label for=\"sn-"
         , pack (show i)
         , "\" class=\""
         , labelCls
         , "\">"
         , labelSym
         , "</label>"
         ]
  let label = RawInline (Format "html") labelHTML

  let inputHTML = mconcat
        [ "<input type=\"checkbox\" id=\"sn-"
        , pack (show i)
        , "\" "
        , "class=\"margin-toggle\"/>"
        ]
  let input             = RawInline (Format "html") inputHTML

  let (ident, _, attrs) = nullAttr
  let noteTypeCls       = if nonu then "marginnote" else "sidenote"
  let note              = Span (ident, [noteTypeCls], attrs) content

  return $ Span ("", ["sidenote-wrapper"], []) [label, input, note]

filterInline :: Inline -> State Int Inline
filterInline (Note blocks) = do
  -- The '{-}' symbol differentiates between margin note and side note
  -- Also '{.}' indicates whether to leave the footnote untouched (a footnote)
  case (getFirstStr blocks) of
    (FootNote, blocks') -> return (Note blocks')
    (MarginNote, blocks') -> filterNote True (coerceToInline blocks')
    (SideNote, blocks') -> filterNote False (coerceToInline blocks')

filterInline inline = return inline

usingSideNotes :: Pandoc -> Pandoc
usingSideNotes (Pandoc meta blocks) =
  Pandoc meta (evalState (walkM filterInline blocks) 0)
