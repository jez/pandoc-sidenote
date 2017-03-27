{-# LANGUAGE FlexibleContexts #-}

module Text.Pandoc.SideNote (usingSideNotes) where

import Data.List (intercalate)

import Control.Monad.Gen

import Text.Pandoc.Walk (walk, walkM)
import Text.Pandoc.JSON

getFirstStr :: [Inline] -> Maybe String
getFirstStr [] = Nothing
getFirstStr (Str text : _) = Just text
getFirstStr (_ : inlines) = getFirstStr inlines

newline :: [Inline]
newline = [LineBreak, LineBreak]

-- Extract inlines from blocks
coerceToInline :: [Block] -> [Inline]
coerceToInline = concatMap deBlock . walk deNote
  where deBlock :: Block -> [Inline]
        deBlock (Plain ls) = ls
        -- Simulate paragraphs with double LineBreak
        deBlock (Para ls) = ls ++ newline
        -- See extension: line_blocks
        deBlock (LineBlock lss) = intercalate [LineBreak] lss ++ newline
        -- Pretend RawBlock is RawInline (might not work!)
        -- Consider: raw <div> now inside RawInline... what happens?
        deBlock (RawBlock fmt str) = [RawInline fmt str]
        -- lists, blockquotes, headers, hrs, and tables are all omitted
        -- Think they shouldn't be? I'm open to sensible PR's.
        deBlock _ = []

        deNote (Note _) = Str ""
        deNote x        = x

filterInline :: Inline -> Gen Int Inline
filterInline (Note blocks) = do
  -- Generate a unique number for the 'for=' attribute
  i <- gen

  -- Note has a [Block], but Span needs [Inline]
  let content = coerceToInline blocks

  -- The '{-}' symbol differentiates between margin note and side note
  let nonu = getFirstStr content == Just "{-}"
  let content' = if nonu then tail content else content

  let labelCls = "margin-toggle" ++ (if nonu then "" else " sidenote-number")
  let labelSym = if nonu then "&#8853;" else ""
  let labelHTML = "<label for=\"sn-" ++ show i ++ "\" class=\"" ++ labelCls ++ "\">" ++ labelSym ++ "</label>"
  let label = RawInline (Format "html") labelHTML

  let inputHTML = "<input type=\"checkbox\" id=\"sn-" ++ show i ++ "\" " ++ "class=\"margin-toggle\"/>"
  let input = RawInline (Format "html") inputHTML

  let (ident, _, attrs) = nullAttr
  let noteTypeCls = if nonu then "marginnote" else "sidenote"
  let note = Span (ident, [noteTypeCls], attrs) content'

  return $ Span nullAttr [label, input, note]

filterInline inline = return inline

usingSideNotes :: Pandoc -> Pandoc
usingSideNotes (Pandoc meta blocks) = Pandoc meta (runGen (walkM filterInline blocks))
