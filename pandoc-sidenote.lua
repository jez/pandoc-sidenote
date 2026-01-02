--
--                            pandoc-sidenote.lua
--
-- Author:    Jacob Zimmerman (@jez)
-- Version:   0.24.0
-- Modified:  2025-11-02
-- URL:       https://github.com/jez/pandoc-sidenote
--

--------------------------------------------------------------------------------
-- The MIT License (MIT)
--
-- Copyright (c) 2025 Jacob Zimmerman
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
-- sell copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--------------------------------------------------------------------------------

local function startsWithStrSpace(inlines)
  if not inlines[1] or inlines[1].tag ~= "Str" then
    return false
  end
  if not inlines[2] or inlines[2].tag ~= "Space" then
    return false
  end

  return true
end

local function stripNoteAttribute(inlines)
  if startsWithStrSpace(inlines) then
    -- The '{-}' symbol differentiates between margin note and side note
    if inlines[1].text == "{-}" then
      inlines:remove(1)
      inlines:remove(1)
      return "marginnote"
    end

    -- Also '{.}' indicates whether to leave the footnote untouched (a footnote)
    if inlines[1].text == "{.}" then
      inlines:remove(1)
      inlines:remove(1)
      return "footnote"
    end
  end

  return "sidenote"
end

local function mungeBlocks(blocks)
  if #blocks == 0 then
    return "sidenote"
  end

  local block = blocks[1]

  if block.tag == "Plain" or block.tag == "Para" then
    return stripNoteAttribute(block.content)
  elseif block.tag == "LineBlock" then
    local firstInlines = block.content[1]
    if firstInlines then
      return stripNoteAttribute(firstInlines)
    end

    return "sidenote"
  else
    return "sidenote"
  end
end

local function append(xs, ys)
  for i = 1, #ys do
    xs[#xs + 1] = ys[i]
  end
end

-- TODO(jez) Can you rewrite this with a walk?
local function accumulateInlines(inlines, block)
  if block.tag == "Plain" then
    append(inlines, block.content)
  elseif block.tag == "Para" then
    -- Simulate paragraphs with double LineBreak
    append(inlines, block.content)
    inlines[#inlines + 1] = pandoc.LineBreak()
    inlines[#inlines + 1] = pandoc.LineBreak()
  elseif block.tag == "LineBlock" then
    -- See extension: line_blocks
    for i = 1, #block.content do
      append(inlines, block.content[i])
    end
  elseif block.tag == "RawBlock" then
    -- Pretend RawBlock is RawInline (might not work!)
    -- Consider: raw <div> now inside RawInline... what happens?
    inlines[#inlines + 1] = pandoc.RawInline(block.format, block.text)
  end

  -- lists, blockquotes, headers, hrs, and tables are all omitted.
  -- Think they shouldn't be? I'm open to sensible PR's.
end

local function walkBlocks(blocks, filter)
  return pandoc.Div(blocks):walk(filter).content
end

-- Extract inlines from blocks. Note has Blocks, but Span needs Inlines
local function coerceToInline(blocks)
  blocks = walkBlocks(blocks, {
    Note = function(note)
      return pandoc.Str("")
    end,
  })

  local inlines = {}

  for i = 1, #blocks do
    accumulateInlines(inlines, blocks[i])
  end

  return inlines
end

local function makeLabel(snIdx, noteKind)
  local labelCls = "margin-toggle"
  if noteKind == "sidenote" then
    labelCls = labelCls .. " sidenote-number"
  end

  local labelSym
  if noteKind == "marginnote" then
    labelSym = "&#8853;"
  else
    labelSym = ""
  end

  local labelFormatStr = '<label for="sn-%d" class="%s">%s</label>'
  local labelHTML = labelFormatStr:format(snIdx, labelCls, labelSym)
  return pandoc.RawInline("html", labelHTML)
end

local function makeInput(snIdx)
  local inputFormatStr = '<input type="checkbox" id="sn-%d" class="margin-toggle"/>'
  local inputHTML = inputFormatStr:format(snIdx)
  return pandoc.RawInline("html", inputHTML)
end

local function makeNoteMarkup(snIdx, noteKind, content)
  local label = makeLabel(snIdx, noteKind)
  local input = makeInput(snIdx)

  local note = pandoc.Span(content, { class = noteKind })
  return pandoc.Span({ label, input, note }, { class = "sidenote-wrapper" })
end

local snIdx = -1
function Note(note)
  local noteKind = mungeBlocks(note.content)
  if noteKind == "footnote" then
    return note
  end

  -- Generate a unique number for the `for=` attribute
  snIdx = snIdx + 1

  local inlines = coerceToInline(note.content)
  return makeNoteMarkup(snIdx, noteKind, inlines)
end
