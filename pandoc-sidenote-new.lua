-- TODO(jez) Copyright and license

-- TODO(jez) Handle footnotes inside Note itself.
-- Actually, it looks like pandoc markdown doesn't parse recursive footnotes.
-- The AST doesn't seem to preclude building them, so maybe other readers or
-- other filters could produce such ASTs.
-- Going to punt for now though, because the typewise (instead of topdown)
-- traversal seems to be working, but would not work if we wanted to support
-- nested footnotes.

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

    -- '{^-}' indicates that it should be a margin note, but using the
    -- hoisted block markup, instead of remaining inline.
    if inlines[1].text == "{^-}" then
      inlines:remove(1)
      inlines:remove(1)
      return "marginnote-block"
    end

    -- '{.}' indicates whether to leave the footnote untouched (a footnote)
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

-- TODO(jez) Will need to make either RawInline or RawBlock
local function makeInput(snIdx)
  local inputFormatStr = '<input type="checkbox" id="sn-%d" class="margin-toggle"/>'
  local inputHTML = inputFormatStr:format(snIdx)
  return pandoc.RawBlock("html", inputHTML)
end

local snIdx = -1

-- TODO(jez) Can you make this use whatever OOP features Lua has?
local function makeBlockWalker()
  local notes = {}
  return {
    notes = notes,
    filter = {
      Note = function(note)
        local noteKind = mungeBlocks(note.content)
        if noteKind == "footnote" then
          return note
        end

        -- Generate a unique number for the `for=` attribute
        snIdx = snIdx + 1

        -- TODO(jez) sidenote and marginnote
        if noteKind ~= "marginnote-block" then
          return note
        end

        notes[#notes + 1] = {
          snIdx = snIdx,
          kind = noteKind,
          content = note.content,
        }
        return makeLabel(snIdx, noteKind)
      end,
    },
  }
end

function Blocks(blocks)
  local result = {}

  for i = 1, #blocks do
    local block = blocks[i]
    local blockWalker = makeBlockWalker()
    local newBlock = block:walk(blockWalker.filter)
    for j = 1, #blockWalker.notes do
      local note = blockWalker.notes[j]
      local contentClass = note.kind
      if contentClass == "marginnote-block" then
        -- Use .marginnote class for backwards compatibility.
        -- If people really want to care about the distinction,
        -- they can write `div.marginnote` or `span.marginnote`
        contentClass = "marginnote"
      end
      result[#result + 1] = pandoc.Div(
        pandoc.Blocks({
          makeInput(note.snIdx),
          pandoc.Div(note.content, { class = contentClass }),
        }),
        { class = "sidenote-wrapper" }
      )
    end
    result[#result + 1] = newBlock
  end

  return pandoc.Blocks(result)
end
