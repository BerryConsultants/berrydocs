-- llms-tables.lua
--
-- Keeps tables in the .llms.md copies of pages. Quarto writes those copies as
-- GitHub-flavoured Markdown without raw HTML ("gfm-raw_html"). A GFM table can
-- only hold one line of text per cell, so pandoc drops any table with a richer
-- cell (several paragraphs, a list, a line break): most of our grid tables,
-- including the settings and output-column reference tables.
--
-- This filter flattens every cell to a single line, keeping inline formatting
-- (code, emphasis, links), so every table fits. Runs before Quarto's own
-- llms.lua filter; see scripts/llms-tables.ts.

local function block_inlines(block)
  if block.t == "Plain" or block.t == "Para" then
    return block.content
  elseif block.t == "LineBlock" then
    local out = pandoc.Inlines({})
    for i, line in ipairs(block.content) do
      if i > 1 then out:insert(pandoc.Str(" ")) end
      out:extend(line)
    end
    return out
  elseif block.t == "BulletList" or block.t == "OrderedList" then
    local out = pandoc.Inlines({})
    for i, item in ipairs(block.content) do
      if i > 1 then out:insert(pandoc.Space()) end
      out:insert(pandoc.Str(block.t == "BulletList" and "•" or (tostring(i) .. ".")))
      out:insert(pandoc.Space())
      for j, inner in ipairs(item) do
        if j > 1 then out:insert(pandoc.Space()) end
        out:extend(block_inlines(inner))
      end
    end
    return out
  elseif block.t == "CodeBlock" then
    return pandoc.Inlines({ pandoc.Code(block.text:gsub("\n", " ")) })
  elseif block.t == "Div" or block.t == "BlockQuote" then
    local out = pandoc.Inlines({})
    for i, inner in ipairs(block.content) do
      if i > 1 then out:insert(pandoc.Space()) end
      out:extend(block_inlines(inner))
    end
    return out
  end
  return pandoc.Inlines({ pandoc.Str(pandoc.utils.stringify(block)) })
end

local function flatten_cell(cell)
  local inlines = pandoc.Inlines({})
  for i, block in ipairs(cell.contents) do
    if i > 1 then
      inlines:insert(pandoc.Space())
      inlines:insert(pandoc.Str("·"))
      inlines:insert(pandoc.Space())
    end
    inlines:extend(block_inlines(block))
  end
  -- A GFM table cell is a single line
  inlines = inlines:walk({
    LineBreak = function() return pandoc.Space() end,
    SoftBreak = function() return pandoc.Space() end,
  })
  cell.contents = #inlines > 0 and pandoc.Blocks({ pandoc.Plain(inlines) })
    or pandoc.Blocks({})
  cell.row_span = 1
  cell.col_span = 1
  return cell
end

local function flatten_rows(rows)
  for _, row in ipairs(rows) do
    for i, cell in ipairs(row.cells) do
      row.cells[i] = flatten_cell(cell)
    end
  end
end

function Table(tbl)
  flatten_rows(tbl.head.rows)
  for _, body in ipairs(tbl.bodies) do
    flatten_rows(body.head)
    flatten_rows(body.body)
  end
  flatten_rows(tbl.foot.rows)
  return tbl
end
