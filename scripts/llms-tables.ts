// llms-tables.ts: put the missing tables back into the .llms.md copies of pages.
//
// With `llms-txt: true`, Quarto writes a Markdown copy of each page
// (page.llms.md) for AI tools such as the Berry Docs Slack bot. It converts the
// page with pandoc to GitHub-flavoured Markdown without raw HTML. That format
// can't hold a table whose cells contain more than one line of text, so pandoc
// drops the table and leaves a "[TABLE]" line instead. That hits most of our
// grid tables, including the settings and output-column reference tables.
//
// This script leaves Quarto's .llms.md as it is and only replaces each
// "[TABLE]" line with the table it stands for. It converts the page's tables
// with Quarto's own pandoc, adding scripts/llms-tables.lua, which flattens each
// cell to one line so the table fits. The tables pandoc drops without that
// filter are the ones behind the "[TABLE]" lines, in page order.
//
// It runs automatically after every render (`post-render` in _quarto.yml). To
// fix the existing docs/ folder without rendering, run:
//
//   quarto run scripts/llms-tables.ts
//
// It never fails the render. If anything doesn't line up for a page, that page
// keeps the copy Quarto wrote and a warning is printed.

const PROJECT_DIR = Deno.cwd().replace(/\\/g, "/");
const TABLE_FILTER = `${PROJECT_DIR}/scripts/llms-tables.lua`;
const MARKER = "[TABLE]";
const PARALLEL = 6;
const SEPARATOR = (i: number) => `LLMSTABLESEPARATOR${i}`;

function exists(path: string): boolean {
  try {
    Deno.statSync(path);
    return true;
  } catch {
    return false;
  }
}

const parent = (path: string) => path.replace(/\\/g, "/").replace(/\/[^/]*$/, "");
const isAbsolute = (path: string) => /^([a-zA-Z]:)?[\\/]/.test(path);

/** Quarto's own pandoc and llms filter, found from the Deno running this. */
function quartoTools(): { pandoc: string; llmsFilter: string } {
  const exe = Deno.build.os === "windows" ? "pandoc.exe" : "pandoc";
  const dirs: string[] = [];
  const binPath = Deno.env.get("QUARTO_BIN_PATH");
  if (binPath) dirs.push(`${binPath}/tools`, binPath);
  for (let dir = parent(Deno.execPath()), i = 0; i < 4; i++, dir = parent(dir)) {
    dirs.push(dir, `${dir}/tools`);
  }
  const pandoc = dirs.map((d) => `${d}/${exe}`).find(exists) ?? "pandoc";

  const sharePath = Deno.env.get("QUARTO_SHARE_PATH");
  const llmsFilter = [
    ...(sharePath ? [`${sharePath}/filters/llms/llms.lua`] : []),
    ...dirs.map((d) => `${d}/share/filters/llms/llms.lua`),
  ].find(exists);
  if (!llmsFilter) throw new Error("couldn't find Quarto's llms.lua filter");
  return { pandoc, llmsFilter };
}

/** The pages this render produced, or every page when run by hand. */
function htmlPages(): string[] {
  const listed = Deno.env.get("QUARTO_PROJECT_OUTPUT_FILES");
  if (listed) {
    return listed.split("\n").map((f) => f.trim())
      .filter((f) => f.endsWith(".html"))
      .map((f) => isAbsolute(f) ? f : `${PROJECT_DIR}/${f}`);
  }
  const outDir = Deno.env.get("QUARTO_PROJECT_OUTPUT_DIR") ?? "docs";
  const pages: string[] = [];
  const walk = (dir: string) => {
    for (const entry of Deno.readDirSync(dir)) {
      const path = `${dir}/${entry.name}`;
      if (entry.isDirectory && entry.name !== "site_libs") walk(path);
      else if (entry.isFile && entry.name.endsWith(".html")) pages.push(path);
    }
  };
  walk(isAbsolute(outDir) ? outDir : `${PROJECT_DIR}/${outDir}`);
  return pages;
}

/** The tables in the page's <main> element, in page order. */
function pageTables(html: string): string[] {
  const start = html.indexOf("<main");
  const end = html.lastIndexOf("</main>");
  if (start === -1 || end === -1) return [];
  return html.slice(start, end).match(/<table[\s>][\s\S]*?<\/table>/g) ?? [];
}

/**
 * Math as Quarto 1.11 passes it to pandoc: MathJax's \( \) and \[ \] spans
 * become TeX script tags, which pandoc reads as math.
 */
function prepareMath(html: string): string {
  return html.replace(
    /<span class="math (inline|display)">([\s\S]*?)<\/span>/g,
    (whole, kind: string, text: string) => {
      const match = kind === "display"
        ? text.match(/^\s*\\\[([\s\S]*)\\\]\s*$/)
        : text.match(/^\s*\\\(([\s\S]*)\\\)\s*$/);
      if (!match) return whole;
      const type = kind === "display" ? "math/tex; mode=display" : "math/tex";
      return `<script type="${type}">${match[1].trim()}</script>`;
    },
  );
}

/** Converts the tables to Markdown, one string per table. */
async function convertTables(
  tables: string[],
  flatten: boolean,
  tools: { pandoc: string; llmsFilter: string },
  tempDir: string,
  name: string,
): Promise<string[] | null> {
  const input = `${tempDir}/${name}.html`;
  const body = tables.map((t, i) => `<p>${SEPARATOR(i)}</p>\n${prepareMath(t)}`)
    .join("\n");
  await Deno.writeTextFile(
    input,
    `<!DOCTYPE html>\n<html>\n<head><meta charset="utf-8"></head>\n<body>\n${body}\n</body>\n</html>`,
  );
  const run = (format: string) =>
    new Deno.Command(tools.pandoc, {
      args: [
        input,
        "-f", "html",
        "-t", format,
        ...(flatten ? ["--lua-filter", TABLE_FILTER] : []),
        "--lua-filter", tools.llmsFilter,
        "--wrap=none",
      ],
      stdout: "piped",
      stderr: "piped",
    }).output();
  // Quarto 1.11 writes gfm-raw_html-tex_math_gfm; older pandoc lacks that
  // extension, as do the Quarto versions that wrote plain gfm-raw_html
  let result = await run("gfm-raw_html-tex_math_gfm");
  if (!result.success) result = await run("gfm-raw_html");
  if (!result.success) {
    console.warn(
      `llms-tables: pandoc failed for ${name}: ${new TextDecoder().decode(result.stderr)}`,
    );
    return null;
  }
  const markdown = new TextDecoder().decode(result.stdout);
  const parts = markdown.split(/^LLMSTABLESEPARATOR\d+\s*$/m).slice(1);
  return parts.length === tables.length ? parts.map((p) => p.trim()) : null;
}

/** The Markdown table rows in a converted table, without its caption. */
function markdownTable(markdown: string): string | null {
  const lines = markdown.split("\n");
  const first = lines.findIndex((line) => line.startsWith("|"));
  if (first === -1) return null;
  let last = first;
  while (last + 1 < lines.length && lines[last + 1].startsWith("|")) last++;
  return lines.slice(first, last + 1).join("\n");
}

/** Fills in the [TABLE] lines of one page's .llms.md. Returns how many. */
async function fixPage(
  page: string,
  tools: { pandoc: string; llmsFilter: string },
  tempDir: string,
  n: number,
): Promise<number> {
  const llmsPath = page.replace(/\.html$/, ".llms.md");
  if (!exists(llmsPath)) return 0; // drafts and pages Quarto skipped
  const llms = await Deno.readTextFile(llmsPath);
  const lines = llms.split("\n");
  const markerLines = lines.flatMap((line, i) => line.trim() === MARKER ? [i] : []);
  if (markerLines.length === 0) return 0;

  const tables = pageTables(await Deno.readTextFile(page));
  const [asQuarto, flattened] = await Promise.all([
    convertTables(tables, false, tools, tempDir, `page${n}-quarto`),
    convertTables(tables, true, tools, tempDir, `page${n}-flat`),
  ]);
  // A dropped table comes out as "[TABLE]", followed by its caption if it has
  // one (Quarto's .llms.md keeps that caption line, so only the table goes in)
  const dropped = asQuarto?.flatMap((md, i) =>
    md.split("\n")[0].trim() === MARKER ? [i] : []
  ) ?? [];
  if (!asQuarto || !flattened || dropped.length !== markerLines.length) {
    console.warn(
      `llms-tables: left ${llmsPath} as Quarto wrote it (${markerLines.length} [TABLE] lines, ${dropped.length} dropped tables found)`,
    );
    return 0;
  }
  const tableRows = dropped.map((i) => markdownTable(flattened[i]));
  if (tableRows.some((rows) => !rows)) {
    console.warn(`llms-tables: left ${llmsPath} as Quarto wrote it (a table didn't convert)`);
    return 0;
  }
  markerLines.forEach((line, k) => {
    lines[line] = tableRows[k]!;
  });
  await Deno.writeTextFile(llmsPath, lines.join("\n"));
  return markerLines.length;
}

async function main() {
  const tools = quartoTools();
  const pages = htmlPages();
  const tempDir = await Deno.makeTempDir({ prefix: "llms-tables-" });
  let fixedPages = 0;
  let fixedTables = 0;
  try {
    for (let i = 0; i < pages.length; i += PARALLEL) {
      const counts = await Promise.all(
        pages.slice(i, i + PARALLEL).map((page, j) =>
          fixPage(page, tools, tempDir, i + j).catch((error) => {
            console.warn(`llms-tables: skipped ${page}: ${error.message}`);
            return 0;
          })
        ),
      );
      for (const count of counts) {
        if (count > 0) {
          fixedPages++;
          fixedTables += count;
        }
      }
    }
  } finally {
    await Deno.remove(tempDir, { recursive: true }).catch(() => {});
  }
  console.log(
    `llms-tables: restored ${fixedTables} tables in ${fixedPages} .llms.md files (${pages.length} pages checked)`,
  );
}

try {
  await main();
} catch (error) {
  console.warn(`llms-tables: skipped (${(error as Error).message})`);
}
