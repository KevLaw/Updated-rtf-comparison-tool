# RTF Comparison Tool

Compare two RTF files and report exactly how their **content** differs — ignoring
cosmetic formatting (fonts, colours, column widths, spacing). Built in **R** for
clinical QC of TLF (Tables, Listings, Figures) outputs, but general enough for any
tabular RTF.

> **Why not just `diff` the files?** RTF is markup. Two RTF files that render the
> *identical* table can have completely different bytes (font tables, control-word
> order, escape encodings). A raw text `diff` produces thousands of meaningless
> differences. This tool extracts the **displayed cell content**, normalises it, and
> compares **cell by cell**, e.g. *"Row 10, Col 2: file 1 = `45 (21.4%)`, file 2 =
> `46 (21.9%)`"*.

---

## Contents

- [What you need](#what-you-need)
- [Quick start — the easy way (no typing)](#quick-start--the-easy-way-no-typing)
- [Compare whole folders at once (batch)](#compare-whole-folders-at-once-batch)
- [Audit log — proof of every run](#audit-log--proof-of-every-run)
- [Keep the tool folder intact](#keep-the-tool-folder-intact)
- [What the report tells you](#what-the-report-tells-you)
- [Command-line use (optional)](#command-line-use-optional)
- [Options and defaults](#options-and-defaults)
- [Generate your own test data](#generate-your-own-test-data)
- [Run the test suite](#run-the-test-suite)
- [How it works](#how-it-works)
- [What has been validated](#what-has-been-validated)
- [Known limitations](#known-limitations)
- [Repository layout](#repository-layout)

---

## What you need

- **R** (version 4.x), already installed. Download: <https://cran.r-project.org>.
  No programming knowledge is required to *use* the tool.
- A one-time install of five R packages (handled for you in Step 1 below):
  `striprtf`, `diffdf`, `data.table`, `testthat`, `optparse`.
- The package install needs an internet connection the first time only.

---

## Quick start

Use the launcher folder for your computer — `windows\` on Windows, `macos/` on a Mac.
Double-click to set up once; then, to compare, you paste two file paths (or use the
file-picker alternative, `2b-…`).

### Step 1 — install the packages (once)

| Windows | Mac |
|---|---|
| Double-click **`windows\1-Install-Packages.bat`** | Double-click **`macos/1-Install-Packages.command`** |

A window opens, installs the five packages, prints **`SUCCESS`**, and waits for a key
press. You only ever do this once per computer.

### Step 2 — compare two files (every time)

| Windows | Mac |
|---|---|
| Double-click **`windows\2-Compare-RTF-Files.bat`** | Double-click **`macos/2-Compare-RTF-Files.command`** |

It prompts for two file paths — **paste or type each path and press Enter** (in Windows
Explorer, Shift + right-click a file → *Copy as path*). Windows and macOS label the selections as:

```
File 1 (Set 1) : <paste the first path>
File 2 (Set 2) : <paste the second path>
```

Backslash paths and surrounding quotes are both handled. The result appears immediately —
**EQUIVALENT** or **N difference(s) found** (each difference listed). **Nothing is saved
automatically**; it then asks whether to export the report as **TXT**, **CSV**, or **both**,
and prompts for where to save it (type a full path or a folder). Finally it offers to
compare another pair. Works with any two RTF files on the machine.

On Windows and macOS, after the comparison and normal report choices are complete, the tool also asks:

> Would you like a separate set of RTF tables generated showing only the differences or no
> differences identified line by line?

Choose **Yes** to create a true change table in each source document's style. The first file
selected is **Set 1**, the second is **Set 2**, and every calculation is **Set 2 minus Set 1**.
The two generated RTFs contain the same aligned rows and results, while retaining the page
setup, headings, column layout, spacing, and footnote styling of their respective source RTFs
as closely as possible.

The table number gains `_Change`. Each original column header is preserved and gains either
`Change` or `NC` on a new line: the label is `NC` only when every body cell in that column is
`NC`; otherwise it is `Change`. Generated data cells contain only:

- `NC` when the supported source values are equal;
- a signed count and displayed-percentage-point change, such as `(+5, -1%)`;
- a signed scalar numeric change, such as `+1.5`;
- `CHG` for a changed value that cannot be reduced to one scalar number; or
- `ONLY IN SET 1` / `ONLY IN SET 2` for a row found in only one set.

Numeric results use up to two significant figures. For count-and-percentage cells, the tool
subtracts the percentages printed in the source cells; it does not calculate a relative rate.
For example, `45 (5%)` in Set 1 and `50 (4%)` in Set 2 becomes `(+5, -1%)`. If the counts are
equal, the result is `NC` even when the displayed percentages differ.

Rows are aligned by their displayed Column 1 description rather than absolute row number.
Repeated descriptions are paired in occurrence order within their section. A row found in
only one set is inserted into the same union of rows in both outputs, and its Column 1 label
gains `(ONLY IN SET 1)` or `(ONLY IN SET 2)`. The files keep the source name with `_change`
inserted before `.rtf` and are saved under:

```
logs/RTF Changes/Set 1/
logs/RTF Changes/Set 2/
```

The separate Set folders prevent same-named table pairs from overwriting one another.
For a folder run, the tool prints progress for every officially accepted pair and finishes
with an expected-versus-completed pair count. If even one accepted pair cannot produce both
validated RTFs, the run is marked incomplete and exits with error code `2`; partial output is
never reported as a successful completed set.

Hidden RTF destinations such as bookmarks and document metadata are retained in the source
formatting but excluded from displayed-cell matching. They therefore cannot be mistaken for
visible table text or prevent otherwise supported change tables from being generated.

Footnotes use Set 2 wording in both generated RTFs. Cosmetic differences in whitespace,
tabs, or spacing around punctuation and hyphens are ignored. For a material difference, only
the changed Set 2 character span is enclosed in parentheses; a Set 1 deletion is shown as
`(missing)`. The exact heading `Footnote changes in brackets` appears above the footnotes only
when there is a material footnote change.

Change-table generation is available from both the **Windows and macOS workflows**.

> **Prefer clicking files in a dialog?** Use `2b-Compare-Using-File-Picker.bat` /
> `2b-Compare-Using-File-Picker.command` instead — same result, file-picker dialogs rather
> than pasted paths.

---

## Compare whole folders at once (batch)

To QC a whole set of tables in one go, compare two **folders** instead of two files:

| Windows | Mac |
|---|---|
| Double-click **`windows\2c-Compare-Folders.bat`** | Double-click **`macos/2c-Compare-Folders.command`** |

Two folder pickers open — choose the **Set 1** folder, then the **Set 2** folder.
The tool compares **every `.rtf` file** in the first folder against its corresponding file in
the second folder. Exact normalized filenames are paired first; a remaining pair may also be
matched when the names differ slightly and the match is uniquely identifiable. It produces
**one report that lists every file** — including the ones that are **EQUIVALENT** — so the
report is a complete record of the set:

```
PER-FILE RESULTS  (every file is listed, including matches):
FILE            RESULT
table_14-1.rtf  EQUIVALENT
table_14-2.rtf  7 difference(s)
table_14-3.rtf  ONLY IN FOLDER 1 (no match)
```

Files that differ have their cell-level differences listed underneath; files present in only
one folder (or that can't be read) are flagged rather than silently skipped. The full report
is **archived automatically** inside the tool's `logs/reports/` folder (a `.txt` and a `.csv`),
and you're also offered a **Save dialog** to keep your own copy wherever you like.

On Windows and macOS, the same final RTF-change-table prompt is offered after a folder run. If selected,
the tool generates Set 1 and Set 2 change tables for every successfully paired file, including
equivalent pairs. The same calculation, semantic row-alignment, header, and footnote rules
described above apply. Unmatched or unreadable files remain identified in the batch report but
do not produce a change RTF.

**Three-pass pairing rule:** exact normalized filenames always take priority. Among remaining
files, the second pass recognizes either (a) long names within 3 single-character edits and at
least 85% similarity or (b) filename families separated by `0`. For the family rule, `s` and
`ae` are treated as generic prefixes; at least two other tokens must occur in the same order and
cover at least two-thirds of the shorter token list. Thus `s0exp0sum.rtf` becomes a candidate for
`s0exp0sum0bystudy.rtf`. Filename candidates in the same relative subfolder must also pass the
rendered-content gate: the displayed title must have at least 35% similarity or the Column 1
descriptions must have at least 20% Dice overlap. Candidate ranking combines the filename,
title, and Column 1 evidence, and the winning pair must be unique in both directions by at least
5 percentage points.

The third pass takes every still-unmatched RTF in Set 1 and checks its rendered content against
every still-unmatched RTF in Set 2, even when the filenames and relative subfolders have no
apparent relationship. When both title and Column 1 evidence are available, each must be at
least 20% and their average must be at least 65%; if only one is available, it must be at least
85%. A content-only pair must be each file's unique best match by at least 10 percentage points.
The report identifies content-only matches and explicitly states when all eligible unmatched
files were checked without finding a safe match. Exact filename pairs are still compared
regardless of content because finding substantial content changes is the tool's core purpose.
For example, `s0ae0by0outcompe0sei.rtf` safely pairs with
`s0ae0by0outcompe0aeosi.rtf`. Ambiguous, weaker, or content-rejected candidates stay unmatched,
and the report records why no safe match was found. A change RTF is generated only for a
successfully compared exact, filename-family, or content-only pair; an unmatched file never
produces a change RTF.

---

## Audit log — proof of every run

**Every** comparison you run — single-file *or* batch, **including runs that find no
differences** — is appended to a timestamped audit log:

```
logs/audit_log.csv
```

One row per run records *when* it ran, *who* ran it, the files or folders compared, the result,
and the counts (files compared / equivalent / differing / errors). Open it in **Excel** or
**Numbers** to review, sort, or print it as evidence of the QC work performed and when.

| Windows | Mac |
|---|---|
| Double-click **`windows\5-View-Audit-Log.bat`** | Double-click **`macos/5-View-Audit-Log.command`** |

The audit log and the archived reports live **inside the tool folder** (`logs/`). See
`logs/README.txt` for the column definitions.

---

## Keep the tool folder intact

> ⚠️ **Do not move the R scripts or launchers out of the tool folder.** The tool writes its
> **audit log** and **archived reports** to the `logs/` folder that sits next to the `R/`
> folder. If you split the folder up, the tool can no longer find `logs/` and cannot save your
> audit trail. Keep the whole folder together (you can put the *whole* folder anywhere — Desktop,
> a network share, a USB drive — just don't take pieces out of it).

---

> **No admin rights / "Run anyway" is blocked (Windows):** that prompt is triggered because
> downloaded files are "blocked". Clear it yourself without admin: right-click the **ZIP** →
> **Properties** → tick **Unblock** → **OK**, *then* extract. If your company blocks `.bat`
> files entirely, drive the tool from the R console instead (no admin, always works):
> `source("C:/path/to/Updated-rtf-comparison-tool/R/run_compare_paths.R")` then paste the two paths.
> See `START HERE (Windows).txt` for the full no-admin walkthrough.

> **Mac, first run only:** macOS may say the file is "from an unidentified developer."
> Right-click the `.command` → **Open** → **Open**. If a launcher isn't clickable, run
> `chmod +x *.command` once in the `macos` folder.

> **Tip:** keep the whole folder together — the launchers find the R scripts via their own
> location, so the tool works from anywhere (Desktop, network share, USB drive).

---

## What the report tells you

**Console / text report (always produced):**

```
============================================================
RTF COMPARISON REPORT
============================================================
File 1:  examples/clinical_table_base.rtf
File 2:  examples/clinical_table_changed.rtf
Options: num_tol=0, rel_tol=FALSE, trim=TRUE, collapse_space=TRUE, casefold=FALSE
Cells compared: 3,202
Result:  7 DIFFERENCE(S) FOUND
------------------------------------------------------------
ROW   COL   STATUS               FILE 1 VALUE          FILE 2 VALUE
3     1     VALUE_DIFF           Safety Population     Safety Population (Final)
10    2     VALUE_DIFF           45 (21.4%)            46 (21.9%)
10    4     VALUE_DIFF           52 (24.5%)            53 (25.0%)
11    6     VALUE_DIFF           (-1.2 to 3.4)         (-1.0 to 3.6)
15    5     VALUE_DIFF           31 (4.9%)             32 (5.1%)
20    1     VALUE_DIFF           Fatigue               Fatigues
542   1     VALUE_DIFF           ...Pharmaceuticals... ...Pharma...
```

**Each difference is classified:**

| STATUS | Meaning |
|---|---|
| `VALUE_DIFF` | The cell exists in both files but the content differs. |
| `CELL_ONLY_IN_FILE1` | The cell exists only in file 1 (e.g. file 1 has an extra row or column). |
| `CELL_ONLY_IN_FILE2` | The cell exists only in file 2. |

`ROW`/`COL` are 1-based positions counted top-to-bottom (titles, subtitles, table rows
and footnotes are all numbered in the order they appear; non-table lines are a single
"column 1").

**CSV (`.csv`):** the same differences as data — columns
`row_index, col_index, status, value_file1, value_file2` — ready for Excel or a pipeline.

---

## Command-line use (optional)

For power users, pipelines, or batch QC. From the project folder:

```bash
Rscript R/compare_rtf.R --file1 path/to/base.rtf --file2 path/to/comparison.rtf
```

The comparison result always prints to the screen; **nothing is written to disk unless you
ask**. Export on demand by naming where each file should go — `--report` for the text report,
`--csv` for the differences:

```bash
Rscript R/compare_rtf.R \
  --file1 base.rtf --file2 comparison.rtf \
  --report result.txt --csv diffs.csv --diffdf
```

**Exit codes** (useful for automation):

| Code | Meaning |
|---|---|
| `0` | Files are **equivalent** (no content differences). |
| `1` | One or more **differences** found. |
| `2` | **Error** (file not found, not a valid RTF, bad arguments). |

Run `Rscript R/compare_rtf.R --help` for the full option list.

---

## Options and defaults

| Option (CLI) | Default | Effect |
|---|---|---|
| `--num-tol <n>` | `0` (exact) | Numeric tolerance. If `> 0`, two cells that both parse as numbers (ignoring `,` and a trailing `%`) and differ by `≤ n` are treated as a match. |
| `--rel-tol` | off | Interpret `--num-tol` as a *relative* tolerance (fraction of the larger value). |
| `--no-collapse-space` | off | Keep internal runs of whitespace instead of collapsing them to one space. |
| `--casefold` | off | Case-insensitive comparison. |
| `--report <path>` | — | Also write the text report to a file. |
| `--csv <path>` | — | Write the differences as CSV. |
| `--diffdf` | off | Also run a second, independent comparison with the `diffdf` package and print whether it agrees. |

Defaults match the confirmed clinical requirements: **exact** comparison, whitespace
trimmed and collapsed, case preserved, and **titles/subtitles/footnotes are compared**
(not just the table body). Special characters (©, ≥, °, µ, α, β, accents, en/minus dashes)
are decoded and compared exactly.

---

## Generate your own test data

You can create realistic synthetic clinical RTF files to experiment with — no Python,
just R.

**Easy way:** double-click **`windows\4-Generate-Test-Data.bat`** /
**`macos/4-Generate-Test-Data.command`**, pick an output folder. It writes three files and
prints a self-check:

| File | Purpose |
|---|---|
| `clinical_table_base.rtf` | The reference table. |
| `clinical_table_reformatted.rtf` | **Same content, different formatting** → must compare **EQUIVALENT** to base. |
| `clinical_table_changed.rtf` | Base with **7 deliberate content edits** → must compare to base as **7 differences**. |

**Command line** (with control over size and seed):

```bash
Rscript R/generate_test_data.R --out my_test_files            # default ~70 rows
Rscript R/generate_test_data.R --out big_files --rows 50000   # large file for speed testing
Rscript R/generate_test_data.R --out repro --seed 12345       # reproducible
```

After generating, compare `base` against `reformatted` (expect EQUIVALENT) and against
`changed` (expect 7 differences) to see the tool in action.

---

## Run the test suite

A full automated suite verifies every part of the tool against small hand-built fixtures,
the large clinical example files, **and** real-world clinical outputs (see
`tests/real_world/`). It also validates change calculations independently, semantic row
alignment, column-level `Change`/`NC` labels, footnote annotations, generated-RTF reparsing,
and the Windows/macOS runners.

**Easy way:** double-click **`windows\3-Run-Tests.bat`** / **`macos/3-Run-Tests.command`**.

**Command line:**

```bash
Rscript R/run_tests.R
```

The final line must report `0 failed, 0 warnings`. The suite covers parsing, normalisation,
all five difference types, numeric tolerance, the report/CSV/exit codes, edge cases
(byte-identical fast path, non-RTF input, trailing empty rows), a large-file performance
check, the two key integration expectations (base↔reformatted EQUIVALENT;
base↔changed = 7 differences), real-world clinical files (`tests/real_world/`), the
**batch folder comparison** (every file listed, including matches), the **audit log**
(every run recorded, including no-difference runs), and the optional Windows/macOS RTF change
tables. Pull requests are additionally checked on `windows-latest` before merge.

---

## How it works

Three independent stages (each separately tested):

```
  RTF file ──► (1) PARSE ──► tall table: one row per cell (row_index, col_index, raw_value)
                  │                 (via striprtf: decodes code page, \'XX and \uN escapes,
                  │                  and table row/cell structure)
                  ▼
              (2) NORMALIZE ──► trim + collapse whitespace, NBSP→space  ──► norm_value
                  │
                  ▼
              (3) COMPARE ──► full outer join of the two tables on (row_index, col_index),
                              classify each cell, summarise, write report + exit code
```

Because rows and cells are matched **positionally**, the approach works for tables of any
shape (any number of columns) with no assumptions, and row/column count mismatches simply
fall out as `CELL_ONLY_IN_FILEn` entries. The comparison uses a fast `data.table` keyed
join so it scales to large files. As an independent cross-check, the `diffdf` package (the
R analogue of SAS `PROC COMPARE`, widely trusted in pharma QC) can corroborate the result.

The optional cross-platform change-table writer is a separate presentation stage. It aligns body
rows semantically by Column 1 description to prevent a row inserted in one set from shifting
every later displayed result. This does not change the normal comparison engine or its
positional audit evidence.

---

## What has been validated

- **Content, not markup:** `clinical_table_base.rtf` vs `clinical_table_reformatted.rtf`
  → **EQUIVALENT**, even though the two files differ on thousands of lines of RTF markup.
- **Real differences are caught precisely:** `base` vs `clinical_table_changed.rtf`
  → **exactly 7 differences** at the correct cells, including a subtitle change and a
  footnote change (proving non-table text is compared).
- **Special characters** decode correctly from both `\'XX` (Windows-1252) and `\uN`
  (Unicode) escapes: ©, ≥, −, °, µ, α, β, –, é, ü.
- **Robustness:** byte-identical files short-circuit instantly; non-RTF/empty input gives a
  clear error (not a stack trace); a trailing blank row in one file does not create spurious
  differences; CRLF and LF line endings are treated the same.

See `docs/RTF_Compare_Implementation_Plan_R.md` for the full build specification and
`examples/README.md` for details of the provided example files.

---

## Known limitations

- **Merged / spanned cells** can shift positional column alignment; such cases are reported
  as mismatches rather than resolved.
- **Embedded images, drawing objects, deeply nested tables** are out of scope — text is
  extracted, objects are ignored.
- **Unicode canonical normalisation is intentionally *not* applied** (comparison is exact),
  so visually similar but distinct code points (e.g. micro sign µ vs Greek mu μ) are
  reported as different — which is correct for exact QC.
- **Very large files:** typical clinical TLF files (a few thousand rows) compare in well
  under a second. Tens of thousands of rows take longer because the RTF parser (`striprtf`)
  is pure R. For genuinely huge datasets, compare the **source data** that produced the RTF
  rather than the rendered RTF.
- **SAS version** is out of scope for now; the architecture is language-agnostic and could
  be ported later.

---

## Repository layout

```
Updated-rtf-comparison-tool/
├── README.md                  ← you are here
├── START HERE (Windows).txt   ← plain-text quick start for the client (Windows)
├── START HERE (Mac).txt       ← plain-text quick start for the client (macOS)
├── AGENTS.md                  ← guide for developers / AI agents working on the repo
├── R/                         ← all the R scripts
│   ├── compare_rtf.R          ← the comparison engine + command-line interface
│   ├── run_compare_paths.R    ← compare by pasting two file paths (default)
│   ├── run_compare.R          ← point-and-click runner (file pickers, the "2b" option)
│   ├── run_compare_folder.R   ← batch: compare two whole folders (the "2c" option)
│   ├── install_packages.R     ← one-time package installer
│   ├── generate_test_data.R   ← synthetic test-file generator (pure R)
│   └── run_tests.R            ← runs the automated test suite
├── windows/                   ← double-click launchers for Windows (.bat)
├── macos/                     ← double-click launchers for macOS (.command)
├── logs/                      ← audit log + reports + optional RTF Changes (DO NOT MOVE)
├── examples/                  ← provided clinical RTF files + their description
├── docs/                      ← the implementation specification
└── tests/                     ← test suite, fixtures, and real-world tests
```

---

*Built by Lawlor Solutions. Licensed under the MIT License (see `LICENSE`).*
