# AGENTS.md — repository guide for AI agents and developers

Read this before changing anything. It captures the project's purpose, the invariants you
must not break, how to set up R on macOS and Windows, and the exact commands to run, test,
and generate data on each OS. End-user (client) docs live in `README.md` and the
`START HERE (Windows).txt` / `START HERE (Mac).txt` files.

---

## 1. What this project is

An **R** tool that compares the *rendered content* of two RTF files and reports their
differences cell by cell — for clinical TLF (Tables/Listings/Figures) QC, but general for
any tabular RTF.

**The cardinal rule:** never diff the raw RTF bytes. RTF is markup; two files that render
the identical table can have completely different bytes. Always **extract the displayed
cell text, normalise it, and compare the normalised content positionally.** The
`*_reformatted` test files exist specifically to prove this.

---

## 2. Golden invariants — do not break these

Run `Rscript R/run_tests.R` after any change. It must report **339 passed, 0 failed**. In
particular:

- **base vs reformatted → EQUIVALENT** (0 differences) — content is compared, not markup.
- **base vs changed → exactly 7 `VALUE_DIFF`s** (`examples/`), at the documented cells.
- **Three separable stages**, each unit-tested in isolation:
  `parse_rtf()` → `normalize_cells()` → `compare_tables()`.
- **Exit codes:** `0` equivalent, `1` differences, `2` error. (CLI + path runner + batch runner.)
- **Default comparison is exact**; numeric tolerance is opt-in (`--num-tol`).
- **The engine never writes the audit log.** `compare_rtf()` / `compare_rtf_folder()` /
  `write_report()` stay pure (write only when handed a path). The *runners* call
  `append_audit_log()`. This is what keeps the test suite from spamming a log and preserves
  the "nothing written unless requested" contract for the CLI.
- **Every interactive run is audit-logged — including no-difference runs.** The three runners
  (`run_compare_paths.R`, `run_compare.R`, `run_compare_folder.R`) append one row per run to
  `logs/audit_log.csv`. Logging failures must never abort a comparison (wrap in `tryCatch`).
- **Batch lists every file**, equivalent ones included; files in only one folder or that fail
  to parse are flagged (`ONLY_IN_FOLDER1/2`, `ERROR`), never silently dropped.
- **Nothing is written to disk unless requested** — the CLI writes only with
  `--report`/`--csv`; the single-file runners show the result then *offer* to export. (The
  batch runner additionally auto-archives its report and appends the audit log, by design.)
- **Special characters must survive decoding unchanged:** `© ≥ − ° µ – é ü α β`. Unicode
  canonical normalisation is intentionally NOT applied (exact comparison).

---

## 3. Project layout

```
R/                     All R scripts live here:
  compare_rtf.R        Engine (parse/normalize/compare/report) + CLI. The CLI auto-runs
                       only when this file is the invoked script (.is_main guard), so
                       sourcing it never triggers the CLI.
  run_compare_paths.R  Interactive runner — paste two paths (default compare launcher).
  run_compare.R        Interactive runner — native file-picker dialogs (the "2b" option).
  run_compare_folder.R Interactive runner — batch-compare two folders (the "2c" option).
  install_packages.R   Idempotent installer for the 5 CRAN packages.
  generate_test_data.R Pure-R synthetic test-data generator (base/reformatted/changed).
  run_tests.R          Runs the testthat suite; exit 0 all-pass, 1 otherwise.
windows/  macos/       Double-click launchers (.bat / .command) + Rscript finders.
                       (2c = batch folders, 5 = view audit log.)
logs/                  Runtime: audit_log.csv (one row per run) + reports/ (archived batch
                       reports). Ships with README.txt only; the data is git-ignored. The
                       tool finds this via rtf_tool_root(), so the folder must stay intact.
examples/              Provided clinical RTF files (canonical integration fixtures).
tests/testthat/        Unit + integration tests (test-01..12) and small fixtures/.
tests/real_world/      Derived-from-real TLF tests (skipped if files removed).
docs/                  Original build specification (historical source of truth for scope).
README.md              Client-facing usage. START HERE (Windows/Mac).txt = quick starts.
```

Packages used: `striprtf` (RTF→text), `data.table` (keyed join), `diffdf` (secondary
cross-check), `testthat` (tests), `optparse` (CLI).

---

## 4. Setup — installing R

### macOS
- **With admin rights (recommended):** install the official CRAN build from
  <https://cran.r-project.org> (the `.pkg`), **or** `brew install --cask r` — note the cask
  runs a `.pkg` installer that prompts for your **admin password**.
- **Without admin / automated / CI:** `brew install r` (the Homebrew *formula*; no sudo).
  CRAN packages then compile from source, which the formula's toolchain supports. This is
  what the current dev machine uses — `Rscript` is at `/opt/homebrew/bin/Rscript`.
- Verify: `Rscript --version`.

### Windows
- `winget install -e --id RProject.R`  (ships with Win 10/11), **or** download the installer
  from <https://cran.r-project.org/bin/windows/base/>. Alternative: `choco install r.project`.
- **Rtools is NOT required** — CRAN provides pre-built binary packages for these deps.
- Verify in a **new** terminal: `Rscript --version`. (Rscript may not be on `PATH`; the
  `windows/_find_rscript.bat` helper locates it under `Program Files\R\R-*\bin`.)

### Install the R packages (either OS)
```
Rscript R/install_packages.R
```
Idempotent; installs only what's missing and prints a status line per package. Or
double-click `windows\1-Install-Packages.bat` / `macos/1-Install-Packages.command`.

---

## 5. Doing things — macOS

```bash
# compare by paths (interactive: prompts for two paths, then offers to export):
Rscript R/run_compare_paths.R
# compare directly (prints result; writes only if you pass --report/--csv):
Rscript R/compare_rtf.R --file1 a.rtf --file2 b.rtf [--report out.txt] [--csv diffs.csv]
# run the whole test suite (expect "339 passed"):
Rscript R/run_tests.R
# generate synthetic test files (raise --rows for a large performance file):
Rscript R/generate_test_data.R --out examples/generated [--rows N] [--seed S]
```
Point-and-click equivalents are in `macos/` (`2-…` paths, `2b-…` file picker, `2c-…` batch
folders, `3-…` tests, `4-…` generate, `5-…` view audit log). The `.command` files must be
executable (`chmod +x macos/*.command`); on
first launch macOS Gatekeeper may require **right-click → Open → Open** (no admin needed).

## 6. Doing things — Windows

```bat
Rscript R/compare_rtf.R --file1 a.rtf --file2 b.rtf
Rscript R/run_tests.R
```
Point-and-click: `windows\2-Compare-RTF-Files.bat` (paste paths), `2b-…` (file picker),
`2c-Compare-Folders.bat` (batch folders), `1-Install-Packages.bat`, `3-Run-Tests.bat`,
`4-Generate-Test-Data.bat`, `5-View-Audit-Log.bat`.

- **Avoid the "Run anyway" / admin prompt:** files arrive "blocked"; clear it without admin
  by right-clicking the **ZIP → Properties → Unblock → OK** *before* extracting.
- **If the company blocks `.bat` entirely:** drive everything from the R console (R is
  installed and allowed): `source("C:/path/to/R/run_compare_paths.R")`, or
  `source("C:/path/to/R/install_packages.R")`. Use forward slashes in the `source()` line;
  the paths pasted at the prompts may use normal backslashes.

---

## 7. Conventions & gotchas

- **Parser delimiter:** `striprtf::read_rtf(..., cell_end = "")`. `strsplit` drops the
  single terminal empty field, so an N-cell table row yields exactly N cells (pinned by
  `test-01-parse.R`). If `striprtf`'s layout ever changes, fix it there first.
- **Trailing blank rows** are trimmed (`.drop_trailing_empty_rows`) so a trailing empty row
  in one file does not create spurious `CELL_ONLY` diffs; mid-document blanks are kept.
- **Fixtures are static, committed files.** Expected results are documented in
  `tests/testthat/fixtures/README.md` and `tests/real_world/README.md`. Keep edits *in
  place* (don't add/remove rows) when you want one clean `VALUE_DIFF` per edit.
- **Real-world tests** (`test-10`) `skip_if_not(file.exists(...))`, so the suite still
  passes if `tests/real_world/` is deleted (e.g. to ship without real data).
- **Reports pad by display width**, not bytes, so columns align with multi-byte characters.
- **Audit log schema is pinned** by `.AUDIT_COLS` in `compare_rtf.R` and asserted in
  `test-12-audit.R`; `append_audit_log()` writes the header only when the file is new, then
  appends one row per run via `data.table::fwrite(append=TRUE)` (handles comma-quoting). If
  you add a column, update both the constant and the test, and `logs/README.txt`.
- **Batch matches files by name** across the two folders (case-insensitive `.rtf` listing,
  exact-name pairing). The byte-identical fast path means two byte-identical *non-RTF* files
  compare EQUIVALENT without parsing — so a batch "ERROR" test needs the two bad files to
  differ in bytes (see `test-11-batch.R`).
- **Known limitation / likely next feature:** real multi-page TLFs repeat the title block
  and footnotes on every printed page; if two versions of the same table paginate
  differently, positional matching drifts. A page-aware "ignore repeated page furniture"
  mode (the spec's deferred `ignore_repeated_headers`) is the natural next addition — see
  `docs/RTF_Compare_Implementation_Plan_R.md` §8/§12.

---

## 8. Definition of done for a change

1. `Rscript R/run_tests.R` → **339 passed, 0 failed** (or more, if you added tests).
2. New behaviour has a test; new options are documented in `README.md` and (if user-facing)
   the `START HERE` files.
3. No stray output files committed (`RTF_comparison_*`, `.DS_Store`, `examples/generated/`
   are git-ignored).
4. The two golden integration results still hold (EQUIVALENT; exactly 7 diffs).
