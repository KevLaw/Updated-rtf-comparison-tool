# RTF Change-Delta Tables — Original Windows Executor Implementation Plan

> **Cross-platform update:** The plan below records the original Windows
> delivery. The validated feature is now also enabled for macOS. Folder-mode
> filenames are paired by exact normalized name first, then by the conservative
> unique like-name rule documented in `README.md`. Fuzzy candidates are also
> validated and ranked using displayed title and Column 1 agreement. The later
> v1.5 update adds `0`-separated filename families and an all-unmatched
> content-only fallback. Change RTFs are generated
> only for successfully compared pairs; unmatched files remain report-only.
> The final repository name is
> `Updated-rtf-comparison-tool`.

## 1. Executor mandate

Implement, test, publish, and merge the Windows-only RTF change-delta feature described in
this document. There are no open product questions. Do not reinterpret the display rules or
substitute a different comparison formula.

The repository may already contain uncommitted work for the first version of RTF change-table
generation. Build from that work; do not discard it. The worktree also contains unrelated
user-owned edits to `macos/*.command`. Preserve those edits locally, do not modify them, and do
not include them in the feature commit or pull request.

The finished work must be published to:

- Repository: `https://github.com/KevLaw/Updated-rtf-comparison-tool`
- Feature branch: `codex/rtf-change-deltas`
- Target branch: `main`
- Final ZIP: `https://github.com/KevLaw/Updated-rtf-comparison-tool/archive/refs/heads/main.zip`

Do not merge until every local test and every required GitHub Windows check passes. After all
checks pass, merge the pull request into `main`, verify the merged revision, and return the
active final ZIP link to the user.

---

## 2. Existing invariants that remain mandatory

Read `AGENTS.md` before editing. Preserve every golden invariant, including:

1. Compare rendered cell content, never raw RTF bytes.
2. Preserve the three independently tested stages:
   `parse_rtf()` -> `normalize_cells()` -> `compare_tables()`.
3. Preserve exit codes: `0` equivalent, `1` differences, `2` error.
4. Keep exact comparison as the default; numeric tolerance remains opt-in.
5. Keep engine/report/audit responsibilities separated.
6. Audit every interactive comparison, including equivalent comparisons.
7. Preserve special characters exactly: `© ≥ − ° µ – é ü α β`.
8. Preserve the existing canonical outcomes:
   - base vs reformatted -> `EQUIVALENT`
   - base vs changed -> exactly seven existing comparison differences
9. The normal comparison report must continue to identify all content differences. The special
   display rules in this plan apply to generated change RTFs, not to the underlying comparison
   engine's evidence or audit report.

The feature is exposed only through the Windows workflows. Shared pure-R library helpers may
remain cross-platform and testable on macOS/Linux, but macOS launchers and macOS interactive
behavior must not change.

---

## 3. User-facing workflow

The Windows path runner, file-picker runner, and folder runner retain the final prompt:

> Would you like a separate set of RTF tables generated showing only the differences or no
> differences identified line by line?

If the user answers Yes:

1. The first selected file/folder is Set 1.
2. The second selected file/folder is Set 2.
3. Generate one RTF for each successfully paired source table in both output sets.
4. Save outputs under:
   - `logs/RTF Changes/Set 1/`
   - `logs/RTF Changes/Set 2/`
5. Preserve relative subdirectories for recursive comparisons.
6. Preserve the original filename and insert `_change` before the extension.
7. Keep Set 1 and Set 2 in separate folders so same-named files never collide.
8. Unmatched or unreadable batch files remain reported by the batch comparison. Generate change
   RTFs only when the pair can be parsed and aligned safely.

The generated Set 1 and Set 2 documents use their respective source document formatting as the
style template, but contain the same logical aligned rows, calculated changes, and annotated
Set 2 footnotes.

---

## 4. Required document appearance

For each generated RTF:

1. Preserve the source page setup, orientation, margins, fonts, font sizes, borders, cell widths,
   paragraph alignment, title block, spacing, and footnote styling as closely as possible.
2. Add `_Change` to every displayed table number, including repeated page titles.
3. Preserve the original column-header text.
4. Add a second line to each repeated column-header cell containing either `Change` or `NC`.
5. Column 1 body cells retain the aligned row description so the user can identify each row.
6. Data cells contain only the calculated numeric change, `NC`, `CHG`, or the required
   only-in-set marker. Do not retain either source data value in a generated data cell.
7. Both generated sets must use the same union of logical rows in the same order so that the
   documents align line by line.

Do not merely append labels to the existing source values. The completed feature produces a
true change table.

---

## 5. Semantic row alignment

### 5.1 Row identity

Stop using absolute row number as the change-table alignment key. The normal comparison engine
may retain its existing positional contract, but the change-table model must align table body
rows by the displayed Column 1 description.

Use the decoded displayed Column 1 string as the identity. Preserve its exact source spelling,
case, punctuation, and indentation in output. Repeated identical descriptions are matched by
occurrence order within their nearest section.

For alignment only, an unindented body description establishes the current section for the
indented descriptions that follow it. An unindented row itself belongs to the root section.
Construct an internal key equivalent to:

`section description + row description + occurrence number`

Repeated pagination headers are not body rows and never participate in row matching. If source
indentation cannot be recovered safely, fall back to global exact-description occurrence order
and record a diagnostic rather than guessing a section.

### 5.2 Deterministic union order

Build an occurrence-aware longest-common-subsequence alignment of the Set 1 and Set 2 row-key
sequences.

1. Set 2 order is authoritative for matched and Set-2-only rows.
2. Insert Set-1-only rows relative to their nearest matched Set 1 neighbors.
3. Preserve the original order of all Set-1-only rows that share an insertion point.
4. Never allow one insertion to shift every later row into a false difference.
5. Emit the resulting union sequence identically in both output documents.

### 5.3 Rows present in only one set

If a description exists only in one set:

1. Insert the row into the union at the deterministic aligned location.
2. Append to Column 1 exactly:
   - ` (ONLY IN SET 1)`, or
   - ` (ONLY IN SET 2)`
3. Put `ONLY IN SET 1` or `ONLY IN SET 2` in every remaining cell in that row.
4. Treat every only-in-set cell as a change when determining the column-header label.
5. Use the originating row's formatting when available; when synthesizing it in the other set,
   clone the nearest compatible body-row style from that output template.

---

## 6. Cell calculation and display rules

The direction is always:

`change = Set 2 - Set 1`

Both generated output sets display the same directional result.

### 6.1 Exact matches

An equal supported cell displays:

`NC`

### 6.2 Count-and-percentage cells

Recognize cells such as `45 (21.4%)`, allowing ordinary formatting variations such as commas,
signed values, and spacing.

1. Parse the displayed count from each set.
2. Parse the displayed percentage from each set.
3. Calculate the count component as `count2 - count1`.
4. Calculate the percentage component strictly as:

   `displayed_percentage2 - displayed_percentage1`

5. Do not calculate relative-rate change. Example: `5% -> 4%` is `-1%`, not `-20%`.
6. Do not recompute the displayed percentages from the column-header denominator for the output
   calculation. The displayed source percentages are authoritative.
7. The parser may validate the displayed percentages against source `N` values and issue a
   non-fatal diagnostic, but that validation must not replace the calculation above.
8. If the counts are equal, display `NC` even if the displayed percentages differ.
9. If the count changes, display both signed components in parentheses:
   - increase: `(+5, +0.08%)`
   - decrease: `(-5, -0.08%)`
10. If a component is mathematically zero in a changed pair, display it as unsigned `0`.

### 6.3 Plain scalar numeric cells

If both cells consist of one plain numeric value, calculate `value2 - value1` and display only
the signed result, for example:

`+1.5`

Display `NC` when the numeric difference is zero.

### 6.4 Unsupported or non-scalar changed cells

Text, dates, confidence intervals, ranges, compound statistics, and other cells that cannot be
unambiguously reduced to one scalar numeric difference display:

`CHG`

An equal unsupported cell still displays `NC`. `CHG` counts as a changed cell for the column
header.

### 6.5 Numeric formatting

Format calculated numbers with up to two significant figures:

- `0.476` -> `0.48`
- `2.38` -> `2.4`
- `12.34` -> `12`
- `1.00` -> `1`

Remove unnecessary trailing zeros, retain a leading zero for magnitudes below one, never emit
scientific notation for normal clinical-table magnitudes, preserve the Unicode minus sign only
when it is part of the selected output style, and avoid negative zero.

Implement and unit-test one central numeric formatter; do not duplicate rounding logic across
writers.

---

## 7. Column-header Change/NC rule

For each logical column:

1. Exclude title paragraphs, footnotes, repeated headers, and wholly blank separator rows.
2. Inspect every aligned body cell, including only-in-set and `CHG` cells.
3. If every included cell is exactly `NC`, add `NC` beneath the original column header.
4. If any included cell is anything other than `NC`, add `Change` beneath the original header.
5. Apply the same computed label to every repeated pagination copy of that header.
6. Apply the rule independently per column, including Column 1.

---

## 8. Footnote comparison and rendering

### 8.1 Footnote section

Treat the non-table paragraphs after the final table row as the footnote section, excluding
blank spacer paragraphs. Preserve their Set 2 order and Set 2 styling. Both generated output
sets contain the same Set 2 footnote text and annotations.

### 8.2 Cosmetic spacing equivalence

For footnote comparison only, ignore:

1. Repeated whitespace.
2. Tabs versus spaces.
3. Whitespace added or removed immediately around punctuation.
4. Whitespace added or removed immediately around hyphens.

Therefore `25-mg` and `25 - mg` are equivalent. Output the exact Set 2 spelling and spacing,
without brackets, and do not count that difference as a footnote change.

Do not ignore changes in case, punctuation characters, digits, words, hyphen presence, or other
displayed characters. Thus `25 mg` versus `25-mg` is material.

### 8.3 Material inline changes

When a paired footnote has a material change:

1. Output only the Set 2 footnote text.
2. Surround each minimal changed Set 2 character span with parentheses.
3. Preserve unchanged Set 2 characters outside the parentheses.
4. Use token-aware sequence alignment after cosmetic-space equivalence has been accounted for.
   Apply character-level alignment within a replacement token only for case-only changes,
   same-length replacements, or close edits; otherwise bracket the entire Set 2 token. This is
   required so `great` -> `rotten` becomes `(rotten)` rather than fragmented spans.
5. Coalesce adjacent changed Set 2 characters or tokens into one parenthesized span.
6. Represent a Set 1 deletion that has no Set 2 characters as `(missing)` at the aligned deletion
   point.

Required example:

- Set 1: `today is a great day July 31, 2025`
- Set 2: `Today is a rotten day July 31, 2026`
- Output: `(T)oday is a (rotten) day July 31, 202(6)`

### 8.4 Entire added or missing footnotes

Align footnote paragraphs with deterministic dynamic programming using cosmetic-normalized
edit distance, an explicit gap penalty, and a tested similarity threshold. A changed footnote
must pair as a replacement when sufficiently similar; an inserted footnote must not cascade all
later footnotes. Document the threshold and tie-break in code and tests.

1. A footnote only in Set 2 is output with its entire Set 2 text in parentheses.
2. A footnote only in Set 1 is represented by `(missing)` at its aligned location.
3. Either case is a material footnote change.

### 8.5 Footnote change heading

If and only if at least one material footnote change exists, insert this exact paragraph
immediately before the first footnote:

`Footnote changes in brackets`

Use the source footnote style. Do not add the heading for cosmetic spacing differences alone.

---

## 9. RTF implementation architecture

The feature must safely preserve arbitrary source formatting while replacing cell content and
inserting aligned rows. Do not rely on a regex-only whole-document transformation.

### 9.1 Required internal model

Introduce tested internal structures for:

1. Document paragraphs and table rows in displayed order.
2. Raw RTF spans for paragraphs, rows, and cells.
3. Decoded displayed cell values.
4. Header/body/blank/footnote classification.
5. Source row formatting templates.
6. Aligned union rows and calculated output values.
7. Footnote alignments and annotated Set 2 text.

### 9.2 Safe raw-span mapping

Use an RTF-aware scanner that tracks braces, destination groups, escaped braces/backslashes,
control words, control symbols, hex escapes, signed Unicode escapes with active `\ucN`, and
table controls. Map displayed cells back to exact source spans and validate the mapping against
`striprtf` output before writing.

If safe mapping cannot be proven for a file, do not write a possibly corrupted RTF. Report a
clear per-file generation error and continue the remainder of a batch.

The first supported version must explicitly reject nested tables, merged cells, inconsistent
column counts, ambiguous multi-row headers, multiple independent tables, unmappable displayed
text, and other unsupported structures. Rejection is preferable to silently corrupt output.

### 9.3 Rendering strategy

For each output template:

1. Preserve the source preamble, font/color/style tables, page settings, title paragraphs, and
   non-table formatting.
2. Reuse original/repeated header row RTF and replace only the second-line status label.
3. Reuse or clone compatible body-row RTF, replacing only displayed cell content.
4. Escape all inserted Unicode/text correctly for the source code page and `\ucN` behavior.
5. Insert the aligned union rows identically in both outputs.
6. Replace the footnote section with the shared annotated Set 2 logical footnotes, rendered in
   each output template's footnote style.
7. Write atomically through a temporary file in the destination directory, validate it, and
   rename it into place only after validation succeeds.

### 9.4 Post-write validation

Every generated RTF must be reparsed before it is reported as successful. Validate:

1. RTF header and balanced grouping.
2. Expected title and `_Change` table numbers.
3. Exact aligned row count and Column 1 descriptions.
4. Expected cell count per aligned row.
5. Expected calculated display values.
6. Expected repeated-header `Change`/`NC` labels.
7. Expected annotated footnotes and heading presence/absence.
8. Preservation of required special characters.

Delete an invalid temporary output and report the generation failure without aborting a batch.

---

## 10. Test implementation

Add focused unit, integration, end-to-end, render, and Windows CI coverage. Tests must be
deterministic and must not write runtime artifacts into the repository.

### 10.1 Cell calculation tests

Cover at minimum:

1. `45 (21.4%) -> 50 (23.8%)` => `(+5, +2.4%)`.
2. `45 (5%) -> 50 (4%)` => `(+5, -1%)`.
3. Decrease => `(-5, -0.08%)` using fixture values that actually yield `-0.08`.
4. Equal counts with differing percentages => `NC`.
5. Plain scalar numeric increase/decrease/equality.
6. Unsupported equal => `NC`; unsupported changed => `CHG`.
7. Positive/negative zero suppression.
8. Two-significant-figure boundary and carry cases.

### 10.2 Alignment tests

Cover:

1. Exact Column 1 matches.
2. Repeated descriptions paired by occurrence within section.
3. One middle row only in Set 1.
4. One middle row only in Set 2.
5. Multiple consecutive insertions.
6. Insertions near the first/last body row.
7. Repeated pagination headers excluded.
8. No positional cascade after an inserted row.
9. Identical union order in both generated outputs.

### 10.3 Column-header tests

Cover:

1. Entire column `NC` => header `NC`.
2. One changed cell => header `Change`.
3. `CHG` => header `Change`.
4. Only-in-set row => header `Change`.
5. Blank separators ignored.
6. Every repeated header receives the same result.

### 10.4 Footnote tests

Cover exact expected strings for:

1. No change.
2. Repeated whitespace only.
3. Tab/space difference only.
4. `25-mg` vs `25 - mg` => no material change and exact Set 2 output.
5. `25 mg` vs `25-mg` => material change.
6. Case-only change => changed Set 2 character bracketed.
7. Word replacement.
8. Digit replacement with minimal changed digit bracketed.
9. Inline Set 1 deletion => `(missing)`.
10. Set 2 insertion => inserted Set 2 span bracketed.
11. Entire Set-2-only footnote => entire text bracketed.
12. Entire Set-1-only footnote => `(missing)`.
13. Exact required sentence example =>
    `(T)oday is a (rotten) day July 31, 202(6)`.
14. Heading present for material changes and absent for cosmetic-only changes.

### 10.5 Generated and canonical RTF tests

Extend `R/generate_test_data.R` or add dedicated static fixtures to include:

1. A five-count change with a known percentage-point delta.
2. A whole unchanged column.
3. A middle row only in each set.
4. Repeated Column 1 descriptions.
5. Plain scalar numeric cells.
6. Cosmetic and material footnote cases.

For every generated change RTF, independently calculate expected results from the parsed source
values, then reparse the output and assert exact agreement. Do not test the writer by calling
the writer's own calculation helper to obtain expected values.

Retain all existing canonical fixtures and tests. The final exact command remains:

```bash
Rscript R/run_tests.R
```

It must report zero failures and zero warnings. The four existing optional real-world skips are
acceptable only while their private files remain absent.

### 10.6 Visual/render QA

Render representative generated RTFs and compare them with their source templates. Inspect at
minimum:

1. First page/title/header.
2. A page containing a repeated header.
3. An inserted only-in-set row.
4. The footnote page/section.
5. A table with all-NC and mixed Change/NC columns.

Record the QA method and inspected artifacts in the PR description. Do not commit generated QA
outputs.

---

## 11. Windows CI and acceptance gate

Add a GitHub Actions workflow targeting `windows-latest` that:

1. Checks out the feature branch/PR revision.
2. Installs a supported R version.
3. Installs the five required CRAN packages.
4. Runs `Rscript R/run_tests.R` with the default 3,000-row performance fixture.
5. Exercises the path, picker, and folder R runners non-interactively with
   `RTF_GENERATE_CHANGES=yes`.
6. Verifies expected exit codes and output paths under `logs/RTF Changes/Set 1` and `Set 2`.
7. Reparses every generated Windows RTF and checks expected delta/NC/footnote text.

The automated `windows-latest` job is the agreed pre-merge Windows validation. Do not merge if
it is skipped, cancelled, neutral, or failing.

Required pre-merge gates:

1. `git diff --check` passes.
2. All edited R files parse.
3. Full local default suite passes.
4. Focused change-table tests pass independently.
5. Representative outputs pass render/visual QA.
6. GitHub Windows job passes.
7. All other required repository/PR checks pass.

---

## 12. Documentation and versioning

Update:

1. `README.md`
2. `START HERE (Windows).txt`
3. `logs/README.txt`
4. `AGENTS.md` only where test totals or durable architecture instructions require it
5. Relevant fixture README files

Document the exact output grammar, Set 2 minus Set 1 direction, displayed-percentage subtraction,
two-significant-figure formatting, semantic row alignment, only-in-set rows, column-header rule,
footnote brackets, output folders, and the original Windows-only availability recorded by this
historical plan. The cross-platform update must document macOS availability and safe like-name
pairing in both START HERE files and the README.

Bump `RTF_TOOL_VERSION` from the current in-progress `1.2.0` to `1.3.0` because this materially
changes the generated artifact contract. The subsequent safe like-filename pairing and macOS
enablement release is `1.4.0`; `0`-token families and all-unmatched content search are `1.5.0`.

Keep `logs/RTF Changes/` ignored by Git.

---

## 13. Git, pull request, merge, and final handoff

1. Recheck the dirty worktree before editing.
2. Preserve and exclude the unrelated `macos/*.command` changes.
3. Create/switch to `codex/rtf-change-deltas`.
4. Implement in reviewable commits or one coherent feature commit.
5. Stage only files belonging to this Windows feature and its shared tested engine support.
6. Review the staged diff for secrets, runtime logs, generated RTFs, and accidental macOS edits.
7. Push the branch to `origin` (`KevLaw/rtf-comparison-tool`).
8. Open a pull request targeting `main` with:
   - behavior summary
   - exact formulas and examples
   - test counts and commands
   - Windows CI result
   - visual QA summary
   - explicit note that macOS launchers were unchanged/excluded
9. Wait until every required check is complete and passing.
10. Merge the PR into `main` only after the complete gate in Section 11 passes.
11. Verify the PR is merged, `main` contains the merge, and the final ZIP endpoint responds.
12. Return the PR URL, merged commit, final test result, and this active link:

[Download the final validated cross-platform ZIP](https://github.com/KevLaw/Updated-rtf-comparison-tool/archive/refs/heads/main.zip)

Do not call the ZIP final or validated before the implementation is merged into `main`.

---

## 14. Definition of done

The work is complete only when all of the following are true:

1. All generated data cells contain only a calculated change, `NC`, `CHG`, or only-in-set marker.
2. Percent changes use displayed Set 2 percentage minus displayed Set 1 percentage.
3. Count-and-percentage and scalar numeric formatting matches this specification exactly.
4. Rows align by Column 1 description and repeated occurrence, without positional cascades.
5. Both generated sets have the identical aligned union row sequence.
6. Column headers correctly display `Change` or `NC` across all repeated pages.
7. Cosmetic footnote spacing is ignored and exact Set 2 text is retained.
8. Material Set 2 footnote spans are minimally parenthesized; deletions show `(missing)`.
9. The footnote-change heading appears exactly when required.
10. Source formatting remains acceptably faithful and every output reparses successfully.
11. Existing comparison/audit/exit-code/special-character invariants remain intact.
12. Local and GitHub Windows tests pass with zero failures and zero warnings.
13. No macOS launcher or macOS workflow changes are included.
14. The PR is merged into `KevLaw/rtf-comparison-tool` `main`.
15. The final response contains the working `main.zip` download link.
