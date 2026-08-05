========================================================================
 logs/  --  AUDIT LOG AND ARCHIVED REPORTS   (DO NOT MOVE OR DELETE)
========================================================================

This folder is part of the RTF Comparison Tool's structure. The tool writes
its records here, so it must stay in place, next to the R/ folder.

  ** Keep the whole tool folder intact. If you move the R scripts or the
     launchers out of this folder, the tool can no longer find logs/ and
     will not be able to save the audit log or archived reports. **

What gets written here:

  audit_log.csv
      An append-only, timestamped record of EVERY comparison run -- including
      runs that found NO differences. One row per run. Open it in Excel or
      Numbers to review, sort, or print proof of which files/folders were
      compared and when. Columns:

        timestamp         when the run finished (local time)
        run_type          "single" (two files) or "batch" (two folders)
        item1, item2       the files or folders compared
        result            short outcome, e.g. EQUIVALENT / 3 DIFFERENCE(S)
        files_compared    number of files compared in the run
        files_equivalent  how many were equivalent
        files_differing   how many differed
        files_only_in_1   files present only in folder 1 (batch runs)
        files_only_in_2   files present only in folder 2 (batch runs)
        files_errored     files that could not be compared
        total_diffs       total cell-level differences across the run
        report_saved      path of the saved/archived report, if any
        user, host        who ran it and on which machine
        tool_version      the tool version that produced the row

  reports/
      Full batch reports are archived here automatically, one timestamped
      file per batch run (a .txt full report and a .csv per-file summary).

  RTF Changes/
      Optional Windows or macOS CSV change tables created when the user answers Yes to
      the final prompt. Set 1 contains CSVs named from the first selected
      file/folder; Set 2 contains CSVs named from the second. Both show the
      same Set 2-minus-Set 1 results and aligned union of Column 1 rows. Each
      output replaces the source .rtf extension with "_change.csv".

      A generated data cell contains only NC, a signed count/percentage-point
      delta such as (+5, -1%), a signed scalar delta such as +1.5, CHG, or an
      ONLY IN SET marker. Column headers say NC only when the entire column is
      NC; otherwise they say Change. Material Set 2 footnote differences are
      shown in parentheses beneath "Footnote changes in brackets"; cosmetic
      spacing changes are ignored and Set 1 deletions appear as (missing).

      A file found in only one compared folder never produces a change CSV.
      Exact normalized names pair first. Like names and related filename
      families separated by "0" must also pass a title/Column 1 content gate.
      Every still-unmatched RTF is then content-checked against every eligible
      unmatched RTF in the other set. The report records content-only matches
      and explicitly states when the complete search finds no safe match.

This folder is safe to back up. You can copy audit_log.csv elsewhere, but do
not move the original out of this folder while you are still using the tool.
========================================================================
