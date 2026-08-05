#!/usr/bin/env Rscript
# =============================================================================
# run_compare_folder.R  --  BATCH compare two FOLDERS of RTF files (file picker)
# =============================================================================
# Pick the Set 1 folder first and the Set 2 folder second. The tool compares every .rtf
# file in the first folder against the same-named file in the second folder and
# produces ONE report listing every file -- including the ones that are
# EQUIVALENT -- so the run is a complete QC record.
#
# Two things are written automatically, both inside the tool folder:
#   * logs/reports/RTF_batch_report_<timestamp>.txt|.csv  -- the full report.
#   * logs/audit_log.csv                                  -- one row per run,
#       a permanent, timestamped record (kept even when no differences are
#       found) so you can prove which folders were checked and when.
# You are also offered a Save dialog to keep your own copy wherever you like.
#
# IMPORTANT: keep the whole tool folder intact. The logs/ folder lives next to
# the R/ folder; if the tool is moved out of its folder it cannot find where to
# write the audit log and reports.
#
# Double-click the launcher for your computer:
#     Windows :  windows\2c-Compare-Folders.bat
#     macOS   :  macos/2c-Compare-Folders.command
# (or run:  Rscript R/run_compare_folder.R)
# =============================================================================

# --- locate this script and load the comparison engine ----------------------
.this_file <- function() {
  ca <- commandArgs(FALSE)
  m <- grep("^--file=", ca, value = TRUE)
  if (length(m)) return(normalizePath(sub("^--file=", "", m[[1]]), mustWork = FALSE))
  if (!is.null(sys.frame(1)$ofile)) return(normalizePath(sys.frame(1)$ofile, mustWork = FALSE))
  getwd()
}
script_dir <- dirname(.this_file())
engine <- file.path(script_dir, "compare_rtf.R")
if (!file.exists(engine)) engine <- file.path(getwd(), "R", "compare_rtf.R")
if (!file.exists(engine)) engine <- file.path(getwd(), "compare_rtf.R")
if (!file.exists(engine)) stop("Cannot find compare_rtf.R (expected in the R/ folder).", call. = FALSE)
source(engine)

# The tool root holds the logs/ folder (audit log + archived reports).
root <- rtf_tool_root(script_dir)

# --- small cross-platform helpers -------------------------------------------
# Resolve preset folders before Tcl/Tk is touched. Scripted validation supplies
# both paths and must remain headless; initializing Tk in that mode can abort R
# on macOS even though no picker or popup is needed.
.cli_args <- commandArgs(trailingOnly = TRUE)
preset1 <- if (length(.cli_args) >= 1L) .cli_args[[1]] else Sys.getenv("RTF_DIR1", "")
preset2 <- if (length(.cli_args) >= 2L) .cli_args[[2]] else Sys.getenv("RTF_DIR2", "")
gui_requested <- !nzchar(preset1)
have_tcltk <- gui_requested && requireNamespace("tcltk", quietly = TRUE) &&
  isTRUE(tryCatch({ tcltk::tclvalue(tcltk::tclVar("ok")); TRUE },
                  error = function(e) FALSE))

say <- function(...) cat(..., "\n", sep = "")
windows_mode <- identical(Sys.info()[["sysname"]], "Windows")
macos_mode <- identical(Sys.info()[["sysname"]], "Darwin")
change_prompt_mode <- windows_mode || macos_mode

CHANGE_CSV_PROMPT <- paste0(
  "Would you like separate CSV and RTF tables generated showing only the ",
  "differences or no differences identified line by line?")

pick_folder <- function(title) {
  if (have_tcltk) {
    d <- tryCatch(as.character(tcltk::tkchooseDirectory(title = title)),
                  error = function(e) "")
    if (length(d) == 0L || !nzchar(d[[1]])) return(NA_character_)
    return(d[[1]])
  }
  say(">>> ", title)
  say("    (type or paste a folder path, then press Enter)")
  con <- file("stdin", open = "r")
  on.exit(close(con))
  d <- tryCatch(readLines(con, n = 1L, warn = FALSE), error = function(e) character(0))
  if (length(d) == 0L) return(NA_character_)
  d <- trimws(sub('^"(.*)"$', "\\1", trimws(d[[1]])))
  if (!nzchar(d)) NA_character_ else d
}

open_path <- function(p) {
  os <- Sys.info()[["sysname"]]
  tryCatch({
    if (os == "Windows")      shell.exec(normalizePath(p))
    else if (os == "Darwin")  system2("open", shQuote(p))
    else                      system2("xdg-open", shQuote(p))
  }, error = function(e) invisible(NULL))
}

popup <- function(title, message, equivalent) {
  if (have_tcltk) {
    tryCatch(tcltk::tkmessageBox(title = title, message = message,
                                 icon = if (equivalent) "info" else "warning",
                                 type = "ok"),
             error = function(e) invisible(NULL))
  }
}

ask_change_prompt <- function() {
  if (have_tcltk) {
    ans <- tryCatch(as.character(tcltk::tkmessageBox(
      title = "Generate CSV and RTF change tables?", icon = "question", type = "yesno",
      message = CHANGE_CSV_PROMPT)), error = function(e) "no")
    return(identical(ans, "yes"))
  }
  cat("\n", CHANGE_CSV_PROMPT, " (y/N): ", sep = "")
  con <- file("stdin", open = "r")
  on.exit(close(con))
  ans <- tryCatch(readLines(con, n = 1L, warn = FALSE),
                  error = function(e) character())
  length(ans) > 0L && tolower(trimws(ans[[1]])) %in% c("y", "yes")
}

OPTS <- "num_tol=0, rel_tol=FALSE, trim=TRUE, collapse_space=TRUE, casefold=FALSE"

# --- run ---------------------------------------------------------------------
say("============================================================")
say("RTF Comparison Tool v", RTF_TOOL_VERSION, " - compare two FOLDERS (batch)")
say("============================================================")

# Folders may be supplied up front (two command-line arguments, or the env vars
# RTF_DIR1 / RTF_DIR2) to skip the dialogs -- handy for scripting and tests.

if (nzchar(preset1)) {
  dir1 <- preset1
} else {
  first_desc <- if (change_prompt_mode) "FIRST folder (Set 1)" else "FIRST (reference) folder"
  say("A folder picker will open. Choose the ", first_desc, ".")
  dir1 <- pick_folder(paste0("Step 1 of 2: choose the ", first_desc, " of RTF files"))
}
if (is.na(dir1) || !nzchar(dir1)) {
  say("\nCancelled - no first folder selected. Exiting."); quit(status = 0L) }
say("  Folder 1: ", dir1)

if (nzchar(preset2)) {
  dir2 <- preset2
} else {
  second_desc <- if (change_prompt_mode) "SECOND folder (Set 2)" else "SECOND (comparison) folder"
  say("\nNow choose the ", second_desc, ".")
  dir2 <- pick_folder(paste0("Step 2 of 2: choose the ", second_desc, " of RTF files"))
}
if (is.na(dir2) || !nzchar(dir2)) {
  say("\nCancelled - no second folder selected. Exiting."); quit(status = 0L) }
say("  Folder 2: ", dir2)

gui_mode <- !nzchar(preset1)
run_time <- Sys.time()
stamp    <- format(run_time, "%Y%m%d_%H%M%S")

say("\nComparing every RTF file (cosmetic formatting is ignored)...\n")

batch <- tryCatch(
  compare_rtf_folder(dir1, dir2, console = TRUE, progress = TRUE),
  error = function(e) {
    msg <- paste0("The batch comparison could not be completed:\n\n", conditionMessage(e))
    say("\nERROR: ", conditionMessage(e))
    popup("RTF Batch Comparison - error", msg, equivalent = FALSE)
    tryCatch(append_audit_log(root, "batch", dir1, dir2, "ERROR",
                              files_compared = 0L, files_equivalent = 0L,
                              files_differing = 0L, files_errored = 0L,
                              total_diffs = 0L, report_saved = "",
                              timestamp = run_time),
             error = function(e2) invisible(NULL))
    quit(status = 2L)
  })

t <- batch$totals

# --- always archive the full report inside the tool folder -------------------
# This is the permanent record. The Save dialog below is an extra copy for the
# user; the archive is written regardless so nothing is ever lost.
report_dir <- file.path(root, "logs", "reports")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
archive_txt <- file.path(report_dir, sprintf("RTF_batch_report_%s.txt", stamp))
archive_csv <- file.path(report_dir, sprintf("RTF_batch_report_%s.csv", stamp))
archived <- tryCatch({
  write_batch_report(batch, txt_path = archive_txt, csv_path = archive_csv,
                     options_str = OPTS, console = FALSE, timestamp = run_time)
  TRUE
}, error = function(e) { say("WARNING: could not archive report: ", conditionMessage(e)); FALSE })
if (archived) say("\nReport archived in the tool folder:\n  ", archive_txt)

# --- record the run in the audit log (always, even with no differences) ------
result_str <- if (isTRUE(batch$all_equivalent)) "ALL EQUIVALENT" else
              sprintf("%d DIFFERING, %d UNMATCHED, %d ERROR(S)",
                      t$n_differing, t$n_only1 + t$n_only2, t$n_errors)
audit_path <- tryCatch(
  append_audit_log(root, "batch", dir1, dir2, result_str,
                   files_compared = t$n_files, files_equivalent = t$n_equivalent,
                   files_differing = t$n_differing, files_only_in_1 = t$n_only1,
                   files_only_in_2 = t$n_only2, files_errored = t$n_errors,
                   total_diffs = t$total_diffs,
                   report_saved = if (archived) archive_txt else "",
                   timestamp = run_time),
  error = function(e) { say("WARNING: could not update audit log: ", conditionMessage(e)); NA })
if (!is.na(audit_path)) say("Audit log updated:\n  ", audit_path)

# --- summarise & notify ------------------------------------------------------
summary_msg <- sprintf(paste0(
  "Compared %d file(s):\n",
  "  Equivalent:        %d\n",
  "  Differing:         %d\n",
  "  Only in folder 1:  %d\n",
  "  Only in folder 2:  %d\n",
  "  Errors:            %d\n\n",
  "Full report archived in the tool folder (logs/reports) and recorded in the audit log."),
  t$n_files, t$n_equivalent, t$n_differing, t$n_only1, t$n_only2, t$n_errors)
say("\n", summary_msg)
if (gui_mode) popup(
  if (isTRUE(batch$all_equivalent)) "RTF Batch - ALL EQUIVALENT" else "RTF Batch - DIFFERENCES FOUND",
  summary_msg, equivalent = isTRUE(batch$all_equivalent))

# --- optional export: save your own copy somewhere convenient ----------------
if (gui_mode && have_tcltk) {
  ans <- tryCatch(as.character(tcltk::tkmessageBox(
    title = "Save a copy of the report?", icon = "question", type = "yesno",
    message = "Save your own copy of the batch report?\n(.txt = full report, .csv = per-file summary)")),
    error = function(e) "no")
  if (identical(ans, "yes")) {
    default_name <- sprintf("RTF_batch_%s", stamp)
    dest <- tryCatch(as.character(tcltk::tkgetSaveFile(
      title = "Save report as  (.txt = full report, .csv = per-file summary)",
      initialfile = paste0(default_name, ".txt"),
      filetypes = "{{Text report} {.txt}} {{CSV summary} {.csv}}")),
      error = function(e) "")
    if (nzchar(dest)) {
      is_csv <- grepl("\\.csv$", dest, ignore.case = TRUE)
      ok <- tryCatch({
        if (is_csv) write_batch_report(batch, csv_path = dest, options_str = OPTS,
                                       console = FALSE, timestamp = run_time)
        else        write_batch_report(batch, txt_path = dest, options_str = OPTS,
                                       console = FALSE, timestamp = run_time)
        TRUE }, error = function(e) { say("ERROR writing file: ", conditionMessage(e)); FALSE })
      if (ok) { say("Saved your copy: ", dest); open_path(dest) }
    }
  }
}

# --- optional per-source CSV and RTF tables (Windows and macOS workflows) ---
generate_changes <- FALSE
change_generation_failed <- FALSE
change_preset <- tolower(trimws(Sys.getenv("RTF_GENERATE_CHANGES", "")))
if (change_preset %in% c("y", "yes", "true", "1")) {
  generate_changes <- TRUE
} else if (!change_preset %in% c("n", "no", "false", "0") &&
           change_prompt_mode && gui_mode) {
  generate_changes <- ask_change_prompt()
}
if (generate_changes) {
  expected_pairs <- sum(batch$summary$status %in% c("EQUIVALENT", "DIFFERENCES"))
  say("\nGenerating change CSVs and RTFs for all ", expected_pairs,
      " officially compared pair(s)...")
  change_files <- write_batch_change_outputs(batch, root, progress = TRUE)
  successes <- change_files[change_files$ok, , drop = FALSE]
  failures <- change_files[!change_files$ok, , drop = FALSE]
  successful_pairs <- length(unique(successes$pair))
  failed_pairs <- length(unique(failures$pair))
  complete <- expected_pairs == successful_pairs && failed_pairs == 0L &&
              nrow(successes) == expected_pairs * 4L &&
              all(file.exists(successes$output))
  if (complete) {
    say("Completed ", successful_pairs, " of ", expected_pairs,
        " compared pair(s) (", nrow(successes), " CSV/RTF files).")
    say("CSV and RTF change tables saved under:")
    say("  ", file.path(root, "logs", "RTF Changes"))
  } else {
    change_generation_failed <- TRUE
    say("ERROR: change CSV/RTF generation was incomplete: ", successful_pairs,
        " of ", expected_pairs, " compared pair(s) completed.")
  }
  if (nrow(failures) > 0L) {
    for (i in seq_len(nrow(failures)))
      say("ERROR [", failures$pair[[i]], "]: ", failures$error[[i]])
  }
  if (gui_mode) popup(
    if (complete) "CSV and RTF change tables saved" else "CSV/RTF change tables incomplete",
    paste0(successful_pairs, " of ", expected_pairs, " compared pair(s) completed (",
           nrow(successes), " CSV/RTF file(s)).\n\n",
           file.path(root, "logs", "RTF Changes")), equivalent = complete)
}

say("\nDone.")
# Exit code: 0 = all equivalent, 1 = differences/mismatches, 2 = error.
quit(status = if (change_generation_failed) 2L else if (isTRUE(batch$all_equivalent)) 0L else 1L)
