#!/usr/bin/env Rscript
# =============================================================================
# run_compare.R  --  point-and-click RTF comparison (no typing required)
# =============================================================================
# Opens two file-picker dialogs (choose Set 1 first, then Set 2), compares the
# files, saves a timestamped text report and CSV next to the
# first file, opens the report, and shows a pop-up with the result.
#
# This is the easiest way to use the tool. Double-click the launcher for your
# computer:
#     Windows :  windows/Compare-RTF-Files.bat
#     macOS   :  macos/Compare-RTF-Files.command
# (or run:  Rscript run_compare.R)
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

# The tool root holds the logs/ folder (audit log). Keep the folder intact.
root <- rtf_tool_root(script_dir)

# --- small cross-platform helpers -------------------------------------------
have_tcltk <- requireNamespace("tcltk", quietly = TRUE) &&
  isTRUE(tryCatch({ tcltk::tclvalue(tcltk::tclVar("ok")); TRUE },
                  error = function(e) FALSE))

say <- function(...) cat(..., "\n", sep = "")
windows_mode <- identical(Sys.info()[["sysname"]], "Windows")

CHANGE_RTF_PROMPT <- paste0(
  "Would you like a separate set of RTF tables generated showing only the ",
  "differences or no differences identified line by line?")

pick_file <- function(title) {
  if (have_tcltk) {
    f <- tryCatch(as.character(tcltk::tkgetOpenFile(
      title = title,
      filetypes = "{{RTF files} {.rtf}} {{All files} {*}}")),
      error = function(e) "")
    if (length(f) == 0L || !nzchar(f[[1]])) return(NA_character_)
    return(f[[1]])
  }
  # Fallback: native chooser without a title bar message, then console prompt.
  say(">>> ", title)
  f <- tryCatch(file.choose(), error = function(e) NA_character_)
  if (length(f) == 0L || is.na(f) || !nzchar(f)) return(NA_character_)
  f
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

# --- run ---------------------------------------------------------------------
say("============================================================")
say("RTF Comparison Tool")
say("============================================================")

# Paths may be supplied up front (two command-line arguments, or the env vars
# RTF_FILE1 / RTF_FILE2) to skip the dialogs -- handy for scripting and tests.
.cli_args <- commandArgs(trailingOnly = TRUE)
preset1 <- if (length(.cli_args) >= 1L) .cli_args[[1]] else Sys.getenv("RTF_FILE1", "")
preset2 <- if (length(.cli_args) >= 2L) .cli_args[[2]] else Sys.getenv("RTF_FILE2", "")

if (nzchar(preset1)) {
  file1 <- preset1
} else {
  first_desc <- if (windows_mode) "FIRST file (Set 1)" else "FIRST (reference) file"
  first_title <- if (windows_mode)
    "Step 1 of 2: choose the FIRST RTF file (Set 1)" else
    "Step 1 of 2: choose the FIRST (reference) RTF file"
  say("A file picker will open. Choose the ", first_desc, ".")
  file1 <- pick_file(first_title)
}
if (is.na(file1) || !nzchar(file1)) {
  say("\nCancelled - no first file selected. Exiting."); quit(status = 0L) }
say("  File 1: ", file1)

if (nzchar(preset2)) {
  file2 <- preset2
} else {
  second_desc <- if (windows_mode) "SECOND file (Set 2)" else "SECOND (comparison) file"
  second_title <- if (windows_mode)
    "Step 2 of 2: choose the SECOND RTF file (Set 2)" else
    "Step 2 of 2: choose the SECOND (comparison) RTF file"
  say("\nNow choose the ", second_desc, ".")
  file2 <- pick_file(second_title)
}
if (is.na(file2) || !nzchar(file2)) {
  say("\nCancelled - no second file selected. Exiting."); quit(status = 0L) }
say("  File 2: ", file2)

# GUI niceties (pop-up, save dialog) only when the user actually used the
# pickers; scripted/preset runs stay quiet and just print to the console.
gui_mode <- !nzchar(preset1)
OPTS <- "num_tol=0, rel_tol=FALSE, trim=TRUE, collapse_space=TRUE, casefold=FALSE"

say("\nComparing... (cosmetic RTF formatting is ignored; only content is compared)\n")

result <- tryCatch(
  compare_rtf(file1, file2, run_diffdf = TRUE, console = TRUE),   # nothing saved here
  error = function(e) {
    msg <- paste0("The comparison could not be completed:\n\n", conditionMessage(e))
    say("\nERROR: ", conditionMessage(e))
    popup("RTF Comparison - error", msg, equivalent = FALSE)
    quit(status = 2L)
  })

# --- summarise & notify (nothing is written unless you choose to export) -----
if (isTRUE(result$equivalent)) {
  summary_msg <- paste0("RESULT: The two files are EQUIVALENT.\n\n",
    "No content differences were found (any differences are cosmetic formatting only).")
  say("\n", summary_msg)
  if (gui_mode) popup("RTF Comparison - EQUIVALENT", summary_msg, equivalent = TRUE)
} else {
  summary_msg <- sprintf("RESULT: %d content difference(s) found (listed above).",
                         result$n_diffs)
  say("\n", summary_msg)
  if (gui_mode) popup("RTF Comparison - DIFFERENCES FOUND", summary_msg, equivalent = FALSE)
}

# --- optional export: pick format + location with a Save dialog --------------
saved_path <- ""
if (gui_mode && have_tcltk) {
  ans <- tryCatch(as.character(tcltk::tkmessageBox(
    title = "Export report?", icon = "question", type = "yesno",
    message = "Save the report to a file?\n(.txt = full report, .csv = differences only)")),
    error = function(e) "no")
  if (identical(ans, "yes")) {
    default_name <- sprintf("RTF_comparison_%s_vs_%s_%s",
      tools::file_path_sans_ext(basename(file1)),
      tools::file_path_sans_ext(basename(file2)),
      format(Sys.time(), "%Y%m%d_%H%M%S"))
    dest <- tryCatch(as.character(tcltk::tkgetSaveFile(
      title = "Save report as  (.txt = full report, .csv = differences)",
      initialfile = paste0(default_name, ".txt"),
      filetypes = "{{Text report} {.txt}} {{CSV differences} {.csv}}")),
      error = function(e) "")
    if (nzchar(dest)) {
      is_csv <- grepl("\\.csv$", dest, ignore.case = TRUE)
      ok <- tryCatch({
        if (is_csv) write_report(result, file1, file2, csv_path = dest, console = FALSE)
        else        write_report(result, file1, file2, txt_path = dest, options_str = OPTS, console = FALSE)
        TRUE }, error = function(e) { say("ERROR writing file: ", conditionMessage(e)); FALSE })
      if (ok) { saved_path <- dest; say("Saved: ", dest); open_path(dest) }
    }
  }
}

# --- record this run in the audit log (always, even when EQUIVALENT) ---------
result_str <- if (isTRUE(result$equivalent)) "EQUIVALENT" else
              sprintf("%d DIFFERENCE(S)", result$n_diffs)
audit_path <- tryCatch(
  append_audit_log(root, "single", file1, file2, result_str,
                   files_compared = 1L,
                   files_equivalent = if (isTRUE(result$equivalent)) 1L else 0L,
                   files_differing  = if (isTRUE(result$equivalent)) 0L else 1L,
                   total_diffs = result$n_diffs, report_saved = saved_path),
  error = function(e) { say("WARNING: could not update audit log: ", conditionMessage(e)); NA })
if (!is.na(audit_path)) say("Audit log updated: ", audit_path)

# --- optional RTF change tables (Windows workflow) --------------------------
generate_changes <- FALSE
change_preset <- tolower(trimws(Sys.getenv("RTF_GENERATE_CHANGES", "")))
if (change_preset %in% c("y", "yes", "true", "1")) {
  generate_changes <- TRUE
} else if (!change_preset %in% c("n", "no", "false", "0") &&
           windows_mode && gui_mode && have_tcltk) {
  ans <- tryCatch(as.character(tcltk::tkmessageBox(
    title = "Generate RTF change tables?", icon = "question", type = "yesno",
    message = CHANGE_RTF_PROMPT)), error = function(e) "no")
  generate_changes <- identical(ans, "yes")
}
if (generate_changes) {
  change_files <- tryCatch(
    write_change_rtf_pair(file1, file2, result, root),
    error = function(e) { say("WARNING: could not generate RTF change tables: ",
                              conditionMessage(e)); NULL })
  if (!is.null(change_files)) {
    say("RTF change tables saved:")
    for (p in change_files$output) say("  ", p)
    if (gui_mode) popup(
      "RTF change tables saved",
      paste0("The Set 1 and Set 2 RTF change tables were saved under:\n\n",
             file.path(root, "logs", "RTF Changes")), equivalent = TRUE)
  }
}

say("\nDone.")
quit(status = if (isTRUE(result$equivalent)) 0L else 1L)
