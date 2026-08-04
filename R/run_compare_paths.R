#!/usr/bin/env Rscript
# =============================================================================
# run_compare_paths.R  --  compare two RTF files by TYPING/PASTING their paths
# =============================================================================
# No file dialogs. You paste two file paths and press Enter. Windows paths with
# backslashes are fine, and surrounding quotes (from "Copy as path" or drag-and-
# drop) are handled automatically. It compares the files, prints the report,
# saves a timestamped .txt and .csv next to the first file, and offers to do
# another pair.
#
# Three ways to run it (all work without administrator rights):
#   * In the R console / RStudio:   source("run_compare_paths.R")
#   * From a terminal:              Rscript run_compare_paths.R
#   * Double-click:                 windows\2-Compare-RTF-Files.bat
#                                   macos/2-Compare-RTF-Files.command
# =============================================================================

# --- locate and load the comparison engine ----------------------------------
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

# --- read a line of input (works in the R console and under Rscript) ---------
.stdin_con <- if (!interactive()) file("stdin", open = "r") else NULL
ask <- function(msg) {
  if (interactive()) return(readline(msg))
  cat(msg)
  line <- tryCatch(readLines(.stdin_con, n = 1L, warn = FALSE), error = function(e) character(0))
  if (length(line) == 0L) "" else line[[1]]
}

# strip surrounding quotes/whitespace that paste or drag-and-drop can add
clean_path <- function(p) {
  p <- trimws(p)
  p <- sub('^"(.*)"$', "\\1", p)
  p <- sub("^'(.*)'$", "\\1", p)
  trimws(p)
}

say <- function(...) cat(..., "\n", sep = "")
windows_mode <- identical(Sys.info()[["sysname"]], "Windows")
macos_mode <- identical(Sys.info()[["sysname"]], "Darwin")
change_prompt_mode <- windows_mode || macos_mode

CHANGE_RTF_PROMPT <- paste0(
  "Would you like a separate set of RTF tables generated showing only the ",
  "differences or no differences identified line by line?")

want_change_rtfs <- function() {
  preset <- tolower(trimws(Sys.getenv("RTF_GENERATE_CHANGES", "")))
  if (preset %in% c("y", "yes", "true", "1")) return(TRUE)
  if (preset %in% c("n", "no", "false", "0")) return(FALSE)
  if (!change_prompt_mode) return(FALSE)
  answer <- tolower(clean_path(ask(paste0("\n", CHANGE_RTF_PROMPT, " (y/N): "))))
  answer %in% c("y", "yes")
}

# default comparison options, shown in the report header
OPTS <- "num_tol=0, rel_tol=FALSE, trim=TRUE, collapse_space=TRUE, casefold=FALSE"

# After showing results on screen, OFFER to export -- TXT (full report) and/or
# CSV (the differences table) -- to a location the user types. Nothing is
# written to disk unless requested.
# Returns the paths actually written (character(0) if nothing was exported), so
# the caller can record them in the audit log.
export_flow <- function(res, f1, f2) {
  saved <- character(0)
  if (isTRUE(res$equivalent))
    say("\n(No differences. You can still save the report as a QC record if you wish.)")
  choice <- tolower(clean_path(ask(
    "\nExport report?  [t] TXT   [c] CSV   [b] both   [Enter] skip : ")))
  if (!nzchar(choice) || choice == "n") { say("Not exported."); return(saved) }
  if (!choice %in% c("t", "c", "b")) { say("Unrecognised choice - not exported."); return(saved) }
  if (choice %in% c("t", "b")) saved <- c(saved, save_export(res, f1, f2, "txt"))
  if (choice %in% c("c", "b")) saved <- c(saved, save_export(res, f1, f2, "csv"))
  saved[nzchar(saved)]
}

save_export <- function(res, f1, f2, ext) {
  default_name <- sprintf("RTF_comparison_%s_vs_%s_%s.%s",
                          tools::file_path_sans_ext(basename(f1)),
                          tools::file_path_sans_ext(basename(f2)),
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ext)
  dest <- clean_path(ask(sprintf(
    "  Save %s as (type a full path, or a folder; Enter to skip): ", toupper(ext))))
  if (!nzchar(dest)) { say("  (", toupper(ext), " skipped)"); return("") }
  if (dir.exists(dest)) {
    path <- file.path(dest, default_name)                 # folder given -> default name
  } else {
    if (!grepl(sprintf("\\.%s$", ext), dest, ignore.case = TRUE)) dest <- paste0(dest, ".", ext)
    path <- dest
  }
  parent <- dirname(path)
  if (!dir.exists(parent)) { say("  ERROR: folder does not exist: ", parent); return("") }
  ok <- tryCatch({
    if (ext == "txt") write_report(res, f1, f2, txt_path = path, options_str = OPTS, console = FALSE)
    else              write_report(res, f1, f2, csv_path = path, console = FALSE)
    TRUE
  }, error = function(e) { say("  ERROR writing file: ", conditionMessage(e)); FALSE })
  if (ok) { path <- normalizePath(path, mustWork = FALSE); say("  Saved: ", path); return(path) }
  ""
}

last_status <- 0L
repeat {
  say("============================================================")
  say("RTF Comparison Tool - compare by path")
  say("============================================================")
  say("Paste or type the full path to each RTF file, then press Enter.")
  say("(Windows backslash paths are fine; surrounding quotes are OK.)")
  say("Leave a path blank to quit.")
  say("")

  f1_label <- if (change_prompt_mode) "File 1 (Set 1) : " else "File 1 (reference)  : "
  f2_label <- if (change_prompt_mode) "File 2 (Set 2) : " else "File 2 (comparison) : "
  f1 <- clean_path(ask(f1_label))
  if (!nzchar(f1)) { say("\nNo path entered - exiting."); break }
  f2 <- clean_path(ask(f2_label))
  if (!nzchar(f2)) { say("\nNo path entered - exiting."); break }

  if (!file.exists(f1)) {
    say("\nERROR: file not found:\n  ", f1)
    last_status <- 2L
  } else if (!file.exists(f2)) {
    say("\nERROR: file not found:\n  ", f2)
    last_status <- 2L
  } else {
    say("\nComparing (cosmetic RTF formatting is ignored)...\n")
    res <- tryCatch(compare_rtf(f1, f2, console = FALSE),
                    error = function(e) { say("\nERROR: ", conditionMessage(e)); NULL })
    if (is.null(res)) {
      last_status <- 2L
    } else {
      write_report(res, f1, f2, options_str = OPTS, console = TRUE)   # show results on screen
      last_status <- if (isTRUE(res$equivalent)) 0L else 1L
      saved <- export_flow(res, f1, f2)                               # offer to save (optional)

      # Record every comparison in the audit log -- including EQUIVALENT runs --
      # so there is a permanent, timestamped record of the QC work performed.
      result_str <- if (isTRUE(res$equivalent)) "EQUIVALENT"
                    else sprintf("%d DIFFERENCE(S)", res$n_diffs)
      audit_path <- tryCatch(
        append_audit_log(root, "single", f1, f2, result_str,
                         files_compared = 1L,
                         files_equivalent = if (isTRUE(res$equivalent)) 1L else 0L,
                         files_differing  = if (isTRUE(res$equivalent)) 0L else 1L,
                         total_diffs = res$n_diffs,
                         report_saved = paste(saved, collapse = "; ")),
        error = function(e) { say("WARNING: could not update audit log: ", conditionMessage(e)); NA })
      if (!is.na(audit_path)) say("Audit log updated: ", audit_path)

      if (want_change_rtfs()) {
        change_files <- tryCatch(
          write_change_rtf_pair(f1, f2, res, root),
          error = function(e) { say("WARNING: could not generate RTF change tables: ",
                                    conditionMessage(e)); NULL })
        if (!is.null(change_files)) {
          say("RTF change tables saved:")
          for (p in change_files$output) say("  ", p)
        }
      }
    }
  }

  again <- tolower(clean_path(ask("\nCompare another pair? (y/N): ")))
  if (!identical(again, "y")) { say("\nDone."); break }
  say("")
}

if (!interactive()) quit(status = last_status)
