#!/usr/bin/env Rscript
# =============================================================================
# compare_rtf.R  --  RTF File Comparison Tool (content-level, not markup)
# =============================================================================
#
# Compares the *rendered content* of two RTF files and reports every
# difference at the cell level. It deliberately ignores cosmetic RTF markup
# (fonts, colours, column widths, spacing) so that two files that render the
# same table are reported as EQUIVALENT even when their raw bytes differ.
#
# Pipeline (three stages, each independently testable):
#   1. parse_rtf()       RTF  -> tall table (row_index, col_index, raw_value)
#   2. normalize_cells() apply per-cell normalisation -> norm_value
#   3. compare_tables()  full outer join on (row_index, col_index) -> classify
#
# This file can be (a) sourced as a library or (b) run from the command line.
# See the CLI section at the bottom, the README, or run with --help.
#
# Author : Lawlor Solutions
# License : MIT
# =============================================================================

# ----------------------------------------------------------------------------
# Dependency loading
# ----------------------------------------------------------------------------
# striprtf and data.table are required for the engine. optparse is required
# only for the command-line interface; diffdf only for the optional secondary
# cross-check. Each is loaded with a clear, actionable error message.

# Tool version, stamped into the audit log so a run can be traced to a build.
RTF_TOOL_VERSION <- "1.5.2"

.need_pkg <- function(pkg, why = "") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      "Required R package '%s' is not installed%s.\n  Install all dependencies by running install_packages.R (see README).",
      pkg, if (nzchar(why)) paste0(" (", why, ")") else ""),
      call. = FALSE)
  }
}

# ----------------------------------------------------------------------------
# Stage 1 -- parse
# ----------------------------------------------------------------------------
#' Parse an RTF file into a tall, one-record-per-cell table.
#'
#' Uses striprtf::read_rtf to decode the RTF (code page, \\'XX and \\uN escapes,
#' table structure) into a character vector: one element per non-table
#' paragraph (title / subtitle / footnote) and one element per table row whose
#' cells are separated by an ASCII Unit Separator (U+001F) sentinel.
#'
#' @param path Path to the RTF file.
#' @return data.table with columns row_index (int), col_index (int),
#'   raw_value (chr). Row/column indices are 1-based and positional.
parse_rtf <- function(path) {
  .need_pkg("striprtf")
  .need_pkg("data.table")
  if (!file.exists(path)) {
    stop(sprintf("File not found: '%s'", path), call. = FALSE)
  }
  # Reject non-RTF / empty input with a clear message (no stack dump).
  header <- tryCatch(readChar(path, 5L, useBytes = TRUE), error = function(e) "")
  if (length(header) == 0L || is.na(header)) header <- ""
  if (!grepl("^\\{\\\\rtf", header)) {
    stop(sprintf("Not a valid RTF file (missing '{\\rtf' header): '%s'", path),
         call. = FALSE)
  }

  CELL <- ""  # Unit Separator: an unlikely sentinel between table cells.

  lines <- tryCatch(
    striprtf::read_rtf(path, row_start = "", row_end = "", cell_end = CELL,
                       ignore_tables = FALSE),
    error = function(e)
      stop(sprintf("Failed to parse RTF '%s': %s", path, conditionMessage(e)),
           call. = FALSE)
  )

  # An empty document -> an empty (but well-formed) tall table.
  if (length(lines) == 0L) {
    return(data.table::data.table(
      row_index = integer(0), col_index = integer(0), raw_value = character(0)))
  }

  # Split each line into its cells. strsplit drops the single terminal empty
  # field produced by the trailing cell delimiter, so an N-cell table row
  # yields exactly N cells. A non-table paragraph (no delimiter) yields one
  # cell; an empty paragraph yields one empty cell -- both preserved so that
  # positional row indices stay aligned between the two files. Built with
  # vectorised operations (no per-row allocation) so large files stay fast.
  parts <- strsplit(lines, CELL, fixed = TRUE)
  parts <- lapply(parts, function(p) if (length(p) == 0L) "" else p)
  ncells <- lengths(parts)

  data.table::data.table(
    row_index = rep.int(seq_along(parts), ncells),
    col_index = sequence(ncells),
    raw_value = enc2utf8(unlist(parts, use.names = FALSE))
  )
}

# ----------------------------------------------------------------------------
# Stage 2 -- normalize
# ----------------------------------------------------------------------------
#' Add a normalised value column to a parsed tall table.
#'
#' Normalisation removes only cosmetic text differences; it never alters the
#' special characters (c, >=, deg, micro, Greek, accents) that are real
#' content. Unicode canonical normalisation is intentionally NOT applied
#' (comparison is exact by default).
#'
#' @param dt              Tall table from parse_rtf().
#' @param trim            Strip leading/trailing whitespace per cell (default ON).
#' @param collapse_space  Collapse internal whitespace runs to one space and
#'                        convert non-breaking spaces to normal spaces (default ON).
#' @param casefold        Upper-case for case-insensitive comparison (default OFF).
#' @return The input table with an added character column 'norm_value'.
normalize_cells <- function(dt, trim = TRUE, collapse_space = TRUE,
                            casefold = FALSE) {
  v <- dt$raw_value
  v <- gsub(" ", " ", v, fixed = TRUE)          # NBSP -> normal space
  if (collapse_space) v <- gsub("\\s+", " ", v, perl = TRUE)
  if (trim)           v <- trimws(v)
  if (casefold)       v <- toupper(v)
  dt$norm_value <- v
  dt[]
}

# ----------------------------------------------------------------------------
# Stage 3 -- compare
# ----------------------------------------------------------------------------
#' Compare two normalised tall tables positionally.
#'
#' Performs a full outer join on (row_index, col_index) and classifies every
#' cell. Row-count and column-count mismatches are not special cases -- they
#' fall out naturally as CELL_ONLY_IN_FILEn entries.
#'
#' @param base,comp  Normalised tall tables (must contain norm_value).
#' @param num_tol    Numeric tolerance. 0 (default) = exact string comparison.
#'                   If > 0, two cells that both parse as numbers (ignoring
#'                   thousands ',' and a trailing '%') and differ by <= the
#'                   tolerance are counted as MATCH.
#' @param rel_tol    Interpret num_tol as a fraction of the larger magnitude.
#' @return list(equivalent, n_cells, n_diffs, diffs) where diffs is a
#'   data.table(row_index, col_index, status, value_file1, value_file2).
compare_tables <- function(base, comp, num_tol = 0, rel_tol = FALSE) {
  .need_pkg("data.table")
  row_index <- col_index <- v1 <- v2 <- norm_value <- status <- NULL  # R CMD check

  m <- merge(
    base[, list(row_index, col_index, v1 = norm_value)],
    comp[, list(row_index, col_index, v2 = norm_value)],
    by = c("row_index", "col_index"), all = TRUE
  )

  # Vectorised numeric-equality test (only consulted when num_tol > 0).
  num_equal <- function(a, b) {
    pa <- suppressWarnings(as.numeric(gsub("[%,]", "", a)))
    pb <- suppressWarnings(as.numeric(gsub("[%,]", "", b)))
    ok <- !is.na(pa) & !is.na(pb)
    d  <- abs(pa - pb)
    tol <- if (rel_tol) num_tol * pmax(abs(pa), abs(pb), na.rm = FALSE)
           else rep(num_tol, length(a))
    res <- rep(FALSE, length(a))
    res[ok] <- d[ok] <= tol[ok]
    res
  }

  m[, status := data.table::fcase(
      is.na(v1),                       "CELL_ONLY_IN_FILE2",
      is.na(v2),                       "CELL_ONLY_IN_FILE1",
      v1 == v2,                        "MATCH",
      num_tol > 0 & num_equal(v1, v2), "MATCH",
      default =                        "VALUE_DIFF"
  )]

  diffs <- m[status != "MATCH"][order(row_index, col_index)]
  data.table::setnames(diffs, c("v1", "v2"), c("value_file1", "value_file2"))
  data.table::setcolorder(diffs,
    c("row_index", "col_index", "status", "value_file1", "value_file2"))

  list(
    equivalent = nrow(diffs) == 0L,
    n_cells    = nrow(m),
    n_diffs    = nrow(diffs),
    diffs      = diffs
  )
}

# ----------------------------------------------------------------------------
# Optional secondary cross-check -- diffdf (the pharma-standard comparator)
# ----------------------------------------------------------------------------
#' Run diffdf as an independent corroboration of the primary comparison.
#'
#' The explicit join in compare_tables() is the source of truth; diffdf is a
#' trusted, independent second opinion that reviewers find reassuring. Failures
#' here never abort the run.
#'
#' @return Short character summary, or NULL if diffdf is unavailable/errors.
run_diffdf_check <- function(base, comp, file = NULL) {
  if (!requireNamespace("diffdf", quietly = TRUE)) {
    return("diffdf not installed - secondary check skipped.")
  }
  tryCatch({
    b <- as.data.frame(base[, c("row_index", "col_index", "norm_value")])
    c <- as.data.frame(comp[, c("row_index", "col_index", "norm_value")])
    res <- diffdf::diffdf(b, c, keys = c("row_index", "col_index"),
                          suppress_warnings = TRUE, file = file)
    if (diffdf::diffdf_has_issues(res))
      "diffdf: differences detected (see secondary report)."
    else
      "diffdf: no differences detected."
  }, error = function(e) paste0("diffdf cross-check could not run: ",
                                conditionMessage(e)))
}

# ----------------------------------------------------------------------------
# Reporting
# ----------------------------------------------------------------------------
.clip <- function(x, n = 38L) {
  x <- ifelse(is.na(x), "<absent>", x)
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1L), "…"), x)
}

.diff_value <- function(x) ifelse(is.na(x), "<absent>", as.character(x))

#' Format a difference table as aligned, fixed-width report lines.
#'
#' Shared by the single-file report (write_report) and the batch report
#' (write_batch_report) so the two stay byte-for-byte consistent. Padding is by
#' display width (not bytes) so columns line up even with multi-byte characters.
#'
#' @param d A diffs data.table (row_index, col_index, status, value_file1/2).
#' @param truncate_values Shorten long values for compact console display.
#' @return Character vector of report lines (empty if there are no differences).
.format_diff_detail <- function(d, truncate_values = FALSE) {
  if (is.null(d) || nrow(d) == 0L) return(character(0))
  padw <- function(x, n) {
    x <- as.character(x)
    paste0(x, strrep(" ", pmax(0L, n - nchar(x, type = "width"))))
  }
  show_value <- if (truncate_values) .clip else .diff_value
  line <- function(a, b, c, e, f)
    paste(padw(a, 5L), padw(b, 5L), padw(c, 20L), padw(e, 39L), f)
  c(
    line("ROW", "COL", "STATUS", "FILE 1 VALUE", "FILE 2 VALUE"),
    vapply(seq_len(nrow(d)), function(i) line(
      d$row_index[i], d$col_index[i], d$status[i],
      show_value(d$value_file1[i]), show_value(d$value_file2[i])), character(1))
  )
}

#' Print and optionally write the comparison report.
#'
#' Always prints a console summary (and the detailed difference table when
#' differences exist). Writes a plain-text report and/or CSV when paths given.
#'
#' @param result       Output of compare_tables().
#' @param file1,file2  Paths shown in the report header.
#' @param txt_path     Optional path for a plain-text report.
#' @param csv_path     Optional path for a machine-readable CSV.
#' @param options_str  Human-readable options string for the header.
#' @param console      Print to the console (default TRUE).
#' @return The text report (character vector), invisibly.
write_report <- function(result, file1, file2, txt_path = NULL, csv_path = NULL,
                         options_str = "", console = TRUE) {
  cells_str <- if (is.na(result$n_cells)) "(skipped - files are byte-identical)"
               else format(result$n_cells, big.mark = ",")
  result_str <- if (result$equivalent) "EQUIVALENT"
                else sprintf("%s DIFFERENCE(S) FOUND",
                             format(result$n_diffs, big.mark = ","))

  hdr <- c(
    "============================================================",
    "RTF COMPARISON REPORT",
    "============================================================",
    paste0("File 1:  ", file1),
    paste0("File 2:  ", file2),
    paste0("Options: ", options_str),
    paste0("Cells compared: ", cells_str),
    paste0("Result:  ", result_str),
    "------------------------------------------------------------"
  )

  detail <- character(0)
  if (!result$equivalent && result$n_diffs > 0L) {
    detail <- .format_diff_detail(result$diffs)
  }

  txt <- c(hdr, detail, "")
  if (console) cat(txt, sep = "\n")

  if (!is.null(txt_path)) {
    writeLines(enc2utf8(txt), txt_path, useBytes = TRUE)
  }
  if (!is.null(csv_path)) {
    .need_pkg("data.table")
    data.table::fwrite(result$diffs, csv_path)
  }
  invisible(txt)
}

# ----------------------------------------------------------------------------
# Robustness helper -- drop fully-empty trailing rows
# ----------------------------------------------------------------------------
# Some RTF generators emit one or more blank rows at the very end of a table.
# If one file has them and the other does not, a naive positional compare would
# report spurious CELL_ONLY differences. Trimming trailing all-empty rows from
# each file (after normalisation) prevents that. Empty rows in the middle of a
# document are kept (dropping them would misalign positional indices).
.drop_trailing_empty_rows <- function(dt) {
  if (nrow(dt) == 0L) return(dt)
  row_index <- norm_value <- NULL
  emp <- dt[, list(empty = all(norm_value == "")), by = row_index][order(row_index)]
  k <- nrow(emp)
  while (k >= 1L && isTRUE(emp$empty[k])) k <- k - 1L
  if (k == 0L) return(dt[0L])
  dt[row_index <= emp$row_index[k]]
}

# ----------------------------------------------------------------------------
# Top-level wrapper
# ----------------------------------------------------------------------------
#' Compare two RTF files end-to-end: parse + normalize + compare + report.
#'
#' @param file1,file2     Paths to the two RTF files.
#' @param trim,collapse_space,casefold  Normalisation options (see normalize_cells).
#' @param num_tol,rel_tol Numeric-tolerance options (see compare_tables).
#' @param txt_path,csv_path  Optional output paths.
#' @param run_diffdf      Also run the diffdf secondary cross-check (default FALSE).
#' @param diffdf_path     Optional path for the diffdf report file.
#' @param console         Print the report to the console (default TRUE).
#' @return list from compare_tables(), invisibly, plus $byte_identical and
#'   $diffdf_summary.
compare_rtf <- function(file1, file2,
                        trim = TRUE, collapse_space = TRUE, casefold = FALSE,
                        num_tol = 0, rel_tol = FALSE,
                        txt_path = NULL, csv_path = NULL,
                        run_diffdf = FALSE, diffdf_path = NULL,
                        console = TRUE) {
  for (f in c(file1, file2)) {
    if (!file.exists(f)) stop(sprintf("File not found: '%s'", f), call. = FALSE)
  }

  options_str <- sprintf(
    "num_tol=%s, rel_tol=%s, trim=%s, collapse_space=%s, casefold=%s",
    num_tol, rel_tol, trim, collapse_space, casefold)

  # Fast path: byte-identical files are trivially equivalent (no parse needed).
  byte_identical <- FALSE
  same_md5 <- tryCatch(
    unname(tools::md5sum(file1) == tools::md5sum(file2)),
    error = function(e) FALSE)
  if (isTRUE(same_md5)) {
    byte_identical <- TRUE
    result <- list(
      equivalent = TRUE, n_cells = NA_integer_, n_diffs = 0L,
      diffs = data.table::data.table(
        row_index = integer(0), col_index = integer(0), status = character(0),
        value_file1 = character(0), value_file2 = character(0)))
    diffdf_summary <- "(skipped - files are byte-identical)"
  } else {
    b  <- .drop_trailing_empty_rows(
             normalize_cells(parse_rtf(file1), trim, collapse_space, casefold))
    cc <- .drop_trailing_empty_rows(
             normalize_cells(parse_rtf(file2), trim, collapse_space, casefold))
    result <- compare_tables(b, cc, num_tol = num_tol, rel_tol = rel_tol)
    diffdf_summary <- if (run_diffdf) run_diffdf_check(b, cc, file = diffdf_path)
                      else NULL
  }

  write_report(result, file1, file2, txt_path, csv_path, options_str,
               console = console)
  if (console && !is.null(diffdf_summary)) {
    cat("Secondary check: ", diffdf_summary, "\n", sep = "")
  }

  result$byte_identical <- byte_identical
  result$diffdf_summary <- diffdf_summary
  # Retain the compared paths for the optional change-table writer.  This does
  # not alter the comparison evidence; it lets write_change_rtf() rebuild a
  # paired semantic model even when called with its historical three-argument
  # interface.
  result$file1 <- normalizePath(file1, mustWork = FALSE)
  result$file2 <- normalizePath(file2, mustWork = FALSE)
  invisible(result)
}

# ============================================================================
# Change-table RTF output
# ============================================================================
# The change-table writer builds one paired semantic model, then renders it
# through each source document's RTF row/paragraph templates.  The comparison
# engine above remains positional by design; semantic alignment is confined to
# this optional artifact.

.read_rtf_text <- function(path) {
  size <- file.info(path)$size
  if (is.na(size) || size < 1L) stop(sprintf("Cannot read empty RTF: '%s'", path), call. = FALSE)
  con <- file(path, open = "rb")
  on.exit(close(con))
  readChar(con, nchars = size, useBytes = TRUE)
}

.write_rtf_text <- function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeChar(text, con, eos = NULL, useBytes = TRUE)
  invisible(normalizePath(path, mustWork = FALSE))
}

# Validate the RTF container with a byte-level scan before asking striprtf to
# interpret its contents.  Counting "{" and "}" characters is insufficient:
# escaped braces and \binN payloads are legal, while content after the root
# group is not.  Word rejects those malformed containers even when a lenient
# text extractor can still display their text.
.validate_rtf_container <- function(text, path = "generated RTF") {
  b <- as.integer(charToRaw(text)); n <- length(b)
  whitespace <- c(9L, 10L, 13L, 32L)
  first <- which(!b %in% whitespace)[1]
  if (is.na(first) || first + 4L > n ||
      !identical(b[first:(first + 4L)], c(123L, 92L, 114L, 116L, 102L))) {
    stop(sprintf("Invalid RTF container in '%s' (missing root {\\rtf group).", path),
         call. = FALSE)
  }

  depth <- 0L; root_seen <- FALSE; root_closed <- FALSE; i <- 1L
  is_alpha <- function(x) (x >= 65L && x <= 90L) || (x >= 97L && x <= 122L)
  is_digit <- function(x) x >= 48L && x <= 57L
  while (i <= n) {
    byte <- b[i]
    if (root_closed) {
      if (!byte %in% whitespace)
        stop(sprintf("Invalid RTF container in '%s' (content follows the root group).", path),
             call. = FALSE)
      i <- i + 1L; next
    }
    if (byte == 123L) { # {
      if (depth == 0L) {
        if (root_seen)
          stop(sprintf("Invalid RTF container in '%s' (multiple root groups).", path),
               call. = FALSE)
        root_seen <- TRUE
      }
      depth <- depth + 1L; i <- i + 1L; next
    }
    if (byte == 125L) { # }
      if (depth <= 0L)
        stop(sprintf("Invalid RTF container in '%s' (unexpected closing brace).", path),
             call. = FALSE)
      depth <- depth - 1L
      if (depth == 0L) root_closed <- TRUE
      i <- i + 1L; next
    }
    if (byte == 92L) { # backslash: control word or escaped symbol
      if (i == n)
        stop(sprintf("Invalid RTF container in '%s' (trailing backslash).", path),
             call. = FALSE)
      j <- i + 1L
      if (is_alpha(b[j])) {
        start_word <- j
        while (j <= n && is_alpha(b[j])) j <- j + 1L
        word <- rawToChar(as.raw(b[start_word:(j - 1L)]))
        sign <- 1L
        if (j <= n && b[j] == 45L) { sign <- -1L; j <- j + 1L }
        start_num <- j
        while (j <= n && is_digit(b[j])) j <- j + 1L
        has_num <- j > start_num
        param <- if (has_num) sign * as.integer(rawToChar(as.raw(b[start_num:(j - 1L)]))) else NA_integer_
        if (j <= n && b[j] == 32L) j <- j + 1L
        if (identical(tolower(word), "bin")) {
          if (is.na(param) || param < 0L || j + param - 1L > n)
            stop(sprintf("Invalid RTF container in '%s' (invalid \\bin payload).", path),
                 call. = FALSE)
          j <- j + param
        }
        i <- j; next
      }
      # A control symbol consumes the following byte (including \{, \}, \\).
      i <- i + 2L; next
    }
    if (depth == 0L && !byte %in% whitespace)
      stop(sprintf("Invalid RTF container in '%s' (content outside the root group).", path),
           call. = FALSE)
    if (byte == 0L)
      stop(sprintf("Invalid RTF container in '%s' (NUL byte outside a \\bin payload).", path),
           call. = FALSE)
    i <- i + 1L
  }
  if (!root_seen || !root_closed || depth != 0L)
    stop(sprintf("Invalid RTF container in '%s' (unterminated root group).", path),
         call. = FALSE)
  invisible(TRUE)
}

# Minimal, deliberately conservative RTF tokenizer.  It is used only to map
# table rows/cells and trailing paragraphs to raw spans.  Unsupported mappings
# fail before any destination is replaced.
.rtf_tokens <- function(text) {
  pattern <- "\\\\(?:'[0-9A-Fa-f]{2}|[A-Za-z]+-?[0-9]* ?|.)|[{}]|[^{}\\\\]+"
  at <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (length(at) == 1L && at[1] == -1L)
    return(data.frame(type = character(), start = integer(), end = integer(),
                      control = character(), param = character()))
  len <- attr(at, "match.length"); raw <- substring(text, at, at + len - 1L)
  type <- ifelse(raw == "{", "group_start", ifelse(raw == "}", "group_end",
                 ifelse(startsWith(raw, "\\"), "control", "text")))
  control <- param <- rep("", length(raw)); ci <- which(type == "control")
  if (length(ci)) for (i in ci) {
    z <- raw[i]
    if (grepl("^\\\\'[0-9A-Fa-f]{2}$", z)) {
      control[i] <- "'"; param[i] <- substring(z, 3L, 4L)
    } else if (grepl("^\\\\[A-Za-z]", z)) {
      control[i] <- sub("^\\\\([A-Za-z]+).*$", "\\1", z)
      if (grepl("^\\\\[A-Za-z]+-?[0-9]", z))
        param[i] <- sub("^\\\\[A-Za-z]+(-?[0-9]+) ?$", "\\1", z)
    } else {
      if (nchar(z) != 2L) stop("Invalid RTF control symbol.", call. = FALSE)
      control[i] <- substring(z, 2L, 2L)
    }
  }
  data.frame(type = type, start = as.integer(at), end = as.integer(at + len - 1L),
             control = control, param = param, stringsAsFactors = FALSE)
}

.rtf_hex_char <- function(hex) {
  z <- tryCatch(iconv(rawToChar(as.raw(strtoi(hex, 16L))), from = "CP1252",
                      to = "UTF-8"), error = function(e) NA_character_)
  if (is.na(z)) stop(sprintf("Unsupported RTF hexadecimal byte: %s", hex), call. = FALSE)
  z
}

.rtf_destination_names <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      # Use the same destination vocabulary as the authoritative striprtf
      # parser. The fallback covers the common hidden groups if that package
      # ever stops exposing its internal vocabulary.
      cached <<- tryCatch(
        get(".destinations", envir = asNamespace("striprtf"), inherits = FALSE),
        error = function(e) c(
          "bkmkend", "bkmkstart", "colortbl", "field", "filetbl", "fonttbl",
          "footer", "footnote", "header", "info", "listtable", "pict",
          "stylesheet", "tc", "upr", "userprops"
        )
      )
    }
    cached
  }
})

.rtf_decode_span <- function(text) {
  tok <- .rtf_tokens(text)
  out <- character(); visible <- logical(nrow(tok)); fallback <- 0L
  destinations <- .rtf_destination_names()
  # RTF properties are scoped by braces. A destination group is non-rendered
  # metadata (for example {\*\bkmkstart ...}); its plain text must never be
  # mistaken for displayed cell content or removed during replacement.
  stack <- list(list(skip = FALSE, uc = 1L, leading = TRUE))
  for (i in seq_len(nrow(tok))) {
    if (tok$type[i] == "group_start") {
      parent <- stack[[length(stack)]]
      stack[[length(stack) + 1L]] <- list(skip = parent$skip, uc = parent$uc,
                                         leading = TRUE)
      next
    }
    if (tok$type[i] == "group_end") {
      if (length(stack) > 1L) stack <- stack[-length(stack)]
      next
    }
    state_index <- length(stack); state <- stack[[state_index]]
    piece <- ""
    if (tok$type[i] == "text") {
      piece <- substr(text, tok$start[i], tok$end[i])
      piece <- gsub("[\r\n]", "", piece)
      if (nzchar(piece)) {
        state$leading <- FALSE; stack[[state_index]] <- state
      }
      if (state$skip) next
      if (fallback > 0L && nzchar(piece)) {
        chars <- strsplit(piece, "", fixed = TRUE)[[1]]
        take <- min(fallback, length(chars)); fallback <- fallback - take
        piece <- paste0(chars[-seq_len(take)], collapse = "")
      }
    } else if (tok$type[i] == "control") {
      ctl <- tok$control[i]; prm <- tok$param[i]
      if (state$leading) {
        if (ctl == "*" || ctl %in% destinations) state$skip <- TRUE
        state$leading <- FALSE; stack[[state_index]] <- state
      }
      if (state$skip) next
      if (ctl == "uc" && nzchar(prm)) {
        state$uc <- suppressWarnings(as.integer(prm))
        if (is.na(state$uc) || state$uc < 0L) state$uc <- 1L
        stack[[state_index]] <- state
      } else if (ctl == "u" && nzchar(prm)) {
        cp <- suppressWarnings(as.integer(prm))
        if (is.na(cp)) stop("Invalid RTF Unicode escape.", call. = FALSE)
        if (cp < 0L) cp <- cp + 65536L
        piece <- intToUtf8(cp); fallback <- state$uc
      } else if (fallback > 0L && ctl %in% c("'", "~", "_", "-", "\\", "{", "}")) {
        fallback <- fallback - 1L
      } else if (ctl == "'") piece <- .rtf_hex_char(prm)
      else if (ctl %in% c("\\", "{", "}")) piece <- ctl
      else if (ctl == "~") piece <- "\u00a0"
      else if (ctl == "_") piece <- "\u2011"
      else {
        specials <- c(
          bullet = "\u2022", emdash = "\u2014", emspace = "\u2003",
          endash = "\u2013", enspace = "\u2002", ldblquote = "\u201c",
          line = "\n", lquote = "\u2018",
          qmspace = "\u2005", rdblquote = "\u201d", rquote = "\u2019",
          tab = "\t"
        )
        if (ctl %in% names(specials)) piece <- unname(specials[[ctl]])
      }
    }
    if (nzchar(piece)) {
      out <- c(out, piece); visible[i] <- TRUE
    }
  }
  list(text = paste0(out, collapse = ""), tokens = tok, visible = visible)
}

.rtf_strip_row_values <- function(raw, source_text = "") {
  .need_pkg("striprtf")
  CELL <- ""
  charset <- regmatches(source_text,
    regexpr("\\\\(?:ansi|mac|pc|pca)(?![A-Za-z])", source_text, perl = TRUE))
  codepage <- regmatches(source_text,
    regexpr("\\\\ansicpg[0-9]+", source_text, perl = TRUE))
  uc <- regmatches(source_text,
    regexpr("\\\\uc[0-9]+", source_text, perl = TRUE))
  if (!length(charset) || !nzchar(charset)) charset <- "\\ansi"
  if (!length(codepage)) codepage <- ""
  if (!length(uc)) uc <- "\\uc1"
  wrapped <- paste0("{\\rtf1", charset, codepage, uc, " ", raw, "}")
  parsed <- tryCatch(
    striprtf::strip_rtf(wrapped, row_start = "", row_end = "", cell_end = CELL,
                        ignore_tables = FALSE),
    error = function(e) character()
  )
  if (!length(parsed)) return(character())
  value <- paste0(parsed, collapse = "")
  strsplit(value, CELL, fixed = TRUE)[[1]]
}

.rtf_escape_insert <- function(x) {
  chars <- strsplit(enc2utf8(x), "", fixed = TRUE)[[1]]
  if (!length(chars)) return("")
  paste0(vapply(chars, function(ch) {
    cp <- utf8ToInt(ch)
    if (ch == "\\") return("\\\\")
    if (ch == "{") return("\\{")
    if (ch == "}") return("\\}")
    if (ch == "\n") return("\\line ")
    if (ch == "\t") return("\\tab ")
    if (cp >= 32L && cp <= 126L) return(ch)
    if (cp <= 65535L) {
      if (cp > 32767L) cp <- cp - 65536L
      return(sprintf("\\u%d?", cp))
    }
    cp <- cp - 65536L
    hi <- 55296L + bitwShiftR(cp, 10L); lo <- 56320L + bitwAnd(cp, 1023L)
    if (hi > 32767L) hi <- hi - 65536L
    if (lo > 32767L) lo <- lo - 65536L
    sprintf("\\u%d?\\u%d?", hi, lo)
  }, character(1)), collapse = "")
}

.rtf_replace_visible <- function(span, value) {
  dec <- .rtf_decode_span(span)
  idx <- which(dec$visible)
  ins <- .rtf_escape_insert(value)
  if (!length(idx)) return(paste0(span, ins))
  ranges <- dec$tokens[idx, c("start", "end"), drop = FALSE]
  chunks <- character(); cursor <- 1L; inserted <- FALSE
  for (i in seq_len(nrow(ranges))) {
    st <- ranges$start[i]; en <- ranges$end[i]
    if (st > cursor) chunks <- c(chunks, substr(span, cursor, st - 1L))
    if (!inserted) { chunks <- c(chunks, ins); inserted <- TRUE }
    cursor <- max(cursor, en + 1L)
  }
  if (cursor <= nchar(span)) chunks <- c(chunks, substr(span, cursor, nchar(span)))
  paste0(chunks, collapse = "")
}

.rtf_row_parts <- function(raw, expected = NULL) {
  tok <- .rtf_tokens(raw)
  ci <- which(tok$type == "control" & tok$control == "cell")
  if (!is.null(expected) && length(ci) != expected)
    stop(sprintf("Unsafe RTF row mapping: expected %d cells, found %d.", expected, length(ci)),
         call. = FALSE)
  seg <- mark <- character(length(ci)); cursor <- 1L
  for (i in seq_along(ci)) {
    t <- ci[i]
    seg[i] <- if (tok$start[t] > cursor) substr(raw, cursor, tok$start[t] - 1L) else ""
    mark[i] <- substr(raw, tok$start[t], tok$end[t]); cursor <- tok$end[t] + 1L
  }
  suffix <- if (cursor <= nchar(raw)) substr(raw, cursor, nchar(raw)) else ""
  list(segments = seg, markers = mark, suffix = suffix,
       values = unname(vapply(seg, function(x) .rtf_decode_span(x)$text, character(1))))
}

.rtf_replace_row <- function(raw, values) {
  p <- .rtf_row_parts(raw, length(values))
  paste0(c(rbind(vapply(seq_along(values), function(i)
    .rtf_replace_visible(p$segments[i], values[i]), character(1)), p$markers),
    p$suffix), collapse = "")
}

.rtf_norm <- function(x) trimws(gsub("\\s+", " ", x, perl = TRUE))

.rtf_parse_change_document <- function(path) {
  text <- .read_rtf_text(path)
  .validate_rtf_container(text, path)
  layout <- .rtf_cell_layout(path); tok <- .rtf_tokens(text)
  unsupported <- c("nesttableprops", "nestrow", "clmgf", "clmrg", "clvmgf", "clvmrg")
  bad <- unique(tok$control[tok$type == "control" & tok$control %in% unsupported])
  if (length(bad))
    stop(sprintf("Merged or nested RTF tables are not supported for change output in '%s' (%s).",
                 path, paste(bad, collapse = ", ")), call. = FALSE)
  starts <- which(tok$type == "control" & tok$control == "trowd")
  ends <- which(tok$type == "control" & tok$control == "row")
  if (!length(starts) || !length(ends))
    stop(sprintf("No safely mappable RTF table found in '%s'.", path), call. = FALSE)
  pairs <- list(); used <- rep(FALSE, length(ends)); k <- 0L
  for (s in starts) {
    epos <- which(ends > s & !used)
    if (!length(epos)) stop(sprintf("Unterminated RTF row in '%s'.", path), call. = FALSE)
    e <- ends[epos[1]]; used[epos[1]] <- TRUE; k <- k + 1L
    pairs[[k]] <- c(tok$start[s], tok$end[e])
  }
  # Nested/multiple table structures are not generated speculatively.
  if (any(vapply(seq_along(pairs)[-1L], function(i) pairs[[i]][1] < pairs[[i-1L]][2], logical(1))))
    stop(sprintf("Nested RTF tables are not supported for change output: '%s'.", path), call. = FALSE)
  table_ids <- unique(layout[is_table == TRUE, row_index])
  if (length(table_ids) != length(pairs))
    stop(sprintf("RTF row mapping did not validate for '%s'.", path), call. = FALSE)
  rows <- vector("list", length(pairs))
  for (i in seq_along(pairs)) {
    raw <- substr(text, pairs[[i]][1], pairs[[i]][2])
    expected <- layout[is_table == TRUE & row_index == table_ids[i], raw_value]
    rp <- .rtf_row_parts(raw, length(expected))
    if (!identical(enc2utf8(rp$values), enc2utf8(expected))) {
      authoritative <- .rtf_strip_row_values(raw, text)
      if (!identical(enc2utf8(authoritative), enc2utf8(expected)))
        stop(sprintf(paste0("Displayed cell mapping did not validate for row %d of '%s' ",
                            "with either RTF decoder."), i, path), call. = FALSE)
    }
    gap <- if (i == 1L) "" else substr(text, pairs[[i - 1L]][2] + 1L,
                                        pairs[[i]][1] - 1L)
    rows[[i]] <- list(raw = raw, gap = gap, values = expected, row_index = table_ids[i])
  }
  sig <- vapply(rows, function(r) paste(.rtf_norm(r$values), collapse = "\u001e"), character(1))
  header_sig <- sig[1]; is_header <- sig == header_sig
  for (i in seq_along(rows)) rows[[i]]$is_header <- is_header[i]
  if (length(rows) > 1L) for (i in 2:length(rows)) {
    between <- .rtf_norm(.rtf_decode_span(rows[[i]]$gap)$text)
    if (nzchar(between) && !is_header[i])
      stop(sprintf("Multiple independent RTF tables are not supported for change output: '%s'.",
                   path), call. = FALSE)
  }

  last_end <- pairs[[length(pairs)]][2]
  tail <- if (last_end < nchar(text)) substr(text, last_end + 1L, nchar(text)) else ""
  tt <- .rtf_tokens(tail); pi <- which(tt$type == "control" & tt$control == "par")
  paras <- list(); cursor <- 1L
  if (length(pi)) for (j in seq_along(pi)) {
    raw <- substr(tail, cursor, tt$end[pi[j]])
    paras[[j]] <- list(raw = raw, value = .rtf_decode_span(raw)$text)
    cursor <- tt$end[pi[j]] + 1L
  }
  suffix <- if (cursor <= nchar(tail)) substr(tail, cursor, nchar(tail)) else ""
  parsed_tail <- layout[is_table == FALSE & row_index > max(table_ids), raw_value]
  if (!identical(enc2utf8(vapply(paras, `[[`, character(1), "value")), enc2utf8(parsed_tail)))
    stop(sprintf("Footnote paragraph mapping did not validate for '%s'.", path), call. = FALSE)

  list(path = path, text = text, rows = rows,
       prefix = substr(text, 1L, pairs[[1]][1] - 1L),
       row_gaps = NULL, tail_paras = paras, tail_suffix = suffix,
       ncol = length(rows[[1]]$values))
}

.change_rtf_name <- function(path) {
  nm <- basename(path)
  if (grepl("\\.rtf$", nm, ignore.case = TRUE))
    sub("(\\.[Rr][Tt][Ff])$", "_change\\1", nm, perl = TRUE)
  else paste0(nm, "_change.rtf")
}

.change_relative_path <- function(path) {
  d <- dirname(path); nm <- .change_rtf_name(path)
  if (identical(d, ".")) nm else file.path(d, nm)
}

.add_change_to_table_number <- function(text) {
  pattern <- paste0("(?i)(\\bTable[[:space:]]+)",
    "([0-9]+(?:\\.[0-9A-Za-z]+)*(?:-[0-9A-Za-z]+)*)(?![0-9A-Za-z.-]|_Change)")
  gsub(pattern, "\\1\\2_Change", text, perl = TRUE)
}

.format_change_number <- function(x, plus = TRUE) {
  if (!is.finite(x)) stop("Cannot format a non-finite change value.", call. = FALSE)
  if (abs(x) < .Machine$double.eps * 100) x <- 0
  if (x == 0) return("0")
  y <- signif(abs(x), 2L)
  decimals <- max(0L, 1L - floor(log10(y)))
  s <- sprintf(paste0("%.", decimals, "f"), y)
  if (grepl(".", s, fixed = TRUE)) {
    s <- sub("0+$", "", s)
    s <- sub("\\.$", "", s)
  }
  paste0(if (x < 0) "-" else if (plus) "+" else "", s)
}

.parse_count_percent <- function(x) {
  pat <- paste0("^\\s*([+-]?(?:[0-9]{1,3}(?:,[0-9]{3})*|[0-9]+)(?:\\.[0-9]+)?)",
                "\\s*\\(\\s*([+-]?(?:[0-9]{1,3}(?:,[0-9]{3})*|[0-9]+)(?:\\.[0-9]+)?)",
                "\\s*%\\s*\\)\\s*$")
  m <- regexec(pat, x, perl = TRUE); g <- regmatches(x, m)[[1]]
  if (length(g) != 3L) return(NULL)
  as.numeric(gsub(",", "", g[-1L], fixed = TRUE))
}

.parse_scalar <- function(x) {
  if (!grepl("^\\s*[+-]?(?:[0-9]{1,3}(?:,[0-9]{3})*|[0-9]+)(?:\\.[0-9]+)?\\s*$",
             x, perl = TRUE)) return(NULL)
  as.numeric(gsub(",", "", trimws(x), fixed = TRUE))
}

.change_cell_value <- function(set1, set2) {
  if (identical(.rtf_norm(set1), .rtf_norm(set2))) return("NC")
  a <- .parse_count_percent(set1); b <- .parse_count_percent(set2)
  if (!is.null(a) && !is.null(b)) {
    dc <- b[1] - a[1]
    if (dc == 0) return("NC")
    return(sprintf("(%s, %s%%)", .format_change_number(dc),
                   .format_change_number(b[2] - a[2])))
  }
  a <- .parse_scalar(set1); b <- .parse_scalar(set2)
  if (!is.null(a) && !is.null(b)) {
    d <- b - a
    return(if (d == 0) "NC" else .format_change_number(d))
  }
  "CHG"
}

.change_body_rows <- function(doc) {
  body <- doc$rows[!vapply(doc$rows, `[[`, logical(1), "is_header")]
  if (!length(body)) return(body)
  desc <- vapply(body, function(r) r$values[1], character(1))
  nonindent <- nzchar(trimws(desc)) & !grepl("^[[:space:]]", desc)
  next_indented <- vapply(seq_along(body), function(i) {
    if (i == length(body)) return(FALSE)
    j <- i + 1L
    while (j <= length(body) && all(.rtf_norm(body[[j]]$values) == "")) j <- j + 1L
    j <= length(body) && grepl("^[[:space:]]+\\S", body[[j]]$values[1], perl = TRUE)
  }, logical(1))
  section <- ""; seen <- new.env(parent = emptyenv())
  for (i in seq_along(body)) {
    vals <- body[[i]]$values
    blank <- all(.rtf_norm(vals) == "")
    is_section <- !blank && (all(.rtf_norm(vals[-1L]) == "") ||
                             (nonindent[i] && next_indented[i]))
    if (is_section) section <- vals[1]
    base <- paste(section, vals[1], sep = "\u001f")
    if (blank) base <- paste0(section, "\u001f<BLANK>")
    count <- if (exists(base, envir = seen, inherits = FALSE)) get(base, seen) + 1L else 1L
    assign(base, count, envir = seen)
    body[[i]]$key <- paste(base, count, sep = "\u001f")
    body[[i]]$blank <- blank; body[[i]]$section <- is_section
  }
  body
}

.align_change_rows <- function(a, b) {
  ka <- vapply(a, `[[`, character(1), "key"); kb <- vapply(b, `[[`, character(1), "key")
  n <- length(ka); m <- length(kb); dp <- matrix(0L, n + 1L, m + 1L)
  if (n && m) for (i in n:1L) for (j in m:1L)
    dp[i, j] <- if (identical(ka[i], kb[j])) 1L + dp[i + 1L, j + 1L]
                else max(dp[i + 1L, j], dp[i, j + 1L])
  out <- list(); z <- 0L; i <- 1L; j <- 1L
  while (i <= n || j <= m) {
    z <- z + 1L
    if (i <= n && j <= m && identical(ka[i], kb[j])) {
      out[[z]] <- list(i1 = i, i2 = j); i <- i + 1L; j <- j + 1L
    } else if (i <= n && (j > m || dp[i + 1L, j] >= dp[i, j + 1L])) {
      out[[z]] <- list(i1 = i, i2 = NA_integer_); i <- i + 1L
    } else {
      out[[z]] <- list(i1 = NA_integer_, i2 = j); j <- j + 1L
    }
  }
  out
}

.cosmetic_footnote <- function(x) {
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("[[:space:]]+", " ", trimws(x), perl = TRUE)
  gsub("[[:space:]]*([[:punct:]])[[:space:]]*", "\\1", x, perl = TRUE)
}

.foot_tokens <- function(x) {
  m <- gregexpr("[[:alnum:]]+|[^[:alnum:][:space:]]", x, perl = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L)
    return(data.frame(value = character(), start = integer(), end = integer()))
  len <- attr(m, "match.length")
  data.frame(value = substring(x, m, m + len - 1L), start = m, end = m + len - 1L,
             stringsAsFactors = FALSE)
}

.sequence_ops <- function(a, b, sub_cost = NULL) {
  n <- length(a); m <- length(b); d <- matrix(0, n + 1L, m + 1L)
  if (n) d[2:(n + 1L), 1] <- seq_len(n)
  if (m) d[1, 2:(m + 1L)] <- seq_len(m)
  for (i in seq_len(n)) for (j in seq_len(m)) {
    sc <- if (identical(a[i], b[j])) 0 else if (is.null(sub_cost)) 1 else sub_cost(a[i], b[j])
    d[i + 1L, j + 1L] <- min(d[i, j + 1L] + 1, d[i + 1L, j] + 1, d[i, j] + sc)
  }
  ops <- list(); i <- n; j <- m
  while (i > 0L || j > 0L) {
    if (i > 0L && j > 0L) {
      sc <- if (identical(a[i], b[j])) 0 else if (is.null(sub_cost)) 1 else sub_cost(a[i], b[j])
      if (abs(d[i + 1L, j + 1L] - (d[i, j] + sc)) < 1e-9) {
        ops <- c(list(list(type = if (sc == 0) "equal" else "sub", i = i, j = j)), ops)
        i <- i - 1L; j <- j - 1L; next
      }
    }
    if (j > 0L && abs(d[i + 1L, j + 1L] - (d[i + 1L, j] + 1)) < 1e-9) {
      ops <- c(list(list(type = "add", i = NA_integer_, j = j)), ops); j <- j - 1L
    } else {
      ops <- c(list(list(type = "del", i = i, j = NA_integer_)), ops); i <- i - 1L
    }
  }
  ops
}

.mark_token_substitution <- function(old, new) {
  ao <- strsplit(old, "", fixed = TRUE)[[1]]; bn <- strsplit(new, "", fixed = TRUE)[[1]]
  if (length(ao) != length(bn) ||
      !(identical(tolower(old), tolower(new)) ||
        (grepl("^[0-9]+$", old) && grepl("^[0-9]+$", new)))) return(rep(TRUE, nchar(new)))
  ao != bn
}

.annotate_footnote <- function(old, new) {
  if (identical(.cosmetic_footnote(old), .cosmetic_footnote(new))) return(new)
  a <- .foot_tokens(old); b <- .foot_tokens(new)
  ops <- .sequence_ops(a$value, b$value)
  flags <- rep(FALSE, nchar(new)); inserts <- list()
  for (op_index in seq_along(ops)) {
    op <- ops[[op_index]]
    if (op$type == "add") flags[b$start[op$j]:b$end[op$j]] <- TRUE
    else if (op$type == "sub") {
      f <- .mark_token_substitution(a$value[op$i], b$value[op$j])
      flags[b$start[op$j]:b$end[op$j]] <- f
    } else if (op$type == "del") {
      nextj <- vapply(ops, function(q) if (!is.na(q$j)) q$j else Inf, numeric(1))
      pos <- if (any(nextj > 0 & is.finite(nextj))) {
        candidates <- nextj[seq_along(nextj) > op_index & is.finite(nextj)]
        if (length(candidates)) b$start[min(candidates)] else nchar(new) + 1L
      } else nchar(new) + 1L
      inserts[[as.character(pos)]] <- paste0(inserts[[as.character(pos)]], "(missing)")
    }
  }
  # Whitespace between adjacent changed lexical tokens belongs to one bracket.
  if (length(flags) && any(flags)) {
    idx <- which(flags)
    for (i in seq_len(length(idx) - 1L)) if (idx[i + 1L] > idx[i] + 1L) {
      between <- substring(new, idx[i] + 1L, idx[i + 1L] - 1L)
      if (grepl("^[[:space:]]+$", between)) flags[(idx[i] + 1L):(idx[i + 1L] - 1L)] <- TRUE
    }
  }
  ch <- strsplit(new, "", fixed = TRUE)[[1]]; out <- character(); in_bracket <- FALSE
  for (p in seq_len(length(ch) + 1L)) {
    key <- as.character(p)
    if (!is.null(inserts[[key]])) out <- c(out, inserts[[key]])
    if (p > length(ch)) break
    if (flags[p] && !in_bracket) { out <- c(out, "("); in_bracket <- TRUE }
    if (!flags[p] && in_bracket) { out <- c(out, ")"); in_bracket <- FALSE }
    out <- c(out, ch[p])
  }
  if (in_bracket) out <- c(out, ")")
  paste0(out, collapse = "")
}

.align_footnotes <- function(f1, f2) {
  a <- .cosmetic_footnote(f1); b <- .cosmetic_footnote(f2)
  sim_cost <- function(x, y) {
    den <- max(nchar(x), nchar(y), 1L)
    min(1.5, as.numeric(adist(x, y)) / den * 2)
  }
  ops <- .sequence_ops(a, b, sim_cost)
  out <- character(); changed <- FALSE
  for (op in ops) {
    if (op$type == "equal") out <- c(out, f2[op$j])
    else if (op$type == "sub") {
      val <- .annotate_footnote(f1[op$i], f2[op$j])
      out <- c(out, val); changed <- changed || !identical(val, f2[op$j])
    } else if (op$type == "add") { out <- c(out, paste0("(", f2[op$j], ")")); changed <- TRUE }
    else { out <- c(out, "(missing)"); changed <- TRUE }
  }
  list(values = out, changed = changed)
}

.build_change_model <- function(file1, file2) {
  d1 <- .rtf_parse_change_document(file1); d2 <- .rtf_parse_change_document(file2)
  if (d1$ncol != d2$ncol)
    stop("Change RTF generation requires the same number of table columns.", call. = FALSE)
  b1 <- .change_body_rows(d1); b2 <- .change_body_rows(d2)
  aligned <- .align_change_rows(b1, b2); rows <- list(); statuses <- list()
  for (k in seq_along(aligned)) {
    z <- aligned[[k]]; only1 <- is.na(z$i2); only2 <- is.na(z$i1)
    if (only1 || only2) {
      src <- if (only1) b1[[z$i1]] else b2[[z$i2]]
      tag <- if (only1) "ONLY IN SET 1" else "ONLY IN SET 2"
      vals <- c(paste0(src$values[1], " (", tag, ")"), rep(tag, d1$ncol - 1L))
      stat <- rep(tag, d1$ncol)
    } else {
      r1 <- b1[[z$i1]]; r2 <- b2[[z$i2]]
      if (isTRUE(r1$blank) && isTRUE(r2$blank)) {
        vals <- rep("", d1$ncol); stat <- rep("NC", d1$ncol)
      } else {
        vals <- character(d1$ncol); stat <- rep("NC", d1$ncol)
        vals[1] <- r2$values[1]
        if (d1$ncol > 1L) for (cc in 2:d1$ncol) {
          vals[cc] <- .change_cell_value(r1$values[cc], r2$values[cc]); stat[cc] <- vals[cc]
        }
      }
    }
    rows[[k]] <- list(values = vals, status = stat, i1 = z$i1, i2 = z$i2,
                      blank = all(vals == ""))
    statuses[[k]] <- stat
  }
  included <- !vapply(rows, `[[`, logical(1), "blank")
  sm <- if (any(included)) do.call(rbind, statuses[included]) else matrix("NC", 1L, d1$ncol)
  labels <- ifelse(vapply(seq_len(d1$ncol), function(j) all(sm[, j] == "NC"), logical(1)),
                   "NC", "Change")
  p1 <- vapply(d1$tail_paras, `[[`, character(1), "value")
  p2 <- vapply(d2$tail_paras, `[[`, character(1), "value")
  foot1 <- p1[nzchar(.rtf_norm(p1))]; foot2 <- p2[nzchar(.rtf_norm(p2))]
  feet <- .align_footnotes(foot1, foot2)
  list(doc1 = d1, doc2 = d2, body1 = b1, body2 = b2, rows = rows,
       header_labels = labels, footnotes = feet$values, footnote_changed = feet$changed)
}

.nearest_template_row <- function(model_row, side, body) {
  idx <- if (side == 1L) model_row$i1 else model_row$i2
  if (!is.na(idx)) return(body[[idx]]$raw)
  avail <- vapply(body, function(x) length(x$values) > 0L, logical(1))
  if (!any(avail)) stop("No compatible body-row template is available.", call. = FALSE)
  body[[which(avail)[1]]]$raw
}

.render_change_document <- function(model, side) {
  doc <- if (side == 1L) model$doc1 else model$doc2
  body <- if (side == 1L) model$body1 else model$body2
  headers <- doc$rows[vapply(doc$rows, `[[`, logical(1), "is_header")]
  header_values <- lapply(headers, function(h)
    paste0(h$values, "\n", model$header_labels))
  first_header <- .rtf_replace_row(headers[[1]]$raw, header_values[[1]])
  # Preserve repeated-page header frequency from this template.
  stream <- doc$rows; seen_body <- 0L; repeat_at <- integer()
  for (i in seq_along(stream)) {
    if (isTRUE(stream[[i]]$is_header)) {
      if (i != 1L) repeat_at <- c(repeat_at, seen_body)
    } else seen_body <- seen_body + 1L
  }
  chunks <- c(.add_change_to_table_number(doc$prefix), first_header)
  hidx <- 2L
  for (i in seq_along(model$rows)) {
    while (hidx <= length(headers) && (i - 1L) >= repeat_at[hidx - 1L]) {
      chunks <- c(chunks, "\n", .add_change_to_table_number(headers[[hidx]]$gap),
                  .rtf_replace_row(headers[[hidx]]$raw,
                                                   header_values[[hidx]]))
      hidx <- hidx + 1L
    }
    tmpl <- .nearest_template_row(model$rows[[i]], side, body)
    chunks <- c(chunks, "\n", .rtf_replace_row(tmpl, model$rows[[i]]$values))
  }
  while (hidx <= length(headers)) {
    chunks <- c(chunks, "\n", .add_change_to_table_number(headers[[hidx]]$gap),
                .rtf_replace_row(headers[[hidx]]$raw, header_values[[hidx]]))
    hidx <- hidx + 1L
  }

  blank_templates <- doc$tail_paras[!nzchar(.rtf_norm(vapply(doc$tail_paras, `[[`, character(1), "value")))]
  foot_templates <- doc$tail_paras[nzchar(.rtf_norm(vapply(doc$tail_paras, `[[`, character(1), "value")))]
  if (length(blank_templates)) chunks <- c(chunks, "\n", blank_templates[[1]]$raw)
  values <- model$footnotes
  if (model$footnote_changed) values <- c("Footnote changes in brackets", values)
  for (i in seq_along(values)) {
    if (length(foot_templates)) tmpl <- foot_templates[[min(i, length(foot_templates))]]$raw
    else tmpl <- "\\pard\\plain \\par"
    chunks <- c(chunks, "\n", .rtf_replace_visible(tmpl, values[i]))
  }
  chunks <- c(chunks, doc$tail_suffix)
  paste0(chunks, collapse = "")
}

.validate_change_output <- function(path, model) {
  .validate_rtf_container(.read_rtf_text(path), path)
  p <- parse_rtf(path)
  source_has_table_number <- grepl(
    "(?i)\\bTable[[:space:]]+[0-9]+(?:\\.[0-9A-Za-z]+)*(?:-[0-9A-Za-z]+)*",
    paste0(model$doc1$prefix, model$doc2$prefix), perl = TRUE)
  if (source_has_table_number && !any(grepl("_Change", p$raw_value, fixed = TRUE)))
    stop("Generated RTF is missing the _Change table tag.", call. = FALSE)
  layout <- .rtf_cell_layout(path)
  ids <- unique(layout[is_table == TRUE, row_index])
  rows <- lapply(ids, function(id) layout[is_table == TRUE & row_index == id, raw_value])
  is_header <- vapply(rows, function(x)
    length(x) == length(model$header_labels) &&
      all(endsWith(x, paste0("\n", model$header_labels))), logical(1))
  if (!any(is_header)) stop("Generated RTF failed header status validation.", call. = FALSE)
  actual_body <- rows[!is_header]
  expected_body <- lapply(model$rows, `[[`, "values")
  if (length(actual_body) != length(expected_body) ||
      !all(vapply(seq_along(expected_body), function(i)
        identical(enc2utf8(actual_body[[i]]), enc2utf8(expected_body[[i]])), logical(1))))
    stop("Generated RTF failed exact aligned body validation.", call. = FALSE)
  expected_feet <- model$footnotes
  if (model$footnote_changed)
    expected_feet <- c("Footnote changes in brackets", expected_feet)
  if (length(expected_feet)) {
    pos <- match(expected_feet, p$raw_value)
    if (anyNA(pos) || is.unsorted(pos, strictly = TRUE))
      stop("Generated RTF failed annotated footnote validation.", call. = FALSE)
  }
  invisible(TRUE)
}

.write_change_atomic <- function(text, path, model) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = paste0(".", basename(path), "."), tmpdir = dirname(path), fileext = ".tmp")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  .write_rtf_text(text, tmp); .validate_change_output(tmp, model)
  backup <- ""
  if (file.exists(path)) {
    backup <- tempfile(pattern = paste0(".", basename(path), "."),
                       tmpdir = dirname(path), fileext = ".bak")
    if (!file.rename(path, backup))
      stop(sprintf("Could not preserve the existing RTF before updating '%s'.", path),
           call. = FALSE)
  }
  if (!file.rename(tmp, path)) {
    restored <- !nzchar(backup) || (file.exists(backup) && file.rename(backup, path))
    stop(sprintf("Could not install validated RTF '%s'%s.", path,
                 if (restored) "" else paste0("; the prior file remains at '", backup, "'")),
         call. = FALSE)
  }
  if (nzchar(backup) && file.exists(backup)) unlink(backup)
  invisible(normalizePath(path, mustWork = FALSE))
}

#' Write one side of a paired change RTF model (compatibility interface).
write_change_rtf <- function(source_path, result, output_path) {
  if (is.null(result$file1) || is.null(result$file2))
    stop("The comparison result does not identify both source RTF paths.", call. = FALSE)
  model <- .build_change_model(result$file1, result$file2)
  side <- if (identical(normalizePath(source_path, mustWork = FALSE), result$file2)) 2L else 1L
  .write_change_atomic(.render_change_document(model, side), output_path, model)
}

#' Write validated Set 1 and Set 2 change RTFs for one compared pair.
write_change_rtf_pair <- function(file1, file2, result, tool_root,
                                  relative1 = basename(file1), relative2 = basename(file2)) {
  model <- .build_change_model(file1, file2)
  base <- file.path(tool_root, "logs", "RTF Changes")
  out <- c(file.path(base, "Set 1", .change_relative_path(relative1)),
           file.path(base, "Set 2", .change_relative_path(relative2)))
  # Validate both temporary documents before either final path is installed.
  txt <- c(.render_change_document(model, 1L), .render_change_document(model, 2L))
  tmps <- character(2L)
  on.exit(unlink(tmps[file.exists(tmps)]), add = TRUE)
  for (i in 1:2) {
    dir.create(dirname(out[i]), recursive = TRUE, showWarnings = FALSE)
    tmps[i] <- tempfile(pattern = paste0(".", basename(out[i]), "."),
                        tmpdir = dirname(out[i]), fileext = ".tmp")
    .write_rtf_text(txt[i], tmps[i]); .validate_change_output(tmps[i], model)
  }

  # Preserve any prior pair before installing either replacement. If one
  # install fails (for example, because a file is open on Windows), restore
  # the entire prior pair instead of leaving a mixed or missing result set.
  backups <- rep("", 2L)
  for (i in 1:2) {
    if (file.exists(out[i])) {
      backups[i] <- tempfile(pattern = paste0(".", basename(out[i]), "."),
                             tmpdir = dirname(out[i]), fileext = ".bak")
      if (!file.rename(out[i], backups[i])) {
        prior <- which(nzchar(backups) & file.exists(backups))
        for (j in prior) file.rename(backups[j], out[j])
        stop(sprintf("Could not preserve the existing RTF pair before updating '%s'.", out[i]),
             call. = FALSE)
      }
    }
  }
  installed <- rep(FALSE, 2L)
  for (i in 1:2) {
    if (!file.rename(tmps[i], out[i])) {
      unlink(out[installed & file.exists(out)])
      restored <- logical(2L)
      for (j in 1:2) {
        restored[j] <- !nzchar(backups[j]) ||
          (file.exists(backups[j]) && file.rename(backups[j], out[j]))
      }
      stranded <- backups[nzchar(backups) & file.exists(backups)]
      detail <- if (all(restored)) "" else
        paste0(" Prior output backup(s) remain at: ", paste(stranded, collapse = "; "))
      stop(paste0("Could not install both validated change RTF outputs; the update was rolled back.",
                  detail), call. = FALSE)
    }
    installed[i] <- TRUE
  }
  unlink(backups[nzchar(backups) & file.exists(backups)])
  data.frame(set = c("Set 1", "Set 2"), source = c(file1, file2),
    output = normalizePath(out, mustWork = FALSE), ok = TRUE, error = "",
    stringsAsFactors = FALSE)
}
# Read the same displayed lines used by parse_rtf(), but retain explicit row
# markers so a one-cell table row can still be distinguished from a paragraph.
.rtf_cell_layout <- function(path) {
  .need_pkg("striprtf")
  .need_pkg("data.table")
  CELL <- ""
  ROW_START <- "<<<RTF_CHANGE_ROW_START>>>"
  ROW_END   <- "<<<RTF_CHANGE_ROW_END>>>"

  lines <- tryCatch(
    striprtf::read_rtf(path, row_start = ROW_START, row_end = ROW_END,
                       cell_end = CELL, ignore_tables = FALSE),
    error = function(e)
      stop(sprintf("Failed to map RTF table cells in '%s': %s", path, conditionMessage(e)),
           call. = FALSE)
  )
  if (length(lines) == 0L) {
    return(data.table::data.table(
      row_index = integer(), col_index = integer(), raw_value = character(),
      is_table = logical()))
  }

  is_table <- startsWith(lines, ROW_START) & endsWith(lines, ROW_END)
  body <- lines
  body[is_table] <- substring(
    body[is_table], nchar(ROW_START) + 1L,
    nchar(body[is_table]) - nchar(ROW_END))
  parts <- strsplit(body, CELL, fixed = TRUE)
  parts <- lapply(parts, function(p) if (length(p) == 0L) "" else p)
  ncells <- lengths(parts)

  layout <- data.table::data.table(
    row_index = rep.int(seq_along(parts), ncells),
    col_index = sequence(ncells),
    raw_value = enc2utf8(unlist(parts, use.names = FALSE)),
    is_table = rep.int(is_table, ncells)
  )

  # Guard the positional contract: adding row markers must not change the text
  # or indices returned by the comparison parser.
  parsed <- parse_rtf(path)
  if (nrow(layout) != nrow(parsed) ||
      !identical(layout$row_index, parsed$row_index) ||
      !identical(layout$col_index, parsed$col_index) ||
      !identical(layout$raw_value, parsed$raw_value)) {
    stop(sprintf("Could not safely map displayed cells back to source RTF: '%s'", path),
         call. = FALSE)
  }
  layout
}


# ============================================================================
# Batch comparison -- compare every RTF in one folder against the same-named
# file in a second folder.
# ============================================================================
#' Compare every RTF file in Set 1 against its corresponding file in Set 2.
#'
#' Lists the .rtf files in each folder, pairs exact normalized names first, and
#' then permits a conservative fuzzy-name match for a unique, highly similar
#' remaining pair. Every file is reported -- including the ones whose content
#' is EQUIVALENT -- so the run doubles as a QC record. Files that cannot be
#' paired confidently are flagged rather than silently skipped, and a parse
#' error on one file never aborts the batch.
#'
#' @param dir1,dir2  The Set 1 folder and the Set 2 folder.
#' @param trim,collapse_space,casefold,num_tol,rel_tol  Passed to compare_rtf().
#' @param recursive  Also descend into sub-folders (default FALSE).
#' @param progress   Print one line per file as it is compared (default = console).
#' @param console    Print a summary block when finished (default TRUE).
#' @return list(dir1, dir2, summary, results, totals, all_equivalent) where
#'   summary is a data.table(file, status, n_cells, n_diffs, note) with one row
#'   per file and results is a named list of the per-file compare_rtf() outputs
#'   (only for files that were actually compared).
.batch_file_key <- function(path) {
  path <- gsub("\\\\", "/", enc2utf8(path))
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  tolower(paste(parts, collapse = "/"))
}

.batch_file_index <- function(files, folder_label) {
  .need_pkg("data.table")
  keys <- vapply(files, .batch_file_key, character(1), USE.NAMES = FALSE)
  idx <- data.table::data.table(norm_key = keys, file = files)
  dup <- idx[, .N, by = norm_key][N > 1L]
  if (nrow(dup) > 0L) {
    bad <- idx[norm_key %in% dup$norm_key, paste(file, collapse = ", "), by = norm_key]
    stop(sprintf(
      "Ambiguous RTF file names in %s after case/spacing normalization: %s",
      folder_label, paste(bad$V1, collapse = "; ")),
      call. = FALSE)
  }
  out <- idx$file
  names(out) <- idx$norm_key
  out
}

.batch_fuzzy_parts <- function(key) {
  key <- gsub("\\\\", "/", key)
  dir <- dirname(key)
  if (identical(dir, ".")) dir <- ""
  base <- basename(key)
  stem <- sub("\\.[Rr][Tt][Ff]$", "", base)
  list(dir = dir, stem = stem)
}

.batch_match_text <- function(x) {
  x <- tolower(.rtf_norm(enc2utf8(x)))
  x <- gsub("\\btable[[:space:]]+[0-9][0-9a-z.-]*\\b", " ", x, perl = TRUE)
  x <- gsub("[^[:alnum:]]+", " ", x, perl = TRUE)
  trimws(gsub("[[:space:]]+", " ", x, perl = TRUE))
}

.batch_text_similarity <- function(a, b) {
  a <- .batch_match_text(a); b <- .batch_match_text(b)
  if (!nzchar(a) || !nzchar(b)) return(NA_real_)
  char_score <- 1 - as.numeric(adist(a, b)[1]) / max(nchar(a), nchar(b))
  tokens <- function(x) {
    z <- unique(strsplit(x, " ", fixed = TRUE)[[1]])
    setdiff(z, c("table", "summary", "listing", "figure", "of", "by",
                 "and", "the", "for", "a", "an", "population"))
  }
  ta <- tokens(a); tb <- tokens(b)
  token_score <- if (!length(ta) || !length(tb)) NA_real_ else
    2 * length(intersect(ta, tb)) / (length(ta) + length(tb))
  mean(c(char_score, token_score), na.rm = TRUE)
}

.batch_set_dice <- function(a, b) {
  a <- unique(.batch_match_text(a)); b <- unique(.batch_match_text(b))
  a <- a[nzchar(a)]; b <- b[nzchar(b)]
  if (!length(a) || !length(b)) return(NA_real_)
  2 * length(intersect(a, b)) / (length(a) + length(b))
}

# Build a small rendered-content profile for fuzzy filename validation. The
# title is the displayed text before the first table row. Column 1 excludes
# repeated copies of the first table row, which is the source header template.
.batch_content_profile <- function(path) {
  tryCatch({
    layout <- .rtf_cell_layout(path)
    table_rows <- sort(unique(layout[is_table == TRUE, row_index]))
    if (!length(table_rows)) stop("no displayed table rows")
    first_table <- table_rows[[1]]
    title <- layout[is_table == FALSE & row_index < first_table, raw_value]
    title <- paste(title[nzchar(.rtf_norm(title))], collapse = " ")

    row_signature <- function(id) {
      paste(.batch_match_text(layout[is_table == TRUE & row_index == id, raw_value]),
            collapse = "\u001e")
    }
    header_signature <- row_signature(first_table)
    body_rows <- table_rows[
      vapply(table_rows, row_signature, character(1)) != header_signature
    ]
    column1 <- layout[is_table == TRUE & row_index %in% body_rows & col_index == 1L,
                      raw_value]
    list(title = title, column1 = column1, error = "")
  }, error = function(e) list(title = "", column1 = character(),
                              error = conditionMessage(e)))
}

.batch_content_evidence <- function(a, b) {
  title <- .batch_text_similarity(a$title, b$title)
  column1 <- .batch_set_dice(a$column1, b$column1)
  available <- sum(!is.na(c(title, column1)))
  # A fuzzy name is never trusted without rendered-content evidence. Either a
  # reasonably similar heading or meaningful Column 1 overlap is sufficient;
  # when both are extremely different, the candidate is rejected.
  accepted <- available > 0L &&
    ((!is.na(title) && title >= 0.35) || (!is.na(column1) && column1 >= 0.20))
  list(title = title, column1 = column1, accepted = accepted)
}

.batch_zero_token_evidence <- function(stem1, stem2) {
  tokens <- function(x) {
    z <- strsplit(tolower(x), "0", fixed = TRUE)[[1]]
    setdiff(z[nzchar(z)], c("s", "ae"))
  }
  a <- tokens(stem1); b <- tokens(stem2)
  n <- length(a); m <- length(b)
  if (!n || !m) return(list(accepted = FALSE, similarity = 0, shared = 0L))
  dp <- matrix(0L, n + 1L, m + 1L)
  for (i in n:1L) for (j in m:1L)
    dp[i, j] <- if (identical(a[i], b[j])) 1L + dp[i + 1L, j + 1L]
                else max(dp[i + 1L, j], dp[i, j + 1L])
  shared <- dp[1L, 1L]
  similarity <- 2 * shared / (n + m)
  coverage <- shared / min(n, m)
  list(accepted = shared >= 2L && coverage >= 2 / 3,
       similarity = similarity, shared = shared)
}

.batch_content_only_evidence <- function(a, b) {
  evidence <- .batch_content_evidence(a, b)
  values <- c(evidence$title, evidence$column1)
  available <- values[!is.na(values)]
  if (!length(available))
    return(c(evidence, list(score = 0, accepted_content_only = FALSE)))
  if (length(available) == 1L) {
    accepted <- available[[1]] >= 0.85
  } else {
    accepted <- min(available) >= 0.20 && mean(available) >= 0.65
  }
  c(evidence, list(score = mean(available), accepted_content_only = accepted))
}

.batch_reciprocal_best <- function(candidates, margin) {
  if (!nrow(candidates)) return(candidates)
  safe_best <- function(group_col) {
    groups <- split(seq_len(nrow(candidates)), candidates[[group_col]])
    chosen <- character()
    for (idx in groups) {
      ord <- idx[order(-candidates$match_score[idx],
                       -ifelse(is.na(candidates$similarity[idx]), 0,
                               candidates$similarity[idx]),
                       ifelse(is.na(candidates$distance[idx]), Inf,
                              candidates$distance[idx]),
                       candidates$key1[idx], candidates$key2[idx])]
      if (length(ord) > 1L &&
          candidates$match_score[ord[1]] - candidates$match_score[ord[2]] < margin)
        next
      chosen <- c(chosen, paste(candidates$key1[ord[1]], candidates$key2[ord[1]],
                                sep = "\u001f"))
    }
    chosen
  }
  reciprocal <- intersect(safe_best("key1"), safe_best("key2"))
  if (!length(reciprocal)) return(candidates[0L, , drop = FALSE])
  ids <- paste(candidates$key1, candidates$key2, sep = "\u001f")
  candidates[match(reciprocal, ids), , drop = FALSE]
}

# Pair exact normalized names first. The second pass accepts either the strict
# edit-distance rule or a family pattern found by splitting names on "0" and
# finding at least two informative tokens in order with two-thirds coverage.
# All such candidates must pass the title/Column-1 content gate and be unique
# reciprocal winners. A final pass compares every still-unmatched Set 1 RTF to
# every still-unmatched Set 2 RTF using stronger content-only evidence.
.batch_pair_files <- function(f1, f2, dir1, dir2) {
  exact <- intersect(names(f1), names(f2))
  pairs <- data.frame(
    key1 = exact, key2 = exact, match_type = rep("exact", length(exact)),
    distance = rep(0L, length(exact)), similarity = rep(1, length(exact)),
    token_similarity = rep(NA_real_, length(exact)),
    title_similarity = rep(NA_real_, length(exact)),
    column1_similarity = rep(NA_real_, length(exact)),
    match_score = rep(1, length(exact)),
    stringsAsFactors = FALSE)

  left1 <- setdiff(names(f1), exact)
  left2 <- setdiff(names(f2), exact)
  candidates <- list(); k <- 0L; rejections <- list(); rj <- 0L
  cache1 <- new.env(parent = emptyenv()); cache2 <- new.env(parent = emptyenv())
  profile <- function(cache, folder, files, key) {
    if (!exists(key, envir = cache, inherits = FALSE))
      assign(key, .batch_content_profile(file.path(folder, unname(files[[key]]))),
             envir = cache)
    get(key, envir = cache, inherits = FALSE)
  }
  for (key1 in left1) {
    p1 <- .batch_fuzzy_parts(key1)
    for (key2 in left2) {
      p2 <- .batch_fuzzy_parts(key2)
      if (!identical(p1$dir, p2$dir)) next
      longest <- max(nchar(p1$stem), nchar(p2$stem))
      shortest <- min(nchar(p1$stem), nchar(p2$stem))
      if (shortest < 8L) next
      distance <- as.integer(adist(p1$stem, p2$stem)[1])
      similarity <- if (longest == 0L) 1 else 1 - distance / longest
      edit_candidate <- distance <= 3L && similarity >= 0.85
      token <- .batch_zero_token_evidence(p1$stem, p2$stem)
      if (edit_candidate || isTRUE(token$accepted)) {
        evidence <- .batch_content_evidence(
          profile(cache1, dir1, f1, key1), profile(cache2, dir2, f2, key2))
        if (!isTRUE(evidence$accepted)) {
          rj <- rj + 1L
          rejections[[rj]] <- data.frame(
            key1 = key1, key2 = key2, similarity = similarity,
            token_similarity = token$similarity,
            title_similarity = evidence$title,
            column1_similarity = evidence$column1,
            candidate_type = if (edit_candidate) "fuzzy" else "zero_token",
            stringsAsFactors = FALSE)
          next
        }
        k <- k + 1L
        filename_score <- max(similarity,
                              if (isTRUE(token$accepted)) token$similarity else 0)
        candidates[[k]] <- data.frame(
          key1 = key1, key2 = key2, distance = distance,
          similarity = similarity,
          token_similarity = token$similarity,
          title_similarity = evidence$title,
          column1_similarity = evidence$column1,
          match_score = 0.50 * filename_score +
            0.25 * ifelse(is.na(evidence$title), 0, evidence$title) +
            0.25 * ifelse(is.na(evidence$column1), 0, evidence$column1),
          match_type = if (edit_candidate) "fuzzy" else "zero_token",
          stringsAsFactors = FALSE)
      }
    }
  }

  template <- data.frame(key1 = character(), key2 = character(),
                         match_type = character(), distance = integer(),
                         similarity = numeric(), token_similarity = numeric(),
                         title_similarity = numeric(), column1_similarity = numeric(),
                         match_score = numeric(), stringsAsFactors = FALSE)
  filename_matches <- template
  if (length(candidates)) {
    cand <- do.call(rbind, candidates)
    filename_matches <- .batch_reciprocal_best(cand, margin = 0.05)
    filename_matches <- filename_matches[, names(template), drop = FALSE]
    pairs <- rbind(pairs, filename_matches)
  }

  # Content-only fallback for files without an accepted filename candidate.
  remain1 <- setdiff(left1, filename_matches$key1)
  remain2 <- setdiff(left2, filename_matches$key2)
  content_candidates <- list(); ck <- 0L
  for (key1 in remain1) for (key2 in remain2) {
    evidence <- .batch_content_only_evidence(
      profile(cache1, dir1, f1, key1), profile(cache2, dir2, f2, key2))
    if (!isTRUE(evidence$accepted_content_only)) next
    p1 <- .batch_fuzzy_parts(key1); p2 <- .batch_fuzzy_parts(key2)
    longest <- max(nchar(p1$stem), nchar(p2$stem))
    distance <- as.integer(adist(p1$stem, p2$stem)[1])
    ck <- ck + 1L
    content_candidates[[ck]] <- data.frame(
      key1 = key1, key2 = key2, match_type = "content_only",
      distance = distance,
      similarity = if (longest == 0L) 1 else 1 - distance / longest,
      token_similarity = .batch_zero_token_evidence(p1$stem, p2$stem)$similarity,
      title_similarity = evidence$title,
      column1_similarity = evidence$column1,
      match_score = evidence$score, stringsAsFactors = FALSE)
  }
  content_matches <- template
  if (length(content_candidates)) {
    content_matches <- .batch_reciprocal_best(
      do.call(rbind, content_candidates), margin = 0.10)
    content_matches <- content_matches[, names(template), drop = FALSE]
    pairs <- rbind(pairs, content_matches)
  }

  rejected <- if (length(rejections)) do.call(rbind, rejections) else
    data.frame(key1 = character(), key2 = character(), similarity = numeric(),
               token_similarity = numeric(),
               title_similarity = numeric(), column1_similarity = numeric(),
               candidate_type = character(),
               stringsAsFactors = FALSE)
  list(
    pairs = pairs,
    only1 = setdiff(names(f1), pairs$key1),
    only2 = setdiff(names(f2), pairs$key2),
    rejected = rejected,
    content_checked1 = setNames(rep(length(remain2), length(remain1)), remain1),
    content_checked2 = setNames(rep(length(remain1), length(remain2)), remain2)
  )
}

compare_rtf_folder <- function(dir1, dir2,
                               trim = TRUE, collapse_space = TRUE, casefold = FALSE,
                               num_tol = 0, rel_tol = FALSE,
                               recursive = FALSE,
                               progress = console, console = TRUE) {
  .need_pkg("data.table")
  for (d in c(dir1, dir2)) {
    if (!dir.exists(d)) stop(sprintf("Folder not found: '%s'", d), call. = FALSE)
  }

  list_rtf <- function(d) {
    files <- list.files(d, recursive = recursive, full.names = FALSE)
    is_rtf <- grepl("\\.rtf$", trimws(gsub("\\\\", "/", enc2utf8(files))),
                    ignore.case = TRUE)
    files <- files[is_rtf]
    files[!file.info(file.path(d, files))$isdir]
  }
  f1 <- .batch_file_index(list_rtf(dir1), "folder 1")
  f2 <- .batch_file_index(list_rtf(dir2), "folder 2")
  all_keys <- sort(unique(c(names(f1), names(f2))))

  if (length(all_keys) == 0L) {
    stop(sprintf("No .rtf files found in either folder:\n  %s\n  %s", dir1, dir2),
         call. = FALSE)
  }

  matching <- .batch_pair_files(f1, f2, dir1, dir2)
  items <- matching$pairs
  if (length(matching$only1)) items <- rbind(items, data.frame(
    key1 = matching$only1, key2 = NA_character_, match_type = "only1",
    distance = NA_integer_, similarity = NA_real_, token_similarity = NA_real_,
    title_similarity = NA_real_, column1_similarity = NA_real_,
    match_score = NA_real_, stringsAsFactors = FALSE))
  if (length(matching$only2)) items <- rbind(items, data.frame(
    key1 = NA_character_, key2 = matching$only2, match_type = "only2",
    distance = NA_integer_, similarity = NA_real_, token_similarity = NA_real_,
    title_similarity = NA_real_, column1_similarity = NA_real_,
    match_score = NA_real_, stringsAsFactors = FALSE))
  display <- ifelse(!is.na(items$key1), unname(f1[items$key1]), unname(f2[items$key2]))
  items <- items[order(tolower(display)), , drop = FALSE]

  rows    <- vector("list", nrow(items))
  results <- list()
  rejected_note <- function(key, side) {
    r <- matching$rejected
    if (!nrow(r) || is.na(key)) return("")
    idx <- which(r[[paste0("key", side)]] == key)
    if (!length(idx)) return("")
    idx <- idx[which.max(r$similarity[idx])]
    other_key <- r[[paste0("key", if (side == 1L) 2L else 1L)]][idx]
    other_name <- if (side == 1L) unname(f2[[other_key]]) else unname(f1[[other_key]])
    evidence_text <- function(x) if (is.na(x)) "unavailable" else sprintf("%.1f%%", 100 * x)
    sprintf("Like-name candidate '%s' rejected by content check (title %s, Column 1 %s)",
            other_name, evidence_text(r$title_similarity[idx]),
            evidence_text(r$column1_similarity[idx]))
  }

  for (i in seq_len(nrow(items))) {
    key1 <- items$key1[[i]]; key2 <- items$key2[[i]]
    in1 <- !is.na(key1); in2 <- !is.na(key2)
    file1_name <- if (in1) unname(f1[[key1]]) else NA_character_
    file2_name <- if (in2) unname(f2[[key2]]) else NA_character_
    display_name <- if (in1) file1_name else file2_name

    if (in1 && !in2) {
      status <- "ONLY_IN_FOLDER1"; ncells <- NA_integer_; ndiffs <- NA_integer_
      note <- rejected_note(key1, 1L)
      if (!nzchar(note)) {
        checked <- matching$content_checked1[[key1]]
        if (is.null(checked)) checked <- 0L
        note <- sprintf(paste0("No exact, 0-token, or safe content match found after ",
                               "checking %d unmatched Set 2 RTF(s)"), checked)
      }
    } else if (!in1 && in2) {
      status <- "ONLY_IN_FOLDER2"; ncells <- NA_integer_; ndiffs <- NA_integer_
      note <- rejected_note(key2, 2L)
      if (!nzchar(note)) {
        checked <- matching$content_checked2[[key2]]
        if (is.null(checked)) checked <- 0L
        note <- sprintf(paste0("No exact, 0-token, or safe content match found after ",
                               "checking %d unmatched Set 1 RTF(s)"), checked)
      }
    } else {
      res <- tryCatch(
        compare_rtf(file.path(dir1, file1_name), file.path(dir2, file2_name),
                    trim = trim, collapse_space = collapse_space, casefold = casefold,
                    num_tol = num_tol, rel_tol = rel_tol, console = FALSE),
        error = function(e) e)
      if (inherits(res, "error")) {
        status <- "ERROR"; ncells <- NA_integer_; ndiffs <- NA_integer_
        note <- conditionMessage(res)
      } else {
        res$file1 <- file.path(dir1, file1_name)
        res$file2 <- file.path(dir2, file2_name)
        res$file1_name <- file1_name
        res$file2_name <- file2_name
        results[[display_name]] <- res
        ncells <- res$n_cells; ndiffs <- res$n_diffs
        status <- if (isTRUE(res$equivalent)) "EQUIVALENT" else "DIFFERENCES"
        evidence_text <- function(x) if (is.na(x)) "unavailable" else sprintf("%.1f%%", 100 * x)
        note   <- if (identical(items$match_type[[i]], "fuzzy")) {
          sprintf(paste0("Fuzzy-matched to '%s' (edit distance %d, filename %.1f%%, ",
                         "title %s, Column 1 %s)"), file2_name, items$distance[[i]],
                  100 * items$similarity[[i]], evidence_text(items$title_similarity[[i]]),
                  evidence_text(items$column1_similarity[[i]]))
        } else if (identical(items$match_type[[i]], "zero_token")) {
          sprintf(paste0("0-token-matched to '%s' (token %.1f%%, filename %.1f%%, ",
                         "title %s, Column 1 %s)"), file2_name,
                  100 * items$token_similarity[[i]], 100 * items$similarity[[i]],
                  evidence_text(items$title_similarity[[i]]),
                  evidence_text(items$column1_similarity[[i]]))
        } else if (identical(items$match_type[[i]], "content_only")) {
          sprintf(paste0("Content-matched to '%s' after all-unmatched search ",
                         "(title %s, Column 1 %s)"), file2_name,
                  evidence_text(items$title_similarity[[i]]),
                  evidence_text(items$column1_similarity[[i]]))
        } else if (!identical(file1_name, file2_name)) {
          sprintf("Matched to '%s' in folder 2 after filename normalization", file2_name)
        } else {
          ""
        }
      }
    }

    rows[[i]] <- data.frame(file = display_name, status = status, n_cells = ncells,
                            n_diffs = ndiffs, note = note, stringsAsFactors = FALSE)
    if (progress) cat(sprintf("  %-44s %s\n", display_name,
                              if (status == "DIFFERENCES")
                                sprintf("%d difference(s)", ndiffs) else status))
  }

  summary <- data.table::rbindlist(rows)
  totals <- list(
    n_files      = nrow(summary),
    n_equivalent = sum(summary$status == "EQUIVALENT"),
    n_differing  = sum(summary$status == "DIFFERENCES"),
    n_only1      = sum(summary$status == "ONLY_IN_FOLDER1"),
    n_only2      = sum(summary$status == "ONLY_IN_FOLDER2"),
    n_errors     = sum(summary$status == "ERROR"),
    total_diffs  = sum(summary$n_diffs, na.rm = TRUE)
  )
  all_equivalent <- totals$n_differing == 0L && totals$n_errors == 0L &&
                    totals$n_only1 == 0L && totals$n_only2 == 0L

  if (console) {
    cat(sprintf(
      "\n%d file(s) compared: %d equivalent, %d differing, %d only in folder 1, %d only in folder 2, %d error(s).\n",
      totals$n_files, totals$n_equivalent, totals$n_differing,
      totals$n_only1, totals$n_only2, totals$n_errors))
  }

  list(dir1 = dir1, dir2 = dir2, summary = summary, results = results,
       totals = totals, all_equivalent = all_equivalent)
}

#' Write annotated Set 1 / Set 2 RTF copies for all successfully compared files.
#'
#' Unmatched and errored files have no comparison result and are intentionally
#' omitted. A failure to annotate one pair is recorded without stopping the
#' remaining batch.
write_batch_change_rtfs <- function(batch, tool_root, progress = FALSE) {
  rows <- list()
  # A change RTF is permitted only when an exact or safely fuzzy-matched pair
  # was actually compared. Unmatched and errored files remain report-only,
  # even if a malformed caller happens to place an extra entry in results.
  eligible <- batch$summary$file[
    batch$summary$status %in% c("EQUIVALENT", "DIFFERENCES")
  ]
  if (length(eligible) == 0L) {
    return(data.frame(pair = character(), set = character(), source = character(), output = character(),
                      ok = logical(), error = character(), stringsAsFactors = FALSE))
  }
  k <- 0L
  for (pair_index in seq_along(eligible)) {
    nm <- eligible[[pair_index]]
    if (isTRUE(progress))
      cat(sprintf("  Generating change RTF pair %d of %d: %s\n",
                  pair_index, length(eligible), nm))
    res <- batch$results[[nm]]
    pair <- if (is.null(res)) {
      data.frame(set = "Pair", source = nm, output = "", ok = FALSE,
                 error = "Internal batch error: an officially compared pair has no stored result.",
                 stringsAsFactors = FALSE)
    } else tryCatch(
        write_change_rtf_pair(res$file1, res$file2, res, tool_root,
                              relative1 = res$file1_name, relative2 = res$file2_name),
        error = function(e) data.frame(
          set = "Pair", source = nm, output = "", ok = FALSE,
          error = conditionMessage(e), stringsAsFactors = FALSE)
      )
    pair$pair <- nm
    pair <- pair[, c("pair", "set", "source", "output", "ok", "error"), drop = FALSE]
    k <- k + 1L
    rows[[k]] <- pair
  }
  do.call(rbind, rows)
}

#' Write the batch comparison report (text and/or CSV).
#'
#' The text report lists EVERY file with its result (equivalent files included),
#' then shows the cell-level differences for each file that differs. The CSV
#' keeps the per-file summary columns and adds one untruncated row per
#' cell-level difference so exported values can be copied in full.
#'
#' @param batch        Output of compare_rtf_folder().
#' @param txt_path,csv_path  Optional output paths.
#' @param options_str  Human-readable options string for the header.
#' @param console      Print the report to the console (default FALSE).
#' @param timestamp    Run time shown in the header (default now).
#' @return The text report (character vector), invisibly.
write_batch_report <- function(batch, txt_path = NULL, csv_path = NULL,
                               options_str = "", console = FALSE,
                               timestamp = Sys.time()) {
  s <- batch$summary
  t <- batch$totals
  overall <- if (isTRUE(batch$all_equivalent)) "ALL FILES EQUIVALENT"
             else "DIFFERENCES / MISMATCHES FOUND"

  hdr <- c(
    "============================================================",
    "RTF BATCH COMPARISON REPORT",
    "============================================================",
    paste0("Folder 1 (reference):  ", batch$dir1),
    paste0("Folder 2 (comparison): ", batch$dir2),
    paste0("Run at:  ", format(timestamp, "%Y-%m-%d %H:%M:%S")),
    paste0("Options: ", options_str),
    sprintf("Files compared: %d  (equivalent: %d, differing: %d, only in folder 1: %d, only in folder 2: %d, errors: %d)",
            t$n_files, t$n_equivalent, t$n_differing, t$n_only1, t$n_only2, t$n_errors),
    paste0("Result:  ", overall),
    "------------------------------------------------------------",
    "PER-FILE RESULTS  (every file is listed, including matches):"
  )

  padw <- function(x, n) {
    x <- as.character(x)
    paste0(x, strrep(" ", pmax(0L, n - nchar(x, type = "width"))))
  }
  result_label <- function(st, nd) data.table::fcase(
    st == "EQUIVALENT",     "EQUIVALENT",
    st == "DIFFERENCES",    sprintf("%s difference(s)", nd),
    st == "ONLY_IN_FOLDER1","ONLY IN FOLDER 1 (no match)",
    st == "ONLY_IN_FOLDER2","ONLY IN FOLDER 2 (no match)",
    st == "ERROR",          "ERROR (could not compare)",
    default =               st)
  fname_w <- max(c(nchar("FILE"), nchar(s$file)))
  table_lines <- c(
    paste0(padw("FILE", fname_w), "  ", "RESULT"),
    vapply(seq_len(nrow(s)), function(i) {
      note <- if (is.na(s$note[i]) || !nzchar(s$note[i])) "" else paste0("  -- ", s$note[i])
      paste0(padw(s$file[i], fname_w), "  ",
             result_label(s$status[i], s$n_diffs[i]), note)
    },
      character(1))
  )

  # Detailed cell-level differences for each file that differs.
  detail <- character(0)
  differing <- s$file[s$status == "DIFFERENCES"]
  if (length(differing) > 0L) {
    detail <- c("", "------------------------------------------------------------",
                "DIFFERENCES BY FILE:")
    for (nm in differing) {
      res <- batch$results[[nm]]
      detail <- c(detail, "",
                  sprintf(">>> %s  (%d difference(s))", nm, res$n_diffs),
                  .format_diff_detail(res$diffs))
    }
  }
  # Note any files that could not be compared at all.
  problem <- s[s$status %in% c("ONLY_IN_FOLDER1", "ONLY_IN_FOLDER2", "ERROR"), ]
  if (nrow(problem) > 0L) {
    detail <- c(detail, "", "------------------------------------------------------------",
                "FILES NOT COMPARED:",
                vapply(seq_len(nrow(problem)), function(i)
                  sprintf("  %s  -- %s", problem$file[i], problem$note[i]), character(1)))
  }

  txt <- c(hdr, table_lines, detail, "")
  if (console) cat(txt, sep = "\n")

  if (!is.null(txt_path)) writeLines(enc2utf8(txt), txt_path, useBytes = TRUE)
  if (!is.null(csv_path)) {
    .need_pkg("data.table")
    data.table::fwrite(.batch_csv_rows(batch), csv_path)
  }
  invisible(txt)
}

.batch_csv_rows <- function(batch) {
  .need_pkg("data.table")
  s <- data.table::copy(batch$summary)
  s[, `:=`(
    row_index = NA_integer_,
    col_index = NA_integer_,
    diff_status = NA_character_,
    value_file1 = NA_character_,
    value_file2 = NA_character_
  )]

  out <- vector("list", nrow(batch$summary))
  for (i in seq_len(nrow(batch$summary))) {
    row <- s[i]
    if (identical(row$status, "DIFFERENCES")) {
      d <- data.table::copy(batch$results[[row$file]]$diffs)
      data.table::setnames(d, "status", "diff_status")
      d[, `:=`(file = row$file, status = row$status, n_cells = row$n_cells,
               n_diffs = row$n_diffs, note = row$note)]
      data.table::setcolorder(d, names(row))
      out[[i]] <- d
    } else {
      out[[i]] <- row
    }
  }
  data.table::rbindlist(out, use.names = TRUE)
}

# ============================================================================
# Tool location + audit log -- proof of which comparisons were run, and when.
# ============================================================================
#' Locate the tool's root folder (the one containing R/compare_rtf.R).
#'
#' Walks up from a hint directory (typically the script's folder), then from
#' RTF_TOOL_ROOT, then the working directory. The audit log and archived reports
#' live under this root, which is why the folder must be kept intact.
#'
#' @param hint Optional directory to start searching from.
#' @return Absolute path to the tool root (falls back to getwd() if not found).
rtf_tool_root <- function(hint = NULL) {
  starts <- c(hint, Sys.getenv("RTF_TOOL_ROOT", ""), getwd())
  for (s in starts) {
    if (is.null(s) || !nzchar(s)) next
    d <- normalizePath(s, mustWork = FALSE)
    for (i in 1:10) {
      if (file.exists(file.path(d, "R", "compare_rtf.R"))) return(d)
      parent <- dirname(d)
      if (identical(parent, d)) break
      d <- parent
    }
  }
  normalizePath(getwd(), mustWork = FALSE)
}

#' Path to the append-only audit log (logs/audit_log.csv under the tool root).
audit_log_path <- function(root) file.path(root, "logs", "audit_log.csv")

# The audit log's column order. One row is appended per run (single or batch).
.AUDIT_COLS <- c("timestamp", "run_type", "item1", "item2", "result",
                 "files_compared", "files_equivalent", "files_differing",
                 "files_only_in_1", "files_only_in_2", "files_errored",
                 "total_diffs", "report_saved", "user", "host", "tool_version")

#' Append one record to the audit log, creating the file (with header) if needed.
#'
#' Every run is logged -- including runs that find NO differences -- so the log
#' is a complete, timestamped record of the QC work performed. Writing the log
#' never aborts a comparison: callers should wrap this in tryCatch and carry on.
#'
#' @param root        Tool root (see rtf_tool_root()).
#' @param run_type    "single" or "batch".
#' @param item1,item2 The two files (single) or folders (batch) compared.
#' @param result      Short result string (e.g. "EQUIVALENT", "1 DIFFERENCE(S)").
#' @param files_*     Per-run counts (defaults suit a single-file run).
#' @param total_diffs Total cell-level differences across the run.
#' @param report_saved Path of any report written for this run ("" if none).
#' @param timestamp   Run time (default now).
#' @return The audit log path, invisibly.
append_audit_log <- function(root, run_type, item1, item2, result,
                             files_compared = 1L, files_equivalent = NA_integer_,
                             files_differing = NA_integer_, files_only_in_1 = 0L,
                             files_only_in_2 = 0L, files_errored = 0L,
                             total_diffs = 0L, report_saved = "",
                             timestamp = Sys.time()) {
  .need_pkg("data.table")
  path <- audit_log_path(root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  entry <- data.frame(
    timestamp        = format(timestamp, "%Y-%m-%d %H:%M:%S"),
    run_type         = run_type,
    item1            = item1,
    item2            = item2,
    result           = result,
    files_compared   = files_compared,
    files_equivalent = files_equivalent,
    files_differing  = files_differing,
    files_only_in_1  = files_only_in_1,
    files_only_in_2  = files_only_in_2,
    files_errored    = files_errored,
    total_diffs      = total_diffs,
    report_saved     = report_saved,
    user             = unname(Sys.info()[["user"]]),
    host             = unname(Sys.info()[["nodename"]]),
    tool_version     = RTF_TOOL_VERSION,
    stringsAsFactors = FALSE
  )[, .AUDIT_COLS]

  is_new <- !file.exists(path)
  data.table::fwrite(entry, path, append = !is_new, col.names = is_new)
  invisible(path)
}

# ============================================================================
# Command-line interface
# ============================================================================
# Runs only when this file is executed directly (Rscript compare_rtf.R ...),
# never when it is sourced by another script (e.g. run_compare.R).

.is_main <- function() {
  cargs <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cargs[grepl("^--file=", cargs)])
  length(file_arg) > 0L && basename(file_arg[[1]]) == "compare_rtf.R"
}

.main <- function() {
  .need_pkg("optparse", why = "needed for the command-line interface")

  option_list <- list(
    optparse::make_option("--file1", type = "character", default = NULL,
      help = "Path to the FIRST (reference) RTF file. [required]"),
    optparse::make_option("--file2", type = "character", default = NULL,
      help = "Path to the SECOND (comparison) RTF file. [required]"),
    optparse::make_option("--num-tol", type = "double", default = 0,
      help = "Numeric tolerance; 0 = exact comparison. [default %default]"),
    optparse::make_option("--rel-tol", action = "store_true", default = FALSE,
      help = "Interpret --num-tol as relative (fraction of larger magnitude)."),
    optparse::make_option("--no-collapse-space", action = "store_true",
      default = FALSE, help = "Do NOT collapse internal whitespace runs."),
    optparse::make_option("--casefold", action = "store_true", default = FALSE,
      help = "Case-insensitive comparison."),
    optparse::make_option("--report", type = "character", default = NULL,
      help = "Write a plain-text report to this path."),
    optparse::make_option("--csv", type = "character", default = NULL,
      help = "Write differences as CSV to this path."),
    optparse::make_option("--diffdf", action = "store_true", default = FALSE,
      help = "Also run the diffdf secondary cross-check."),
    optparse::make_option("--quiet", action = "store_true", default = FALSE,
      help = "Suppress the console report (still writes files and sets exit code).")
  )

  parser <- optparse::OptionParser(
    usage = "Rscript compare_rtf.R --file1 <a.rtf> --file2 <b.rtf> [options]",
    option_list = option_list,
    description = paste(
      "\nCompare the rendered content of two RTF files.",
      "Exit codes: 0 = equivalent, 1 = differences found, 2 = error.", sep = "\n"))

  opt <- tryCatch(optparse::parse_args(parser),
                  error = function(e) { message(conditionMessage(e)); quit(status = 2L) })

  if (is.null(opt$file1) || is.null(opt$file2)) {
    optparse::print_help(parser)
    message("\nERROR: both --file1 and --file2 are required.")
    quit(status = 2L)
  }

  result <- tryCatch(
    compare_rtf(
      file1 = opt$file1, file2 = opt$file2,
      trim = TRUE, collapse_space = !opt$`no-collapse-space`,
      casefold = opt$casefold,
      num_tol = opt$`num-tol`, rel_tol = opt$`rel-tol`,
      txt_path = opt$report, csv_path = opt$csv,
      run_diffdf = opt$diffdf, diffdf_path = NULL,
      console = !opt$quiet),
    error = function(e) {
      message("ERROR: ", conditionMessage(e))
      quit(status = 2L)
    })

  quit(status = if (isTRUE(result$equivalent)) 0L else 1L)
}

if (.is_main()) .main()
