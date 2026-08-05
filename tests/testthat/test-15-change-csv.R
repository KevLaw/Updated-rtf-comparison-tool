# M15 -- deterministic CSV change-table output. These tests intentionally use
# synthetic structures that have caused real-world RTF rewriting failures.

.csv_test_rtf <- function(path, header, rows, title = "Table 99.1 Synthetic Table",
                          footnotes = character(), hidden = "") {
  ncol <- max(length(header), max(vapply(rows, length, integer(1))))
  pad <- function(x) c(x, rep("", ncol - length(x)))
  render_row <- function(values) {
    values <- pad(values)
    cellx <- paste0("\\cellx", seq_len(ncol) * 1800L, collapse = "")
    cells <- paste0("\\pard\\intbl\\plain\\f0\\fs18 ",
                    vapply(values, .rtf_escape_insert, character(1)), "\\cell",
                    collapse = "\n")
    paste0("\\trowd\\trgaph108\\trleft0", cellx, "\n", hidden,
           cells, "\n\\row")
  }
  body <- c(
    "{\\rtf1\\ansi\\ansicpg1252\\deff0",
    "{\\fonttbl{\\f0\\fnil\\fcharset0 Arial;}}",
    paste0("\\pard\\plain\\f0\\fs20 ", .rtf_escape_insert(title), "\\par"),
    render_row(header),
    vapply(rows, render_row, character(1)),
    vapply(footnotes, function(x)
      paste0("\\pard\\plain\\f0\\fs18 ", .rtf_escape_insert(x), "\\par"),
      character(1)),
    "}"
  )
  writeLines(body, path, useBytes = TRUE)
  path
}

.csv_change_fixture <- function(tag = "csv_change") {
  source(file.path(RTF_ROOT, "R", "generate_test_data.R"), local = TRUE)
  d <- file.path(tempdir(), paste0(tag, "_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  list(dir = d, files = generate_test_data(d, n_filler = 4L, seed = 321L,
                                           verbose = FALSE))
}

test_that("change CSV pair has source-named files and exact delta-only content", {
  g <- .csv_change_fixture("csv_pair")
  result <- compare_rtf(g$files[["base"]], g$files[["changed"]], console = FALSE)
  written <- write_change_csv_pair(g$files[["base"]], g$files[["changed"]],
                                   result, file.path(g$dir, "tool"))

  expect_equal(written$set, c("Set 1", "Set 2"))
  expect_true(all(written$ok))
  expect_true(all(file.exists(written$output)))
  expect_equal(basename(written$output),
               c("clinical_table_base_change.csv", "clinical_table_changed_change.csv"))
  expect_true(all(grepl("logs/RTF Changes/Set [12]", chartr("\\", "/", written$output))))
  expect_true(all(vapply(written$output, function(path)
    identical(as.integer(readBin(path, "raw", n = 3L)), c(239L, 187L, 191L)), logical(1))))

  csv <- lapply(written$output, .read_change_csv)
  expect_identical(names(csv[[1]]), names(csv[[2]]))
  expect_equal(csv[[1]][[1]][3], "Safety Population")
  expect_equal(csv[[2]][[1]][3], "Safety Population (Final)")
  expect_true(all(grepl("\n(?:Change|NC)$", names(csv[[1]]), perl = TRUE)))
  expect_true(any(csv[[1]][[1]] == "Table 14.3.1.2_Change"))
  headache <- csv[[1]][trimws(csv[[1]][[1]]) == "Headache", , drop = FALSE]
  expect_equal(unname(as.character(headache[1, ])),
               c("    Headache", "(+1, +0.5%)", "NC", "(+1, +0.5%)", "NC", "NC"))
  expect_false(any(grepl("45 (21.4%)", unlist(csv[[1]], use.names = FALSE), fixed = TRUE)))
  expect_true(any(csv[[1]][[1]] == "Footnote changes in brackets"))
  expect_true(any(grepl("Sponsor \\(Pharma\\)", csv[[1]][[1]])))
})

test_that("equivalent tables produce NC headers and NC body values", {
  g <- .csv_change_fixture("csv_equivalent")
  result <- compare_rtf(g$files[["base"]], g$files[["reformatted"]], console = FALSE)
  written <- write_change_csv_pair(g$files[["base"]], g$files[["reformatted"]],
                                   result, g$dir)
  csv <- .read_change_csv(written$output[[1]])
  expect_true(all(endsWith(names(csv), "\nNC")))
  body <- csv[grepl("^[[:space:]]", csv[[1]]) | csv[[1]] == "Placebo", , drop = FALSE]
  expect_true(any(as.matrix(body) == "NC"))
  expect_false(any(as.matrix(csv) == "No Change"))
})

test_that("CSV generation does not call the raw RTF change-document mapper", {
  d <- file.path(tempdir(), paste0("csv_no_raw_mapper_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  hidden <- "{\\*\\oldcprops \\cell\\cell\\cell\\row\\trowd\\par\\clmgf}"
  f1 <- .csv_test_rtf(file.path(d, "set1.rtf"), c("Term", "N", "Percent"),
                      list(c("Headache", "5", "5%")), hidden = hidden)
  f2 <- .csv_test_rtf(file.path(d, "set2.rtf"), c("Term", "N", "Percent"),
                      list(c("Headache", "7", "4%")), hidden = hidden)
  result <- compare_rtf(f1, f2, console = FALSE)

  target <- environment(write_change_csv_pair)
  original <- get(".rtf_parse_change_document", envir = target)
  on.exit(assign(".rtf_parse_change_document", original, envir = target), add = TRUE)
  assign(".rtf_parse_change_document",
         function(...) stop("raw mapper must not be called", call. = FALSE), envir = target)
  written <- write_change_csv_pair(f1, f2, result, d)
  expect_true(all(written$ok))
  expect_true(all(file.exists(written$output)))
  expect_false(any(grepl("raw mapper", vapply(written$output, function(path) "", character(1)))))
})

test_that("different source column counts are padded and remain reviewable", {
  d <- file.path(tempdir(), paste0("csv_column_mismatch_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  f1 <- .csv_test_rtf(file.path(d, "two_cols.rtf"), c("Term", "Value"),
                      list(c("Headache", "5 (5%)")))
  f2 <- .csv_test_rtf(file.path(d, "three_cols.rtf"), c("Term", "Value", "New Metric"),
                      list(c("Headache", "7 (4%)", "3")))
  result <- compare_rtf(f1, f2, console = FALSE)
  written <- write_change_csv_pair(f1, f2, result, d)
  set1 <- .read_change_csv(written$output[[1]])
  set2 <- .read_change_csv(written$output[[2]])
  expect_equal(ncol(set1), 3L)
  expect_equal(ncol(set2), 3L)
  expect_equal(names(set1)[3], "Column 3\nChange")
  expect_equal(names(set2)[3], "New Metric\nChange")
  row <- set1[set1[[1]] == "Headache", , drop = FALSE]
  expect_equal(unname(as.character(row[1, ])), c("Headache", "(+2, -1%)", "CHG"))
})

test_that("CSV round trip preserves Unicode commas quotes leading spaces and newlines", {
  d <- file.path(tempdir(), paste0("csv_specials_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  f1 <- .csv_test_rtf(
    file.path(d, "special1.rtf"), c("  Term, \"quoted\"", "Result\nValue"),
    list(c("  © ≥ − ° µ – é ü α β", "1.5")),
    footnotes = "Note, \"quoted\": 25-mg")
  f2 <- .csv_test_rtf(
    file.path(d, "special2.rtf"), c("  Term, \"quoted\"", "Result\nValue"),
    list(c("  © ≥ − ° µ – é ü α β", "3.0")),
    footnotes = "Note, \"quoted\": 25 - mg")
  result <- compare_rtf(f1, f2, console = FALSE)
  written <- write_change_csv_pair(f1, f2, result, d)
  csv <- .read_change_csv(written$output[[1]])
  expect_equal(names(csv)[1], "  Term, \"quoted\"\nNC")
  expect_equal(names(csv)[2], "Result\nValue\nChange")
  expect_true(any(csv[[1]] == "  © ≥ − ° µ – é ü α β"))
  expect_true(any(csv[[2]] == "+1.5"))
  expect_true(any(csv[[1]] == "Note, \"quoted\": 25 - mg"))
  expect_false(any(csv[[1]] == "Footnote changes in brackets"))
})

test_that("failed CSV validation leaves symmetric folders and no partial pair", {
  d <- file.path(tempdir(), paste0("csv_atomic_failure_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  f1 <- .csv_test_rtf(file.path(d, "set1.rtf"), c("Term", "N"), list(c("A", "1")))
  f2 <- .csv_test_rtf(file.path(d, "set2.rtf"), c("Term", "N"), list(c("A", "2")))
  result <- compare_rtf(f1, f2, console = FALSE)

  target <- environment(write_change_csv_pair)
  original <- get(".validate_change_csv_output", envir = target)
  calls <- 0L
  on.exit(assign(".validate_change_csv_output", original, envir = target), add = TRUE)
  assign(".validate_change_csv_output", function(...) {
    calls <<- calls + 1L
    if (calls == 2L) stop("forced second CSV validation failure", call. = FALSE)
    invisible(TRUE)
  }, envir = target)
  expect_error(write_change_csv_pair(f1, f2, result, d),
               "forced second CSV validation failure", fixed = TRUE)
  base <- file.path(d, "logs", "RTF Changes")
  expect_true(dir.exists(file.path(base, "Set 1")))
  expect_true(dir.exists(file.path(base, "Set 2")))
  expect_length(list.files(base, pattern = "\\.csv$", recursive = TRUE), 0L)
})

test_that("100-pair synthetic folder run creates and reparses all 200 CSVs", {
  d <- file.path(tempdir(), paste0("csv_100_pairs_", as.integer(runif(1, 1, 1e9))))
  d1 <- file.path(d, "set1"); d2 <- file.path(d, "set2")
  dir.create(d1, recursive = TRUE); dir.create(d2, recursive = TRUE)
  for (i in 1:100) {
    nm <- sprintf("synthetic_table_%03d.rtf", i)
    hidden <- if (i %% 3L == 0L)
      "{\\*\\oldcprops \\cell\\cell\\cell\\row\\trowd\\par}" else ""
    label <- if (i %% 10L == 0L) paste0("  Event, \"", i, "\"") else paste0("  Event ", i)
    base_count <- 100L + i
    delta <- if (i %% 4L == 0L) 0L else (i %% 7L) + 1L
    .csv_test_rtf(file.path(d1, nm), c("Term", "Count", "Rate"),
                  list(c(label, as.character(base_count), sprintf("%s%%", i %% 20L))),
                  title = sprintf("Table 99.%d Synthetic ©", i), hidden = hidden)
    .csv_test_rtf(file.path(d2, nm), c("Term", "Count", "Rate"),
                  list(c(label, as.character(base_count + delta), sprintf("%s%%", (i + 1L) %% 20L))),
                  title = sprintf("Table 99.%d Synthetic ©", i), hidden = hidden)
  }
  batch <- compare_rtf_folder(d1, d2, console = FALSE, progress = FALSE)
  expect_equal(nrow(batch$summary), 100L)
  expect_equal(length(batch$results), 100L)
  written <- write_batch_change_csvs(batch, file.path(d, "tool"), progress = FALSE)
  expect_equal(nrow(written), 200L)
  expect_equal(length(unique(written$pair)), 100L)
  expect_true(all(written$ok))
  expect_true(all(file.exists(written$output)))
  expect_equal(length(unique(written$output)), 200L)
  reparsed <- lapply(written$output, .read_change_csv)
  expect_true(all(vapply(reparsed, nrow, integer(1)) >= 3L))
  expect_true(all(vapply(reparsed, function(x)
    all(grepl("\n(?:Change|NC)$", names(x), perl = TRUE)), logical(1))))
  expect_length(list.files(file.path(d, "tool", "logs", "RTF Changes"),
                           pattern = "_change\\.rtf$", recursive = TRUE), 0L)
})

test_that("large synthetic tables generate complete CSVs without truncation", {
  source(file.path(RTF_ROOT, "R", "generate_test_data.R"), local = TRUE)
  d <- file.path(tempdir(), paste0("csv_large_table_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  files <- generate_test_data(d, n_filler = 500L, seed = 804L, verbose = FALSE)
  result <- compare_rtf(files[["base"]], files[["changed"]], console = FALSE)
  written <- write_change_csv_pair(files[["base"]], files[["changed"]], result,
                                   file.path(d, "tool"))
  expect_true(all(written$ok))
  expect_true(all(file.info(written$output)$size > 20000L))
  csv <- lapply(written$output, .read_change_csv)
  expect_true(all(vapply(csv, nrow, integer(1)) > 500L))
  expect_equal(vapply(csv, nrow, integer(1)), rep(nrow(csv[[1]]), 2L))
  expect_true(all(vapply(csv, function(x)
    any(trimws(x[[1]]) == "Headache"), logical(1))))
})

test_that("batch CSV writer skips unmatched files and accounts for a missing result", {
  d <- file.path(tempdir(), paste0("csv_batch_accounting_", as.integer(runif(1, 1, 1e9))))
  d1 <- file.path(d, "set1"); d2 <- file.path(d, "set2")
  dir.create(d1, recursive = TRUE); dir.create(d2, recursive = TRUE)
  .csv_test_rtf(file.path(d1, "matched.rtf"), c("Term", "N"), list(c("A", "1")))
  .csv_test_rtf(file.path(d2, "matched.rtf"), c("Term", "N"), list(c("A", "2")))
  .csv_test_rtf(file.path(d1, "only_set1.rtf"), c("Term", "N"), list(c("X", "1")))
  batch <- compare_rtf_folder(d1, d2, console = FALSE, progress = FALSE)
  written <- write_batch_change_csvs(batch, file.path(d, "tool"))
  expect_equal(nrow(written), 2L)
  expect_true(all(written$ok))
  expect_false(any(grepl("only_set1", written$output, fixed = TRUE)))

  incomplete <- batch
  incomplete$results[["matched.rtf"]] <- NULL
  failed <- write_batch_change_csvs(incomplete, file.path(d, "incomplete"))
  expect_equal(nrow(failed), 1L)
  expect_false(failed$ok)
  expect_match(failed$error, "officially compared pair has no stored result", fixed = TRUE)
})
