# M16 -- safe semantic RTF output.  Unlike the historical compatibility
# writer, these tests prove that grouped displayed columns are regenerated as
# clean, readable RTF rather than copied through source Word/SAS metadata.

.rtf16_simple <- function(path, header, rows,
                          title = "Table 99.16 Semantic RTF Boundary Test",
                          footnotes = character(), hidden = "") {
  n <- max(length(header), max(vapply(rows, length, integer(1))))
  pad <- function(x) c(x, rep("", n - length(x)))
  row <- function(x) paste0(
    "\\trowd", paste0("\\cellx", seq_len(n) * 1800L, collapse = ""), hidden,
    paste0("\\pard\\intbl\\f0\\fs18 ",
           vapply(pad(x), .rtf_escape_insert, character(1)), "\\cell", collapse = ""),
    "\\row")
  writeLines(c(
    "{\\rtf1\\ansi\\ansicpg1252\\deff0{\\fonttbl{\\f0 Arial;}}\\uc1",
    paste0("\\pard\\f0\\fs20 ", .rtf_escape_insert(title), "\\par"),
    row(header), vapply(rows, row, character(1)),
    vapply(footnotes, function(x)
      paste0("\\pard\\f0\\fs18 ", .rtf_escape_insert(x), "\\par"), character(1)),
    "}"
  ), path, useBytes = TRUE)
  path
}

.rtf16_grouped <- function(path, rows, style = c("merged", "wide"),
                           footnote = "Source: grouped synthetic.") {
  style <- match.arg(style)
  full <- paste0("\\cellx", c(2100, 3600, 4600, 5600, 6600, 7600, 8600, 9600),
                 collapse = "")
  if (style == "merged") {
    top_props <- paste0("\\cellx2100\\cellx3600",
      "\\clmgf\\cellx4600\\clmrg\\cellx5600",
      "\\clmgf\\cellx6600\\clmrg\\cellx7600",
      "\\clmgf\\cellx8600\\clmrg\\cellx9600")
    top_values <- c("", "Outcome", "Dose A", "", "Dose B", "", "Total", "")
  } else {
    top_props <- "\\cellx2100\\cellx3600\\cellx5600\\cellx7600\\cellx9600"
    top_values <- c("", "Outcome", "Dose A", "Dose B", "Total")
  }
  render <- function(props, values, trhdr = FALSE) paste0(
    "\\trowd", if (trhdr) "\\trhdr" else "", props,
    paste0("\\pard\\intbl\\f0\\fs18 ",
           vapply(values, .rtf_escape_insert, character(1)), "\\cell", collapse = ""),
    "\\row")
  writeLines(c(
    "{\\rtf1\\ansi\\ansicpg1252\\deff0{\\fonttbl{\\f0 Arial;}}\\uc1",
    "\\pard\\f0\\fs20 Table 99.17 Grouped Boundary Test\\par",
    render(top_props, top_values, TRUE),
    render(full, c("", "", "n", "(%)", "n", "(%)", "n", "(%)"), TRUE),
    vapply(rows, function(x) render(full, x), character(1)),
    paste0("\\pard\\f0\\fs18 ", .rtf_escape_insert(footnote), "\\par"),
    "}"
  ), path, useBytes = TRUE)
  path
}

.rtf16_table_rows <- function(path) {
  layout <- .rtf_cell_layout(path)
  ids <- unique(layout$row_index[layout$is_table == TRUE])
  lapply(ids, function(id) layout$raw_value[
    layout$is_table == TRUE & layout$row_index == id])
}

test_that("combined pair writes named CSV and readable semantic RTF outputs", {
  d <- file.path(tempdir(), paste0("rtf16_pair_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  f1 <- .rtf16_simple(file.path(d, "set-one.rtf"), c("Term", "Count", "Rate"),
    list(c("Headache", "5", "5%"), c("Nausea", "2", "2%")),
    footnotes = "Note: 25-mg dose; © ≥ − ° µ – é ü α β")
  f2 <- .rtf16_simple(file.path(d, "set-two.rtf"), c("Term", "Count", "Rate"),
    list(c("Headache", "10", "4%"), c("Nausea", "2", "2%")),
    footnotes = "Note: 25 - mg dose; © ≥ − ° µ – é ü α β")
  result <- compare_rtf(f1, f2, console = FALSE)
  written <- write_change_output_pair(f1, f2, result, file.path(d, "tool"))

  expect_equal(nrow(written), 4L)
  expect_equal(written$format, c("CSV", "CSV", "RTF", "RTF"))
  expect_true(all(written$ok) && all(file.exists(written$output)))
  expect_equal(basename(written$output),
    c("set-one_change.csv", "set-two_change.csv",
      "set-one_change.rtf", "set-two_change.rtf"))
  rows <- .rtf16_table_rows(written$output[written$format == "RTF"][[1]])
  expect_equal(rows[[1]], c("Term\nNC", "Count\nChange", "Rate\nChange"))
  expect_equal(rows[[2]], c("Headache", "+5", "-1%"))
  paragraphs <- .rtf_cell_layout(written$output[written$format == "RTF"][[2]])
  paragraphs <- paragraphs$raw_value[paragraphs$is_table == FALSE]
  expect_true("Table 99.16_Change Semantic RTF Boundary Test" %in% paragraphs)
  expect_true("Note: 25 - mg dose; © ≥ − ° µ – é ü α β" %in% paragraphs)
  expect_false("Footnote changes in brackets" %in% paragraphs)
})

test_that("merged and wide n-percent columns stay consolidated in semantic RTF", {
  for (style in c("merged", "wide")) {
    d <- file.path(tempdir(), paste0("rtf16_group_", style, "_",
                                     as.integer(runif(1, 1, 1e9))))
    dir.create(d, recursive = TRUE)
    a <- list(c("Headache", "Overall", "5", "(2.5)", "3", "(1.5)", "8", "(2.0)"))
    b <- list(c("Headache", "Overall", "7", "(3.5)", "3", "(1.5)", "10", "(2.5)"))
    f1 <- .rtf16_grouped(file.path(d, "a.rtf"), a, style)
    f2 <- .rtf16_grouped(file.path(d, "b.rtf"), b, style)
    result <- compare_rtf(f1, f2, console = FALSE)
    written <- write_semantic_change_rtf_pair(f1, f2, result, d)
    rows <- .rtf16_table_rows(written$output[[1]])
    expect_equal(length(rows[[1]]), 5L, info = style)
    expect_equal(rows[[1]], c("\nNC", "Outcome\nNC", "Dose A\nn (%)\nChange",
                              "Dose B\nn (%)\nNC", "Total\nn (%)\nChange"), info = style)
    expect_equal(rows[[2]], c("Headache", "NC", "(+2, +1%)", "NC", "(+2, +0.5%)"),
                 info = style)
    expect_false(any(grepl("Column [0-9]", unlist(rows), perl = TRUE)), info = style)
  }
})

test_that("footnote substitutions additions deletions and cosmetic spacing follow rules", {
  cases <- list(
    list("25-mg", "25 - mg", "25 - mg", FALSE),
    list("today is a great day July 31, 2025",
         "Today is a rotten day July 31, 2026",
         "(T)oday is a (rotten) day July 31, 202(6)", TRUE),
    list("alpha beta gamma", "alpha gamma", "alpha (missing)gamma", TRUE),
    list("alpha gamma", "alpha beta gamma", "alpha (beta) gamma", TRUE),
    list("A1 foot", "A2 foot", "(A2) foot", TRUE)
  )
  for (i in seq_along(cases)) {
    x <- cases[[i]]; aligned <- .align_footnotes(x[[1]], x[[2]])
    expect_identical(aligned$values, x[[3]], info = paste("case", i))
    expect_identical(aligned$changed, x[[4]], info = paste("case", i))
  }

  d <- file.path(tempdir(), paste0("rtf16_feet_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  old <- c("25-mg", "today is a great day July 31, 2025", "alpha beta gamma")
  new <- c("25 - mg", "Today is a rotten day July 31, 2026", "alpha gamma")
  f1 <- .rtf16_simple(file.path(d, "old.rtf"), c("Term", "N"), list(c("A", "1")),
                      footnotes = old)
  f2 <- .rtf16_simple(file.path(d, "new.rtf"), c("Term", "N"), list(c("A", "2")),
                      footnotes = new)
  result <- compare_rtf(f1, f2, console = FALSE)
  written <- write_semantic_change_rtf_pair(f1, f2, result, d)
  layout <- .rtf_cell_layout(written$output[[2]])
  feet <- layout$raw_value[layout$is_table == FALSE]
  expect_true(all(c("Footnote changes in brackets", "25 - mg",
    "(T)oday is a (rotten) day July 31, 202(6)", "alpha (missing)gamma") %in% feet))
  expect_false(any(old %in% feet))
})

test_that("row-only cases long text and hostile source metadata produce valid RTF", {
  d <- file.path(tempdir(), paste0("rtf16_boundaries_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  hostile <- "{\\*\\oldcprops \\cell\\row\\trowd\\clmgf\\clmrg}"
  long <- paste(rep("very long displayed description with braces {x} and slash \\", 40),
                collapse = " ")
  f1 <- .rtf16_simple(file.path(d, "left.rtf"), c("Term", "Value"),
    list(c("Only left", "1"), c(long, "10")), hidden = hostile)
  f2 <- .rtf16_simple(file.path(d, "right.rtf"), c("Term", "Value"),
    list(c("Only right", "2"), c(long, "12")), hidden = hostile)
  result <- compare_rtf(f1, f2, console = FALSE)
  written <- write_semantic_change_rtf_pair(f1, f2, result, d)
  expect_true(all(file.info(written$output)$size > 1000L))
  for (path in written$output) {
    expect_silent(.validate_rtf_container(.read_rtf_text(path), path))
    rows <- .rtf16_table_rows(path)
    expect_true(any(grepl("ONLY IN SET 1", unlist(rows), fixed = TRUE)))
    expect_true(any(grepl("ONLY IN SET 2", unlist(rows), fixed = TRUE)))
    expect_true(any(grepl("very long displayed description", unlist(rows), fixed = TRUE)))
  }
})

test_that("30-pair folder run creates and reparses all 120 CSV and RTF outputs", {
  d <- file.path(tempdir(), paste0("rtf16_batch_", as.integer(runif(1, 1, 1e9))))
  d1 <- file.path(d, "set1"); d2 <- file.path(d, "set2")
  dir.create(d1, recursive = TRUE); dir.create(d2, recursive = TRUE)
  for (i in 1:30) {
    nm <- sprintf("table_%02d.rtf", i)
    .rtf16_simple(file.path(d1, nm), c("Term", "N", "Rate"),
                  list(c(sprintf("Event %02d", i), as.character(i), sprintf("%s%%", i))))
    .rtf16_simple(file.path(d2, nm), c("Term", "N", "Rate"),
                  list(c(sprintf("Event %02d", i), as.character(i + 1L), sprintf("%s%%", i + 1L))))
  }
  batch <- compare_rtf_folder(d1, d2, console = FALSE, progress = FALSE)
  written <- write_batch_change_outputs(batch, file.path(d, "tool"), progress = FALSE)
  expect_equal(nrow(written), 120L)
  expect_equal(length(unique(written$pair)), 30L)
  format_counts <- table(written$format)
  expect_equal(names(format_counts), c("CSV", "RTF"))
  expect_equal(as.integer(format_counts), c(60L, 60L))
  expect_true(all(written$ok) && all(file.exists(written$output)))
  for (path in written$output[written$format == "RTF"])
    expect_true(length(.rtf16_table_rows(path)) == 2L)
})

test_that("600-row semantic RTF is complete and not truncated", {
  d <- file.path(tempdir(), paste0("rtf16_large_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  rows1 <- lapply(1:600, function(i)
    c(sprintf("Repeated event %03d", i), as.character(1000L + i), sprintf("%.2f%%", i / 10)))
  rows2 <- lapply(rows1, function(x) {
    x[[2]] <- as.character(as.integer(x[[2]]) + 5L)
    x[[3]] <- sprintf("%.2f%%", as.numeric(sub("%", "", x[[3]], fixed = TRUE)) + 0.25)
    x
  })
  f1 <- .rtf16_simple(file.path(d, "large1.rtf"), c("Term", "Count", "Rate"), rows1)
  f2 <- .rtf16_simple(file.path(d, "large2.rtf"), c("Term", "Count", "Rate"), rows2)
  result <- compare_rtf(f1, f2, console = FALSE)
  written <- write_semantic_change_rtf_pair(f1, f2, result, d)
  expect_true(all(file.info(written$output)$size > 150000L))
  for (path in written$output) {
    rows <- .rtf16_table_rows(path)
    expect_equal(length(rows), 601L)
    expect_equal(rows[[2]], c("Repeated event 001", "+5", "+0.25%"))
    expect_equal(rows[[601]], c("Repeated event 600", "+5", "+0.25%"))
  }
})

test_that("semantic RTF validation failure leaves no partial Set pair", {
  d <- file.path(tempdir(), paste0("rtf16_atomic_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  f1 <- .rtf16_simple(file.path(d, "a.rtf"), c("Term", "N"), list(c("A", "1")))
  f2 <- .rtf16_simple(file.path(d, "b.rtf"), c("Term", "N"), list(c("A", "2")))
  result <- compare_rtf(f1, f2, console = FALSE)
  target <- environment(write_semantic_change_rtf_pair)
  original <- get(".validate_semantic_change_rtf_output", envir = target)
  calls <- 0L
  on.exit(assign(".validate_semantic_change_rtf_output", original, envir = target), add = TRUE)
  assign(".validate_semantic_change_rtf_output", function(...) {
    calls <<- calls + 1L
    if (calls == 2L) stop("forced second semantic RTF failure", call. = FALSE)
    invisible(TRUE)
  }, envir = target)
  expect_error(write_semantic_change_rtf_pair(f1, f2, result, d),
               "forced second semantic RTF failure", fixed = TRUE)
  base <- file.path(d, "logs", "RTF Changes")
  expect_true(dir.exists(file.path(base, "Set 1")))
  expect_true(dir.exists(file.path(base, "Set 2")))
  expect_length(list.files(base, pattern = "_change\\.rtf$", recursive = TRUE), 0L)
})

test_that("different semantic column counts are padded in readable RTF", {
  d <- file.path(tempdir(), paste0("rtf16_columns_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  f1 <- .rtf16_simple(file.path(d, "two.rtf"), c("Term", "Value"),
                      list(c("Headache", "5 (5%)")))
  f2 <- .rtf16_simple(file.path(d, "three.rtf"), c("Term", "Value", "New Metric"),
                      list(c("Headache", "7 (4%)", "3")))
  result <- compare_rtf(f1, f2, console = FALSE)
  written <- write_semantic_change_rtf_pair(f1, f2, result, d)
  rows1 <- .rtf16_table_rows(written$output[[1]])
  rows2 <- .rtf16_table_rows(written$output[[2]])
  expect_equal(length(rows1[[1]]), 3L)
  expect_equal(rows1[[1]], c("Term\nNC", "Value\nChange", "\nChange"))
  expect_equal(rows2[[1]], c("Term\nNC", "Value\nChange", "New Metric\nChange"))
  expect_equal(rows1[[2]], c("Headache", "(+2, -1%)", "CHG"))
  expect_identical(rows1[[2]], rows2[[2]])
})

test_that("64-column boundary keeps strictly increasing RTF cell positions", {
  d <- file.path(tempdir(), paste0("rtf16_64cols_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  header <- c("Term", sprintf("Metric %02d", 2:64))
  row1 <- c("Event", as.character(2:64))
  row2 <- row1; row2[[64]] <- "71"
  f1 <- .rtf16_simple(file.path(d, "wide1.rtf"), header, list(row1))
  f2 <- .rtf16_simple(file.path(d, "wide2.rtf"), header, list(row2))
  result <- compare_rtf(f1, f2, console = FALSE)
  written <- write_semantic_change_rtf_pair(f1, f2, result, d)
  rows <- .rtf16_table_rows(written$output[[1]])
  expect_equal(length(rows[[1]]), 64L)
  expect_equal(rows[[2]][[64]], "+7")
  text <- .read_rtf_text(written$output[[1]])
  tokens <- .rtf_tokens(text)
  cellx <- as.integer(tokens$param[tokens$control == "cellx"])
  first <- cellx[seq_len(64L)]
  expect_true(!anyNA(first) && !is.unsorted(first, strictly = TRUE))
})

test_that("macOS TextEdit conversion opens a representative generated RTF", {
  skip_if_not(identical(Sys.info()[["sysname"]], "Darwin"))
  skip_if(Sys.which("textutil") == "", "macOS textutil is unavailable")
  d <- file.path(tempdir(), paste0("rtf16_textutil_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  f1 <- .rtf16_simple(file.path(d, "a.rtf"), c("Term", "N"), list(c("Headache", "1")))
  f2 <- .rtf16_simple(file.path(d, "b.rtf"), c("Term", "N"), list(c("Headache", "2")))
  result <- compare_rtf(f1, f2, console = FALSE)
  output <- write_semantic_change_rtf_pair(f1, f2, result, d)$output[[2]]
  text <- suppressWarnings(system2("textutil", c("-convert", "txt", "-stdout",
                                                  shQuote(output)),
                                   stdout = TRUE, stderr = TRUE))
  status <- attr(text, "status"); if (is.null(status)) status <- 0L
  expect_equal(status, 0L)
  expect_true(any(grepl("Table 99.16_Change", text, fixed = TRUE)))
  expect_true(any(grepl("Headache", text, fixed = TRUE)))
})
