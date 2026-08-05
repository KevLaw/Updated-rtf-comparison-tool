# Core rules for semantic change-delta RTFs.  Expected values in this file are
# literal product requirements, not values calculated by the writer itself.

test_that("central formatter uses two significant figures and suppresses signed zero", {
  expect_equal(.format_change_number(0.476), "+0.48")
  expect_equal(.format_change_number(2.38), "+2.4")
  expect_equal(.format_change_number(12.34), "+12")
  expect_equal(.format_change_number(99.6), "+100")
  expect_equal(.format_change_number(-0.0801), "-0.08")
  expect_equal(.format_change_number(-1e-16), "0")
})

test_that("count-percent deltas use displayed percentages and count equality wins", {
  expect_equal(.change_cell_value("45 (21.4%)", "50 (23.8%)"), "(+5, +2.4%)")
  expect_equal(.change_cell_value("45 (5%)", "50 (4%)"), "(+5, -1%)")
  expect_equal(.change_cell_value("50 (5.08%)", "45 (5%)"), "(-5, -0.08%)")
  expect_equal(.change_cell_value("1,000 (5%)", "1,005 (5.08%)"), "(+5, +0.08%)")
  expect_equal(.change_cell_value("45 (5%)", "45 (4%)"), "NC")
})

test_that("plain scalar values calculate while unsupported values use CHG", {
  expect_equal(.change_cell_value("12.5", "14.0"), "+1.5")
  expect_equal(.change_cell_value("14", "12.5"), "-1.5")
  expect_equal(.change_cell_value("12.50", "12.5"), "NC")
  expect_equal(.change_cell_value("same text", "same text"), "NC")
  expect_equal(.change_cell_value("alpha", "beta"), "CHG")
  expect_equal(.change_cell_value("(1.2 to 3.4)", "(1.3 to 3.5)"), "CHG")
})

.fake_change_rows <- function(keys) {
  lapply(keys, function(k) list(key = k, values = c(k, "1"), raw = "", blank = FALSE))
}

test_that("semantic LCS alignment prevents positional cascades", {
  a <- .fake_change_rows(c("A", "B", "B#2", "D"))
  b <- .fake_change_rows(c("A", "X", "B", "B#2", "D", "Z"))
  z <- .align_change_rows(a, b)
  pairs <- vapply(z, function(x) paste(ifelse(is.na(x$i1), "-", x$i1),
                                        ifelse(is.na(x$i2), "-", x$i2), sep = ":"),
                  character(1))
  expect_equal(pairs, c("1:1", "-:2", "2:3", "3:4", "4:5", "-:6"))
})

test_that("Column 1 keys preserve sections and repeated occurrence order", {
  rows <- list(
    list(values = c("Section A", ""), raw = "", is_header = FALSE),
    list(values = c("    Headache", "1"), raw = "", is_header = FALSE),
    list(values = c("    Headache", "2"), raw = "", is_header = FALSE),
    list(values = c("Section B", "7"), raw = "", is_header = FALSE),
    list(values = c("    Headache", "3"), raw = "", is_header = FALSE))
  keyed <- .change_body_rows(list(rows = rows))
  keys <- vapply(keyed, `[[`, character(1), "key")
  expect_equal(length(unique(keys)), 5L)
  expect_match(keys[2], "Headache.*1$")
  expect_match(keys[3], "Headache.*2$")
  expect_false(identical(keys[2], keys[5]))
})

test_that("footnote cosmetic spacing is ignored and exact Set 2 text is retained", {
  x <- .align_footnotes(c("Dose was 25-mg.  Next"), c("Dose was 25 - mg.\tNext"))
  expect_false(x$changed)
  expect_equal(x$values, "Dose was 25 - mg.\tNext")
  expect_equal(.cosmetic_footnote("a  b\tc ; d-e"),
               .cosmetic_footnote("a b c;d - e"))
})

test_that("material footnote spans receive minimal brackets", {
  expect_equal(
    .annotate_footnote("today is a great day July 31, 2025",
                       "Today is a rotten day July 31, 2026"),
    "(T)oday is a (rotten) day July 31, 202(6)")
  expect_equal(.annotate_footnote("abc def", "abc"), "abc(missing)")
  expect_equal(.annotate_footnote("abc", "abc xyz"), "abc (xyz)")
  expect_equal(.annotate_footnote("25 mg", "25-mg"), "25(-)mg")
})

test_that("whole added and missing footnotes are bracketed", {
  added <- .align_footnotes("same", c("same", "new footnote"))
  expect_true(added$changed)
  expect_equal(added$values, c("same", "(new footnote)"))
  removed <- .align_footnotes(c("same", "old footnote"), "same")
  expect_true(removed$changed)
  expect_equal(removed$values, c("same", "(missing)"))
})

test_that("inserted text is safely RTF escaped and decodes unchanged", {
  value <- "© ≥ − ° µ – é ü α β {x} \\"
  raw <- paste0("\\pard\\intbl\\plain\\f0\\fs18 ", .rtf_escape_insert(value), "\\cell")
  expect_equal(.rtf_decode_span(sub("\\\\cell$", "", raw))$text, value)
})

test_that("change decoder follows rendered RTF destinations and special controls", {
  raw <- paste0(
    "{\\*\\bkmkstart hidden-bookmark}",
    "Visible\\line Next\\tab Value\\emspace End"
  )
  decoded <- .rtf_decode_span(raw)
  expect_equal(decoded$text, "Visible\nNext\tValue\u2003End")
  expect_false(any(grepl("hidden-bookmark", decoded$text, fixed = TRUE)))

  replaced <- .rtf_replace_visible(raw, "Replacement")
  expect_match(replaced, "bkmkstart hidden-bookmark", fixed = TRUE)
  expect_equal(.rtf_decode_span(replaced)$text, "Replacement")
})

test_that("paired RTF outputs share one union row order with only-in markers", {
  gen <- new.env(parent = globalenv())
  sys.source(file.path(RTF_ROOT, "R", "generate_test_data.R"), gen)
  m1 <- gen$build_model(n_filler = 0L, seed = 41L)
  m2 <- m1
  find_row <- function(model, label) which(vapply(model, function(x)
    identical(x$kind, "row") && identical(x$cells[1], label), logical(1)))[1]
  dizziness <- find_row(m2, "    Dizziness")
  m2 <- m2[-dizziness]
  headache <- find_row(m2, "    Headache")
  added <- m2[[headache]]
  added$cells <- c("    New event", "50 (4%)", "14", "unchanged text", "2", "range")
  m2 <- append(m2, list(added), after = headache)

  d <- file.path(tempdir(), paste0("union_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  f1 <- file.path(d, "set1.rtf"); f2 <- file.path(d, "set2.rtf")
  writeLines(gen$render_rtf(m1, gen$STYLE_BASE), f1, useBytes = TRUE)
  writeLines(gen$render_rtf(m2, gen$STYLE_BASE), f2, useBytes = TRUE)
  result <- compare_rtf(f1, f2, console = FALSE)
  written <- write_change_rtf_pair(f1, f2, result, d)
  parsed <- lapply(written$output, parse_rtf)
  col1 <- lapply(parsed, function(x) x[col_index == 1L, raw_value])
  body <- lapply(col1, function(x) x[!grepl("\\n(?:Change|NC)$", x, perl = TRUE) &
                                       !grepl("^Table ", x)])
  expect_identical(body[[1]], body[[2]])
  expect_true(any(grepl("Dizziness \\(ONLY IN SET 1\\)$", body[[1]])))
  expect_true(any(grepl("New event \\(ONLY IN SET 2\\)$", body[[1]])))

  row2 <- parsed[[1]]$row_index[grepl("New event", parsed[[1]]$raw_value)][1]
  expect_equal(parsed[[1]][row_index == row2 & col_index > 1L, raw_value],
               rep("ONLY IN SET 2", 5L))
})
