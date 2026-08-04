# M11 -- batch comparison of two FOLDERS of RTF files (compare_rtf_folder,
# write_batch_report) and the folder-picker runner (run_compare_folder.R).

# Build a fresh pair of folders under tempdir() and populate them from the
# existing single-file fixtures. Returns the two folder paths.
new_pair <- function(tag) {
  base <- file.path(tempdir(), paste0("batch_", tag, "_", as.integer(runif(1, 1, 1e9))))
  d1 <- file.path(base, "folder1"); d2 <- file.path(base, "folder2")
  dir.create(d1, recursive = TRUE, showWarnings = FALSE)
  dir.create(d2, recursive = TRUE, showWarnings = FALSE)
  list(d1 = d1, d2 = d2)
}
put <- function(dir, name, fixture) file.copy(fx(fixture), file.path(dir, name))
write_one_cell_rtf <- function(path, value) {
  lines <- c(
    "{\\rtf1\\ansi\\ansicpg1252\\deff0",
    "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}}",
    "\\trowd\\trgaph108\\trleft0",
    "\\cellx12000",
    paste0("\\pard\\intbl\\plain\\f0\\fs18 ", value, "\\cell"),
    "\\row",
    "}"
  )
  writeLines(lines, path, useBytes = TRUE)
}
write_content_match_rtf <- function(path, title, descriptions) {
  row <- function(values) c(
    "\\trowd\\trgaph108\\trleft0\\cellx6000\\cellx12000",
    paste0("\\pard\\intbl\\plain\\f0\\fs18 ", values[[1]], "\\cell"),
    paste0("\\pard\\intbl\\plain\\f0\\fs18 ", values[[2]], "\\cell\\row"))
  lines <- c(
    "{\\rtf1\\ansi\\ansicpg1252\\deff0",
    "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}}",
    paste0("\\pard\\plain\\f0\\fs18 ", title, "\\par"),
    row(c("Description", "Value")),
    unlist(lapply(descriptions, function(x) row(c(x, "1"))), use.names = FALSE),
    "}"
  )
  writeLines(lines, path, useBytes = TRUE)
}

test_that("every file is reported, including ones with NO differences", {
  p <- new_pair("mixed")
  put(p$d1, "same.rtf",  "identical_A.rtf");  put(p$d2, "same.rtf",  "identical_B.rtf")
  put(p$d1, "diff.rtf",  "value_diff_A.rtf"); put(p$d2, "diff.rtf",  "value_diff_B.rtf")
  write_content_match_rtf(file.path(p$d1, "only1.rtf"), "Adverse Events",
                          c("Headache", "Nausea"))
  write_content_match_rtf(file.path(p$d2, "only2.rtf"), "Demographics",
                          c("Age", "Sex"))

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)

  expect_equal(nrow(b$summary), 4L)
  st <- setNames(b$summary$status, b$summary$file)
  expect_equal(st[["same.rtf"]],  "EQUIVALENT")          # the no-difference file IS listed
  expect_equal(st[["diff.rtf"]],  "DIFFERENCES")
  expect_equal(st[["only1.rtf"]], "ONLY_IN_FOLDER1")
  expect_equal(st[["only2.rtf"]], "ONLY_IN_FOLDER2")
  expect_equal(b$summary$n_diffs[b$summary$file == "diff.rtf"], 1L)
  expect_false(b$all_equivalent)
})

test_that("totals are correct and add up", {
  p <- new_pair("totals")
  put(p$d1, "same.rtf",  "identical_A.rtf");  put(p$d2, "same.rtf",  "identical_B.rtf")
  put(p$d1, "diff.rtf",  "value_diff_A.rtf"); put(p$d2, "diff.rtf",  "value_diff_B.rtf")
  put(p$d1, "only1.rtf", "identical_A.rtf")

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)
  t <- b$totals
  expect_equal(t$n_files, 3L)
  expect_equal(t$n_equivalent, 1L)
  expect_equal(t$n_differing, 1L)
  expect_equal(t$n_only1, 1L)
  expect_equal(t$n_only2, 0L)
  expect_equal(t$total_diffs, 1L)
})

test_that("all-matching folders report all_equivalent = TRUE", {
  p <- new_pair("allmatch")
  put(p$d1, "a.rtf", "identical_A.rtf"); put(p$d2, "a.rtf", "identical_B.rtf")
  put(p$d1, "b.rtf", "identical_A.rtf"); put(p$d2, "b.rtf", "identical_B.rtf")

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)
  expect_true(b$all_equivalent)
  expect_equal(b$totals$n_equivalent, 2L)
  expect_true(all(b$summary$status == "EQUIVALENT"))
})

test_that("batch pairing recognizes filenames with case and edge-space differences", {
  p <- new_pair("normalized_names")
  put(p$d1, " Patient Listing.RTF ", "identical_A.rtf")
  put(p$d2, "patient listing.rtf", "identical_B.rtf")

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)

  expect_true(b$all_equivalent)
  expect_equal(nrow(b$summary), 1L)
  expect_equal(b$summary$status, "EQUIVALENT")
  expect_match(b$summary$note, "filename normalization")
  expect_equal(b$totals$n_only1 + b$totals$n_only2, 0L)
})

test_that("batch pairing recognizes the requested like-name example", {
  p <- new_pair("fuzzy_requested_example")
  name1 <- "s0ae0by0outcompe0sei.rtf"
  name2 <- "s0ae0by0outcompe0aeosi.rtf"
  put(p$d1, name1, "value_diff_A.rtf")
  put(p$d2, name2, "value_diff_B.rtf")

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)

  expect_equal(nrow(b$summary), 1L)
  expect_equal(b$summary$file, name1)
  expect_equal(b$summary$status, "DIFFERENCES")
  expect_match(b$summary$note, name2, fixed = TRUE)
  expect_match(b$summary$note, "edit distance 3", fixed = TRUE)
  expect_match(b$summary$note, "86.4%", fixed = TRUE)
  expect_equal(b$totals$n_only1 + b$totals$n_only2, 0L)
  expect_equal(b$results[[name1]]$file2_name, name2)
})

test_that("a unique one-letter difference in long filenames is paired", {
  p <- new_pair("fuzzy_one_letter")
  put(p$d1, "subject_outcome_a.rtf", "identical_A.rtf")
  put(p$d2, "subject_outcome_b.rtf", "identical_B.rtf")

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)

  expect_true(b$all_equivalent)
  expect_equal(nrow(b$summary), 1L)
  expect_match(b$summary$note, "Fuzzy-matched", fixed = TRUE)
  expect_match(b$summary$note, "edit distance 1", fixed = TRUE)
})

test_that("zero-separated family tokens identify the requested prefix example", {
  p <- new_pair("zero_token_prefix")
  put(p$d1, "s0exp0sum.rtf", "identical_A.rtf")
  put(p$d2, "s0exp0sum0bystudy.rtf", "identical_B.rtf")

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)

  expect_true(b$all_equivalent)
  expect_equal(nrow(b$summary), 1L)
  expect_equal(b$results[["s0exp0sum.rtf"]]$file2_name,
               "s0exp0sum0bystudy.rtf")
  expect_match(b$summary$note, "0-token-matched", fixed = TRUE)
  expect_match(b$summary$note, "token 80.0%", fixed = TRUE)
  report <- write_batch_report(b, console = FALSE)
  expect_true(any(grepl("0-token-matched", report, fixed = TRUE)))
})

test_that("zero-token candidates still require supporting table content", {
  p <- new_pair("zero_token_content_reject")
  write_content_match_rtf(file.path(p$d1, "s0exp0sum.rtf"), "Adverse Events",
                          c("Headache", "Nausea"))
  write_content_match_rtf(file.path(p$d2, "s0exp0sum0bystudy.rtf"), "Demographics",
                          c("Age", "Sex"))

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)

  expect_equal(b$totals$n_only1, 1L)
  expect_equal(b$totals$n_only2, 1L)
  expect_length(b$results, 0L)
  expect_true(all(grepl("rejected by content check", b$summary$note, fixed = TRUE)))
})

test_that("rendered content chooses the correct fuzzy filename candidate", {
  p <- new_pair("fuzzy_content_choice")
  write_content_match_rtf(file.path(p$d1, "clinical_report_x.rtf"),
                          "Adverse Events by Preferred Term",
                          c("Headache", "Nausea", "Dizziness"))
  write_content_match_rtf(file.path(p$d2, "clinical_report_a.rtf"),
                          "Demographic Characteristics",
                          c("Age", "Sex", "Race"))
  write_content_match_rtf(file.path(p$d2, "clinical_report_b.rtf"),
                          "Adverse Events by Preferred Term",
                          c("Headache", "Nausea", "Fatigue"))

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)
  st <- setNames(b$summary$status, b$summary$file)

  expect_equal(st[["clinical_report_x.rtf"]], "DIFFERENCES")
  expect_equal(b$results[["clinical_report_x.rtf"]]$file2_name,
               "clinical_report_b.rtf")
  expect_equal(st[["clinical_report_a.rtf"]], "ONLY_IN_FOLDER2")
  expect_match(b$summary$note[b$summary$file == "clinical_report_x.rtf"],
               "title 100.0%, Column 1 66.7%", fixed = TRUE)
})

test_that("similar filenames with unrelated rendered content are not paired", {
  p <- new_pair("fuzzy_content_reject")
  write_content_match_rtf(file.path(p$d1, "clinical_output_a.rtf"),
                          "Adverse Events by Preferred Term",
                          c("Headache", "Nausea", "Dizziness"))
  write_content_match_rtf(file.path(p$d2, "clinical_output_b.rtf"),
                          "Demographic Characteristics",
                          c("Age", "Sex", "Race"))

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)

  expect_equal(nrow(b$summary), 2L)
  expect_equal(b$totals$n_only1, 1L)
  expect_equal(b$totals$n_only2, 1L)
  expect_length(b$results, 0L)
  expect_true(all(grepl("rejected by content check", b$summary$note, fixed = TRUE)))
  expect_true(all(grepl("Column 1 0.0%", b$summary$note, fixed = TRUE)))
})

test_that("exact filename matches take priority over fuzzy alternatives", {
  p <- new_pair("fuzzy_exact_priority")
  put(p$d1, "clinical_table_alpha.rtf", "identical_A.rtf")
  put(p$d2, "clinical_table_alpha.rtf", "identical_B.rtf")
  put(p$d2, "clinical_table_alphb.rtf", "identical_B.rtf")

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)
  st <- setNames(b$summary$status, b$summary$file)

  expect_equal(st[["clinical_table_alpha.rtf"]], "EQUIVALENT")
  expect_equal(st[["clinical_table_alphb.rtf"]], "ONLY_IN_FOLDER2")
  expect_equal(b$totals$n_equivalent, 1L)
  expect_equal(b$totals$n_only2, 1L)
})

test_that("ambiguous fuzzy candidates remain unmatched", {
  p <- new_pair("fuzzy_ambiguous")
  put(p$d1, "clinical_table_x.rtf", "identical_A.rtf")
  put(p$d2, "clinical_table_a.rtf", "identical_B.rtf")
  put(p$d2, "clinical_table_b.rtf", "identical_B.rtf")

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)

  expect_equal(nrow(b$summary), 3L)
  expect_equal(b$totals$n_only1, 1L)
  expect_equal(b$totals$n_only2, 2L)
  expect_length(b$results, 0L)
})

test_that("weak filename similarity can match through the content-only fallback", {
  p <- new_pair("fuzzy_weak")
  put(p$d1, "clinical_adverse_events.rtf", "identical_A.rtf")
  put(p$d2, "clinical_efficacy_table.rtf", "identical_B.rtf")

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)

  expect_true(b$all_equivalent)
  expect_equal(nrow(b$summary), 1L)
  expect_equal(b$results[["clinical_adverse_events.rtf"]]$file2_name,
               "clinical_efficacy_table.rtf")
  expect_match(b$summary$note, "Content-matched", fixed = TRUE)
})

test_that("content-only fallback checks unmatched RTFs across relative subfolders", {
  p <- new_pair("fuzzy_subfolder_boundary")
  dir.create(file.path(p$d1, "tables"))
  dir.create(file.path(p$d2, "listings"))
  put(p$d1, file.path("tables", "subject_outcome_a.rtf"), "identical_A.rtf")
  put(p$d2, file.path("listings", "subject_outcome_b.rtf"), "identical_B.rtf")

  b <- compare_rtf_folder(p$d1, p$d2, recursive = TRUE,
                          console = FALSE, progress = FALSE)

  expect_true(b$all_equivalent)
  expect_equal(nrow(b$summary), 1L)
  expect_equal(b$results[[file.path("tables", "subject_outcome_a.rtf")]]$file2_name,
               file.path("listings", "subject_outcome_b.rtf"))
  expect_match(b$summary$note, "Content-matched", fixed = TRUE)
})

test_that("content-only fallback chooses a unique reciprocal match", {
  p <- new_pair("content_only_unique")
  write_content_match_rtf(file.path(p$d1, "alpha_source.rtf"),
                          "Adverse Events by Preferred Term",
                          c("Headache", "Nausea", "Dizziness"))
  write_content_match_rtf(file.path(p$d2, "unrelated_filename.rtf"),
                          "Adverse Events by Preferred Term",
                          c("Headache", "Nausea", "Dizziness"))
  write_content_match_rtf(file.path(p$d2, "demographic_output.rtf"),
                          "Demographic Characteristics", c("Age", "Sex", "Race"))

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)
  st <- setNames(b$summary$status, b$summary$file)

  expect_equal(st[["alpha_source.rtf"]], "EQUIVALENT")
  expect_equal(b$results[["alpha_source.rtf"]]$file2_name, "unrelated_filename.rtf")
  expect_equal(st[["demographic_output.rtf"]], "ONLY_IN_FOLDER2")
  expect_match(b$summary$note[b$summary$file == "alpha_source.rtf"],
               "after all-unmatched search", fixed = TRUE)
  report <- write_batch_report(b, console = FALSE)
  expect_true(any(grepl("Content-matched", report, fixed = TRUE)))
})

test_that("no content fallback match is explicitly reported", {
  p <- new_pair("content_only_none")
  write_content_match_rtf(file.path(p$d1, "alpha_source.rtf"), "Adverse Events",
                          c("Headache", "Nausea"))
  write_content_match_rtf(file.path(p$d2, "demographic_output.rtf"), "Demographics",
                          c("Age", "Sex"))

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)

  expect_equal(nrow(b$summary), 2L)
  expect_length(b$results, 0L)
  expect_true(all(grepl("No exact, 0-token, or safe content match found",
                        b$summary$note, fixed = TRUE)))
  expect_true(all(grepl("checking 1 unmatched", b$summary$note, fixed = TRUE)))
})

test_that("an unreadable file becomes an ERROR row without aborting the batch", {
  p <- new_pair("err")
  put(p$d1, "ok.rtf",  "identical_A.rtf"); put(p$d2, "ok.rtf",  "identical_B.rtf")
  # The two bad files must differ in bytes, or the byte-identical fast path would
  # call them EQUIVALENT without ever parsing (and so never hit the RTF check).
  writeLines("this is not an RTF file", file.path(p$d1, "bad.rtf"))
  writeLines("this is a different non-RTF file", file.path(p$d2, "bad.rtf"))

  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)
  expect_equal(nrow(b$summary), 2L)
  expect_equal(b$summary$status[b$summary$file == "bad.rtf"], "ERROR")
  expect_equal(b$summary$status[b$summary$file == "ok.rtf"], "EQUIVALENT")  # batch carried on
  expect_equal(b$totals$n_errors, 1L)
})

test_that("compare_rtf_folder errors on a missing folder and on empty folders", {
  p <- new_pair("empty")
  expect_error(compare_rtf_folder(file.path(p$d1, "nope"), p$d2, console = FALSE),
               "Folder not found")
  expect_error(compare_rtf_folder(p$d1, p$d2, console = FALSE), "No .rtf files")
})

test_that("write_batch_report lists every file (incl. equivalent) in TXT and CSV", {
  p <- new_pair("report")
  put(p$d1, "same.rtf", "identical_A.rtf");  put(p$d2, "same.rtf", "identical_B.rtf")
  put(p$d1, "diff.rtf", "value_diff_A.rtf"); put(p$d2, "diff.rtf", "value_diff_B.rtf")
  b <- compare_rtf_folder(p$d1, p$d2, console = FALSE, progress = FALSE)

  txt <- tempfile(fileext = ".txt"); csv <- tempfile(fileext = ".csv")
  on.exit(unlink(c(txt, csv)), add = TRUE)
  write_batch_report(b, txt_path = txt, csv_path = csv, console = FALSE)

  lines <- readLines(txt)
  expect_true(any(grepl("RTF BATCH COMPARISON REPORT", lines)))
  expect_true(any(grepl("^same\\.rtf\\s+EQUIVALENT", lines)))        # match is listed
  expect_true(any(grepl("diff\\.rtf", lines) & grepl("difference", lines, ignore.case = TRUE)))
  expect_true(any(grepl("VALUE_DIFF", lines)))                       # detail block present

  d <- data.table::fread(csv)
  expect_equal(sort(d$file), c("diff.rtf", "same.rtf"))              # both rows, match included
  expect_equal(names(d), c("file", "status", "n_cells", "n_diffs", "note",
                           "row_index", "col_index", "diff_status",
                           "value_file1", "value_file2"))
  expect_equal(d$diff_status[d$file == "diff.rtf"], "VALUE_DIFF")
  expect_true(is.na(d$value_file1[d$file == "same.rtf"]))
})

test_that("the folder runner writes an audit log + archived report end-to-end", {
  # Run run_compare_folder.R from an isolated copy of the tool so its logs/
  # folder lands in a temp root (not the real repo).
  rscript <- file.path(R.home("bin"), "Rscript")
  troot <- file.path(tempdir(), paste0("toolroot_", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(troot, "R"), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(RTF_ROOT, "R", "compare_rtf.R"),        file.path(troot, "R"))
  file.copy(file.path(RTF_ROOT, "R", "run_compare_folder.R"), file.path(troot, "R"))

  p <- new_pair("runner")
  put(p$d1, "same.rtf", "identical_A.rtf");  put(p$d2, "same.rtf", "identical_B.rtf")
  put(p$d1, "diff.rtf", "value_diff_A.rtf"); put(p$d2, "diff.rtf", "value_diff_B.rtf")

  runner <- file.path(troot, "R", "run_compare_folder.R")
  out <- suppressWarnings(system2(rscript, c(runner, p$d1, p$d2),
                                  stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status"); if (is.null(st)) st <- 0L
  expect_equal(as.integer(st), 1L)                                  # differences -> exit 1

  log <- file.path(troot, "logs", "audit_log.csv")
  expect_true(file.exists(log))
  a <- data.table::fread(log)
  expect_equal(nrow(a), 1L)
  expect_equal(a$run_type, "batch")
  expect_equal(a$files_compared, 2L)
  expect_equal(a$files_equivalent, 1L)
  expect_equal(a$files_differing, 1L)

  reports <- list.files(file.path(troot, "logs", "reports"), pattern = "\\.txt$")
  expect_true(length(reports) >= 1L)
})

test_that("batch runner exports full difference values without ellipsis end-to-end", {
  rscript <- file.path(R.home("bin"), "Rscript")
  troot <- file.path(tempdir(), paste0("toolroot_long_", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(troot, "R"), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(RTF_ROOT, "R", "compare_rtf.R"),        file.path(troot, "R"))
  file.copy(file.path(RTF_ROOT, "R", "run_compare_folder.R"), file.path(troot, "R"))

  p <- new_pair("runner_long")
  long_a <- paste(rep("alpha-full-row-value", 12L), collapse = " ")
  long_b <- paste(rep("beta-full-row-value", 12L), collapse = " ")
  write_one_cell_rtf(file.path(p$d1, "Long Difference.RTF"), long_a)
  write_one_cell_rtf(file.path(p$d2, "long difference.rtf"), long_b)

  runner <- file.path(troot, "R", "run_compare_folder.R")
  out <- suppressWarnings(system2(rscript, c(runner, p$d1, p$d2),
                                  stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status"); if (is.null(st)) st <- 0L
  expect_equal(as.integer(st), 1L)

  report_dir <- file.path(troot, "logs", "reports")
  txt <- file.path(report_dir, list.files(report_dir, pattern = "\\.txt$", full.names = FALSE)[[1]])
  csv <- file.path(report_dir, list.files(report_dir, pattern = "\\.csv$", full.names = FALSE)[[1]])
  text <- paste(readLines(txt, warn = FALSE), collapse = "\n")
  d <- data.table::fread(csv)

  expect_match(text, long_a, fixed = TRUE)
  expect_match(text, long_b, fixed = TRUE)
  expect_false(grepl(intToUtf8(8230L), text, fixed = TRUE))
  expect_equal(d$value_file1[d$diff_status == "VALUE_DIFF"], long_a)
  expect_equal(d$value_file2[d$diff_status == "VALUE_DIFF"], long_b)
})
