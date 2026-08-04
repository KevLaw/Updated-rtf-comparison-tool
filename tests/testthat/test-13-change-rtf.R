# M13 -- optional RTF change-table output for the Windows and macOS single-file
# and folder workflows. The writer annotates copies of both source RTFs so
# their native page/table formatting is retained.

.new_change_fixture <- function(tag = "change") {
  source(file.path(RTF_ROOT, "R", "generate_test_data.R"), local = TRUE)
  d <- file.path(tempdir(), paste0(tag, "_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  files <- generate_test_data(d, n_filler = 4L, seed = 321L, verbose = FALSE)
  list(dir = d, files = files)
}

test_that("the existing test suite can generate valid tabular RTF output", {
  g <- .new_change_fixture("generator_capability")
  expect_true(all(file.exists(g$files)))
  parsed <- parse_rtf(g$files[["base"]])
  expect_gt(max(parsed$col_index), 1L)
  expect_true(any(grepl("Table 14.3.1.2", parsed$raw_value, fixed = TRUE)))
})

test_that("change RTF pair preserves formatting and writes delta-only cells", {
  g <- .new_change_fixture("change_pair")
  result <- compare_rtf(g$files[["base"]], g$files[["changed"]], console = FALSE)
  root <- file.path(tempdir(), paste0("change_tool_", as.integer(runif(1, 1, 1e9))))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  written <- write_change_rtf_pair(g$files[["base"]], g$files[["changed"]],
                                   result, root)
  expect_equal(written$set, c("Set 1", "Set 2"))
  expect_true(all(written$ok))
  expect_true(all(file.exists(written$output)))
  expect_equal(basename(written$output),
               c("clinical_table_base_change.rtf", "clinical_table_changed_change.rtf"))
  portable_output <- chartr("\\", "/", written$output)
  expect_match(portable_output[[1]], "logs/RTF Changes/Set 1", fixed = TRUE)
  expect_match(portable_output[[2]], "logs/RTF Changes/Set 2", fixed = TRUE)

  source_text <- .read_rtf_text(g$files[["base"]])
  output_text <- .read_rtf_text(written$output[[1]])
  # Page setup, font table, and widths remain source-native.
  expect_match(output_text,
               "\\paperw12240\\paperh15840\\margl1080\\margr1080", fixed = TRUE)
  expect_match(output_text,
               "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}", fixed = TRUE)
  expect_match(output_text, "\\cellx2880\\cellx4320\\cellx5760", fixed = TRUE)
  source_footnote <- "Note: Subjects are counted once within each System Organ Class and Preferred Term."
  expect_match(source_text, source_footnote, fixed = TRUE)
  expect_match(output_text, source_footnote, fixed = TRUE)

  parsed <- parse_rtf(written$output[[1]])
  expect_equal(parsed$raw_value[parsed$row_index == 1L], "Table 14.3.1.2_Change")
  headers <- parsed$raw_value[parsed$row_index == 5L]
  expect_true(all(grepl("\n(?:Change|NC)$", headers, perl = TRUE)))

  # Body values are replaced, not annotated. Direction is Set 2 minus Set 1.
  headache_row <- parsed$row_index[grepl("^    Headache", parsed$raw_value)][[1]]
  headache <- parsed[row_index == headache_row]
  expect_equal(headache$raw_value, c("    Headache", "(+1, +0.5%)", "NC",
                                     "(+1, +0.5%)", "NC", "NC"))
  expect_false(any(grepl("45 \\(21\\.4%\\)", headache$raw_value)))

  # A changed Column 1 identity becomes two explicit union rows.
  labels <- parsed$raw_value[parsed$col_index == 1L]
  expect_true(any(grepl("Fatigue \\(ONLY IN SET 1\\)$", labels)))
  expect_true(any(grepl("Fatigues \\(ONLY IN SET 2\\)$", labels)))
  expect_true(any(parsed$raw_value == "Footnote changes in brackets"))
  expect_true(any(grepl("Sponsor \\(Pharma\\)", parsed$raw_value)))
})

test_that("equivalent source tables use NC data and NC column headers", {
  g <- .new_change_fixture("no_change_pair")
  result <- compare_rtf(g$files[["base"]], g$files[["reformatted"]], console = FALSE)
  expect_true(result$equivalent)
  out <- tempfile(fileext = ".rtf")
  write_change_rtf(g$files[["base"]], result, out)
  parsed <- parse_rtf(out)
  headers <- parsed$raw_value[grepl("System Organ Class / Preferred Term", parsed$raw_value,
                                   fixed = TRUE)]
  expect_true(all(endsWith(headers, "\nNC")))
  expect_true(any(parsed$raw_value == "NC"))
  expect_false(any(parsed$raw_value == "No Change"))
})

test_that("every repeated table number receives the _Change suffix", {
  text <- paste("Table 14.3.1.2", "Table 14.3.1.2a", "Table 14.3-2", sep = "\\par ")
  changed <- .add_change_to_table_number(text)
  expect_equal(length(gregexpr("_Change", changed, fixed = TRUE)[[1]]), 3L)
  expect_match(changed, "Table 14.3.1.2a_Change", fixed = TRUE)
  expect_match(changed, "Table 14.3-2_Change", fixed = TRUE)
})

test_that("batch change writer emits both sets and skips unmatched files", {
  g <- .new_change_fixture("batch_changes")
  d1 <- file.path(g$dir, "folder1"); d2 <- file.path(g$dir, "folder2")
  dir.create(d1); dir.create(d2)
  file.copy(g$files[["base"]], file.path(d1, "table_a.rtf"))
  file.copy(g$files[["changed"]], file.path(d2, "table_a.rtf"))
  file.copy(g$files[["base"]], file.path(d1, "unmatched_alpha.rtf"))
  file.copy(fx("identical_B.rtf"), file.path(d2, "orphan_omega.rtf"))
  batch <- compare_rtf_folder(d1, d2, console = FALSE, progress = FALSE)
  root <- file.path(g$dir, "tool")

  # Even a malformed caller cannot smuggle an unmatched item into change
  # generation: eligibility comes from the comparison summary.
  batch$results[["unmatched_alpha.rtf"]] <- batch$results[["table_a.rtf"]]
  written <- write_batch_change_rtfs(batch, root)

  expect_equal(nrow(written), 2L)
  expect_true(all(written$ok))
  expect_equal(basename(written$output), rep("table_a_change.rtf", 2L))
  expect_false(any(grepl("unmatched_alpha", written$output, fixed = TRUE)))
  expect_false(any(grepl("orphan_omega", written$output, fixed = TRUE)))
  expect_false(file.exists(file.path(root, "logs", "RTF Changes", "Set 1",
                                     "unmatched_alpha_change.rtf")))
  expect_false(file.exists(file.path(root, "logs", "RTF Changes", "Set 2",
                                     "orphan_omega_change.rtf")))
})

test_that("batch change writer accounts for every officially compared pair", {
  d <- file.path(tempdir(), paste0("many_change_pairs_", as.integer(runif(1, 1, 1e9))))
  d1 <- file.path(d, "set1"); d2 <- file.path(d, "set2")
  dir.create(d1, recursive = TRUE); dir.create(d2, recursive = TRUE)

  pairs <- list(
    c("exact_table.rtf", "exact_table.rtf"),
    c("s0ae0by0outcompe0sei.rtf", "s0ae0by0outcompe0aeosi.rtf"),
    c("s0exp0sum.rtf", "s0exp0sum0bystudy.rtf")
  )
  for (pair in pairs) {
    file.copy(fx("value_diff_A.rtf"), file.path(d1, pair[[1]]))
    file.copy(fx("value_diff_B.rtf"), file.path(d2, pair[[2]]))
  }

  batch <- compare_rtf_folder(d1, d2, console = FALSE, progress = FALSE)
  expect_equal(nrow(batch$summary), 3L)
  expect_equal(length(batch$results), 3L)
  expect_true(all(batch$summary$status == "DIFFERENCES"))

  progress <- capture.output(
    written <- write_batch_change_rtfs(batch, file.path(d, "tool"), progress = TRUE)
  )
  expect_equal(nrow(written), 6L)
  expect_equal(length(unique(written$pair)), 3L)
  pair_counts <- table(written$pair)
  expect_equal(sort(names(pair_counts)), sort(batch$summary$file))
  expect_equal(unname(as.integer(pair_counts)), rep(2L, 3L))
  expect_true(all(written$ok))
  expect_true(all(file.exists(written$output)))
  expect_true(any(grepl("pair 1 of 3", progress, fixed = TRUE)))
  expect_true(any(grepl("pair 3 of 3", progress, fixed = TRUE)))

  expected_names <- sort(paste0(
    sub("\\.[Rr][Tt][Ff]$", "", unlist(pairs, use.names = FALSE)), "_change.rtf"
  ))
  expect_equal(sort(basename(written$output)), expected_names)

  # An inconsistent internal result must be reported as a failed official
  # pair; it must never be silently filtered out of the generated set.
  missing_name <- batch$summary$file[[2]]
  incomplete <- batch
  incomplete$results[[missing_name]] <- NULL
  retried <- write_batch_change_rtfs(incomplete, file.path(d, "incomplete_tool"))
  expect_equal(length(unique(retried$pair)), 3L)
  missing_row <- retried[retried$pair == missing_name, , drop = FALSE]
  expect_equal(nrow(missing_row), 1L)
  expect_false(missing_row$ok)
  expect_match(missing_row$error, "officially compared pair has no stored result", fixed = TRUE)
})

test_that("Word-incompatible content outside the root RTF group is rejected", {
  malformed <- "{\\rtf1\\ansi This text remains visible.\\par}not-in-root"
  expect_error(.validate_rtf_container(malformed, "malformed_change.rtf"),
               "content follows the root group", fixed = TRUE)

  balanced_but_invalid <- "{\\rtf1\\ansi First.}{\\rtf1\\ansi Second.}"
  expect_error(.validate_rtf_container(balanced_but_invalid, "two-roots.rtf"),
               "content follows the root group", fixed = TRUE)
})

test_that("RTF structural validation handles escaped braces and binary payloads", {
  valid <- paste0("{\\rtf1\\ansi escaped \\{ brace \\} slash \\\\ ",
                  "\\bin4 ", rawToChar(as.raw(c(123L, 125L, 92L, 0L))), "\\par}")
  expect_silent(.validate_rtf_container(valid, "binary.rtf"))
})

test_that("generated change RTFs render through the native macOS document converter", {
  skip_if_not(identical(Sys.info()[["sysname"]], "Darwin"))
  textutil <- Sys.which("textutil")
  skip_if(!nzchar(textutil), "macOS textutil is unavailable")

  d <- file.path(tempdir(), paste0("native_render_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  result <- compare_rtf(fx("value_diff_A.rtf"), fx("value_diff_B.rtf"), console = FALSE)
  written <- write_change_rtf_pair(fx("value_diff_A.rtf"), fx("value_diff_B.rtf"),
                                   result, d)
  for (i in seq_len(nrow(written))) {
    docx <- file.path(d, paste0("rendered-", i, ".docx"))
    output <- suppressWarnings(system2(
      textutil, c("-convert", "docx", "-output", shQuote(docx),
                  shQuote(written$output[[i]])),
      stdout = TRUE, stderr = TRUE
    ))
    status <- attr(output, "status"); if (is.null(status)) status <- 0L
    expect_equal(as.integer(status), 0L, info = paste(output, collapse = "\n"))
    expect_true(file.exists(docx))
    expect_gt(file.info(docx)$size, 0L)
  }
})

test_that("fuzzy filename pairs generate source-named change RTFs", {
  g <- .new_change_fixture("fuzzy_batch_changes")
  d1 <- file.path(g$dir, "fuzzy1"); d2 <- file.path(g$dir, "fuzzy2")
  dir.create(d1); dir.create(d2)
  name1 <- "s0ae0by0outcompe0sei.rtf"
  name2 <- "s0ae0by0outcompe0aeosi.rtf"
  file.copy(g$files[["base"]], file.path(d1, name1))
  file.copy(g$files[["changed"]], file.path(d2, name2))
  file.copy(g$files[["base"]], file.path(d1, "unmatched_alpha.rtf"))

  batch <- compare_rtf_folder(d1, d2, console = FALSE, progress = FALSE)
  root <- file.path(g$dir, "fuzzy_tool")
  written <- write_batch_change_rtfs(batch, root)

  expect_equal(nrow(written), 2L)
  expect_true(all(written$ok))
  expect_equal(basename(written$output),
               c("s0ae0by0outcompe0sei_change.rtf",
                 "s0ae0by0outcompe0aeosi_change.rtf"))
  expect_false(any(grepl("unmatched_alpha", written$output, fixed = TRUE)))
})

test_that("zero-token filename pairs generate source-named change RTFs", {
  g <- .new_change_fixture("zero_token_batch_changes")
  d1 <- file.path(g$dir, "zero1"); d2 <- file.path(g$dir, "zero2")
  dir.create(d1); dir.create(d2)
  name1 <- "s0exp0sum.rtf"
  name2 <- "s0exp0sum0bystudy.rtf"
  file.copy(g$files[["base"]], file.path(d1, name1))
  file.copy(g$files[["changed"]], file.path(d2, name2))

  batch <- compare_rtf_folder(d1, d2, console = FALSE, progress = FALSE)
  root <- file.path(g$dir, "zero_tool")
  written <- write_batch_change_rtfs(batch, root)

  expect_equal(batch$summary$status, "DIFFERENCES")
  expect_match(batch$summary$note, "0-token-matched", fixed = TRUE)
  expect_true(all(written$ok))
  expect_equal(basename(written$output),
               c("s0exp0sum_change.rtf", "s0exp0sum0bystudy_change.rtf"))
})

test_that("content-only pairs generate source-named change RTFs", {
  g <- .new_change_fixture("content_only_batch_changes")
  d1 <- file.path(g$dir, "content1"); d2 <- file.path(g$dir, "content2")
  dir.create(d1); dir.create(d2)
  name1 <- "alpha_source.rtf"
  name2 <- "unrelated_filename.rtf"
  file.copy(g$files[["base"]], file.path(d1, name1))
  file.copy(g$files[["changed"]], file.path(d2, name2))

  batch <- compare_rtf_folder(d1, d2, console = FALSE, progress = FALSE)
  root <- file.path(g$dir, "content_tool")
  written <- write_batch_change_rtfs(batch, root)

  expect_equal(batch$summary$status, "DIFFERENCES")
  expect_match(batch$summary$note, "Content-matched", fixed = TRUE)
  expect_true(all(written$ok))
  expect_equal(basename(written$output),
               c("alpha_source_change.rtf", "unrelated_filename_change.rtf"))
})

test_that("Windows and macOS runners contain the required final change-table prompt", {
  required <- paste0(
    "Would you like a separate set of RTF tables generated showing only the ",
    "differences or no differences identified line by line?")
  for (script in c("run_compare_paths.R", "run_compare.R", "run_compare_folder.R")) {
    text <- paste(readLines(file.path(RTF_ROOT, "R", script), warn = FALSE), collapse = " ")
    expect_match(text, "Would you like a separate set of RTF tables generated", fixed = TRUE)
    expect_match(text, "differences or no differences identified line by line?", fixed = TRUE)
    expect_match(text, "Darwin", fixed = TRUE)
  }
  expect_true(nzchar(required))
})

test_that("macOS path workflow offers the prompt and generates review RTFs", {
  skip_if_not(identical(Sys.info()[["sysname"]], "Darwin"))
  rscript <- file.path(R.home("bin"), "Rscript")
  troot <- file.path(tempdir(), paste0("mac_change_runner_", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(troot, "R"), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(RTF_ROOT, "R", "compare_rtf.R"), file.path(troot, "R"))
  file.copy(file.path(RTF_ROOT, "R", "run_compare_paths.R"), file.path(troot, "R"))
  file.copy(file.path(RTF_ROOT, "tests", "testthat", "fixtures", "value_diff_A.rtf"),
            file.path(troot, "set1.rtf"))
  file.copy(file.path(RTF_ROOT, "tests", "testthat", "fixtures", "value_diff_B.rtf"),
            file.path(troot, "set2.rtf"))

  prior_change_setting <- Sys.getenv("RTF_GENERATE_CHANGES", unset = NA_character_)
  on.exit({
    if (is.na(prior_change_setting)) Sys.unsetenv("RTF_GENERATE_CHANGES")
    else Sys.setenv(RTF_GENERATE_CHANGES = prior_change_setting)
  }, add = TRUE)
  Sys.unsetenv("RTF_GENERATE_CHANGES")

  input <- file.path(troot, "input.txt")
  writeLines(c(file.path(troot, "set1.rtf"), file.path(troot, "set2.rtf"),
               "", "y", ""), input)
  out <- suppressWarnings(system2(
    rscript, file.path(troot, "R", "run_compare_paths.R"),
    stdin = input, stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status"); if (is.null(st)) st <- 0L

  expect_equal(as.integer(st), 1L)
  expect_true(any(grepl("Would you like a separate set of RTF tables", out,
                        fixed = TRUE)))
  expect_true(file.exists(file.path(troot, "logs", "RTF Changes", "Set 1",
                                    "set1_change.rtf")))
  expect_true(file.exists(file.path(troot, "logs", "RTF Changes", "Set 2",
                                    "set2_change.rtf")))
})

test_that("single-file picker runner can generate change RTFs end to end", {
  rscript <- file.path(R.home("bin"), "Rscript")
  troot <- file.path(tempdir(), paste0("change_runner_", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(troot, "R"), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(RTF_ROOT, "R", "compare_rtf.R"), file.path(troot, "R"))
  file.copy(file.path(RTF_ROOT, "R", "run_compare.R"), file.path(troot, "R"))
  file.copy(file.path(RTF_ROOT, "tests", "testthat", "fixtures", "value_diff_A.rtf"),
            file.path(troot, "set1.rtf"))
  file.copy(file.path(RTF_ROOT, "tests", "testthat", "fixtures", "value_diff_B.rtf"),
            file.path(troot, "set2.rtf"))

  runner <- file.path(troot, "R", "run_compare.R")
  prior_change_setting <- Sys.getenv("RTF_GENERATE_CHANGES", unset = NA_character_)
  on.exit({
    if (is.na(prior_change_setting)) Sys.unsetenv("RTF_GENERATE_CHANGES")
    else Sys.setenv(RTF_GENERATE_CHANGES = prior_change_setting)
  }, add = TRUE)
  # Set the parent process environment so the child inherits it. Passing an
  # `env` argument to system2() is not portable to the Windows runner.
  Sys.setenv(RTF_GENERATE_CHANGES = "yes")
  out <- suppressWarnings(system2(
    rscript, c(runner, file.path(troot, "set1.rtf"), file.path(troot, "set2.rtf")),
    stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status"); if (is.null(st)) st <- 0L
  expect_equal(as.integer(st), 1L)
  expect_true(file.exists(file.path(troot, "logs", "RTF Changes", "Set 1",
                                    "set1_change.rtf")))
  expect_true(file.exists(file.path(troot, "logs", "RTF Changes", "Set 2",
                                    "set2_change.rtf")))
})
