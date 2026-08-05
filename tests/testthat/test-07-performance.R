# M7 -- performance: a large generated file compares in reasonable time using
# the data.table keyed join. Size is configurable via RTF_PERF_ROWS (default
# 3000 filler rows) so the routine suite stays brisk; set it higher (e.g.
# 50000) for a heavier check.

test_that("a large file compares in reasonable time and correctly", {
  source(file.path(RTF_ROOT, "R", "generate_test_data.R"), local = TRUE)
  n <- as.integer(Sys.getenv("RTF_PERF_ROWS", "3000"))
  d <- file.path(tempdir(), paste0("perf_", n)); dir.create(d, showWarnings = FALSE)
  files <- generate_test_data(d, n_filler = n, verbose = FALSE)

  timing <- system.time(
    r <- compare_rtf(files["base"], files["reformatted"], console = FALSE))

  expect_true(r$equivalent)
  expect_gt(r$n_cells, n * 6L)             # at least the filler rows x 6 columns
  # The hosted Windows runner has materially slower RTF decoding and variable
  # shared-runner load.  Preserve a strict guard while avoiding false failures
  # for correct runs like the observed 135-second comparison.
  limit <- if (identical(Sys.info()[["sysname"]], "Windows")) 180 else 120
  expect_lt(timing[["elapsed"]], limit)
  message(sprintf("  [perf] %s cells compared in %.1fs (filler rows = %d)",
                  format(r$n_cells, big.mark = ","), timing[["elapsed"]], n))
})
