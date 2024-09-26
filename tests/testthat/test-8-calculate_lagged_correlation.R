test_that("calculate_lagged_correlation_works", {
  data("step_data", package = "laggedcor")
  data("heart_data", package = "laggedcor")

  fpath <- testthat::test_path("test-data/calculate_lagged_correlation_result.rds")
  expected_res <- readRDS(fpath)

  x <- step_data$step
  time1 <- step_data$time

  y <- heart_data$heart
  time2 <- heart_data$time

  start_time <- Sys.time()
  result <-
    calculate_lagged_correlation(
      x = x,
      y = y,
      time1 = time1,
      time2 = time2,
      time_tol = 0.2,
      step = 2 / 60,
      min_matched_sample = 10,
      threads = 16,
      cor_method = "spearman"
    )
  end_time <- Sys.time()
  testthat_print(end_time - start_time)

  expect_s3_class(result, "lagged_cor_result")
  max_cor <- extract_max_cor(object = result)
  global_cor <- extract_global_cor(object = result)

  # TODO: make the test for values better
  expect_equal(max_cor, extract_max_cor(object = expected_res))
  expect_equal(global_cor, extract_global_cor(object = expected_res))
})
