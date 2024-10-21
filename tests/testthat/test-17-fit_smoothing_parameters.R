test_that("fit smoothing parameters work", {
  data("step_data", package = "laggedcor")
  data("heart_data", package = "laggedcor")
  best_params <- fit_smoothing_parameters(x = step_data$step, time = step_data$time)

  # expect is list
  expect_is(best_params, "list")
  # expect has span and degree
  expect_named(best_params, c("span", "degree"))
  
})
