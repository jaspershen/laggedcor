test_that("data smoothing works", {
  data("step_data", package = "laggedcor")
  data("heart_data", package = "laggedcor")

  res1 <- smooth_data(
    x = step_data$step,
    time = step_data$time,
    should_plot = TRUE
  )

  expect_is(res1, "list")
  expect_is(res1$plot, "gg")
  expect_equal(res1$original, step_data$step)
  expect_equal(length(res1$smoothed), length(step_data$step))

  res2 <- smooth_data(
    x = heart_data$heart,
    time = heart_data$time,
    span = 0.1,
    should_plot = FALSE
  )

  # expect res2$plot to be NULL
  expect_is(res2, "list")
  expect_is(res2$plot, "NULL")
  expect_equal(res2$original, heart_data$heart)
  expect_equal(length(res2$smoothed), length(heart_data$heart))
})

test_that("data smoothing fails when x and time have different lengths", {
  data("step_data", package = "laggedcor")

  expect_error(smooth_data(
    x = step_data$step,
    time = step_data$time[1:10],
    span = 0.1,
    should_plot = TRUE
  ), "The length of x and time must be the same.")
})
