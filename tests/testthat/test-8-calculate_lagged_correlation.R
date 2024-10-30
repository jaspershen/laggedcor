generate_time_series <- function(seed = 123, lag_minutes = 10) {
  set.seed(seed = seed)
  # Generate time series parameters
  n_hours <- 24  # 24 hours of data
  freq <- 1 / 60   # One observation per minute
  n <- n_hours / freq
  # lag_minutes <- 10  # Lag in minutes
  
  # Generate time sequence
  time_seq <- seq(
    from = ymd_hms("2024-01-01 00:00:00"),
    by = sprintf("%d sec", 60),
    # One minute intervals
    length.out = n
  )
  
  # Generate base signal (combination of sine waves for complexity)
  base_signal <- sin(2 * pi * seq_len(n + lag_minutes) / (60 * 4)) +  # 4-hour cycle
    0.5 * sin(2 * pi * seq_len(n + lag_minutes) / (60 * 1)) +  # 1-hour cycle
    0.3 * sin(2 * pi * seq_len(n + lag_minutes) / (60 * 12))   # 12-hour cycle
  
  # Add different types of noise
  generate_noise <- function(n, sd = 0.1) {
    # Combine white noise and random walk
    white_noise <- rnorm(n, mean = 0, sd = sd)
    random_walk <- cumsum(rnorm(n, mean = 0, sd = sd / 5))
    # Normalize random walk
    random_walk <- random_walk * (sd / sd(random_walk))
    return(white_noise + random_walk)
  }
  
  # Create the two time series with lag
  ts1 <- base_signal + generate_noise(n + lag_minutes, sd = 0.2)
  ts1 <- ts1[(lag_minutes + 1):(n + lag_minutes)]
  ts2 <- base_signal[1:n] + generate_noise(n, sd = 0.2)
  
  # Create data frame
  return(list(
    x = ts1,
    y = ts2,
    time1 = time_seq,
    time2 = time_seq
  ))
}



test_that("calculate_lagged_correlation_works", {
  start_time <- Sys.time()
  
  lag_minutes = 5
  
  test = generate_time_series(lag_minutes = lag_minutes)
  
  result <-
    calculate_lagged_correlation(
      x = test$x,
      y = test$y,
      time1 = test$time1,
      time2 = test$time2,
      time_tol = 0.5,
      step = 1 / 60,
      min_matched_sample = 10,
      threads = 16,
      align_method = "linear",
      cor_method = "spearman"
    )
  end_time <- Sys.time()
  testthat_print(end_time - start_time)
  
  expect_s4_class(result, "lagged_cor_result")
  max_cor <- extract_max_cor(object = result)
  shift_time_num <- extract_shift_time(object = result)
  true_shift_idx <- which(shift_time_num == -lag_minutes)[1]
  true_shift_time <- result@shift_time[true_shift_idx]
  testthat::expect_equal(names(max_cor), true_shift_time)
})
