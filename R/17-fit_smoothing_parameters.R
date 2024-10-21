#' Fit parameters for LOESS data smoothing
#'
#' This function fits the span and degree parameters for the LOESS data smoothing method
#' by testing different combinations of span and degree values and selecting the best
#' parameters based on the R² value.
#'
#' @param x A numeric vector of values to be smoothed.
#' @param time A numeric vector of timestamps corresponding to `x`. (POSIXct)
#' @param seed An integer for the random seed (default is 123).
#' @param span_values A numeric vector of span values to test (default is seq(0.1, 0.5, by = 0.01)).
#' @param degree_values A numeric vector of degree values to test (default is 1:2).
#'
#' @return A list of the best span and degree parameters for the LOESS data smoothing method.
#'
#' @examples
#' data("test_data", package = "laggedcor")
#' best_params <- fit_smoothing_parameters(data = test_data, time_var = "time", value_var = "value", dataset_name = "test")
#' best_params
#'
#' @export
#'
#' #
#' @author Zirui Qiang \email{{zirui_qiang@outlook.com}}
#' @author Minghang Li \email{{minghli@ethz.ch}}
fit_smoothing_parameters <- function(x,
                                     time,
                                     threads = 10,
                                     span_values = seq(0.1, 0.5, by = 0.01),
                                     degree_values = 1:2,
                                     progressbar = TRUE,
                                     seed = 123) {
  # for reproducibility
  set.seed(seed)
  
  # Sort the time series data by time variable
  data <- data.frame(time = time, x = x)
  data <- data %>%
    dplyr::arrange(time)
  
  # Select the first third of the dataset for testing
  n <- nrow(data)
  test_data <- data[1:floor(n / 3), ]
  
  # Ensure that there are no NA values and x > 0
  test_data <- test_data %>%
    filter(!is.na(x) & x > 0)
  
  message("Number of valid observations in test_data:",
          nrow(test_data),
          "\n")
  
  if (nrow(test_data) < 2) {
    stop(crayon::red("Not enough valid data points after filtering to fit the model."))
  }
  
  span_values <- seq(0.1, 0.5, by = 0.01)
  degree_values <- 1:2
  # Define the parameter combinations
  param_grid <- expand.grid(span = span_values, degree = degree_values)
  
  # Define the function to be applied in parallel
  fit_loess <- function(params, data, test_data) {
    tryCatch({
      span <- params$span
      degree <- params$degree
      
      # Fit the LOESS model
      model <- loess(
        x ~ as.numeric(time),
        data = test_data,
        span = span,
        degree = degree
      )
      
      # Make predictions for the entire original data
      predictions <- predict(model, newdata = data)
      
      # Handle NA values in predictions
      predictions[is.na(predictions)] <- 0
      predictions[data$x == 0] <- 0 # Ensure predictions for original zeros are 0
      
      # Calculate R²
      r_squared <- cor(data$x, predictions, use = "complete.obs") ^ 2
      
      # Return the span, degree, and r_squared as a named list
      return(list(
        span = span,
        degree = degree,
        r_squared = r_squared
      ))
    }, error = function(e) {
      return(list(
        span = NA,
        degree = NA,
        r_squared = NA
      ))
    })
  }
  
  # Set up the parallel backend
  bpparam <- NULL
  if (get_os() == "windows") {
    bpparam <- BiocParallel::SnowParam(workers = threads, progressbar = progressbar)
  } else {
    # Ref: https://support.bioconductor.org/p/9140528/
    bpparam <- BiocParallel::MulticoreParam(workers = threads,
                                            force.GC = FALSE,
                                            progressbar = progressbar)
  }
  
  # Run the parallel computation
  result <- BiocParallel::bplapply(
    X = split(param_grid, seq(nrow(param_grid))),
    FUN = fit_loess,
    data = data,
    test_data = test_data,
    BPPARAM = bpparam
  )
  
  raw_result <- result
  # Process the results
  result <- do.call(rbind, lapply(result, function(x) {
    data.frame(
      span = x$span,
      degree = x$degree,
      r_squared = x$r_squared
    )
  }))
  result <- na.omit(result)
  
  # Ensure the result is a data frame
  df_results <- as.data.frame(result)
  
  # Select the best parameters based on proximity to R² target
  best_result <- df_results[which.min(abs(df_results$r_squared - 0.6)), ]
  
  # Save the best parameters
  best_params <- list(span = best_result$span, degree = best_result$degree)
  
  # Assign the best parameters to a global variable
  return(best_params)
}