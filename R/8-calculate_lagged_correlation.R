#' Calculate Lagged Correlation Between Two Time Series
#'
#' This function calculates the lagged correlation between two time series data
#' sets over specified time windows, with the capability to adjust for time tolerance,
#' step size, and the minimum number of matched samples. The function also allows
#' for the use of different correlation methods and parallel processing.
#'
#' @param x A numeric vector for the first time series.
#' @param y A numeric vector for the second time series.
#' @param time1 A numeric vector of timestamps corresponding to `x`. (POSIXct).
#' @param time2 A numeric vector of timestamps corresponding to `y`. (POSIXct).
#' @param time_tol Tolerance for the lag time, in hours (default is 1 hour).
#' @param step The step size for the lag window, in hours (default is 1/60 hour, namely 1 min).
#' @param min_matched_sample The minimum number of matched samples to consider
#'   a valid correlation (default is 10).
#' @param progressbar Logical indicating whether to show a progress bar (default is TRUE).
#' @param all_idx An optional precomputed index list to speed up calculations.
#' @param threads The number of threads to use for parallel processing (default is 10).
#' @param cor_method The method for computing correlation: "spearman" or "pearson"
#'   (default is "spearman").
#'
#' @return An object of class "lagged_cor_result" containing the lagged correlation
#'   results, indices, and other relevant data.
#'
#' @details The function scales the input time series and computes correlations
#'   over a range of time lags, handling mismatches in time series lengths by
#'   a tolerance window. Parallel processing is implemented to improve performance
#'   for large datasets.
#'
#' @export
#' @author Xiaotao Shen \email{shenxt1990@stanford.edu}
#' @examples
#' data("heart_data", package = "laggedcor")
#' data("step_data", package = "laggedcor")
#'
#' dim(heart_data)
#' dim(step_data)
#'
#' x <- step_data$step
#' time1 <- step_data$time
#'
#' y <- heart_data$heart
#' time2 <- heart_data$time
#'
#' object <-
#'   calculate_lagged_correlation(
#'     x = x,
#'     y = y,
#'     time1 = time1,
#'     time2 = time2,
#'     time_tol = 0.2,
#'     step = 2 / 60,
#'     min_matched_sample = 10,
#'     progressbar = TRUE,
#'     threads = 16,
#'     cor_method = "spearman"
#'   )
#' object

calculate_lagged_correlation <-
  function(x,
           y,
           time1,
           time2,
           time_tol = 1,
           # lag.max in ccf
           step = 1 / 60,
           # 1 min
           min_matched_sample = 10,
           progressbar = TRUE,
           all_idx = NULL,
           # level = 0.95,
           B = 1000,
           smooth = FALSE,
           threads = 10,
           align_method = c("linear", "constant"),
           cor_method = c("spearman", "pearson")) {
    cor_method <- match.arg(cor_method)
    align_method <- match.arg(align_method)
    if (length(x) == 0 || length(y) == 0) {
      return(NULL)
    }
    # rescale the input data to the same scale
    x <- as.numeric(x) %>%
      scale() %>%
      as.numeric()

    y <- as.numeric(y) %>%
      scale() %>%
      as.numeric()
    

    # Find common time range
    start_time <- max(min(time1), min(time2))
    end_time <- min(max(time1), max(time2))
    
    # align the two time series
    time_window1 <-
      seq(from = step / 2, to = time_tol, by = step)
    
    time_window2 <-
      -rev(seq(from = step / 2, to = time_tol, by = step))
    
    time_window <- sort(c(time_window2, time_window1))
    
    # convert step back to min
    target_freq = paste(step * 60, "min")
    
    # Create regular time sequence at target frequency
    regular_times <- seq(from = start_time, to = end_time, by = target_freq)
    
    x_aligned <- approx(
      x = time1,
      y = x,
      xout = regular_times,
      method = align_method
    )$y
    
    y_aligned <- approx(
      x = time2,
      y = y,
      xout = regular_times,
      method = align_method
    )$y
    
    # Rank the data to make ccf calculation based on spearman possible
    if (cor_method == "spearman") {
      x_transformed <- rank(x_aligned)
      y_transformed <- rank(y_aligned)
      attributes(x_transformed) <- attributes(x_aligned)
      attributes(y_transformed) <- attributes(y_aligned)
    } else {
      x_transformed <- x_aligned
      y_transformed <- y_aligned
    }
    
    lag.max = time_tol / step - 1
    
    ccf_res <- ccf(x_transformed,
                   y_transformed,
                   lag.max = lag.max,
                   plot = FALSE)
    
    lags <- ccf_res$lag[, 1, 1]
    cors <- ccf_res$acf[, 1, 1]
    
    shift_time <-
      paste(
        "(", 
        paste(
          round(time_window[-length(time_window)] * 60, 2), 
          round(time_window[-1] * 60, 2), 
          sep = ","
        ), 
        "]", 
        sep = ""
      )
    
    shift_time_num =
      shift_time %>%
      lapply(function(x) {
        x %>%
          stringr::str_replace("\\(", "") %>%
          stringr::str_replace("\\]", "") %>%
          stringr::str_split(",") %>%
          `[[`(1) %>%
          as.numeric() %>%
          mean()
      }) %>%
      unlist()
    
    # assert len(time_window) - 1 is the same as regular_times
    if (length(time_window) - 1 != length(lags)) {
      stop(
        "Vectors x and y must have the same length: ",
        "length(time_window) = ",
        length(time_window) - 1,
        ", length(lags) = ",
        length(lags)
      )
    }
    
    find_interpolation_points <- function(x, xout) {
      sapply(xout, function(point) {
        left <- max(which(x <= point))
        right <- min(which(x >= point))
        c(left = left, right = right)
      })
    }
    
    all_idx <- list() # dummy
    
    all_cor_result <- ccf_res # I really don't know what to give
    all_cor_p <- 2 * (1 - pnorm(
      abs(cors),
      mean = 0,
      sd = 1 / sqrt(ccf_res$n.used)
    ))
    all_cor <- cors
    
    which_max_idx <-
      which.max(abs(all_cor))
    
    max_idx <- list() # dummy
    
    # find the idx that cross the 0 lag point
    which_global_idx <-
      purrr::map(shift_time, function(x) {
        x <-
          stringr::str_replace(x, "\\(", "") %>%
          stringr::str_replace("\\]", "") %>%
          stringr::str_split(",") %>%
          `[[`(1) %>%
          as.numeric()
        x[1] < 0 & x[2] > 0
      }) %>%
      unlist() %>%
      which()
    
    # get the 0 lag index
    global_idx <- list() # dummy()
    
    
    global_cor <- all_cor[which_global_idx]
    global_cor_p <- all_cor_p[which_global_idx]
    
    parameter <-
      new(
        Class = "tidymass_parameter",
        pacakge_name = "laggedcor",
        function_name = "calculate_lagged_correlation",
        parameter = list(
          time_tol = time_tol,
          step = step,
          min_matched_sample = min_matched_sample,
          progressbar = progressbar,
          threads = threads,
          cor_method = cor_method
        ),
        time = Sys.time()
      )
    
    object <- new(
      Class = "lagged_cor_result",
      x = x,
      time1 = time1,
      y = y,
      time2 = time2,
      idx = all_idx,
      all_cor = all_cor,
      all_cor_p = all_cor_p,
      shift_time = shift_time,
      which_max_idx = which_max_idx,
      which_global_idx = which_global_idx,
      max_idx = max_idx,
      max_cor = all_cor[which_max_idx],
      global_idx = global_idx,
      global_cor = global_cor,
      parameter = parameter
    )
    
    return(object)
  }
