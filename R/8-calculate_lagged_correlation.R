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
#' \dontrun{
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
#'     time_tol = 0.1,
#'     step = 1 / 60,
#'     min_matched_sample = 10,
#'     progressbar = TRUE,
#'     threads = 5,
#'     cor_method = "spearman"
#'   )
#' object
#' }
calculate_lagged_correlation <-
  function(x,
           y,
           time1,
           time2,
           time_tol = 1,
           step = 1 / 60,
           min_matched_sample = 10,
           progressbar = TRUE,
           all_idx = NULL,
           threads = 10,
           p_threshold = 0.05,
           cor_method = c("spearman", "pearson")) {
    cor_method <- match.arg(cor_method)
    if (length(x) == 0 || length(y) == 0) {
      return(NULL)
    }
    ## time_tol unit is hour
    ## step unit is hour

    # rescale the input data to the same scale
    x <- as.numeric(x) %>%
      scale() %>%
      as.numeric()

    y <- as.numeric(y) %>%
      scale() %>%
      as.numeric()

    x_smooth <- x %>% arrange(time1) %>% function(step) {
      ifelse(
        step > 0, predict(loess(step ~ as.numeric(time1), span = 0.2)),
      )
    }

    findpeaks <- function(x) {
      peak_idx <- which(diff(sign(diff(x))) == -2) + 1
      return(peak_idx)
    }

    x_peaks <- findpeaks(x_smooth)

    x_first_peak <- x_peaks[1]

    y_smooth <- y %>% arrange(time2) %>% function(step) {
      ifelse(
        step > 0, predict(loess(step ~ as.numeric(time2), span = 0.2)),
      )
    }

    y_peaks <- findpeaks(y_smooth)

    y_first_peak <- y_peaks[1]

    tentative_shift <- x_first_peak - y_first_peak

    time_window1 <-
      seq(from = step / 2, to = time_tol, by = step)

    time_window2 <-
      -rev(seq(from = step / 2, to = time_tol, by = step))

    time_window <- sort(c(time_window2, time_window1))

    # temp_fun <-
    #   function(temp_idx,
    #            time_window,
    #            x,
    #            y,
    #            time1,
    #            time2) {
    #     idx <-
    #       time1 %>%
    #       purrr::map(function(temp_time1) {
    #         # cat(match(temp_time1, time1), " ")
    #         diff_time <-
    #           difftime(temp_time1, time2, units = "hours")
    #         which(diff_time > time_window[temp_idx] &
    #           diff_time <= time_window[temp_idx + 1])
    #       })
    #   }

    if (get_os() == "windows") {
      bpparam <- BiocParallel::SnowParam(
        workers = threads,
        # progressbar = TRUE
      )
    } else {
      bpparam <- BiocParallel::MulticoreParam(
        workers = threads,
        force.GC = FALSE,
        # progressbar = TRUE
      )
    }

    old_all_idx <- all_idx

    # if (is.null(all_idx)) {
    #   all_idx <-
    #     BiocParallel::bplapply(
    #       X = seq_along(time_window)[-length(time_window)],
    #       FUN = temp_fun,
    #       time_window = time_window,
    #       x = x,
    #       y = y,
    #       time1 = time1,
    #       time2 = time2,
    #       BPPARAM = bpparam
    #     )
    # }

    if (is.null(all_idx)) {
      all_idx <- BiocParallel::bplapply(
        X = seq_along(time_window)[-length(time_window)],
        FUN = function(temp_idx) {
          idx <- BiocParallel::bplapply(
            X = time1,
            FUN = function(temp_time1) {
              diff_time <-
                difftime(temp_time1, time2, units = "hours")
              which(diff_time > time_window[temp_idx] &
                diff_time <= time_window[temp_idx + 1])
            },
            BPPARAM = bpparam
          )

          return(idx) # explicit return
        },
        BPPARAM = BiocParallel::SerialParam(
          progressbar = TRUE
        )
      )
    }

    all_cor_result <-
      purrr::map(all_idx, function(idx) {
        temp_y <-
          lapply(idx, function(x) {
            mean(y[x])
          }) %>%
          unlist()
        temp_x <- x[which(!is.na(temp_y))]
        temp_y <- temp_y[which(!is.na(temp_y))]
        if (length(temp_x) < min_matched_sample) {
          return(NA)
        } else {
          tryCatch(
            expr = cor.test(temp_x, temp_y, method = cor_method),
            error = function(na) {
              return(NA)
            }
          )
        }
      })

    all_cor_p <-
      all_cor_result %>%
      purrr::map(function(x) {
        # if (class(all_cor_result[[1]]) != "htest") {
        if (!is(all_cor_result[[1]], "htest")) {
          return(NA)
        } else {
          x$p.value
        }
      }) %>%
      unlist()

    all_cor <-
      all_cor_result %>%
      purrr::map(function(x) {
        # if (class(all_cor_result[[1]]) != "htest") {
        if (!is(all_cor_result[[1]], "htest")) {
          return(NA)
        } else {
          x$estimate
        }
      }) %>%
      unlist() %>%
      unname()

    which_max_idx <-
      which.max(abs(all_cor))

    max_idx <- all_idx[[which_max_idx]]

    shift_time <-
      paste("(",
        paste(round(time_window[-length(time_window)] * 60, 2),
          round(time_window[-1] * 60, 2),
          sep = ","
        ),
        "]",
        sep = ""
      )

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

    global_idx <- all_idx[[which_global_idx]]

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
          p_threshold = p_threshold,
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
