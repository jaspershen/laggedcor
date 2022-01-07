#' @title calculate_lagged_correlation
#' @description calculate_lagged_correlation
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x Time-series data 1. A numeric vector.
#' @param y Time-series data 2. A numeric vector.
#' @param time1 Time-series data 1. A time vector (POSIXct).
#' @param time2 Time-series data 2. A time vector (POSIXct).
#' @param time_tol Time tolerance to match time-series 1 and time-series data 2. Unit is hour.
#' @param step Step to calculate lagged correlation.
#' @param min_matched_sample Minimum matched sample numner for one lagged correlation.
#' @param progressbar progressbar
#' @param threads threads number
#' @param cor_method spearman or pearson.
#' @param all_idx all_idx
#' @export
#' @return A lagged_cor class object.
#' @examples
#'\dontrun{
#' data("heart_data", package = "laggedcor")
#' data("step_data", package = "laggedcor")
#' 
#' dim(heart_data)
#' dim(step_data)
#' 
#' x = step_data$step
#' time1 = step_data$time
#' 
#' y = heart_data$heart
#' time2 = heart_data$time
#' 
#' object =
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

calculate_lagged_correlation =
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
           cor_method = c("spearman", "pearson")) {
    cor_method = match.arg(cor_method)
    ##time_tol unit is hour
    ##step unit is hour
    x = as.numeric(x) %>%
      scale() %>%
      as.numeric()
    
    y = as.numeric(y) %>%
      scale() %>%
      as.numeric()
    
    time_window1 =
      seq(from = step / 2, to = time_tol, by = step)
    
    time_window2 =
      -rev(seq(from = step / 2, to = time_tol, by = step))
    
    time_window = sort(c(time_window2, time_window1))
    
    temp_fun =
      function(temp_idx,
               time_window,
               x,
               y,
               time1,
               time2) {
        idx =
          time1 %>%
          purrr::map(function(temp_time1) {
            # cat(match(temp_time1, time1), " ")
            diff_time =
              difftime(temp_time1, time2, units = "hours")
            which(diff_time > time_window[temp_idx] &
                    diff_time <= time_window[temp_idx + 1])
          })
      }
    
    if (get_os() == "windows") {
      bpparam = BiocParallel::SnowParam(workers = threads,
                                        progressbar = TRUE)
    } else{
      bpparam = BiocParallel::MulticoreParam(workers = threads,
                                             progressbar = TRUE)
    }
    
    old_all_idx = all_idx
    
    if (is.null(all_idx)) {
      all_idx =
        BiocParallel::bplapply(
          X = seq_along(time_window)[-length(time_window)],
          FUN = temp_fun,
          time_window = time_window,
          x = x,
          y = y,
          time1 = time1,
          time2 = time2,
          BPPARAM = bpparam
        )
    }
    
    all_cor_result =
      purrr::map(all_idx, function(idx) {
        temp_y =
          lapply(idx, function(x) {
            mean(y[x])
          }) %>%
          unlist()
        temp_x = x[which(!is.na(temp_y))]
        temp_y = temp_y[which(!is.na(temp_y))]
        if (length(temp_x) < min_matched_sample) {
          return(NA)
        } else{
          tryCatch(
            expr = cor.test(temp_x, temp_y, method = cor_method),
            error = function(na) {
              return(NA)
            }
          )
        }
      })
    
    all_cor_p =
      all_cor_result %>%
      purrr::map(function(x) {
        if (is.na(x)) {
          return(NA)
        } else{
          x$p.value
        }
      }) %>%
      unlist()
    
    all_cor =
      all_cor_result %>%
      purrr::map(function(x) {
        if (is.na(x)) {
          return(NA)
        } else{
          x$estimate
        }
      }) %>%
      unlist() %>%
      unname()
    
    which_max_idx =
      which.max(abs(all_cor))
    
    max_idx = all_idx[[which_max_idx]]
    
    shift_time =
      paste("(",
            paste(round(time_window[-length(time_window)] * 60, 2),
                  round(time_window[-1] * 60, 2), sep = ','),
            "]", sep = "")
    
    which_global_idx =
      purrr::map(shift_time, function(x) {
        x =
          stringr::str_replace(x, "\\(", "") %>%
          stringr::str_replace("\\]", "") %>%
          stringr::str_split(",") %>%
          `[[`(1) %>%
          as.numeric()
        x[1] < 0 & x[2] > 0
      }) %>%
      unlist() %>%
      which()
    
    global_idx = all_idx[[which_global_idx]]
    
    global_cor = all_cor[which_global_idx]
    global_cor_p = all_cor_p[which_global_idx]
    
    parameter =
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
