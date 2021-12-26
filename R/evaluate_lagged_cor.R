#' @title evaluate_lagged_cor
#' @description evaluate_lagged_cor
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object a lagged_scatter_result class object.
#' @param plot return plot or not.
#' @export
#' @return A ggplot2 object.
#' @examples 
#' data("object", package = "laggedcor")
#' result = 
#'   evaluate_lagged_cor(object = object, plot = TRUE)
#' 
#' result$score
#' result$plot

evaluate_lagged_cor =
  function(object,
           plot = TRUE) {
    if (is.null(object)) {
      return(list(score = 0, plot = NULL))
    }
    
    cor =
      object@all_cor
    
    shift_time =
      object@shift_time %>%
      stringr::str_replace("\\(|\\]", "") %>%
      stringr::str_replace("\\]", "") %>%
      stringr::str_split("\\,") %>%
      purrr::map(function(x) {
        mean(as.numeric(x))
      }) %>%
      unlist()
    
    ###loess fit
    cor[is.na(cor)] = 0
    span = 3 / length(cor)
    if (span < 0.1) {
      span = 0.1
    }
    loess_lm = loess(cor ~ shift_time,
                     data = data.frame(cor, shift_time),
                     span = span)
    new_shift_time = seq(min(shift_time), max(shift_time), 1)
    new_cor =
      predict(object = loess_lm,
              newdata = data.frame(shift_time = new_shift_time))
    
    ####positive fit
    if (any(new_cor < 0)) {
      cor_positive = new_cor - min(new_cor)
    } else{
      cor_positive = new_cor
    }
    
    pk.pos = which.max(cor_positive)
    pks_positive = fitpeaks(y = cor_positive, pos = pk.pos)
    
    if (!is.na(pks_positive[1, 1])) {
      fitted_y_positive =
        dnorm(
          x = 1:length(cor_positive),
          mean = as.numeric(pks_positive[1, "rt"]),
          sd = as.numeric(pks_positive[1, "sd"])
        ) * as.numeric(pks_positive[1, "area"])
      fitted_y_positive[is.na(fitted_y_positive)] = 0
      score_positive = cor(cor_positive,
                           fitted_y_positive, method = "spearman")
    } else{
      score_positive = 0
    }
    
    if (is.na(score_positive)) {
      score_positive = 0
    }
    
    ####negative fit
    if (any(new_cor > 0)) {
      cor_negative = new_cor - max(cor)
    } else{
      cor_negative = new_cor
    }
    
    pk.pos = which.max(-cor_negative)
    pks_negative = fitpeaks(y = -cor_negative, pos = pk.pos)
    
    if (!is.na(pks_negative[1, 1])) {
      fitted_y_negative =
        dnorm(
          x = 1:length(cor_negative),
          mean = as.numeric(pks_negative[1, "rt"]),
          sd = as.numeric(pks_negative[1, "sd"])
        ) * as.numeric(pks_negative[1, "area"])
      fitted_y_negative[is.na(fitted_y_negative)] = 0
      score_negative = cor(-cor_negative,
                           fitted_y_negative, method = "spearman")
    } else{
      score_negative = 0
    }
    
    if (is.na(score_negative)) {
      score_negative = 0
    }
    
    positive_max_cor = max(object@all_cor)
    negative_max_cor = min(object@all_cor)
    
    if (positive_max_cor <= 0) {
      score_positive = 0
    }
    
    if (negative_max_cor >= 0) {
      score_negative = 0
    }
    
    score = max(c(score_positive, score_negative))
    
    if (score_positive >= score_negative) {
      true = new_cor
      fitted = fitted_y_positive
      if (any(new_cor < 0)) {
        fitted = fitted_y_positive + min(new_cor)
      } else{
        fitted = fitted_y_positive
      }
    } else{
      true = new_cor
      fitted = -fitted_y_negative
      if (any(new_cor > 0)) {
        fitted = -fitted_y_negative + max(new_cor)
      } else{
        fitted = -fitted_y_negative
      }
    }
    
    if (plot) {
      temp_data1 =
        data.frame(new_shift_time, true, fitted)
      
      temp_data2 =
        temp_data1 %>%
        dplyr::filter(new_shift_time %in% shift_time)
      
      temp_data2$all_cor_p = object@all_cor_p
      
      temp_data2 =
        temp_data2 %>%
        dplyr::mutate(yesornot =
                        case_when(all_cor_p < 0.05 ~ "yes",
                                  all_cor_p >= 0.05 ~ "no")) %>%
        dplyr::mutate(log_p = -log(all_cor_p, 10))
      
      temp_data2$log_p[is.infinite(temp_data2$log_p)] =
        max(temp_data2$log_p[!is.infinite(temp_data2$log_p)])
      
      p =
        ggplot() +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        geom_point(
          aes(
            x = new_shift_time,
            y = true,
            color = yesornot,
            size = log_p
          ),
          show.legend = FALSE,
          data = temp_data2
        ) +
        scale_color_manual(values = c("yes" = "red", "no" = "grey")) +
        scale_size_continuous(range = c(2, 5)) +
        geom_line(aes(new_shift_time, fitted, group = 1),
                  data = temp_data1,
                  color = "red") +
        theme_bw() +
        theme(panel.grid.minor = element_blank()) +
        labs(x = "Shift time (min)",
             y = "Spearson correlation")
    } else{
      p = NULL
    }
    
    list(score = score,
         plot = p)
  }
