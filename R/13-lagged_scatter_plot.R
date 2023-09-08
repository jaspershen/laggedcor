#' @title lagged_scatter_plot
#' @description lagged_scatter_plot
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object a lagged_scatter_result class object.
#' @param x_name x_name
#' @param y_name y_name.
#' @param which global and max
#' @param hex TRUE or not
#' @export
#' @return A ggplot2 object.
#' @examples 
#' data("object", package = "laggedcor")
#' lagged_scatter_plot(
#'   object = object,
#'   x_name = "Step",
#'   y_name = "HR",
#'   hex = TRUE, 
#'   which = "max"
#' )
#' 
#' lagged_scatter_plot(
#'   object = object,
#'   x_name = "Step",
#'   y_name = "HR",
#'   hex = TRUE, 
#'   which = "global"
#' )

lagged_scatter_plot =
  function(object,
           x_name = "x",
           y_name = "y",
           which = c("global", "max"),
           hex = FALSE) {
    which = match.arg(which)
    
    if (which == "global") {
      idx = object@global_idx
      
      which_global_idx = stringr::str_split(object@shift_time, ",") %>%
        purrr::map(function(x) {
          mean(as.numeric(stringr::str_replace(x, "\\(|\\]", "")))
        }) %>%
        unlist() %>%
        as.numeric() %>%
        `==`(0) %>%
        which()
      shift = object@shift_time[which_global_idx]
      correlation = round(object@global_cor, 3)
    } else{
      idx = object@max_idx
      shift = object@shift_time[object@which_max_idx]
      correlation = round(object@max_cor, 3)
    }
    
    time1 = object@time1
    time2 = object@time2
    x = object@x
    y = object@y
    
    idx1 =
      lapply(idx, function(x) {
        length(x)
      }) %>%
      unlist() %>%
      `>`(0) %>%
      which()
    
    x2 = x[idx1]
    
    y2 =
      lapply(idx, function(z) {
        mean(y[z])
      }) %>%
      unlist()
    
    y2 = y2[!is.na(y2)]
    
    value = data.frame(x2, y2)
    
    if (hex) {
      plot =
        value %>%
        ggplot(aes(x2, y2)) +
        stat_binhex(aes(fill = log(..count..))) +
        # geom_hex(fill=log(..count..)) +
        geom_smooth(method = "lm") +
        base_theme +
        labs(x = y_name, y = x_name) +
        scale_fill_gradient(low = ggsci::pal_aaas()(n = 10)[1],
                            high = ggsci::pal_aaas()(n = 10)[2])
    } else{
      plot =
        value %>%
        ggplot(aes(x2, y2)) +
        geom_point(size = 3) +
        geom_smooth(method = "lm") +
        base_theme +
        labs(x = y_name, y = x_name)
    }
    
    if (which == "max") {
      correlation =
        object@max_cor
      correlation_p = object@all_cor_p[object@which_max_idx]
      shift_time = object@shift_time[object@which_max_idx]
    } else{
      correlation =
        object@global_cor
      correlation_p = object@all_cor_p[object@which_global_idx]
      shift_time = object@shift_time[object@which_global_idx]
    }
    
    plot =
      plot +
      annotate(
        geom = "text",
        x = -Inf,
        y = Inf,
        hjust = 0,
        vjust = 1,
        label = paste(
          "Correlation: ",
          round(correlation, 4),
          "\n",
          "p-value: ",
          correlation_p,
          "\n",
          "Shift time: ",
          shift_time
        )
      )
    
    plot
  }