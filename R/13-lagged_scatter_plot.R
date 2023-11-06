#' Lagged Scatter Plot
#'
#' This function creates a scatter plot of lagged time series data, 
#' highlighting the correlation between two variables at different lags.
#' The plot can be a regular scatter plot or a hexbin plot.
#'
#' @param object An S4 object containing time series data and associated information
#'               such as global indices, maximum correlation indices, shift times,
#'               and correlation values.
#' @param x_name The name to be used for the x-axis label, defaults to "x".
#' @param y_name The name to be used for the y-axis label, defaults to "y".
#' @param which Determines the type of index to use for plotting, either "global" or "max".
#'              "global" uses the global index and "max" uses the index of maximum correlation.
#'              Defaults to c("global", "max").
#' @param hex A logical value, if TRUE, a hexbin plot will be created, otherwise a regular 
#'            scatter plot. Defaults to FALSE.
#'
#' @return A ggplot object representing the lagged scatter plot with appropriate annotations.
#'
#' @details The function calculates the average y-values at specific lags defined by the
#'          index chosen (global or max). The scatter plot is then created using ggplot2 
#'          and further annotated with correlation information. If `hex` is TRUE, the plot 
#'          uses `stat_binhex` to create hexagonal binning. Otherwise, points are plotted 
#'          directly with `geom_point`. A linear model fit is added in both cases using 
#'          `geom_smooth`.
#'
#'
#' @note The function expects the `object` to have specific slots: `@global_idx`, `@max_idx`, 
#'       `@shift_time`, `@global_cor`, `@max_cor`, `@which_max_idx`, `@time1`, `@time2`, 
#'       `@x`, and `@y`. If `hex` is TRUE, `@which_global_idx` is also used. It is important 
#'       to ensure these slots are present in the object passed to the function. Also, 
#'       the function assumes that `base_theme` is defined elsewhere in the user's environment.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @export
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