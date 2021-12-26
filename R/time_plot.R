#' @title time_plot
#' @description time_plot
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x x
#' @param time time
#' @param color color
#' @param y_axis_name y_axis_name
#' @param time_gap time_gap
#' @param add_point add_point
#' @export
#' @return A ggplot2 object.
#' @examples
#' data("step_data", package = "laggedcor")
#' 
#' x = step_data$step
#' time = step_data$time
#' 
#' time_plot(x, time)

time_plot = function(x,
                     time,
                     color = "blue",
                     y_axis_name = "y",
                     time_gap = 12,
                     add_point = FALSE) {
  x = data.frame(time, value = as.numeric(x),
                 stringsAsFactors = FALSE)
  
  sun_rise =
    lubridate::ymd_hms(paste(unique(lubridate::date(x$time)), c("6:00:00")),
                       tz = lubridate::tz(x$time))
  sun_set =
    lubridate::ymd_hms(paste(unique(lubridate::date(x$time)), c("18:00:00")),
                       tz = lubridate::tz(x$time))
  
  day_night_df =
    data.frame(start = sun_rise,
               end = sun_set) %>%
    dplyr::filter()
  
  plot =
    ggplot() +
    geom_rect(
      mapping = aes(
        xmin = start,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "lightyellow",
      data = day_night_df,
      show.legend = FALSE
    ) +
    geom_line(aes(x = time,
                  y = value),
              color = color,
              data = x) +
    labs(x = "", y = y_axis_name) +
    scale_x_datetime(
      breaks = scales::date_breaks(paste(time_gap, "hour")),
      date_labels = "%a %H:%M",
      timezone = lubridate::tz(x$time)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))) +
    base_theme +
    theme(
      axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        hjust = 1,
        size = 10
      ),
      axis.line.x = element_blank(),
      # axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = alpha("grey", 0.2)),
      plot.margin = margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0,
        unit = "pt"
      )
    )
  
  if (add_point) {
    plot =
      plot +
      geom_point(
        aes(x = time,
            y = value),
        color = color,
        shape = 16,
        data = x
      )
  }
  return(plot)
}
