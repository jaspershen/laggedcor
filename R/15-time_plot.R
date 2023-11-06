#' Time Series Plot with Day/Night Shading
#'
#' This function generates a time series plot using ggplot2 with shaded regions indicating day and night times.
#' It can plot data points, allow for faceting by day, and adjust for time zones.
#'
#' @param x A numeric vector of values to plot.
#' @param time A POSIXct or POSIXlt object representing the time points of the x values.
#' @param color The color to be used for the lines and points in the plot, defaults to "blue".
#' @param y_axis_name The label for the y-axis, defaults to "Value".
#' @param sun_rise_time A character string representing sunrise time, defaults to "6:00:00".
#' @param sun_set_time A character string representing sunset time, defaults to "18:00:00".
#' @param time_gap The gap in hours for the x-axis breaks, defaults to 12.
#' @param add_point Logical, whether to add points to the line plot, defaults to FALSE.
#' @param facet Logical, whether to facet the plot by day, defaults to FALSE.
#'
#' @return A ggplot object that represents the time series plot.
#'
#' @details This function utilizes the ggplot2, lubridate, and scales packages to create a time series plot.
#'          Day and night periods are shaded to distinguish between them visually. Optional faceting by day
#'          can be enabled to separate the plot into panels for each day. Additionally, data points can be
#'          added to the plot if desired.
#'
#' @note This function assumes that 'base_theme' is a ggplot2 theme object defined elsewhere in the user's environment.
#'       Adjust the 'sun_rise_time' and 'sun_set_time' to match the local times for sunrise and sunset.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @export
#' @examples
#' data("step_data", package = "laggedcor")
#'
#' x = step_data$step
#' time = step_data$time
#'
#' time_plot(x, time)

time_plot <-
  function(x,
           time,
           color = "blue",
           y_axis_name = "Value",
           sun_rise_time = "6:00:00",
           sun_set_time = "18:00:00",
           time_gap = 12,
           add_point = FALSE,
           facet = FALSE) {
    day <-
      lubridate::date(time)
    
    temp_data <- data.frame(
      accurate_time = time,
      time = strftime(time,
                      format = "%H:%M:%S",
                      tz = lubridate::tz(time)) %>%
        hms::as_hms(),
      day,
      value = as.numeric(x),
      stringsAsFactors = FALSE
    ) %>%
      dplyr::mutate(time = as.POSIXct(time),
                    week = format(accurate_time, "%a")) %>%
      dplyr::mutate(week = paste(week,
                                 lubridate::month(day),
                                 lubridate::day(day),
                                 sep = "-")) %>%
      dplyr::mutate(week = factor(week, unique(week)))
    
    sun_rise <-
      lubridate::ymd_hms(paste(unique(
        lubridate::date(temp_data$accurate_time)
      ), c(sun_rise_time)),
      tz = lubridate::tz(temp_data$accurate_time))
    sun_set =
      lubridate::ymd_hms(paste(unique(
        lubridate::date(temp_data$accurate_time)
      ), c(sun_set_time)),
      tz = lubridate::tz(temp_data$accurate_time))
    
    day_night_df <-
      data.frame(start = sun_rise,
                 end = sun_set,
                 day = lubridate::date(sun_rise)) %>%
      dplyr::mutate(
        start_time = as.POSIXct(hms::as_hms(start)),
        end_time = as.POSIXct(hms::as_hms(end)),
        week = format(day, "%a")
      ) %>%
      dplyr::mutate(week = paste(week,
                                 lubridate::month(day),
                                 lubridate::day(day),
                                 sep = "-")) %>%
      dplyr::mutate(week = factor(week, unique(week)))
    
    
    if (facet) {
      plot <-
        ggplot() +
        geom_rect(
          mapping = aes(
            xmin = start_time,
            xmax = end_time,
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
                  data = temp_data) +
        scale_x_datetime(
          breaks = scales::date_breaks(paste(time_gap, "hour")),
          date_labels = "%H:%M",
          timezone = lubridate::tz(temp_data$time)
        )
      
    } else{
      plot <-
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
        geom_line(aes(x = accurate_time,
                      y = value),
                  color = color,
                  data = temp_data) +
        scale_x_datetime(
          breaks = scales::date_breaks(paste(time_gap, "hour")),
          date_labels = "%a %H:%M",
          timezone = lubridate::tz(temp_data$time)
        )
    }
    
    plot <-
      plot +
      labs(x = "", y = y_axis_name) +
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
          data = temp_data
        )
    }
    return(plot)
  }
