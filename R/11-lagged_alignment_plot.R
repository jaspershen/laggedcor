#' @title lagged_alignment_plot
#' @description lagged_alignment_plot
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object a lagged_scatter_result class object.
#' @param x_color x_color
#' @param y_color y_color
#' @param x_name x_name
#' @param y_name y_name
#' @param which global or maz
#' @param x_limit x_limit
#' @param non_matched_point_size non_matched_point_size
#' @param y_point_size y_point_size
#' @param x_point_size x_point_size
#' @param integrated integrated
#' @param add_connect_line add_connect_line
#' @param add_point add_point
#' @param time_gap time_gap in x axis.
#' @export
#' @return A ggplot2 object.
#' @examples
#' data("object", package = "laggedcor")
#'
#' lagged_alignment_plot(object = object,
#'                       x_limit = c(1, 1000))
#' lagged_alignment_plot(object = object,
#'                       x_limit = c(1, 10000),
#'                       time_gap = 10)
#'
#' lagged_alignment_plot(
#'   object = object,
#'   x_limit = c(1, 100),
#'   time_gap = 1,
#'   add_point = TRUE,
#'   x_point_size = 1,
#'   y_point_size = 1,
#'   integrated = TRUE,
#'   add_connect_line = TRUE
#' )
#'
#' lagged_alignment_plot(
#'   object = object,
#'   x_limit = c(1, 100),
#'   time_gap = 1,
#'   add_point = TRUE,
#'   x_point_size = 1,
#'   y_point_size = 1,
#'   integrated = FALSE
#' )

lagged_alignment_plot =
  function(object,
           x_color = "#631879FF",
           y_color = "#E377C2FF",
           x_name = "x",
           y_name = "y",
           which = c("global", "max"),
           x_limit = c(1, 1000),
           non_matched_point_size = 0.1,
           y_point_size = 1,
           x_point_size = 3,
           integrated = FALSE,
           add_connect_line = FALSE,
           add_point = FALSE,
           time_gap = 4) {
    which = match.arg(which)
    
    if (which == "global") {
      idx = object@global_idx
      shift = object@shift_time[object@which_global_idx]
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
    
    # if (x_limit[2] > length(x)) {
    #   x_limit[2] = length(x)
    # }
    
    if (max(x) > min(y)) {
      x = x - (max(x) - min(y))
    }
    
    #######non integrated
    if (integrated) {
      time2 =
        purrr::map(idx, function(x) {
          mean(time2[c(head(x, 1), tail(x, 1))])
        }) %>%
        unlist() %>%
        lubridate::as_datetime(tz = "America/Los_Angeles")
      
      y =
        purrr::map(idx, function(x) {
          mean(y[x])
        }) %>%
        unlist()
      
      if (max(x, na.rm = TRUE) > min(y, na.rm = TRUE)) {
        x = x - (max(x, na.rm = TRUE) - min(y, na.rm = TRUE))
      }
      
      x2 = data.frame(time = time1,
                      value = x,
                      class = "x")
      y2 = data.frame(time = time2,
                      value = y,
                      class = "y")
      
      x2$matched = 'NO'
      x2$matched[which(unlist(lapply(idx, length)) > 0)] =  "YES"
      
      y2$matched = 'YES'
      
      y2 =
        y2 %>%
        dplyr::filter(!is.na(time))
      
      value = rbind(x2, y2)
      
      value =
        value %>%
        dplyr::mutate(matched =
                        case_when(matched == "YES" ~ class,
                                  matched == "NO" ~ "NO"))
    } else{
      x2 = data.frame(time = time1,
                      value = x,
                      class = "x")
      y2 = data.frame(time = time2,
                      value = y,
                      class = "y")
      
      x2$matched = 'NO'
      x2$matched[which(unlist(lapply(idx, length)) > 0)] =  "YES"
      
      y2$matched = 'NO'
      y2$matched[unique(unlist(idx))] = "YES"
      
      value = rbind(x2, y2)
      
      value =
        value %>%
        dplyr::mutate(matched =
                        case_when(matched == "YES" ~ class,
                                  matched == "NO" ~ "NO"))
    }
    
    time1 = sort(unique(value$time))[x_limit[1]]
    time2 = sort(unique(value$time))[x_limit[2]]
    
    value =
      value %>%
      dplyr::filter(time >= time1 & time < time2)
    
    sun_rise =
      lubridate::ymd_hms(paste(unique(lubridate::date(value$time)), c("6:00:00")),
                         tz = lubridate::tz(value$time))
    sun_set =
      lubridate::ymd_hms(paste(unique(lubridate::date(value$time)), c("18:00:00")),
                         tz = lubridate::tz(value$time))
    
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
      geom_line(
        data = value,
        aes(
          x = time,
          y = value,
          group = class,
          color = class
        ),
        show.legend = TRUE
      )
    
    plot =
      plot +
      scale_size_manual(values = c("x" = x_point_size,
                                   "y" = y_point_size,
                                   "NO" = non_matched_point_size)) +
      scale_x_datetime(
        breaks = scales::date_breaks(paste(time_gap, "hour")),
        date_labels = "%a %H:%M",
        timezone = "America/Los_Angeles",
        limits = c(min(time1[!is.na(time1)][x_limit[1]],
                       time2[!is.na(time2)][x_limit[1]]),
                   max(time1[!is.na(time1)][x_limit[2]],
                       time2[!is.na(time2)][x_limit[2]]))
      ) +
      labs(
        x = "",
        y = "",
        title = paste("Shift window: ",
                      shift,
                      "; Correlation: ",
                      correlation,
                      sep = "")
      ) +
      guides(color = guide_legend(title = "",
                                  override.aes = list(size = 3)),
             size = "none") +
      scale_color_manual(
        values = c(
          "x" = unname(x_color),
          "y" = unname(y_color),
          'NO' = "grey"
        ),
        labels = c("x" = x_name,
                   "y" = y_name,
                   "NO" = "Non matched")
      )
    
    if (add_point) {
      plot =
        plot +
        geom_point(
          data = value,
          aes(
            x = time,
            y = value,
            group = class,
            color = matched,
            size = matched
          ),
          shape = 16,
          show.legend = TRUE
        )
    }
    
    if (add_connect_line) {
      ###get the segment data
      segment_data =
        purrr::map(seq_along(idx), function(i) {
          if (length(idx[[i]]) > 0) {
            data.frame(
              time1 = time1[i],
              x = x[i],
              time2 = time2[idx[[i]]],
              y = y[idx[[i]]]
            )
          }
        }) %>%
        dplyr::bind_rows()
      
      plot =
        plot +
        geom_segment(
          data = segment_data,
          aes(
            x = time1,
            y = x,
            xend = time2,
            yend = y
          ),
          color = x_color,
          show.legend = FALSE
        )
    }
    
    plot =
      plot +
      base_theme +
      theme(
        legend.position = "top",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          size = 10
        ),
        axis.line.x = element_blank(),
        plot.margin = margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0,
          unit = "pt"
        )
      )
    plot
  }
