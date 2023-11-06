# sample_matching_plot =
#   function(object,
#            index = 1,
#            only_remain_matched = TRUE,
#            day_night_df = NULL,
#            add_text = FALSE) {
#     match_idx = object@idx[[index]]
#     time1 = object@time1
#     time2 = object@time2
#     
#     idx1 =
#       lapply(match_idx, length) %>%
#       unlist() %>%
#       `!=`(0) %>%
#       which()
#     
#     if (length(idx1) == 0) {
#       return(NULL)
#     }
#     
#     idx2 = match_idx[idx1]
#     
#     segment_data =
#       purrr::map2(.x = idx1, .y = idx2, function(x, y) {
#         data.frame(
#           x = time1[x],
#           xend = time2[y],
#           y = "time1",
#           yend = "time2"
#         )
#       }) %>%
#       do.call(rbind, .) %>%
#       as.data.frame()
#     
#     if (only_remain_matched) {
#       temp_data =
#         data.frame(x = c(time1[unique(idx1)], time2[unique(unlist(idx2))]),
#                    y = c(rep("time1", length(time1[unique(idx1)])),
#                          rep("time2", length(time2[unique(unlist(idx2))]))))
#     } else{
#       temp_data =
#         data.frame(x = c(time1, time2),
#                    y = c(rep("time1", length(time1)), rep("time2", length(time2))))
#     }
#     
#     if (!is.null(day_night_df)) {
#       plot =
#         ggplot() +
#         geom_rect(
#           mapping = aes(
#             xmin = start,
#             xmax = end,
#             ymin = -Inf,
#             ymax = Inf
#           ),
#           fill = "lightyellow",
#           data = day_night_df,
#           show.legend = FALSE
#         ) +
#         geom_segment(aes(
#           x = x,
#           y = y,
#           xend = xend,
#           yend = yend
#         ),
#         data = segment_data) +
#         geom_point(aes(x = x, y = y,
#                        color = y),
#                    data = temp_data,
#                    show.legend = FALSE) +
#         scale_x_datetime(
#           breaks = scales::date_breaks("12 hour"),
#           date_labels = "%a %H:%M",
#           timezone = "America/Los_Angeles"
#         ) +
#         theme_bw() +
#         theme(axis.text.x = element_text(
#           angle = 45,
#           hjust = 1,
#           vjust = 1
#         ),
#         panel.grid = element_blank()) +
#         labs(x = "", y = "") +
#         ggsci::scale_color_jama()
#     } else{
#       plot =
#         ggplot() +
#         geom_segment(aes(
#           x = x,
#           y = y,
#           xend = xend,
#           yend = yend
#         ),
#         data = segment_data) +
#         geom_point(aes(x = x, y = y,
#                        color = y),
#                    data = temp_data,
#                    show.legend = FALSE) +
#         scale_x_datetime(
#           breaks = scales::date_breaks("12 hour"),
#           date_labels = "%a %H:%M",
#           timezone = "America/Los_Angeles"
#         ) +
#         theme_bw() +
#         theme(axis.text.x = element_text(
#           angle = 45,
#           hjust = 1,
#           vjust = 1
#         ),
#         panel.grid = element_blank()) +
#         labs(x = "", y = "") +
#         ggsci::scale_color_jama()
#     }
#     
#     shift_time =
#       object$shift_time[index]
#     
#     plot =
#       plot +
#       ggtitle(
#         label = paste("Shift time(min):", shift_time, sep = ""),
#         subtitle = paste(length(idx1), "matched samples")
#       )
#     
#     if (add_text) {
#       plot =
#         plot +
#         ggrepel::geom_text_repel(
#           mapping = aes(
#             x = x,
#             y = y,
#             label = paste(lubridate::hour(x), lubridate::minute(x), sep = ":")
#           ),
#           data = temp_data,
#           size = 2
#         )
#     }
#     
#     plot
#     
#   }
