#' Perform Data Smoothing with Loess
#'
#' This function performs data smoothing using the Loess method and opitonally plot
#' the original data and smoothed data to visualize the effect of different span
#' values on the smoothing process.
#'
#' @param x A numeric vector of values to be smoothed.
#' @param time A POSIXct or POSIXlt object representing the time points of the x values.
#' @param span A numeric vector of span values to be used in the Loess smoothing.
#' @param should_plot A logical value indicating whether to plot the original and smoothed data.
#' @param color_orignal The color to be used for the original data in the plot. Default is "blue".
#' @param color_smoothed The color to be used for the smoothed data in the plot. Default is "red".
#'
#' @return A list containing the original and smoothed data, as well as the span values used.
#'
#' @examples
#' data("step_data", package = "laggedcor")
#' x <- step_data$step
#' time <- step_data$time
#' span <- 0.1
#'
#' res <- smooth_data(x, time, span, should_plot = TRUE)
#'
#' @export
#' @author Zirui Qiang \email{{zirui_qiang@outlook.com}}
#' @author Minghang Li \email{{minghli@ethz.ch}}

smooth_data <- function(x,
                        time,
                        span,
                        should_plot = FALSE,
                        color_original = "blue",
                        color_smoothed = "red") {
  # Check the input
  if (length(x) != length(time)) {
    stop("The length of x and time must be the same.")
  }

  # Create a data frame
  data <- data.frame(time = time, x = x)

  # Perform data smoothing
  smoothed <- predict(loess(x ~ as.numeric(time), data = data, span = span))

  # Plot the original and smoothed data
  plot <- NULL
  if (should_plot) {
    plot <- ggplot(data, aes(x = time, y = x)) +
      geom_line(color = color_original, alpha = 0.5) + # Original data
      geom_line(aes(y = smoothed), color = color_smoothed) + # Smoothed data
      labs(
        title = "Data Smoothing with Loess",
        x = "Timestamp",
        y = "Value"
      ) +
      scale_color_manual(values = c(
        "Original" = color_original,
        "Smoothed" = color_smoothed
      )) +
      theme_minimal()
  }

  # Return the original and smoothed data
  return(list(
    original = x,
    smoothed = smoothed,
    span = span,
    plot = plot
  ))
}
