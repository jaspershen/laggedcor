# Load packages
library(ggplot2)
library(dplyr)
library(zoo)

# Load step_data and heart_data from CSV files
#step_data <- read.csv("~/Desktop/multi-omics/data_smooth/data/step_data.csv")
#heart_data <- read.csv("~/Desktop/multi-omics/data_smooth/data/heart_data.csv")

# Load step_data and heart_data datasets
load("data/step_data.rda")
load("data/heart_data.rda")

# Check structure
str(step_data)
str(heart_data)

# Smooth step_data
step_data <- step_data %>%
  arrange(time) %>%
  mutate(# Only smooth the regions where step count is greater than 0
    smoothed_step = ifelse(step > 0, predict(loess(
      step ~ as.numeric(time), span = 0.1
    )), NA))

# Smooth heart_data
heart_data <- heart_data %>%
  arrange(time) %>%
  mutate(smoothed_heart = predict(loess(heart ~ as.numeric(time), span = 0.1)))


# Check structure
str(step_data)
str(heart_data)

# Plot the original and smoothed step count data for comparison
ggplot(step_data, aes(x = time)) +
  geom_line(aes(y = step, color = "Original Step")) +
  geom_line(aes(y = smoothed_step, color = "Smoothed Step")) +
  labs(title = "Step Data: Original vs Smoothed", x = "Timestamp", y = "Step") +
  scale_color_manual(values = c(
    "Original Step" = "blue",
    "Smoothed Step" = "red"
  )) +
  theme_minimal()

# Plot the original and smoothed heart rate data for comparison
ggplot(heart_data, aes(x = time)) +
  geom_line(aes(y = heart, color = "Original Heart Rate")) +
  geom_line(aes(y = smoothed_heart, color = "Smoothed Heart Rate")) +
  labs(title = "Heart Rate Data: Original vs Smoothed", x = "Timestamp", y = "Heart Rate") +
  scale_color_manual(values = c(
    "Original Heart Rate" = "blue",
    "Smoothed Heart Rate" = "red"
  )) +
  theme_minimal()

#If the curve is too smooth and doesn’t capture enough detail, reduce the span.
#If the curve is too jagged and noisy, increase the span.
#This is just a sample program to demonstrate the effect of different span values and does not handle missing values.
# Set different span values
span_values <- c(0.01, 0.05, 0.1, 0.5)

# Visualize smoothing with different spans
ggplot(step_data, aes(x = time, y = step)) +
  geom_line(color = "blue", alpha = 0.5) + # Original data
  geom_line(aes(y = predict(loess(
    step ~ as.numeric(time), span = 0.01
  )), color = "0.01")) +
  geom_line(aes(y = predict(loess(
    step ~ as.numeric(time), span = 0.05
  )), color = "0.05")) +
  geom_line(aes(y = predict(loess(
    step ~ as.numeric(time), span = 0.1
  )), color = "0.1")) +
  geom_line(aes(y = predict(loess(
    step ~ as.numeric(time), span = 0.5
  )), color = "0.5")) +
  labs(title = "Step Data Smoothing with Different Span Values", x = "Timestamp", y = "Step") +
  scale_color_manual(
    name = "Span Value",
    values = c(
      "0.01" = "red",
      "0.05" = "green",
      "0.1" = "orange",
      "0.5" = "purple"
    )
  ) +
  theme_minimal()

# Save the processed data to a new CSV file
#write.csv(step_data, file = "~/Desktop/multi-omics/data_smooth/data/smoothed_step_data.csv", row.names = FALSE)
#write.csv(heart_data, file = "~/Desktop/multi-omics/data_smooth/data/smoothed_heart_data.csv", row.names = FALSE)

# Save the processed (smoothed) data to .rda files
save(step_data, file = "~/Desktop/multi-omics/data_smooth/data/smoothed_step_data.rda")
save(heart_data, file = "~/Desktop/multi-omics/data_smooth/data/smoothed_heart_data.rda")
