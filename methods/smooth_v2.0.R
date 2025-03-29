# Load packages
library(ggplot2)
library(dplyr)
library(zoo)

# Load step_data and heart_data from CSV files
#step_data <- read.csv("~/Desktop/multi-omics/data_smooth/data/step_data.csv")
#heart_data <- read.csv("~/Desktop/multi-omics/data_smooth/data/heart_data.csv")

# Load step_data and heart_data datasets
load("~/Desktop/multi-omics/data_smooth/data/step_data.rda")   
load("~/Desktop/multi-omics/data_smooth/data/heart_data.rda") 

# Check structure
str(step_data)
str(heart_data)

# Function to optimize LOESS parameters for different datasets
optimize_loess_parameters <- function(data, time_var, value_var, dataset_name) {
  set.seed(123)  # For reproducibility
  
  # Sort the data by time variable
  data <- data %>%
    arrange(!!sym(time_var))
  
  # Select the first third of the dataset for testing
  n <- nrow(data)
  test_data <- data[1:floor(n / 3), ]
  
  # Ensure that there are no NA values and value_var > 0
  test_data <- test_data %>%
    filter(!is.na(!!sym(value_var)) & !!sym(value_var) > 0)
  
  # Output the number of valid observations
  cat("Number of valid observations in test_data:", nrow(test_data), "\n")
  
  # If filtered test_data has no valid values, stop execution
  if (nrow(test_data) < 2) {
    stop("Not enough valid data points after filtering to fit the model.")
  }
  
  span_values <- seq(0.1, 0.5, by = 0.01)
  degree_values <- 1:2
  
  best_r2 <- -Inf
  best_params <- list(span = NA, degree = NA)
  
  for (span in span_values) {
    for (degree in degree_values) {
      # Check data length before fitting the model
      if (nrow(test_data) < 2) {
        next  # Skip if not enough data to fit
      }
      
      # Try-catch block to handle errors
      tryCatch({
        # Fit the LOESS model
        model <- loess(as.formula(paste(value_var, "~ as.numeric(", time_var, ")")), 
                       data = test_data, 
                       span = span, 
                       degree = degree)
        
        # Make predictions for the entire original data
        predictions <- predict(model, newdata = data)
        
        # Replace NA predictions with 0 for original zeros
        predictions[is.na(predictions)] <- 0
        predictions[data[[value_var]] == 0] <- 0  # Ensure predictions for original zeros are 0
        
        # Calculate R² only if there are no NA values in predictions and actual values
        if (any(is.na(predictions)) || any(is.na(data[[value_var]]))) {
          next  # Skip to next iteration if NA values are present
        }
        
        r_squared <- cor(data[[value_var]], predictions, use = "complete.obs")^2
        
        # Output current parameters and R² value for debugging
        cat(sprintf("Testing span = %.2f, degree = %d, R² = %.4f\n", span, degree, r_squared))
        
        # Update best parameters if the current R² is better
        if (abs(r_squared - 0.6) < abs(best_r2 - 0.6)) {
          best_r2 <- r_squared
          best_params <- list(span = span, degree = degree)
        }
        
      }, error = function(e) {
        message(sprintf("Error with span = %.2f and degree = %d: %s", span, degree, e$message))
      })
    }
  }
  
  # Check if best_params were updated
  if (is.na(best_params$span) || is.na(best_params$degree)) {
    cat("No valid parameters found for", dataset_name, ".\n")
  } else {
    # Save the best parameters with a clear distinction
    param_file <- paste0("~/Desktop/multi-omics/data_smooth/data/best_loess_parameters_", dataset_name, ".rda")
    save(best_params, file = param_file)
  }
  
  # Return the best parameters with a clear name
  assign(paste0(dataset_name, "_params"), best_params, envir = .GlobalEnv)  # Assign to global environment
}



# Function to smooth data using LOESS
smooth_data <- function(data, time_var, value_var, output_var, span, degree) {
  data %>%
    arrange(!!sym(time_var)) %>%
    mutate(!!sym(output_var) := case_when(
      !!sym(value_var) > 0 ~ predict(loess(!!sym(value_var) ~ as.numeric(!!sym(time_var)), 
                                           span = span, 
                                           degree = degree)),
      TRUE ~ 0
    ))
}

#MAE (Mean Absolute Error): Calculates the average of the absolute differences between the original data and the smoothed data, reflecting the absolute size of the errors.
#MSE (Mean Squared Error): Calculates the average of the squared differences, emphasizing the impact of larger errors.
#R² (Coefficient of Determination): Measures the model's ability to explain the data, with values closer to 1 indicating a better model.

# Function to evaluate smoothing effect
evaluate_smoothing <- function(original, smoothed) {
  if (length(original) != length(smoothed)) {
    stop("Original and smoothed vectors must be of the same length.")
  }
  
  mae <- mean(abs(original - smoothed))
  mse <- mean((original - smoothed) ^ 2)
  ss_total <- sum((original - mean(original)) ^ 2)
  ss_residual <- sum((original - smoothed) ^ 2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  return(list(MAE = mae, MSE = mse, R_squared = r_squared))
}

# Function to plot data
plot_data <- function(data, time_var, value_var, smoothed_var, title) {
  ggplot(data, aes_string(x = time_var)) +
    geom_line(aes_string(y = value_var, color = "'Original'"), size = 0.8) + 
    geom_line(aes_string(y = smoothed_var, color = "'Smoothed'"), size = 1) +
    labs(title = title, x = "Time", y = "Value") +
    scale_color_manual(values = c("Original" = "blue", "Smoothed" = "red")) +
    theme_minimal() +
    theme(legend.title = element_blank())
}


# Optimize parameters
optimize_loess_parameters(step_data, "time", "step", "step_data")
optimize_loess_parameters(heart_data, "time", "heart", "heart_data")

# Smooth the datasets with their respective best parameters
step_params <- step_data_params  # Retrieve step data parameters
step_data <- smooth_data(step_data, "time", "step", "smoothed_step", 
                         step_params$span, step_params$degree)

heart_params <- heart_data_params  # Retrieve heart data parameters
heart_data <- smooth_data(heart_data, "time", "heart", "smoothed_heart", 
                          heart_params$span, heart_params$degree)

#We focus on the R² value, which indicates that a higher value represents a closer fit to the original data, but with a weaker smoothing effect.
# Evaluate smoothing effect
step_eval <- evaluate_smoothing(step_data$step, step_data$smoothed_step)
print(step_eval)
heart_eval <- evaluate_smoothing(heart_data$heart, heart_data$smoothed_heart)
print(heart_eval)

# Use the function to plot
plot_data(step_data, "time", "step", "smoothed_step", "Step Data: Original vs Smoothed")
plot_data(heart_data, "time", "heart", "smoothed_heart", "Heart Rate Data: Original vs Smoothed")

# Save the processed data to a new CSV file
#write.csv(step_data, file = "~/Desktop/multi-omics/data_smooth/data/smoothed_step_data.csv", row.names = FALSE)
#write.csv(heart_data, file = "~/Desktop/multi-omics/data_smooth/data/smoothed_heart_data.csv", row.names = FALSE)

# Save the processed (smoothed) data to .rda files
save(step_data, file = "~/Desktop/multi-omics/data_smooth/data/smoothed_step_data.rda")
save(heart_data, file = "~/Desktop/multi-omics/data_smooth/data/smoothed_heart_data.rda")

