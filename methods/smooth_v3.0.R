# Load packages
library(ggplot2)
library(dplyr)
library(zoo)
library(parallel)  # For parallel processing
library(foreach)   # For parallel loops
library(doParallel)
#library(caret)  # For cross-validation

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
  
  cat("Number of valid observations in test_data:", nrow(test_data), "\n")
  
  if (nrow(test_data) < 2) {
    stop("Not enough valid data points after filtering to fit the model.")
  }
  
  span_values <- seq(0.1, 0.5, by = 0.01)
  degree_values <- 1:2
  
  # Register parallel backend
  registerDoParallel(detectCores() - 1)
  
  # Initialize an empty list to store results
  result <- foreach(span = span_values, .combine = rbind, .packages = c('dplyr')) %:%
    foreach(degree = degree_values, .combine = rbind) %dopar% {
      tryCatch({
        # Fit the LOESS model
        model <- loess(as.formula(paste(value_var, "~ as.numeric(", time_var, ")")), 
                       data = test_data, 
                       span = span, 
                       degree = degree)
        
        # Make predictions for the entire original data
        predictions <- predict(model, newdata = data)
        
        # Handle NA values in predictions
        predictions[is.na(predictions)] <- 0
        predictions[data[[value_var]] == 0] <- 0  # Ensure predictions for original zeros are 0
        
        # Calculate R²
        r_squared <- cor(data[[value_var]], predictions, use = "complete.obs")^2
        
        # Return the span, degree, and r_squared as a named list (or data.frame)
        return(data.frame(span = span, degree = degree, r_squared = r_squared))
        
      }, error = function(e) {
        return(NULL)  # Ignore errors and return NULL for problematic cases
      })
    }
  
  # Ensure the result is a data frame
  valid_results <- as.data.frame(result)
  
  # Select the best parameters based on proximity to R² target
  best_result <- valid_results[which.min(abs(valid_results$r_squared - 0.6)), ]
  
  # Save the best parameters
  param_file <- paste0("~/Desktop/multi-omics/data_smooth/data/best_loess_parameters_", dataset_name, ".rda")
  best_params <- list(span = best_result$span, degree = best_result$degree)
  save(best_params, file = param_file)
  
  # Assign the best parameters to a global variable
  assign(paste0(dataset_name, "_params"), best_params, envir = .GlobalEnv)
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

# Evaluate smoothing effect
evaluate_smoothing <- function(original, smoothed) {
  mae <- mean(abs(original - smoothed))
  mse <- mean((original - smoothed) ^ 2)
  r_squared <- 1 - sum((original - smoothed) ^ 2) / sum((original - mean(original)) ^ 2)
  
  return(list(MAE = mae, MSE = mse, R_squared = r_squared))
}

# Plot data
plot_data <- function(data, time_var, value_var, smoothed_var, title) {
  ggplot(data, aes_string(x = time_var)) +
    geom_line(aes_string(y = value_var, color = "'Original'"), size = 0.8) + 
    geom_line(aes_string(y = smoothed_var, color = "'Smoothed'"), size = 1) +
    labs(title = title, x = "Time", y = "Value") +
    scale_color_manual(values = c("Original" = "blue", "Smoothed" = "red")) +
    theme_minimal() +
    theme(legend.title = element_blank())
}

# Dataset and parameter names
datasets <- list(step_data = step_data, heart_data = heart_data)
var_names <- list(step_data = list(time = "time", value = "step"),
                  heart_data = list(time = "time", value = "heart"))

# Loop through datasets to optimize, smooth, and evaluate
for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]
  time_var <- var_names[[dataset_name]]$time
  value_var <- var_names[[dataset_name]]$value
  
  # Optimize parameters
  optimize_loess_parameters(dataset, time_var, value_var, dataset_name)
  
  # Smooth data
  best_params <- get(paste0(dataset_name, "_params"))
  dataset <- smooth_data(dataset, time_var, value_var, paste0("smoothed_", value_var), 
                         best_params$span, best_params$degree)
  
  # Evaluate smoothing effect
  eval_results <- evaluate_smoothing(dataset[[value_var]], dataset[[paste0("smoothed_", value_var)]])
  print(paste0(dataset_name, " smoothing evaluation:"))
  print(eval_results)
  
  # Plot data
  # plot_data(dataset, time_var, value_var, paste0("smoothed_", value_var), paste0(dataset_name, ": Original vs Smoothed"))
  
  # Save smoothed data
  save(dataset, file = paste0("~/Desktop/multi-omics/data_smooth/data/smoothed_", dataset_name, ".rda"))
}

