# HW7_client_test.R
# Client Test Code for Patient No-Show Prediction API

library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(lubridate)

# API base URL (assuming server is running locally on default port 8000)
base_url <- "http://127.0.0.1:8000"

# Check if server is running
cat("Checking if API server is running...\n")
tryCatch({
  response <- GET(paste0(base_url, "/health"), timeout(2))
  cat("✓ Server is running!\n\n")
}, error = function(e) {
  cat("✗ ERROR: Cannot connect to API server at", base_url, "\n")
  cat("\nPlease start the server first by running:\n")
  cat("  library(plumber)\n")
  cat("  library(tidymodels)\n")
  cat("  api <- plumb('HW7_api_server.R')\n")
  cat("  api$run(host = '127.0.0.1', port = 8000)\n\n")
  stop("Server not running. Start the server and try again.")
})

# Create test data matching your model's features
# Features: provider_id, address, age, specialty, days_in_advance, appt_hour
# The appt_wday variables are derived features (created by the recipe)
raw_test <- read_csv("/Users/varunramanathan/Downloads/test_dataset.csv.gz")
test_data <- raw_test %>%
  mutate(
    days_in_advance = as.numeric(difftime(appt_time, appt_made, units = "days")),
    appt_hour       = lubridate::hour(appt_time),
    appt_wday       = lubridate::wday(appt_time, label = TRUE)
  ) %>%
  # only keep the predictor columns (no outcome)
  select(provider_id, address, age, specialty,
         days_in_advance, appt_hour, appt_wday) %>%
  # just take a few rows to test
  slice(1:3)

cat("Test Data (from HW4 test set):\n")
print(test_data)
cat("\n")

# Test 1: Health Check
cat("=" , rep("=", 50), "\n", sep = "")
cat("Test 1: Health Check\n")
cat("=" , rep("=", 50), "\n", sep = "")

response <- GET(paste0(base_url, "/health"))
if (status_code(response) == 200) {
  result <- content(response, as = "parsed")
  cat("Status:", result$status[[1]], "\n")
  cat("Message:", result$message[[1]], "\n")
  cat("✓ Health check passed\n\n")
} else {
  cat("✗ Health check failed with status code:", status_code(response), "\n\n")
}

# Test 2: Model Info
cat("=" , rep("=", 50), "\n", sep = "")
cat("Test 2: Model Information\n")
cat("=" , rep("=", 50), "\n", sep = "")

response <- GET(paste0(base_url, "/model_info"))
if (status_code(response) == 200) {
  result <- content(response, as = "parsed")
  cat("Model Class:", result$model_class[[1]], "\n")
  cat("Message:", result$message[[1]], "\n")
  cat("✓ Model info retrieved successfully\n\n")
} else {
  cat("✗ Model info failed with status code:", status_code(response), "\n\n")
}

# Test 3: Predict Probability
cat("=" , rep("=", 50), "\n", sep = "")
cat("Test 3: Predict Probability Endpoint\n")
cat("=" , rep("=", 50), "\n", sep = "")

response <- POST(
  url = paste0(base_url, "/predict_prob"),
  body = list(data = test_data),
  encode = "json",
  content_type_json()
)

if (status_code(response) == 200) {
  # Unserialize the return value
  result <- content(response, as = "parsed")
  predictions <- unlist(result$predictions)
  
  cat("Predictions (probabilities):\n")
  print(predictions)
  
  # Verify output
  cat("\nVerification:\n")
  cat("- Number of predictions:", length(predictions), "\n")
  cat("- Expected number:", nrow(test_data), "\n")
  cat("- All values between 0 and 1:", all(predictions >= 0 & predictions <= 1), "\n")
  
  if (length(predictions) == nrow(test_data) && all(predictions >= 0 & predictions <= 1)) {
    cat("✓ predict_prob endpoint test passed\n\n")
  } else {
    cat("✗ predict_prob endpoint test failed\n\n")
  }
} else {
  cat("✗ predict_prob request failed with status code:", status_code(response), "\n")
  cat("Error message:", content(response, as = "text"), "\n\n")
}

# Test 4: Predict Class (default threshold)
cat("=" , rep("=", 50), "\n", sep = "")
cat("Test 4: Predict Class Endpoint (default threshold = 0.5)\n")
cat("=" , rep("=", 50), "\n", sep = "")

response <- POST(
  url = paste0(base_url, "/predict_class"),
  body = list(data = test_data),
  encode = "json",
  content_type_json()
)

if (status_code(response) == 200) {
  # Unserialize the return value
  result <- content(response, as = "parsed")
  predictions <- unlist(result$predictions)
  
  cat("Predictions (classes):\n")
  print(predictions)
  
  # Verify output
  cat("\nVerification:\n")
  cat("- Number of predictions:", length(predictions), "\n")
  cat("- Expected number:", nrow(test_data), "\n")
  cat("- All values are 0 or 1:", all(predictions %in% c(0, 1)), "\n")
  
  if (length(predictions) == nrow(test_data) && all(predictions %in% c(0, 1))) {
    cat("✓ predict_class endpoint test passed\n\n")
  } else {
    cat("✗ predict_class endpoint test failed\n\n")
  }
} else {
  cat("✗ predict_class request failed with status code:", status_code(response), "\n")
  cat("Error message:", content(response, as = "text"), "\n\n")
}

# Test 5: Predict Class (custom threshold)
cat("=" , rep("=", 50), "\n", sep = "")
cat("Test 5: Predict Class Endpoint (custom threshold = 0.3)\n")
cat("=" , rep("=", 50), "\n", sep = "")

response <- POST(
  url = paste0(base_url, "/predict_class"),
  body = list(data = test_data, threshold = 0.3),
  encode = "json",
  content_type_json()
)

if (status_code(response) == 200) {
  # Unserialize the return value
  result <- content(response, as = "parsed")
  predictions <- unlist(result$predictions)
  
  cat("Predictions (classes with threshold = 0.3):\n")
  print(predictions)
  
  # Verify output
  cat("\nVerification:\n")
  cat("- Number of predictions:", length(predictions), "\n")
  cat("- Expected number:", nrow(test_data), "\n")
  cat("- All values are 0 or 1:", all(predictions %in% c(0, 1)), "\n")
  
  if (length(predictions) == nrow(test_data) && all(predictions %in% c(0, 1))) {
    cat("✓ predict_class endpoint (custom threshold) test passed\n\n")
  } else {
    cat("✗ predict_class endpoint (custom threshold) test failed\n\n")
  }
} else {
  cat("✗ predict_class request failed with status code:", status_code(response), "\n")
  cat("Error message:", content(response, as = "text"), "\n\n")
}

# Test 6: Single row prediction
cat("=" , rep("=", 50), "\n", sep = "")
cat("Test 6: Single Row Prediction\n")
cat("=" , rep("=", 50), "\n", sep = "")

single_row <- test_data[1, , drop = FALSE]
cat("Single row test data:\n")
print(single_row)

response <- POST(
  url = paste0(base_url, "/predict_prob"),
  body = list(data = single_row),
  encode = "json",
  content_type_json()
)

if (status_code(response) == 200) {
  result <- content(response, as = "parsed")
  prediction <- unlist(result$predictions)
  
  cat("\nPrediction probability:", prediction, "\n")
  cat("✓ Single row prediction test passed\n\n")
} else {
  cat("✗ Single row prediction failed\n\n")
}

cat("=" , rep("=", 50), "\n", sep = "")
cat("All tests completed!\n")
cat("=" , rep("=", 50), "\n", sep = "")