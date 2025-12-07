# HW7_api_server.R
# API Server for Patient No-Show Prediction Model

library(plumber)
library(tidymodels)
library(workflows)

model <- readRDS("noshow_best_model.rds")

# Helper: cast incoming data to match model's expected types
cast_to_model_types <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  # Basic numeric/integer casting
  if ("provider_id" %in% names(df)) {
    df$provider_id <- as.integer(df$provider_id)
  }
  
  if ("address" %in% names(df)) {
    df$address <- as.integer(df$address)
  }
  
  if ("age" %in% names(df)) {
    df$age <- as.numeric(df$age)
  }
  
  if ("specialty" %in% names(df)) {
    df$specialty <- as.numeric(df$specialty)
  }
  
  if ("days_in_advance" %in% names(df)) {
    df$days_in_advance <- as.numeric(df$days_in_advance)
  }
  
  if ("appt_hour" %in% names(df)) {
    df$appt_hour <- as.numeric(df$appt_hour)
  }
  
  # 🔴 IMPORTANT PART: make appt_wday match the model's type exactly
  if ("appt_wday" %in% names(df)) {
    # Get the prototype column from the workflow (what it was trained with)
    trained_wday <- model$pre$mold$blueprint$ptypes$predictors$appt_wday
    
    # Use its levels and ordered-ness
    trained_levels  <- levels(trained_wday)
    trained_ordered <- is.ordered(trained_wday)
    
    df$appt_wday <- factor(
      df$appt_wday,
      levels  = trained_levels,
      ordered = trained_ordered
    )
  }
  
  df
}

#* @apiTitle Patient No-Show Prediction API
#* @apiDescription API for predicting patient appointment no-shows

#* Predict probability of no-show
#* @param data:object Data frame with patient appointment features
#* @post /predict_prob
#* @serializer unboxedJSON
function(data) {
  df <- as.data.frame(data, stringsAsFactors = FALSE)
  df <- cast_to_model_types(df)
  
  if (nrow(df) == 0) {
    stop("Input data frame is empty")
  }
  
  predictions <- predict(model, new_data = df, type = "prob")
  prob_col <- grep("^.pred_", names(predictions), value = TRUE)[1]
  pred_vector <- predictions[[prob_col]]
  
  list(predictions = as.numeric(pred_vector))
}

#* Predict class (0 or 1) for no-show
#* @param data:object Data frame with patient appointment features
#* @param threshold:numeric Classification threshold (default 0.5)
#* @post /predict_class
#* @serializer unboxedJSON
function(data, threshold = 0.5) {
  df <- as.data.frame(data, stringsAsFactors = FALSE)
  df <- cast_to_model_types(df)
  
  if (nrow(df) == 0) {
    stop("Input data frame is empty")
  }
  
  threshold <- as.numeric(threshold)
  if (is.na(threshold) || threshold < 0 || threshold > 1) {
    threshold <- 0.5
  }
  
  prob_preds <- predict(model, new_data = df, type = "prob")
  prob_col <- grep("^.pred_", names(prob_preds), value = TRUE)[1]
  probs <- prob_preds[[prob_col]]
  
  classes <- ifelse(probs >= threshold, 1, 0)
  
  list(predictions = as.integer(classes))
}

#* Health check endpoint
#* @get /health
function() {
  list(status = "OK", message = "API is running")
}

#* Get model information
#* @get /model_info
function() {
  list(
    model_class = class(model)[1],
    message = "Patient no-show prediction model"
  )
}
