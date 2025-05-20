## Load libraries
library(nflreadr)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(pROC)
library(caret)
library(gbm)
library(scales)  # For normalization

# -------------------------------
# Step 1: Data Acquisition (Using 2023 Data)
# -------------------------------
pbp_2023 <- load_pbp(2023)

# Filter key variables and handle missing values for yards gained
pbp_clean <- pbp_2023 %>%
  filter(
    !is.na(game_seconds_remaining),
    !is.na(yardline_100),
    !is.na(down),
    !is.na(score_differential),
    !is.na(posteam_timeouts_remaining)
  ) %>%
  mutate(
    yards_gained = ifelse(is.na(yards_gained), 0, yards_gained)
  )

# -------------------------------
# Step 2: Drive-Level Aggregation (Corrected)
# -------------------------------
drive_data <- pbp_clean %>%
  group_by(game_id, drive) %>%
  summarize(
    posteam       = first(posteam),
    home_team     = first(home_team),      # For defensive team identification
    away_team     = first(away_team),
    drive_start   = max(game_seconds_remaining, na.rm = TRUE),
    drive_end     = min(game_seconds_remaining, na.rm = TRUE),
    drive_time    = drive_start - drive_end,
    start_F       = first(yardline_100),   # Starting field position (F)
    end_F         = last(yardline_100),
    max_down      = max(down, na.rm = TRUE),  # Current down (d)
    timeouts      = first(posteam_timeouts_remaining),
    # If trailing, require (margin + 3); otherwise require 3 points
    P_req_initial = ifelse(first(score_differential) < 0,
                           abs(first(score_differential)) + 3,
                           3),
    P_req_current = ifelse(last(score_differential) < 0,
                           abs(last(score_differential)) + 3,
                           3),
    .groups = "drop"
  ) %>%
  filter(drive_time > 0, start_F > 0) %>%
  # Identify defensive team
  mutate(defteam = ifelse(posteam == home_team, away_team, home_team))

# Compute drive progress (OPI)
drive_data <- drive_data %>%
  mutate(
    actual_yards   = pmax(start_F - end_F, 0),     # Prevent negative
    expected_yards = 100 - start_F,
    OPI            = (actual_yards + 1) / (expected_yards + 1)  # Smoothing
  )

# -------------------------------
# Step 3: Pressure Index Calculations (Revised)
# -------------------------------
drive_data <- drive_data %>%
  mutate(
    # Required scoring rates
    IRSR = P_req_initial / drive_time,             # Eq. (1)
    CRSR = P_req_current / drive_time,             # Eq. (2) corrected
    
    # Offensive Pressure Indices
    PIO1      = (CRSR / IRSR) * (1 + max_down / 4),     # Eq. (3)
    PIO2      = (CRSR / IRSR) * exp(start_F / 50),      # Eq. (4)
    PIO1_norm = rescale(PIO1, to = c(0, 100)),
    PIO2_norm = rescale(PIO2, to = c(0, 100)),
    
    # Defensive Pressure Index
    PID = OPI * (((4 - max_down) / 4) + exp(-start_F / 100))  # Eq. (7)
  )

# -------------------------------
# Step 4: Optimal Weight for PIO3 (Improved)
# -------------------------------
# First, merge drive success indicator into drive_data.
# A drive is considered successful if any play in that drive resulted in a touchdown.
drive_success <- pbp_clean %>%
  group_by(game_id, drive) %>%
  summarize(
    success = as.factor(ifelse(sum(touchdown, na.rm = TRUE) > 0, "Success", "Failure")),
    .groups = "drop"
  )

drive_data <- drive_data %>%
  left_join(drive_success, by = c("game_id", "drive"))

# Grid search for optimal weight w for the composite offensive pressure index PIO3.
w_values <- seq(0, 1, by = 0.05)
grid_results <- data.frame(w = numeric(), AUC = numeric())

for (w in w_values) {
  drive_data$PIO3_temp <- w * drive_data$PIO1_norm + (1 - w) * drive_data$PIO2_norm
  
  # Fit a logistic regression model using PIO3_temp, PID, and timeouts to predict drive success.
  model_temp <- glm(
    success ~ PIO3_temp + PID + timeouts,
    data = drive_data,
    family = binomial
  )
  
  # Obtain predicted probabilities and compute ROC AUC.
  pred_probs_temp <- predict(model_temp, type = "response")
  roc_temp <- roc(response = as.numeric(drive_data$success) - 1,
                  predictor = pred_probs_temp)
  auc_temp <- auc(roc_temp)
  
  grid_results <- rbind(grid_results, data.frame(w = w, AUC = auc_temp))
}

print(grid_results)  # View AUC for each candidate weight
best_w <- grid_results$w[which.max(grid_results$AUC)]
cat("Optimal weight w is: ", best_w, "\n")

# Compute the final composite Offensive Pressure Index (PIO3) using the optimal weight.
drive_data <- drive_data %>%
  mutate(PIO3 = best_w * PIO1_norm + (1 - best_w) * PIO2_norm)

# -------------------------------
# Step 5: Modeling (Defensive Team Fix)
# -------------------------------
set.seed(123)
train_idx <- createDataPartition(drive_data$success, p = 0.8, list = FALSE)
train_data <- drive_data[train_idx, ]
test_data <- drive_data[-train_idx, ]

# Define model formulas.
log_formula <- success ~ PIO3 + PID + timeouts
ensemble_formula <- success ~ PIO1_norm + PIO2_norm + PIO3 + PID + timeouts

# Logistic Regression Model.
log_model <- glm(log_formula, data = train_data, family = binomial)
summary(log_model)

# Random Forest Model.
# Remove rows with missing values to avoid errors.
train_data_clean <- na.omit(train_data)
rf_model <- train(
  ensemble_formula,
  data = train_data_clean,
  method = "rf",
  tuneGrid = expand.grid(mtry = 3),
  trControl = trainControl(method = "cv", number = 5),
  ntree = 200
)
print(rf_model)

# Gradient Boosting Model.
gbm_model <- train(
  ensemble_formula,
  data = train_data_clean,
  method = "gbm",
  tuneGrid = expand.grid(
    interaction.depth = 4,
    n.trees = 200,
    shrinkage = 0.05,
    n.minobsinnode = 15
  ),
  verbose = FALSE
)
print(gbm_model)

# -------------------------------
# Step 6: Model Evaluation
# -------------------------------
test_data <- test_data %>%
  mutate(
    log_pred = predict(log_model, newdata = test_data, type = "response"),
    rf_pred = predict(rf_model, newdata = test_data, type = "prob")[, "Success"],
    gbm_pred = predict(gbm_model, newdata = test_data, type = "prob")[, "Success"]
  )

roc_log <- roc(test_data$success, test_data$log_pred)
roc_rf <- roc(test_data$success, test_data$rf_pred)
roc_gbm <- roc(test_data$success, test_data$gbm_pred)

# (1) Increase all base sizes
par(
  cex.main = 1.6,   # Title
  cex.lab  = 1.4,   # Axis labels
  cex.axis = 1.2,   # Tick labels
  mar      = c(5,5,4,2) + 0.1  # Margins (bottom, left, top, right)
)

# (2) Plot with thicker lines & a grid
plot(
  roc_log,
  col         = "blue",
  lwd         = 4,            # thicker curve
  lty         = 1,            # solid line
  legacy.axes = TRUE,         # Specificity on x
  xlab        = "Specificity",
  ylab        = "Sensitivity",
  main        = "ROC Curves",
  print.auc   = FALSE         # we'll do in legend
)




# (3) Add the other two curves
lines(roc_rf,  col = "forestgreen", lwd = 4, lty = 1)  # dashed green
lines(roc_gbm, col = "firebrick",   lwd = 4, lty = 1)  # dotted red



# (5) Legend with no box, larger text
legend(
  "bottomright",
  legend = c(
    sprintf("Logistic Regression (AUC = %.3f)", auc(roc_log)),
    sprintf("Random Forest        (AUC = %.3f)", auc(roc_rf)),
    sprintf("Gradient Boosting    (AUC = %.3f)", auc(roc_gbm))
  ),
  col    = c("blue", "forestgreen", "firebrick"),
  lwd    = 4,
  lty    = c(1,1,1),
  bty    = "0",     # box around legend
  cex    = 1      # legend text size
)


# -------------------------------
# Step 7: Metrics Calculation for All Models
# -------------------------------
# Define a function to compute metrics: Accuracy, Precision, Recall, F1-Score, and AUC.
compute_metrics <- function(probabilities, true_labels, threshold = 0.5) {
  # Ensure true_labels has the correct levels
  true_labels <- factor(true_labels, levels = c("Failure", "Success"))
  pred_class <- factor(ifelse(probabilities > threshold, "Success", "Failure"), 
                       levels = c("Failure", "Success"))
  
  # Print contingency table for debugging
  print(table(Predicted = pred_class, True = true_labels))
  
  cm <- confusionMatrix(pred_class, true_labels)
  accuracy <- as.numeric(cm$overall["Accuracy"])
  precision <- as.numeric(cm$byClass["Pos Pred Value"])
  recall <- as.numeric(cm$byClass["Sensitivity"])
  
  # Compute F1-Score; if precision+recall is 0, then F1 is set to NA.
  if (is.na(precision) || is.na(recall) || (precision + recall) == 0) {
    f1 <- NA
  } else {
    f1 <- (2 * precision * recall) / (precision + recall)
  }
  
  auc_val <- auc(roc(true_labels, probabilities))
  return(c(Accuracy = accuracy, Precision = precision, Recall = recall, F1_Score = f1, AUC = auc_val))
}

metrics_log <- compute_metrics(test_data$log_pred, test_data$success)
metrics_rf <- compute_metrics(test_data$rf_pred, test_data$success)
metrics_gbm <- compute_metrics(test_data$gbm_pred, test_data$success)

report <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "GBM"),
  Accuracy = c(metrics_log["Accuracy"], metrics_rf["Accuracy"], metrics_gbm["Accuracy"]),
  Precision = c(metrics_log["Precision"], metrics_rf["Precision"], metrics_gbm["Precision"]),
  Recall = c(metrics_log["Recall"], metrics_rf["Recall"], metrics_gbm["Recall"]),
  F1_Score = c(metrics_log["F1_Score"], metrics_rf["F1_Score"], metrics_gbm["F1_Score"]),
  AUC = c(metrics_log["AUC"], metrics_rf["AUC"], metrics_gbm["AUC"])
)
print(report)

# -------------------------------
# Step 8: Team Analysis (Defensive Correction)
# -------------------------------
# Offensive pressure summary by posteam.
offensive_summary <- drive_data %>%
  group_by(posteam) %>%
  summarize(
    avg_PIO3 = mean(PIO3, na.rm = TRUE),
    off_success_rate = mean(success == "Success", na.rm = TRUE),
    .groups = "drop"
  )

# Defensive pressure summary by defensive team (defteam).
defensive_summary <- drive_data %>%
  group_by(defteam) %>%
  summarize(
    avg_PID = mean(PID, na.rm = TRUE),
    def_success_rate = mean(success == "Failure", na.rm = TRUE),  # Defense success = offensive failure
    .groups = "drop"
  )

# For offensive pressure: Arrange in descending order so the highest value is at the top.
offensive_summary <- offensive_summary %>% 
  arrange(desc(avg_PIO3))
offensive_summary$posteam <- factor(offensive_summary$posteam, levels = offensive_summary$posteam)

ggplot(offensive_summary, aes(x = posteam, y = avg_PIO3)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  scale_x_discrete(limits = rev(levels(offensive_summary$posteam))) +
  labs(title = "Offensive Pressure (PIO3) by Team", x = "Team", y = "Average PIO3") +
  coord_flip()

# For defensive pressure: Arrange in descending order.
defensive_summary <- defensive_summary %>% 
  arrange(desc(avg_PID))
defensive_summary$defteam <- factor(defensive_summary$defteam, levels = defensive_summary$defteam)

ggplot(defensive_summary, aes(x = defteam, y = avg_PID)) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  scale_x_discrete(limits = rev(levels(defensive_summary$defteam))) +
  labs(title = "Defensive Pressure (PID) by Team", x = "Team", y = "Average PID") +
  coord_flip()
