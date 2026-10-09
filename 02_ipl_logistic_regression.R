# =============================================================================
# 02_ipl_logistic_regression.R
# Predicting IPL match outcomes with Logistic Regression
# (baselines: Linear Regression, Random Forest).  Run from the project folder.
# =============================================================================
pkgs <- c("caret", "ranger", "pROC", "ggplot2", "corrplot")
new  <- setdiff(pkgs, rownames(installed.packages()))
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))
options(width = 100, bitmapType = "cairo")
dir.create("plots", showWarnings = FALSE)
STEEL <- "#4682B4"; ORNG <- "#FF8C00"
theme_set(theme_minimal(base_size = 14))

# ---- SLIDE 3: Loading Dataset ----
ipl <- read.csv("ipl_matches.csv", stringsAsFactors = FALSE)
head(ipl)

# ---- SLIDE 4: Anatomy of the Dataset ----
dim(ipl)
names(ipl)

# ---- SLIDE 5: Structure of the Dataset ----
str(ipl)

# ---- SLIDE 6: Summary Statistics ----
summary(ipl)

# ---- SLIDE 7: Missing Values Check ----
na_summary <- colSums(is.na(ipl))
na_summary

# ---- SLIDE 8: Data Cleaning ----
dup_count <- sum(duplicated(ipl))
cat("Duplicate rows:", dup_count, "\n")
ipl_clean <- ipl[!duplicated(ipl), ]
get_mode <- function(x) { ux <- unique(na.omit(x)); ux[which.max(tabulate(match(x, ux)))] }
for (col in names(ipl_clean)) {
  if (is.numeric(ipl_clean[[col]])) {
    ipl_clean[[col]][is.na(ipl_clean[[col]])] <- median(ipl_clean[[col]], na.rm = TRUE)
  } else {
    ipl_clean[[col]][is.na(ipl_clean[[col]])] <- get_mode(ipl_clean[[col]])
  }
}
ipl_clean$team1_win <- ifelse(ipl_clean$winner == ipl_clean$team1, 1, 0)
ipl_clean <- ipl_clean[, !(names(ipl_clean) %in% c("match_id", "date", "city", "umpire1", "umpire2"))]
cat("Remaining NAs:", sum(is.na(ipl_clean)), "| Columns kept:", ncol(ipl_clean), "\n")

# ---- SLIDE 9: Visualization 1 - Target Distribution ----
table(ipl_clean$team1_win)
prop.table(table(ipl_clean$team1_win))
p1 <- ggplot(ipl_clean, aes(x = factor(team1_win))) +
  geom_bar(fill = c(ORNG, STEEL), width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 5) +
  labs(title = "Distribution of team1_win", x = "team1_win (0 = team2 wins, 1 = team1 wins)", y = "Matches")
ggsave("plots/01_target_distribution.png", p1, width = 7, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 10: Visualization 2 - Toss Decision ----
table(ipl_clean$toss_decision, ipl_clean$team1_win)
ipl_clean$toss_team1 <- ifelse(ipl_clean$toss_winner == ipl_clean$team1, 1, 0)
p2 <- ggplot(ipl_clean, aes(x = toss_decision, fill = factor(winner == toss_winner,
              labels = c("Toss loser won", "Toss winner won")))) +
  geom_bar(position = "dodge") + scale_fill_manual(values = c(ORNG, STEEL)) +
  labs(title = "Toss decision vs match outcome", x = "Toss decision", y = "Matches", fill = NULL)
ggsave("plots/02_toss_decision.png", p2, width = 7, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 11: Visualization 3 - Teams, Correlation, Outliers ----
win_counts <- sort(table(ipl_clean$winner), decreasing = TRUE)
win_counts
top_df <- data.frame(team = names(win_counts), wins = as.vector(win_counts))
p3 <- ggplot(top_df, aes(x = reorder(team, wins), y = wins)) +
  geom_col(fill = STEEL) + coord_flip() +
  labs(title = "Top 10 winning teams", x = NULL, y = "Wins")
ggsave("plots/03_top_teams.png", p3, width = 7, height = 5, dpi = 150, bg = "white")
num_vars <- ipl_clean[, sapply(ipl_clean, is.numeric)]
cor_mat <- cor(num_vars, use = "pairwise.complete.obs")
round(cor_mat[, "team1_win"], 3)
png("plots/04_correlation.png", width = 1000, height = 900, res = 150, bg = "white")
corrplot(cor_mat, method = "color", tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7)
dev.off()
box_df <- stack(ipl_clean[, c("team1_runs", "team2_runs", "team1_wickets")])
p5 <- ggplot(box_df, aes(x = ind, y = values)) + geom_boxplot(fill = STEEL, alpha = 0.7, outlier.color = ORNG) +
  facet_wrap(~ ind, scales = "free") +
  labs(title = "Boxplots for outlier detection", x = NULL, y = "Value") +
  theme(axis.text.x = element_blank())
ggsave("plots/05_boxplots.png", p5, width = 7, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 12: Train/Test Split ----
home_venue <- c("Chennai Super Kings" = "MA Chidambaram Stadium", "Mumbai Indians" = "Wankhede Stadium",
  "Royal Challengers Bangalore" = "M Chinnaswamy Stadium", "Kolkata Knight Riders" = "Eden Gardens",
  "Delhi Capitals" = "Arun Jaitley Stadium", "Rajasthan Royals" = "Sawai Mansingh Stadium",
  "Sunrisers Hyderabad" = "Rajiv Gandhi Stadium", "Punjab Kings" = "PCA Stadium Mohali",
  "Lucknow Super Giants" = "Ekana Stadium", "Gujarat Titans" = "Narendra Modi Stadium")
team_levels <- names(home_venue)
# pre-match features only (runs / wickets / winner are known only AFTER the match = leakage)
model_df <- data.frame(
  team1         = factor(ipl_clean$team1, levels = team_levels),
  team2         = factor(ipl_clean$team2, levels = team_levels),
  toss_team1    = ipl_clean$toss_team1,
  toss_decision = factor(ipl_clean$toss_decision),
  team1_home    = as.numeric(ipl_clean$venue == home_venue[ipl_clean$team1]),
  team2_home    = as.numeric(ipl_clean$venue == home_venue[ipl_clean$team2]),
  team1_win     = ipl_clean$team1_win)
set.seed(123)
trainIndex <- createDataPartition(factor(model_df$team1_win), p = 0.8, list = FALSE)
train <- model_df[trainIndex, ]
test  <- model_df[-trainIndex, ]
cat("Train rows:", nrow(train), "| Test rows:", nrow(test), "\n")
round(prop.table(table(train$team1_win)), 3)
round(prop.table(table(test$team1_win)), 3)

# ---- SLIDE 14: Logistic Regression - Code & Summary ----
glm_model <- glm(team1_win ~ ., data = train, family = binomial)
summary(glm_model)
round(exp(coef(glm_model))[c("toss_team1", "team1_home", "team2_home")], 2)   # odds ratios

# ---- SLIDE 15: Logistic Regression - Evaluation ----
glm_probs      <- predict(glm_model, newdata = test, type = "response")
threshold      <- 0.5
glm_pred_class <- ifelse(glm_probs > threshold, 1, 0)
pred_factor    <- factor(glm_pred_class, levels = c(0, 1))
ref_factor     <- factor(test$team1_win, levels = c(0, 1))
conf <- caret::confusionMatrix(pred_factor, ref_factor, positive = "1")
print(conf)
cm_df <- as.data.frame(conf$table)
p_cm <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") + geom_text(aes(label = Freq), size = 9, color = "white") +
  scale_fill_gradient(low = "#9fc0dc", high = "#2b5d8a") + labs(title = "Confusion matrix (test set)") +
  theme(legend.position = "none")
ggsave("plots/06_confusion_matrix.png", p_cm, width = 6, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 16: ROC Curve & AUC ----
roc_obj <- pROC::roc(test$team1_win, glm_probs, levels = c(0, 1), direction = "<")
auc_glm <- as.numeric(pROC::auc(roc_obj))
cat("Logistic Regression AUC:", round(auc_glm, 4), "\n")
png("plots/07_roc_curve.png", width = 1000, height = 900, res = 150, bg = "white")
plot(roc_obj, col = STEEL, lwd = 4, print.auc = TRUE, main = "ROC curve - Logistic Regression")
dev.off()

# ---- SLIDE 17: Linear Regression - Baseline ----
lm_model <- lm(team1_win ~ ., data = train)
lm_pred  <- predict(lm_model, newdata = test)
lm_rmse  <- sqrt(mean((lm_pred - test$team1_win)^2))
lm_r2    <- cor(lm_pred, test$team1_win)^2
cat("Linear Regression -> RMSE:", round(lm_rmse, 4), " R2:", round(lm_r2, 4), "\n")
cat("Predictions outside [0,1]:", sum(lm_pred < 0 | lm_pred > 1), "\n")
lm_df <- data.frame(actual = test$team1_win, predicted = lm_pred)
p_lm <- ggplot(lm_df, aes(x = factor(actual), y = predicted)) +
  geom_jitter(width = 0.15, alpha = 0.6, color = STEEL) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = ORNG, linewidth = 1) +
  labs(title = "Linear Regression: predicted vs actual", x = "Actual team1_win", y = "Predicted value")
ggsave("plots/08_linear_actual_vs_predicted.png", p_lm, width = 7, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 18: Random Forest - Comparison ----
train_rf <- train; train_rf$team1_win <- factor(train_rf$team1_win, levels = c(0, 1))
set.seed(123)
rf_model <- ranger(team1_win ~ ., data = train_rf, num.trees = 200,
                   probability = TRUE, importance = "impurity")
rf_probs <- predict(rf_model, data = test)$predictions[, "1"]
rf_class <- factor(ifelse(rf_probs > 0.5, 1, 0), levels = c(0, 1))
rf_acc   <- mean(rf_class == ref_factor)
auc_rf   <- as.numeric(pROC::auc(pROC::roc(test$team1_win, rf_probs, levels = c(0, 1), direction = "<")))
cat("Ranger Random Forest -> Accuracy:", round(rf_acc, 4), " AUC:", round(auc_rf, 4), "\n")
imp <- sort(rf_model$variable.importance, decreasing = TRUE)
round(imp, 2)
imp_df <- data.frame(feature = names(imp), importance = as.numeric(imp))
p_imp <- ggplot(imp_df, aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = STEEL) + coord_flip() + labs(title = "Random Forest: feature importance", x = NULL, y = "Impurity importance")
ggsave("plots/09_rf_importance.png", p_imp, width = 7, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 19: Random Forest - Actual vs Predicted ----
rf_df <- data.frame(actual = factor(test$team1_win), predicted = rf_probs)
p_rf <- ggplot(rf_df, aes(x = actual, y = predicted)) +
  geom_jitter(width = 0.15, alpha = 0.6, color = STEEL) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = ORNG, linewidth = 1) +
  labs(title = "Random Forest: predicted probability vs actual", x = "Actual team1_win", y = "Predicted P(team1 wins)")
ggsave("plots/10_rf_actual_vs_predicted.png", p_rf, width = 7, height = 5, dpi = 150, bg = "white")

# ---- SLIDE 20: Model Comparison Table ----
glm_acc <- as.numeric(conf$overall["Accuracy"])
results <- data.frame(
  model = c("Logistic Regression", "Linear Regression", "Random Forest"),
  type  = c("Classification", "Regression", "Ensemble"),
  accuracy_or_r2 = round(c(glm_acc, lm_r2, rf_acc), 4),
  rmse  = c(NA, round(lm_rmse, 4), NA),
  auc   = round(c(auc_glm, NA, auc_rf), 4),
  kappa = round(c(as.numeric(conf$overall["Kappa"]), NA, NA), 4))
write.csv(results, "model_results.csv", row.names = FALSE)
results
# write the significant logistic predictors to a file for reference
coefs <- summary(glm_model)$coefficients
write.csv(round(coefs, 4), "glm_coefficients.csv")
