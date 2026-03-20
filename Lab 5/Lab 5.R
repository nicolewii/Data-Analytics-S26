library(e1071)
library(randomForest)
library(caret)
set.seed(42)

col_names <- c("Class",
               "Alcohol", "MalicAcid", "Ash", "AlcalAsh", "Magnesium",
               "TotalPhenols", "Flavanoids", "NonflavPhenols",
               "Proanthocyanins", "ColorIntensity", "Hue",
               "OD280", "Proline")
setwd("~/Documents/GitHub/Data-Analytics-S26/Lab 5")

wine <- read.csv("wine.data", header = FALSE, col.names = col_names)
wine$Class <- as.factor(wine$Class)
print(table(wine$Class))

# Finding top features
numeric_class <- as.numeric(wine$Class)
cors <- sapply(wine[, -1], function(x) abs(cor(x, numeric_class)))
print(sort(cors, decreasing = TRUE))

#Top 6 features
features <- c("Flavanoids", "OD280", "TotalPhenols", "Proline", "Hue", "AlcalAsh")

#Train/Test split 
train_idx  <- createDataPartition(wine$Class, p = 0.75, list = FALSE)
train_data <- wine[train_idx, ]
test_data  <- wine[-train_idx, ]

X_train <- train_data[, features]
y_train <- train_data$Class
X_test  <- test_data[, features]
y_test  <- test_data$Class

cat(sprintf("Train: %d  |  Test: %d\n\n", nrow(train_data), nrow(test_data)))

# helper function to get all of the precision, recall, and f1 stats
classification_metrics <- function(cm_table, model_name) {
  classes <- rownames(cm_table)
  n       <- length(classes)
  
  precision <- recall <- f1 <- numeric(n)
  for (i in seq_len(n)) {
    tp <- cm_table[i, i]
    fp <- sum(cm_table[, i]) - tp
    fn <- sum(cm_table[i, ]) - tp
    precision[i] <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
    recall[i]    <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
    f1[i]        <- ifelse((precision[i] + recall[i]) == 0, 0,
                           2 * precision[i] * recall[i] /
                             (precision[i] + recall[i]))
  }
  
  macro_p  <- mean(precision)
  macro_r  <- mean(recall)
  macro_f1 <- mean(f1)
  
  #Printing the model
  cat(sprintf("-----%s-----\n",
              model_name))
  #Print confusion matrix
  cat("Confusion matrix:\n")
  print(cm_table)
  
  #Print precision, recall, and f1 for each model
  cat(sprintf("\n%-12s %10s %10s %10s\n", "Class", "Precision", "Recall", "F1"))
  cat(strrep("-", 46), "\n")
  for (i in seq_len(n)) {
    cat(sprintf("%-12s %10.4f %10.4f %10.4f\n",
                classes[i], precision[i], recall[i], f1[i]))
  }
  #Also print its micro avg
  cat("\n")
  cat(sprintf("%-12s %10.4f %10.4f %10.4f\n",
              "Macro avg", macro_p, macro_r, macro_f1))
  
  #Returning values but not printing them yet
  invisible(list(precision = precision, recall = recall, f1 = f1,
                 macro_p = macro_p, macro_r = macro_r, macro_f1 = macro_f1))
}

# Tuning linear kernel
cat("SVM (linear kernel)\n")
tune_lin <- tune.svm(
  x      = X_train,
  y      = y_train,
  kernel = "linear",
  cost   = c(0.01, 0.1, 1, 10, 100),
  tunecontrol = tune.control(cross = 5)   # 5-fold CV
)
cat("Linear SVM best parameters:\n")
print(tune_lin$best.parameters)
cat(sprintf("Best CV error: %.4f  (accuracy: %.4f)\n",
            tune_lin$best.performance,
            1 - tune_lin$best.performance))

svm_lin  <- tune_lin$best.model
pred_lin <- predict(svm_lin, X_test)
cm_lin   <- table(True = y_test, Predicted = pred_lin)
m_lin    <- classification_metrics(cm_lin, "SVM – Linear kernel")

cat("SVM (RBF kernel)\n")
tune_rbf <- tune.svm(
  x      = X_train,
  y      = y_train,
  kernel = "radial",
  cost   = c(0.1, 1, 10, 100),
  gamma  = c(0.001, 0.01, 0.1, 1),
  tunecontrol = tune.control(cross = 5)
)
cat("RBF SVM best parameters:\n")
print(tune_rbf$best.parameters)
cat(sprintf("Best CV error: %.4f  (accuracy: %.4f)\n",
            tune_rbf$best.performance,
            1 - tune_rbf$best.performance))

svm_rbf  <- tune_rbf$best.model
pred_rbf <- predict(svm_rbf, X_test)
cm_rbf   <- table(True = y_test, Predicted = pred_rbf)
m_rbf    <- classification_metrics(cm_rbf, "SVM – RBF kernel")

cat("\nTraining Random Forest (500 trees)\n")
rf_model <- randomForest(
  x      = X_train,
  y      = y_train,
  ntree  = 500,
  mtry   = floor(sqrt(length(features))),  # default for classification
  importance = TRUE
)
pred_rf <- predict(rf_model, X_test)
cm_rf   <- table(True = y_test, Predicted = pred_rf)
m_rf    <- classification_metrics(cm_rf, "Random Forest")

cat("\nVariable importance (Mean Decrease Gini):\n")
print(importance(rf_model, type = 2))

cat("\n Comparing Models")
cat(sprintf("%-18s %12s %12s %12s\n", "Model", "Precision", "Recall", "F1-Score"))
results <- list(
  "SVM Linear"   = m_lin,
  "SVM RBF"      = m_rbf,
  "Random Forest" = m_rf
)
for (nm in names(results)) {
  r <- results[[nm]]
  cat(sprintf("%-18s %12.4f %12.4f %12.4f\n",
              nm, r$macro_p, r$macro_r, r$macro_f1))
}

cat("\n F1 Scores Per class\n")
cat(sprintf("%-18s %10s %10s %10s\n", "Model", "Class 1", "Class 2", "Class 3"))
for (nm in names(results)) {
  r <- results[[nm]]
  cat(sprintf("%-18s %10.3f %10.3f %10.3f\n",
              nm, r$f1[1], r$f1[2], r$f1[3]))
}
