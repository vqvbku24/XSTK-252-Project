# 1. Load library
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))

install.packages(c("randomForest", "caret", "pROC", "ggplot2"), quiet = TRUE)

library(randomForest)
library(caret)
library(pROC)
library(ggplot2)

cat("Packages loaded instantly! Starting training...\n")

# 2. Load the datasets
train_df <- read.csv("/content/XSTK-252-Project/Preprocessed_data/train_1.csv")
test_df <- read.csv("/content/XSTK-252-Project/Preprocessed_data/test_1.csv")

# 3. CONVERT CLASS FIRST (Crucial Step)
# This changes 'class' from text/character to a factor so na.roughfix can handle it
train_df$class <- as.factor(train_df$class)
test_df$class <- as.factor(test_df$class)

# #4 Fix missing value
# train_df <- na.roughfix(train_df)
# test_df <- na.roughfix(test_df)

# 5. Train the Random Forest Model
set.seed(123)
rf_model <- randomForest(
  class ~ .,
  data = train_df,
  ntree = 500,
  importance = TRUE,
  classwt = c("ad." = 1, "nonad." = 1)
)

# 6. Evaluation
print(rf_model)
predictions <- predict(rf_model, newdata = test_df)
conf_results <- confusionMatrix(predictions, test_df$class)
print(conf_results)

# 7. Visualization
varImpPlot(rf_model, n.var = 20, main = "Top 20 Important Features")
