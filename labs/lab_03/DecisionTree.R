# ============================================================
# CÀI ĐẶT & LOAD PACKAGES
# ============================================================
install.packages(c("rpart", "rpart.plot", "caret", "pROC", "tidyverse", "vip"))

library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(vip)

# ============================================================
# PHẦN 1: TITANIC — DECISION TREE
# ============================================================
titanic <- read.csv("D:/Github/R_Intro/labs/dataset/titanic.csv", stringsAsFactors = TRUE)

titanic_clean <- titanic %>%
  select(survived, pclass, sex, age, sibsp, parch, fare) %>%
  mutate(survived = factor(survived, levels = c(0, 1),
                           labels = c("No", "Yes"))) %>%
  drop_na()

set.seed(42)
train_idx <- createDataPartition(titanic_clean$survived, p = 0.8, list = FALSE)
tit_train <- titanic_clean[train_idx, ]
tit_test  <- titanic_clean[-train_idx, ]

cat("Train:", nrow(tit_train), "| Test:", nrow(tit_test), "\n")
prop.table(table(tit_train$survived))

# --- Cây mặc định ---
set.seed(42)
tree_default <- rpart(survived ~ ., data = tit_train, method = "class")

rpart.plot(tree_default, type = 0, extra = 104, under = TRUE,
           fallen.leaves = TRUE, main = "Decision Tree — Titanic (Mặc định)")

print(tree_default)
printcp(tree_default)
plotcp(tree_default)

# Dự đoán
pred_class <- predict(tree_default, newdata = tit_test, type = "class")
pred_prob  <- predict(tree_default, newdata = tit_test, type = "prob")[, "Yes"]

cm <- confusionMatrix(pred_class, tit_test$survived, positive = "Yes")
print(cm)

roc_tree <- roc(tit_test$survived, pred_prob, levels = c("No", "Yes"), quiet = TRUE)
cat("AUC (Decision Tree mặc định):", round(auc(roc_tree), 4), "\n")

# --- Bias-Variance Tradeoff ---
depths    <- 1:10
acc_train <- numeric(length(depths))
acc_test  <- numeric(length(depths))

for (i in seq_along(depths)) {
  tree_d <- rpart(survived ~ ., data = tit_train, method = "class",
                  control = rpart.control(maxdepth = depths[i], cp = 0))
  acc_train[i] <- mean(predict(tree_d, tit_train, type = "class") == tit_train$survived)
  acc_test[i]  <- mean(predict(tree_d, tit_test,  type = "class") == tit_test$survived)
}

data.frame(depth = depths, Train = acc_train, Test = acc_test) %>%
  pivot_longer(-depth, names_to = "Set", values_to = "Accuracy") %>%
  ggplot(aes(x = depth, y = Accuracy, color = Set, linetype = Set)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_color_manual(values = c("Train" = "steelblue", "Test" = "tomato")) +
  labs(title = "Bias–Variance Tradeoff — Decision Tree (Titanic)",
       subtitle = "Train Accuracy vs Test Accuracy theo độ sâu cây",
       x = "Độ sâu cây (maxdepth)", y = "Accuracy") +
  theme_minimal()

# --- Pruning ---
set.seed(42)
tree_full <- rpart(survived ~ ., data = tit_train, method = "class",
                   control = rpart.control(cp = 0, minsplit = 2))

cp_table <- as.data.frame(tree_full$cptable)
print(cp_table)
plotcp(tree_full, main = "Cross-Validation Error theo Complexity Parameter (cp)")

best_cp <- cp_table$CP[which.min(cp_table$xerror)]
cat("cp tối ưu:", round(best_cp, 6), "\n")

tree_pruned <- prune(tree_full, cp = best_cp)
rpart.plot(tree_pruned, type = 4, extra = 104, fallen.leaves = TRUE,
           main = paste("Cây sau Pruning (cp =", round(best_cp, 4), ")"))

acc_full   <- mean(predict(tree_full,   tit_test, type = "class") == tit_test$survived)
acc_pruned <- mean(predict(tree_pruned, tit_test, type = "class") == tit_test$survived)

cat(sprintf("Accuracy — Cây đầy đủ:  %.4f\n", acc_full))
cat(sprintf("Accuracy — Sau pruning: %.4f\n", acc_pruned))

# ============================================================
# PHẦN 2: GERMAN CREDIT — SO SÁNH LR vs DECISION TREE
# ============================================================
german <- read.csv("D:/Github/R_Intro/labs/dataset/german1.csv")
german$target <- factor(german$target, levels = c(1, 2),
                        labels = c("Good", "Bad"))

# Dùng createDataPartition thay initial_split
set.seed(42)
g_idx   <- createDataPartition(german$target, p = 0.8, list = FALSE)
g_train <- german[g_idx, ]
g_test  <- german[-g_idx, ]

cat("German — Train:", nrow(g_train), "| Test:", nrow(g_test), "\n")

ctrl_compare <- trainControl(
  method = "cv", number = 10,
  classProbs = TRUE, summaryFunction = twoClassSummary
)

# Logistic Regression
set.seed(42)
m_lr <- train(target ~ ., data = g_train, method = "glm",
              family = "binomial", trControl = ctrl_compare, metric = "ROC")

# Decision Tree (tuning cp)
set.seed(42)
m_tree <- train(target ~ ., data = g_train, method = "rpart",
                trControl = ctrl_compare, metric = "ROC",
                tuneGrid = expand.grid(cp = c(0, 0.001, 0.005, 0.01, 0.02, 0.05)))

# So sánh CV
results <- resamples(list(
  LogisticRegression = m_lr,
  DecisionTree       = m_tree
))

summary(results, metric = "ROC")
dotplot(results, metric = "ROC",
        main = "So sánh AUC (10-Fold CV) — German Credit")

# Đánh giá trên test set
get_auc <- function(model, test, truth) {
  p       <- predict(model, newdata = test, type = "prob")[, "Bad"]
  roc_obj <- roc(test[[truth]], p, levels = c("Good", "Bad"), quiet = TRUE)
  round(auc(roc_obj), 4)
}

cat("AUC trên TEST SET:\n")
cat("  Logistic Regression:", get_auc(m_lr,   g_test, "target"), "\n")
cat("  Decision Tree:      ", get_auc(m_tree, g_test, "target"), "\n")
