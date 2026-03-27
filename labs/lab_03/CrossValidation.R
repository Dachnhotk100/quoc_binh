# Cài đặt nếu chưa có
install.packages(c("tidyverse", "caret", "pROC", "rsample", "yardstick"))

library(tidyverse)
library(caret)
library(pROC)

medical <- read.csv("D:/Github/R_Intro/labs/dataset/medical_care.csv", stringsAsFactors = TRUE)
dim(medical)
head(medical)

# Kiểm tra phân phối biến mục tiêu
table(medical$UCURNINS)
prop.table(table(medical$UCURNINS))

# Holdout — Train/Test Split
set.seed(42)

# Tạo chỉ số cho tập training (70%)
train_idx <- createDataPartition(medical$UCURNINS, p = 0.7, list = FALSE)

train_data <- medical[train_idx, ]
test_data  <- medical[-train_idx, ]

cat("Kích thước tập train:", nrow(train_data), "\n")
cat("Kích thước tập test: ", nrow(test_data),  "\n")

# Kiểm tra tỷ lệ lớp trong từng tập
prop.table(table(train_data$UCURNINS))
prop.table(table(test_data$UCURNINS))

# Huấn luyện mô hình
model_lr <- glm(UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + FHOSP +
                  FDENT + FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT +
                  U_WKSLY + U_USHRS + UBRACE + GENDER + UEDUC3,
                data  = train_data,
                family = binomial(link = "logit"))

# Dự đoán xác suất trên tập test
pred_prob <- predict(model_lr, newdata = test_data, type = "response")

# Tính AUC
roc_obj  <- roc(test_data$UCURNINS, pred_prob, levels = c("No", "Yes"))
cat("AUC (Holdout):", round(auc(roc_obj), 4), "\n")

set.seed(NULL)  # Bỏ seed cố định để thấy sự ngẫu nhiên

auc_scores <- numeric(20)

for (i in 1:20) {
  idx   <- createDataPartition(medical$UCURNINS, p = 0.7, list = FALSE)
  train <- medical[idx, ]
  test  <- medical[-idx, ]
  
  m   <- glm(UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + FHOSP +
               FDENT + FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT +
               U_WKSLY + U_USHRS + UBRACE + GENDER + UEDUC3,
             data = train, family = binomial())
  
  p   <- predict(m, newdata = test, type = "response")
  roc_i <- roc(test$UCURNINS, p, levels = c("No", "Yes"), quiet = TRUE)
  auc_scores[i] <- auc(roc_i)
}

cat("AUC min:", round(min(auc_scores),  4), "\n")
cat("AUC max:", round(max(auc_scores),  4), "\n")
cat("AUC sd: ", round(sd(auc_scores),   4), "\n")

# Trực quan hóa
hist(auc_scores, main = "Phân phối AUC — Holdout (20 lần)",
     xlab = "AUC", col = "steelblue", border = "white")


set.seed(NULL)  # Bỏ seed cố định để thấy sự ngẫu nhiên

auc_scores <- numeric(20)

for (i in 1:20) {
  idx   <- createDataPartition(medical$UCURNINS, p = 0.7, list = FALSE)
  train <- medical[idx, ]
  test  <- medical[-idx, ]

  m   <- glm(UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + FHOSP +
               FDENT + FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT +
               U_WKSLY + U_USHRS + UBRACE + GENDER + UEDUC3,
             data = train, family = binomial())

  p   <- predict(m, newdata = test, type = "response")
  roc_i <- roc(test$UCURNINS, p, levels = c("No", "Yes"), quiet = TRUE)
  auc_scores[i] <- auc(roc_i)
}

cat("AUC min:", round(min(auc_scores),  4), "\n")
cat("AUC max:", round(max(auc_scores),  4), "\n")
cat("AUC sd: ", round(sd(auc_scores),   4), "\n")

# Trực quan hóa
hist(auc_scores, main = "Phân phối AUC — Holdout (20 lần)",
     xlab = "AUC", col = "steelblue", border = "white")


# K-Fold Cross-Validation
# Cấu hình 10-Fold CV
ctrl_10fold <- trainControl(
  method          = "cv",
  number          = 10,
  classProbs      = TRUE,          # Cần thiết để tính AUC
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

set.seed(123)
model_cv <- train(
  UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + FHOSP +
    FDENT + FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT +
    U_WKSLY + U_USHRS + UBRACE + GENDER + UEDUC3,
  data      = medical,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl_10fold,
  metric    = "ROC"
)

# Xem kết quả CV
print(model_cv)
cat("AUC trung bình (10-Fold CV):", round(model_cv$results$ROC, 4), "\n")

library(pROC)

set.seed(456)
k    <- 10
folds <- createFolds(medical$UCURNINS, k = k, list = TRUE)

auc_train <- numeric(k)
auc_val   <- numeric(k)

for (i in seq_len(k)) {
  val_idx  <- folds[[i]]
  train_cv <- medical[-val_idx, ]
  val_cv   <- medical[val_idx, ]
  
  m <- glm(UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + FHOSP +
             FDENT + FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT +
             U_WKSLY + U_USHRS + UBRACE + GENDER + UEDUC3,
           data = train_cv, family = binomial())
  
  p_train <- predict(m, newdata = train_cv, type = "response")
  p_val   <- predict(m, newdata = val_cv,   type = "response")
  
  auc_train[i] <- auc(roc(train_cv$UCURNINS, p_train,
                          levels = c("No","Yes"), quiet = TRUE))
  auc_val[i]   <- auc(roc(val_cv$UCURNINS,   p_val,
                          levels = c("No","Yes"), quiet = TRUE))
  
  cat(sprintf("Fold %2d | Train AUC: %.4f | Val AUC: %.4f\n",
              i, auc_train[i], auc_val[i]))
}

cat("\n--- Tổng kết ---\n")
cat("Train AUC trung bình:", round(mean(auc_train), 4), "\n")
cat("Val   AUC trung bình:", round(mean(auc_val),   4), "\n")
cat("Chênh lệch (overfit?):", round(mean(auc_train) - mean(auc_val), 4), "\n")

# 5.4. Repeated K-Fold Cross-Validation
set.seed(NULL)

results_comparison <- data.frame()

for (run in 1:10) {
  for (k_val in c(5, 10)) {
    folds_k  <- createFolds(medical$UCURNINS, k = k_val, list = TRUE)
    auc_k    <- numeric(k_val)
    
    for (i in seq_len(k_val)) {
      val_idx  <- folds_k[[i]]
      train_k  <- medical[-val_idx, ]
      val_k    <- medical[val_idx,  ]
      
      m <- glm(UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + FHOSP +
                 FDENT + FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT +
                 U_WKSLY + U_USHRS + UBRACE + GENDER + UEDUC3,
               data = train_k, family = binomial())
      
      p     <- predict(m, newdata = val_k, type = "response")
      auc_k[i] <- auc(roc(val_k$UCURNINS, p,
                          levels = c("No","Yes"), quiet = TRUE))
    }
    
    results_comparison <- rbind(results_comparison,
                                data.frame(run = run, k = paste0(k_val, "-Fold"),
                                           auc = mean(auc_k)))
  }
}

# Tổng kết theo từng loại K-Fold
results_comparison %>%
  group_by(k) %>%
  summarise(
    mean_auc = round(mean(auc), 4),
    sd_auc   = round(sd(auc),   4),
    .groups  = "drop"
  )

print(results_comparison)
