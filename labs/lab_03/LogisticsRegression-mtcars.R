# Bài thực hành Hồi quy Logistic trong R

# BÀI THỰC HÀNH: XÂY DỰNG MÔ HÌNH HỒI QUY LOGISTIC
# =================================================

# TÓM TẮT BÀI TOÁN:
# Bài thực hành này sử dụng bộ dữ liệu mtcars để dự đoán loại hộp số của xe (tự động hay số sàn)
# dựa trên các đặc tính kỹ thuật khác của xe. Chúng ta sẽ sử dụng mô hình hồi quy logistic để 
# xây dựng bộ phân loại nhị phân, đánh giá hiệu suất của mô hình và trực quan hóa kết quả.
# Biến mục tiêu: am (0 = hộp số tự động, 1 = hộp số sàn)


# Bước 1: cài đặt các gối cần thiết
if(!require(caret)) install.packages("caret")
if(!require(pROC)) install.packages("pROC")

library(caret) # Để chia dữ liệu, đánh giá mô hình
library(pROC) # Để vẽ đường cong ROC

# Bước 2: Tìm hiểu/ khai phá dữ liệu
data("mtcars") # Tải bộ dữ liệu mtcars
View(mtcars)
dataset_name<- "Motor Trend Car Road Test (mtcars)"

# Chuyển đổi biến mục tiêu thành factor để phù hợp với bài toán loại
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am)<-c("Automatic", "Manual") # Đặt tên cho các mức

# Thông tin cơ bản về bộ dữ liệu
cat("Thông tin về tập dữ liệu", dataset_name, ":\n")
cat("Số mẫu", nrow(mtcars), "\n")
cat("Số đặc trưng:", ncol(mtcars)-1, "\n\n")

# Hiển thị cấu trúc dữ liệu
cat("Cấu trúc dữ liệu:\n")
str(mtcars)


# Hiển thị các dòng đầu tiên
print(head(mtcars))

# Thống kê dữ liệu
print(summary(mtcars))

# Kiểm tra phân phối biến mục tiêu
print("\n Phân phối biến mục tiêu (loại hộp số\n:")
print(table(mtcars$am))
print(prop.table(table(mtcars$am))*100)

# Bước 3: Phân chia dữ liệu thành tập huấn luyện và tập kiểm tra
set.seed(42)
train_indices <- createDataPartition(mtcars$am, p=0.7, list = FALSE)
train_data <-mtcars[train_indices, ]
test_data<-mtcars[-train_indices, ]

cat("\n Kích thước tập huấn luyện", dim(train_data))
cat("\n Kích thước tập test", dim(test_data))

# Bước 4: Xây dựng và huấn luyện mô hình hồi quy logistic
# Chúng ta chọn các biến có liên quan đến hiệu suất
model<-glm(am~wt,
           data = train_data,
           family = binomial(link="logit"))

# Bước 5: Kiểm tra
cat("\n Tóm tắt mô hình:\n")
summary_model <- summary(model)
print(summary_model)

# Tính giá trị thang logit cho dự đoán
# GIá trị càng lớn thì có khả năng là số sàn càng cao
cat("\n Hệ số hồi quy:\n")
coef_df <-data.frame(
  feature = names(coef(model)),
  coefficients=coef(model),
  odds_ratio = exp(coef(model)),
  p_value = summary_model$coefficients[,4]
)
print(coef_df)

# Bước 6: Dự đoán trên tập kiểm tra
# Dự đoán xác suất
probabilities <- predict(model, newdata=test_data, type="response")
probabilities

# Đặt ngưỡng 0.5 để phân loại
predicted_classed <- ifelse(probabilities>0.5, "Manual", "Automatic")
predicted_classed <- factor(predicted_classed, levels=c("Automatic", "Manual"))

predicted_classed

# Bước 7: Đánh giá mô hình
# Tạo ma trận nhầm lẫn
conf_matrix<-confusionMatrix(predicted_classed, test_data$am)
print(conf_matrix$table)

# Các chỉ số đánh giá
cat("\nCác chỉ số đánh giá mô hình:\n")
cat("Độ chính xác (Accuracy):", round(conf_matrix$overall["Accuracy"], 4), "\n")
cat("Độ nhạy (Sensitivity/Recall):", round(conf_matrix$byClass["Sensitivity"], 4), "\n")
cat("Độ đặc hiệu (Specificity):", round(conf_matrix$byClass["Specificity"], 4), "\n")
cat("Độ chính xác dương tính (Precision):", round(conf_matrix$byClass["Pos Pred Value"], 4), "\n")
cat("Chỉ số F1:", round(conf_matrix$byClass["F1"], 4), "\n")

# Vẽ đường cong ROC
roc_obj <- roc(test_data$am, probabilities)
# Tính diện tích dưới đường cong ROC
auc_value <- auc(roc_obj)  # Đảm bảo tên biến là auc_value (chữ thường)
cat("Diện tích dưới đường cong ROC (AUC):", round(auc_value, 4), "\n")

plot(roc_obj, main="Đường cong ROC", col="blue", lwd=2)

