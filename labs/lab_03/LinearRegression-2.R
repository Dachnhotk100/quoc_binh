# ==============================================================
# PHẦN 8: HỒI QUY TUYẾN TÍNH ĐA BIẾN ĐẦY ĐỦ VỚI ĐÁNH GIÁ MÔ HÌNH
# (Sử dụng Boston Housing Dataset - Train/Test Split)
# ==============================================================

# Cài đặt các gói nếu chưa có, sau đó tải vào phiên làm việc
# require() trả về FALSE nếu gói chưa được cài → kích hoạt install.packages()
if (!require(MASS))  install.packages("MASS")   # Dataset Boston, các hàm thống kê
if (!require(caret)) install.packages("caret")  # Công cụ ML: chia dữ liệu, cross-validation

library(MASS)    # Cung cấp bộ dữ liệu Boston (506 quan sát, 14 biến)
library(caret)   # Cung cấp createDataPartition() để chia train/test có phân tầng

# ------------------------------------------------------------------
# BƯỚC 1: TẢI VÀ CHUẨN BỊ DỮ LIỆU
# ------------------------------------------------------------------

data(Boston)

# Tách ma trận đặc trưng X (13 biến dự báo) và vector mục tiêu y
# Các biến X gồm: crim, zn, indus, chas, nox, rm, age, dis, rad, tax,
#                 ptratio, black, lstat
X <- Boston[, 1:13]   # Tất cả cột trừ cột cuối (medv)
y <- Boston$medv      # medv: Median value of owner-occupied homes ($1000s)

dataset_name <- "Boston housing"

# ------------------------------------------------------------------
# BƯỚC 2: KHÁM PHÁ DỮ LIỆU (Exploratory Data Analysis)
# ------------------------------------------------------------------

# In thông tin tổng quan về tập dữ liệu
cat("Thông tin về tập dữ liệu", dataset_name, ":\n")
cat("Số mẫu:", nrow(X), "\n")       # Số quan sát (hàng)
cat("Số đặc trưng:", ncol(X), "\n\n") # Số biến độc lập (cột)

# Xem 6 hàng đầu để kiểm tra dữ liệu có vẻ hợp lý không
cat("Năm hàng đầu tiên của dữ liệu:\n")
print(head(X))

# Thống kê mô tả: phát hiện outlier, missing values, phân phối lệch
cat("\nThống kê mô tả về dữ liệu:\n")
print(summary(X))

# ------------------------------------------------------------------
# BƯỚC 3: PHÂN CHIA DỮ LIỆU TRAIN / TEST (80% - 20%)
# ------------------------------------------------------------------

# Đặt seed để kết quả có thể tái lập (reproducibility)
# Bất kỳ con số nào đều được; 42 là quy ước phổ biến
set.seed(42)

# createDataPartition(): chia có phân tầng (stratified) theo y
# p = 0.8 → 80% dùng để huấn luyện, 20% để kiểm tra
# list = FALSE → trả về vector chỉ số (index), không phải list
train_indices <- createDataPartition(y, p = 0.8, list = FALSE)

# Chia X và y theo chỉ số train
X_train <- X[train_indices, ]    # Ma trận đặc trưng tập huấn luyện
X_test  <- X[-train_indices, ]   # Ma trận đặc trưng tập kiểm tra (dấu '-' = loại trừ)
y_train <- y[train_indices]      # Nhãn mục tiêu tập huấn luyện
y_test  <- y[-train_indices]     # Nhãn mục tiêu tập kiểm tra

# Kiểm tra kích thước sau khi chia: dim() trả về (số hàng, số cột)
cat("\nKích thước tập huấn luyện:", dim(X_train), "\n")
cat("Kích thước tập kiểm tra:",   dim(X_test),  "\n")

# ------------------------------------------------------------------
# BƯỚC 4-5: XÂY DỰNG VÀ HUẤN LUYỆN MÔ HÌNH HỒI QUY TUYẾN TÍNH
# ------------------------------------------------------------------

# Công thức 'y_train ~ .' nghĩa là:
#   y_train là biến phụ thuộc
#   dấu '.' = sử dụng TẤT CẢ các cột còn lại trong data = X_train làm biến độc lập
# → Mô hình: medv = β0 + β1*crim + β2*zn + ... + β13*lstat + ε
model <- lm(y_train ~ ., data = X_train)

# Kết quả summary gồm:
#   - Coefficients: hệ số, Std. Error, t-value, p-value từng biến
#   - Multiple R²: mức độ giải thích phương sai của mô hình trên tập train
#   - F-statistic: kiểm định toàn bộ mô hình có ý nghĩa không
summary(model)

# ------------------------------------------------------------------
# BƯỚC 6: PHÂN TÍCH HỆ SỐ HỒI QUY
# ------------------------------------------------------------------

# coef(model): vector hệ số gồm Intercept + 13 biến
# [-1]: bỏ Intercept, chỉ lấy hệ số của các biến độc lập
coef_df <- data.frame(
  feature     = names(coef(model)[-1]),   # Tên biến
  coefficient = coef(model)[-1]           # Giá trị hệ số tương ứng
)

# Sắp xếp giảm dần theo giá trị hệ số
# Hệ số dương lớn → biến tác động mạnh chiều tăng giá nhà
# Hệ số âm lớn   → biến tác động mạnh chiều giảm giá nhà
coef_df <- coef_df[order(coef_df$coefficient, decreasing = TRUE), ]

cat("\nHệ số hồi quy (sắp xếp theo độ lớn):\n")
print(coef_df)

# In riêng Intercept β0: giá trị medv dự đoán khi tất cả biến X = 0
cat("Hệ số chặn (Intercept):", coef(model)[1], "\n")

# ------------------------------------------------------------------
# BƯỚC 7: DỰ ĐOÁN TRÊN TẬP KIỂM TRA
# ------------------------------------------------------------------

# predict() áp dụng mô hình đã huấn luyện lên dữ liệu mới (X_test)
# newdata phải có đúng tên cột như dữ liệu train
y_pred <- predict(model, newdata = X_test)

# ------------------------------------------------------------------
# BƯỚC 8: ĐÁNH GIÁ MÔ HÌNH BẰNG CÁC CHỈ SỐ HỒI QUY
# ------------------------------------------------------------------

# MSE (Mean Squared Error): trung bình bình phương sai số
# Nhạy cảm với outlier; đơn vị là $1000² → khó diễn giải trực tiếp
mse <- mean((y_test - y_pred)^2)
mse

# RMSE (Root MSE): căn bậc hai của MSE
# Cùng đơn vị với y ($1000) → dễ diễn giải hơn MSE
rmse <- sqrt(mse)
rmse

# R² (Coefficient of Determination): tỉ lệ phương sai y_test được giải thích bởi mô hình
# R² = 1 → dự đoán hoàn hảo; R² = 0 → mô hình không tốt hơn dự đoán bằng mean(y)
# Công thức: R² = 1 - SS_res / SS_tot
r2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

cat("\nĐánh giá mô hình trên tập kiểm tra:\n")
cat("Mean Squared Error (MSE):",         round(mse,  4), "\n")
cat("Root Mean Squared Error (RMSE):",   round(rmse, 4), "\n")
cat("R-squared (R²):",                   round(r2,   4), "\n")

# ------------------------------------------------------------------
# BƯỚC 9: TRỰC QUAN HÓA KẾT QUẢ DỰ ĐOÁN
# ------------------------------------------------------------------

# Scatter plot: trục x = giá thực tế, trục y = giá dự đoán
# Nếu mô hình hoàn hảo → tất cả điểm nằm trên đường y = x
# pch = 16: điểm đặc hình tròn; col = "blue": màu xanh
plot(y_test, y_pred,
     main = "So sánh giữa giá trị thực tế và dự đoán",
     xlab = "Giá trị thực tế (medv)",
     ylab = "Giá trị dự đoán (medv)",
     pch  = 16,
     col  = "blue")

# Vẽ đường tham chiếu y = x (slope = 1, intercept = 0)
# lwd = 2: độ dày nét vẽ; lty = 2: nét đứt
# Điểm càng gần đường này → dự đoán càng chính xác
abline(0, 1, col = "red", lwd = 2, lty = 2)

# Thêm lưới để dễ đọc giá trị trên biểu đồ
grid()

