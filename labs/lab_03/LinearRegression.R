# ==============================================================
# PHẦN 1: THIẾT LẬP MÔI TRƯỜNG & KHÁM PHÁ DỮ LIỆU LONGLEY
# ==============================================================

# Thay đổi ngôn ngữ hệ thống sang tiếng Anh để tránh lỗi encoding
# khi hiển thị output (đặc biệt quan trọng trên Windows)
Sys.setlocale("LC_ALL", "English")
Sys.setenv(LANGUAGE = "en")

# ------------------------------------------------------------------
# Dataset 'longley': Dữ liệu kinh tế vĩ mô Mỹ giai đoạn 1947-1962
# Gồm 7 biến: GNP.deflator, GNP, Unemployed, Armed.Forces,
#              Population, Year, Employed (biến mục tiêu)
# ------------------------------------------------------------------

# Xem thống kê mô tả: min, max, mean, median, Q1, Q3 cho từng biến
View(longley)
summary(longley)

# Kiểm tra cấu trúc dữ liệu: kiểu dữ liệu và số quan sát/biến
str(longley)


# ==============================================================
# PHẦN 2: PHÂN TÍCH TƯƠNG QUAN (LONGLEY)
# ==============================================================

# Tính ma trận tương quan Pearson giữa TẤT CẢ các cặp biến
# Giá trị gần 1 hoặc -1 → tương quan mạnh; gần 0 → tương quan yếu
cor(longley)

# Tính riêng hệ số tương quan giữa GNP và Employed
# Dùng để kiểm tra trước khi đưa vào mô hình hồi quy đơn
cor(longley$GNP, longley$Employed)

# --- Cài đặt package (chỉ chạy lần đầu) ---
install.packages("corrplot")
install.packages("ggcorrplot")

library(corrplot)
library(ggcorrplot)

# --- Tính ma trận tương quan ---
cor_matrix <- cor(longley)
print(round(cor_matrix, 3))

# Tương quan riêng giữa GNP và Employed
cor(longley$GNP, longley$Employed)


# ============================================================
# CÁCH 1: corrplot (nhanh, đẹp, phổ biến nhất)
# ============================================================
corrplot(cor_matrix,
         method  = "color",      # tô màu ô
         # type    = "under",      # chỉ hiện tam giác trên
         addCoef.col = "black",  # hiện số hệ số
         number.cex  = 0.8,      # cỡ chữ số
         tl.col  = "black",      # màu nhãn biến
         tl.srt  = 45,           # xoay nhãn 45 độ
         col     = COL2("RdBu"), # thang màu đỏ-xanh
         title   = "Ma trận tương quan - Longley",
         mar     = c(0,0,1,0))

# ============================================================
# CÁCH 2: ggcorrplot (tích hợp ggplot2, dễ tuỳ chỉnh)
# ============================================================
ggcorrplot(cor_matrix,
           method   = "square",   # hoặc "circle"
           type     = "upper",    # tam giác trên
           lab      = TRUE,       # hiện hệ số
           lab_size = 3,
           colors   = c("#C0392B", "white", "#2980B9"),
           title    = "Ma trận tương quan - Longley",
           ggtheme  = theme_minimal())


# ==============================================================
# PHẦN 3: HỒI QUY TUYẾN TÍNH ĐƠN BIẾN (Simple Linear Regression)
# ==============================================================

# Xây mô hình: GNP = β0 + β1 * Employed + ε
# lm(formula, data): hàm tuyến tính cơ bản trong R
# formula dạng: biến_phụ_thuộc ~ biến_độc_lập

model<-lm(GNP~Employed, data=longley)

# In tóm tắt ngắn gọn (chỉ hiện coefficients)
model

# In kết quả đầy đủ: R², p-value, F-statistic, Std. Error, t-value
# R² (Multiple R-squared): tỉ lệ phương sai GNP được giải thích bởi Employed
# p-value < 0.05 → mô hình có ý nghĩa thống kê
summary(model)

# Trích xuất toàn bộ vector hệ số hồi quy: Intercept (β0) và slope (β1)
model$coefficients

# Lấy riêng hệ số chặn β0 (Intercept): giá trị GNP khi Employed = 0
model$coefficients[[1]]

# Lấy riêng hệ số góc β1 (slope): GNP thay đổi bao nhiêu khi Employed tăng 1 đơn vị
model$coefficients[[2]]

# Xem lại data frame gốc được dùng để khớp mô hình (gồm biến Y và X)
model$model

# ==============================================================
# PHẦN 4: HỒI QUY TUYẾN TÍNH ĐA BIẾN (Multiple Linear Regression)
# ==============================================================

# Xây mô hình với 2 biến độc lập:
# GNP = β0 + β1 * Employed + β2 * Armed.Forces + ε
# Dùng dấu '+' để thêm nhiều biến độc lập vào mô hình
model1 <- lm(GNP ~ Employed + Armed.Forces, data = longley)

# Kiểm tra từng hệ số có ý nghĩa không (p-value từng biến)
# Adjusted R²: phiên bản R² đã điều chỉnh theo số biến (dùng để so sánh mô hình)
summary(model1)

# Trích xuất vector hệ số: β0, β1 (Employed), β2 (Armed.Forces)
model1$coefficients

# β0: Intercept (hằng số)
model1$coefficients[[1]]

# β1: Hệ số của biến Employed trong mô hình đa biến
model1$coefficients[[2]]



# Xây mô hình với tất cả biến độc lập:
# Dùng dấu '.' để lấy tất cả
model2 <- lm(GNP ~ ., data = longley)

# Kiểm tra từng hệ số có ý nghĩa không (p-value từng biến)
# Adjusted R²: phiên bản R² đã điều chỉnh theo số biến (dùng để so sánh mô hình)
summary(model2)

# Trích xuất vector hệ số: β0, β1 (Employed), β2 (Armed.Forces)
model2$coefficients

# β0: Intercept (hằng số)
model2$coefficients[[1]]

# β1: Hệ số của biến Employed trong mô hình đa biến
model2$coefficients[[2]]


# ==============================================================
# PHẦN 5: KHÁM PHÁ DỮ LIỆU BOSTON (Dataset từ package MASS)
# ==============================================================

library(MASS)   # Package chứa nhiều dataset thống kê kinh điển
data(Boston)    # Tải dataset Boston Housing (giá nhà Boston, 506 quan sát, 14 biến)
# Biến mục tiêu: medv (median value of homes, đơn vị $1000)

# Kiểm tra kiểu dữ liệu và cấu trúc các cột
str(Boston)

# Thống kê mô tả toàn bộ dataset
summary(Boston)

# ==============================================================
# PHẦN 6: PHÂN TÍCH TƯƠNG QUAN (BOSTON)
# ==============================================================

# Vẽ ma trận scatter plot (pairs plot) cho tất cả cặp biến
# Giúp nhìn nhanh quan hệ tuyến tính/phi tuyến giữa các biến
pairs(Boston)

# Tính ma trận tương quan cho toàn bộ Boston dataset
correlation <- cor(Boston)

# In hàng tương quan của biến mục tiêu 'medv' với tất cả biến còn lại
# → Xác định biến nào tương quan mạnh nhất với giá nhà
print(correlation["medv", ])

# ==============================================================
# PHẦN 7: TRỰC QUAN HÓA VÀ MÔ HÌNH (BOSTON)
# ==============================================================

# Vẽ scatter plot giữa:
#   rm   (x): số phòng ngủ trung bình mỗi căn nhà
#   medv (y): giá trị nhà trung bình (nghìn USD)
# abline(): vẽ đường hồi quy tuyến tính lên biểu đồ (màu đỏ)
# Lưu ý: abline() phải đặt SAU plot() và TRONG cùng lệnh plot()
plot(Boston$rm, Boston$medv,
     xlab = "So phong trung binh (rm)",
     ylab = "Gia nha trung binh (medv)",
     main = "Moi quan he giua rm va medv",
     abline(lm(medv ~ rm, data = Boston), col = "red"))
