# BÀI THỰC HÀNH: PHÂN TÍCH CHẤT LƯỢNG NƯỚC BẰNG HỒI QUY LOGISTIC
# ==========================================================

# TÓM TẮT BÀI TOÁN:
# Bài thực hành này sử dụng bộ dữ liệu water_potability để dự đoán liệu một mẫu nước có uống được hay không
# dựa trên các chỉ số hóa học và vật lý khác nhau. Chúng ta sẽ sử dụng mô hình hồi quy logistic để
# xây dựng bộ phân loại nhị phân, đánh giá hiệu suất của mô hình và trực quan hóa kết quả.
# Biến mục tiêu: Potability (0 = không uống được, 1 = uống được)

library(readr)
water_potability <- read_csv("graphics - water quality/water_potability.csv")
View(water_potability)