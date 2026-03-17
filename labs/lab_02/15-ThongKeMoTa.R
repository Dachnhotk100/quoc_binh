# ==============================================================================
# BÀI 11: THỐNG KÊ MÔ TẢ
# ==============================================================================
# Mục tiêu học tập:
# - Hiểu khái niệm và vai trò của thống kê mô tả
# - Tính và giải thích các số đo xu hướng trung tâm (mean, median, mode)
# - Tính và giải thích các số đo độ phân tán (variance, SD, range, IQR)
# - Tính và giải thích các số đo hình dạng (skewness, kurtosis)
# - Tạo và phân tích bảng tần số
# - Sử dụng R để tính toán và trực quan hóa thống kê mô tả
# - Áp dụng thống kê mô tả vào bài toán thực tế
# ==============================================================================

# ------------------------------------------------------------------------------
# 11.1 Thống kê mô tả là gì?
# ------------------------------------------------------------------------------

# 11.1.1 Định nghĩa
#
# Thống kê mô tả (Descriptive Statistics) là tập hợp các phương pháp để 
# TÓM TẮT, TỔ CHỨC và TRÌNH BÀY dữ liệu một cách có ý nghĩa.
#
# Mục đích:
# - TÓM TẮT dữ liệu thành các con số dễ hiểu
# - MÔ TẢ đặc điểm chính của tập dữ liệu
# - TRỰC QUAN HÓA dữ liệu bằng biểu đồ
# - SO SÁNH các nhóm dữ liệu khác nhau

# 11.1.2 Ví dụ minh họa

# Tình huống: Điểm thi của 10 sinh viên
scores <- c(85, 90, 78, 92, 88, 95, 80, 89, 91, 87)

# Thay vì nhìn cả 10 số, ta có thể tóm tắt:
cat("Điểm trung bình:", mean(scores), "\n")
cat("Điểm cao nhất:", max(scores), "\n")
cat("Điểm thấp nhất:", min(scores), "\n")
cat("Độ lệch chuẩn:", sd(scores), "\n")

# 11.1.3 Các loại thống kê mô tả
#
# Thống kê mô tả gồm 3 nhóm chính:
#
# 1. Số đo xu hướng trung tâm (Central Tendency)
#    - Trung bình (Mean)
#    - Trung vị (Median)
#    - Mode (Yếu vị)
#
# 2. Số đo độ phân tán (Dispersion/Spread)
#    - Phương sai (Variance)
#    - Độ lệch chuẩn (Standard Deviation)
#    - Khoảng biến thiên (Range)
#    - IQR (Interquartile Range)
#
# 3. Số đo hình dạng phân phối (Shape)
#    - Độ lệch (Skewness)
#    - Độ nhọn (Kurtosis)

# ------------------------------------------------------------------------------
# 11.2 Số đo xu hướng trung tâm
# ------------------------------------------------------------------------------

# 11.2.1 Trung bình (Mean)
#
# Định nghĩa: Trung bình là tổng các giá trị chia cho số lượng giá trị.
#
# Công thức:
# x̄ = (x₁ + x₂ + ... + xₙ) / n = Σxᵢ / n

# Ví dụ 1: Tính điểm trung bình
# Điểm: 8, 7, 9, 6, 8
# x̄ = (8 + 7 + 9 + 6 + 8) / 5 = 38 / 5 = 7.6

scores <- c(8, 7, 9, 6, 8)
mean_score <- mean(scores)
cat("Điểm trung bình:", mean_score, "\n")  # 7.6

# Ví dụ 2: Thu nhập trung bình
# Thu nhập (triệu): 10, 12, 15, 11, 13, 50
# x̄ = (10 + 12 + 15 + 11 + 13 + 50) / 6 = 18.5

income <- c(10, 12, 15, 11, 13, 50)
mean_income <- mean(income)
cat("Thu nhập TB:", mean_income, "triệu\n")  # 18.5

# Lưu ý: Trung bình NHẠY CẢM VỚI GIÁ TRỊ NGOẠI LỆ (outliers)
# Trong ví dụ trên, giá trị 50 làm trung bình cao hơn rất nhiều

# 11.2.2 Trung vị (Median)
#
# Định nghĩa: Trung vị là giá trị nằm chính giữa khi dữ liệu được sắp xếp.
#
# Cách tính:
# 1. Sắp xếp dữ liệu theo thứ tự tăng dần
# 2. Nếu n lẻ: Median = giá trị ở vị trí (n+1)/2
# 3. Nếu n chẵn: Median = trung bình của 2 giá trị giữa

# Ví dụ 1: n lẻ
# Dữ liệu: 8, 7, 9, 6, 8
# Sắp xếp: 6, 7, 8, 8, 9
# Median = 8 (vị trí thứ 3)

data <- c(8, 7, 9, 6, 8)
median_value <- median(data)
cat("Trung vị:", median_value, "\n")  # 8

# Ví dụ 2: n chẵn
# Dữ liệu: 10, 12, 15, 11, 13, 50
# Sắp xếp: 10, 11, 12, 13, 15, 50
# Median = (12 + 13) / 2 = 12.5

income <- c(10, 12, 15, 11, 13, 50)
median_income <- median(income)
cat("Trung vị:", median_income, "triệu\n")  # 12.5

# So sánh Mean vs Median
cat("\n=== SO SÁNH MEAN VS MEDIAN ===\n")
income <- c(10, 12, 15, 11, 13, 50)
cat("Mean:", mean(income), "\n")      # 18.5
cat("Median:", median(income), "\n")  # 12.5

cat("\nMedian không bị ảnh hưởng bởi outlier (50)\n")
cat("Mean bị kéo lên bởi outlier\n")

# Khi nào dùng Median?
# - Khi dữ liệu có OUTLIERS (giá trị cực đoan)
# - Thu nhập, giá nhà (thường có outliers)
# - Dữ liệu LỆCH (không đối xứng)

# 11.2.3 Mode (Yếu vị)
#
# Định nghĩa: Mode là giá trị xuất hiện NHIỀU NHẤT trong tập dữ liệu.

# R không có hàm mode sẵn, ta tự viết
get_mode <- function(x) {
  uniq_x <- unique(x)
  tab <- tabulate(match(x, uniq_x))
  uniq_x[which.max(tab)]
}

# Ví dụ 1: Mode duy nhất
# Điểm: 8, 7, 9, 8, 6, 8, 7
# Mode = 8 (xuất hiện 3 lần)

scores <- c(8, 7, 9, 8, 6, 8, 7)
mode_value <- get_mode(scores)
cat("Mode:", mode_value, "\n")  # 8

# Ví dụ 2: Kích cỡ áo bán chạy nhất
sizes <- c("M", "L", "M", "S", "M", "L", "M", "XL")
table(sizes)
# M xuất hiện nhiều nhất → Mode = M

# Khi nào dùng Mode?
# - Dữ liệu PHÂN LOẠI (categorical): màu sắc, kích cỡ áo
# - Tìm giá trị PHỔ BIẾN NHẤT

# 11.2.4 So sánh Mean, Median, Mode

cat("\n=== SO SÁNH 3 LOẠI ===\n")
set.seed(42)
data <- c(5, 6, 6, 7, 7, 7, 8, 8, 9, 25)  # 25 là outlier

cat("Mean:", mean(data), "\n")      # 8.8
cat("Median:", median(data), "\n")  # 7
cat("Mode:", get_mode(data), "\n")  # 7

cat("\nNhận xét:\n")
cat("- Mean cao nhất (bị ảnh hưởng bởi outlier 25)\n")
cat("- Median và Mode gần nhau hơn (đại diện tốt hơn)\n")

# ------------------------------------------------------------------------------
# 11.3 Số đo độ phân tán
# ------------------------------------------------------------------------------

# 11.3.1 Tại sao cần đo độ phân tán?
#
# Hai tập dữ liệu có thể có CÙNG TRUNG BÌNH nhưng ĐỘ PHÂN TÁN KHÁC NHAU.

# Ví dụ:
class_A <- c(70, 75, 80, 85, 90)
class_B <- c(50, 60, 80, 100, 110)

cat("=== VÍ DỤ ĐỘ PHÂN TÁN ===\n")
cat("Mean A:", mean(class_A), "\n")  # 80
cat("Mean B:", mean(class_B), "\n")  # 80

cat("SD A:", sd(class_A), "\n")      # 7.9
cat("SD B:", sd(class_B), "\n")      # 26.5

cat("\nCả hai lớp đều có điểm TB = 80, nhưng:\n")
cat("- Lớp A: Điểm TẬP TRUNG quanh 80\n")
cat("- Lớp B: Điểm PHÂN TÁN rộng\n")

# 11.3.2 Khoảng biến thiên (Range)
#
# Định nghĩa: Range là khoảng cách giữa giá trị LỚN NHẤT và NHỎ NHẤT.
#
# Công thức:
# Range = Max - Min

# Ví dụ:
scores <- c(78, 82, 90, 85, 88)

# Cách 1
range_value <- max(scores) - min(scores)
cat("Range:", range_value, "\n")  # 12

# Cách 2
range_values <- range(scores)
cat("Min:", range_values[1], ", Max:", range_values[2], "\n")
cat("Range:", diff(range_values), "\n")

# Ưu điểm: Dễ tính, dễ hiểu
# Nhược điểm: Chỉ dùng 2 giá trị (min, max), nhạy cảm với outliers

# 11.3.3 Phương sai (Variance)
#
# Định nghĩa: Phương sai đo MỨC ĐỘ PHÂN TÁN của dữ liệu quanh trung bình.
#
# Công thức (mẫu):
# s² = Σ(xᵢ - x̄)² / (n - 1)
#
# Giải thích:
# - (xᵢ - x̄): Độ lệch của mỗi giá trị so với trung bình
# - (xᵢ - x̄)²: Bình phương để tránh âm
# - Chia cho (n-1): Ước lượng không chệch cho tổng thể

# Ví dụ tính tay:
# Dữ liệu: 4, 6, 8
# Mean = (4 + 6 + 8) / 3 = 6
#
# (4 - 6)² = 4
# (6 - 6)² = 0
# (8 - 6)² = 4
#
# s² = (4 + 0 + 4) / (3 - 1) = 8 / 2 = 4

data <- c(4, 6, 8)
variance <- var(data)
cat("Phương sai:", variance, "\n")  # 4

# Tính tay để hiểu
mean_val <- mean(data)
squared_diff <- (data - mean_val)^2
variance_manual <- sum(squared_diff) / (length(data) - 1)
cat("Phương sai (tay):", variance_manual, "\n")  # 4

# Ý nghĩa:
# - Phương sai NHỎ: Dữ liệu tập trung gần trung bình
# - Phương sai LỚN: Dữ liệu phân tán rộng

# 11.3.4 Độ lệch chuẩn (Standard Deviation)
#
# Định nghĩa: Độ lệch chuẩn là CĂN BẬC HAI của phương sai.
#
# Công thức:
# s = √s²
#
# Tại sao dùng SD thay vì Variance?
# - SD có CÙNG ĐƠN VỊ với dữ liệu gốc
# - Dễ GIẢI THÍCH hơn

# Ví dụ:
scores <- c(85, 90, 78, 92, 88, 95, 80, 89, 91, 87)

variance <- var(scores)
sd_value <- sd(scores)

cat("Variance:", variance, "\n")  # 31.4
cat("SD:", sd_value, "\n")        # 5.6

# SD = sqrt(Variance)
cat("SD (tính từ var):", sqrt(variance), "\n")

# Quy tắc 68-95-99.7 (cho phân phối chuẩn):
# - 68% dữ liệu nằm trong ±1 SD
# - 95% dữ liệu nằm trong ±2 SD
# - 99.7% dữ liệu nằm trong ±3 SD

# Ví dụ minh họa
set.seed(42)
data <- rnorm(1000, mean = 100, sd = 15)  # IQ scores

mean_val <- mean(data)
sd_val <- sd(data)

# Đếm % trong các khoảng
within_1sd <- sum(data >= mean_val - sd_val & data <= mean_val + sd_val) / length(data) * 100
within_2sd <- sum(data >= mean_val - 2*sd_val & data <= mean_val + 2*sd_val) / length(data) * 100
within_3sd <- sum(data >= mean_val - 3*sd_val & data <= mean_val + 3*sd_val) / length(data) * 100

cat("\n=== QUY TẮC 68-95-99.7 ===\n")
cat("Trong ±1 SD:", round(within_1sd, 1), "%\n")  # ~68%
cat("Trong ±2 SD:", round(within_2sd, 1), "%\n")  # ~95%
cat("Trong ±3 SD:", round(within_3sd, 1), "%\n")  # ~99.7%

# 11.3.5 Hệ số biến thiên (Coefficient of Variation)
#
# Định nghĩa: CV là tỷ lệ giữa độ lệch chuẩn và trung bình, biểu thị bằng %.
#
# Công thức:
# CV = (s / x̄) × 100%
#
# Tại sao dùng CV?
# - So sánh độ phân tán của các tập dữ liệu có ĐƠN VỊ KHÁC NHAU
# - So sánh độ phân tán của các tập dữ liệu có TRUNG BÌNH KHÁC NHAU

# Ví dụ: So sánh độ biến động giữa cân nặng và chiều cao

# Cân nặng (kg)
weight <- c(60, 65, 70, 68, 72)
mean_w <- mean(weight)
sd_w <- sd(weight)
cv_w <- (sd_w / mean_w) * 100

cat("\n=== HỆ SỐ BIẾN THIÊN ===\n")
cat("Cân nặng - Mean:", mean_w, "kg, SD:", round(sd_w, 2), "\n")
cat("CV:", round(cv_w, 2), "%\n")

# Chiều cao (cm)
height <- c(165, 170, 175, 168, 172)
mean_h <- mean(height)
sd_h <- sd(height)
cv_h <- (sd_h / mean_h) * 100

cat("\nChiều cao - Mean:", mean_h, "cm, SD:", round(sd_h, 2), "\n")
cat("CV:", round(cv_h, 2), "%\n")

# So sánh
if (cv_w > cv_h) {
  cat("\nCân nặng biến động nhiều hơn chiều cao\n")
} else {
  cat("\nChiều cao biến động nhiều hơn cân nặng\n")
}

# Giải thích CV:
# - CV < 10%: Biến động THẤP
# - 10% ≤ CV ≤ 20%: Biến động TRUNG BÌNH
# - CV > 20%: Biến động CAO

# 11.3.6 Tứ phân vị (Quartiles) và IQR
#
# Tứ phân vị chia dữ liệu thành 4 phần bằng nhau:
# - Q1 (25%): 25% dữ liệu ≤ Q1
# - Q2 (50%): Trung vị
# - Q3 (75%): 75% dữ liệu ≤ Q3
#
# IQR (Interquartile Range):
# IQR = Q3 - Q1
#
# IQR chứa 50% dữ liệu giữa, không bị ảnh hưởng bởi outliers.

# Ví dụ:
scores <- c(50, 60, 70, 75, 80, 85, 90, 95, 100)

# Tính tứ phân vị
quartiles <- quantile(scores, probs = c(0.25, 0.5, 0.75))
cat("\n=== TỨ PHÂN VỊ ===\n")
cat("Q1 (25%):", quartiles[1], "\n")  # 67.5
cat("Q2 (50%):", quartiles[2], "\n")  # 80
cat("Q3 (75%):", quartiles[3], "\n")  # 91.25

# Tính IQR
iqr_value <- IQR(scores)
cat("IQR:", iqr_value, "\n")  # 23.75

# Hoặc
cat("IQR (tay):", quartiles[3] - quartiles[1], "\n")

# Phát hiện Outliers bằng IQR:
#
# Outliers là các giá trị:
# - < Q1 - 1.5 × IQR
# - > Q3 + 1.5 × IQR

scores <- c(50, 60, 70, 75, 80, 85, 90, 95, 100, 150)  # 150 là outlier?

Q1 <- quantile(scores, 0.25)
Q3 <- quantile(scores, 0.75)
IQR_val <- IQR(scores)

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

cat("\n=== PHÁT HIỆN OUTLIERS ===\n")
cat("Lower bound:", lower_bound, "\n")
cat("Upper bound:", upper_bound, "\n")

# Tìm outliers
outliers <- scores[scores < lower_bound | scores > upper_bound]
cat("Outliers:", outliers, "\n")

# ------------------------------------------------------------------------------
# 11.4 Số đo hình dạng phân phối
# ------------------------------------------------------------------------------

# 11.4.1 Độ lệch (Skewness)
#
# Định nghĩa: Skewness đo ĐỘ BẤT ĐỐI XỨNG của phân phối.
#
# Công thức:
# Skewness = E[(X - μ)³] / σ³
#
# Giải thích:
# - Skewness = 0: Phân phối ĐỐI XỨNG (symmetric)
# - Skewness > 0: Phân phối LỆCH PHẢI (right-skewed, positive skew)
#   - Đuôi dài bên phải
#   - Mean > Median
# - Skewness < 0: Phân phối LỆCH TRÁI (left-skewed, negative skew)
#   - Đuôi dài bên trái
#   - Mean < Median

# Cần cài đặt package moments
install.packages("moments")
library(moments)

# 1. Phân phối đối xứng
set.seed(42)
symmetric <- rnorm(1000, mean = 50, sd = 10)
skew_sym <- skewness(symmetric)
cat("\n=== SKEWNESS ===\n")
cat("Skewness (đối xứng):", round(skew_sym, 3), "\n")  # ≈ 0

# 2. Phân phối lệch phải
right_skewed <- c(rnorm(900, 50, 5), rnorm(100, 80, 5))
skew_right <- skewness(right_skewed)
cat("Skewness (lệch phải):", round(skew_right, 3), "\n")  # > 0

# 3. Phân phối lệch trái  
left_skewed <- c(rnorm(900, 50, 5), rnorm(100, 20, 5))
skew_left <- skewness(left_skewed)
cat("Skewness (lệch trái):", round(skew_left, 3), "\n")  # < 0

# Ví dụ thực tế:
# - Thu nhập: Lệch phải (ít người thu nhập rất cao)
# - Tuổi tử vong: Lệch trái (hầu hết sống đến tuổi già)
# - Chiều cao: Gần đối xứng

# 11.4.2 Độ nhọn (Kurtosis)
#
# Định nghĩa: Kurtosis đo ĐỘ NHỌN của phân phối, 
# hay mức độ tập trung ở đỉnh và đuôi.
#
# Công thức:
# Kurtosis = E[(X - μ)⁴] / σ⁴
#
# Giải thích:
# - Kurtosis = 3: Phân phối chuẩn (mesokurtic)
# - Kurtosis > 3: LEPTOKURTIC (nhọn hơn, đuôi dày hơn)
# - Kurtosis < 3: PLATYKURTIC (tù hơn, đuôi mỏng hơn)

# 1. Phân phối chuẩn
set.seed(42)
normal_data <- rnorm(1000, 50, 10)
kurt_normal <- kurtosis(normal_data)
cat("\n=== KURTOSIS ===\n")
cat("Kurtosis (chuẩn):", round(kurt_normal, 3), "\n")  # ≈ 3

# 2. Leptokurtic (nhọn hơn)
leptokurtic <- rt(1000, df = 3)  # t-distribution
kurt_lepto <- kurtosis(leptokurtic)
cat("Kurtosis (nhọn):", round(kurt_lepto, 3), "\n")  # > 3

# 3. Platykurtic (tù hơn)
platykurtic <- runif(1000, 0, 100)  # Uniform distribution
kurt_platy <- kurtosis(platykurtic)
cat("Kurtosis (tù):", round(kurt_platy, 3), "\n")  # < 3

# ------------------------------------------------------------------------------
# 11.5 Bảng tần số (Frequency Table)
# ------------------------------------------------------------------------------

# 11.5.1 Bảng tần số cho dữ liệu rời rạc

# Ví dụ: Số con trong các gia đình
children <- c(1, 2, 2, 3, 2, 1, 4, 2, 3, 2, 1, 2, 3, 2, 1, 2)

cat("\n=== BẢNG TẦN SỐ ===\n")

# Tạo bảng tần số
freq_table <- table(children)
print(freq_table)

# Tạo bảng tần suất (%)
prop_table <- prop.table(freq_table) * 100
cat("\nTần suất (%):\n")
print(round(prop_table, 2))

# Tạo data frame đẹp hơn
freq_df <- data.frame(
  So_con = as.numeric(names(freq_table)),
  Tan_so = as.vector(freq_table),
  Tan_suat = round(as.vector(prop_table), 2)
)
cat("\nBảng tần số chi tiết:\n")
print(freq_df)

# 11.5.2 Bảng tần số cho dữ liệu liên tục
#
# Với dữ liệu liên tục, ta cần CHIA THÀNH KHOẢNG (bins).

# Ví dụ: Chiều cao của 30 sinh viên (cm)
set.seed(42)
heights <- rnorm(30, mean = 165, sd = 8)

# Phương pháp 1: Dùng cut() để chia khoảng
breaks <- seq(140, 190, by = 10)
height_groups <- cut(heights, breaks = breaks, right = FALSE)

# Tạo bảng tần số
freq_table <- table(height_groups)
cat("\nBảng tần số chiều cao:\n")
print(freq_table)

# Tạo bảng đẹp
freq_df <- data.frame(
  Khoang = names(freq_table),
  Tan_so = as.vector(freq_table),
  Tan_suat = round(as.vector(freq_table) / sum(freq_table) * 100, 2)
)
print(freq_df)

# Phương pháp 2: Tự động chia khoảng
hist_data <- hist(heights, plot = FALSE)
cat("\nSố khoảng:", length(hist_data$breaks) - 1, "\n")
cat("Khoảng:\n")
print(hist_data$breaks)
cat("Tần số:\n")
print(hist_data$counts)

# 11.5.3 Bảng chéo (Cross-tabulation)

# Ví dụ: Khảo sát giữa giới tính và sở thích
set.seed(42)
gender <- sample(c("Nam", "Nữ"), 100, replace = TRUE)
preference <- sample(c("Bóng đá", "Bóng rổ", "Cầu lông"), 100, replace = TRUE)

# Tạo bảng chéo
cat("\n=== BẢNG CHÉO ===\n")
cross_tab <- table(gender, preference)
print(cross_tab)

# Thêm tổng hàng và cột
cross_tab_with_margin <- addmargins(cross_tab)
cat("\nBảng chéo có tổng:\n")
print(cross_tab_with_margin)

# Tính tỷ lệ theo hàng (%)
prop_by_row <- prop.table(cross_tab, margin = 1) * 100
cat("\nTỷ lệ theo hàng (%):\n")
print(round(prop_by_row, 2))

# Tính tỷ lệ theo cột (%)
prop_by_col <- prop.table(cross_tab, margin = 2) * 100
cat("\nTỷ lệ theo cột (%):\n")
print(round(prop_by_col, 2))

# ------------------------------------------------------------------------------
# 11.6 Tóm tắt thống kê với summary()
# ------------------------------------------------------------------------------

# 11.6.1 Sử dụng summary()

# Tạo dữ liệu mẫu
data(mtcars)

cat("\n=== SUMMARY() ===\n")

# Tóm tắt một biến
cat("Summary cho mpg:\n")
print(summary(mtcars$mpg))
# Min, Q1, Median, Mean, Q3, Max

# Tóm tắt toàn bộ data frame
cat("\nSummary toàn bộ mtcars:\n")
print(summary(mtcars))

# 11.6.2 Hàm describe() từ package psych

# install.packages("psych")
library(psych)

cat("\n=== DESCRIBE() ===\n")

# Mô tả chi tiết
cat("Describe cho mpg:\n")
print(describe(mtcars$mpg))
# n, mean, sd, median, trimmed, mad, min, max, range, skew, kurtosis, se

# Mô tả nhiều biến
cat("\nDescribe nhiều biến:\n")
print(describe(mtcars[, c("mpg", "hp", "wt")]))

# ------------------------------------------------------------------------------
# 11.7 Ví dụ tổng hợp
# ------------------------------------------------------------------------------

# Ví dụ 1: Phân tích điểm thi

cat("\n" , rep("=", 70), "\n", sep = "")
cat("VÍ DỤ 1: PHÂN TÍCH ĐIỂM THI\n")
cat(rep("=", 70), "\n", sep = "")

# Tạo dữ liệu điểm thi của 50 sinh viên
set.seed(123)
scores <- round(rnorm(50, mean = 75, sd = 12))
scores <- pmax(pmin(scores, 100), 0)  # Giới hạn 0-100

# 1. Số đo xu hướng trung tâm
cat("\n1. XU HƯỚNG TRUNG TÂM\n")
cat("Trung bình:", round(mean(scores), 2), "\n")
cat("Trung vị:", median(scores), "\n")
cat("Mode:", get_mode(scores), "\n")

# 2. Số đo độ phân tán
cat("\n2. ĐỘ PHÂN TÁN\n")
cat("Range:", max(scores) - min(scores), "\n")
cat("Variance:", round(var(scores), 2), "\n")
cat("SD:", round(sd(scores), 2), "\n")
cat("IQR:", IQR(scores), "\n")
cat("CV:", round(sd(scores)/mean(scores)*100, 2), "%\n")

# 3. Số đo hình dạng
cat("\n3. HÌNH DẠNG\n")
cat("Skewness:", round(skewness(scores), 3), "\n")
cat("Kurtosis:", round(kurtosis(scores), 3), "\n")

# 4. Tứ phân vị
cat("\n4. TỨ PHÂN VỊ\n")
quartiles <- quantile(scores, probs = c(0, 0.25, 0.5, 0.75, 1))
print(quartiles)

# 5. Bảng tần số
cat("\n5. BẢNG TẦN SỐ\n")
score_groups <- cut(scores, breaks = c(0, 50, 60, 70, 80, 90, 100),
                    labels = c("0-50", "50-60", "60-70", "70-80", "80-90", "90-100"),
                    right = FALSE)
freq_table <- table(score_groups)
print(freq_table)

# 6. Vẽ histogram
hist(scores, breaks = 10,
     main = "Phân phối điểm thi",
     xlab = "Điểm",
     ylab = "Tần số",
     col = "lightblue")
abline(v = mean(scores), col = "red", lwd = 2, lty = 2)
abline(v = median(scores), col = "blue", lwd = 2, lty = 2)
legend("topright", 
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = 2, lwd = 2)

# Ví dụ 2: So sánh 2 nhóm

cat("\n" , rep("=", 70), "\n", sep = "")
cat("VÍ DỤ 2: SO SÁNH 2 LỚP\n")
cat(rep("=", 70), "\n", sep = "")

# Điểm của 2 lớp
set.seed(42)
class_A <- round(rnorm(30, mean = 75, sd = 8))
class_B <- round(rnorm(30, mean = 78, sd = 15))

# Tạo data frame
comparison <- data.frame(
  Metric = c("N", "Mean", "Median", "SD", "Min", "Max", "Range", "IQR"),
  Class_A = c(
    length(class_A),
    round(mean(class_A), 2),
    median(class_A),
    round(sd(class_A), 2),
    min(class_A),
    max(class_A),
    max(class_A) - min(class_A),
    IQR(class_A)
  ),
  Class_B = c(
    length(class_B),
    round(mean(class_B), 2),
    median(class_B),
    round(sd(class_B), 2),
    min(class_B),
    max(class_B),
    max(class_B) - min(class_B),
    IQR(class_B)
  )
)

print(comparison)

# Vẽ boxplot so sánh
boxplot(class_A, class_B,
        names = c("Lớp A", "Lớp B"),
        main = "So sánh điểm 2 lớp",
        ylab = "Điểm",
        col = c("lightblue", "lightcoral"))

# Nhận xét
cat("\nNHẬN XÉT:\n")
cat("- Lớp B có điểm TB cao hơn\n")
cat("- Lớp A có độ phân tán thấp hơn (đồng đều hơn)\n")
cat("- Lớp B có khoảng điểm rộng hơn\n")

# ==============================================================================
# BÀI TẬP THỰC HÀNH
# ==============================================================================

cat("\n" , rep("=", 70), "\n", sep = "")
cat("BÀI TẬP THỰC HÀNH\n")
cat(rep("=", 70), "\n", sep = "")

# ------------------------------------------------------------------------------
# Bài tập 1: Thống kê cơ bản
# ------------------------------------------------------------------------------

cat("\nBÀI TẬP 1: THỐNG KÊ CƠ BẢN\n")
cat("Cho điểm thi của 15 sinh viên:\n")
cat("78, 85, 92, 88, 76, 90, 84, 88, 79, 91, 87, 83, 86, 89, 88\n\n")

cat("Tính:\n")
cat("1. Mean, Median, Mode\n")
cat("2. Variance, SD\n")
cat("3. Range, IQR\n")
cat("4. Tứ phân vị Q1, Q2, Q3\n")
cat("5. Hệ số biến thiên (CV)\n")

# Gợi ý code:
# scores <- c(78, 85, 92, 88, 76, 90, 84, 88, 79, 91, 87, 83, 86, 89, 88)
# mean(scores)
# median(scores)
# get_mode(scores)
# var(scores)
# sd(scores)
# ...

# ------------------------------------------------------------------------------
# Bài tập 2: Phân tích dữ liệu
# ------------------------------------------------------------------------------

cat("\nBÀI TẬP 2: PHÂN TÍCH DỮ LIỆU\n")
cat("Sử dụng dữ liệu mtcars trong R:\n")
cat("1. Tính thống kê mô tả cho biến mpg\n")
cat("2. So sánh mpg giữa các nhóm cyl (4, 6, 8 xy-lanh)\n")
cat("3. Tìm outliers trong mpg bằng phương pháp IQR\n")
cat("4. Vẽ histogram và boxplot cho mpg\n")

# ------------------------------------------------------------------------------
# Bài tập 3: Bảng tần số
# ------------------------------------------------------------------------------

cat("\nBÀI TẬP 3: BẢNG TẦN SỐ\n")
cat("Cho dữ liệu tuổi của 40 người:\n")
cat("set.seed(123)\n")
cat("ages <- sample(18:65, 40, replace = TRUE)\n\n")

cat("1. Tạo bảng tần số với các khoảng: 18-25, 26-35, 36-45, 46-55, 56-65\n")
cat("2. Tính tần suất (%) cho mỗi khoảng\n")
cat("3. Vẽ histogram\n")

# ==============================================================================
# CÂU HỎI ÔN TẬP
# ==============================================================================

cat("\n" , rep("=", 70), "\n", sep = "")
cat("CÂU HỎI ÔN TẬP\n")
cat(rep("=", 70), "\n", sep = "")

cat("\n1. Phân biệt Mean, Median và Mode? Khi nào nên dùng mỗi loại?\n")
cat("2. Giải thích ý nghĩa của Standard Deviation?\n")
cat("3. Tại sao chia cho (n-1) thay vì n khi tính variance mẫu?\n")
cat("4. IQR đo lường gì? Tại sao IQR tốt hơn Range?\n")
cat("5. Skewness dương nghĩa là gì? Cho ví dụ?\n")
cat("6. Phân biệt Variance và Standard Deviation?\n")
cat("7. Hệ số biến thiên (CV) dùng để làm gì?\n")
cat("8. Làm thế nào để phát hiện outliers?\n")

# ==============================================================================
# TÀI LIỆU THAM KHẢO
# ==============================================================================

cat("\n" , rep("=", 70), "\n", sep = "")
cat("TÀI LIỆU THAM KHẢO\n")
cat(rep("=", 70), "\n", sep = "")

cat("\n1. R Documentation: ?mean, ?median, ?sd, ?var\n")
cat("2. Package psych: Hàm describe() và describeBy()\n")
cat("3. Package moments: Hàm skewness() và kurtosis()\n")
cat("4. Quick-R: https://www.statmethods.net/stats/descriptives.html\n")

# ==============================================================================
# TỔNG KẾT
# ==============================================================================

cat("\n" , rep("=", 70), "\n", sep = "")
cat("TỔNG KẾT\n")
cat(rep("=", 70), "\n", sep = "")

cat("\nCông thức tổng hợp:\n")
cat("- Mean:      Σx / n\n")
cat("- Median:    Giá trị giữa\n")
cat("- Variance:  Σ(x-x̄)²/(n-1)\n")
cat("- SD:        √Variance\n")
cat("- Range:     Max - Min\n")
cat("- IQR:       Q3 - Q1\n")
cat("- CV:        (SD/Mean)×100%\n")
cat("- Skewness:  E[(X-μ)³]/σ³\n")
cat("- Kurtosis:  E[(X-μ)⁴]/σ⁴\n")

cat("\nLưu ý quan trọng:\n")
cat("✅ Mean: Nhạy cảm với outliers\n")
cat("✅ Median: Tốt khi có outliers\n")
cat("✅ SD: Cùng đơn vị với dữ liệu gốc\n")
cat("✅ IQR: Không bị ảnh hưởng outliers\n")
cat("✅ CV: So sánh độ phân tán giữa các tập khác nhau\n")
cat("✅ Skewness: Đo độ bất đối xứng\n")
cat("✅ Kurtosis: Đo độ nhọn\n")

# Cập nhật: Tháng 3/2026