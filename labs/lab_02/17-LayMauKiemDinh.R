# ==============================================================================
# BÀI 13: LẤY MẪU VÀ KIỂM ĐỊNH THỐNG KÊ
# ==============================================================================
# Mục tiêu học tập:
# - Hiểu khái niệm tổng thể và mẫu
# - Nắm vững các phương pháp lấy mẫu
# - Hiểu sai số chuẩn và khoảng tin cậy
# - Nắm vững các bước kiểm định giả thuyết
# - Thực hiện kiểm định t-test (một mẫu, hai mẫu)
# - Thực hiện kiểm định chi-square
# - Hiểu p-value và mức ý nghĩa α
# - Áp dụng kiểm định vào bài toán thực tế
# ==============================================================================

# ------------------------------------------------------------------------------
# 13.1 Tổng thể và Mẫu
# ------------------------------------------------------------------------------

# 13.1.1 Định nghĩa
#
# Tổng thể (Population)
# - Tập hợp TẤT CẢ các đối tượng quan tâm
# - Ký hiệu tham số: μ (mean), σ (SD), p (tỷ lệ)
#
# Mẫu (Sample)
# - Một TẬP CON của tổng thể
# - Ký hiệu thống kê: x̄ (mean), s (SD), p̂ (tỷ lệ)
#
# Ví dụ:
# - Tổng thể: Tất cả sinh viên Việt Nam (hàng triệu người)
# - Mẫu: 1000 sinh viên được khảo sát

# Minh họa
set.seed(42)

# Tổng thể (giả sử)
population <- rnorm(100000, mean = 170, sd = 8)
cat("Tổng thể - μ:", mean(population), "σ:", sd(population), "\n")

# Lấy mẫu
sample_data <- sample(population, 100)
cat("Mẫu - x̄:", mean(sample_data), "s:", sd(sample_data), "\n")

# 13.1.2 Tại sao cần lấy mẫu?
#
# Lý do:
# 1. CHI PHÍ: Khảo sát toàn bộ tổng thể rất tốn kém
# 2. THỜI GIAN: Mất quá nhiều thời gian
# 3. KHÔNG KHẢ THI: Một số tổng thể vô hạn hoặc không tiếp cận được
# 4. PHÁ HỦY: Kiểm tra tuổi thọ bóng đèn → phải phá hủy
#
# Mục tiêu: Từ mẫu, ƯỚC LƯỢNG tham số của tổng thể.

# ------------------------------------------------------------------------------
# 13.2 Các phương pháp lấy mẫu
# ------------------------------------------------------------------------------

# 13.2.1 Lấy mẫu ngẫu nhiên đơn giản (Simple Random Sampling)
#
# Mỗi phần tử có XÁC SUẤT NHƯ NHAU được chọn.

cat("\n=== LẤY MẪU NGẪU NHIÊN ĐƠN GIẢN ===\n")

# Tổng thể: 1000 sinh viên
population_ids <- 1:1000

# Lấy mẫu 50 sinh viên
set.seed(42)
sample_ids <- sample(population_ids, 50)
cat("Mẫu 50 sinh viên:", head(sample_ids), "...\n")

# Ưu điểm: Đơn giản, công bằng
# Nhược điểm: Cần danh sách đầy đủ tổng thể

# 13.2.2 Lấy mẫu phân tầng (Stratified Sampling)
#
# Chia tổng thể thành CÁC TẦNG (strata), sau đó lấy mẫu từ mỗi tầng.
#
# Ví dụ: Khảo sát sinh viên, chia theo khoa

cat("\n=== LẤY MẪU PHÂN TẦNG ===\n")

# Tổng thể: 1000 sinh viên từ 4 khoa
set.seed(42)
students <- data.frame(
  id = 1:1000,
  khoa = sample(c("CNTT", "KT", "NN", "SP"), 1000, replace = TRUE)
)

# Số lượng mỗi khoa
cat("Số lượng mỗi khoa:\n")
print(table(students$khoa))

# Lấy 10% từ mỗi khoa
library(dplyr)
stratified_sample <- students %>%
  group_by(khoa) %>%
  sample_frac(0.1)

cat("\nMẫu phân tầng:\n")
print(table(stratified_sample$khoa))

# Ưu điểm: Đảm bảo đại diện cho các nhóm
# Nhược điểm: Cần biết cấu trúc tổng thể

# 13.2.3 Lấy mẫu cụm (Cluster Sampling)
#
# Chia tổng thể thành CÁC CỤM, chọn ngẫu nhiên một số cụm.
#
# Ví dụ: Khảo sát sinh viên, chọn ngẫu nhiên 5 lớp

cat("\n=== LẤY MẪU CỤM ===\n")

# Tổng thể: 20 lớp, mỗi lớp 50 sinh viên
set.seed(42)

# Chọn ngẫu nhiên 5 lớp
selected_classes <- sample(1:20, 5)
cat("Các lớp được chọn:", selected_classes, "\n")

# Khảo sát tất cả sinh viên trong 5 lớp đó
# Tổng: 5 × 50 = 250 sinh viên
cat("Tổng số sinh viên: 250\n")

# Ưu điểm: Tiết kiệm chi phí, dễ thực hiện
# Nhược điểm: Sai số lớn hơn nếu các cụm không đồng nhất

# 13.2.4 Lấy mẫu hệ thống (Systematic Sampling)
#
# Chọn phần tử đầu tiên ngẫu nhiên, sau đó chọn MỖI K PHẦN TỬ.

cat("\n=== LẤY MẪU HỆ THỐNG ===\n")

# Tổng thể: 1000 sinh viên
# Muốn lấy mẫu 100 → k = 1000/100 = 10

set.seed(42)
start <- sample(1:10, 1)  # Chọn điểm bắt đầu ngẫu nhiên
systematic_sample <- seq(start, 1000, by = 10)

cat("Bắt đầu từ:", start, "\n")
cat("Mẫu:", head(systematic_sample), "...\n")
cat("Kích thước mẫu:", length(systematic_sample), "\n")

# Ưu điểm: Dễ thực hiện
# Nhược điểm: Có thể bị sai lệch nếu có chu kỳ trong dữ liệu

# ------------------------------------------------------------------------------
# 13.3 Phân phối mẫu và Sai số chuẩn
# ------------------------------------------------------------------------------

# 13.3.1 Phân phối mẫu (Sampling Distribution)
#
# Phân phối mẫu là phân phối của một THỐNG KÊ (như x̄) 
# từ tất cả các mẫu có thể.

cat("\n=== PHÂN PHỐI MẪU ===\n")

set.seed(42)

# Tổng thể
population <- rnorm(100000, mean = 100, sd = 15)

# Lấy 1000 mẫu, mỗi mẫu 30 phần tử
n_samples <- 1000
sample_size <- 30

sample_means <- replicate(n_samples, {
  sample_data <- sample(population, sample_size)
  mean(sample_data)
})

# Vẽ phân phối mẫu
hist(sample_means, breaks = 30,
     main = "Phân phối mẫu của x̄",
     xlab = "Trung bình mẫu",
     col = "lightblue",
     probability = TRUE)

# Thêm đường cong lý thuyết
curve(dnorm(x, mean = 100, sd = 15/sqrt(30)),
      add = TRUE, col = "red", lwd = 2)

legend("topright",
       legend = c("Phân phối mẫu", "Lý thuyết"),
       col = c("lightblue", "red"),
       lwd = c(10, 2))

# 13.3.2 Sai số chuẩn (Standard Error)
#
# Sai số chuẩn (SE) là độ lệch chuẩn của phân phối mẫu.
#
# Công thức:
# SE = σ / √n
#
# Nếu không biết σ, dùng s (SD mẫu):
# SE = s / √n

cat("\n=== SAI SỐ CHUẨN ===\n")

# Ví dụ
sample_data <- c(85, 90, 78, 92, 88, 95, 80, 89, 91, 87)

s <- sd(sample_data)
n <- length(sample_data)
se <- s / sqrt(n)

cat("SD mẫu (s):", round(s, 2), "\n")
cat("Kích thước mẫu (n):", n, "\n")
cat("Sai số chuẩn (SE):", round(se, 2), "\n")

# Ý nghĩa:
# - SE NHỎ: x̄ gần μ
# - SE LỚN: x̄ khác xa μ
# - SE GIẢM khi n TĂNG

# ------------------------------------------------------------------------------
# 13.4 Khoảng tin cậy (Confidence Interval)
# ------------------------------------------------------------------------------

# 13.4.1 Định nghĩa
#
# Khoảng tin cậy là một khoảng giá trị có XÁC SUẤT CHỨA THAM SỐ tổng thể.
#
# Khoảng tin cậy 95% cho μ:
# x̄ ± 1.96 × SE
#
# Giải thích: 95% các khoảng như vậy sẽ chứa μ.

# 13.4.2 Khoảng tin cậy cho trung bình (σ biết)
#
# Công thức:
# CI = x̄ ± z(α/2) × (σ / √n)
#
# Với:
# - z(α/2): Giá trị z tương ứng mức tin cậy
# - 90%: z = 1.645
# - 95%: z = 1.96
# - 99%: z = 2.576

cat("\n=== KHOẢNG TIN CẬY (σ biết) ===\n")

# Mẫu 100 sinh viên, x̄ = 75, σ = 10 (biết trước)
xbar <- 75
sigma <- 10
n <- 100

# Khoảng tin cậy 95%
z <- qnorm(0.975)  # 1.96
se <- sigma / sqrt(n)
margin <- z * se

lower <- xbar - margin
upper <- xbar + margin

cat("95% CI: [", round(lower, 2), ",", round(upper, 2), "]\n")

# 13.4.3 Khoảng tin cậy cho trung bình (σ không biết)
#
# Dùng PHÂN PHỐI T thay vì z.
#
# Công thức:
# CI = x̄ ± t(α/2, df) × (s / √n)
#
# Với df = n - 1 (bậc tự do)

cat("\n=== KHOẢNG TIN CẬY (σ không biết) ===\n")

# Ví dụ
sample_data <- c(85, 90, 78, 92, 88, 95, 80, 89, 91, 87)

# Cách 1: Tự tính
xbar <- mean(sample_data)
s <- sd(sample_data)
n <- length(sample_data)
df <- n - 1

# t-value cho 95% CI
t_value <- qt(0.975, df)
se <- s / sqrt(n)
margin <- t_value * se

lower <- xbar - margin
upper <- xbar + margin

cat("95% CI (tự tính): [", round(lower, 2), ",", round(upper, 2), "]\n")

# Cách 2: Dùng hàm t.test()
ci <- t.test(sample_data)$conf.int
cat("95% CI (t.test):  [", round(ci[1], 2), ",", round(ci[2], 2), "]\n")

# 13.4.4 Giải thích khoảng tin cậy
#
# SAI: "Xác suất μ nằm trong [80, 90] là 95%"
# ĐÚNG: "95% các khoảng tin cậy tính theo cách này sẽ chứa μ"

cat("\n=== MINH HỌA KHOẢNG TIN CẬY ===\n")

set.seed(42)

# Tổng thể với μ = 100
population <- rnorm(100000, mean = 100, sd = 15)

# Tính 100 khoảng tin cậy
n_intervals <- 100
sample_size <- 30

results <- data.frame(
  lower = numeric(n_intervals),
  upper = numeric(n_intervals),
  contains_mu = logical(n_intervals)
)

for (i in 1:n_intervals) {
  sample_data <- sample(population, sample_size)
  ci <- t.test(sample_data)$conf.int
  results$lower[i] <- ci[1]
  results$upper[i] <- ci[2]
  results$contains_mu[i] <- (100 >= ci[1] & 100 <= ci[2])
}

# Đếm bao nhiêu khoảng chứa μ
count <- sum(results$contains_mu)
cat("Số khoảng chứa μ = 100:", count, "/", n_intervals, "\n")
cat("Tỷ lệ:", count/n_intervals * 100, "%\n")

# ------------------------------------------------------------------------------
# 13.5 Kiểm định giả thuyết (Hypothesis Testing)
# ------------------------------------------------------------------------------

# 13.5.1 Khái niệm cơ bản
#
# Giả thuyết không (H₀): Giả thuyết ban đầu, thường là "không có sự khác biệt"
# Giả thuyết đối (H₁ hoặc Hₐ): Giả thuyết mà ta muốn chứng minh
#
# Ví dụ:
# - H₀: μ = 100 (IQ trung bình = 100)
# - H₁: μ ≠ 100 (IQ trung bình khác 100)

# 13.5.2 Các loại kiểm định

cat("\n=== CÁC LOẠI KIỂM ĐỊNH ===\n")

# Thiết lập layout
par(mfrow = c(1, 3))

# Hai đuôi
curve(dnorm(x), -4, 4, main = "Hai đuôi (Two-tailed)", ylab = "")
abline(v = c(-1.96, 1.96), col = "red", lty = 2)
text(0, 0.2, "H₁: μ ≠ μ₀")

# Đuôi phải
curve(dnorm(x), -4, 4, main = "Đuôi phải (Right-tailed)", ylab = "")
abline(v = 1.645, col = "red", lty = 2)
text(0, 0.2, "H₁: μ > μ₀")

# Đuôi trái
curve(dnorm(x), -4, 4, main = "Đuôi trái (Left-tailed)", ylab = "")
abline(v = -1.645, col = "red", lty = 2)
text(0, 0.2, "H₁: μ < μ₀")

# Reset layout
par(mfrow = c(1, 1))

# 13.5.3 P-value
#
# P-value là xác suất quan sát được dữ liệu CỰC ĐOAN NHƯ VẬY HOẶC HƠN, 
# nếu H₀ đúng.
#
# Quy tắc:
# - p-value < α: BÁC BỎ H₀
# - p-value ≥ α: KHÔNG BÁC BỎ H₀
#
# Thường dùng α = 0.05 (mức ý nghĩa 5%)

# 13.5.4 Các bước kiểm định
#
# Bước 1: Thiết lập giả thuyết H₀ và H₁
# Bước 2: Chọn mức ý nghĩa α (thường 0.05)
# Bước 3: Tính thống kê kiểm định (t, z, chi-square, ...)
# Bước 4: Tính p-value
# Bước 5: Kết luận
#   - p < α: Bác bỏ H₀
#   - p ≥ α: Không bác bỏ H₀

# 13.5.5 Loại sai lầm
#
# Sai lầm loại I (Type I Error):
# - Bác bỏ H₀ khi H₀ ĐÚNG
# - Xác suất = α
#
# Sai lầm loại II (Type II Error):
# - Không bác bỏ H₀ khi H₀ SAI
# - Xác suất = β
#
# Bảng quyết định:
# |                    | H₀ đúng              | H₀ sai               |
# |--------------------|----------------------|----------------------|
# | Bác bỏ H₀          | Sai lầm I (α)        | Đúng (Power = 1-β)   |
# | Không bác bỏ H₀    | Đúng                 | Sai lầm II (β)       |

# ------------------------------------------------------------------------------
# 13.6 Kiểm định t (t-test)
# ------------------------------------------------------------------------------

# 13.6.1 Kiểm định t một mẫu (One-sample t-test)
#
# Mục đích: Kiểm tra xem trung bình mẫu có KHÁC với một giá trị cho trước không.
#
# Giả thuyết:
# - H₀: μ = μ₀
# - H₁: μ ≠ μ₀
#
# Thống kê:
# t = (x̄ - μ₀) / (s / √n)

cat("\n=== ONE-SAMPLE T-TEST ===\n")

# Ví dụ: Chiều cao nam sinh viên có trung bình = 170cm?

# Dữ liệu: Chiều cao 20 nam sinh viên
set.seed(42)
heights <- c(168, 172, 165, 175, 170, 173, 169, 171, 174, 167,
             172, 170, 168, 176, 171, 169, 173, 170, 174, 172)

# H0: μ = 170
# H1: μ ≠ 170

# Kiểm định
result <- t.test(heights, mu = 170)
print(result)

cat("\nKẾT LUẬN:\n")
if (result$p.value < 0.05) {
  cat("p-value =", round(result$p.value, 4), "< 0.05\n")
  cat("BÁC BỎ H0: Chiều cao TB KHÁC 170cm\n")
} else {
  cat("p-value =", round(result$p.value, 4), "≥ 0.05\n")
  cat("KHÔNG BÁC BỎ H0: Chiều cao TB có thể = 170cm\n")
}

# 13.6.2 Kiểm định t hai mẫu độc lập (Independent two-sample t-test)
#
# Mục đích: So sánh trung bình của HAI NHÓM ĐỘC LẬP.
#
# Giả thuyết:
# - H₀: μ₁ = μ₂
# - H₁: μ₁ ≠ μ₂

cat("\n=== INDEPENDENT TWO-SAMPLE T-TEST ===\n")

# Ví dụ: So sánh điểm thi giữa nam và nữ

# Dữ liệu
set.seed(42)
male_scores <- c(75, 80, 72, 85, 78, 82, 76, 79, 81, 77)
female_scores <- c(82, 85, 80, 88, 83, 86, 84, 87, 85, 83)

# H0: μ_nam = μ_nữ
# H1: μ_nam ≠ μ_nữ

# Kiểm định
result <- t.test(male_scores, female_scores)
print(result)

cat("\nTHỐNG KÊ MÔ TẢ:\n")
cat("Nam - Mean:", mean(male_scores), "SD:", sd(male_scores), "\n")
cat("Nữ - Mean:", mean(female_scores), "SD:", sd(female_scores), "\n")

cat("\nKẾT LUẬN:\n")
if (result$p.value < 0.05) {
  cat("p-value =", round(result$p.value, 4), "< 0.05\n")
  cat("BÁC BỎ H0: Có sự KHÁC BIỆT giữa nam và nữ\n")
} else {
  cat("p-value =", round(result$p.value, 4), "≥ 0.05\n")
  cat("KHÔNG BÁC BỎ H0: KHÔNG có sự khác biệt\n")
}

# 13.6.3 Kiểm định t hai mẫu ghép đôi (Paired t-test)
#
# Mục đích: So sánh TRƯỚC VÀ SAU trong cùng nhóm.
#
# Giả thuyết:
# - H₀: μ_diff = 0
# - H₁: μ_diff ≠ 0

cat("\n=== PAIRED T-TEST ===\n")

# Ví dụ: Điểm thi trước và sau khóa học

# Dữ liệu
set.seed(42)
before <- c(65, 70, 68, 72, 69, 71, 67, 73, 70, 68)
after <- c(70, 75, 72, 78, 74, 76, 71, 77, 75, 73)

# H0: μ_after - μ_before = 0
# H1: μ_after - μ_before ≠ 0

# Kiểm định
result <- t.test(after, before, paired = TRUE)
print(result)

# Tính chênh lệch
diff <- after - before
cat("\nChênh lệch trung bình:", mean(diff), "\n")
cat("SD chênh lệch:", sd(diff), "\n")

cat("\nKẾT LUẬN:\n")
if (result$p.value < 0.05) {
  cat("p-value =", round(result$p.value, 4), "< 0.05\n")
  cat("BÁC BỎ H0: Điểm SAU cao hơn TRƯỚC\n")
} else {
  cat("p-value =", round(result$p.value, 4), "≥ 0.05\n")
  cat("KHÔNG BÁC BỎ H0: KHÔNG có sự cải thiện\n")
}

# ------------------------------------------------------------------------------
# 13.7 Kiểm định Chi-square
# ------------------------------------------------------------------------------

# 13.7.1 Kiểm định Chi-square độc lập
#
# Mục đích: Kiểm tra MỐI QUAN HỆ giữa hai biến phân loại.
#
# Giả thuyết:
# - H₀: Hai biến ĐỘC LẬP
# - H₁: Hai biến CÓ QUAN HỆ

cat("\n=== CHI-SQUARE TEST (ĐỘC LẬP) ===\n")

# Ví dụ: Giới tính và sở thích môn học

# Tạo bảng chéo
data <- matrix(c(30, 10, 15, 25, 20, 30), nrow = 2, byrow = TRUE)
rownames(data) <- c("Nam", "Nữ")
colnames(data) <- c("Toán", "Văn", "Anh")

cat("Bảng chéo:\n")
print(data)

# H0: Giới tính và sở thích độc lập
# H1: Giới tính và sở thích có quan hệ

# Kiểm định
result <- chisq.test(data)
print(result)

cat("\nKẾT LUẬN:\n")
if (result$p.value < 0.05) {
  cat("p-value =", round(result$p.value, 4), "< 0.05\n")
  cat("BÁC BỎ H0: Giới tính VÀ sở thích CÓ QUAN HỆ\n")
} else {
  cat("p-value =", round(result$p.value, 4), "≥ 0.05\n")
  cat("KHÔNG BÁC BỎ H0: Giới tính và sở thích ĐỘC LẬP\n")
}

# Xem tần số kỳ vọng
cat("\nTần số kỳ vọng:\n")
print(result$expected)

# 13.7.2 Kiểm định Chi-square phù hợp (Goodness-of-fit)
#
# Mục đích: Kiểm tra xem dữ liệu có PHÙ HỢP với phân phối lý thuyết không.

cat("\n=== CHI-SQUARE TEST (PHÙ HỢP) ===\n")

# Ví dụ: Xúc xắc có cân đối không?

# Tung xúc xắc 120 lần
observed <- c(18, 22, 19, 21, 20, 20)
names(observed) <- 1:6

cat("Tần số quan sát:\n")
print(observed)

# H0: Xúc xắc cân đối (mỗi mặt p = 1/6)
# H1: Xúc xắc không cân đối

# Tần số kỳ vọng (nếu cân đối)
expected <- rep(120/6, 6)
cat("\nTần số kỳ vọng:\n")
print(expected)

# Kiểm định
result <- chisq.test(observed, p = rep(1/6, 6))
print(result)

cat("\nKẾT LUẬN:\n")
if (result$p.value < 0.05) {
  cat("p-value =", round(result$p.value, 4), "< 0.05\n")
  cat("BÁC BỎ H0: Xúc xắc KHÔNG cân đối\n")
} else {
  cat("p-value =", round(result$p.value, 4), "≥ 0.05\n")
  cat("KHÔNG BÁC BỎ H0: Xúc xắc CÂN ĐỐI\n")
}

# ------------------------------------------------------------------------------
# 13.8 Ví dụ tổng hợp
# ------------------------------------------------------------------------------

# Ví dụ 1: Nghiên cứu thuốc mới
#
# Bối cảnh: Thử nghiệm thuốc giảm cholesterol trên 30 bệnh nhân.

cat("\n", rep("=", 70), "\n", sep = "")
cat("VÍ DỤ 1: NGHIÊN CỨU THUỐC MỚI\n")
cat(rep("=", 70), "\n", sep = "")

set.seed(123)

# Cholesterol trước và sau dùng thuốc
before <- rnorm(30, mean = 220, sd = 20)
after <- before - rnorm(30, mean = 15, sd = 8)

# 1. Thống kê mô tả
cat("\n1. THỐNG KÊ MÔ TẢ\n")
cat("Trước - Mean:", round(mean(before), 2), "SD:", round(sd(before), 2), "\n")
cat("Sau - Mean:", round(mean(after), 2), "SD:", round(sd(after), 2), "\n")
cat("Chênh lệch TB:", round(mean(before) - mean(after), 2), "\n")

# 2. Kiểm định paired t-test
cat("\n2. KIỂM ĐỊNH\n")
cat("H0: Thuốc KHÔNG có tác dụng (μ_diff = 0)\n")
cat("H1: Thuốc CÓ tác dụng (μ_diff ≠ 0)\n\n")

result <- t.test(before, after, paired = TRUE)
print(result)

# 3. Kết luận
cat("\n3. KẾT LUẬN\n")
if (result$p.value < 0.05) {
  cat("p-value < 0.05 → BÁC BỎ H0\n")
  cat("Thuốc CÓ HIỆU QUẢ giảm cholesterol\n")
  cat("Giảm trung bình:", round(mean(before - after), 2), "mg/dL\n")
  cat("95% CI:", round(result$conf.int, 2), "\n")
} else {
  cat("p-value ≥ 0.05 → KHÔNG BÁC BỎ H0\n")
  cat("CHƯA có bằng chứng thuốc có hiệu quả\n")
}

# 4. Vẽ biểu đồ
boxplot(before, after,
        names = c("Trước", "Sau"),
        main = "Cholesterol trước và sau dùng thuốc",
        ylab = "Cholesterol (mg/dL)",
        col = c("lightcoral", "lightblue"))

# Ví dụ 2: Khảo sát A/B Testing

cat("\n", rep("=", 70), "\n", sep = "")
cat("VÍ DỤ 2: A/B TESTING\n")
cat(rep("=", 70), "\n", sep = "")

# Dữ liệu
# Phiên bản A: 1000 người, 150 click
# Phiên bản B: 1000 người, 180 click

n_A <- 1000
clicks_A <- 150
n_B <- 1000
clicks_B <- 180

# Tỷ lệ
p_A <- clicks_A / n_A
p_B <- clicks_B / n_B

cat("\nPhiên bản A: Tỷ lệ click =", p_A * 100, "%\n")
cat("Phiên bản B: Tỷ lệ click =", p_B * 100, "%\n")

# Kiểm định tỷ lệ
# H0: p_A = p_B
# H1: p_A ≠ p_B

result <- prop.test(c(clicks_A, clicks_B), c(n_A, n_B))
print(result)

cat("\nKẾT LUẬN:\n")
if (result$p.value < 0.05) {
  cat("p-value < 0.05 → CÓ SỰ KHÁC BIỆT\n")
  if (p_B > p_A) {
    cat("Phiên bản B TỐT HƠN phiên bản A\n")
  } else {
    cat("Phiên bản A TỐT HƠN phiên bản B\n")
  }
} else {
  cat("p-value ≥ 0.05 → KHÔNG có sự khác biệt\n")
  cat("Cả 2 phiên bản TƯƠNG ĐƯƠNG\n")
}

# ==============================================================================
# BÀI TẬP THỰC HÀNH
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("BÀI TẬP THỰC HÀNH\n")
cat(rep("=", 70), "\n", sep = "")

# ------------------------------------------------------------------------------
# Bài tập 1: Khoảng tin cậy
# ------------------------------------------------------------------------------

cat("\nBÀI TẬP 1: KHOẢNG TIN CẬY\n")
cat("Một mẫu 25 sinh viên có điểm TB = 78, SD = 8.\n\n")

cat("1. Tính 95% CI cho điểm TB tổng thể\n")
cat("2. Tính 99% CI\n")
cat("3. Giải thích ý nghĩa của CI\n")

# Gợi ý:
# xbar <- 78
# s <- 8
# n <- 25
# t_value_95 <- qt(0.975, n-1)
# se <- s / sqrt(n)
# ...

# ------------------------------------------------------------------------------
# Bài tập 2: One-sample t-test
# ------------------------------------------------------------------------------

cat("\nBÀI TẬP 2: ONE-SAMPLE T-TEST\n")
cat("Nhà máy sản xuất bóng đèn tuyên bố tuổi thọ TB = 1000 giờ.\n")
cat("Kiểm tra 20 bóng đèn:\n")
cat("980, 1020, 990, 1010, 1005, 995, 1015, 985, 1000, 1010,\n")
cat("995, 1005, 1000, 990, 1015, 1010, 995, 1000, 1005, 1010\n\n")

cat("Kiểm định với α = 0.05: Tuổi thọ TB có = 1000 giờ?\n")

# Gợi ý:
# data <- c(980, 1020, 990, ...)
# t.test(data, mu = 1000)

# ------------------------------------------------------------------------------
# Bài tập 3: Independent t-test
# ------------------------------------------------------------------------------

cat("\nBÀI TẬP 3: INDEPENDENT T-TEST\n")
cat("So sánh điểm thi giữa 2 lớp:\n")
cat("Lớp A: 75, 80, 72, 85, 78, 82, 76, 79, 81, 77\n")
cat("Lớp B: 82, 85, 80, 88, 83, 86, 84, 87, 85, 83\n\n")

cat("Có sự khác biệt không?\n")

# ------------------------------------------------------------------------------
# Bài tập 4: Paired t-test
# ------------------------------------------------------------------------------

cat("\nBÀI TẬP 4: PAIRED T-TEST\n")
cat("Cân nặng trước và sau chế độ ăn kiêng (10 người):\n")
cat("Trước: 70, 75, 68, 80, 72, 77, 69, 74, 76, 71\n")
cat("Sau: 68, 72, 66, 77, 70, 74, 67, 71, 73, 69\n\n")

cat("Chế độ ăn có hiệu quả không?\n")

# ------------------------------------------------------------------------------
# Bài tập 5: Chi-square
# ------------------------------------------------------------------------------

cat("\nBÀI TẬP 5: CHI-SQUARE\n")
cat("Khảo sát 200 người về sở thích phim:\n")
cat("           Hành động  Tâm lý  Hài\n")
cat("Nam            40      30     30\n")
cat("Nữ             20      50     30\n\n")

cat("Giới tính và sở thích phim có quan hệ không?\n")

# ==============================================================================
# CÂU HỎI ÔN TẬP
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("CÂU HỎI ÔN TẬP\n")
cat(rep("=", 70), "\n", sep = "")

cat("\n1. Phân biệt tổng thể và mẫu?\n")
cat("2. Sai số chuẩn (SE) là gì? Khác gì với SD?\n")
cat("3. Giải thích ý nghĩa của khoảng tin cậy 95%?\n")
cat("4. Phân biệt H₀ và H₁?\n")
cat("5. P-value là gì? Khi nào bác bỏ H₀?\n")
cat("6. Phân biệt sai lầm loại I và loại II?\n")
cat("7. Khi nào dùng z-test? Khi nào dùng t-test?\n")
cat("8. Paired t-test khác gì independent t-test?\n")

# ==============================================================================
# TÀI LIỆU THAM KHẢO
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("TÀI LIỆU THAM KHẢO\n")
cat(rep("=", 70), "\n", sep = "")

cat("\n1. R Documentation: ?t.test, ?chisq.test, ?prop.test\n")
cat("2. Statistics by Jim: https://statisticsbyjim.com/\n")
cat("3. Khan Academy: Hypothesis Testing\n")

# ==============================================================================
# TỔNG KẾT
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("TỔNG KẾT\n")
cat(rep("=", 70), "\n", sep = "")

cat("\nCông thức tổng hợp:\n\n")

cat("SAI SỐ CHUẨN:\n")
cat("SE = s / √n\n\n")

cat("KHOẢNG TIN CẬY:\n")
cat("CI = x̄ ± t(α/2, df) × SE\n\n")

cat("THỐNG KÊ T:\n")
cat("t = (x̄ - μ₀) / SE\n\n")

cat("Quy trình kiểm định:\n")
cat("1. Thiết lập: H₀ và H₁\n")
cat("2. Chọn: Mức ý nghĩa α\n")
cat("3. Tính: Thống kê kiểm định\n")
cat("4. Tính: p-value\n")
cat("5. Kết luận: So sánh p với α\n\n")

cat("Lưu ý:\n")
cat("✅ α = 0.05: Mức ý nghĩa thường dùng\n")
cat("✅ p < 0.05: Bác bỏ H₀\n")
cat("✅ CI 95%: Khoảng tin cậy phổ biến\n")
cat("✅ n ≥ 30: Mẫu lớn\n")
cat("✅ Paired: Cùng đối tượng, trước/sau\n")
cat("✅ Independent: Hai nhóm khác nhau\n")

# Cập nhật: Tháng 3/2026