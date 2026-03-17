# ==============================================================================
# BÀI 12: PHÂN PHỐI XÁC SUẤT
# ==============================================================================
# Mục tiêu học tập:
# - Hiểu khái niệm biến ngẫu nhiên và phân phối xác suất
# - Phân biệt biến ngẫu nhiên rời rạc và liên tục
# - Nắm vững các phân phối xác suất quan trọng (Binomial, Poisson, Normal)
# - Tính xác suất bằng các hàm phân phối trong R
# - Hiểu và áp dụng phân phối chuẩn (Normal Distribution)
# - Sử dụng Z-score và bảng Z
# - Áp dụng phân phối xác suất vào bài toán thực tế
# ==============================================================================

# ------------------------------------------------------------------------------
# 12.1 Biến ngẫu nhiên
# ------------------------------------------------------------------------------

# 12.1.1 Biến ngẫu nhiên là gì?
#
# Biến ngẫu nhiên (Random Variable) là một biến có giá trị được xác định 
# bởi KẾT QUẢ của một SỰ KIỆN NGẪU NHIÊN.
#
# Ký hiệu: Thường dùng chữ in hoa X, Y, Z
#
# Ví dụ:
# - X = Số chấm khi tung xúc xắc (X có thể = 1, 2, 3, 4, 5, 6)
# - Y = Chiều cao của sinh viên (Y có thể = 165cm, 170cm, ...)
# - Z = Số cuộc gọi đến tổng đài trong 1 giờ

# 12.1.2 Phân loại biến ngẫu nhiên

# 1. Biến ngẫu nhiên rời rạc (Discrete Random Variable)
# - Nhận SỐ HỮU HẠN hoặc ĐẾM ĐƯỢC các giá trị
# - Thường là SỐ ĐẾM

# Ví dụ: Tung xúc xắc 10 lần
set.seed(42)
dice_rolls <- sample(1:6, 10, replace = TRUE)
cat("Kết quả tung xúc xắc:", dice_rolls, "\n")
cat("Các giá trị khác nhau:", unique(dice_rolls), "\n")

# 2. Biến ngẫu nhiên liên tục (Continuous Random Variable)
# - Nhận VÔ SỐ giá trị trong một khoảng
# - Thường là PHÉP ĐO

# Ví dụ: Chiều cao ngẫu nhiên
set.seed(42)
heights <- rnorm(10, mean = 170, sd = 5)
cat("Chiều cao (cm):", round(heights, 2), "\n")

# 12.1.3 Hàm phân phối xác suất
#
# - Hàm khối xác suất (PMF) - cho biến rời rạc
#   P(X = x): Xác suất X nhận giá trị x
#   Tổng tất cả xác suất = 1
#
# - Hàm mật độ xác suất (PDF) - cho biến liên tục
#   f(x): Mật độ xác suất tại x
#   Diện tích dưới đường cong = 1
#
# - Hàm phân phối tích lũy (CDF)
#   F(x) = P(X ≤ x): Xác suất X nhỏ hơn hoặc bằng x

# ------------------------------------------------------------------------------
# 12.2 Phân phối Binomial (Nhị thức)
# ------------------------------------------------------------------------------

# 12.2.1 Định nghĩa
#
# Phân phối Binomial mô tả số lần THÀNH CÔNG trong N LẦN THỬ ĐỘC LẬP, 
# mỗi lần có xác suất thành công là P.
#
# Điều kiện áp dụng:
# 1. Có N LẦN THỬ độc lập
# 2. Mỗi lần chỉ có 2 KẾT QUẢ: thành công hoặc thất bại
# 3. Xác suất thành công P GIỐNG NHAU ở mỗi lần
#
# Ký hiệu: X ~ B(n, p)
# - n: số lần thử
# - p: xác suất thành công
#
# Công thức:
# P(X = k) = C(n,k) × p^k × (1-p)^(n-k)

# 12.2.2 Ví dụ cơ bản

# Ví dụ 1: Tung đồng xu 10 lần, xác suất ra mặt ngửa = 0.5
n <- 10
p <- 0.5
k <- 5

# Xác suất có đúng 5 lần ngửa
prob <- dbinom(k, size = n, prob = p)
cat("P(X = 5) =", round(prob, 4), "\n")
# 0.2461

# Ví dụ 2: Bắn cung 20 lần, xác suất trúng đích = 0.7
cat("\n=== VÍ DỤ BẮN CUNG ===\n")

# Xác suất trúng đích đúng 15 lần
prob_15 <- dbinom(15, size = 20, prob = 0.7)
cat("P(X = 15) =", round(prob_15, 4), "\n")

# Xác suất trúng ít nhất 15 lần
prob_at_least_15 <- sum(dbinom(15:20, size = 20, prob = 0.7))
cat("P(X ≥ 15) =", round(prob_at_least_15, 4), "\n")

# 12.2.3 Các hàm trong R
#
# R có 4 hàm cho phân phối Binomial:
#
# 1. dbinom(x, size, prob) - Hàm khối xác suất (PMF)
#    Tính P(X = x)
#
# 2. pbinom(q, size, prob) - Hàm phân phối tích lũy (CDF)
#    Tính P(X ≤ q)
#
# 3. qbinom(p, size, prob) - Hàm phân vị (Quantile)
#    Tìm x sao cho P(X ≤ x) = p
#
# 4. rbinom(n, size, prob) - Sinh số ngẫu nhiên
#    Sinh n số ngẫu nhiên từ phân phối Binomial

n <- 10
p <- 0.5

cat("\n=== CÁC HÀM BINOMIAL ===\n\n")

# 1. dbinom - P(X = k)
cat("1. P(X = 5) =", dbinom(5, n, p), "\n")

# 2. pbinom - P(X ≤ k)
cat("2. P(X ≤ 5) =", pbinom(5, n, p), "\n")

# 3. qbinom - Tìm k sao cho P(X ≤ k) = 0.5
cat("3. Median =", qbinom(0.5, n, p), "\n")

# 4. rbinom - Sinh 10 số ngẫu nhiên
set.seed(42)
random_values <- rbinom(10, size = n, prob = p)
cat("4. Random:", random_values, "\n")

# 12.2.4 Mean và Variance của Binomial
#
# Kỳ vọng (Mean):
# E(X) = n × p
#
# Phương sai (Variance):
# Var(X) = n × p × (1 - p)

n <- 100
p <- 0.3

mean_val <- n * p
var_val <- n * p * (1 - p)
sd_val <- sqrt(var_val)

cat("\n=== MEAN VÀ VARIANCE ===\n")
cat("Mean:", mean_val, "\n")         # 30
cat("Variance:", var_val, "\n")      # 21
cat("SD:", round(sd_val, 2), "\n")   # 4.58

# 12.2.5 Ví dụ thực tế
#
# Ví dụ: Một công ty có tỷ lệ sản phẩm lỗi là 5%. Kiểm tra 50 sản phẩm.

cat("\n=== VÍ DỤ THỰC TẾ: SẢN PHẨM LỖI ===\n")

n <- 50
p <- 0.05

# a) Xác suất có đúng 3 sản phẩm lỗi?
prob_3 <- dbinom(3, n, p)
cat("a) P(X = 3) =", round(prob_3, 4), "\n")

# b) Xác suất có nhiều hơn 5 sản phẩm lỗi?
prob_more_5 <- 1 - pbinom(5, n, p)
cat("b) P(X > 5) =", round(prob_more_5, 4), "\n")

# c) Số sản phẩm lỗi kỳ vọng?
expected <- n * p
cat("c) Kỳ vọng:", expected, "sản phẩm\n")

# 12.2.6 Vẽ biểu đồ Binomial

n <- 20
p <- 0.3

# Tạo dữ liệu
x <- 0:n
pmf <- dbinom(x, n, p)

# Vẽ biểu đồ
barplot(pmf, names.arg = x,
        main = paste("Binomial Distribution (n =", n, ", p =", p, ")"),
        xlab = "Số lần thành công",
        ylab = "Xác suất",
        col = "steelblue")

# Thêm đường trung bình
abline(v = n*p + 0.5, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Mean"), col = "red", lty = 2, lwd = 2)

# ------------------------------------------------------------------------------
# 12.3 Phân phối Poisson
# ------------------------------------------------------------------------------

# 12.3.1 Định nghĩa
#
# Phân phối Poisson mô tả số lần một sự kiện xảy ra trong một 
# KHOẢNG THỜI GIAN/KHÔNG GIAN CỐ ĐỊNH.
#
# Điều kiện áp dụng:
# 1. Sự kiện xảy ra ĐỘC LẬP
# 2. Tỷ lệ trung bình (λ) KHÔNG ĐỔI
# 3. Hai sự kiện KHÔNG XẢY RA ĐỒNG THỜI
#
# Ký hiệu: X ~ Poisson(λ)
# - λ (lambda): Số sự kiện trung bình
#
# Công thức:
# P(X = k) = (e^(-λ) × λ^k) / k!

# 12.3.2 Ví dụ cơ bản
#
# Ví dụ: Số cuộc gọi đến tổng đài trung bình 3 cuộc/phút

cat("\n=== PHÂN PHỐI POISSON ===\n")

lambda <- 3

# Xác suất có đúng 5 cuộc gọi
prob_5 <- dpois(5, lambda)
cat("P(X = 5) =", round(prob_5, 4), "\n")

# Xác suất có nhiều hơn 5 cuộc gọi
prob_more_5 <- 1 - ppois(5, lambda)
cat("P(X > 5) =", round(prob_more_5, 4), "\n")

# 12.3.3 Các hàm trong R
#
# 1. dpois(x, lambda) - Hàm khối xác suất
#    Tính P(X = x)
#
# 2. ppois(q, lambda) - Hàm phân phối tích lũy
#    Tính P(X ≤ q)
#
# 3. qpois(p, lambda) - Hàm phân vị
#    Tìm x sao cho P(X ≤ x) = p
#
# 4. rpois(n, lambda) - Sinh số ngẫu nhiên

lambda <- 4

cat("\n=== CÁC HÀM POISSON ===\n\n")

# 1. dpois
cat("1. P(X = 4) =", dpois(4, lambda), "\n")

# 2. ppois
cat("2. P(X ≤ 4) =", ppois(4, lambda), "\n")

# 3. qpois
cat("3. Median =", qpois(0.5, lambda), "\n")

# 4. rpois
set.seed(42)
random_values <- rpois(10, lambda)
cat("4. Random:", random_values, "\n")

# 12.3.4 Mean và Variance của Poisson
#
# Đặc điểm quan trọng: Mean = Variance = λ

lambda <- 5

cat("\n=== MEAN VÀ VARIANCE ===\n")
cat("Mean:", lambda, "\n")
cat("Variance:", lambda, "\n")
cat("SD:", sqrt(lambda), "\n")

# 12.3.5 Ví dụ thực tế
#
# Ví dụ: Một trang web nhận trung bình 100 lượt truy cập/giờ.

cat("\n=== VÍ DỤ THỰC TẾ: TRUY CẬP WEBSITE ===\n")

lambda <- 100

# a) Xác suất có đúng 90 lượt trong 1 giờ?
prob_90 <- dpois(90, lambda)
cat("a) P(X = 90) =", round(prob_90, 4), "\n")

# b) Xác suất có ít hơn 80 lượt?
prob_less_80 <- ppois(79, lambda)
cat("b) P(X < 80) =", round(prob_less_80, 4), "\n")

# c) Xác suất có từ 90 đến 110 lượt?
prob_90_110 <- ppois(110, lambda) - ppois(89, lambda)
cat("c) P(90 ≤ X ≤ 110) =", round(prob_90_110, 4), "\n")

# 12.3.6 Vẽ biểu đồ Poisson

lambda <- 5

# Tạo dữ liệu
x <- 0:15
pmf <- dpois(x, lambda)

# Vẽ biểu đồ
barplot(pmf, names.arg = x,
        main = paste("Poisson Distribution (λ =", lambda, ")"),
        xlab = "Số sự kiện",
        ylab = "Xác suất",
        col = "coral")

# Thêm đường trung bình
abline(v = lambda + 0.5, col = "red", lwd = 2, lty = 2)

# ------------------------------------------------------------------------------
# 12.4 Phân phối Chuẩn (Normal Distribution)
# ------------------------------------------------------------------------------

# 12.4.1 Định nghĩa
#
# Phân phối chuẩn (hay phân phối Gauss) là phân phối liên tục 
# quan trọng nhất trong thống kê.
#
# Ký hiệu: X ~ N(μ, σ²)
# - μ (mu): Trung bình
# - σ² (sigma²): Phương sai
# - σ: Độ lệch chuẩn
#
# Đặc điểm:
# 1. Hình dạng CHUÔNG (bell-shaped)
# 2. ĐỐI XỨNG qua μ
# 3. Mean = Median = Mode = μ
# 4. Diện tích dưới đường cong = 1

# 12.4.2 Phân phối chuẩn chuẩn hóa
#
# Phân phối chuẩn chuẩn hóa: Z ~ N(0, 1)
# - Mean = 0
# - SD = 1
#
# Công thức chuyển đổi (Z-score):
# Z = (X - μ) / σ
#
# Z-score cho biết X cách trung bình bao nhiêu độ lệch chuẩn.

cat("\n=== PHÂN PHỐI CHUẨN ===\n")

# Ví dụ: IQ có mean = 100, SD = 15
# IQ = 130 tương ứng Z-score bao nhiêu?

x <- 130
mu <- 100
sigma <- 15

z <- (x - mu) / sigma
cat("Z-score của IQ 130:", z, "\n")  # 2

cat("Giải thích: IQ 130 cao hơn trung bình 2 độ lệch chuẩn\n")

# 12.4.3 Các hàm trong R
#
# 1. dnorm(x, mean, sd) - Hàm mật độ xác suất (PDF)
#    Tính mật độ tại x
#
# 2. pnorm(q, mean, sd) - Hàm phân phối tích lũy (CDF)
#    Tính P(X ≤ q)
#
# 3. qnorm(p, mean, sd) - Hàm phân vị
#    Tìm x sao cho P(X ≤ x) = p
#
# 4. rnorm(n, mean, sd) - Sinh số ngẫu nhiên

mu <- 100
sigma <- 15

cat("\n=== CÁC HÀM NORMAL ===\n\n")

# 1. dnorm - Mật độ
cat("1. Mật độ tại x=100:", dnorm(100, mu, sigma), "\n")

# 2. pnorm - P(X ≤ x)
cat("2. P(X ≤ 100) =", pnorm(100, mu, sigma), "\n")  # 0.5

# 3. qnorm - Tìm x
cat("3. Giá trị tại 95%:", qnorm(0.95, mu, sigma), "\n")

# 4. rnorm - Sinh số ngẫu nhiên
set.seed(42)
random_values <- rnorm(5, mu, sigma)
cat("4. Random:", round(random_values, 2), "\n")

# 12.4.4 Quy tắc 68-95-99.7
#
# Trong phân phối chuẩn:
# - 68% dữ liệu nằm trong μ ± 1σ
# - 95% dữ liệu nằm trong μ ± 2σ
# - 99.7% dữ liệu nằm trong μ ± 3σ

mu <- 100
sigma <- 15

cat("\n=== QUY TẮC 68-95-99.7 ===\n")

# Tính xác suất trong các khoảng
prob_1sd <- pnorm(mu + sigma, mu, sigma) - pnorm(mu - sigma, mu, sigma)
prob_2sd <- pnorm(mu + 2*sigma, mu, sigma) - pnorm(mu - 2*sigma, mu, sigma)
prob_3sd <- pnorm(mu + 3*sigma, mu, sigma) - pnorm(mu - 3*sigma, mu, sigma)

cat("P(μ ± 1σ) =", round(prob_1sd, 4), "\n")  # 0.6827
cat("P(μ ± 2σ) =", round(prob_2sd, 4), "\n")  # 0.9545
cat("P(μ ± 3σ) =", round(prob_3sd, 4), "\n")  # 0.9973

# 12.4.5 Ví dụ tính xác suất
#
# Ví dụ: Điểm thi có phân phối chuẩn với μ = 70, σ = 10

cat("\n=== VÍ DỤ: ĐIỂM THI ===\n")

mu <- 70
sigma <- 10

# a) Xác suất sinh viên đạt trên 80 điểm?
# P(X > 80)
prob_above_80 <- 1 - pnorm(80, mu, sigma)
cat("a) P(X > 80) =", round(prob_above_80, 4), "\n")

# b) Xác suất đạt từ 60-80 điểm?
# P(60 ≤ X ≤ 80)
prob_60_80 <- pnorm(80, mu, sigma) - pnorm(60, mu, sigma)
cat("b) P(60 ≤ X ≤ 80) =", round(prob_60_80, 4), "\n")

# c) Điểm tối thiểu để vào top 10%?
# Tìm x sao cho P(X ≥ x) = 0.1
# Tức P(X ≤ x) = 0.9
cutoff <- qnorm(0.9, mu, sigma)
cat("c) Điểm tối thiểu:", round(cutoff, 2), "\n")

# 12.4.6 Sử dụng Z-score

cat("\n=== SỬ DỤNG Z-SCORE ===\n")

# Ví dụ: IQ ~ N(100, 15²)
mu <- 100
sigma <- 15

# Câu hỏi: Xác suất IQ > 130?

# Cách 1: Dùng trực tiếp
prob1 <- 1 - pnorm(130, mu, sigma)

# Cách 2: Chuyển sang Z-score
z <- (130 - mu) / sigma
prob2 <- 1 - pnorm(z, 0, 1)

cat("Cách 1:", round(prob1, 4), "\n")
cat("Cách 2:", round(prob2, 4), "\n")
cat("Hai cách cho kết quả giống nhau!\n")

# 12.4.7 Vẽ đường cong chuẩn

mu <- 100
sigma <- 15

# Tạo dữ liệu
x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 200)
y <- dnorm(x, mu, sigma)

# Vẽ đường cong
plot(x, y, type = "l", lwd = 2, col = "blue",
     main = "Phân phối chuẩn N(100, 15²)",
     xlab = "Giá trị",
     ylab = "Mật độ")

# Tô màu vùng dưới đường cong (ví dụ: P(X ≤ 100))
x_shade <- seq(mu - 4*sigma, mu, length.out = 100)
y_shade <- dnorm(x_shade, mu, sigma)
polygon(c(x_shade, rev(x_shade)), c(y_shade, rep(0, length(y_shade))),
        col = rgb(0, 0, 1, 0.3), border = NA)

# Thêm đường mean
abline(v = mu, col = "red", lwd = 2, lty = 2)
text(mu, max(y)*0.9, "μ = 100", col = "red")

# 12.4.8 Kiểm tra tính chuẩn (Normality Test)
#
# Q-Q Plot (Quantile-Quantile Plot) - Kiểm tra trực quan

cat("\n=== KIỂM TRA TÍNH CHUẨN ===\n")

# Tạo dữ liệu chuẩn
set.seed(42)
data_normal <- rnorm(100, 50, 10)

# Q-Q plot
qqnorm(data_normal, main = "Q-Q Plot - Dữ liệu chuẩn")
qqline(data_normal, col = "red", lwd = 2)

cat("Nếu các điểm nằm gần đường thẳng → dữ liệu có phân phối chuẩn\n")

# Shapiro-Wilk Test - Kiểm tra thống kê
#
# H0: Dữ liệu có phân phối chuẩn
# H1: Dữ liệu không có phân phối chuẩn

# Test với dữ liệu chuẩn
test_result <- shapiro.test(data_normal)
cat("\nShapiro-Wilk Test:\n")
cat("P-value:", test_result$p.value, "\n")

# Nếu p-value > 0.05 → Không bác bỏ H0 → Dữ liệu có phân phối chuẩn
if (test_result$p.value > 0.05) {
  cat("Kết luận: Dữ liệu có phân phối chuẩn\n")
} else {
  cat("Kết luận: Dữ liệu KHÔNG có phân phối chuẩn\n")
}

# ------------------------------------------------------------------------------
# 12.5 Định lý giới hạn trung tâm (Central Limit Theorem)
# ------------------------------------------------------------------------------

# 12.5.1 Định lý
#
# Định lý giới hạn trung tâm (CLT) phát biểu:
#
# Khi lấy mẫu đủ lớn (n ≥ 30) từ một tổng thể BẤT KỲ, 
# phân phối của TRUNG BÌNH MẪU sẽ tiến đến phân phối chuẩn.
#
# X̄ ~ N(μ, σ²/n)
#
# Trong đó:
# - μ: Trung bình tổng thể
# - σ²: Phương sai tổng thể
# - n: Kích thước mẫu

# 12.5.2 Minh họa CLT

cat("\n=== ĐỊNH LÝ GIỚI HẠN TRUNG TÂM ===\n")

# Minh họa CLT với phân phối đều (không chuẩn)
set.seed(42)

n_samples <- 1000
sample_size <- 30

# Tạo phân phối gốc (đều từ 0-10)
population <- runif(10000, 0, 10)

# Lấy mẫu và tính trung bình
sample_means <- replicate(n_samples, {
  sample_data <- sample(population, sample_size)
  mean(sample_data)
})

# Vẽ histogram của trung bình mẫu
hist(sample_means, breaks = 30,
     main = "Phân phối của trung bình mẫu (CLT)",
     xlab = "Trung bình mẫu",
     col = "lightblue",
     probability = TRUE)

# Thêm đường cong chuẩn
curve(dnorm(x, mean = mean(population), sd = sd(population)/sqrt(sample_size)),
      add = TRUE, col = "red", lwd = 2)

legend("topright", 
       legend = c("Histogram", "Phân phối chuẩn lý thuyết"),
       col = c("lightblue", "red"),
       lwd = c(10, 2))

cat("Phân phối gốc: Đều (không chuẩn)\n")
cat("Phân phối trung bình mẫu: Gần chuẩn!\n")

# 12.5.3 Ứng dụng CLT
#
# Ví dụ: Chiều cao sinh viên có μ = 165cm, σ = 8cm. Lấy mẫu 40 sinh viên.
# Xác suất trung bình mẫu > 167cm?

cat("\n=== ỨNG DỤNG CLT ===\n")

mu <- 165
sigma <- 8
n <- 40

# Theo CLT: X̄ ~ N(165, 8²/40)
mu_xbar <- mu
sigma_xbar <- sigma / sqrt(n)

cat("Trung bình mẫu ~ N(", mu_xbar, ",", round(sigma_xbar^2, 2), ")\n")

# P(X̄ > 167)
prob <- 1 - pnorm(167, mu_xbar, sigma_xbar)
cat("P(X̄ > 167) =", round(prob, 4), "\n")

# ------------------------------------------------------------------------------
# 12.6 Các phân phối khác
# ------------------------------------------------------------------------------

# 12.6.1 Phân phối Uniform (Đều)
#
# Mọi giá trị trong khoảng [a, b] có XÁC SUẤT NHƯ NHAU.

cat("\n=== PHÂN PHỐI UNIFORM ===\n")

# Uniform(0, 1)
x <- seq(0, 1, 0.01)
plot(x, dunif(x, 0, 1), type = "l",
     main = "Uniform Distribution (0, 1)",
     ylab = "Density",
     lwd = 2)

# Sinh số ngẫu nhiên
set.seed(42)
random_uniform <- runif(10, 0, 1)
cat("Random:", round(random_uniform, 3), "\n")

# 12.6.2 Phân phối Exponential (Mũ)
#
# Mô tả THỜI GIAN CHỜ giữa các sự kiện trong Poisson.

cat("\n=== PHÂN PHỐI EXPONENTIAL ===\n")

# Exponential(lambda = 2)
lambda <- 2

x <- seq(0, 5, 0.01)
plot(x, dexp(x, lambda), type = "l",
     main = "Exponential Distribution (λ = 2)",
     ylab = "Density",
     lwd = 2,
     col = "darkgreen")

# Ví dụ: Thời gian chờ bus
# Trung bình 10 phút = 1/lambda
mean_wait <- 1/lambda
cat("Thời gian chờ TB:", mean_wait, "đơn vị\n")

# ------------------------------------------------------------------------------
# 12.7 Tổng hợp và so sánh
# ------------------------------------------------------------------------------

# 12.7.1 Bảng so sánh các phân phối
#
# | Phân phối    | Loại      | Tham số | Ứng dụng              | Hàm R      |
# |--------------|-----------|---------|------------------------|------------|
# | Binomial     | Rời rạc   | n, p    | Số lần thành công     | dbinom()   |
# | Poisson      | Rời rạc   | λ       | Số sự kiện/thời gian  | dpois()    |
# | Normal       | Liên tục  | μ, σ    | Nhiều hiện tượng      | dnorm()    |
# | Uniform      | Liên tục  | a, b    | Số ngẫu nhiên         | dunif()    |
# | Exponential  | Liên tục  | λ       | Thời gian chờ         | dexp()     |

# 12.7.2 So sánh trực quan

cat("\n=== SO SÁNH CÁC PHÂN PHỐI ===\n")

# Thiết lập layout
par(mfrow = c(2, 2))

# 1. Binomial
x <- 0:20
plot(x, dbinom(x, 20, 0.5), type = "h", lwd = 2,
     main = "Binomial(20, 0.5)",
     xlab = "x", ylab = "P(X = x)")

# 2. Poisson
x <- 0:15
plot(x, dpois(x, 5), type = "h", lwd = 2, col = "coral",
     main = "Poisson(5)",
     xlab = "x", ylab = "P(X = x)")

# 3. Normal
x <- seq(-4, 4, 0.01)
plot(x, dnorm(x, 0, 1), type = "l", lwd = 2, col = "blue",
     main = "Normal(0, 1)",
     xlab = "x", ylab = "f(x)")

# 4. Exponential
x <- seq(0, 5, 0.01)
plot(x, dexp(x, 1), type = "l", lwd = 2, col = "darkgreen",
     main = "Exponential(1)",
     xlab = "x", ylab = "f(x)")

# Reset layout
par(mfrow = c(1, 1))

# ==============================================================================
# BÀI TẬP THỰC HÀNH
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("BÀI TẬP THỰC HÀNH\n")
cat(rep("=", 70), "\n", sep = "")

# ------------------------------------------------------------------------------
# Bài tập 1: Binomial
# ------------------------------------------------------------------------------

cat("\nBÀI TẬP 1: BINOMIAL\n")
cat("Một học sinh làm bài trắc nghiệm 20 câu, mỗi câu 4 đáp án.\n")
cat("Học sinh đoán ngẫu nhiên.\n\n")

cat("1. Xác suất trả lời đúng 5 câu?\n")
cat("2. Xác suất trả lời đúng ít nhất 10 câu?\n")
cat("3. Số câu đúng kỳ vọng?\n")

# Gợi ý:
# n <- 20
# p <- 0.25  # 1/4
# dbinom(5, n, p)
# 1 - pbinom(9, n, p)
# n * p

# ------------------------------------------------------------------------------
# Bài tập 2: Poisson
# ------------------------------------------------------------------------------

cat("\nBÀI TẬP 2: POISSON\n")
cat("Một cửa hàng nhận trung bình 12 khách hàng/giờ.\n\n")

cat("1. Xác suất có đúng 10 khách trong 1 giờ?\n")
cat("2. Xác suất có nhiều hơn 15 khách?\n")
cat("3. Xác suất có từ 10-15 khách?\n")

# ------------------------------------------------------------------------------
# Bài tập 3: Normal
# ------------------------------------------------------------------------------

cat("\nBÀI TẬP 3: NORMAL\n")
cat("Điểm thi có phân phối N(75, 10²).\n\n")

cat("1. Xác suất sinh viên đạt trên 85 điểm?\n")
cat("2. Xác suất đạt từ 65-85 điểm?\n")
cat("3. Điểm tối thiểu để vào top 20%?\n")
cat("4. Tính Z-score của điểm 90\n")

# ------------------------------------------------------------------------------
# Bài tập 4: CLT
# ------------------------------------------------------------------------------

cat("\nBÀI TẬP 4: CLT\n")
cat("Thời gian làm bài có μ = 45 phút, σ = 8 phút.\n")
cat("Lấy mẫu 36 sinh viên.\n\n")

cat("1. Xác suất trung bình thời gian > 47 phút?\n")
cat("2. Xác suất trung bình thời gian < 43 phút?\n")

# ==============================================================================
# CÂU HỎI ÔN TẬP
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("CÂU HỎI ÔN TẬP\n")
cat(rep("=", 70), "\n", sep = "")

cat("\n1. Phân biệt biến ngẫu nhiên rời rạc và liên tục?\n")
cat("2. Khi nào dùng Binomial? Khi nào dùng Poisson?\n")
cat("3. Giải thích quy tắc 68-95-99.7?\n")
cat("4. Z-score là gì? Cách tính?\n")
cat("5. Định lý giới hạn trung tâm nói gì?\n")
cat("6. Phân phối nào có Mean = Variance?\n")
cat("7. Tại sao phân phối chuẩn quan trọng?\n")

# ==============================================================================
# TÀI LIỆU THAM KHẢO
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("TÀI LIỆU THAM KHẢO\n")
cat(rep("=", 70), "\n", sep = "")

cat("\n1. R Documentation: ?dbinom, ?dpois, ?dnorm\n")
cat("2. Probability Distributions: https://www.stat.umn.edu/geyer/old/5101/rlook.html\n")
cat("3. Central Limit Theorem: Khan Academy\n")

# ==============================================================================
# TỔNG KẾT
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("TỔNG KẾT\n")
cat(rep("=", 70), "\n", sep = "")

cat("\nCông thức tổng hợp:\n\n")

cat("BINOMIAL:\n")
cat("- P(X = k) = C(n,k) × p^k × (1-p)^(n-k)\n")
cat("- E(X) = np, Var(X) = np(1-p)\n\n")

cat("POISSON:\n")
cat("- P(X = k) = (e^(-λ) × λ^k) / k!\n")
cat("- E(X) = Var(X) = λ\n\n")

cat("NORMAL:\n")
cat("- X ~ N(μ, σ²)\n")
cat("- Z = (X - μ) / σ\n")
cat("- Quy tắc 68-95-99.7\n\n")

cat("Hàm R quan trọng:\n")
cat("- d: Density/Mass (PDF/PMF)\n")
cat("- p: Probability (CDF)\n")
cat("- q: Quantile\n")
cat("- r: Random\n\n")

cat("Lưu ý:\n")
cat("✅ Binomial: n lần thử, 2 kết quả\n")
cat("✅ Poisson: Sự kiện hiếm, λ = mean\n")
cat("✅ Normal: Đối xứng, quy tắc 68-95-99.7\n")
cat("✅ CLT: n ≥ 30 → X̄ ~ Normal\n")
cat("✅ Z-score: Chuẩn hóa dữ liệu\n")

# Cập nhật: Tháng 3/2026