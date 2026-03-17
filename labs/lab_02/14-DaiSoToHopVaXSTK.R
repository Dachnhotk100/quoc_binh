# ==============================================================================
# BÀI 10: ĐẠI SỐ TỔ HỢP VÀ XÁC SUẤT CƠ BẢN
# ==============================================================================
# Mục tiêu học tập:
# - Hiểu và tính được giai thừa, hoán vị, tổ hợp
# - Phân biệt được khi nào dùng hoán vị, khi nào dùng tổ hợp
# - Áp dụng đại số tổ hợp vào bài toán xác suất thực tế
# - Sử dụng R để tính toán các bài toán tổ hợp
# - Giải quyết được các bài toán đếm trong thực tế
# ==============================================================================

# ------------------------------------------------------------------------------
# 10.1 Giai thừa (Factorial)
# ------------------------------------------------------------------------------

# 10.1.1 Giai thừa là gì?
#
# Giai thừa của một số nguyên dương n, ký hiệu là n!, là tích của tất cả 
# các số nguyên dương từ 1 đến n.
#
# Công thức:
# n! = 1 × 2 × 3 × ... × n
#
# Quy ước đặc biệt:
# 0! = 1  (theo quy ước)

# 10.1.2 Ví dụ tính giai thừa
#
# Ví dụ 1: Tính 5!
# 5! = 1 × 2 × 3 × 4 × 5 = 120
#
# Ví dụ 2: Tính 3!
# 3! = 1 × 2 × 3 = 6
#
# Ví dụ 3: Tính 0!
# 0! = 1  (theo quy ước)

# 10.1.3 Tính giai thừa trong R

# Hàm factorial() có sẵn trong R

# Tính 5!
factorial(5)  # Kết quả: 120

# Tính 0!
factorial(0)  # Kết quả: 1

# Tính 10!
factorial(10)  # Kết quả: 3,628,800

# Tính giai thừa cho nhiều số cùng lúc
n <- 1:10
factorial(n)
# Kết quả: 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800

# 10.1.4 Ứng dụng của giai thừa
#
# Câu hỏi: Có bao nhiêu cách sắp xếp 5 quyển sách khác nhau trên kệ?
#
# Giải thích:
# - Vị trí thứ 1: có 5 lựa chọn
# - Vị trí thứ 2: có 4 lựa chọn (đã dùng 1 quyển)
# - Vị trí thứ 3: có 3 lựa chọn
# - Vị trí thứ 4: có 2 lựa chọn
# - Vị trí thứ 5: có 1 lựa chọn
#
# Đáp án: 5 × 4 × 3 × 2 × 1 = 5! = 120 cách

factorial(5)  # 120

# 10.1.5 Viết hàm tính giai thừa

# Tự viết hàm tính giai thừa (để hiểu logic)
my_factorial <- function(n) {
  if (n < 0) {
    return("Không tính được giai thừa của số âm")
  }
  if (n == 0 || n == 1) {
    return(1)
  }
  
  result <- 1
  for (i in 2:n) {
    result <- result * i
  }
  
  return(result)
}

# Test
my_factorial(5)   # 120
my_factorial(0)   # 1
my_factorial(10)  # 3628800

# ------------------------------------------------------------------------------
# 10.2 Hoán vị (Permutation)
# ------------------------------------------------------------------------------

# 10.2.1 Hoán vị là gì?
#
# Hoán vị là cách sắp xếp có thứ tự của các phần tử.
#
# Đặc điểm:
# - CÓ QUAN TÂM ĐẾN THỨ TỰ
# - Ví dụ: ABC khác CBA
#
# Ký hiệu: P(n, r) hoặc A(n, r)
#
# Công thức:
# P(n, r) = n! / (n - r)!
#
# Trong đó:
# - n: tổng số phần tử
# - r: số phần tử được chọn

# 10.2.2 Giải thích công thức
#
# P(n, r) = n! / (n - r)!
#
# - Vị trí thứ 1: có n lựa chọn
# - Vị trí thứ 2: có (n-1) lựa chọn
# - Vị trí thứ 3: có (n-2) lựa chọn
# - ...
# - Vị trí thứ r: có (n-r+1) lựa chọn
#
# Tổng cộng: n × (n-1) × (n-2) × ... × (n-r+1)
#
# Ví dụ: P(5, 3) = 5 × 4 × 3 = 60

# 10.2.3 Ví dụ về hoán vị
#
# Ví dụ 1: Có 5 ghế và 3 người. Hỏi có bao nhiêu cách xếp 3 người vào 5 ghế?
#
# Phân tích:
# - Ghế thứ 1: 3 người
# - Ghế thứ 2: 2 người (1 người đã ngồi)
# - Ghế thứ 3: 1 người
# - Tổng: 3 × 2 × 1 = 6 cách
#
# Nhưng có 5 ghế để chọn!
# - Chọn ghế thứ 1: có 5 cách
# - Sau khi chọn ghế 1, chọn ghế 2: có 4 cách
# - Sau khi chọn ghế 1 và 2, chọn ghế 3: có 3 cách
#
# Công thức:
# P(5, 3) = 5! / (5-3)! = 5! / 2! = 120 / 2 = 60
#
# Ví dụ 2: Có bao nhiêu cách xếp 4 quyển sách khác nhau từ 10 quyển?
# P(10, 4) = 10! / (10-4)! = 10! / 6! = 5040

# 10.2.4 Tính hoán vị trong R

# Viết hàm tính hoán vị
permutation <- function(n, r) {
  factorial(n) / factorial(n - r)
}

# Ví dụ 1: Xếp 3 người vào 5 ghế
permutation(5, 3)  # 60

# Ví dụ 2: Xếp 4 quyển sách từ 10 quyển
permutation(10, 4)  # 5040

# Ví dụ 3: Xếp 3 chữ số từ 0-9
permutation(10, 3)  # 720

# 10.2.5 Bài tập thực hành
#
# Bài 1: Một lớp có 30 học sinh. Giáo viên muốn chọn 3 học sinh làm 
# lớp trưởng, lớp phó, thủ quỹ. Hỏi có bao nhiêu cách chọn?

# Có thứ tự: lớp trưởng khác lớp phó
permutation(30, 3)  # 24,360 cách

# Bài 2: Có bao nhiêu số có 4 chữ số khác nhau được tạo từ các chữ số 
# 1, 2, 3, 4, 5, 6?

permutation(6, 4)  # 360 số

# ------------------------------------------------------------------------------
# 10.3 Tổ hợp (Combination)
# ------------------------------------------------------------------------------

# 10.3.1 Tổ hợp là gì?
#
# Tổ hợp là cách chọn các phần tử KHÔNG QUAN TÂM ĐẾN THỨ TỰ.
#
# Đặc điểm:
# - KHÔNG QUAN TÂM ĐẾN THỨ TỰ
# - Ví dụ: {A, B, C} = {C, B, A}
#
# Ký hiệu: C(n, r) hoặc "n chọn r"
#
# Công thức:
# C(n, r) = n! / (r! × (n - r)!)
#
# Trong đó:
# - n: tổng số phần tử
# - r: số phần tử được chọn

# 10.3.2 Phân biệt Hoán vị và Tổ hợp
#
# | Tiêu chí    | Hoán vị (P)         | Tổ hợp (C)          |
# |-------------|---------------------|---------------------|
# | Thứ tự      | CÓ quan tâm         | KHÔNG quan tâm      |
# | Ví dụ       | ABC ≠ CBA           | {A,B,C} = {C,B,A}   |
# | Công thức   | n!/(n-r)!           | n!/(r!(n-r)!)       |
# | Khi nào     | Sắp xếp, xếp hàng   | Chọn, tập hợp       |
#
# Mẹo nhớ:
# - P: Permutation = Position matters (vị trí quan trọng)
# - C: Combination = Choice only (chỉ chọn thôi)

# 10.3.3 Ví dụ về tổ hợp
#
# Ví dụ 1: Từ 5 học sinh, chọn 3 học sinh vào đội thi. 
# Hỏi có bao nhiêu cách chọn?
#
# Phân tích:
# - Không quan tâm thứ tự
# - Chọn A, B, C = Chọn C, B, A
# - Dùng tổ hợp
#
# C(5, 3) = 5! / (3! × 2!) = 120 / (6 × 2) = 10 cách
#
# Ví dụ 2: Trong một bộ bài 52 lá, có bao nhiêu cách rút 5 lá bài?
# C(52, 5) = 52! / (5! × 47!) = 2,598,960 cách

# 10.3.4 Tính tổ hợp trong R

# Hàm choose() có sẵn trong R

# Ví dụ 1: Chọn 3 học sinh từ 5 học sinh
choose(5, 3)  # 10

# Ví dụ 2: Chọn 5 lá bài từ 52 lá
choose(52, 5)  # 2,598,960

# Ví dụ 3: Chọn 3 câu hỏi từ 20 câu
choose(20, 3)  # 1,140

# Tính tổ hợp cho nhiều giá trị
n <- 10
r <- 0:10
results <- data.frame(n = n, r = r, combinations = choose(n, r))
print(results)

# 10.3.5 So sánh Hoán vị và Tổ hợp

# Ví dụ: Từ 5 phần tử, chọn 3 phần tử

# Hoán vị (có thứ tự)
permutation(5, 3)  # 60

# Tổ hợp (không thứ tự)
choose(5, 3)       # 10

# Mối quan hệ: P(n,r) = C(n,r) × r!
permutation(5, 3) == choose(5, 3) * factorial(3)  # TRUE
# 60 = 10 × 6

# 10.3.6 Bài tập thực hành
#
# Bài 1: Một lớp có 30 học sinh. Chọn 5 học sinh để tham gia hoạt động 
# ngoại khóa. Hỏi có bao nhiêu cách chọn?

choose(30, 5)  # 142,506 cách

# Bài 2: Trong một đề thi có 10 câu, sinh viên phải chọn 6 câu để làm. 
# Hỏi có bao nhiêu cách chọn?

choose(10, 6)  # 210 cách

# ------------------------------------------------------------------------------
# 10.4 So sánh và Lựa chọn
# ------------------------------------------------------------------------------

# 10.4.1 Bảng so sánh
#
# | Tình huống                      | Công thức          | Ví dụ                  |
# |---------------------------------|--------------------|------------------------|
# | Sắp xếp tất cả                  | n!                 | Xếp 5 người vào 5 ghế  |
# | Sắp xếp một phần (có thứ tự)    | P(n,r) = n!/(n-r)! | Xếp 3 người vào 5 ghế  |
# | Chọn (không thứ tự)             | C(n,r)             | Chọn 3 người từ 5      |

# 10.4.2 Cách nhận biết
#
# Câu hỏi có từ khóa:
# - "Sắp xếp", "Xếp hàng", "Thứ tự" → Dùng Hoán vị (P)
# - "Chọn", "Lấy", "Tập hợp" → Dùng Tổ hợp (C)

# 10.4.3 Ví dụ nhận biết
#
# Ví dụ 1: "Có bao nhiêu cách CHỌN 3 học sinh từ 10 học sinh?"
# - Từ khóa: "chọn"
# - Đáp án:
choose(10, 3)  # 120

# Ví dụ 2: "Có bao nhiêu cách XẾP 3 học sinh vào 3 vị trí: 
# lớp trưởng, lớp phó, thư ký?"
# - Từ khóa: "xếp", "vị trí"
# - Đáp án:
permutation(10, 3)  # 720

# Ví dụ 3: "Có bao nhiêu cách SẮP XẾP 5 quyển sách?"
# - Từ khóa: "sắp xếp"
# - Đáp án:
factorial(5)  # 120

# ------------------------------------------------------------------------------
# 10.5 Ứng dụng trong Xác suất
# ------------------------------------------------------------------------------

# 10.5.1 Xác suất là gì?
#
# Xác suất là khả năng xảy ra của một sự kiện.
#
# Công thức:
# P(A) = Số kết quả thuận lợi / Tổng số kết quả có thể
#
# Giá trị: 0 ≤ P(A) ≤ 1
# - P(A) = 0: Sự kiện không thể xảy ra
# - P(A) = 1: Sự kiện chắc chắn xảy ra
# - 0 < P(A) < 1: Sự kiện có thể xảy ra

# 10.5.2 Ví dụ về xác suất

# Ví dụ 1: Tung đồng xu
# Mặt ngửa hoặc mặt sấp
# P(Ngửa) = 1/2 = 0.5

prob_heads <- 1/2
cat("Xác suất mặt ngửa:", prob_heads, "\n")

# Ví dụ 2: Gieo xúc xắc
# 6 mặt: 1, 2, 3, 4, 5, 6
# P(ra số 6) = 1/6

prob_six <- 1/6
cat("Xác suất ra số 6:", prob_six, "\n")
cat("Xác suất ra số 6:", round(prob_six, 3), "\n")  # 0.167

# 10.5.3 Ứng dụng tổ hợp trong xác suất

# Ví dụ 1: Xác suất rút 4 quân Át từ bộ bài 52 lá
#
# Phân tích:
# - Tổng số cách chọn 4 lá: C(52, 4)
# - Số cách chọn 4 quân Át: C(4, 4) = 1
# - Xác suất = 1 / C(52, 4)

# Tổng số cách chọn 4 lá
total_ways <- choose(52, 4)
cat("Tổng số cách chọn 4 lá:", total_ways, "\n")

# Số cách chọn 4 quân Át
ace_ways <- choose(4, 4)
cat("Số cách chọn 4 Át:", ace_ways, "\n")

# Xác suất
prob_4_aces <- ace_ways / total_ways
cat("Xác suất rút 4 quân Át:", prob_4_aces, "\n")
cat("Xác suất:", format(prob_4_aces, scientific = FALSE), "\n")
# Rất nhỏ: 0.000003694...

# Ví dụ 2: Xác suất có đúng 2 quân Át trong 5 lá bài
#
# Phân tích:
# - Chọn 2 Át từ 4 quân Át: C(4, 2)
# - Chọn 3 lá khác từ 48 lá còn lại: C(48, 3)
# - Tổng số cách: C(52, 5)

# Số cách chọn 2 Át và 3 lá khác
ways_2_aces <- choose(4, 2) * choose(48, 3)
cat("Số cách chọn 2 Át và 3 lá khác:", ways_2_aces, "\n")

# Tổng số cách chọn 5 lá
total_ways_5 <- choose(52, 5)
cat("Tổng số cách chọn 5 lá:", total_ways_5, "\n")

# Xác suất
prob_2_aces <- ways_2_aces / total_ways_5
cat("Xác suất có đúng 2 quân Át:", prob_2_aces, "\n")
cat("Xác suất:", round(prob_2_aces, 4), "\n")  # 0.0399

# Ví dụ 3: Xổ số - Chọn 6 số từ 45 số

# Tổng số cách chọn 6 số từ 45 số
total_combinations <- choose(45, 6)
cat("Tổng số tổ hợp:", total_combinations, "\n")

# Xác suất trúng jackpot (chọn đúng 6 số)
prob_jackpot <- 1 / total_combinations
cat("Xác suất trúng jackpot:", prob_jackpot, "\n")
cat("Xác suất:", format(prob_jackpot, scientific = FALSE), "\n")
# 0.00000012...

# Tỷ lệ: 1 trên bao nhiêu?
cat("Tỷ lệ: 1 trên", total_combinations, "\n")
# 1 trên 8,145,060

# 10.5.4 Ví dụ thực tế khác

# Ví dụ 4: Một lớp có 30 sinh viên, trong đó có 12 nữ. 
# Chọn ngẫu nhiên 5 sinh viên. Tính xác suất có đúng 3 sinh viên nữ.

# Tổng số cách chọn 5 sinh viên từ 30
total <- choose(30, 5)

# Số cách chọn 3 nữ từ 12 nữ
female_ways <- choose(12, 3)

# Số cách chọn 2 nam từ 18 nam
male_ways <- choose(18, 2)

# Số cách chọn 3 nữ và 2 nam
favorable <- female_ways * male_ways

# Xác suất
prob <- favorable / total
cat("Xác suất có đúng 3 nữ:", round(prob, 4), "\n")

# ------------------------------------------------------------------------------
# 10.6 Tam giác Pascal
# ------------------------------------------------------------------------------

# 10.6.1 Tam giác Pascal là gì?
#
# Tam giác Pascal là một cách sắp xếp các số tổ hợp thành hình tam giác.
#
# Đặc điểm:
# - Hàng thứ n chứa các giá trị C(n, 0), C(n, 1), ..., C(n, n)
# - Mỗi số = tổng 2 số ở hàng trên
#
# Ví dụ:
#          1              (hàng 0)
#        1   1            (hàng 1)
#      1   2   1          (hàng 2)
#    1   3   3   1        (hàng 3)
#  1   4   6   4   1      (hàng 4)

# 10.6.2 Vẽ tam giác Pascal trong R

# Hàm vẽ tam giác Pascal
pascal_triangle <- function(n) {
  for (i in 0:n) {
    # In khoảng trắng để căn giữa
    cat(rep(" ", n - i), sep = "")
    
    # In các giá trị tổ hợp
    for (j in 0:i) {
      cat(choose(i, j), " ")
    }
    cat("\n")
  }
}

# Vẽ tam giác Pascal với 6 hàng
pascal_triangle(6)

# 10.6.3 Tính chất của tam giác Pascal

# Tính chất 1: Tổng các số trong hàng n = 2^n
n <- 5
row_sum <- sum(choose(n, 0:n))
cat("Tổng hàng", n, ":", row_sum, "\n")
cat("2^", n, "=", 2^n, "\n")
# Tổng hàng 5: 32
# 2^5 = 32

# Tính chất 2: Đối xứng: C(n, r) = C(n, n-r)
n <- 10
r <- 3
cat("C(10, 3) =", choose(n, r), "\n")
cat("C(10, 7) =", choose(n, n-r), "\n")
# Cả hai đều = 120

# ------------------------------------------------------------------------------
# 10.7 Bài tập nâng cao
# ------------------------------------------------------------------------------

# Bài tập 1: Chọn đội bóng
#
# Một câu lạc bộ có 20 thành viên, trong đó có 8 nữ. Cần chọn 1 đội bóng 
# chuyền gồm 6 người, trong đó phải có ít nhất 2 nữ. Hỏi có bao nhiêu cách chọn?

# Giải:
# Ít nhất 2 nữ = 2 nữ + 3 nữ + 4 nữ + 5 nữ + 6 nữ

# Cách 1: Tính từng trường hợp
case_2f <- choose(8, 2) * choose(12, 4)  # 2 nữ, 4 nam
case_3f <- choose(8, 3) * choose(12, 3)  # 3 nữ, 3 nam
case_4f <- choose(8, 4) * choose(12, 2)  # 4 nữ, 2 nam
case_5f <- choose(8, 5) * choose(12, 1)  # 5 nữ, 1 nam
case_6f <- choose(8, 6) * choose(12, 0)  # 6 nữ, 0 nam

total_ways <- case_2f + case_3f + case_4f + case_5f + case_6f
cat("Tổng số cách:", total_ways, "\n")

# Cách 2: Tổng - Bù
# Tổng các cách - (0 nữ + 1 nữ)
total_all <- choose(20, 6)
case_0f <- choose(8, 0) * choose(12, 6)
case_1f <- choose(8, 1) * choose(12, 5)
total_ways_2 <- total_all - case_0f - case_1f
cat("Tổng số cách (cách 2):", total_ways_2, "\n")

# Bài tập 2: Mật khẩu
#
# Có bao nhiêu mật khẩu dài 6 ký tự, sử dụng 26 chữ cái 
# (không phân biệt hoa thường), trong đó:
# - Không có ký tự nào lặp lại
# - Phải có ít nhất 2 nguyên âm (a, e, i, o, u)

# Đây là bài toán phức tạp hơn
# Sinh viên cần phân tích từng bước

# Gợi ý:
# - Số nguyên âm: 5
# - Số phụ âm: 21
# - Không lặp → Hoán vị
# - Ít nhất 2 nguyên âm → Tính nhiều trường hợp

# Bài tập 3: Chia nhóm
#
# Một lớp có 24 học sinh. Giáo viên muốn chia thành 3 nhóm: 
# Nhóm A (8 người), Nhóm B (8 người), Nhóm C (8 người). 
# Hỏi có bao nhiêu cách chia?

# Lưu ý: Các nhóm có thể hoán đổi cho nhau
# Công thức: C(24,8) × C(16,8) × C(8,8) / 3!

ways <- choose(24, 8) * choose(16, 8) * choose(8, 8)
# Chia cho 3! vì 3 nhóm hoán đổi cho nhau
total <- ways / factorial(3)
cat("Số cách chia:", total, "\n")

# ==============================================================================
# BÀI TẬP THỰC HÀNH
# ==============================================================================

# ------------------------------------------------------------------------------
# Bài tập 1: Giai thừa cơ bản
# ------------------------------------------------------------------------------

# 1. Tính các giai thừa sau:
#    - 6!
#    - 8!
#    - 12!

# 2. Tính: 10! / 7!

# 3. Tính: (n+2)! / n! với n = 5

# Gợi ý câu 2:
factorial(10) / factorial(7)
# Hoặc đơn giản hóa: 10!/7! = 10 × 9 × 8

# ------------------------------------------------------------------------------
# Bài tập 2: Phân biệt Hoán vị và Tổ hợp
# ------------------------------------------------------------------------------

# Cho biết các bài toán sau dùng Hoán vị hay Tổ hợp, sau đó tính:
#
# 1. Có bao nhiêu cách chọn 3 học sinh từ 15 học sinh để tham gia Olympic?
# 2. Có bao nhiêu cách xếp 3 học sinh vào 3 vị trí: Lớp trưởng, Lớp phó, Bí thư?
# 3. Có bao nhiêu cách chọn 5 câu hỏi từ 20 câu trong đề thi?
# 4. Có bao nhiêu cách sắp xếp 7 quyển sách trên kệ?

# ------------------------------------------------------------------------------
# Bài tập 3: Bài toán đếm
# ------------------------------------------------------------------------------

# 1. Có bao nhiêu số có 4 chữ số khác nhau được tạo từ 1, 2, 3, 4, 5, 6, 7?

# 2. Có bao nhiêu cách chia 12 học sinh thành 3 nhóm, mỗi nhóm 4 người?

# 3. Một đội bóng đá có 15 cầu thủ. Huấn luyện viên cần chọn:
#    - 1 thủ môn từ 2 thủ môn
#    - 4 hậu vệ từ 6 hậu vệ
#    - 4 tiền vệ từ 5 tiền vệ
#    - 2 tiền đạo từ 2 tiền đạo
#    Hỏi có bao nhiêu cách chọn đội hình?

# ------------------------------------------------------------------------------
# Bài tập 4: Xác suất
# ------------------------------------------------------------------------------

# 1. Rút ngẫu nhiên 3 lá bài từ bộ bài 52 lá. Tính xác suất:
#    - Cả 3 lá đều là át
#    - Có đúng 2 lá át
#    - Có ít nhất 1 lá át

# 2. Một hộp có 10 viên bi, trong đó 6 viên đỏ và 4 viên xanh. 
#    Lấy ngẫu nhiên 3 viên. Tính xác suất:
#    - Cả 3 viên đều đỏ
#    - Có đúng 2 viên đỏ
#    - Có ít nhất 1 viên đỏ

# 3. Một lớp có 25 sinh viên, trong đó 10 nữ. 
#    Chọn ngẫu nhiên 4 sinh viên. Tính xác suất:
#    - Cả 4 đều là nữ
#    - Có đúng 2 nữ
#    - Có ít nhất 1 nữ

# ------------------------------------------------------------------------------
# Bài tập 5: Ứng dụng thực tế
# ------------------------------------------------------------------------------

# 1. Xổ số: Trong xổ số Mega, người chơi chọn 6 số từ 1-45. Tính:
#    - Tổng số tổ hợp có thể
#    - Xác suất trúng jackpot (đúng cả 6 số)
#    - Xác suất trúng giải 2 (đúng 5 số)

# 2. Mật khẩu: Có bao nhiêu mật khẩu dài 8 ký tự, gồm:
#    - Chỉ dùng chữ cái (a-z, A-Z)?
#    - Chỉ dùng chữ số (0-9)?
#    - Dùng cả chữ và số, không lặp?

# 3. Chia nhóm: Một công ty có 20 nhân viên cần chia thành:
#    - 2 nhóm, mỗi nhóm 10 người
#    - 4 nhóm, mỗi nhóm 5 người
#    - Hỏi cách chia nào có nhiều khả năng hơn?

# ==============================================================================
# CÂU HỎI ÔN TẬP
# ==============================================================================

# 1. Giải thích sự khác biệt giữa Hoán vị và Tổ hợp?
# 2. Khi nào dùng công thức n!, P(n,r), C(n,r)?
# 3. Tại sao C(n,r) = C(n, n-r)?
# 4. Giải thích ý nghĩa của tam giác Pascal?
# 5. Trong xác suất, tổ hợp được dùng như thế nào?
# 6. Cho ví dụ thực tế về hoán vị và tổ hợp?
# 7. Tính P(10, 3) và C(10, 3), giải thích tại sao khác nhau?
# 8. Nêu mối quan hệ giữa P(n,r) và C(n,r)?

# ==============================================================================
# TÀI LIỆU THAM KHẢO
# ==============================================================================

# 1. R Documentation: ?factorial, ?choose
# 2. Toán học rời rạc: Giáo trình Đại học
# 3. Xác suất thống kê: Giáo trình cơ bản
# 4. Wolfram MathWorld: http://mathworld.wolfram.com/

# ==============================================================================
# TỔNG KẾT
# ==============================================================================

# Những điểm cần nhớ:
#
# 1. ✅ Giai thừa (n!): Sắp xếp tất cả n phần tử
#    - Công thức: n! = 1 × 2 × 3 × ... × n
#    - Quy ước: 0! = 1
#
# 2. ✅ Hoán vị P(n,r): Sắp xếp có thứ tự
#    - Công thức: P(n,r) = n! / (n-r)!
#    - Có quan tâm thứ tự
#    - ABC ≠ CBA
#
# 3. ✅ Tổ hợp C(n,r): Chọn không thứ tự
#    - Công thức: C(n,r) = n! / (r!(n-r)!)
#    - Không quan tâm thứ tự
#    - {A,B,C} = {C,B,A}
#
# 4. ✅ Cách nhận biết:
#    - "Sắp xếp", "Xếp hàng" → Hoán vị
#    - "Chọn", "Lấy" → Tổ hợp
#
# 5. ✅ Trong xác suất:
#    - P(A) = Số trường hợp thuận lợi / Tổng số trường hợp
#    - Thường dùng tổ hợp để đếm
#
# 6. ✅ Tam giác Pascal:
#    - Hàng n chứa C(n,0), C(n,1), ..., C(n,n)
#    - Tổng hàng n = 2^n

# Công thức quan trọng:
#
# Giai thừa:     n! = 1 × 2 × 3 × ... × n
# Hoán vị:       P(n,r) = n! / (n-r)!
# Tổ hợp:        C(n,r) = n! / (r! × (n-r)!)
# Mối quan hệ:   P(n,r) = C(n,r) × r!
# Xác suất:      P(A) = |A| / |Ω|

# Lưu ý quan trọng:
# - LUÔN phân tích kỹ đề bài trước khi chọn công thức
# - KIỂM TRA xem có quan tâm thứ tự hay không
# - VẼ SƠ ĐỒ hoặc liệt kê các trường hợp nhỏ để hiểu bài toán
# - SỬ DỤNG R để tính toán và kiểm tra kết quả
# - THỰC HÀNH nhiều bài tập để thành thạo

# Cập nhật: Tháng 3/2026