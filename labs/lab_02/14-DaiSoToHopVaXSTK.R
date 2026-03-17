# ==============================================================================
# BÀI 10: ĐẠI SỐ TỔ HỢP VÀ XÁC SUẤT CƠ BẢN
# ==============================================================================
# Mục tiêu học tập:
# - Hiểu và tính được giai thừa, hoán vị, chỉnh hợp, tổ hợp
# - Phân biệt được hoán vị, chỉnh hợp và tổ hợp (có lặp và không lặp)
# - Nắm vững các quy tắc đếm cơ bản
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

# ------------------------------------------------------------------------------
# 10.2 Quy tắc đếm cơ bản
# ------------------------------------------------------------------------------

# 10.2.1 Quy tắc cộng (Addition Rule)
#
# Quy tắc: Nếu có m cách thực hiện việc A và n cách thực hiện việc B, 
# và hai việc KHÔNG THỂ XẢY RA ĐỒNG THỜI, thì có m + n cách thực hiện A hoặc B.
#
# Ví dụ: 
# - Trong lớp có 15 nam và 12 nữ
# - Chọn 1 học sinh làm lớp trưởng
# - Số cách chọn = 15 + 12 = 27 cách

# Quy tắc cộng
male <- 15
female <- 12
total_ways <- male + female
cat("Số cách chọn:", total_ways, "\n")  # 27

# 10.2.2 Quy tắc nhân (Multiplication Rule)
#
# Quy tắc: Nếu có m cách thực hiện việc A và n cách thực hiện việc B, 
# và hai việc ĐỘC LẬP, thì có m × n cách thực hiện A và B.
#
# Ví dụ: 
# - Có 3 áo và 4 quần
# - Số cách phối đồ = 3 × 4 = 12 cách

# Quy tắc nhân
shirts <- 3
pants <- 4
outfits <- shirts * pants
cat("Số cách phối đồ:", outfits, "\n")  # 12

# 10.2.3 Ví dụ tổng hợp
#
# Ví dụ: Mật khẩu gồm 1 chữ cái (A-Z) và 3 chữ số (0-9). 
# Hỏi có bao nhiêu mật khẩu?
#
# Giải:
# - Chữ cái: 26 cách
# - Chữ số thứ 1: 10 cách
# - Chữ số thứ 2: 10 cách
# - Chữ số thứ 3: 10 cách
# - Tổng: 26 × 10 × 10 × 10 = 26,000

passwords <- 26 * 10 * 10 * 10
cat("Số mật khẩu:", passwords, "\n")  # 26000

# ------------------------------------------------------------------------------
# 10.3 Hoán vị (Permutation)
# ------------------------------------------------------------------------------

# 10.3.1 Hoán vị không lặp
#
# Hoán vị là cách sắp xếp TẤT CẢ các phần tử theo thứ tự, 
# các phần tử KHÁC NHAU.
#
# Ký hiệu: P(n)
#
# Công thức:
# P(n) = n!

# Ví dụ: Có bao nhiêu cách sắp xếp 3 người A, B, C?
# P(3) = 3! = 6 cách
# ABC, ACB, BAC, BCA, CAB, CBA

# Hoán vị 3 người
factorial(3)  # 6

# 10.3.2 Hoán vị lặp (Permutation with Repetition)
#
# Hoán vị lặp là hoán vị khi có các phần tử GIỐNG NHAU.
#
# Công thức:
# P(n; n₁, n₂, ..., nₖ) = n! / (n₁! × n₂! × ... × nₖ!)
#
# Trong đó:
# - n: tổng số phần tử
# - n₁, n₂, ..., nₖ: số lần lặp của mỗi phần tử

# Ví dụ 1: Có bao nhiêu cách sắp xếp các chữ cái trong từ "BANANA"?
#
# Phân tích:
# - Tổng: 6 chữ cái
# - B: 1 lần
# - A: 3 lần
# - N: 2 lần
#
# P = 6! / (1! × 3! × 2!) = 720 / (1 × 6 × 2) = 60

# Hoán vị lặp cho BANANA
total_letters <- 6
b_count <- 1
a_count <- 3
n_count <- 2

ways <- factorial(total_letters) / (factorial(b_count) * factorial(a_count) * factorial(n_count))
cat("Số cách sắp xếp BANANA:", ways, "\n")  # 60

# Ví dụ 2: Có bao nhiêu cách sắp xếp 10 viên bi: 5 đỏ, 3 xanh, 2 vàng?

total_balls <- 10
red <- 5
blue <- 3
yellow <- 2

ways <- factorial(total_balls) / (factorial(red) * factorial(blue) * factorial(yellow))
cat("Số cách sắp xếp 10 viên bi:", ways, "\n")  # 2520

# Viết hàm tổng quát cho hoán vị lặp
hoan_vi_lap <- function(n, ...) {
  counts <- c(...)
  denominator <- prod(factorial(counts))
  return(factorial(n) / denominator)
}

# Test hàm
hoan_vi_lap(6, 1, 3, 2)  # BANANA: 60
hoan_vi_lap(10, 5, 3, 2)  # Bi: 2520

# ------------------------------------------------------------------------------
# 10.4 Chỉnh hợp (Arrangement)
# ------------------------------------------------------------------------------

# 10.4.1 Chỉnh hợp không lặp
#
# Chỉnh hợp là cách chọn và sắp xếp r phần tử từ n phần tử khác nhau, 
# CÓ QUAN TÂM THỨ TỰ.
#
# Ký hiệu: A(n, r)
#
# Công thức:
# A(n, r) = n! / (n - r)!

# Ví dụ: Chọn 3 người từ 5 người để làm Lớp trưởng, Lớp phó, Thư ký.
# A(5, 3) = 5! / 2! = 60

# Hàm tính chỉnh hợp
chinh_hop <- function(n, r) {
  factorial(n) / factorial(n - r)
}

chinh_hop(5, 3)  # 60

# 10.4.2 Chỉnh hợp lặp (Arrangement with Repetition)
#
# Chỉnh hợp lặp là chọn r phần tử từ n phần tử, 
# mỗi phần tử CÓ THỂ CHỌN NHIỀU LẦN.
#
# Công thức:
# A'(n, r) = nʳ

# Ví dụ 1: Mật khẩu gồm 4 chữ số (0-9), mỗi chữ số có thể lặp lại. 
# Có bao nhiêu mật khẩu?
#
# A'(10, 4) = 10⁴ = 10,000

# Chỉnh hợp lặp
n <- 10  # Chữ số 0-9
r <- 4   # 4 vị trí
ways <- n^r
cat("Số mật khẩu 4 chữ số:", ways, "\n")  # 10000

# Ví dụ 2: Có bao nhiêu số có 3 chữ số (từ 0-9, có thể lặp)?
# Từ 000 đến 999

ways <- 10^3
cat("Số có 3 chữ số (kể cả 000):", ways, "\n")  # 1000

# So sánh chỉnh hợp và chỉnh hợp lặp

# Không lặp (các chữ số khác nhau)
cat("Không lặp:", chinh_hop(10, 3), "\n")  # 720

# Có lặp (các chữ số có thể giống nhau)
cat("Có lặp:", 10^3, "\n")  # 1000

# Hàm tính chỉnh hợp lặp
chinh_hop_lap <- function(n, r) {
  return(n^r)
}

chinh_hop_lap(10, 4)  # 10000

# ------------------------------------------------------------------------------
# 10.5 Tổ hợp (Combination)
# ------------------------------------------------------------------------------

# 10.5.1 Tổ hợp không lặp
#
# Tổ hợp là cách chọn r phần tử từ n phần tử, 
# KHÔNG QUAN TÂM THỨ TỰ.
#
# Ký hiệu: C(n, r)
#
# Công thức:
# C(n, r) = n! / (r! × (n - r)!)

# Ví dụ: Chọn 3 học sinh từ 5 học sinh vào đội thi.
# C(5, 3) = 10

choose(5, 3)  # 10

# 10.5.2 Tổ hợp lặp (Combination with Repetition)
#
# Tổ hợp lặp là chọn r phần tử từ n phần tử, 
# mỗi phần tử CÓ THỂ CHỌN NHIỀU LẦN, không quan tâm thứ tự.
#
# Công thức:
# C'(n, r) = C(n + r - 1, r) = (n + r - 1)! / (r! × (n - 1)!)

# Ví dụ 1: Mua 5 quả táo từ 3 loại (táo đỏ, táo xanh, táo vàng). 
# Có bao nhiêu cách mua?
#
# Phân tích:
# - n = 3 (3 loại táo)
# - r = 5 (mua 5 quả)
# - Có thể mua cùng loại
#
# C'(3, 5) = C(3 + 5 - 1, 5) = C(7, 5) = 21

# Hàm tính tổ hợp lặp
to_hop_lap <- function(n, r) {
  choose(n + r - 1, r)
}

n <- 3  # 3 loại táo
r <- 5  # 5 quả
ways <- to_hop_lap(n, r)
cat("Số cách mua 5 táo từ 3 loại:", ways, "\n")  # 21

# Ví dụ 2: Có bao nhiêu cách chọn 4 viên kẹo từ 3 loại kẹo 
# (có thể chọn cùng loại)?

n <- 3  # 3 loại kẹo
r <- 4  # 4 viên
ways <- to_hop_lap(n, r)
cat("Số cách chọn 4 kẹo từ 3 loại:", ways, "\n")  # 15

# Giải thích bằng ví dụ cụ thể:
# - Loại A, B, C
# - Chọn 4 viên, ví dụ: AAAB, AABC, BBBC, ...
# - C'(3, 4) = C(6, 4) = 15 cách

# ------------------------------------------------------------------------------
# 10.6 Bảng tổng hợp
# ------------------------------------------------------------------------------

# 10.6.1 So sánh tất cả các loại
#
# | Loại              | Ký hiệu      | Công thức           | Thứ tự | Lặp    |
# |-------------------|--------------|---------------------|---------|--------|
# | Hoán vị           | P(n)         | n!                  | Có      | Không  |
# | Hoán vị lặp       | P(n;n₁,n₂)   | n!/(n₁!×n₂!...)     | Có      | Có     |
# | Chỉnh hợp         | A(n,r)       | n!/(n-r)!           | Có      | Không  |
# | Chỉnh hợp lặp     | A'(n,r)      | nʳ                  | Có      | Có     |
# | Tổ hợp            | C(n,r)       | n!/(r!(n-r)!)       | Không   | Không  |
# | Tổ hợp lặp        | C'(n,r)      | C(n+r-1,r)          | Không   | Có     |

# 10.6.2 Ví dụ so sánh với cùng n=5, r=3

cat("=== SO SÁNH CÁC LOẠI VỚI n=5, r=3 ===\n\n")

# Hoán vị (xếp tất cả 5)
cat("Hoán vị P(5):", factorial(5), "\n")

# Chỉnh hợp (chọn 3 từ 5, có thứ tự, không lặp)
cat("Chỉnh hợp A(5,3):", chinh_hop(5, 3), "\n")

# Chỉnh hợp lặp (chọn 3 từ 5, có thứ tự, có lặp)
cat("Chỉnh hợp lặp A'(5,3):", chinh_hop_lap(5, 3), "\n")

# Tổ hợp (chọn 3 từ 5, không thứ tự, không lặp)
cat("Tổ hợp C(5,3):", choose(5, 3), "\n")

# Tổ hợp lặp (chọn 3 từ 5, không thứ tự, có lặp)
cat("Tổ hợp lặp C'(5,3):", to_hop_lap(5, 3), "\n")

# ------------------------------------------------------------------------------
# 10.7 Ví dụ tổng hợp
# ------------------------------------------------------------------------------

# Ví dụ 1: Mật khẩu
#
# Câu hỏi: Mật khẩu gồm 6 ký tự từ a-z (26 chữ cái). 
# Tính số mật khẩu nếu:
# a) Các ký tự có thể lặp
# b) Các ký tự không được lặp

cat("\n=== VÍ DỤ 1: MẬT KHẨU ===\n")

# a) Có lặp - Chỉnh hợp lặp
passwords_with_rep <- 26^6
cat("a) Có lặp:", passwords_with_rep, "\n")

# b) Không lặp - Chỉnh hợp
passwords_no_rep <- chinh_hop(26, 6)
cat("b) Không lặp:", passwords_no_rep, "\n")

# Ví dụ 2: Xếp bi
#
# Câu hỏi: Có 8 viên bi: 3 đỏ, 3 xanh, 2 vàng. 
# Có bao nhiêu cách xếp thành hàng?

cat("\n=== VÍ DỤ 2: XẾP BI ===\n")

# Giải: Hoán vị lặp
total <- 8
red <- 3
blue <- 3
yellow <- 2

ways <- factorial(total) / (factorial(red) * factorial(blue) * factorial(yellow))
cat("Số cách xếp 8 viên bi:", ways, "\n")  # 560

# Ví dụ 3: Mua hoa quả
#
# Câu hỏi: Mua 6 quả từ 4 loại (cam, táo, chuối, nho). 
# Có bao nhiêu cách mua?

cat("\n=== VÍ DỤ 3: MUA HOA QUẢ ===\n")

# Giải: Tổ hợp lặp
n <- 4  # 4 loại
r <- 6  # 6 quả
ways <- to_hop_lap(n, r)
cat("Số cách mua 6 quả từ 4 loại:", ways, "\n")  # 84

# Ví dụ 4: Xếp sách
#
# Câu hỏi: 
# a) Xếp 5 quyển sách khác nhau
# b) Xếp 5 quyển sách trong đó có 2 quyển giống nhau

cat("\n=== VÍ DỤ 4: XẾP SÁCH ===\n")

# a) Hoán vị
ways_a <- factorial(5)
cat("a) Sách khác nhau:", ways_a, "\n")  # 120

# b) Hoán vị lặp
ways_b <- factorial(5) / factorial(2)
cat("b) Có 2 sách giống:", ways_b, "\n")  # 60

# ------------------------------------------------------------------------------
# 10.8 Ứng dụng trong Xác suất
# ------------------------------------------------------------------------------

# 10.8.1 Xác suất cơ bản
#
# Công thức:
# P(A) = Số kết quả thuận lợi / Tổng số kết quả có thể

# 10.8.2 Ví dụ về xác suất

cat("\n=== ỨNG DỤNG XÁC SUẤT ===\n")

# Ví dụ 1: Xác suất rút 4 quân Át từ bộ bài 52 lá

# Tổng số cách chọn 4 lá
total_ways <- choose(52, 4)
cat("Tổng số cách chọn 4 lá:", total_ways, "\n")

# Số cách chọn 4 quân Át
ace_ways <- choose(4, 4)
cat("Số cách chọn 4 Át:", ace_ways, "\n")

# Xác suất
prob <- ace_ways / total_ways
cat("Xác suất rút 4 Át:", prob, "\n")
cat("Xác suất:", format(prob, scientific = FALSE), "\n")

# Ví dụ 2: Xác suất có đúng 2 quân Át trong 5 lá bài

# Chọn 2 Át từ 4 Át
ways_2_aces <- choose(4, 2)

# Chọn 3 lá khác từ 48 lá
ways_3_others <- choose(48, 3)

# Tổng số cách chọn 5 lá
total_ways <- choose(52, 5)

# Xác suất
prob <- (ways_2_aces * ways_3_others) / total_ways
cat("\nXác suất có đúng 2 Át:", round(prob, 4), "\n")

# Ví dụ 3: Xổ số - Chọn 6 số từ 45 số

# Tổng số cách chọn
total <- choose(45, 6)
cat("\nTổng số tổ hợp:", total, "\n")

# Xác suất trúng jackpot
prob_jackpot <- 1 / total
cat("Xác suất trúng:", prob_jackpot, "\n")
cat("Tỷ lệ: 1 trên", total, "\n")

# ------------------------------------------------------------------------------
# 10.9 Tam giác Pascal
# ------------------------------------------------------------------------------

# 10.9.1 Tam giác Pascal

# Vẽ tam giác Pascal
pascal_triangle <- function(n) {
  cat("\n=== TAM GIÁC PASCAL ===\n\n")
  for (i in 0:n) {
    cat(rep(" ", n - i), sep = "")
    for (j in 0:i) {
      cat(choose(i, j), " ")
    }
    cat("\n")
  }
}

pascal_triangle(6)

# 10.9.2 Tính chất

# Tính chất 1: Tổng hàng n = 2^n
n <- 5
row_sum <- sum(choose(n, 0:n))
cat("\nTổng hàng", n, ":", row_sum, "\n")
cat("2^", n, "=", 2^n, "\n")

# Tính chất 2: Đối xứng C(n, r) = C(n, n-r)
cat("\nC(10, 3) =", choose(10, 3), "\n")
cat("C(10, 7) =", choose(10, 7), "\n")

# ==============================================================================
# BÀI TẬP THỰC HÀNH
# ==============================================================================

cat("\n=== BÀI TẬP THỰC HÀNH ===\n")

# ------------------------------------------------------------------------------
# Bài tập 1: Phân loại
# ------------------------------------------------------------------------------

# Xác định mỗi bài toán sau dùng công thức nào:
#
# 1. Sắp xếp 7 quyển sách khác nhau
# 2. Sắp xếp các chữ cái trong từ "MATHEMATICS"
# 3. Mật khẩu 5 chữ số, mỗi chữ số từ 0-9, có thể lặp
# 4. Chọn 4 học sinh từ 20 học sinh vào đội thi
# 5. Chọn 3 viên kẹo từ 5 loại, có thể chọn cùng loại

# Đáp án:

cat("\nBài tập 1:\n")

# 1. Hoán vị
cat("1. Hoán vị:", factorial(7), "\n")

# 2. Hoán vị lặp (M:2, A:2, T:2, H:1, E:1, I:1, C:1, S:1)
result_2 <- factorial(11) / (factorial(2) * factorial(2) * factorial(2))
cat("2. Hoán vị lặp:", result_2, "\n")

# 3. Chỉnh hợp lặp
cat("3. Chỉnh hợp lặp:", 10^5, "\n")

# 4. Tổ hợp
cat("4. Tổ hợp:", choose(20, 4), "\n")

# 5. Tổ hợp lặp
cat("5. Tổ hợp lặp:", to_hop_lap(5, 3), "\n")

# ------------------------------------------------------------------------------
# Bài tập 2: Mật khẩu
# ------------------------------------------------------------------------------

# Mật khẩu gồm 8 ký tự. Tính số mật khẩu nếu:
# 1. Chỉ dùng chữ số 0-9, có thể lặp
# 2. Chỉ dùng chữ số 0-9, không lặp
# 3. Dùng cả chữ và số (a-z, 0-9), có thể lặp

cat("\nBài tập 2 - Mật khẩu:\n")
cat("1. Chỉ số, có lặp:", 10^8, "\n")
cat("2. Chỉ số, không lặp:", chinh_hop(10, 8), "\n")
cat("3. Chữ+số, có lặp:", 36^8, "\n")

# ------------------------------------------------------------------------------
# Bài tập 3: Xếp người
# ------------------------------------------------------------------------------

# 1. Có bao nhiêu cách xếp 5 người A, B, C, D, E thành hàng ngang?
# 2. Có bao nhiêu cách nếu A và B phải đứng cạnh nhau?
# 3. Có bao nhiêu cách nếu A và B không được đứng cạnh nhau?

cat("\nBài tập 3 - Xếp người:\n")

# 1. Hoán vị bình thường
cat("1. Xếp 5 người:", factorial(5), "\n")

# 2. A và B cạnh nhau: Coi AB như 1 khối, xếp 4 khối, AB có thể đổi chỗ
cat("2. A, B cạnh nhau:", factorial(4) * 2, "\n")

# 3. A và B không cạnh nhau = Tổng - A,B cạnh nhau
cat("3. A, B không cạnh nhau:", factorial(5) - factorial(4) * 2, "\n")

# ------------------------------------------------------------------------------
# Bài tập 4: Phân phối
# ------------------------------------------------------------------------------

# Có 10 viên bi giống nhau, chia cho 3 bạn. 
# Mỗi bạn nhận ít nhất 1 viên. Có bao nhiêu cách chia?

# Giải: Cho mỗi người 1 viên trước, còn 7 viên cần chia
# Dùng công thức stars and bars: C(n-1, k-1)
# C(7+3-1, 3-1) = C(9, 2)

cat("\nBài tập 4 - Phân phối:\n")
cat("Số cách chia:", choose(9, 2), "\n")

# ------------------------------------------------------------------------------
# Bài tập 5: Tổ hợp lặp
# ------------------------------------------------------------------------------

# 1. Có bao nhiêu cách mua 8 quả từ 4 loại trái cây?
# 2. Phương trình x₁ + x₂ + x₃ = 10, với x₁, x₂, x₃ ≥ 0, 
#    có bao nhiêu nghiệm nguyên không âm?

cat("\nBài tập 5 - Tổ hợp lặp:\n")
cat("1. Mua 8 quả từ 4 loại:", to_hop_lap(4, 8), "\n")
cat("2. Nghiệm PT:", to_hop_lap(3, 10), "\n")

# ==============================================================================
# CÂU HỎI ÔN TẬP
# ==============================================================================

# 1. Phân biệt Hoán vị, Chỉnh hợp và Tổ hợp?
# 2. Khi nào dùng công thức có lặp?
# 3. Giải thích sự khác biệt giữa A(n,r) và nʳ?
# 4. Tại sao C'(n,r) = C(n+r-1, r)?
# 5. Quy tắc cộng và quy tắc nhân khác nhau như thế nào?
# 6. Cho ví dụ thực tế về mỗi loại công thức?

# ==============================================================================
# TÀI LIỆU THAM KHẢO
# ==============================================================================

# 1. R Documentation: ?factorial, ?choose
# 2. Toán học rời rạc: Giáo trình Đại học
# 3. Xác suất thống kê: Giáo trình cơ bản

# ==============================================================================
# TỔNG KẾT
# ==============================================================================

cat("\n=== TỔNG KẾT ===\n")
cat("\nCông thức tổng hợp:\n")
cat("1. Hoán vị:        P(n) = n!\n")
cat("2. Hoán vị lặp:    n!/(n₁!×n₂!...)\n")
cat("3. Chỉnh hợp:      A(n,r) = n!/(n-r)!\n")
cat("4. Chỉnh hợp lặp:  nʳ\n")
cat("5. Tổ hợp:         C(n,r) = n!/(r!(n-r)!)\n")
cat("6. Tổ hợp lặp:     C'(n,r) = C(n+r-1,r)\n")

cat("\nLưu ý:\n")
cat("- Thứ tự quan trọng → Hoán vị/Chỉnh hợp\n")
cat("- Thứ tự không quan trọng → Tổ hợp\n")
cat("- Có lặp → Dùng công thức lặp\n")
cat("- Không lặp → Dùng công thức thường\n")
