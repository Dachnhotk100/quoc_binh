# ==============================================================================
# BÀI 9: TRỰC QUAN HÓA DỮ LIỆU CỞ BẢN
# ==============================================================================
# Mục tiêu học tập:
# - Hiểu tầm quan trọng của việc trực quan hóa dữ liệu
# - Nắm vững các loại biểu đồ cơ bản trong R
# - Biết cách tùy chỉnh biểu đồ (màu sắc, nhãn, tiêu đề)
# - Vẽ được nhiều biểu đồ cùng lúc để so sánh
# - Áp dụng biểu đồ phù hợp cho từng loại dữ liệu
# ==============================================================================

# ------------------------------------------------------------------------------
# 9.1 Tại sao cần trực quan hóa dữ liệu?
# ------------------------------------------------------------------------------

# Trực quan hóa giúp:
# - Hiểu dữ liệu nhanh chóng
# - Phát hiện xu hướng, mẫu hình (patterns)
# - Tìm ra giá trị bất thường (outliers)
# - Truyền đạt thông tin hiệu quả
# - Ra quyết định tốt hơn

# Ví dụ minh họa: Anscombe's Quartet
# 4 tập dữ liệu có cùng thống kê mô tả nhưng rất khác nhau khi vẽ


data()
data("anscombe")
print(anscombe)

# Tính thống kê cho x1 và y1
cat("Trung bình x1: ", mean(anscombe$x1), "\n")
cat("Trung bình y1:", mean(anscombe$y1), "\n")
cat("Độ lệch chuẩn x1:", sd(anscombe$x1), "\n")
cat("Độ lệch chuẩn y1:", sd(anscombe$y1), "\n")
cat("Tương quan:", cor(anscombe$x1, anscombe$y1), "\n")

# Thống kê tương tự cho các cặp khác
# Nhưng khi vẽ ra sẽ thấy rất khác nhau!


# ------------------------------------------------------------------------------
# 9.2 Biểu đồ cột (Bar Plot)
# ------------------------------------------------------------------------------

# 9.2.1 Biểu đồ cột cơ bản

# Biểu đồ cột được sử dụng để hiển thị dữ liệu phân loại (categorical data)
# Ví dụ: Đếm số lượng xe theo số xy-lanh

# Sử dụng dữ liệu mtcars
data("mtcars")
View(mtcars)
# Đếm số xe theo số động cơ
table_cyl <- table(mtcars$cyl)

# Vẽ biểu đồ cột
barplot(table_cyl)


# 9.2.2 Tùy chỉnh biểu đồ cột

# Thêm tiêu đề và nhãn trục
barplot(table_cyl,
        main = "Số lượng xe theo số xy-lanh", # Tiêu đồ
        xlab = "Số xy-lanh", 
        ylab = "Số lượng xe",
        col = "steelblue" # Màu cột
)

# Thay đổi màu sắc cho mỗi cột
barplot(table_cyl,
        main = "Số lượng xe theo số xy-lanh",
        xlab = "Số xy-lanh",
        ylab = "Số lượng xe",
        col = c("red", "green", "blue"))

# Biểu đồ cột ngang (horizontal)
barplot(table_cyl,
        main = "Số lượng xe theo số xy-lanh",
        xlab = "Số lượng xe",
        ylab = "Số xy-lanh",
        col = "orange",
        horiz = TRUE)  # Vẽ ngang

# 9.2.3 Biểu đồ cột cho dữ liệu số
# Ví dụ: Điểm trung bình của 5 sinh viên
students <- c("Tùng", "Hùng", "Dũng", "Linh", "Mai")
scores <- c(8.5, 9.0, 7.5, 8.8, 9.2)

barplot(scores,
        names.arg = students,  # Tên cho mỗi cột
        main = "Điểm trung bình sinh viên",
        xlab = "Sinh viên",
        ylab = "Điểm",
        col = "skyblue",
        ylim = c(0, 10),       # Giới hạn trục y
        border = "darkblue")   # Màu viền


# Thêm giá trị lên đầu mỗi cột
barplot(scores,
        names.arg = students,
        main = "Điểm trung bình sinh viên",
        xlab = "Sinh viên",
        ylab = "Điểm",
        col = "lightgreen",
        ylim = c(0, 10))

# Thêm text
text(x = 1:5*1.2-0.5, # Vị trị x
     y = scores + 0.3, # Vị trí của y tên cột một chút
     labels = scores
     )

# 9.2.4 Biểu đồ cột nhóm (Grouped Bar Plot)

# Ví dụ: So sánh điểm của 2 lớp
class_A <- c(8.5, 7.8, 9.0, 8.2, 7.5)
class_B <- c(8.0, 8.5, 8.8, 7.9, 8.3)

# Tạo ma trận
scores_matrix <- rbind(class_A, class_B)
colnames(scores_matrix) <- c("Môn 1", "Môn 2", "Môn 3", "Môn 4", "Môn 5")

barplot(scores_matrix,
        main = "So sánh điểm 2 lớp",
        xlab = "Môn học",
        ylab = "Điểm",
        col = c("lightblue", "lightcoral"),
        beside = TRUE, # Các cột cạnh nhau (không chồng nhau)
        legend = c("Lớp A", "Lớp B"))

# Biểu đồ cột chồng (Stacked Bar Plot)
barplot(scores_matrix,
        main = "So sánh điểm 2 lớp (Stacked)",
        xlab = "Môn học",
        ylab = "Tổng điểm",
        col = c("lightblue", "lightcoral"),
        legend = c("Lớp A", "Lớp B"))

# ------------------------------------------------------------------------------
# 9.3 Biểu đồ tần số (Histogram)
# ------------------------------------------------------------------------------

# 9.3.1 Histogram cơ bản

# Histogram được sử dụng để hiển thị phân phối của dữ liệu liên tục
# Khác với barplot: Histogram dùng cho dữ liệu số, barplot cho dữ liệu phân loại

# Ví dụ: Phân phối mpg (miles per gallon)
data("mtcars")

hist(mtcars$mpg)

# 9.3.2 Tùy chỉnh Histogram

# Thêm tiêu đề và nhãn
hist(mtcars$mpg,
     main = "Phân phối tiêu thụ nhiên liệu",
     xlab = "Miles per Gallon",
     ylab = "Tần số",
     col = "lightblue",
     border = "darkblue")

# Thay đổi số lượng bins (cột)
hist(mtcars$mpg,
     main = "Histogram với 5 bins",
     xlab = "MPG",
     breaks = 5,    # Số bins
     col = "coral")

hist(mtcars$mpg,
     main = "Histogram với 15 bins",
     xlab = "MPG",
     breaks = 15,
     col = "lightgreen")

hist(mtcars$mpg,
     main = "Histogram với 15 bins",
     xlab = "MPG",
     breaks = 30,
     col = "lightgreen")

# 9.3.3 Histogram với đường cong mật độ

# Tạo histogram với xác suất thay vì tần số
hist(mtcars$mpg,
     main = "Histogram với đường cong mật độ",
     xlab = "MPG",
     col = "lightgray",
     probability = TRUE)  # Trục y là mật độ xác suất

# Thêm đường cong mật độ
lines(density(mtcars$mpg), 
      col = "red", 
      lwd = 2)  # Độ dày đường


# 9.3.4 Ví dụ thực tế

# Phân phối chiều cao sinh viên (giả định)
set.seed(42)
heights <- rnorm(100, mean = 165, sd = 8)  # 100 sinh viên, TB=165cm, SD=8cm

hist(heights,
     main = "Phân phối chiều cao sinh viên",
     xlab = "Chiều cao (cm)",
     ylab = "Số sinh viên",
     col = "skyblue",
     breaks = 10)

# Thêm đường trung bình
abline(v = mean(heights), 
       col = "red", 
       lwd = 2, 
       lty = 2)  # Nét đứt

# Thêm chú thích
legend("topright", 
       legend = paste("TB =", round(mean(heights), 1), "cm"),
       col = "red",
       lty = 2,
       lwd = 2)


# ------------------------------------------------------------------------------
# 9.4 Biểu đồ hộp (Box Plot)
# ------------------------------------------------------------------------------

# 9.4.1 Box Plot cơ bản

# Box plot hiển thị:
# - Trung vị (median): Đường giữa hộp
# - Q1 (25%): Cạnh dưới hộp
# - Q3 (75%): Cạnh trên hộp
# - IQR = Q3 - Q1: Chiều cao hộp
# - Whiskers (râu): Kéo dài đến 1.5*IQR
# - Outliers: Các điểm ngoài whiskers

boxplot(mtcars$mpg)

# 9.4.2 Tùy chỉnh Box Plot

boxplot(mtcars$mpg,
        main = "Box Plot của MPG",
        ylab = "Miles per Gallon",
        col = "lightgreen",
        border = "darkgreen")

# 9.4.3 So sánh nhiều nhóm

# So sánh mpg theo số xy-lanh
boxplot(mpg ~ cyl, 
        data = mtcars,
        main = "MPG theo số xy-lanh",
        xlab = "Số xy-lanh",
        ylab = "Miles per Gallon",
        col = c("red", "green", "blue"))

# Giải thích:
# - Xe 4 xy-lanh có mpg cao nhất
# - Xe 8 xy-lanh có mpg thấp nhất
# - Có một số outliers

# 9.4.4 Box Plot ngang

boxplot(mpg ~ cyl, 
        data = mtcars,
        main = "MPG theo số xy-lanh (ngang)",
        xlab = "Miles per Gallon",
        ylab = "Số xy-lanh",
        col = c("lightblue", "lightcoral", "lightyellow"),
        horizontal = TRUE)


# 9.4.5 Ví dụ với dữ liệu iris

data(iris)

# So sánh chiều dài đài hoa giữa các loài
boxplot(Sepal.Length ~ Species,
        data = iris,
        main = "Chiều dài đài hoa theo loài",
        xlab = "Loài",
        ylab = "Chiều dài đài hoa (cm)",
        col = c("pink", "lightblue", "lightgreen"))

# Nhận xét từ biểu đồ:
# - Virginica có đài hoa dài nhất
# - Setosa có đài hoa ngắn nhất
# - Có một số outliers ở Virginica

# 9.4.6 Nhiều Box Plot cùng lúc

# So sánh tất cả 4 biến số của iris
boxplot(iris[, 1:4],
        main = "So sánh các đặc điểm của hoa",
        col = rainbow(4),
        names = c("Sepal.L", "Sepal.W", "Petal.L", "Petal.W"))

# ------------------------------------------------------------------------------
# 9.5 Biểu đồ phân tán (Scatter Plot)
# ------------------------------------------------------------------------------

# 9.5.1 Scatter Plot cơ bản

# Scatter plot hiển thị mối quan hệ giữa 2 biến số

# Ví dụ: Mối quan hệ giữa trọng lượng và mpg

plot(mtcars$wt, mtcars$mpg)

# 9.5.2 Tùy chỉnh Scatter Plot

plot(mtcars$wt, mtcars$mpg,
     main = "Mối quan hệ giữa trọng lượng và MPG",
     xlab = "Trọng lượng (1000 lbs)",
     ylab = "Miles per Gallon",
     col = "blue",
     pch = 19)  # Kiểu điểm (19 = điểm tròn đặc)

# 9.5.3 Các kiểu điểm khác nhau

# Vẽ ví dụ về các kiểu điểm
plot(1:25, 1:25,
     main = "Các kiểu điểm (pch)",
     pch = 1:25,
     col = 1:25,
     cex = 2,  # Kích thước điểm
     xlab = "pch value",
     ylab = "")
text(1:25, 1:25, labels = 1:25, pos = 3)  # Thêm số

# 9.5.4 Thêm đường xu hướng (trend line)

plot(mtcars$wt, mtcars$mpg,
     main = "MPG vs Trọng lượng",
     xlab = "Trọng lượng (1000 lbs)",
     ylab = "Miles per Gallon",
     col = "darkblue",
     pch = 19)

# Thêm đường hồi quy tuyến tính
model <- lm(mpg ~ wt, data = mtcars)
abline(model, col = "red", lwd = 2)

# Thêm công thức hồi quy
coef <- round(coef(model), 2)
formula_text <- paste("y =", coef[1], "+", coef[2], "* x")
text(4.5, 30, formula_text, col = "red")

# 9.5.5 Phân nhóm theo màu

# Phân biệt xe theo số xy-lanh
plot(mtcars$wt, mtcars$mpg,
     main = "MPG vs Trọng lượng (theo xy-lanh)",
     xlab = "Trọng lượng",
     ylab = "MPG",
     col = mtcars$cyl,  # Màu theo cyl
     pch = 19,
     cex = 1.5)

# Thêm chú thích
legend("topright",
       legend = c("4 cyl", "6 cyl", "8 cyl"),
       col = c(4, 6, 8),
       pch = 19)

# 9.5.6 Scatter Plot với nhiều biến

# Ma trận scatter plot
pairs(mtcars[, c("mpg", "wt", "hp", "qsec")],
      main = "Ma trận Scatter Plot",
      col = "blue",
      pch = 19)

# Giải thích:
# - Mỗi ô hiển thị mối quan hệ giữa 2 biến
# - Đường chéo là tên biến
# - Giúp nhìn tổng quan mối quan hệ giữa nhiều biến

# ------------------------------------------------------------------------------
# 9.6 Biểu đồ đường (Line Plot)
# ------------------------------------------------------------------------------

# 9.6.1 Line Plot cơ bản

# Line plot thường dùng cho dữ liệu chuỗi thời gian

# Ví dụ: Doanh thu theo tháng
months <- 1:12
revenue <- c(50, 55, 60, 58, 65, 70, 75, 80, 78, 85, 90, 95)

plot(months, revenue, type = "l")  # type = "l" cho đường

# 9.6.2 Tùy chỉnh Line Plot

plot(months, revenue,
     type = "l",
     main = "Doanh thu theo tháng",
     xlab = "Tháng",
     ylab = "Doanh thu (triệu đồng)",
     col = "blue",
     lwd = 2,  # Độ dày đường
     ylim = c(0, 100))

# Thêm điểm
points(months, revenue, pch = 19, col = "red")

# 9.6.3 Nhiều đường trên cùng biểu đồ

# Doanh thu 2 chi nhánh
revenue_A <- c(50, 55, 60, 58, 65, 70, 75, 80, 78, 85, 90, 95)
revenue_B <- c(45, 50, 58, 62, 68, 72, 70, 75, 80, 82, 88, 92)

plot(months, revenue_A,
     type = "l",
     main = "So sánh doanh thu 2 chi nhánh",
     xlab = "Tháng",
     ylab = "Doanh thu (triệu đồng)",
     col = "blue",
     lwd = 2,
     ylim = c(40, 100))

# Thêm đường thứ 2
lines(months, revenue_B, col = "red", lwd = 2)

# Thêm chú thích
legend("topleft",
       legend = c("Chi nhánh A", "Chi nhánh B"),
       col = c("blue", "red"),
       lwd = 2)

# 9.6.4 Kết hợp đường và điểm

plot(months, revenue_A,
     type = "b",  # "b" = both (cả đường và điểm)
     main = "Doanh thu chi nhánh A",
     xlab = "Tháng",
     ylab = "Doanh thu",
     col = "darkgreen",
     lwd = 2,
     pch = 19)



# ------------------------------------------------------------------------------
# 9.7 Biểu đồ tròn (Pie Chart)
# ------------------------------------------------------------------------------

# 9.7.1 Pie Chart cơ bản

# Ví dụ: Phân bố sinh viên theo khoa
faculties <- c("CNTT", "Kinh tế", "Ngoại ngữ", "Cơ khí")
students <- c(450, 320, 280, 350)

pie(students, labels = faculties)

# 9.7.2 Tùy chỉnh Pie Chart

pie(students,
    labels = faculties,
    main = "Phân bố sinh viên theo khoa",
    col = rainbow(4))  # Màu cầu vồng

# Thêm phần trăm
percentages <- round(students/ sum(students)*100, 1)
labels_with_pct <- paste(faculties, "\n", percentages, "%", sep=" ")

pie(students,
    labels = labels_with_pct,
    main = "Phân bố sinh viên theo khoa",
    col = c("lightblue", "lightcoral", "lightgreen", "lightyellow"))


# 9.7.3 Lưu ý khi sử dụng Pie Chart

# Pie chart tốt cho:
# - Hiển thị tỷ lệ phần trăm
# - Số lượng nhóm ít (3-5 nhóm)

# Không nên dùng khi:
# - Nhiều hơn 5-6 nhóm
# - Cần so sánh chính xác giữa các nhóm
# - Trong trường hợp này nên dùng Bar Chart

# So sánh:
par(mfrow = c(1, 2))  # Chia màn hình làm 2 cột

pie(students,
    labels = faculties,
    main = "Pie Chart",
    col = rainbow(4))

barplot(students,
        names.arg = faculties,
        main = "Bar Chart",
        col = rainbow(4),
        ylab = "Số sinh viên")


par(mfrow = c(1, 1))  # Chia màn hình làm 1 cột

# ------------------------------------------------------------------------------
# 9.8 Vẽ nhiều biểu đồ cùng lúc
# ------------------------------------------------------------------------------

# 9.8.1 Chia màn hình với par()

# Chia thành 2 hàng, 2 cột (tổng 4 ô)
par(mfrow = c(2, 2))

# Vẽ 4 biểu đồ
hist(mtcars$mpg, main = "Histogram: MPG", col = "lightblue")
boxplot(mtcars$mpg, main = "Boxplot: MPG", col = "lightgreen")
plot(mtcars$wt, mtcars$mpg, main = "Scatter: MPG vs WT", pch = 19)
barplot(table(mtcars$cyl), main = "Bar: Cylinders", col = "coral")

# Trở về chế độ 1 biểu đồ
par(mfrow = c(1, 1))

# 9.8.2 Ví dụ thực tế: Phân tích một biến

# Phân tích toàn diện biến mpg
par(mfrow = c(2, 2))

# 1. Histogram
hist(mtcars$mpg,
     main = "Phân phối MPG",
     xlab = "MPG",
     col = "skyblue",
     breaks = 10)

# 2. Boxplot
boxplot(mtcars$mpg,
        main = "Boxplot MPG",
        ylab = "MPG",
        col = "lightgreen")

# 3. Boxplot theo nhóm
boxplot(mpg ~ cyl,
        data = mtcars,
        main = "MPG theo Cylinders",
        xlab = "Cylinders",
        ylab = "MPG",
        col = c("red", "green", "blue"))

# 4. Scatter với biến khác
plot(mtcars$wt, mtcars$mpg,
     main = "MPG vs Weight",
     xlab = "Weight",
     ylab = "MPG",
     pch = 19,
     col = "darkblue")
abline(lm(mpg ~ wt, data = mtcars), col = "red", lwd = 2)

par(mfrow = c(1, 1))

# 9.8.3 Chia màn hình không đều

# Tạo layout tùy chỉnh
layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE))


# Biểu đồ 1: Chiếm 2 ô trên
hist(mtcars$mpg,
     main = "Histogram lớn",
     col = "lightblue")

# Biểu đồ 2: Ô dưới trái
boxplot(mtcars$mpg,
        main = "Boxplot nhỏ",
        col = "lightgreen")

# Biểu đồ 3: Ô dưới phải
barplot(table(mtcars$cyl),
        main = "Barplot nhỏ",
        col = "coral")

# Reset layout
layout(1)
