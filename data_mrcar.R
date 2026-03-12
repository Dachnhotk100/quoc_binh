# ============================================================================
# R DATA CLEANING TUTORIAL: MTCARS DATASET
# ============================================================================
# Tên Dataset: Motor Trends Car Road Tests (mtcars)
# Nguồn: Built-in R dataset
# Số Quan Sát: 32 ô tô
# Số Biến: 11 biến (numeric & categorical)
# Mục Tiêu: Xử lý dữ liệu và chuẩn bị cho phân tích
# ============================================================================

# Các biến trong dataset:
#   mpg    - Mức tiêu hao nhiên liệu (Miles/gallon) - NUMERIC
#   cyl    - Số xi-lanh (4, 6, 8) - CATEGORICAL (Ordinal)
#   disp   - Dung tích động cơ (cu.in.) - NUMERIC
#   hp     - Công suất (horsepower) - NUMERIC
#   drat   - Tỷ lệ trục sau - NUMERIC
#   wt     - Trọng lượng (1000 lbs) - NUMERIC
#   qsec   - Thời gian tăng tốc 1/4 dặm - NUMERIC
#   vs     - Loại động cơ (V/straight) - CATEGORICAL (Nominal)
#   am     - Loại hộp số (Auto/Manual) - CATEGORICAL (Nominal)
#   gear   - Số bánh răng (3, 4, 5) - CATEGORICAL (Ordinal)
#   carb   - Số bộ cấp nhiên liệu - CATEGORICAL (Ordinal)

# ============================================================================
# BƯỚC 1: LOAD & KHÁM PHÁ DỮ LIỆU
# ============================================================================
# Mục Tiêu: Hiểu rõ cấu trúc, hình dạng và đặc điểm của dữ liệu
# ============================================================================

cars <- mtcars

cat("=" %*% 70, "\n")
cat("BƯỚC 1: LOAD & KHÁM PHÁ DỮ LIỆU\n")
cat("=" %*% 70, "\n\n")

# Xem 6 dòng đầu tiên
cat("► Xem 6 dòng đầu tiên:\n")
print(head(cars))

# Kiểm tra cấu trúc: loại dữ liệu mỗi cột, số quan sát
cat("\n► Cấu trúc dữ liệu:\n")
str(cars)

# Tóm tắt thống kê mô tả
cat("\n► Thống kê tóm tắt:\n")
summary(cars)

# ============================================================================
# BƯỚC 2: TẠO MISSING DATA (Demo)
# ============================================================================
# Mục Tiêu: Mô phỏng dữ liệu thực tế (thường có missing values)
# Cách làm: Sử dụng set.seed() để reproducible
# ============================================================================

cat("\n" %*% 70, "\n")
cat("BƯỚC 2: TẠO MISSING DATA\n")
cat("=" %*% 70, "\n\n")

set.seed(123)  # Để kết quả reproducible

# Thêm missing values (NA) vào các cột khác nhau
cars$mpg[c(5, 12)] <- NA      # 2 NA vào mpg
cars$hp[8] <- NA               # 1 NA vào hp
cars$wt[c(15, 20)] <- NA       # 2 NA vào wt
cars$qsec[25] <- NA            # 1 NA vào qsec

cat("Đã thêm 6 missing values vào 4 cột khác nhau\n\n")

# ============================================================================
# BƯỚC 3: PHÁT HIỆN & XỬ LÝ MISSING DATA
# ============================================================================
# Mục Tiêu: 
#   - Phát hiện NA ở đâu
#   - Quyết định cách xử lý (deletion, imputation, etc)
#   - Điền giá trị thiếu
# ============================================================================

cat("=" %*% 70, "\n")
cat("BƯỚC 3: PHÁT HIỆN & XỬ LÝ MISSING DATA\n")
cat("=" %*% 70, "\n\n")

# A. PHÁT HIỆN MISSING DATA
cat("A. PHÁT HIỆN MISSING DATA:\n")
cat("─" %*% 70, "\n\n")

# Tìm dòng có NA
rows_na <- cars[!complete.cases(cars), ]
cat("Số dòng có NA:", nrow(rows_na), "\n\n")

# Đếm NA theo từng cột
cat("Số NA trong mỗi cột:\n")
na_counts <- colSums(is.na(cars))
print(na_counts)

# B. XỬ LÝ MISSING DATA
cat("\n\nB. XỬ LÝ MISSING DATA:\n")
cat("─" %*% 70, "\n\n")

# Phương pháp: MEDIAN IMPUTATION
# Lý do: Robust (ít bị outlier ảnh hưởng) hơn mean
# Công thức: NA value → median của biến đó
# Ưu điểm: Đơn giản, mọi observations được giữ lại

cat("Phương pháp: Median Imputation\n")
cat("Lý do: Median ít bị outlier ảnh hưởng\n\n")

# Thay thế NA bằng median của từng biến
cars$mpg[is.na(cars$mpg)] <- median(cars$mpg, na.rm = TRUE)
cars$hp[is.na(cars$hp)] <- median(cars$hp, na.rm = TRUE)
cars$wt[is.na(cars$wt)] <- median(cars$wt, na.rm = TRUE)
cars$qsec[is.na(cars$qsec)] <- median(cars$qsec, na.rm = TRUE)

cat("✓ Điền NA xong\n\n")

# Kiểm tra kết quả
na_remaining <- sum(is.na(cars))
cat("NA còn lại:", na_remaining, "\n")
if (na_remaining == 0) cat("✓ Hoàn hảo!\n")

# ============================================================================
# BƯỚC 4: CHUYỂN ĐỔI CATEGORICAL VARIABLES THÀNH FACTOR
# ============================================================================
# Mục Tiêu:
#   - Chuyển numeric → factor (các biến phân loại)
#   - Thêm labels có ý nghĩa
#   - Phân biệt Nominal (không thứ tự) vs Ordinal (có thứ tự)
# ============================================================================

cat("\n" %*% 70, "\n")
cat("BƯỚC 4: CHUYỂN ĐỔI CATEGORICAL VARIABLES\n")
cat("=" %*% 70, "\n\n")

cat("Ordinal Factors (Có thứ tự):\n")
cat("─" %*% 70, "\n\n")

# CYL: 4 < 6 < 8 (có thứ tự logic)
cars$cyl <- factor(cars$cyl,
                   levels = c(4, 6, 8),
                   labels = c("4 cyl", "6 cyl", "8 cyl"),
                   ordered = TRUE)  # Chỉ định ordered = TRUE

cat("✓ cyl: 4 cylinders < 6 cylinders < 8 cylinders\n")

# GEAR: 3 < 4 < 5 (có thứ tự logic - gears)
cars$gear <- factor(cars$gear,
                    levels = c(3, 4, 5),
                    labels = c("3 gears", "4 gears", "5 gears"),
                    ordered = TRUE)

cat("✓ gear: 3 gears < 4 gears < 5 gears\n")

# CARB: 1 < 2 < ... < 8 (có thứ tự logic)
cars$carb <- factor(cars$carb,
                    levels = c(1, 2, 3, 4, 6, 8),
                    labels = c("1", "2", "3", "4", "6", "8"),
                    ordered = TRUE)

cat("✓ carb: 1 < 2 < ... < 8\n")

cat("\n\nNominal Factors (Không thứ tự):\n")
cat("─" %*% 70, "\n\n")

# VS: V-shaped hoặc Straight (không có thứ tự)
cars$vs <- factor(cars$vs,
                  levels = c(0, 1),
                  labels = c("V-shaped", "Straight"))
# Không có ordered = TRUE

cat("✓ vs: V-shaped | Straight (không thứ tự)\n")

# AM: Automatic hoặc Manual (không có thứ tự)
cars$am <- factor(cars$am,
                  levels = c(0, 1),
                  labels = c("Automatic", "Manual"))

cat("✓ am: Automatic | Manual (không thứ tự)\n")

cat("\n► Cấu trúc sau chuyển đổi:\n")
str(cars)

# ============================================================================
# BƯỚC 5: TẠO BIẾN MỚI TỪ BIẾN CÓ SẴN
# ============================================================================
# Mục Tiêu: Tạo biến tổng hợp để hỗ trợ phân tích
# Kỹ thuật: cut() function để binning (chia thành nhóm)
# ============================================================================

cat("\n" %*% 70, "\n")
cat("BƯỚC 5: TẠO BIẾN MỚI\n")
cat("=" %*% 70, "\n\n")

# BIẾN 1: Efficiency (Hiệu suất nhiên liệu)
# Chia MPG thành 3 nhóm: Low (≤15), Medium (15-25), High (>25)
cars$efficiency <- cut(cars$mpg,
                       breaks = c(0, 15, 25, Inf),
                       labels = c("Low", "Medium", "High"),
                       ordered = TRUE)

cat("✓ Tạo efficiency: Low | Medium | High\n")
cat("  Phân bố:\n")
print(table(cars$efficiency))

# BIẾN 2: Power (Công suất)
# Chia HP thành 3 nhóm tự động với equal width
cars$power <- cut(cars$hp,
                  breaks = 3,
                  labels = c("Low", "Medium", "High"),
                  ordered = TRUE)

cat("\n✓ Tạo power: Low | Medium | High\n")
cat("  Phân bố:\n")
print(table(cars$power))

# ============================================================================
# BƯỚC 6: PHÂN TÍCH TÓMO TẮT
# ============================================================================
# Mục Tiêu: 
#   - Tính toán thống kê cơ bản
#   - Xem mối quan hệ giữa các biến
#   - Chuẩn bị cho phân tích chi tiết
# ============================================================================

cat("\n" %*% 70, "\n")
cat("BƯỚC 6: PHÂN TÍCH TÓM TẮT\n")
cat("=" %*% 70, "\n\n")

# Chỉ lấy các biến numeric
numeric_cols <- c("mpg", "disp", "hp", "drat", "wt", "qsec")

cat("Correlation Matrix (Tương quan):\n")
cor_matrix <- cor(cars[, numeric_cols])
print(round(cor_matrix, 2))

cat("\n\nTrung bình MPG theo CYL:\n")
mpg_by_cyl <- tapply(cars$mpg, cars$cyl, mean)
print(round(mpg_by_cyl, 2))

cat("\nTrung bình HP theo AM:\n")
hp_by_am <- tapply(cars$hp, cars$am, mean)
print(round(hp_by_am, 2))

# ============================================================================
# BƯỚC 7: KIỂM TRA CHẤT LƯỢNG DỮ LIỆU (FINAL CHECK)
# ============================================================================
# Mục Tiêu:
#   - Xác nhận dữ liệu đã được làm sạch
#   - Không còn NA
#   - Tất cả biến đúng loại
# ============================================================================

cat("\n" %*% 70, "\n")
cat("BƯỚC 7: KIỂM TRA CHẤT LƯỢNG DỮ LIỆU\n")
cat("=" %*% 70, "\n\n")

cat("Cấu trúc cuối cùng:\n")
str(cars)

cat("\n\nThống kê cuối cùng:\n")
summary(cars)

cat("\n\nKiểm tra NA:\n")
missing_final <- sum(is.na(cars))
cat("Tổng NA:", missing_final, "\n")

if (missing_final == 0) {
  cat("✓ HOÀN HẢO! Dữ liệu sạch sẽ và sẵn sàng!\n")
}

# Thống kê chung
cat("\n\nThống kê chung:\n")
cat("Số quan sát:", nrow(cars), "\n")
cat("Số biến:", ncol(cars), "\n")
cat("Biến numeric:", sum(sapply(cars, is.numeric)), "\n")
cat("Biến factor:", sum(sapply(cars, is.factor)), "\n")

# ============================================================================
# BƯỚC 8: LƯU DỮ LIỆU SẠCH
# ============================================================================
# Mục Tiêu: Export dữ liệu clean thành file CSV để sử dụng sau
# ============================================================================

cat("\n" %*% 70, "\n")
cat("BƯỚC 8: LƯU DỮ LIỆU SẠCH\n")
cat("=" %*% 70, "\n\n")

output_file <- "/mnt/user-data/outputs/mtcars_cleaned.csv"
write.csv(cars, output_file, row.names = TRUE)

cat("✓ Dữ liệu đã lưu tại:\n")
cat("  ", output_file, "\n\n")

cat("=" %*% 70, "\n")
cat("✓ DATA CLEANING HOÀN THÀNH!\n")
cat("=" %*% 70, "\n\n")

cat("Dataset sạch sẽ và sẵn sàng cho các phân tích tiếp theo:\n")
cat("  • Descriptive Statistics\n")
cat("  • Visualization (Biểu đồ)\n")
cat("  • Statistical Testing (Kiểm định)\n")
cat("  • Regression Modeling (Mô hình hồi quy)\n")
cat("  • Machine Learning (Học máy)\n")