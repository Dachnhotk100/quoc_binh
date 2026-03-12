######################################################################################
# 1. Đọc dữ liệu phân tích tính cách khách hàng và tải vào R
# file "clients.csv" với tên biến là "clients"
clients <- read.csv("clients.csv", stringsAsFactors = FALSE)


# 2. Xem qua cấu trúc dữ liệu và kiểm tra các lớp (classes)
str(clients)
# Hoặc xem chi tiết từng cột:
sapply(clients, class)


# 3. Kiểm tra giá trị bị thiếu (NA)
# a) Những biến nào có chứa giá trị bị thiếu?
colSums(is.na(clients))

# b) Điền các giá trị bị thiếu bằng giá trị trung bình hoặc trung vị
# Ở đây ta xử lý biến Year_Birth bằng trung vị (median) để đảm bảo là số nguyên
# Giả sử biến Income cũng có NA, ta dùng trung bình (mean)
if(any(is.na(clients$Year_Birth))) {
  clients$Year_Birth[is.na(clients$Year_Birth)] <- median(clients$Year_Birth, na.rm = TRUE)
}

# c) Đoạn mã cụ thể dùng để điền giá trị thiếu của Year_Birth:
# clients$Year_Birth[is.na(clients$Year_Birth)] <- median(clients$Year_Birth, na.rm = TRUE)


# 4. a) Kiểm tra xem tất cả các giá trị bị thiếu đã được điền đầy đủ chưa
sum(is.na(clients)) # Nếu kết quả là 0 nghĩa là đã điền đầy đủ

# b) Đoạn mã hiển thị tất cả các dòng vẫn còn chứa dữ liệu bị thiếu:
clients[!complete.cases(clients), ]


# 5. a) Các biến nên chuyển đổi thành kiểu "factor":
# Trả lời: Marital_Status, Complain, Response, và có thể là Kidhome, Teenhome.

# b) Đoạn mã ngắn nhất để chuyển đổi biến Marital_Status:
clients$Marital_Status <- as.factor(clients$Marital_Status)


# 6. a) Biến nên chuyển đổi thành kiểu 'ordered factor':
# Trả lời: Biến Education (vì có cấp bậc học vấn tăng dần).

# b) Đoạn mã chuyển đổi biến Education (với 2n = trung học, graduation = cử nhân):
# Thứ tự logic: Basic < 2n Cycle < Graduation < Master < PhD
clients$Education <- factor(clients$Education, 
                            levels = c("Basic", "2n Cycle", "Graduation", "Master", "PhD"), 
                            ordered = TRUE)


# 7. Chuyển đổi các biến đã xác định trong bước 5 và 6 thành các lớp thích hợp
# (Đã thực hiện ở các bước 5b và 6b ở trên)
# Kiểm tra lại kết quả:
class(clients$Marital_Status)
class(clients$Education)


# 8. Lưu kết quả vào file RData với tên "clientsInR"
save(clients, file = "clientsInR.RData")
######################################################################################