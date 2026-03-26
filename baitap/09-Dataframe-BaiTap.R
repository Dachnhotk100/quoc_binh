### Phần 1 ############################
# # Nhiệm vụ
# # 1. Tạo và thêm tên duy nhất vào năm vector có độ dài 8. 
# Làm cho các kiểu dữ liệu của chúng đa dạng. 
# Tạo một dataframe có tên "mySet1" từ các vector đã tạo.
# # a) Hiển thị dòng thứ 5 của dataframe đã tạo.
# # b) Thay đổi tên của cột thứ hai của dataframe 
# mySet1 thành "column02"
# # c) Hiển thị 7 dòng đầu tiên của dataframe mySet1. 
# Sử dụng hai phương pháp khác nhau - với chỉ số và với một hàm.
#a
mySet1 <- data.frame(vector1,vector2,vector3,vector4,vecctor5)
mySet1[5,]
# Tạo dataframe

# 2. Sử dụng bộ dữ liệu iris. 
# Sử dụng chỉ số để hiển thị giá trị của mỗi 
# dòng thứ 3 giữa quan sát thứ 40 và 120.
# Cố gắng sử dụng một dòng lệnh duy nhất 
# (rút gọn mã để nó vừa trong một dòng duy nhất, 
# không có bất kỳ bước trung gian nào).
iris
seq(40,120,3)
iris[seq(40,120,3),]
# 3. Sử dụng bộ dữ liệu có sẵn "women".
# a) Thay đổi kiểu của cột đầu tiên thành kiểu ký tự.
# b) Thêm hai dòng mới vào bộ dữ liệu với các số tự tạo. Đảm bảo rằng bạn không làm mất các kiểu của biến trong dataframe chính trong quá trình này.
# c) Thêm biến mới vào bộ dữ liệu và đặt tên là "shoe_size". Sử dụng hàm runif để tạo các giá trị cho biến này. Kích thước giày phải là số nguyên giữa 35 và 42.







### Phần 2 ############################
### Các bài tập ######################################################################
### Sử dụng bộ dữ liệu có sẵn CO2 cho các nhiệm vụ sau:
# 1. In giá trị CO2 uptake từ lớn nhất đến nhỏ nhất.
sort(CO2$uptake, decreasing = TRUE)


# 2. Hiển thị các dòng của bộ dữ liệu CO2 có Type là Quebec và Treatment là chilled.
CO2[CO2$Type=="Quebec" & CO2$Treatment=="chilled",]


# 3. Hiển thị các dòng của bộ dữ liệu CO2 có uptake lớn hơn 40 và 
# được sắp xếp theo giá trị conc từ nhỏ nhất đến lớn nhất.
# Điểm thưởng nếu giữ toàn bộ mã trong một dòng duy nhất. Nếu cần tạo
# một đối tượng trung gian, hãy đặt tên là 'temp'.
CO2[order(CO2$conc), ][CO2[order(CO2$conc), ]$uptake > 40, ]

# 4. Làm thế nào để sắp xếp ngẫu nhiên bộ dữ liệu CO2? GỢI Ý: Bạn có thể cần tạo
# một vector với các chỉ số ngẫu nhiên từ kết quả của order(runif(...)).
# Tham khảo phần "Picking random rows from data".
# Điểm thưởng nếu viết mã trong một dòng duy nhất không có đối tượng trung gian.
### Chạy mã này trước khi thực hiện các nhiệm vụ tiếp theo
set.seed(123)
missCO2 <- CO2
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"uptake"] <- NA
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"conc"] <- NA
missCO2$weight <- paste0(as.integer(runif(nrow(missCO2))*30),"kg")

CO2[order(runif(nrow(CO2))),]

# 5. Hiển thị các dòng của bộ dữ liệu missCO2 có ít nhất một giá trị bị thiếu.
missCO2[!complete.cases(missCO2),]
# 6. Điền các giá trị uptake bị thiếu với giá trị 20.
missCO2$uptake[is.na(missCO2$uptake)] <- 20
# 7. Điền các giá trị conc bị thiếu với giá trị trung bình của conc.
missCO2$conc[is.na(missCO2$conc)]<- mean(missCO2$conc, na.rm = TRUE)
# 8. Trích xuất giá trị số từ biến weight và lưu trong cột mới
# "weightNumber". Điểm thưởng nếu giữ mã trong một dòng,
# không sử dụng đối tượng trung gian nào.
missCO2$weightNumber <- as.numeric(gsub("kg","", missCO2$weight))



# Lưu DATA FRAME vào một tệp CSV
write.csv(missCO2, "missCO2.csv", row.names = FALSE)

# Lưu DATA FRAME vào một tệp RDS (định dạng R)
saveRDS(missCO2, "missCO2.rds")

readRDS("missCO2.rds")
# Lưu nhiều đối tượng vào một tệp RData


# save(iris, mtcars, file = "multiple_datasets.RData")
save(CO2, missCO2, file = "multiple_dataset.RData")
load("multiple_dataset.RData")
