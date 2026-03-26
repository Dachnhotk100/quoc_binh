
# LOAD VÀ KHÁM PHÁ DỮ LIỆU

# Load dữ liệu
life <- read.csv("C:/Users/user/OneDrive/Desktop/PTVTQHD/data/Life Expectancy Data.csv")

# Xem dữ liệu
View(life)

# Xem 6 dòng đầu tiên
head(life)

# Tóm tắt thống kê
summary(life)

# Kiểm tra cấu trúc dữ liệu
str(life)

# Bước 2: Xử lý missing data

# Phát hiện missing data
life[!complete.cases(life), ]

# Đếm số dòng bị thiếu
length(life[!complete.cases(life), ])

# Đếm missing theo từng biến
colSums(is.na(life))

# XỬ LÝ MISSING DATA

# 1. Life expectancy

# Xem phân bố
summary(life$Life.expectancy)

# Tính median (bỏ NA)
median_life <- median(life$Life.expectancy, na.rm = TRUE)

# Điền giá trị missing bằng median
life$Life.expectancy[is.na(life$Life.expectancy)] <- median_life

# Kiểm tra lại
life$Life.expectancy[is.na(life$Life.expectancy)]

# 2. Adult Mortality

#Xem phân bố
summary(life$Adult.Mortality)

#Tính median
median_adult <- median(life$Adult.Mortality, na.rm = TRUE)

#Điền giá trị missing bằng median
life$Adult.Mortality[is.na(life$Adult.Mortality)] <- median_adult

#Kiểm tra lại
life$Adult.Mortality[is.na(life$Adult.Mortality)]

# 3. Alcohol

#Xem phân bố
summary(life$Alcohol)

#Tính median
median_alcohol <- median(life$Alcohol, na.rm = TRUE)

#Điền giá trị missing bằng median
life$Alcohol[is.na(life$Alcohol)] <- median_alcohol

#Kiểm tra lại
life$Alcohol[is.na(life$Alcohol)]

# 4. Hepatitis B
#Xem phân bố
summary(life$Hepatitis.B)

#Tính media
median_hepb <- median(life$Hepatitis.B, na.rm = TRUE)

#Điền giá trị missing bằng median
life$Hepatitis.B[is.na(life$Hepatitis.B)] <- median_hepb

#Kiểm tra lại
life$Hepatitis.B[is.na(life$Hepatitis.B)]

# 5. BMI

#Xem phân bố
summary(life$BMI)

#Tính median
median_bmi <- median(life$BMI, na.rm = TRUE)

#Điền missing bằng median
life$BMI[is.na(life$BMI)] <- median_bmi

#Kiểm tra lại
life$BMI[is.na(life$BMI)]

# 6. Polio

#Xem phân bố
summary(life$Polio)

#Tính median
median_polio <- median(life$Polio, na.rm = TRUE)

#Điền missing bằng median
life$Polio[is.na(life$Polio)] <- median_polio

#Kiểm tra lại
life$Polio[is.na(life$Polio)]

# 7. Total expenditure

#Xem phân bố
summary(life$Total.expenditure)

#Tính median
median_expenditure <- median(life$Total.expenditure, na.rm = TRUE)

#Điền missing bằng median
life$Total.expenditure[is.na(life$Total.expenditure)] <- median_expenditure

#Kiểm tra lại
life$Total.expenditure[is.na(life$Total.expenditure)]

# 8. Diphtheria

#Xem phân bố
summary(life$Diphtheria)

#Tính median
median_diph <- median(life$Diphtheria, na.rm = TRUE)

#Điền missing bằng median
life$Diphtheria[is.na(life$Diphtheria)] <- median_diph

#Kiểm tra lại
life$Diphtheria[is.na(life$Diphtheria)]

# 9. GDP

#Xem phân bố
summary(life$GDP)

#Tính median
median_gdp <- median(life$GDP, na.rm = TRUE)

#Điền missing bằng median
life$GDP[is.na(life$GDP)] <- median_gdp

#Kiểm tra lại
life$GDP[is.na(life$GDP)]

# 10. Population

#Xem phân bố
summary(life$Population)

#Tính median
median_pop <- median(life$Population, na.rm = TRUE)

#Điền missing bằng median
life$Population[is.na(life$Population)] <- median_pop

#Kiểm tra lại
life$Population[is.na(life$Population)]

# 11. Thinness 1-19 years

#Xem phân bố
summary(life$thinness.1.19.years)

#Tính median
median_thin1 <- median(life$thinness.1.19.years, na.rm = TRUE)

#Điền missing bằng median
life$thinness.1.19.years[is.na(life$thinness.1.19.years)] <- median_thin1

#Kiểm tra lại
life$thinness.1.19.years[is.na(life$thinness.1.19.years)]

# 12. Thinness 5-9 years

#Xem phân bố
summary(life$thinness.5.9.years)

#Tính median
median_thin2 <- median(life$thinness.5.9.years, na.rm = TRUE)

#Điền missing bằng median
life$thinness.5.9.years[is.na(life$thinness.5.9.years)] <- median_thin2

#Kiểm tra lại
life$thinness.5.9.years[is.na(life$thinness.5.9.years)]

# 13. Income composition of resources

#Xem phân bố
summary(life$Income.composition.of.resources)

#Tính median
median_income <- median(life$Income.composition.of.resources, na.rm = TRUE)

#Điền missing bằng median
life$Income.composition.of.resources[is.na(life$Income.composition.of.resources)] <- median_income

#Kiểm tra lại
life$Income.composition.of.resources[is.na(life$Income.composition.of.resources)]

# 14. Schooling

#Xem phân bố
summary(life$Schooling)

#Tính median
median_school <- median(life$Schooling, na.rm = TRUE)

#Điền missing bằng median
life$Schooling[is.na(life$Schooling)] <- median_school

#Kiểm tra lại
life$Schooling[is.na(life$Schooling)]

# CHUYỂN ĐỔI CATEGORICAL DATA THÀNH FACTOR

# Country
summary(factor(life$Country))

life$Country <- factor(life$Country)

# Status
summary(factor(life$Status))

life$Status <- factor(life$Status,
                      levels = c("Developing", "Developed"),
                      labels = c("Developing Country",
                                 "Developed Country"))

# Kiểm tra lại
str(life)

#: KIỂM TRA CÁC BIẾN NUMERIC


numericVariables <- c(
  "Life.expectancy",
  "Adult.Mortality",
  "Alcohol",
  "Hepatitis.B",
  "Measles",
  "BMI",
  "under.five.deaths",
  "Polio",
  "Total.expenditure",
  "Diphtheria",
  "HIV.AIDS",
  "GDP",
  "Population",
  "thinness.1.19.years",
  "thinness.5.9.years",
  "Income.composition.of.resources",
  "Schooling"
)

# Xem phân bố của từng biến
lapply(life[,numericVariables], summary)

#KIỂM TRA YEAR

summary(life$Year)

# Year là biến numeric nên giữ nguyên

#KIỂM TRA COUNTRY

# Đếm số quốc gia
length(unique(life$Country))

# Số dòng của từng country
table(life$Country)

#KIỂM TRA STATUS

table(life$Status)

# Tỷ lệ phần trăm
prop.table(table(life$Status))

# KIỂM TRA CẤU TRÚC DỮ LIỆU

str(life)

summary(life)

#KIỂM TRA MISSING DATA

sum(!complete.cases(life))

#LƯU DỮ LIỆU SẠCH
write.csv(life,"C:/Users/user/OneDrive/Desktop/PTVTQHD/data/life_expectancy_clean.csv",row.names = FALSE)
