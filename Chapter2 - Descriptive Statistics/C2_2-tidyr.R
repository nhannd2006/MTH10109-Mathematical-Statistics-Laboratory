# 26/3 - 24KDL
# Chương 2.2: GIỚI THIỆU tidyr

# ─────────────────────────────────────────────
# 1. Giới thiệu tidyverse
# ─────────────────────────────────────────────
# tidyr   : thao tác dữ liệu (reshape)
# dplyr   : thao tác và khám phá dữ liệu
# ggplot2 : trực quan hóa dữ liệu

# Tidy data phải thỏa 3 nguyên tắc:
### 1.1. Mỗi giá trị (value) phải nằm trong một ô duy nhất
### 1.2. Mỗi biến (variable) phải nằm trong một cột riêng biệt
### 1.3. Mỗi quan sát (observation) phải nằm trên một hàng riêng biệt


# ─────────────────────────────────────────────
# 2. Chuyển dữ liệu từ dạng Rộng (Wide) sang dạng Dài (Long)
# ─────────────────────────────────────────────
# Wide: các giá trị của cùng một biến số dàn trải ra nhiều cột (thường gặp trong Excel)
# Long: TIDY - tất cả giá trị của một biến số được gom vào một cột duy nhất

install.packages("tidyr")   # chạy 1 lần
library(tidyr)

# Chuyển dữ liệu từ dạng wide sang long: pivot_longer()
Experiment <- data.frame(Time = c("T1", "T2", "T3"),
                         Subject1 = c(12, 10, 9),
                         Subject2 = c(9, 8, 7),
                         Subject3 = c(16, 15, 12),
                         Subject4 = c(12, 12, 11))
head(Experiment)   # Subject1..Subject4 cần gộp lại thành 2 biến: Subject và Response

pivot_longer(
  data = Experiment,
  cols = -Time,   # lấy tất cả các cột trừ Time
  # cols = c(Subject1, Subject2, Subject3, Subject4),
  # cols = Subject1:Subject4,
  # cols = 2:5,
  # cols = starts_with("Subject"),
  names_to = "Subject",
  values_to = "Response"
)


# ─────────────────────────────────────────────
# A. Các cách chọn cột dữ liệu trong pivot_longer()
# ─────────────────────────────────────────────
### 1. Chọn thủ công (Explicit Selection)
# dùng khi biết chính xác tên cột và số lượng cột ít
# --> cols = c(Cot_1, Cot_2)

### 2. Chọn theo khoảng (Range Selection)
# dùng khi các cột cần chọn nằm liên tiếp nhau
# --> cols = Cot_Bat_Dau : Cot_Ket_Thuc

### 3. Chọn bằng phương pháp loại trừ (Exclusion)
# chọn những cột muốn giữ nguyên (định danh) và thêm dấu - phía trước
# --> cols = -Cot_Muon_Giu_Nguyen

### 4. Sử dụng hàm hỗ trợ (Tidyselect Helpers)
# starts_with("string")  : chọn các cột bắt đầu bằng chuỗi cụ thể
# ends_with("string")    : chọn các cột kết thúc bằng chuỗi cụ thể
# contains("string")     : chọn các cột có chứa chuỗi cụ thể
# matches("\\d{4}")      : chọn cột dựa trên biểu thức chính quy (regex), vd 4 chữ số
# everything()           : chọn tất cả các cột

### 5. Chọn theo kiểu dữ liệu (Predicate Selection)
# dùng khi muốn xoay tất cả các cột cùng định dạng
# --> cols = where(is.numeric)
# --> cols = where(is.character)

## Mẹo nhỏ
# 1. Tên cột là số --> để trong dấu huyền: cols = `2021`:`2023`
# 2. Kết hợp nhiều cách: cols = c(ID, starts_with("Q"))
# 3. Kiểm tra: số hàng sau pivot_longer = số hàng cũ × số cột đã chọn --> đúng


#========================================#
#Bài tập 1.1:
# Bảng gốc (wide):
#   Time  Subject1  Subject2  Subject3  Subject4
#   T1    12        9         16        12
#   T2    10        8         15        12
#   T3    9         7         12        11
# Bảng tidy (long):
experiment_tidy <- data.frame(
  Time = rep(c("T1", "T2", "T3"), times = 4),
  Subject = rep(c("Subject1", "Subject2", "Subject3", "Subject4"), each = 3),
  Value = c(12, 10, 9,
            9, 8, 7,
            16, 15, 12,
            12, 12, 11)
)
experiment_tidy

#Bài tập 2.1:
grades <- read.csv("data/grades_wide.csv", header = TRUE)
head(grades)
pivot_longer(
  data = grades,
  cols = -Hoc_vien,
  names_to = "Subject",
  values_to = "Response"
)

healths <- read.csv("data/healths_wide.csv")
head(healths)
pivot_longer(
  data = healths,
  cols = starts_with("Tuan_"),
  names_to = "Tuan",
  values_to = "Gia tri"
)

sales <- read.csv("data/sales_wide.csv")
head(sales)
pivot_longer(
  data = sales,
  cols = X2021:X2023,
  names_to = "Year",
  values_to = "Number"
)