# 26/3 - 24KDL
# Chương 2.3: GIỚI THIỆU dplyr

# ─────────────────────────────────────────────
# 1. Pipeline trong tidyverse
# ─────────────────────────────────────────────

### 1.1. Pipe và pipeline --------------------------
setwd("D:/GitHub/MTH10109-Mathematical-Statistics-Laboratory/")
load("data/dplyr_lecture.RData")
head(df)
str(df)
# Biến: ID, country, gender, education, age, income, experience, score, satisfaction, married

## Pipe: toán tử chuyển kết quả của bước trước vào bước sau
# %>%  (magrittr - tidyverse)
# |>   (base R từ phiên bản 4.1)
# --> lấy đối tượng A, đưa vào hàm B, tiếp tục đưa vào hàm C, ..., thu kết quả

# Tidyverse: mỗi hàm có argument đầu tiên là data frame, input/output đều là data frame
# --> pipe làm việc nối các bước trở nên mượt mà và dễ đọc

# install.packages("dplyr")   # chạy 1 lần
library(dplyr)

# Không dùng pipe
df1 <- filter(df, age > 20)
df2 <- select(df1, country, income)
df3 <- arrange(df2, desc(income))
head(df3)

# Dùng pipe
df %>%
  filter(age > 20) %>%
  select(country, income) %>%
  arrange(desc(income)) %>%
  slice_head(n = 6)

## Nguyên tắc vận hành của pipe
# df %>% f(x)  tương đương với  f(df, x)
# --> pipe đơn giản là đưa output bên trái vào làm input đầu tiên của hàm bên phải

## Pipeline: chuỗi nhiều pipes nối với nhau
# df -> lọc (filter) -> đổi tên cột (rename) -> tạo biến mới (mutate)
#    -> nhóm và tóm tắt (group_by + summarize) -> kết quả cuối cùng
df %>%
  filter(score > 50) %>%
  mutate(log_income = log(income)) %>%
  group_by(gender) %>%
  summarize(mean_log_income = mean(log_income)) %>%
  arrange(desc(mean_log_income))

## Ưu điểm của pipeline
# - Code rõ ràng, dễ đọc, tuần tự từng bước, không lồng hàm phức tạp
# - Ít biến trung gian (không cần df1, df2, df3...)
# - Tư duy xử lý dữ liệu dạng "flow"
# - Tương thích toàn bộ tidyverse: filter(), mutate(), select(), group_by(), summarize()

## Khi nào không nên dùng pipe
# - Khi hàm cần tham số đầu tiên không phải là data frame
# - Khi phải debug một biểu thức dài và phức tạp
# - Khi cần gán vào nhiều đối tượng khác nhau

# Phím tắt tạo %>%: Ctrl + Shift + M (Windows)


# ─────────────────────────────────────────────
# 2. Các hàm cơ bản để xử lý dữ liệu
# ─────────────────────────────────────────────

### 2.1. select() - Chọn biến -----------------------
df %>% select(ID, country, gender, age, income)
# Chọn tất cả trừ một số cột
df %>% select(-satisfaction, -score)

### 2.2. filter() - Lọc dòng theo điều kiện ---------
# Nhiều điều kiện AND (phân cách bằng dấu phẩy)
df %>% filter(country == "Vietnam", age > 30)
# Nhiều điều kiện OR
df %>% filter(gender == "Female" | married == "Yes")

### 2.3. arrange() - Sắp xếp dữ liệu ----------------
df %>% arrange(age)              # tăng dần
df %>% arrange(desc(age))        # giảm dần
df %>% arrange(country, desc(age))   # sắp xếp theo nhiều biến ưu tiên

### 2.4. mutate() - Tạo biến mới hoặc thay đổi biến --
df %>%
  mutate(
    income_log = log(income),
    age_group = ifelse(age < 30, "Young", "Adult")
  )

### 2.5. group_by() và summarize() ------------------
# Chuyển data frame sang dạng "grouped" để thống kê theo nhóm
df %>% group_by(country)

# Tính chỉ số mẫu cho từng nhóm
df %>%
  group_by(country) %>%
  summarize(
    mean_income = mean(income),
    median_age = median(age),
    n = n()
  )

# Sau summarize()/mutate() theo nhóm, nên ungroup() để tránh lỗi ở pipeline sau
df %>%
  group_by(education) %>%
  summarize(mean_score = mean(score)) %>%
  ungroup()

# summarize() cũng dùng được mà không cần group_by() (tóm tắt cả bộ dữ liệu)
# Hàm hay dùng trong summarize(): sum(), mean(), median(), min(), max(), n()

### 2.6. rename() - Đổi tên biến --------------------
# Cú pháp: rename(data, new_name = old_name) -- tên mới ghi trước, dễ đọc, tránh nhầm lẫn
df %>% rename(monthly_income = income)

# Đổi tên nhiều biến cùng lúc
df %>%
  rename(
    age_years = age,
    edu_level = education,
    is_married = married
  )

# Chọn và đổi tên cùng lúc bằng select()
df %>%
  select(
    ID,
    Country = country,
    Gender = gender,
    Income = income
  )

### 2.7. slice_min() - Lấy các giá trị nhỏ nhất ------
df %>% slice_min(order_by = income, n = 5)        # 5 người thu nhập thấp nhất
df %>% slice_min(order_by = score, prop = 0.1)    # 10% dòng có score thấp nhất

# Kết hợp group_by(): 3 người trẻ nhất trong từng quốc gia
df %>%
  group_by(country) %>%
  slice_min(order_by = age, n = 3)

### 2.8. slice_max() - Lấy các giá trị lớn nhất ------
df %>% slice_max(order_by = income, n = 5)          # 5 người thu nhập cao nhất
df %>% slice_max(order_by = satisfaction, prop = 0.2)   # 20% quan sát satisfaction cao nhất

# Kết hợp group_by(): người lớn tuổi nhất trong từng mức học vấn
df %>%
  group_by(education) %>%
  slice_max(order_by = age, n = 1)


#========================================#
#Bài tập 1.1:
# (a) Lọc các quan sát có age >= 18 và income > 3000
df %>%
  filter(age >= 18) %>%
  filter(income > 3000)

# (b) Lọc ra những người có trình độ học vấn là "Master" và có thu nhập trên 5000
df %>%
  filter(education == "Master") %>%
  filter(income > 5000)

# (c) Tập trung vào các quan sát tại "Vietnam" hoặc "Thailand" và có kinh nghiệm >= 10 năm
df %>%
  filter(country %in% c("Vietnam", "Thailand")) %>%
  filter(experience >= 10)

# (d) Tìm những người dưới 30 tuổi có điểm bài kiểm tra (score) đạt trên 80 điểm
df %>%
  filter(age < 30) %>%
  filter(score > 80)

# (e) Lọc ra tất cả nữ giới, sau đó chỉ giữ lại ID và satisfaction
df %>%
  filter(gender == "Female") %>%
  select(ID, satisfaction)

# (f) Tìm những người từ 60 tuổi trở lên và đã kết hôn
df %>%
  filter(age >= 60) %>%
  filter(married == "Yes")

#Bài tập 1.2:
# (a) Chuỗi lệnh giữ lại các quan sát tại quốc gia Singapore,
#     sau đó chỉ giữ lại hai cột income và education.
# (b) Chuỗi lệnh giữ lại các quan sát có satisfaction > 0.7 và gender là "Male"
#     (tức lọc những nam giới có mức độ hài lòng trên 0.7).

#Bài tập 2.1:
counties <- readRDS("data/counties.rds")
glimpse(counties)
ncol(counties)          # số biến của dữ liệu
counties$income[1]      # giá trị đầu tiên của biến income

#Bài tập 2.2:
counties %>%
  select(state, county, population, poverty)

#Bài tập 2.3:
counties_selected <- counties %>%
  select(state, county, population,
         private_work, public_work, self_employed)

#Bài tập 2.4:
# (1) Sắp xếp các quan sát của biến public_work theo thứ tự giảm dần
counties_selected %>%
  arrange(desc(public_work))

# (2) Chỉ tìm các county có dân số trên một triệu
counties_selected %>%
  filter(population > 10^6)

# (3) Chỉ tìm các county trong tiểu bang California cũng có dân số trên một triệu
counties_selected %>%
  filter(state == "California", population > 10^6)

# (4) Lọc các county trong tiểu bang Texas có hơn mười nghìn người (10000) và
#     sắp xếp giảm dần theo tỷ lệ phần trăm người làm việc trong khu vực tư nhân
counties_selected %>%
  filter(state == "Texas", population > 10000) %>%
  arrange(desc(private_work))

#Bài tập 2.5:
counties_selected %>% 
  mutate(public_workers = as.integer(public_work * population / 100)) %>% 
  arrange(desc(public_workers))

#Bài tập 2.6:
counties %>%
  select(state, county, population, men, women) %>%
  mutate(proportion_women = women / population * 100)

#Bài tập 2.7:
counties_men <- counties %>% 
  mutate(proportion_men = men / population * 100) %>% 
  select(state, county, population, proportion_men) %>% 
  filter(population >= 10000) %>% 
  arrange(desc(proportion_men))
counties_men[1,]

#Bài tập 2.8:
counties_selected <- counties %>%
  select(county, population, income, unemployment)
counties_selected %>%
  summarize(
    min_population = min(population),
    max_unemployment = max(unemployment),
    average_income = mean(income)
  )

#Bài tập 2.9:
counties_selected <- counties %>%
  select(state, county, population, land_area)
counties_selected %>% 
  group_by(state) %>% 
  summarize(
    total_area = sum(land_area),
    total_population = sum(population)
  ) %>% 
  ungroup() %>% 
  mutate(density = total_population / total_area) %>% 
  arrange(desc(density))

#Bài tập 2.10:
counties_selected <- counties %>%
  select(region, state, county, population)

# (1) Tổng dân số theo từng tổ hợp region + state
by_region_state <- counties_selected %>%
  group_by(region, state) %>%
  summarize(total_pop = sum(population))
by_region_state

# (2) Dân số trung bình và trung vị của các state trong từng region
by_region_state %>%
  ungroup() %>%
  group_by(region) %>%
  summarize(
    average_pop = mean(total_pop),
    median_pop = median(total_pop)
  )

# Cách khác: vì summarize() tự bỏ group theo biến cuối (state), giữ lại các biến đầu (region)
counties_selected %>% 
  group_by(region, state) %>% 
  summarize(
    total_pop = sum(population)
  ) %>% 
  summarize(
    average_pop = mean(total_pop),
    median_pop = median(total_pop)
  )