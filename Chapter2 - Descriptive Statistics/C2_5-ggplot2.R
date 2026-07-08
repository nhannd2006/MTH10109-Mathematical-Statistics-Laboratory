# 1/4 - 24KDL
# Chương 2.5: GRAMMAR OF GRAPHICS VÀ ggplot2

# ─────────────────────────────────────────────
# 1. ggplot2
# ─────────────────────────────────────────────

### 1.1. Giới thiệu --------------------------------
# install.packages("tidyverse")        # chạy 1 lần
# install.packages("palmerpenguins")   # chạy 1 lần
library(tidyverse)          # ggplot2 nằm trong tidyverse
library(palmerpenguins)     # chứa penguins dataset

### 1.2. Grammar of Graphics ------------------------
# (1) Data: phải ở dạng Tidy Data
# (2) Aesthetics (aes - ánh xạ thuộc tính): kết nối giữa dữ liệu và thị giác
#     x, y      : vị trí trên trục tọa độ
#     color/fill: màu sắc
#     size      : kích thước
#     shape     : hình dạng
# (3) Geometries (geom - hình dạng biểu diễn): thực thể hình học xuất hiện trên biểu đồ
#     geom_point() : biểu đồ phân tán
#     geom_bar()   : biểu đồ cột
#     geom_line()  : biểu đồ đường
# (4) Facets (phân mảnh): chia biểu đồ tổng thể thành nhiều biểu đồ con
# (5) Scales (thang đo): kiểm soát cách dữ liệu chuyển đổi sang thuộc tính trực quan
# (6) Coordinate System: không gian dữ liệu được vẽ lên
#     Cartesian: hệ trục x, y vuông góc thông thường
#     Polar    : hệ trục cực (biểu đồ tròn/mạng nhện)
# (7) Theme (giao diện): phục vụ thẩm mỹ - font, màu nền, gridlines, vị trí legend...

### 1.3. Các thao tác cơ bản -------------------------
## (1) Tạo một plot
ggplot(data = penguins)   # biểu đồ trống, chưa có mapping

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)   # đã ánh xạ x, y nhưng chưa có geom nên vẫn trống

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()   # thêm 1 lớp điểm -> biểu đồ phân tán

## (2) Thêm các layers và các yếu tố aesthetics khác
# Ánh xạ màu theo species (global aes)
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point()

# Thêm đường hồi quy tuyến tính (geom mới, riêng theo từng loài vì color kế thừa từ global)
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = 'lm')

# Chỉ định color = species cục bộ (local) cho geom_point() -> chỉ 1 đường smooth chung
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm")

# Thêm shape cho từng loài
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm")

# Thêm nhãn bằng labs()
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  )

## (3) Một số yếu tố aes khác thường gặp
# color (hoặc colour): màu đường viền/điểm/đường - dùng cho biến categorical
# fill               : màu bên trong hình - dùng cho geom có diện tích (bar, boxplot, histogram, density, area)
# size               : kích thước - phù hợp cho biến continuous
# shape              : hình dạng điểm - nên dùng cho biến categorical ít mức (<= 6)
# alpha              : độ trong suốt - giảm overplotting, đặt ngoài aes() nếu là hằng số
# linetype           : kiểu đường (liền/đứt/chấm) - dùng với geom_line(), geom_smooth()
# group              : gom quan sát thành nhóm - không hiển thị trực tiếp nhưng ảnh hưởng hình vẽ

# Lưu ý:
# mapping trong aes(): biến dữ liệu -> thuộc tính trực quan
# setting ngoài aes() : giá trị cố định cho toàn bộ geom

### 1.4. Hai cách gọi hàm ggplot() -------------------
# Cách 1: gọi với tham số data trực tiếp
# ggplot(data = df, aes(x = xvar, y = yvar)) + geom_xxx()

# Cách 2: dùng ggplot() trong pipeline (%>%), sau các bước xử lý dplyr
# df %>%
#   dplyr::verb_1() %>%
#   dplyr::verb_2() %>%
#   ggplot(aes(x = xvar, y = yvar)) +
#   geom_xxx()


# ─────────────────────────────────────────────
# 2. Trực quan hóa dữ liệu 1 biến
# ─────────────────────────────────────────────

### 2.1. Biến rời rạc (discrete) ---------------------
# Biểu đồ cột để kiểm tra phân bố của biến rời rạc
ggplot(penguins, aes(x = species)) +
  geom_bar()

# Sắp xếp cột theo tần suất: chuyển biến thành factor và sắp lại mức
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

### 2.2. Biến liên tục (continuous) ------------------
## Histogram
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)   # cân bằng hợp lý

# So sánh các giá trị binwidth khác nhau
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)    # quá hẹp -> quá nhiều cột
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)  # quá cao -> chỉ còn vài cột

## Density plot
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density(color = "blue")

## Kết hợp geom_histogram() và geom_density()
# Cần đưa 2 lớp về cùng thang đo (density) bằng after_stat(density)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(aes(y = after_stat(density))) +
  geom_density(color = "blue", linewidth = 2)


# ─────────────────────────────────────────────
# 3. Trực quan hóa dữ liệu 2 biến
# ─────────────────────────────────────────────

### 3.1. Một biến categorical + một biến continuous --
# categorical: chia nhóm | continuous: so sánh phân bố/mức độ giữa các nhóm

## Nguyên tắc 1: Side-by-side comparison (so sánh song song)
# Boxplot: median, IQR, outliers
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()

# Violin plot: nhấn mạnh hình dạng phân bố
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_violin()

# Kết hợp boxplot + violin
ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  geom_violin(alpha = 0.3)

## Nguyên tắc 2: Tách nhóm bằng aesthetics (color/fill)
# Histogram + fill theo nhóm, chồng lên nhau
ggplot(penguins, aes(x = body_mass_g, fill = species)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 30)

# Density + color theo nhóm
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 1)

### 3.2. Hai biến categorical -------------------------
# position: 'stack' (tổng + thành phần), 'dodge' (số lượng từng nhóm), 'fill' (tỷ lệ)
ggplot(penguins, aes(x = species, fill = sex)) +
  geom_bar(position = "stack")
ggplot(penguins, aes(x = species, fill = sex)) +
  geom_bar(position = "dodge")
ggplot(penguins, aes(x = species, fill = sex)) +
  geom_bar(position = "fill")

### 3.3. Hai biến continuous --------------------------
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()


#========================================#
#Bài tập 1.1:
glimpse(penguins)
?penguins
# 1. Số dòng/biến: xem trong glimpse() (Rows/Columns)
# 2. bill_depth_mm: độ sâu (bề dày) của mỏ chim cánh cụt, tính bằng mm (xem ?penguins)
# 3. species: loài chim cánh cụt; flipper_length_mm: chiều dài vây (mm);
#    body_mass_g: khối lượng cơ thể (g)

#Bài tập 1.2:
# (1) Biểu đồ phân tán giữa bill_depth_mm (y) và bill_length_mm (x)
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  labs(
    title = "Bill length & Bill depth (mm)",
    subtitle = "Data from palmerpenguins package"
  ) +
  theme_minimal()
# Nhận xét: mối quan hệ không rõ ràng theo 1 xu hướng chung (có vẻ âm nhẹ),
# nhưng nếu tách theo species sẽ thấy quan hệ dương rõ ràng hơn trong từng loài.

# (2) Biểu đồ phân tán giữa species và bill_depth_mm
ggplot(penguins, aes(x = species, y = bill_depth_mm)) +
  geom_point() +
  labs(
    title = "Scatterplot: Species & Bill Depth",
    x = "Loài", y = "Bill Depth (mm)"
  ) +
  theme_minimal()
# species là biến categorical nên geom_point() không phù hợp (điểm chồng lên nhau theo cột) ->
# nên dùng geom_boxplot() hoặc geom_jitter() thay vì geom_point()

# (3) ggplot(data = penguins) + geom_point() báo lỗi vì chưa khai báo aes(x = ..., y = ...)
# Khắc phục: thêm mapping = aes(x = ..., y = ...) cho ggplot() hoặc cho geom_point()

# (4) na.rm trong geom_point(): quyết định cách xử lý giá trị NA khi vẽ.
# Mặc định na.rm = FALSE (sẽ có warning về các dòng bị loại vì missing values).
# na.rm = TRUE: loại NA khỏi dữ liệu khi vẽ mà không báo warning.
ggplot(penguins, aes(flipper_length_mm, body_mass_g)) +
  geom_point(na.rm = TRUE) +
  labs(title = "Biểu đồ dùng na.rm = TRUE sẽ không còn warning")

# (5) Thêm caption
ggplot(penguins, aes(flipper_length_mm, body_mass_g)) +
  geom_point(na.rm = TRUE) +
  labs(
    title = "Quan hệ giữa Flipper Length và Body Mass",
    caption = "Dữ liệu đến từ gói palmerpenguins.",
    x = "Flipper Length", y = "Body Mass"
  ) +
  theme_minimal()

# (6) Tạo lại hình ảnh: color theo bill_depth_mm (biến liên tục) + geom_smooth
ggplot(penguins, aes(flipper_length_mm, body_mass_g)) +
  geom_point(aes(color = bill_depth_mm), na.rm = TRUE) +
  geom_smooth(method = "lm")
# Nếu color là biến định lượng: thang màu là gradient liên tục (đậm nhạt), không phải các màu rời rạc như biến phân loại
# Nếu đổi color thành shape cho bill_depth_mm: sẽ báo lỗi/warning vì shape chỉ dùng được cho biến rời rạc, không dùng được cho biến liên tục

# (7) So sánh 2 plot
# Plot thứ nhất
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth()
# Plot thứ hai
ggplot() +
  geom_point(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_smooth(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g))
# Hai plot cho kết quả giống nhau, nhưng cách hoạt động khác nhau:
# - Plot 1: data + mapping khai báo ở ggplot() (global) -> mọi geom phía sau tự động kế thừa.
# - Plot 2: ggplot() chỉ tạo khung tranh (canvas) rỗng, mỗi geom phải tự khai báo data/mapping riêng (local).
#   Cách 2 hữu ích khi các geom cần dùng data khác nhau.

#Bài tập 1.3:
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point()

penguins %>%
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point()

#Bài tập 2.1:
# (1) Gán species cho thuộc tính trục y
ggplot(penguins, aes(y = species)) +
  geom_bar()
# Biểu đồ cột nằm ngang thay vì thẳng đứng, dễ đọc tên loài hơn khi tên dài

# (2)
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")   # chỉ tô viền cột màu đỏ
ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")    # tô toàn bộ phần thân cột màu đỏ
# fill hữu ích hơn để đổi màu cột vì geom_bar() là geom có diện tích

# (3) bins trong geom_histogram() quy định số lượng cột (khoảng chia) của histogram.
# Mặc định bins = 30.

# (4)
data("diamonds")
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.1)
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.5)
# binwidth = 0.01 cho thấy nhiều mẫu thú vị nhất (các đỉnh nhọn tại carat tròn số như 0.3, 0.5, 1, 1.5, 2...)
# do xu hướng làm tròn carat khi cắt kim cương

#Bài tập 3.1:
# (1) So sánh phân bố body_mass_g giữa species theo 2 phương pháp
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()
ggplot(penguins, aes(x = body_mass_g, fill = species)) +
  geom_density(alpha = 0.4)

# (2) Mối quan hệ giữa bill_length_mm và bill_depth_mm, có khác nhau theo species không
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", na.rm = TRUE)
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", na.rm = TRUE)
# Khi gộp chung, quan hệ có vẻ âm; nhưng khi tách theo species, mỗi loài đều có quan hệ dương rõ ràng
# (đây là ví dụ về nghịch lý Simpson - Simpson's paradox)

# (3) So sánh body_mass_g giữa các nhóm sex
ggplot(penguins, aes(x = sex, y = body_mass_g)) +
  geom_boxplot(na.rm = TRUE)

#Bài tập 3.2:
# (1) Quy mô và cơ cấu giới tính theo loài -> cột chồng (stack)
ggplot(penguins, aes(x = species, fill = sex)) +
  geom_bar(position = "stack")

# (2) So sánh số lượng giới tính giữa các loài -> cột kề (dodge)
ggplot(penguins, aes(x = species, fill = sex)) +
  geom_bar(position = "dodge")

# (3) Tỷ lệ giới tính trong từng loài -> cột tỷ lệ (fill)
ggplot(penguins, aes(x = species, fill = sex)) +
  geom_bar(position = "fill")

# (4) Phân bố loài theo từng đảo -> cột chồng
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "stack")

# (5) So sánh số lượng loài giữa các đảo -> cột kề
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "dodge")

# (6) Tỷ lệ loài trên từng đảo -> cột tỷ lệ
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")