# 11/6 - 24KDL
# Chương 5: KIỂM ĐỊNH GIẢ THUYẾT THỐNG KÊ

library(ggplot2)
library(dplyr)

# ─────────────────────────────────────────────
# 1. Các nguyên tắc chính khi kiểm định giả thuyết thống kê
# ─────────────────────────────────────────────

# Quy trình kiểm định giả thuyết:
# 1. Phát biểu rõ H0 và H1
# 2. Chọn kiểm định phù hợp với dữ liệu và giả định
# 3. So sánh p-value với alpha để ra quyết định thống kê

# H0: giả thuyết "chuẩn", "không khác biệt", "không có hiệu ứng" (bị kiểm tra)
# H1 (Ha): điều nhà nghiên cứu quan tâm kiểm chứng

# Quy tắc ra quyết định (với mức ý nghĩa alpha, thường = 0.05):
#   Nếu p-value < alpha  => Bác bỏ H0
#   Nếu p-value >= alpha => Không bác bỏ H0

# Diễn giải p-value:
# p-value KHÔNG phải là "xác suất H0 đúng".
# p-value = P(dữ liệu quan sát được hoặc cực đoan hơn | H0 đúng)
# "Bác bỏ H0" khác "H0 sai tuyệt đối"; "Không bác bỏ H0" khác "chấp nhận H0"

# Một bài toán kiểm định cần có các bước:
# 1. Mô tả & ước lượng tham số
# 2. Trực quan dữ liệu
# 3. Phát biểu giả thuyết
# 4. Chọn kiểm định phù hợp (kiểm tra điều kiện)
# 5. Diễn giải kết quả


# ─────────────────────────────────────────────
# 2. Kiểm định giả thuyết về trung bình mu
# ─────────────────────────────────────────────

## 2.1. One-sample t-test
# Hai phía:      H0: mu = mu0    H1: mu != mu0   -> t.test(x, mu=mu0, alternative="two.sided")
# Một phía phải: H0: mu <= mu0   H1: mu > mu0    -> t.test(x, mu=mu0, alternative="greater")
# Một phía trái: H0: mu >= mu0   H1: mu < mu0    -> t.test(x, mu=mu0, alternative="less")

# Ví dụ 2.1: Giá trung bình kim cương có khác 4000 USD không?

# Bước 1: Ước lượng tham số
diamonds %>%
  summarise(
    mean_price = mean(price),
    sd_price = sd(price),
    n = n()
  )

# Bước 2: Trực quan phân phối
ggplot(diamonds, aes(x = price)) +
  geom_histogram(fill = "steelblue", color = "white") +
  geom_vline(xintercept = 4000, linetype = "dashed",
             color = "red", linewidth = 1) +
  annotate("text", x = 5000, y = 4000,
           label = "Giá trị trung bình\nmuốn kiểm định",
           size = 5, hjust = 0) +
  annotate("segment", x = 4800, xend = 4000, y = 3950, yend = 3500,
           arrow = arrow(length = unit(0.3, "cm"))) +
  labs(title = "Histogram của giá kim cương",
       x = "Giá", y = "Tần số") +
  theme_bw()

# Bước 3: Phát biểu giả thuyết
# H0: mu = 4000  vs  H1: mu != 4000
# H0: mu <= 4000 vs  H1: mu > 4000
# H0: mu >= 4000 vs  H1: mu < 4000

# Bước 4: Thực hiện kiểm định và diễn giải
t.test(diamonds$price, mu = 4000)$p.value
# t = -3.9121, df = 53939, p-value = 9.159e-05
# --> p-value rất nhỏ, bác bỏ H0. Khoảng tin cậy 95%: [3899.13, 3966.47], không chứa 4000
#     --> Giá trung bình khác có ý nghĩa thống kê so với 4000 USD.


## 2.2. Two-sample t-test
# Hai phía:      H0: mu1 = mu2   H1: mu1 != mu2  -> t.test(x ~ group, alternative="two.sided")
# Một phía phải: H0: mu1 <= mu2  H1: mu1 > mu2   -> t.test(x ~ group, alternative="greater")
# Một phía trái: H0: mu1 >= mu2  H1: mu1 < mu2   -> t.test(x ~ group, alternative="less")
# x: biến định lượng ; group: biến phân loại (2 levels)


## 2.3. Paired t-test
# Hai phía:      H0: mu_d = 0   H1: mu_d != 0  -> t.test(x1, x2, paired=TRUE, alternative="two.sided")
# Một phía phải: H0: mu_d <= 0  H1: mu_d > 0   -> t.test(x1, x2, paired=TRUE, alternative="greater")
# Một phía trái: H0: mu_d >= 0  H1: mu_d < 0   -> t.test(x1, x2, paired=TRUE, alternative="less")
# paired=TRUE: tạo di = x1i - x2i rồi thực hiện one-sample t-test trên d

# Ví dụ 2.2: Bộ dữ liệu sleep - thuốc 2 có làm tăng thời gian ngủ hơn thuốc 1 không?
# H0: mu_d >= 0  vs  H1: mu_d < 0 , với d = extra1 - extra2

sleep %>%
  arrange(ID, group) %>%
  group_by(ID) %>%
  summarise(d = diff(extra)) %>%
  summarise(mu_d_hat = mean(d))

sleep %>%
  arrange(ID, group) %>%
  group_by(ID) %>%
  summarise(d = diff(extra)) %>%
  ggplot(aes(x = d)) +
  geom_histogram(bins = 30,
                 fill = "steelblue",
                 color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Phân phối sai khác d = extra(group 2) - extra(group 1)",
    x = "Sai khác d",
    y = "Tần số"
  ) +
  theme_minimal()

t.test(sleep$extra[sleep$group == 1],
       sleep$extra[sleep$group == 2],
       paired = TRUE, alternative = "less")
# t = -4.0621, df = 9, p-value = 0.001416
# --> p-value < 0.05, bác bỏ H0. Có bằng chứng thuốc 2 làm tăng thời gian ngủ hơn thuốc 1.


# ─────────────────────────────────────────────
# 3. Kiểm định giả thuyết về tỷ lệ p
# ─────────────────────────────────────────────

# prop.test(x, n, p, alternative = c("two.sided","less","greater"))
# x: số lần thành công ; n: kích thước mẫu ; p: tỷ lệ p0 trong H0: p = p0
# Hoặc: prop.test(table, p, alternative = ...) với table là vector 2 quan sát

# Ví dụ 3.1: Tỷ lệ kim cương loại Ideal có bằng 40% không?
# H0: p = 0.4  vs  H1: p != 0.4

# Tính tỷ lệ
diamonds %>%
  mutate(is_Ideal = cut == "Ideal") %>%
  summarise(
    total = n(),
    Ideal = sum(is_Ideal),
    p_hat = mean(is_Ideal)
  )

diamonds %>%
  summarise(p_hat = mean(cut == "Ideal"))

# Trực quan dữ liệu
diamonds %>%
  mutate(is_Ideal = cut == "Ideal") %>%
  count(is_Ideal) %>% # summarize(n = n())
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = is_Ideal, y = prop, fill = is_Ideal)) +
  geom_col(width = 0.9) +
  geom_hline(yintercept = 0.4, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Cut = Ideal?",
    y = "Tỷ lệ",
    title = "Tỷ lệ kim cương Ideal trong dữ liệu diamonds"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Thực hiện kiểm định
x <- sum(diamonds$cut == "Ideal")
n <- nrow(diamonds)
prop.test(x, n, p = 0.4)

# Dùng table
tab <- table(diamonds$cut == "Ideal")
prop.test(tab, p = 0.6)

# Ví dụ 3.2: Tỷ lệ Ideal giữa color E và G có bằng nhau không?
# H0: pE = pG  vs  H1: pE != pG
prop_EG <- diamonds %>%
  filter(color %in% c("E", "G")) %>%
  mutate(is_Ideal = cut == "Ideal") %>%
  group_by(color) %>%
  summarise(
    total = n(),
    Ideal = sum(is_Ideal),
    p_hat = mean(is_Ideal)
  )
prop_EG

ggplot(prop_EG, aes(x = color, y = p_hat, fill = color)) +
  geom_col(width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Color",
    y = "Tỷ lệ Ideal",
    title = "So sánh tỷ lệ kim cương Ideal giữa hai nhóm màu E và G"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

tab <- table(diamonds$cut == "Ideal", diamonds$color)[, c("E", "G")]
prop.test(tab)


# ─────────────────────────────────────────────
# 4. Kiểm định chi bình phương (Chi-square test)
# ─────────────────────────────────────────────

## 4.1. Cơ sở toán học
# Q = sum( (Xi - n*pi0)^2 / (n*pi0) )
# Nếu H0 đúng, Q ~ Chi-square(df = k-1)

## 4.2.1. Kiểm định sự phù hợp (Goodness-of-Fit Test)
# Ví dụ 4.1: số liệu thực nghiệm (53,20,19,8) có theo tỷ lệ lý thuyết (9,3,3,1) không?
thuc_nghiem <- c(53, 20, 19, 8)
ly_thuyet <- c(9, 3, 3, 1)
prob_ly_thuyet <- ly_thuyet / sum(ly_thuyet)
chisq.test(thuc_nghiem, p = prob_ly_thuyet)
# X-squared = 0.7644, df = 3, p-value = 0.858 --> Không bác bỏ H0

## 4.2.2. Kiểm định tính đồng nhất (Test of Homogeneity)
# Ví dụ 4.2: đối chiếu cấu trúc mẫu A và B
table_ex <- data.frame(A = c(8,19,20,53), B = c(12,38,40,110))
table_ex
chisq.test(table_ex)
# X-squared = 0.44908, df = 3, p-value = 0.9299 --> hai mẫu đồng nhất

## 4.2.3. Kiểm định tính độc lập (Test of Independence)
# Ví dụ 4.3: mối liên hệ giữa cut và color trong diamonds
table(diamonds$cut, diamonds$color)
chisq.test(table(diamonds$cut, diamonds$color))
# X-squared = 310.32, df = 24, p-value < 2.2e-16 --> bác bỏ H0, cut và color không độc lập


# ─────────────────────────────────────────────
# 5. Một số lưu ý
# ─────────────────────────────────────────────

# Kiểm định trung bình: t.test()
# Kiểm định tỷ lệ: prop.test()
# Kiểm định biến định tính: chisq.test()
# Các kiểm định khác: var.test(), shapiro.test(), ks.test(), fisher.test()

# Nguyên tắc chung: p-value < alpha => bác bỏ H0
# Luôn kết hợp: tính toán - trực quan - kiểm định


# ─────────────────────────────────────────────
# BÀI TẬP
# ─────────────────────────────────────────────

#========================================#
#Exercise 2.1: Diễn giải kết quả 2 kiểm định một phía về giá kim cương

t.test(diamonds$price, mu = 4000, alternative = "greater")
# t = -3.9121, df = 53939, p-value = 0.9999542
# H0: mu <= 4000  vs  H1: mu > 4000
# --> p-value gần bằng 1 nên không bác bỏ được H0. Không có gì bất ngờ vì mean(price)
#     = 3932.8 đã nhỏ hơn 4000 rồi, nên dữ liệu không thể ủng hộ chiều "lớn hơn 4000".

t.test(diamonds$price, mu = 4000, alternative = "less")
# t = -3.9121, df = 53939, p-value = 4.58e-05
# H0: mu >= 4000  vs  H1: mu < 4000
# --> p-value = 4.58e-05 < 0.05, bác bỏ H0. Có bằng chứng cho thấy giá trung bình
#     thực sự nhỏ hơn 4000 USD.

#========================================#
#Exercise 2.2: Giá TB kim cương cut=Ideal và cut=Premium có khác nhau không?

# 1. Tính trung bình giá
diamonds %>%
  filter(cut %in% c("Ideal", "Premium")) %>%
  group_by(cut) %>%
  summarise(mean_price = mean(price), sd_price = sd(price), n = n())

# 2. Trực quan phân phối trên cùng plot
diamonds %>%
  filter(cut %in% c("Ideal", "Premium")) %>%
  ggplot(aes(x = price, fill = cut)) +
  geom_density(alpha = 0.4) +
  labs(title = "Phân phối giá kim cương: Ideal vs Premium",
       x = "Giá", y = "Mật độ", fill = "Cut") +
  theme_minimal()

# 3. Kiểm định giả thuyết
# H0: mu_Ideal = mu_Premium  vs  H1: mu_Ideal != mu_Premium
df_ip <- diamonds %>% filter(cut %in% c("Ideal", "Premium")) %>%
  mutate(cut = droplevels(cut))
t.test(price ~ cut, data = df_ip)
# Welch t = 24.918, df = 26552, p-value < 2.2e-16
# mean(Premium) = 4584.26 ; mean(Ideal) = 3457.54 ; 95% CI hiệu số = [1038.09, 1215.34]
# --> p-value cực nhỏ, bác bỏ H0. Giá trung bình Premium cao hơn Ideal khoảng 1127 USD,
#     chênh lệch này có ý nghĩa thống kê rất rõ.

#========================================#
#Exercise 2.3 (Bài 5.101): Tỷ lệ ăn mòn hai dung dịch

sample1 <- c(9.9, 10.6, 9.4, 10.3, 9.3, 10.0, 9.6, 10.3, 10.2, 10.10)
sample2 <- c(10.2, 10.0, 10.6, 10.2, 10.7, 10.7, 10.4, 10.4, 10.5, 10.3)

# 1. Tạo dataframe
df_etch <- data.frame(
  sample = rep(c(1, 2), each = 10),
  value = c(sample1, sample2)
)
df_etch

# 2. Mean và sd từng mẫu
df_etch %>%
  group_by(sample) %>%
  summarise(mean_val = mean(value), sd_val = sd(value))

# 3. Vẽ phân phối
ggplot(df_etch, aes(x = value, fill = factor(sample))) +
  geom_density(alpha = 0.4) +
  labs(title = "Phân phối tỷ lệ ăn mòn theo từng dung dịch",
       x = "Tỷ lệ ăn mòn (mils/phút)", fill = "Mẫu") +
  theme_minimal()

# 4. Kiểm định: H0: mu1 = mu2  vs  H1: mu1 != mu2
t.test(sample1, sample2, var.equal = TRUE)
# t = -2.8278, df = 18, p-value = 0.01115
# mean(sample1) = 9.97 ; mean(sample2) = 10.40
# --> p-value = 0.01115 < 0.05, bác bỏ H0. Tỷ lệ ăn mòn trung bình của hai dung dịch
#     khác nhau có ý nghĩa thống kê (dung dịch 2 ăn mòn nhanh hơn trung bình ~0.43 mils/phút).

#========================================#
#Exercise 2.4: Diễn giải kết quả paired t-test thuốc ngủ (đã thực hiện ở Ví dụ 2.2)
# Kết quả: t = -4.0621, df = 9, p-value = 0.001416
# H0: mu_d >= 0 (thuốc 2 không làm tăng giấc ngủ hơn thuốc 1)
# H1: mu_d < 0  (thuốc 2 làm tăng giấc ngủ hơn thuốc 1)
# --> p-value = 0.0014 < 0.05: Bác bỏ H0.
# Kết luận: Có bằng chứng thống kê cho thấy thuốc 2 làm tăng thời gian ngủ trung bình
#           nhiều hơn thuốc 1 (mean difference = -1.58, nghĩa là extra2 > extra1 trung bình 1.58).

#========================================#
#Exercise 2.5: Cân nặng gà con (ChickWeight) từ ngày 2 đến ngày 10

# 1. Lọc Time = 2 và Time = 10, giữ gà có đủ 2 quan sát
chick_2_10 <- ChickWeight %>%
  filter(Time %in% c(2, 10)) %>%
  group_by(Chick) %>%
  filter(n() == 2) %>%
  arrange(Chick, Time) %>%
  summarise(d = diff(weight))

# (a) Trung bình của d
chick_2_10 %>% summarise(mean_d = mean(d))

# (b) Vẽ phân phối của d
ggplot(chick_2_10, aes(x = d)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Phân phối sai khác cân nặng (ngày 10 - ngày 2)",
       x = "Sai khác d", y = "Tần số") +
  theme_minimal()

# 2. Kiểm định giả thuyết: H0: mu_d <= 0  vs  H1: mu_d > 0
t.test(chick_2_10$d, mu = 0, alternative = "greater")
# t = 17.732, df = 48, p-value < 2.2e-16 ; mean(d) = 58.33
# --> p-value cực nhỏ, bác bỏ H0. Cân nặng gà con tăng có ý nghĩa thống kê rất mạnh
#     từ ngày 2 đến ngày 10 (trung bình tăng khoảng 58.3g).

# 3. Kiểm tra từ ngày 10 đến ngày 12
chick_10_12 <- ChickWeight %>%
  filter(Time %in% c(10, 12)) %>%
  group_by(Chick) %>%
  filter(n() == 2) %>%
  arrange(Chick, Time) %>%
  summarise(d = diff(weight))

t.test(chick_10_12$d, mu = 0, alternative = "greater")
# t = 12.619, df = 48, p-value < 2.2e-16 ; mean(d) = 21.41
# --> Cũng bác bỏ H0: cân nặng vẫn tăng có ý nghĩa thống kê từ ngày 10 đến ngày 12,
#     nhưng mức tăng trung bình (~21.4g) nhỏ hơn hẳn giai đoạn ngày 2-10 (~58.3g),
#     cho thấy tốc độ tăng trưởng của gà con chậm dần khi lớn hơn.

#========================================#
#Exercise 3.1: Tính tỷ lệ, trực quan, kiểm định 4 giả thuyết

# (1) mtcars: tỷ lệ xe số tự động (am=0) có lớn hơn 50% không?
mtcars %>% summarise(p_hat = mean(am == 0))

ggplot(mtcars, aes(x = factor(am == 0))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), fill = "steelblue") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Tỷ lệ xe số tự động", x = "Số tự động (am=0)?", y = "Tỷ lệ") +
  theme_minimal()

x_am <- sum(mtcars$am == 0)
n_am <- nrow(mtcars)
prop.test(x_am, n_am, p = 0.5, alternative = "greater")
# X-squared = 0.78125, df = 1, p-value = 0.1884 ; p_hat = 19/32 = 0.594
# --> p-value = 0.1884 > 0.05, không bác bỏ được H0. Dù tỷ lệ mẫu (59.4%) cao hơn 50%,
#     nhưng cỡ mẫu n=32 khá nhỏ nên chưa đủ bằng chứng thống kê để khẳng định
#     tỷ lệ xe số tự động thực sự lớn hơn 50%.

# (2) Titanic: tỷ lệ sống sót có nhỏ hơn 35% không?
titanic_df <- as.data.frame(Titanic)
survived_yes <- sum(titanic_df$Freq[titanic_df$Survived == "Yes"])
total_titanic <- sum(titanic_df$Freq)
survived_yes / total_titanic

prop.test(survived_yes, total_titanic, p = 0.35, alternative = "less")
# X-squared = 6.9166, df = 1, p-value = 0.00427 ; p_hat = 711/2201 = 0.323
# --> p-value = 0.00427 < 0.05, bác bỏ H0. Có bằng chứng cho thấy tỷ lệ sống sót
#     thực sự nhỏ hơn 35%.

# (3) mtcars: tỷ lệ số sàn (am=1) giữa nhóm 4 xy-lanh và 6 xy-lanh có khác nhau không?
mtcars %>%
  filter(cyl %in% c(4, 6)) %>%
  group_by(cyl) %>%
  summarise(p_hat = mean(am == 1), n = n())

tab_cyl <- mtcars %>%
  filter(cyl %in% c(4, 6)) %>%
  mutate(is_manual = am == 1) %>%
  count(cyl, is_manual) %>%
  tidyr::pivot_wider(names_from = is_manual, values_from = n, values_fill = 0)
tab_cyl

prop.test(x = c(sum(mtcars$am[mtcars$cyl==4]==1), sum(mtcars$am[mtcars$cyl==6]==1)),
          n = c(sum(mtcars$cyl==4), sum(mtcars$cyl==6)))
# X-squared = 0.59504, df = 1, p-value = 0.4405
# p_hat(4 xy-lanh) = 8/11 = 0.727 ; p_hat(6 xy-lanh) = 3/7 = 0.429
# --> p-value = 0.4405 > 0.05, không bác bỏ H0: chưa đủ bằng chứng cho thấy tỷ lệ xe
#     số sàn khác nhau giữa nhóm 4 và 6 xy-lanh. Lưu ý R có cảnh báo "Chi-squared
#     approximation may be incorrect" vì cỡ mẫu mỗi nhóm quá nhỏ (11 và 7 xe),
#     nên kết quả này chỉ mang tính tham khảo, không nên diễn giải quá chắc chắn.

# (4) ToothGrowth: tỷ lệ răng dài (>20) giữa supp=VC và supp=OJ có khác nhau không?
ToothGrowth %>%
  group_by(supp) %>%
  summarise(p_hat = mean(len > 20), n = n())

prop.test(x = c(sum(ToothGrowth$len[ToothGrowth$supp=="VC"] > 20),
                sum(ToothGrowth$len[ToothGrowth$supp=="OJ"] > 20)),
          n = c(sum(ToothGrowth$supp=="VC"), sum(ToothGrowth$supp=="OJ")))
# X-squared = 3.2812, df = 1, p-value = 0.07008
# p_hat(VC) = 10/30 = 0.333 ; p_hat(OJ) = 18/30 = 0.6
# --> p-value = 0.07008, hơi lớn hơn 0.05 nên không bác bỏ H0 ở mức ý nghĩa 5%.
#     Tuy nhiên p-value khá gần ngưỡng 0.05 và chênh lệch tỷ lệ khá lớn (33% vs 60%),
#     nên có thể xem đây là dấu hiệu khác biệt chưa đủ mạnh về mặt thống kê với cỡ mẫu này,
#     chứ không hẳn là "không có khác biệt gì".

#========================================#
#Exercise (Optional): Datasaurus Dozen - vai trò của trực quan hóa

library(datasauRus)

datasaurus_dozen %>%
  group_by(dataset) %>%
  summarise(
    mean_x = mean(x),
    sd_x = sd(x),
    mean_y = mean(y),
    sd_y = sd(y)
  )

ggplot(datasaurus_dozen, aes(x = x, y = y, color = dataset)) +
  geom_point(
    alpha = 0.7,
    size = 1.5
  ) +
  facet_wrap(~ dataset, ncol = 4) +
  labs(
    title = "Cùng thống kê mô tả, nhưng phân phối hoàn toàn khác nhau",
    subtitle = "Bộ dữ liệu Datasaurus Dozen minh họa vai trò của trực quan",
    x = "x",
    y = "y",
    caption = "Nguồn: gói datasauRus"
  ) +
  theme_bw()
# --> Nhận xét: dù mean/sd của x,y gần như giống hệt nhau giữa các dataset,
#     hình dạng phân phối (scatter plot) lại hoàn toàn khác nhau.
#     Kết luận: không nên chỉ dựa vào số liệu thống kê mô tả (mean, sd) mà bỏ qua
#     bước trực quan hóa dữ liệu, vì thống kê tóm tắt có thể "giấu" cấu trúc thật
#     của dữ liệu.