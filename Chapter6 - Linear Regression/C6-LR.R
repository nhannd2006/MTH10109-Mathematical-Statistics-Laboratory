# 18/6 - 24KDL
# Chương 6: HỒI QUY TUYẾN TÍNH ĐƠN (OLS)

library(dplyr)
library(ggplot2)

# ─────────────────────────────────────────────
# 1. Mô hình hồi quy tuyến tính đơn
# ─────────────────────────────────────────────

# Mô hình: Y = beta0 + beta1*X + epsilon
# Y: biến phụ thuộc (response) ; X: biến độc lập (predictor)
# beta0: hệ số chặn (Intercept) ; beta1: hệ số góc (Slope)
# epsilon: sai số ngẫu nhiên
# beta1: mức thay đổi trung bình của Y khi X tăng 1 đơn vị
# Đường hồi quy: đường "tốt nhất" theo tiêu chí bình phương sai số nhỏ nhất (OLS)

# Ví dụ 1.1: mtcars - wt (trọng lượng xe) và mpg (mức tiêu thụ nhiên liệu)
# Trọng lượng xe có mối quan hệ như thế nào đến mức tiêu thụ nhiên liệu?

# Bước 1: Trực quan mối quan hệ bằng Scatter plot
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "darkblue") +
  labs(title = "Scatter plot giữa mpg và weight",
       x = "Weight", y = "mpg") +
  theme_minimal()
# --> Nhận xét: xu hướng giảm, quan hệ có vẻ tuyến tính (âm)

# Thêm đường hồi quy vào scatter plot
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "darkblue") +
  labs(title = "Scatter plot giữa mpg và weight",
       x = "Weight", y = "mpg") +
  geom_smooth(method = "lm", se = F) +
  theme_minimal()

# Bước 2: Ước lượng mô hình
model <- lm(mpg ~ wt, data = mtcars)
summary(model)
# Coefficients:
#   (Intercept) 37.2851 ; wt -5.3445
# Multiple R-squared: 0.7528 ; p-value: 1.294e-10

# Bước 3: Diễn giải kết quả
# 1. Hệ số hồi quy
#    - Intercept (beta0 = 37.29): khi wt = 0 (giả định toán học), mpg ~ 37.29
#      (không nhất thiết có ý nghĩa thực tế, nhưng cần cho mô hình)
#    - Slope (beta1 = -5.34): khi wt tăng 1 đơn vị (1000 lbs), mpg giảm trung bình 5.34
#    - Phương trình: mpg = 37.29 - 5.34*wt
#
# 2. Kiểm định ý nghĩa hệ số (t-value, Pr(>|t|))
#    H0: beta1 = 0  vs  H1: beta1 != 0
#    p-value của wt rất nhỏ (<0.001) --> trọng lượng xe ảnh hưởng đáng kể đến mpg
#
# 3. Hệ số xác định R^2
#    Multiple R-squared = 0.7528 --> ~75.3% biến thiên của mpg được giải thích bởi wt
#
# 4. Diễn giải bằng ngôn ngữ tự nhiên:
#    Trọng lượng xe có ảnh hưởng âm và có ý nghĩa thống kê đến mức tiêu thụ nhiên liệu.
#    Khi trọng lượng tăng 1000 lbs, mpg trung bình giảm khoảng 5.34 (p<0.001).
#    Mô hình giải thích được khoảng 75% biến thiên của mpg.

# Bước 4: Kiểm tra residuals (optional, sẽ học sâu hơn ở học phần khác)

# Bước 5: Dự đoán giá trị theo phương trình hồi quy
# Xe nặng 3 tấn thì mpg trung bình dự đoán là bao nhiêu?
new_data <- data.frame(wt = 3)
predict(model, newdata = new_data)

# Dự đoán cho nhiều giá trị cùng lúc
wt_seq <- data.frame(
  wt = seq(min(mtcars$wt), max(mtcars$wt), length.out = 10)
)
wt_seq$pred_mpg <- predict(model, newdata = wt_seq)
wt_seq


# ─────────────────────────────────────────────
# 2. Trích xuất giá trị của object model
# ─────────────────────────────────────────────

model <- lm(mpg ~ wt, data = mtcars)
# object model là một list, mỗi thành phần gắn với 1 khái niệm thống kê:
# coef -> phương trình hồi quy ; fitted -> dự đoán trung bình
# residuals -> sai lệch cá thể ; r.squared -> mức độ giải thích của mô hình

## 2.1. Hệ số hồi quy (coefficients)
coef(model)
model$coefficients
# --> beta0_hat (Intercept) và beta1_hat (wt): mpg_hat = beta0_hat + beta1_hat*wt

## 2.2. Giá trị dự đoán (fitted values)
fitted(model)
model$fitted.values
# --> yi_hat: mpg dự đoán cho từng xe

## 2.3. Residuals (phần dư)
resid(model)
model$residuals
# ei = yi - yi_hat
sum(resid(model))   # theo lý thuyết, tổng phần dư = 0

## 2.4. Bảng hệ số chi tiết (t-value, p-value)
summary(model)$coefficients
# Gồm: Estimate, Std. Error, t value, Pr(>|t|)

## 2.5. Hệ số xác định R^2
summary(model)$r.squared


# ─────────────────────────────────────────────
# BÀI TẬP
# ─────────────────────────────────────────────

#========================================#
#Exercise 1.1: Phân tích dữ liệu 401K
# Yêu cầu cài đặt gói wooldridge để có bộ dữ liệu k401k (prate, mrate)
# install.packages("wooldridge")
library(wooldridge)
data("k401k")

# 1. Tỷ lệ tham gia (prate) TB và tỷ lệ đóng góp đối ứng (mrate) TB
k401k %>%
  summarise(mean_prate = mean(prate), mean_mrate = mean(mrate))
# --> mean_prate = 87.36 (%) ; mean_mrate = 0.732

# 2. Ước lượng phương trình hồi quy đơn: prate ~ mrate
model_401k <- lm(prate ~ mrate, data = k401k)
summary(model_401k)
# Coefficients: (Intercept) = 83.08 (p<2e-16) ; mrate = 5.86 (t=11.12, p<2e-16)
# Residual SE = 16.09 ; R^2 = 0.0747
n_401k <- nrow(k401k)
r2_401k <- summary(model_401k)$r.squared
cat("n =", n_401k, " R^2 =", r2_401k, "\n")
# --> n = 1534 ; R^2 = 0.0747

# 3. Ý nghĩa hệ số chặn và hệ số của mrate
# Intercept = 83.08: khi mrate = 0, tỷ lệ tham gia trung bình dự đoán là 83.08%
# Hệ số mrate = 5.86: khi mrate tăng thêm 1 đơn vị, prate trung bình tăng thêm ~5.86 điểm %

# 4. Dự báo prate khi mrate = 3.5
predict(model_401k, newdata = data.frame(mrate = 3.5))
# --> Kết quả: 103.59 (%). Đây rõ ràng KHÔNG phải dự báo hợp lý, vì prate là tỷ lệ %
#     nên không thể vượt quá 100%. Lý do: mrate=3.5 nằm ngoài phạm vi mrate quan sát
#     được trong mẫu (đa số công ty có mrate nhỏ hơn nhiều), nên đây là ngoại suy
#     (extrapolation) - mô hình tuyến tính ước lượng trên vùng dữ liệu quan sát
#     không còn đáng tin cậy khi áp dụng ra ngoài vùng đó.

# 5. % biến thiên của prate được giải thích bởi mrate
r2_401k
# --> R^2 = 0.0747, nghĩa là mrate chỉ giải thích được khoảng 7.5% sự biến thiên
#     của prate. Đây là mức khá thấp, cho thấy còn rất nhiều yếu tố khác (quy mô
#     công ty, ngành nghề, chính sách nhân sự,...) ảnh hưởng đến tỷ lệ tham gia
#     mà mô hình đơn biến này chưa nắm bắt được.

#========================================#
#Exercise 1.2: Lương CEO
data("ceosal2")

# 1. Mức lương TB và thâm niên TB
ceosal2 %>%
  summarise(mean_salary = mean(salary), mean_ceoten = mean(ceoten))
# --> mean_salary = 865.86 (nghìn USD) ; mean_ceoten = 7.95 (năm)

# 2. Số CEO có ceoten = 0 và thâm niên dài nhất
sum(ceosal2$ceoten == 0)
max(ceosal2$ceoten)
# --> Có 5 CEO đang ở năm đầu tiên (ceoten=0) ; thâm niên lâu nhất là 37 năm

# 3. Mô hình: log(salary) = beta0 + beta1*ceoten + epsilon
model_ceo <- lm(log(salary) ~ ceoten, data = ceosal2)
summary(model_ceo)
# Coefficients: Intercept = 6.5055 (p<2e-16) ; ceoten = 0.009724 (t=1.528, p=0.128)
# R^2 = 0.01316
coef(model_ceo)["ceoten"] * 100
# --> % tăng thêm xấp xỉ khi ceoten tăng 1 năm: ~0.97%. Tuy nhiên p-value = 0.128 > 0.05,
#     nghĩa là hệ số này KHÔNG có ý nghĩa thống kê ở mức 5%: chưa đủ bằng chứng cho thấy
#     thâm niên làm CEO thực sự ảnh hưởng đến mức lương trong mẫu này (R^2 cũng rất thấp,
#     chỉ 1.3%).

# 4. Trực quan mối quan hệ log(salary) và ceoten + đường hồi quy
ggplot(ceosal2, aes(x = ceoten, y = log(salary))) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Quan hệ giữa log(salary) và ceoten",
       x = "Số năm làm CEO (ceoten)", y = "log(salary)") +
  theme_minimal()

#========================================#
#Exercise 1.3: Dữ liệu penguins
# install.packages("palmerpenguins")
library(palmerpenguins)
data("penguins")
penguins <- penguins %>% tidyr::drop_na(bill_length_mm, bill_depth_mm, species)

# 1. Nội dung các biến:
# bill_length_mm: chiều dài mỏ (mm)
# bill_depth_mm: độ sâu/dày mỏ (mm)
# species: loài chim cánh cụt (Adelie, Chinstrap, Gentoo)

# 2. Hồi quy bill_length_mm ~ bill_depth_mm
model_pen <- lm(bill_length_mm ~ bill_depth_mm, data = penguins)
summary(model_pen)
# Coefficients: Intercept = 55.07 ; bill_depth_mm = -0.65 (t=-4.459, p=1.12e-05)
# R^2 = 0.0553

# (a) Mối quan hệ: xem dấu và độ lớn của hệ số slope
coef(model_pen)
# --> Hệ số bill_depth_mm = -0.65, mang dấu ÂM và có ý nghĩa thống kê (p=1.12e-05).

# (b) Khi bill_depth_mm tăng 1 đơn vị, bill_length_mm tăng/giảm bao nhiêu?
# --> Khi gộp cả 3 loài, bill_depth_mm tăng 1mm thì bill_length_mm giảm trung bình 0.65mm.
#     Điều này NGƯỢC với trực giác sinh học (mỏ dài hơn thường cũng sâu/dày hơn).
#     Nguyên nhân nhiều khả năng là Simpson's Paradox: xu hướng chung khi gộp 3 loài
#     bị chi phối bởi việc các loài có kích thước mỏ khác nhau một cách hệ thống
#     (ví dụ Gentoo có mỏ dài nhưng lại dẹt/mỏng hơn Adelie và Chinstrap), nên
#     xu hướng gộp này không phản ánh đúng quan hệ sinh học thật bên trong từng loài.
#     Phần (c) và câu 3-4 bên dưới sẽ kiểm chứng lại điều này.

# (c) Trực quan cùng đường hồi quy
ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Hồi quy bill_length_mm ~ bill_depth_mm (gộp loài)",
       x = "Bill depth (mm)", y = "Bill length (mm)") +
  theme_minimal()

# 3. Trực quan chia màu theo species
ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm, color = species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Hồi quy bill_length_mm ~ bill_depth_mm theo từng loài",
       x = "Bill depth (mm)", y = "Bill length (mm)", color = "Species") +
  theme_minimal()
# --> Hợp lý hơn: khi tách theo màu loài, đường hồi quy trong từng loài dốc lên (dương),
#     ngược hẳn với đường hồi quy dốc xuống khi gộp chung ở câu 2. Số liệu cụ thể được
#     kiểm chứng lại ở câu 4 bên dưới.

# 4. Phương trình hồi quy riêng cho từng loài
penguins %>%
  group_by(species) %>%
  summarise(
    model_list = list(lm(bill_length_mm ~ bill_depth_mm, data = across(everything())))
  ) -> temp  # cách này phức tạp, dùng cách rõ ràng hơn bên dưới

for (sp in unique(penguins$species)) {
  cat("=== Loài:", sp, "===\n")
  df_sp <- penguins %>% filter(species == sp)
  model_sp <- lm(bill_length_mm ~ bill_depth_mm, data = df_sp)
  print(coef(model_sp))
  cat("R^2 =", summary(model_sp)$r.squared, "\n\n")
}
# --> Kết quả thực tế:
#     Adelie:    slope = +0.86  (R^2 = 0.153)
#     Gentoo:    slope = +2.02  (R^2 = 0.414)
#     Chinstrap: slope = +1.92  (R^2 = 0.427)
#     Cả 3 loài đều có hệ số DƯƠNG, đúng với trực giác sinh học, trong khi hồi quy
#     gộp chung ở câu 2 lại cho hệ số ÂM (-0.65). Đây chính là ví dụ kinh điển của
#     Simpson's Paradox: xu hướng khi gộp dữ liệu trái ngược hoàn toàn với xu hướng
#     bên trong từng nhóm con.

#========================================#
#Exercise 2.1: Phân tích dữ liệu 401K (trích xuất từ object model)

model_401k <- lm(prate ~ mrate, data = k401k)

# 1a. Hệ số chặn và hệ số mrate
coef(model_401k)["(Intercept)"]  # 83.08
coef(model_401k)["mrate"]        # 5.86

# 1b. p-value của hệ số mrate
summary(model_401k)$coefficients["mrate", "Pr(>|t|)"]
# --> 1.098e-27, cực kỳ nhỏ, hệ số mrate có ý nghĩa thống kê rất mạnh.

# 1c. R^2
summary(model_401k)$r.squared  # 0.0747

# 1d. Kích thước mẫu n
nobs(model_401k)  # 1534

# 1e. Giá trị dự đoán cho từng quan sát
prate_hat <- fitted(model_401k)
head(prate_hat)
# --> 6 giá trị đầu: 84.31, 91.40, 88.41, 85.54, 86.18, 93.74

# 1f. Residuals
residual_401k <- resid(model_401k)
head(residual_401k)
# --> 6 giá trị đầu: -58.21, 8.60, 9.19, 14.46, -3.68, 6.26

# 1g. Kiểm tra prate = prate_hat + residual
all.equal(k401k$prate, prate_hat + residual_401k)
# --> Kết quả trả về "names for current but not for target" chứ không phải TRUE.
#     Về mặt số học hai vế bằng nhau y hệt (đây là tính chất bắt buộc của OLS: tổng
#     giá trị dự đoán + phần dư luôn bằng giá trị quan sát), chỉ khác nhau ở attribute
#     "names" - vì fitted()/resid() giữ lại tên dòng (row names) từ k401k, còn
#     k401k$prate thì không có tên. Muốn all.equal() trả về TRUE thật sự, cần bỏ names:
all.equal(k401k$prate, as.numeric(prate_hat + residual_401k))
# --> TRUE

# 2. Hồi quy đơn cho từng biến numeric (trừ prate)
numeric_vars <- k401k %>%
  select(where(is.numeric)) %>%
  select(-prate) %>%
  names()

results_401k <- data.frame()
for (v in numeric_vars) {
  formula_v <- as.formula(paste("prate ~", v))
  model_v <- lm(formula_v, data = k401k)
  coef_v <- coef(model_v)[2]
  pval_v <- summary(model_v)$coefficients[2, "Pr(>|t|)"]
  r2_v <- summary(model_v)$r.squared
  results_401k <- rbind(results_401k,
                        data.frame(variable = v, slope = coef_v,
                                   p_value = pval_v, R2 = r2_v))
}
results_401k <- results_401k %>% arrange(desc(R2))
results_401k
# --> Kết quả thực tế (sắp theo R2 giảm dần):
#     mrate (R2=0.0747), ltotemp (R2=0.0499), age (R2=0.0269), sole (R2=0.0251),
#     totelg (R2=0.0058), totemp (R2=0.0047), totpart (R2 ~ 0.0000174)

# (c) Diễn giải:
# - mrate có R^2 lớn nhất (0.0747), giải thích prate tốt nhất trong số các biến numeric,
#   dù vẫn ở mức khá thấp.
# - Tất cả các biến đều có p-value < 0.05 (có ý nghĩa thống kê), NGOẠI TRỪ totpart
#   (p = 0.870). Đáng chú ý là totpart không rơi vào kiểu "hệ số lớn nhưng không ý nghĩa"
#   mà thực chất hệ số của nó cũng gần như bằng 0 (1.5e-05) và R^2 gần như bằng 0 luôn
#   --> đây là biến gần như KHÔNG liên quan gì đến prate, chứ không phải trường hợp
#   "coi chừng hệ số to mà bị nhiễu" như mình tưởng ban đầu.

#========================================#
#Exercise 2.2: Lương CEO - lặp lại yêu cầu 2 cho log(salary)

ceosal2_log <- ceosal2 %>% mutate(log_salary = log(salary))

numeric_vars_ceo <- ceosal2_log %>%
  select(where(is.numeric)) %>%
  select(-salary, -log_salary) %>%
  names()

results_ceo <- data.frame()
for (v in numeric_vars_ceo) {
  formula_v <- as.formula(paste("log_salary ~", v))
  model_v <- lm(formula_v, data = ceosal2_log)
  coef_v <- coef(model_v)[2]
  pval_v <- summary(model_v)$coefficients[2, "Pr(>|t|)"]
  r2_v <- summary(model_v)$r.squared
  results_ceo <- rbind(results_ceo,
                       data.frame(variable = v, slope = coef_v,
                                  p_value = pval_v, R2 = r2_v))
}
results_ceo <- results_ceo %>% arrange(desc(R2))
results_ceo
# --> Chạy thử thì thấy dòng đầu tiên là "lsalary" với slope=1, p=0, R^2=1 - lúc đầu
#     tưởng model chạy sai, nhưng thực ra là do dữ liệu ceosal2 có sẵn cột lsalary
#     (= log(salary) tính sẵn từ trước), và vì mình chỉ loại "salary" với "log_salary"
#     (cột tự tạo ở trên) chứ không loại "lsalary" (tên khác 1 chữ!), nên nó vẫn lọt vào
#     numeric_vars_ceo. Hồi quy log_salary ~ lsalary thực chất là hồi quy một biến lên
#     chính bản sao của nó nên R^2=1 là hiển nhiên, không có ý nghĩa gì và nên bỏ qua.
#
# - Bỏ qua lsalary, biến giải thích log(salary) tốt nhất thực sự là lsales (R^2=0.281),
#   theo sau là lmktval (R^2=0.232), cả hai đều có p-value rất nhỏ.
# - college là ví dụ rõ cho "hệ số tương đối lớn nhưng KHÔNG có ý nghĩa thống kê":
#   slope = -0.237 (có bằng đại học ứng với log(salary) thấp hơn ~23.7%), nhưng
#   p-value = 0.390 > 0.05, nên không thể kết luận có tác động thực sự trong mẫu này.
# - ceoten cũng không có ý nghĩa thống kê ở đây (p=0.128), khớp với kết quả đã thấy
#   ở Exercise 1.2.