# 21/5 - 24KDL
# Chương 3 - Buổi 5: ĐỊNH LÝ GIỚI HẠN TRUNG TÂM (Central Limit Theorem)

library(tidyr)
library(dplyr)
library(ggplot2)

# ─────────────────────────────────────────────
# 1. Mô phỏng Định lý Giới hạn Trung tâm trong R
# ─────────────────────────────────────────────

# Định lý: nếu X1,...,Xn ~ i.i.d với mean mu, sd sigma, khi n lớn:
# X_bar ~ N(mu, (sigma/sqrt(n))^2)

# Ví dụ: X ~ Exp(lambda = 1), muX = 1, sigmaX = 1
x <- rexp(10000, 1)

ggplot(tibble(x), aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill = "lightblue", col = "white", bins = 30) +
  geom_density(col = "blue", linewidth = 1) +
  labs(title = "Phân phối của X") +
  theme_bw()

# Mô phỏng trung bình mẫu với n = 2
R <- 10000   # số lần lặp
xbar_2 <- c()
for (i in 1:R) {
  sample_i <- sample(x, 2)
  xbar_2[i] <- mean(sample_i)
}

ggplot(tibble(xbar_2), aes(xbar_2)) +
  geom_density(col = "blue", linewidth = 1) +
  labs(title = "Trung bình mẫu", subtitle = "n = 2",
       x = "Giá trị x_bar", y = "Sampling Density") +
  theme_bw()

# Thực hiện tương tự với n = 5, 30, 100
xbar_5 <- c()
for (i in 1:R) {
  sample_i <- sample(x, 5)
  xbar_5[i] <- mean(sample_i)
}

xbar_30 <- c()
for (i in 1:R) {
  sample_i <- sample(x, 30)
  xbar_30[i] <- mean(sample_i)
}

xbar_100 <- c()
for (i in 1:R) {
  sample_i <- sample(x, 100)
  xbar_100[i] <- mean(sample_i)
}

# Trực quan tất cả các xbar trong cùng một plot
CLT_df <- tibble(xbar_2, xbar_5, xbar_30, xbar_100) %>%
  pivot_longer(cols = everything(),
               names_to = "size", names_prefix = "xbar_",
               values_to = "xbar") %>%
  mutate(size = factor(as.integer(size)))

ggplot(CLT_df, aes(x = xbar, fill = size, color = size)) +
  geom_density(position = "identity", alpha = 0.3) +
  labs(title = "So sánh phân phối trung bình mẫu với các cỡ mẫu khác nhau",
       x = "Giá trị trung bình mẫu", y = "Sampling Density",
       fill = "Cỡ mẫu", color = "Cỡ mẫu") +
  theme_bw()

# --> Nhận xét: khi n nhỏ (n=2), phân phối trung bình mẫu vẫn lệch giống tổng thể gốc.
#     Khi n tăng (5 -> 30), phân phối trở nên đối xứng hơn, tập trung quanh mean lý thuyết.
#     Với n=100, phân phối gần như phân phối chuẩn.

# Bảng so sánh giá trị thực nghiệm và lý thuyết
CLT_df %>%
  group_by(size) %>%
  summarise(
    Mean_emperical = mean(xbar),
    SD_empirical   = sd(xbar)
  ) %>%
  ungroup() %>%
  mutate(
    Mean_theoretical = mean(x),
    SD_theoretical   = sd(x) / sqrt(as.numeric(as.character(size)))
  )
# --> Đúng với CLT: X_bar ~ N(muX, (sigmaX/sqrt(n))^2), SD giảm dần khi n tăng.


# ─────────────────────────────────────────────
# 2. Function trong R
# ─────────────────────────────────────────────

# function(): đóng gói đoạn code để tái sử dụng nhiều lần
# tên_hàm <- function(tham_so_1, tham_so_2, ...) { ... ; return(...) }

binh_phuong <- function(x) {
  return(x^2)
}
binh_phuong(5)

# Hàm tính trung bình mẫu (sample means)
sample_means <- function(data, n, r = 1000) {
  xbar <- c()
  for (i in 1:r) {
    sample_data <- sample(data, n, replace = TRUE)
    xbar[i] <- mean(sample_data)
  }
  return(xbar)
}

set.seed(123)
data <- rexp(10000, rate = 1)
xbar_30c <- sample_means(data, n = 30)

ggplot(tibble(xbar_30c), aes(xbar_30c)) +
  geom_density(col = "blue", linewidth = 1) +
  labs(title = "Trung bình mẫu", subtitle = "n = 30",
       x = "Giá trị x_bar", y = "Sampling Density") +
  theme_bw()


# ─────────────────────────────────────────────
# 3. Một số phân phối mẫu (sampling distribution) theo CLT
# ─────────────────────────────────────────────

# Giả sử X1,...,Xn i.i.d, kỳ vọng mu, phương sai sigma^2

## 3.1. Phân phối mẫu của trung bình mẫu
# X_bar ~ N(muX, sigmaX^2 / n)

## 3.2. Phân phối mẫu của tổng mẫu
# Sn = sum(Xi) ~ N(n*muX, n*sigmaX^2)

## 3.3. Trung bình mẫu chuẩn hóa
# Z = (X_bar - muX) / (sigmaX/sqrt(n)) ~ N(0,1)

## 3.4. Hiệu hai trung bình mẫu
# X_bar1 - X_bar2 ~ N(mu1-mu2, sigma1^2/n1 + sigma2^2/n2)

## 3.5. Tổ hợp tuyến tính của các trung bình mẫu
# Y = a1*X_bar1 + ... + ak*X_bark ~ N(sum(ai*mui), sum(ai^2*sigmai^2/ni))

## 3.6. Phân phối mẫu của tỷ lệ mẫu
# X Bernoulli(p), p_hat = X_bar ~ N(p, p(1-p)/n)

## 3.7. Hiệu hai tỷ lệ mẫu
# p_hat1 - p_hat2 ~ N(p1-p2, p1(1-p1)/n1 + p2(1-p2)/n2)


# ─────────────────────────────────────────────
# BÀI TẬP
# ─────────────────────────────────────────────

#========================================#
#Bài tập 1.1: Dữ liệu diamonds$price
data("diamonds", package = "ggplot2")
price <- diamonds$price

# Histogram và density của price
ggplot(tibble(price), aes(price)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill = "lightblue", col = "white", bins = 30) +
  geom_density(col = "blue", linewidth = 1) +
  labs(title = "Phân phối của price") +
  theme_bw()
# --> Biến gốc lệch phải (right-skewed), không đối xứng.

# Mô phỏng CLT với n = 10, 30, 50, 100
R <- 10000
mo_phong_xbar <- function(data, n, r = R) {
  xbar <- c()
  for (i in 1:r) {
    sample_i <- sample(data, n, replace = TRUE)
    xbar[i] <- mean(sample_i)
  }
  return(xbar)
}

xbar_10  <- mo_phong_xbar(price, 10)
xbar_30b <- mo_phong_xbar(price, 30)
xbar_50  <- mo_phong_xbar(price, 50)
xbar_100b <- mo_phong_xbar(price, 100)

CLT_price <- tibble(xbar_10, xbar_30b, xbar_50, xbar_100b) %>%
  pivot_longer(cols = everything(),
               names_to = "size", names_prefix = "xbar_",
               values_to = "xbar") %>%
  mutate(size = factor(size, levels = c("10", "30b", "50", "100b"),
                       labels = c("10", "30", "50", "100")))

ggplot(CLT_price, aes(x = xbar, fill = size, color = size)) +
  geom_density(position = "identity", alpha = 0.3) +
  labs(title = "So sánh phân phối trung bình mẫu của price",
       x = "Giá trị trung bình mẫu", y = "Sampling Density",
       fill = "Cỡ mẫu", color = "Cỡ mẫu") +
  theme_bw()

# Bảng so sánh mean và sd
CLT_price %>%
  group_by(size) %>%
  summarise(
    Mean_emperical = mean(xbar),
    SD_empirical   = sd(xbar)
  ) %>%
  ungroup() %>%
  mutate(
    Mean_theoretical = mean(price),
    SD_theoretical   = sd(price) / sqrt(as.numeric(as.character(size)))
  )

# Trả lời câu hỏi:
# - Biến gốc (price) lệch phải, không đối xứng.
# - Khi n tăng, phân phối trung bình mẫu càng đối xứng và hẹp lại quanh mean lý thuyết.
# - Kết quả thực nghiệm gần với lý thuyết CLT (mean/sd thực nghiệm ~ mean/sd lý thuyết).

#========================================#
#Bài tập 1.2: Dùng sum() thay vì mean()
mo_phong_sum <- function(data, n, r = R) {
  s <- c()
  for (i in 1:r) {
    sample_i <- sample(data, n, replace = TRUE)
    s[i] <- sum(sample_i)
  }
  return(s)
}

sum_10  <- mo_phong_sum(price, 10)
sum_30  <- mo_phong_sum(price, 30)
sum_100 <- mo_phong_sum(price, 100)

CLT_sum <- tibble(sum_10, sum_30, sum_100) %>%
  pivot_longer(cols = everything(),
               names_to = "size", names_prefix = "sum_",
               values_to = "s") %>%
  mutate(size = factor(size, levels = c("10", "30", "100")))

ggplot(CLT_sum, aes(x = s, fill = size, color = size)) +
  geom_density(position = "identity", alpha = 0.3) +
  labs(title = "Phân phối tổng mẫu (sum) của price",
       x = "Giá trị tổng mẫu", y = "Sampling Density",
       fill = "Cỡ mẫu", color = "Cỡ mẫu") +
  theme_bw()
# --> Khi dùng sum() thay vì mean(), giá trị trung tâm và độ trải rộng đều tăng
#     theo n: mean(Sn) = n*mu, sd(Sn) = sqrt(n)*sigma (thay vì sigma/sqrt(n)).

#========================================#
#Bài tập 2.1: Tạo hàm với input là data, n, r=1000; áp dụng cho depth và carat

# Hàm sample_means() đã có ở phần 2, áp dụng cho depth và carat
depth_data <- diamonds$depth
carat_data <- diamonds$carat

ns <- c(2, 10, 15, 20, 100)

CLT_depth <- tibble(n = ns) %>%
  rowwise() %>%
  mutate(xbar = list(sample_means(depth_data, n))) %>%
  unnest(xbar) %>%
  mutate(n = factor(n, levels = ns))

ggplot(CLT_depth, aes(x = xbar, fill = n, color = n)) +
  geom_density(position = "identity", alpha = 0.3) +
  labs(title = "Trung bình mẫu của depth với các cỡ mẫu khác nhau",
       x = "Giá trị trung bình mẫu", y = "Sampling Density",
       fill = "Cỡ mẫu", color = "Cỡ mẫu") +
  theme_bw()

CLT_carat <- tibble(n = ns) %>%
  rowwise() %>%
  mutate(xbar = list(sample_means(carat_data, n))) %>%
  unnest(xbar) %>%
  mutate(n = factor(n, levels = ns))

ggplot(CLT_carat, aes(x = xbar, fill = n, color = n)) +
  geom_density(position = "identity", alpha = 0.3) +
  labs(title = "Trung bình mẫu của carat với các cỡ mẫu khác nhau",
       x = "Giá trị trung bình mẫu", y = "Sampling Density",
       fill = "Cỡ mẫu", color = "Cỡ mẫu") +
  theme_bw()

#========================================#
#Bài tập 2.2: Thực hiện hàm với dữ liệu tự tạo từ Exp, Poisson, Geom

set.seed(1)
data_exp  <- rexp(10000, rate = 1)
data_pois <- rpois(10000, lambda = 3)
data_geom <- rgeom(10000, prob = 0.3)

ns2 <- c(2, 10, 30, 100)

CLT_exp <- tibble(n = ns2) %>%
  rowwise() %>%
  mutate(xbar = list(sample_means(data_exp, n))) %>%
  unnest(xbar) %>%
  mutate(n = factor(n, levels = ns2))

ggplot(CLT_exp, aes(x = xbar, fill = n, color = n)) +
  geom_density(position = "identity", alpha = 0.3) +
  labs(title = "CLT - Phân phối Exponential", x = "x_bar", fill = "Cỡ mẫu", color = "Cỡ mẫu") +
  theme_bw()

CLT_pois <- tibble(n = ns2) %>%
  rowwise() %>%
  mutate(xbar = list(sample_means(data_pois, n))) %>%
  unnest(xbar) %>%
  mutate(n = factor(n, levels = ns2))

ggplot(CLT_pois, aes(x = xbar, fill = n, color = n)) +
  geom_density(position = "identity", alpha = 0.3) +
  labs(title = "CLT - Phân phối Poisson", x = "x_bar", fill = "Cỡ mẫu", color = "Cỡ mẫu") +
  theme_bw()

CLT_geom <- tibble(n = ns2) %>%
  rowwise() %>%
  mutate(xbar = list(sample_means(data_geom, n))) %>%
  unnest(xbar) %>%
  mutate(n = factor(n, levels = ns2))

ggplot(CLT_geom, aes(x = xbar, fill = n, color = n)) +
  geom_density(position = "identity", alpha = 0.3) +
  labs(title = "CLT - Phân phối Geometric", x = "x_bar", fill = "Cỡ mẫu", color = "Cỡ mẫu") +
  theme_bw()

#========================================#
#Bài tập 3.1: Hàm tính các phân phối mẫu, thử với Bernoulli, Poisson, Exponential

# (1) Tổng mẫu
sample_sums <- function(data, n, r = 1000) {
  s <- c()
  for (i in 1:r) {
    s[i] <- sum(sample(data, n, replace = TRUE))
  }
  return(s)
}

# (2) Trung bình mẫu chuẩn hóa
sample_means_standardized <- function(data, n, mu, sigma, r = 1000) {
  xbar <- sample_means(data, n, r)
  z <- (xbar - mu) / (sigma / sqrt(n))
  return(z)
}

# (3) Tỷ lệ mẫu (data Bernoulli 0/1)
sample_proportions <- function(data, n, r = 1000) {
  return(sample_means(data, n, r))
}

# (4) Tổ hợp tuyến tính các trung bình mẫu
linear_combo_means <- function(list_data, list_n, coefs, r = 1000) {
  k <- length(list_data)
  xbars <- matrix(NA, nrow = r, ncol = k)
  for (j in 1:k) {
    xbars[, j] <- sample_means(list_data[[j]], list_n[j], r)
  }
  Y <- xbars %*% coefs
  return(as.vector(Y))
}

# (5) Hiệu hai trung bình mẫu
diff_two_means <- function(data1, n1, data2, n2, r = 1000) {
  xbar1 <- sample_means(data1, n1, r)
  xbar2 <- sample_means(data2, n2, r)
  return(xbar1 - xbar2)
}

# (6) Hiệu hai tỷ lệ mẫu
diff_two_proportions <- function(data1, n1, data2, n2, r = 1000) {
  p1 <- sample_means(data1, n1, r)
  p2 <- sample_means(data2, n2, r)
  return(p1 - p2)
}

## Thử nghiệm với Bernoulli, Poisson, Exponential
set.seed(42)
bern_data <- rbinom(10000, size = 1, prob = 0.3)   # Bernoulli(p=0.3)
pois_data <- rpois(10000, lambda = 4)
exp_data  <- rexp(10000, rate = 0.5)

# --- Tổng mẫu (Exponential) ---
Sn_exp <- sample_sums(exp_data, n = 30)
tibble(Sn = Sn_exp) %>%
  ggplot(aes(Sn)) +
  geom_histogram(aes(y = after_stat(density)), fill = "lightblue", color = "white", bins = 30) +
  geom_function(fun = dnorm,
                args = list(mean = 30 * mean(exp_data),
                            sd = sqrt(30) * sd(exp_data)),
                color = "red", linewidth = 1) +
  labs(title = "Tổng mẫu (Sn) - Exponential, n=30") +
  theme_bw()

tibble(
  Mean_emp = mean(Sn_exp), SD_emp = sd(Sn_exp),
  Mean_theo = 30 * mean(exp_data), SD_theo = sqrt(30) * sd(exp_data)
)

# --- Trung bình mẫu chuẩn hóa (Poisson) ---
z_pois <- sample_means_standardized(pois_data, n = 30,
                                    mu = mean(pois_data), sigma = sd(pois_data))
tibble(z = z_pois) %>%
  ggplot(aes(z)) +
  geom_histogram(aes(y = after_stat(density)), fill = "lightgreen", color = "white", bins = 30) +
  geom_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red", linewidth = 1) +
  labs(title = "Trung bình mẫu chuẩn hóa (Z) - Poisson, n=30") +
  theme_bw()

tibble(Mean_emp = mean(z_pois), SD_emp = sd(z_pois),
       Mean_theo = 0, SD_theo = 1)

# --- Tỷ lệ mẫu (Bernoulli) ---
phat_bern <- sample_proportions(bern_data, n = 50)
tibble(phat = phat_bern) %>%
  ggplot(aes(phat)) +
  geom_histogram(aes(y = after_stat(density)), fill = "orange", color = "white", bins = 30) +
  geom_function(fun = dnorm,
                args = list(mean = 0.3, sd = sqrt(0.3 * 0.7 / 50)),
                color = "red", linewidth = 1) +
  labs(title = "Tỷ lệ mẫu (p_hat) - Bernoulli(p=0.3), n=50") +
  theme_bw()

tibble(Mean_emp = mean(phat_bern), SD_emp = sd(phat_bern),
       Mean_theo = 0.3, SD_theo = sqrt(0.3 * 0.7 / 50))

# --- Tổ hợp tuyến tính (Poisson + Exponential) ---
Y_combo <- linear_combo_means(
  list_data = list(pois_data, exp_data),
  list_n = c(20, 20),
  coefs = c(0.5, 0.5)
)
tibble(Y = Y_combo) %>%
  ggplot(aes(Y)) +
  geom_histogram(aes(y = after_stat(density)), fill = "purple", color = "white", alpha = 0.5, bins = 30) +
  labs(title = "Tổ hợp tuyến tính Y = 0.5*Xbar1 + 0.5*Xbar2") +
  theme_bw()

mu_theo <- 0.5 * mean(pois_data) + 0.5 * mean(exp_data)
sd_theo <- sqrt(0.5^2 * var(pois_data)/20 + 0.5^2 * var(exp_data)/20)
tibble(Mean_emp = mean(Y_combo), SD_emp = sd(Y_combo),
       Mean_theo = mu_theo, SD_theo = sd_theo)

# --- Hiệu hai trung bình mẫu (Poisson vs Exponential) ---
diff_means <- diff_two_means(pois_data, 30, exp_data, 30)
tibble(diff = diff_means) %>%
  ggplot(aes(diff)) +
  geom_histogram(aes(y = after_stat(density)), fill = "brown", color = "white", bins = 30) +
  labs(title = "Hiệu hai trung bình mẫu (Poisson - Exponential)") +
  theme_bw()

mu_diff_theo <- mean(pois_data) - mean(exp_data)
sd_diff_theo <- sqrt(var(pois_data)/30 + var(exp_data)/30)
tibble(Mean_emp = mean(diff_means), SD_emp = sd(diff_means),
       Mean_theo = mu_diff_theo, SD_theo = sd_diff_theo)

# --- Hiệu hai tỷ lệ mẫu (2 mẫu Bernoulli khác p) ---
bern_data2 <- rbinom(10000, size = 1, prob = 0.5)
diff_props <- diff_two_proportions(bern_data, 50, bern_data2, 50)
tibble(diff = diff_props) %>%
  ggplot(aes(diff)) +
  geom_histogram(aes(y = after_stat(density)), fill = "darkcyan", color = "white", bins = 30) +
  labs(title = "Hiệu hai tỷ lệ mẫu (p1=0.3 vs p2=0.5)") +
  theme_bw()

p1 <- 0.3; p2 <- 0.5
sd_diff_p_theo <- sqrt(p1*(1-p1)/50 + p2*(1-p2)/50)
tibble(Mean_emp = mean(diff_props), SD_emp = sd(diff_props),
       Mean_theo = p1 - p2, SD_theo = sd_diff_p_theo)