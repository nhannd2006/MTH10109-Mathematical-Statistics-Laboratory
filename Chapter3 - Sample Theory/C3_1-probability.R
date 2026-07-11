# 14/5 - 24KDL
# Chương 3: TÍNH TOÁN VÀ TRỰC QUAN CÁC PHÂN PHỐI XÁC SUẤT TRONG R

# ─────────────────────────────────────────────
# I. Phân phối rời rạc (Discrete Distribution)
# ─────────────────────────────────────────────

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

### 1. Phân phối đều rời rạc ------------------------
# sample(x, size, replace = FALSE, prob = NULL)
# x: vector phần tử để chọn hoặc số nguyên dương
# size: số lượng mục để lựa chọn
# replace: có hoàn lại (TRUE) hay không (FALSE, mặc định)
# prob: vector trọng số xác suất

set.seed(2025)
mat_xuc_xac <- c(1, 2, 3, 4, 5, 6)
sample(mat_xuc_xac, 1)   # tung xúc xắc 1 lần

# Tung xúc xắc 100 lần và trực quan hóa
thi_nghiem <- sample(mat_xuc_xac, size = 100, replace = TRUE)

df_thi_nghiem <- tibble(mat_so = thi_nghiem)

thong_ke <- df_thi_nghiem %>%
  count(mat_so)
thong_ke

ggplot(df_thi_nghiem, aes(x = factor(mat_so))) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(
    title = "Phân phối kết quả tung xúc xắc 100 lần",
    x = "Mặt xúc xắc",
    y = "Tần suất xuất hiện"
  ) +
  theme_minimal()

### 2. Phân phối nhị thức ----------------------------
## 2.1. Giới thiệu *binom() trong R
# X ~ Binom(size = 10, prob = 0.3)
# dbinom(x, size, prob) - PMF: P(X = x)
# pbinom(q, size, prob) - CDF: P(X <= q)
# qbinom(p, size, prob) - Quantile: giá trị k nhỏ nhất sao cho P(X<=k) >= p
# rbinom(n, size, prob) - Sinh mẫu ngẫu nhiên

dbinom(5, size = 10, prob = 0.3)   # P(X = 5)
pbinom(5, size = 10, prob = 0.3)   # P(X <= 5)
rbinom(100, size = 10, prob = 0.3) # 100 lần thí nghiệm, mỗi lần tung 10 lần

## 2.2. Tính toán xác suất cho phân phối Nhị thức
# X ~ Binom(10, 0.5)
dbinom(3, size = 10, prob = 0.5)   # P(X = 3)
pbinom(3, size = 10, prob = 0.5)   # P(X <= 3)

# Bảng quy tắc khi F rời rạc:
# P(X = t)          -> dF(t,...)
# P(X <= t)         -> pF(t,...)
# P(X < t)          -> pF(t-1,...)
# P(X >= t)         -> 1 - pF(t-1,...)
# P(X > t)          -> 1 - pF(t,...)
# P(a <= X <= b)    -> pF(b,...) - pF(a-1,...)
# P(a < X <= b)     -> pF(b,...) - pF(a,...)
# P(a <= X < b)     -> pF(b-1,...) - pF(a-1,...)
# P(a < X < b)      -> pF(b-1,...) - pF(a,...)
# F^-1(c)           -> qF(c,...)

## 2.3. Trực quan hàm pmf của phân phối Nhị thức
# Dùng geom_segment() và geom_point()
df_binom <- tibble(
  x = 0:10,
  y = dbinom(0:10, size = 10, prob = 0.5)
)
ggplot(df_binom, aes(x = x, y = y)) +
  geom_segment(aes(xend = x, yend = 0), color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 3) +
  labs(title = "PMF của Binomial(10, 0.5)", x = "x", y = "P(X = x)") +
  scale_x_continuous(breaks = 0:10) +
  theme_minimal()

# Dùng geom_col()
ggplot(df_binom, aes(x = factor(x), y = y)) +
  geom_col(fill = "lightblue", color = "white") +
  labs(title = "PMF of Binomial(10, 0.5)", x = "x", y = "P(X = x)") +
  theme_minimal()

# So sánh nhiều pmf trong cùng một plot (long format)
df_compare <- tibble(
  x = rep(0:10, 2),
  prob = c(dbinom(0:10, 10, 0.3), dbinom(0:10, 10, 0.7)),
  p_val = rep(c("p = 0.3", "p = 0.7"), each = 11)
)
ggplot(df_compare, aes(x = x, y = prob, color = p_val)) +
  geom_segment(aes(xend = x, yend = 0), linewidth = 1.5, alpha = 0.4) +
  geom_point(size = 3, alpha = 0.6) +
  scale_x_continuous(breaks = 0:10) +
  labs(title = "So sánh hai phân phối Binomial (n=10)",
       x = "x", y = "P(X = x)", color = "Tham số") +
  theme_minimal() +
  theme(legend.position = "top")

ggplot(df_compare, aes(x = factor(x), y = prob, fill = p_val)) +
  geom_col(position = "identity", alpha = 0.4) +
  labs(title = "Hai phân phối Binomial", x = "x", y = "P(X = x)", fill = "Tham số") +
  theme_minimal()

### 3. Phân phối Bernoulli ---------------------------
# Không có hàm riêng, dùng phân phối nhị thức với size = 1
# dbinom(x, size = 1, prob); pbinom(); qbinom(); rbinom()


### 4. Phân phối Hình học (Geometric) ----------------
# P(X=k) = (1-p)^(k-1) * p, k = 1,2,3,...
# E(X) = 1/p ; Var(X) = (1-p)/p^2
# Trong R: X là số lần THẤT BẠI trước thành công đầu tiên (X = 0,1,2,...)
# dgeom(x, prob); pgeom(q, prob); qgeom(p, prob); rgeom(n, prob)

### 5. Phân phối Poisson -----------------------------
# P(X=k) = e^(-lambda) * lambda^k / k! , k=0,1,2,...
# E(X) = Var(X) = lambda
# dpois(x, lambda); ppois(q, lambda); qpois(p, lambda); rpois(n, lambda)

### 6. Trực quan hàm cdf và quantile -----------------
## 6.1. Trực quan hàm cdf
# CHÚ Ý: trực quan cdf bằng geom_segment/geom_point như pmf là SAI về mặt lý thuyết
# vì CDF phải là hàm bậc thang (step function), không phải các điểm rời rạc đơn lẻ.

# Cách đúng: X ~ Binom(10, 0.4)
tibble(
  x = 0:10,
  cdf = pbinom(0:10, size = 10, prob = 0.4)
) %>%
  ggplot() +
  geom_segment(aes(x = x, xend = x + 1, y = cdf, yend = cdf),
               color = "blue", linewidth = 1) +
  geom_point(aes(x = x + 1, y = cdf), shape = 21, fill = "white",
             color = "blue", size = 2.5) +
  geom_point(aes(x = x, y = cdf), color = "blue", size = 2.5) +
  scale_x_continuous(breaks = -1:11, limits = c(-0.5, 11.5)) +
  labs(title = "CDF Binomial(10, 0.4)", x = "Giá trị x", y = "F(x) = P(X <= x)") +
  theme_minimal()

## 6.2. Trực quan hàm quantile
# Q(p) là hàm ngược của CDF, liên tục trái (khác CDF liên tục phải)
gia_tri <- 0:10
p_points <- c(0, pbinom(gia_tri, size = 10, prob = 0.4))
p_trai <- p_points[1:length(p_points) - 1]
p_phai <- p_points[2:length(p_points)]

tibble(gia_tri, p_trai, p_phai) %>%
  ggplot() +
  geom_segment(aes(y = gia_tri, x = p_trai, yend = gia_tri, xend = p_phai),
               linewidth = 1, color = "darkgreen") +
  geom_point(aes(x = p_trai, y = gia_tri),
             shape = 21, fill = "white", color = "darkgreen", size = 2.5) +
  geom_point(aes(x = p_phai, y = gia_tri),
             color = "darkgreen", fill = "darkgreen", size = 2.5) +
  scale_y_continuous(breaks = 0:10) +
  labs(title = "Quantile Function - Binomial(10, 0.4)",
       x = "Xác suất tích lũy (p)", y = "Giá trị phân vị (x)") +
  theme_minimal()

### 7. Trực quan bốn hàm probs trong R ---------------
## 7.1. Thực tế hóa lý thuyết
# sample(): mô phỏng đơn lẻ (discrete uniform, chọn 1/nhiều phần tử từ tập hợp)
# rbinom(): mô phỏng một quá trình - "tung đồng xu size lần, đếm số ngửa"
sample(c("S", "N"), size = 10, replace = TRUE)   # kết quả từng lần tung
rbinom(n = 1, size = 10, prob = 0.5)             # tổng số lần ngửa trong 10 lần tung

## 7.2. Lưu ý tham số
# size: số phép thử Bernoulli trong MỘT thí nghiệm
# n: số lần LẶP LẠI cả thí nghiệm đó

# Minh họa Luật số lớn
samples <- rbinom(10000, size = 10, prob = 0.4)
mean(samples)   # hội tụ về E(X) = size*prob = 4

ggplot(tibble(samples = samples), aes(x = factor(samples))) +
  geom_bar(fill = "lightblue", color = "white") +
  labs(title = "Mô phỏng 10.000 thí nghiệm Binomial",
       x = "Số lần thành công (x)", y = "Tần suất (Frequency)") +
  theme_minimal()

## 7.3. Trực quan 4 hàm phân phối trong cùng một plot
n <- 10; p <- 0.4; x_v <- 0:n; pb <- pbinom(x_v, n, p)
df <- tibble(x = x_v, pmf = dbinom(x_v, n, p), cdf = pb)
df_q <- tibble(y = x_v, p_start = c(0, pb[-(n+1)]), p_end = pb)

p1 <- ggplot(df, aes(x, pmf)) +
  geom_col(fill = "lightblue", color = "white") +
  labs(title = "Hàm khối xác suất (dbinom)", y = "P(X=x)") +
  scale_x_continuous(breaks = x_v)

p2 <- ggplot(tibble(s = rbinom(1000, n, p)), aes(s)) +
  geom_bar(fill = "coral", color = "white") +
  labs(title = "Mô phỏng 1.000 lần (rbinom)", y = "Tần suất", x = "x") +
  scale_x_continuous(breaks = seq(0, 10, 1))

p3 <- ggplot(df, aes(x, cdf)) +
  geom_segment(aes(xend = x + 1, yend = cdf), color = "blue") +
  geom_point(aes(x = x + 1), shape = 21, fill = "white", color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Hàm xác suất tích lũy (pbinom)", y = "F(x) = p") +
  scale_x_continuous(breaks = seq(0, 10, 1))

p4 <- ggplot(df_q, aes(y = y)) +
  geom_segment(aes(x = p_start, xend = p_end, yend = y), color = "darkgreen") +
  geom_point(aes(x = p_start), shape = 21, fill = "white", color = "darkgreen") +
  geom_point(aes(x = p_end), color = "darkgreen") +
  labs(title = "Hàm quantile (qbinom)", x = "F(x) = p", y = "x") +
  scale_y_continuous(breaks = seq(0, 10, 1))

(p1 | p2) / (p3 | p4) &
  theme_minimal() &
  theme(panel.grid = element_blank())


# ─────────────────────────────────────────────
# II. Phân phối liên tục (Continuous Distribution)
# ─────────────────────────────────────────────

### 8. Các phân phối liên tục cơ bản -----------------
## 8.1. Phân phối Đều (Uniform)
# f(x) = 1/(b-a), a<=x<=b ; E(X)=(a+b)/2 ; Var(X)=(b-a)^2/12
# dunif(x, min, max); punif(); qunif(); runif()

## 8.2. Phân phối Mũ (Exponential)
# f(x) = lambda*e^(-lambda*x), x>=0 ; E(X)=1/lambda ; Var(X)=1/lambda^2
# dexp(x, rate); pexp(); qexp(); rexp()

## 8.3. Phân phối Chuẩn (Normal)
# f(x) = 1/sqrt(2*pi*sigma^2) * exp(-(x-mu)^2/(2*sigma^2))
# dnorm(x, mean, sd); pnorm(); qnorm(); rnorm()


### 9. Tính toán cơ bản với phân phối liên tục -------
# Bảng quy tắc khi F liên tục:
# f_X(t)                     -> dF(t,...)
# P(X<=t) = P(X<t)           -> pF(t,...)
# P(a<=X<=b) (mọi biên)      -> pF(b,...) - pF(a,...)
# P(X>=a) = P(X>a)           -> 1 - pF(a,...)
# F^-1(c)                    -> qF(c,...)

### 10. Trực quan phân phối liên tục -----------------
## 10.1. Làm quen với geom_function()
# geom_function(mapping, data, fun, xlim, n = 101, args = list(), ...)

ggplot() +
  xlim(-3, 3) +
  geom_function(fun = function(x) x^2, color = "blue", linewidth = 1) +
  labs(title = "Hàm f(x) = x^2", y = "f(x)")

ggplot() +
  xlim(-pi, pi) +
  geom_function(fun = sin, color = "blue", linewidth = 1) +
  labs(title = "Hàm f(x) = sin(x)", y = "f(x)") +
  theme_minimal()

# Nhiều hàm trong cùng một đồ thị
ggplot() +
  geom_function(fun = function(x) x^2, aes(color = "x^2"), linewidth = 1) +
  geom_function(fun = function(x) x^3, aes(color = "x^3"), linewidth = 1) +
  xlim(-3, 3) +
  labs(title = "So sánh f(x) = x^2 và g(x) = x^3", x = "x", y = "Giá trị hàm", color = "Hàm số") +
  scale_color_manual(values = c("x^2" = "blue", "x^3" = "red")) +
  theme_minimal()

## 10.2. Trực quan phân phối liên tục
# X ~ N(mu=15, sigma^2=3^2)
ggplot() +
  geom_function(fun = dnorm, args = list(mean = 15, sd = 3),
                xlim = c(4, 26), color = "blue", linewidth = 1) +
  labs(title = "PDF của Normal(15, 3^2)", x = "X", y = "f(x)") +
  theme_minimal()

ggplot() +
  geom_function(fun = pnorm, args = list(mean = 15, sd = 3),
                xlim = c(4, 26), color = "red", linewidth = 1) +
  labs(title = "CDF của Normal(15, 3^2)", x = "X", y = "f(x)") +
  theme_minimal()

ggplot() +
  geom_function(fun = qnorm, args = list(mean = 15, sd = 3),
                xlim = c(0, 1), color = "darkgreen", linewidth = 1) +
  labs(title = "Hàm phân vị của Normal(15, 3^2)",
       x = "Xác suất tích lũy (p)", y = "Giá trị phân vị (x)") +
  theme_minimal()

# 4 hàm phân phối chuẩn tắc trong cùng một plot
p1 <- ggplot() +
  geom_function(fun = dnorm, args = list(mean = 0, sd = 1),
                xlim = c(-4, 4), color = "blue", linewidth = 1) +
  labs(title = "Hàm mật độ xác suất (dnorm)", x = "Giá trị (x)", y = "f(x)") +
  theme_minimal()

df_sim <- tibble(val = rnorm(1000, mean = 0, sd = 1))
p2 <- ggplot(df_sim, aes(x = val)) +
  geom_histogram(aes(y = after_stat(density)), fill = "coral", color = "white") +
  geom_density(color = "blue", linewidth = 1) +
  labs(title = "Mô phỏng 1.000 mẫu (rnorm)", x = "Giá trị mô phỏng", y = "Mật độ") +
  theme_minimal()

p3 <- ggplot() +
  geom_function(fun = pnorm, args = list(mean = 0, sd = 1),
                xlim = c(-4, 4), color = "red", linewidth = 1) +
  labs(title = "Hàm tích lũy (pnorm)", x = "Giá trị (x)", y = "F(x)") +
  theme_minimal()

p4 <- ggplot() +
  geom_function(fun = qnorm, args = list(mean = 0, sd = 1),
                xlim = c(0, 1), color = "darkgreen", linewidth = 1) +
  labs(title = "Hàm phân vị (qnorm)", x = "Xác suất tích lũy (p)", y = "Giá trị (x)") +
  theme_minimal()

(p1 | p2) / (p3 | p4)


# ─────────────────────────────────────────────
# BÀI TẬP
# ─────────────────────────────────────────────

#========================================#
#Bài tập 1.1: Mô phỏng 1000 lần tung xúc xắc và kiểm tra xác suất lý thuyết
set.seed(2025)
mo_phong_1000 <- sample(x = 1:6, size = 1000, replace = T)

thong_ke_1000 <- tibble(mat_so = mo_phong_1000) %>% 
  count(mat_so) %>% 
  rename(tan_so = n) %>% 
  mutate(tan_suat = tan_so / sum(tan_so),
         chenh_lech = tan_suat - 1/6)

thong_ke_1000 %>% 
  ggplot(aes(x = factor(mat_so), y = tan_suat)) +
  geom_col(fill = "steelblue") + 
  geom_hline(yintercept = 1/6, linetype = "dashed", color = "red") +
  theme_minimal()

# Barplot cho 10.000, 100.000, 1.000.000 lần tung
ve_barplot <- function(size) {
  mo_phong <- sample(x = 1:6, size = size, replace = T)
  
  tibble(mat_so = mo_phong) %>%
    ggplot(aes(x = factor(mat_so))) +
    geom_bar(fill = "steelblue") + 
    geom_hline(yintercept = size/6, linetype = "dashed", color = "red") +
    theme_minimal()
}

ve_barplot(1000)
ve_barplot(10000)
ve_barplot(100000)
ve_barplot(1000000)

#Bài tập 1.2: Mô phỏng xúc xắc hai lần liên tiếp
set.seed(2025)
lan1 <- sample(mat_xuc_xac, size = 100, replace = TRUE)
lan2 <- sample(mat_xuc_xac, size = 100, replace = TRUE)

# Thống kê tần suất từng lần
tibble(mat_so = lan1) %>% count(mat_so)
tibble(mat_so = lan2) %>% count(mat_so)

ggplot(tibble(mat_so = lan1), aes(x = factor(mat_so))) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(title = "Lần 1", x = "Mặt xúc xắc", y = "Tần suất") +
  theme_minimal()

ggplot(tibble(mat_so = lan2), aes(x = factor(mat_so))) +
  geom_bar(fill = "coral", color = "white") +
  labs(title = "Lần 2", x = "Mặt xúc xắc", y = "Tần suất") +
  theme_minimal()
# --> Nhận xét: phân phối tương đối đều nhau giữa các mặt xúc xắc.

# Biến tong: tổng số mặt xúc xắc của lần 1 và lần 2
tong <- lan1 + lan2
tibble(tong = tong) %>%
  count(tong) %>%
  mutate(xac_suat = n / sum(n))

ggplot(tibble(tong = tong), aes(x = factor(tong))) +
  geom_bar(fill = "darkgreen", color = "white") +
  labs(title = "Tổng hai lần tung xúc xắc", x = "Tổng", y = "Tần suất") +
  theme_minimal()

# Trực quan hóa biến tong với số lần mô phỏng khác nhau
ve_tong <- function(n) {
  l1 <- sample(mat_xuc_xac, size = n, replace = TRUE)
  l2 <- sample(mat_xuc_xac, size = n, replace = TRUE)
  ggplot(tibble(tong = l1 + l2), aes(x = factor(tong))) +
    geom_bar(fill = "darkgreen", color = "white") +
    labs(title = paste("Tổng 2 lần tung,", n, "lần mô phỏng"),
         x = "Tổng", y = "Tần suất") +
    theme_minimal()
}
ve_tong(1000)
ve_tong(10000)
ve_tong(100000)
# --> Nhận xét: khi số lần mô phỏng tăng, phân phối của "tong" càng rõ dạng
#     hình tam giác (giống phân phối tam giác), tập trung quanh giá trị 7.

#========================================#
#Bài tập 2.1: n = 8, p = 0.7
n <- 8; p <- 0.7
pbinom(3, size = n, prob = p)              # P(X <= 3)
1 - pbinom(2, size = n, prob = p)          # P(X >= 3)
pbinom(2, size = n, prob = p)              # P(X < 3)
dbinom(3, size = n, prob = p)              # P(X = 3)
pbinom(5, size = n, prob = p) - pbinom(2, size = n, prob = p)  # P(3<=X<=5)

#Bài tập 2.2: n = 9, p = 0.3
n <- 9; p <- 0.3
dbinom(2, size = n, prob = p)              # P(x = 2)
pbinom(1, size = n, prob = p)              # P(x < 2)
1 - pbinom(2, size = n, prob = p)          # P(x > 2)
pbinom(4, size = n, prob = p) - pbinom(1, size = n, prob = p)  # P(2<=x<=4)

#========================================#
#Bài tập 2.3: n=8,p=0.7 và n=9,p=0.3
tibble(x = 0:8, y = dbinom(0:8, size = 8, prob = 0.7)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_segment(aes(xend = x, yend = 0), color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 3) +
  labs(title = "PMF của Binomial(8, 0.7)", x = "x", y = "P(X = x)") +
  scale_x_continuous(breaks = 0:8) +
  theme_minimal()

tibble(x = 0:9, y = dbinom(0:9, size = 9, prob = 0.3)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_segment(aes(xend = x, yend = 0), color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  labs(title = "PMF của Binomial(9, 0.3)", x = "x", y = "P(X = x)") +
  scale_x_continuous(breaks = 0:9) +
  theme_minimal()

#Bài tập 2.4:
# (1) Cùng size = 20, các p khác nhau
tibble(
  x = rep(0:20, 3),
  y = c(dbinom(0:20, 20, 0.4),
        dbinom(0:20, 20, 0.1),
        dbinom(0:20, 20, 0.7)),
  probs = rep(c("p=0.4", "p=0.1", "p=0.7"), each = 21)
) %>%
  ggplot(aes(x = x, y = y, color = probs)) +
  geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Binomial(size=20, p khác nhau)", x = "x", y = "P(X=x)", color = "Tham số") +
  theme_minimal()

# (2) Cùng p = 0.4, các size khác nhau
tibble(
  x = c(0:20, 0:15, 0:10),
  y = c(dbinom(x = 0:20, size = 20, prob = 0.4),
        dbinom(x = 0:15, size = 15, prob = 0.4),
        dbinom(x = 0:10, size = 10, prob = 0.4)),
  sizes = c(rep("n = 20", times = 21),
            rep("n = 15", times = 16),
            rep("n = 10", times = 11))
) %>% 
  ggplot(aes(x = factor(x), y = y, fill = sizes)) +
  geom_col(position = "identity", alpha = 0.5) +
  labs(title = "Binomial(p=0.4, size khác nhau)", x = "x", y = "P(X=x)", color = "Tham số") +
  theme_minimal()

#========================================#
#Bài tập 4.1:
# (1) p = 0.4
dgeom(1, prob = 0.4); dgeom(2, prob = 0.4); dgeom(3, prob = 0.4)
pgeom(2, prob = 0.4)               # P(X<=2)
1 - pgeom(4, prob = 0.4)           # P(X>=5)
pgeom(5, prob = 0.4)               # P(X<6)
pgeom(4, prob = 0.4) - pgeom(0, prob = 0.4)  # P(1<=X<=4)

# (2) p = 0.3
dgeom(1, prob = 0.3); dgeom(2, prob = 0.3)
1 - pgeom(1, prob = 0.3)           # P(X>1)
pgeom(3, prob = 0.3)               # P(X<=3)
1 - pgeom(3, prob = 0.3)           # P(X>3)
pgeom(5, prob = 0.3) - pgeom(2, prob = 0.3)  # P(3<=X<=5)

# (3) p = 0.5
dgeom(1, prob = 0.5); dgeom(2, prob = 0.5)
1 - pgeom(1, prob = 0.5)           # P(X>1)
dgeom(5, prob = 0.5)               # P(X=5)

# (4) p = 0.8
dgeom(1, prob = 0.8)
pgeom(2, prob = 0.8)               # P(X<=2)
1 - pgeom(2, prob = 0.8)           # P(X>2)
pgeom(4, prob = 0.8) - pgeom(1, prob = 0.8)  # P(2<=X<=4)

#Bài tập 4.2: Trực quan trên cùng biểu đồ prob = {0.3, 0.5, 0.8}
tibble(
  x = rep(0:10, 3),
  prob_val = c(dgeom(0:10, 0.3), dgeom(0:10, 0.5), dgeom(0:10, 0.8)),
  p_label = rep(c("p=0.3", "p=0.5", "p=0.8"), each = 11)
) %>%
  ggplot(aes(x = x, y = prob_val, color = p_label)) +
  geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Phân phối hình học với các p khác nhau",
       x = "x", y = "P(X=x)", color = "Tham số") +
  theme_minimal()

#========================================#
#Bài tập 5.1: Trực quan 4 phân phối Poisson trên cùng biểu đồ
df_pois <- tibble(
  x = rep(0:15, 4),
  prob = c(dpois(0:15, 2.5), dpois(0:15, 3), dpois(0:15, 2), dpois(0:15, 0.8)),
  lambda_val = rep(c("lambda=2.5", "lambda=3", "lambda=2", "lambda=0.8"), each = 16)
)
ggplot(df_pois, aes(x = x, y = prob, color = lambda_val)) +
  geom_segment(aes(xend = x, yend = 0), linewidth = 1, alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Các phân phối Poisson", x = "x", y = "P(X=x)", color = "Tham số") +
  theme_minimal()

# (1) lambda = 2.5
dpois(0, 2.5); dpois(1, 2.5); dpois(2, 2.5)
ppois(2, 2.5)                      # P(x<=2)
1 - ppois(4, 2.5)                  # P(x>=5)
ppois(5, 2.5)                      # P(x<6)
ppois(4, 2.5) - ppois(0, 2.5)      # P(1<=x<=4)

# (2) lambda = 3
dpois(0, 3); dpois(1, 3)
1 - ppois(1, 3)                    # P(x>1)
ppois(3, 3)                        # P(x<=3)
1 - ppois(3, 3)                    # P(x>3)
ppois(5, 3) - ppois(2, 3)          # P(3<=x<=5)

# (3) lambda = 2
dpois(0, 2); dpois(1, 2)
1 - ppois(1, 2)                    # P(x>1)
dpois(5, 2)                        # P(x=5)

# (4) lambda = 0.8
dpois(0, 0.8)
ppois(2, 0.8)                      # P(x<=2)
1 - ppois(2, 0.8)                  # P(x>2)
ppois(4, 0.8) - ppois(1, 0.8)      # P(2<=x<=4)

#========================================#
#Bài tập 6.1: CDF của Geometric(p=0.3) và Poisson(lambda=2), kết hợp PMF

# Geometric(p = 0.3)
x_geom <- 0:15
df_geom <- tibble(x = x_geom, pmf = dgeom(x_geom, 0.3), cdf = pgeom(x_geom, 0.3))

p1_geom <- ggplot(df_geom, aes(x, pmf)) +
  geom_col(fill = "lightblue", color = "white") +
  labs(title = "PMF Geometric(0.3)", y = "P(X=x)") +
  theme_minimal()

p2_geom <- ggplot(df_geom, aes(x, cdf)) +
  geom_segment(aes(xend = x + 1, yend = cdf), color = "blue") +
  geom_point(aes(x = x + 1), shape = 21, fill = "white", color = "blue") +
  geom_point(color = "blue") +
  labs(title = "CDF Geometric(0.3)", y = "F(x)") +
  theme_minimal()

(p1_geom | p2_geom)

# Poisson(lambda = 2)
x_pois <- 0:10
df_pois2 <- tibble(x = x_pois, pmf = dpois(x_pois, 2), cdf = ppois(x_pois, 2))

p1_pois <- ggplot(df_pois2, aes(x, pmf)) +
  geom_col(fill = "coral", color = "white") +
  labs(title = "PMF Poisson(2)", y = "P(X=x)") +
  theme_minimal()

p2_pois <- ggplot(df_pois2, aes(x, cdf)) +
  geom_segment(aes(xend = x + 1, yend = cdf), color = "red") +
  geom_point(aes(x = x + 1), shape = 21, fill = "white", color = "red") +
  geom_point(color = "red") +
  labs(title = "CDF Poisson(2)", y = "F(x)") +
  theme_minimal()

(p1_pois | p2_pois)

#========================================#
#Bài tập 6.2: Quantile của Geometric(p=0.3) và Poisson(lambda=2), kết hợp PMF

ve_quantile <- function(pmf_fun, cdf_fun, gia_tri, mau, tieu_de) {
  p_pts <- c(0, cdf_fun(gia_tri))
  p_trai <- p_pts[1:length(p_pts) - 1]
  p_phai <- p_pts[2:length(p_pts)]
  ggplot(tibble(gia_tri, p_trai, p_phai)) +
    geom_segment(aes(y = gia_tri, x = p_trai, yend = gia_tri, xend = p_phai),
                 linewidth = 1, color = mau) +
    geom_point(aes(x = p_trai, y = gia_tri), shape = 21, fill = "white", color = mau) +
    geom_point(aes(x = p_phai, y = gia_tri), color = mau) +
    labs(title = tieu_de, x = "Xác suất tích lũy (p)", y = "Giá trị phân vị (x)") +
    theme_minimal()
}

ve_quantile(function(x) dgeom(x, 0.3), function(x) pgeom(x, 0.3),
            0:15, "blue", "Quantile - Geometric(0.3)")
ve_quantile(function(x) dpois(x, 2), function(x) ppois(x, 2),
            0:10, "red", "Quantile - Poisson(2)")

#========================================#
#Bài tập 9.1: X ~ Uniform(0,10)
punif(5, min = 0, max = 10)                              # P(x<5)
punif(7, min = 0, max = 10) - punif(3, min = 0, max = 10) # P(3<x<7)
1 - punif(8, min = 0, max = 10)                           # P(x>8)
punif(8.3, min = 0, max = 10) - punif(2.5, min = 0, max = 10) # P(2.5<x<8.3)

#Bài tập 9.2: X ~ Uniform(-1,1)
punif(0, min = -1, max = 1)                                # P(x<0)
1 - punif(0.7, min = -1, max = 1)                          # P(x>0.7)
punif(0.5, min = -1, max = 1) - punif(-0.5, min = -1, max = 1)  # P(-0.5<x<0.5)
punif(0.2, min = -1, max = 1) - punif(-0.7, min = -1, max = 1)  # P(-0.7<x<0.2)

#Bài tập 9.3: X ~ Exp(lambda=1)
1 - pexp(1, rate = 1)                    # P(x>1)
pexp(5, rate = 1) - pexp(1, rate = 1)    # P(1<x<5)
pexp(1.5, rate = 1)                      # P(x<1.5)
pexp(4, rate = 1) - pexp(2, rate = 1)    # P(2<x<4)

#Bài tập 9.4: X ~ Exp(lambda=0.2)
1 - pexp(6, rate = 0.2)                    # P(x>6)
pexp(6, rate = 0.2) - pexp(4, rate = 0.2)  # P(4<x<6)
pexp(5, rate = 0.2)                        # P(x<5)
pexp(8, rate = 0.2) - pexp(3, rate = 0.2)  # P(3<x<8)

#Bài tập 9.5: Z ~ N(0,1)
pnorm(c(2, 1.88, 2.81, 2.33, 1.645))                   # z < ...
1 - pnorm(c(1.16, 5, 2.81, 1.96))                       # z > ...
pnorm(2.33) - pnorm(-2.33)                              # -2.33<z<2.33
pnorm(3) - pnorm(-3)                                    # -3<z<3
pnorm(2.58) - pnorm(-2.58)                              # -2.58<z<2.58

#Bài tập 9.6: Z ~ N(0,1)
pnorm(1.6)                    # (a) trái của 1.6
pnorm(1.83)                   # (b) trái của 1.83
1 - pnorm(-1.83)              # (c) phải của -1.83
pnorm(4.18)                   # (d) trái của 4.18
1 - pnorm(-1.96)              # (e) phải của -1.96
pnorm(1.4) - pnorm(-1.4)      # (f) giữa -1.4 và 1.4
pnorm(0.68) - pnorm(-1.43)    # (g) giữa -1.43 và 0.68
pnorm(-0.44) - pnorm(-1.55)   # (h) giữa -1.55 và -0.44
1 - pnorm(1.34)                # (i) lớn hơn 1.34
pnorm(-4.32)                   # (j) nhỏ hơn -4.32
pnorm(1.74) - pnorm(0.58)      # (k) giữa 0.58 và 1.74
pnorm(1.96) - pnorm(-1.96)     # (l) giữa -1.96 và 1.96

# Phân vị thứ 90, 95, 98, 99
qnorm(c(0.90, 0.95, 0.98, 0.99))

# Tìm c sao cho:
qnorm(1 - 0.025)                    # (a) P(z>c)=0.025
qnorm(0.9251)                       # (b) P(z<c)=0.9251
qnorm((1 + 0.8262)/2)               # (c) P(-c<z<c)=0.8262
qnorm(0.9505)                       # (d) diện tích trái = 0.9505
qnorm(0.05)                         # (e) diện tích trái = 0.05
qnorm((1 + 0.90)/2)                 # (f) P(-c<z<c)=0.90
qnorm((1 + 0.99)/2)                 # (g) P(-c<z<c)=0.99

#Bài tập 9.7:
# (1) mu=10, sd=2: P(x>13.5)
1 - pnorm(13.5, mean = 10, sd = 2)
# (2) mu=10, sd=2: P(x<8.2)
pnorm(8.2, mean = 10, sd = 2)
# (3) mu=10, sd=2: P(9.4<x<10.6)
pnorm(10.6, mean = 10, sd = 2) - pnorm(9.4, mean = 10, sd = 2)
# (4) mu=1.2, sd=0.15: P(1.00<x<1.10)
pnorm(1.10, mean = 1.2, sd = 0.15) - pnorm(1.00, mean = 1.2, sd = 0.15)
# (5) mu=1.2, sd=0.15: P(x>1.38)
1 - pnorm(1.38, mean = 1.2, sd = 0.15)
# (6) mu=1.2, sd=0.15: P(1.35<x<1.50)
pnorm(1.50, mean = 1.2, sd = 0.15) - pnorm(1.35, mean = 1.2, sd = 0.15)
# (7) mu=35, sd=10: x sao cho diện tích 0.01 bên phải
qnorm(1 - 0.01, mean = 35, sd = 10)
# (8) mu=50, sd=15: x=0 có bất thường không?
pnorm(0, mean = 50, sd = 15)   # xác suất cực nhỏ --> x = 0 là bất thường

#========================================#
#Bài tập 10.1: Vẽ các hàm toán học trong cùng plot

# Hàm đa thức
ggplot() +
  geom_function(fun = function(x) x^2, aes(color = "x^2"), linewidth = 1) +
  geom_function(fun = function(x) x^3 - 3*x, aes(color = "x^3 - 3x"), linewidth = 1) +
  xlim(-4, 4) +
  labs(title = "Hàm đa thức", x = "x", y = "y", color = "Hàm số") +
  theme_minimal()

# Hàm mũ và logarit
ggplot() +
  geom_function(fun = exp, aes(color = "e^x"), linewidth = 1) +
  geom_function(fun = log, aes(color = "log(x)"), linewidth = 1) +
  geom_function(fun = function(x) 10^x, aes(color = "10^x"), linewidth = 1) +
  geom_function(fun = sqrt, aes(color = "sqrt(x)"), linewidth = 1) +
  xlim(0.01, 3) +
  ylim(0, 10) +
  labs(title = "Hàm mũ và logarit", x = "x", y = "y", color = "Hàm số") +
  theme_minimal()

# Hàm lượng giác
ggplot() +
  geom_function(fun = sin, aes(color = "sin(x)"), linewidth = 1) +
  geom_function(fun = cos, aes(color = "cos(x)"), linewidth = 1) +
  geom_function(fun = tan, aes(color = "tan(x)"), linewidth = 1) +
  xlim(-pi, pi) +
  ylim(-5, 5) +
  labs(title = "Hàm lượng giác", x = "x", y = "y", color = "Hàm số") +
  theme_minimal()