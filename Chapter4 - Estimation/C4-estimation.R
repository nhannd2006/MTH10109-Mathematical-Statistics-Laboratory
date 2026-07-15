# 28/5 - 24KDL
# Chương 4: ƯỚC LƯỢNG ĐIỂM VÀ KHOẢNG TRONG R

library(ggplot2)
library(dplyr)

# ─────────────────────────────────────────────
# 1. Ước lượng điểm (Point Estimation)
# ─────────────────────────────────────────────

## 1.1. Phương pháp Mô-men (Method of Moments - MoM)
# Bản chất: đồng nhất mô-men mẫu với mô-men lý thuyết của tổng thể để giải ra tham số.
# mean(x)  -> mô-men mẫu bậc 1 (Xbar)
# sd(x)/var(x) -> mô-men trung tâm mẫu bậc 2 (đã hiệu chỉnh Bessel, chia n-1)
# MoM là một THUẬT TOÁN tìm tham số, dùng mean()/var() làm nguyên liệu đầu vào.

# Ví dụ 1.1: MoM cho phân phối chuẩn X ~ N(10, 2^2)
set.seed(123)
n <- 50
true_mu <- 10
true_sigma <- 2
x <- rnorm(n, mean = true_mu, sd = true_sigma)

# MoM cho kỳ vọng (mu)
mu_mom <- sum(x) / n
# MoM cho phương sai (sigma^2): chia cho n (không phải n-1)
var_mom <- sum((x - mu_mom)^2) / n
# MoM cho độ lệch chuẩn
sd_mom <- sqrt(var_mom)

# So sánh với hàm có sẵn của R
mu_r <- mean(x)
sd_r <- sd(x)
var_r <- var(x)

comparison_table <- data.frame(
  Dac_Trung = c("Kỳ vọng (Mean)", "Phương sai (Variance)", "Độ lệch chuẩn (SD)"),
  MoM = c(mu_mom, var_mom, sd_mom),
  R_function = c(mu_r, var_r, sd_r)
)
comparison_table
# --> sd()/var() của R dùng hiệu chỉnh Bessel (chia n-1) để ước lượng không chệch,
#     còn MoM lấy đúng mô-men mẫu (chia n).

# Nếu muốn dùng hàm var() của R để tính MoM, cần nhân lại (n-1)/n
var_mom_tu_R <- (n - 1) * var(x) / n
var_mom_tu_R

# Trực quan histogram + đường cong Normal theo MoM
tibble(x) %>%
  ggplot(aes(x)) +
  geom_histogram(aes(y = after_stat(density),
                     fill = "Phân phối mẫu"),
                 color = "white") +
  geom_function(
    fun = dnorm,
    args = list(mean = mu_mom, sd = sd_mom),
    aes(color = "Normal Curve theo MoM"),
    linewidth = 1
  ) +
  scale_color_manual(values = c("Normal Curve theo MoM" = "blue")) +
  scale_fill_manual(values = c("Phân phối mẫu" = "lightblue")) +
  labs(title = "Phân phối dữ liệu mẫu và đường cong Normal",
       subtitle = "Theo ước lượng MoM",
       color = "", fill = "") +
  theme_bw() +
  theme(legend.position = "bottom")

# Ví dụ 1.2: MoM cho phân phối đều X ~ U(a, b)
# Hệ phương trình mô-men:
#   (a+b)/2 = Xbar         -> a + b = 2*Xbar
#   (b-a)^2/12 = S^2_MoM   -> (b-a)^2 = 12*S^2_MoM
# Giải ra: a_hat = Xbar - sqrt(3*S^2_MoM) ; b_hat = Xbar + sqrt(3*S^2_MoM)

set.seed(42)
true_a <- 2
true_b <- 10
n_size <- 500
sample_data <- runif(n_size, min = true_a, max = true_b)

estimate_uniform_mom <- function(data) {
  n <- length(data)
  x_bar <- mean(data)
  s2_mom <- (n - 1) * var(data) / n
  a_hat <- x_bar - sqrt(3 * s2_mom)
  b_hat <- x_bar + sqrt(3 * s2_mom)
  return(c(a_estimate = a_hat, b_estimate = b_hat))
}

results <- estimate_uniform_mom(sample_data)
results

# Bảng tổng hợp công thức MoM cho các phân phối thường gặp (tham khảo):
# Bernoulli(p):      p_hat = Xbar
# Binomial(k,p):     p_hat = Xbar/k  (k đã biết)
# Poisson(lambda):   lambda_hat = Xbar
# Exponential(lambda): lambda_hat = 1/Xbar
# Normal(mu, sigma^2): mu_hat = Xbar ; sigma^2_hat = S^2
# Uniform(a,b):      a_hat = Xbar - sqrt(3*S^2) ; b_hat = Xbar + sqrt(3*S^2)
# Gamma(alpha, lambda): alpha_hat = Xbar^2/S^2 ; lambda_hat = Xbar/S^2
# Beta(alpha, beta): alpha_hat = Xbar*(Xbar(1-Xbar)/S^2 - 1)
#                    beta_hat  = (1-Xbar)*(Xbar(1-Xbar)/S^2 - 1)


## 1.2. Phương pháp Hợp lý cực đại (Maximum Likelihood Estimation - MLE)
# L(theta|x) = prod f(xi|theta)
# log-likelihood: l(theta|x) = sum ln f(xi|theta)
# theta_MLE = argmax l(theta|x)
# Trong R: dùng nlminb()/optim() để CỰC TIỂU hóa Negative Log-Likelihood (NLL = -l)

# Ví dụ 1.3: MLE cho phân phối chuẩn X ~ N(mu, sigma^2)
set.seed(42)
n_size <- 200
true_mu <- 15
true_sigma <- 3
x <- rnorm(n_size, mean = true_mu, sd = true_sigma)

neg_log_lik_normal <- function(params, data) {
  mu_guess <- params[1]
  sigma_guess <- params[2]
  log_lik_vector <- dnorm(data, mean = mu_guess, sd = sigma_guess, log = TRUE)
  nll <- -sum(log_lik_vector)
  return(nll)
}

start_values <- c(mu = 0, sigma = 1)
mle_optim <- nlminb(start = start_values,
                    objective = neg_log_lik_normal,
                    data = x,
                    lower = c(-Inf, 1e-5))

mu_mle <- mle_optim$par[1]
sigma_mle <- mle_optim$par[2]
cat(" - Ước lượng mu: ", mu_mle, "\n")
cat(" - Ước lượng sigma: ", sigma_mle, "\n\n")


# ─────────────────────────────────────────────
# 2. Ước lượng khoảng (Interval Estimation)
# ─────────────────────────────────────────────

## 2.1. Khoảng tin cậy cho trung bình tổng thể (mu)
# (Xbar - t(n-1,1-a/2)*S/sqrt(n) , Xbar + t(n-1,1-a/2)*S/sqrt(n))
T1 <- c(5,3,6,4,6,7,6,9,10,10,8,7,6,4,3,5,6,7,2,3,9,8,8,9,10,4,5,6,8,4,3)

# Tính thủ công
n <- length(T1)
x_bar <- mean(T1)
s <- sd(T1)
margin <- qt(0.975, df = n - 1) * s / sqrt(n)
CI_manual <- c(x_bar - margin, x_bar + margin)
CI_manual

# Dùng hàm có sẵn t.test()
t.test(T1, conf.level = 0.95)$conf.int

## 2.2. Khoảng tin cậy cho hiệu số hai trung bình (mu1 - mu2)
# Giả định hai tổng thể chuẩn, phương sai bằng nhau (sigma1^2 = sigma2^2)
# Sp^2 = ((n1-1)*S1^2 + (n2-1)*S2^2) / (n1+n2-2) ; df = n1+n2-2
T2 <- c(5,3,6,4,6,7,6,9,10,10,8,7,6,4,3,5,6,
        7,2,3,9,8,8,9,10,4,5,6,8,4,3)
t.test(T1, T2, var.equal = TRUE, conf.level = 0.95)$conf.int

## 2.3. Khoảng tin cậy cho tỷ lệ (p)
# (p_hat - z(1-a/2)*sqrt(p_hat(1-p_hat)/n), p_hat + z(1-a/2)*sqrt(p_hat(1-p_hat)/n))
n <- 100
p_hat <- 48 / n
margin <- qnorm(0.975) * sqrt(p_hat * (1 - p_hat) / n)
CI_prop <- c(p_hat - margin, p_hat + margin)
CI_prop

## 2.4. Khoảng tin cậy cho phương sai (sigma^2)
# Dựa trên phân phối Chi-bình phương:
# ((n-1)*S^2/chi^2(n-1,1-a/2) , (n-1)*S^2/chi^2(n-1,a/2))
n <- length(T1)
s2 <- var(T1)
chi_lower <- qchisq(0.975, df = n - 1)
chi_upper <- qchisq(0.025, df = n - 1)
CI_var <- c((n - 1) * s2 / chi_lower, (n - 1) * s2 / chi_upper)
CI_var

## 2.5. Khoảng tin cậy cho trung vị phi tham số (Median)
# Dựa trên thống kê thứ tự X(k), m1/m2 xác định từ phân vị Binom(n, 0.5)
n <- length(T1)
alpha <- 0.05
m1 <- qbinom(alpha/2, size = n, prob = 0.5)
m2 <- qbinom(1 - alpha/2, size = n, prob = 0.5)
sorted_T1 <- sort(T1)
CI_median <- c(sorted_T1[m1], sorted_T1[m2 + 1])
CI_median


# ─────────────────────────────────────────────
# BÀI TẬP
# ─────────────────────────────────────────────

#========================================#
#Exercise 1.1: Viết hàm và thử nghiệm MoM cho các phân phối trong bảng tổng hợp

mom_bernoulli <- function(data) {
  p_hat <- mean(data)
  return(c(p_hat = p_hat))
}

mom_binomial <- function(data, k) {
  p_hat <- mean(data) / k
  return(c(p_hat = p_hat))
}

mom_poisson <- function(data) {
  lambda_hat <- mean(data)
  return(c(lambda_hat = lambda_hat))
}

mom_exponential <- function(data) {
  lambda_hat <- 1 / mean(data)
  return(c(lambda_hat = lambda_hat))
}

mom_normal <- function(data) {
  mu_hat <- mean(data)
  n <- length(data)
  sigma2_hat <- (n - 1) * var(data) / n
  return(c(mu_hat = mu_hat, sigma2_hat = sigma2_hat))
}

mom_gamma <- function(data) {
  xbar <- mean(data)
  n <- length(data)
  s2 <- (n - 1) * var(data) / n
  alpha_hat <- xbar^2 / s2
  lambda_hat <- xbar / s2
  return(c(alpha_hat = alpha_hat, lambda_hat = lambda_hat))
}

mom_beta <- function(data) {
  xbar <- mean(data)
  n <- length(data)
  s2 <- (n - 1) * var(data) / n
  common <- xbar * (1 - xbar) / s2 - 1
  alpha_hat <- xbar * common
  beta_hat <- (1 - xbar) * common
  return(c(alpha_hat = alpha_hat, beta_hat = beta_hat))
}

# Thử nghiệm
set.seed(1)
mom_bernoulli(rbinom(1000, size = 1, prob = 0.4))
mom_binomial(rbinom(1000, size = 20, prob = 0.4), k = 20)
mom_poisson(rpois(1000, lambda = 5))
mom_exponential(rexp(1000, rate = 0.5))
mom_normal(rnorm(1000, mean = 20, sd = 4))
mom_gamma(rgamma(1000, shape = 3, rate = 2))
mom_beta(rbeta(1000, shape1 = 2, shape2 = 5))

#========================================#
#Exercise 1.2: Hàm Negative Log-Likelihood cho Bernoulli, Poisson, Mũ

# (1) Bernoulli: X ~ Bern(p)
# l(p|x) = (sum xi)*ln(p) + (n - sum xi)*ln(1-p)
neg_log_lik_bernoulli <- function(params, data) {
  p_guess <- params[1]
  n <- length(data)
  sum_x <- sum(data)
  ll <- sum_x * log(p_guess) + (n - sum_x) * log(1 - p_guess)
  return(-ll)
}

set.seed(1)
data_bern <- rbinom(500, size = 1, prob = 0.3)
mle_bern <- nlminb(start = c(p = 0.5),
                   objective = neg_log_lik_bernoulli,
                   data = data_bern,
                   lower = 1e-6, upper = 1 - 1e-6)
p_mle <- mle_bern$par
cat(" - Ước lượng p (Bernoulli): ", p_mle, "\n")

# (2) Poisson: X ~ P(lambda)
# l(lambda|x) = ln(lambda)*sum(xi) - n*lambda - sum(ln(xi!))
neg_log_lik_poisson <- function(params, data) {
  lambda_guess <- params[1]
  ll <- sum(dpois(data, lambda = lambda_guess, log = TRUE))
  return(-ll)
}

data_pois <- rpois(500, lambda = 4)
mle_pois <- nlminb(start = c(lambda = 1),
                   objective = neg_log_lik_poisson,
                   data = data_pois,
                   lower = 1e-6)
lambda_mle <- mle_pois$par
cat(" - Ước lượng lambda (Poisson): ", lambda_mle, "\n")

# (3) Mũ (Exponential): X ~ Exp(lambda)
# l(lambda|x) = n*ln(lambda) - lambda*sum(xi)
neg_log_lik_exp <- function(params, data) {
  lambda_guess <- params[1]
  ll <- sum(dexp(data, rate = lambda_guess, log = TRUE))
  return(-ll)
}

data_exp <- rexp(500, rate = 0.8)
mle_exp <- nlminb(start = c(lambda = 1),
                  objective = neg_log_lik_exp,
                  data = data_exp,
                  lower = 1e-6)
lambda_exp_mle <- mle_exp$par
cat(" - Ước lượng lambda (Exponential): ", lambda_exp_mle, "\n")