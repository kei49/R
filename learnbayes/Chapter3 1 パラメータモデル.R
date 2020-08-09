# Chapter3 1 パラメータモデル
## 3.2 平均が既知で分散が未知の正規分布
library(LearnBayes)
data("footballscores")
attach(footballscores)
d <- favorite - underdog - spread
n <- length(d)
v <- sum(d^2)

P <- rchisq(1000, n)/v
s <- sqrt(1/P)
hist(s, main="")
quantile(s, probs=c(0.025, 0.5, 0.0975))

## 3.3 心臓移植手術の死亡率を推定する
alpha <- 16; beta <- 15174
yobs <- 1; ex <- 66
y <- 0:10
lam <- alpha/beta
py <- dpois(y, lam * ex) * dgamma(lam,shape = alpha, rate = beta)/dgamma(lam, shape = alpha + y, rate = beta + ex)
cbind(y, round(py, 3))
lambdaA <- rgamma(1000, shape = alpha + yobs, rate = beta + ex)

yobs <- 4; ex <- 1767
y <- 0:10
py <- dpois(y, lam * ex) * dgamma(lam,shape = alpha, rate = beta)/dgamma(lam, shape = alpha + y, rate = beta + ex)
cbind(y, round(py, 3))
lambdaB <- rgamma(1000, shape = alpha + yobs, rate = beta + ex)

par(mfrow = c(2, 1))
plot(density(lambdaA), main="HOSPITAL A",
     xlab = "lambdaA", lwd = 3)
curve(dgamma(x, shape = alpha, rate = beta), add = TRUE)
legend("topright", legend = c("prior", "posterior"), lwd = c(1, 3))
plot(density(lambdaB), main="HOSPITAL B",
     xlab = "lambdaB", lwd = 3)
curve(dgamma(x, shape = alpha, rate = beta), add = TRUE)
legend("topright", legend = c("prior", "posterior"), lwd = c(1, 3))


## 3.4 ベイズ法の頑健性について
### 正規分布を利用した事後密度の計算
quantile1 <- list(p=.5, x=100)
quantile2 <- list(p=.95, x = 120)
normal.select(quantile1, quantile2)

mu <- 100
tau <- 12.16
sigma <- 15
n <- 4
se <- sigma/sqrt(4)
ybar <- c(110, 125, 140)
tau1 <- 1/sqrt(1/se^2 + 1/tau^2)
mu1 <- (ybar/se^2 + mu/tau^2) * tau1^2
summ1 <- cbind(ybar, mu1, tau1)
summ1

### t密度を利用した事後密度の計算
tscale <- 20/qt(0.95, 2)  
tscale
par(mfrow <- c(1, 1))
curve(1/tscale * dt((x-mu)/tscale, 2),
      from = 60, to = 140, xlab = "theta", ylab = "Prior Density")  
curve(dnorm(x, mean = mu, sd = tau), add =TRUE, lwd = 3)
legend("topright", legend=c("t density", "normal density"), lwd = c(1, 3))

norm.t.compute <- function(ybar) {
  theta <- seq(60, 180, length = 500)
  like <- dnorm(theta, mean = ybar, sd = sigma/sqrt(n))
  prior <- dt((theta - mu)/tscale, 2)
  post <- prior * like
  post <- post/sum(post)
  m <- sum(theta * post)
  s <- sqrt(sum(theta^2 * post) - m^2)
  c(ybar, m, s)
}
summ2 <- t(sapply(c(110, 125, 140), norm.t.compute))
dimnames(summ2)[[2]] <- c("ybar", "mu1 t", "tau1 t")
summ2

cbind(summ1, summ2)

theta <- seq(60, 180, length = 500)
normpost <- dnorm(theta, mu1[3], tau1)
normpost <- normpost/sum(normpost)
plot(theta, normpost,type="l", lwd=3, ylab="Posterior Density")
like <- dnorm(theta, mean=140, sd=sigma/sqrt(n))
prior <- dt((theta - mu)/tscale, 2)
tpost <- prior * like / sum(prior * like)
lines(theta, tpost)
legend("topright", legend = c("t prior", "normal prior"), lwd = c(1, 3))


## 3.5 共役事前分布の混合形
library(LearnBayes)
probs <- c(.5, .5)
beta.par1 <- c(6, 14)
beta.par2 <- c(14, 6)
betapar <- rbind(beta.par1, beta.par2)
data <- c(7, 3)
post <- binomial.beta.mix(probs, betapar, data)
post

curve(post$probs[1] * dbeta(x, 13, 17)
      + post$probs[2] * dbeta(x, 21, 9),
      from = 0, to = 1, lwd =3, xlab = "P", ylab="DENSITY")
curve(.5 * dbeta(x, 6, 14) + .5 * dbeta(x, 14, 6), 0, 1, add = TRUE)
legend("topleft", legend=c("Prior", "Posterior"), lwd=c(1, 3))


## 3.6 コインの偏りについてのベイズ検定

