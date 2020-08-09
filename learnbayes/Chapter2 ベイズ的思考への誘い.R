# Chapter2 ベイズ的思考への誘い
# 2.3 離散事前分布の利用
## 事前の情報を元に、pの値を重み付けして設定する
p <- seq(0.05, 0.95, by=0.1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior/sum(prior)
plot(p, prior, type="h", ylab="Prior Probability")

## 事後確率をpdiscを利用して求める
library(LearnBayes)
data <- c(11, 16)
post <- pdisc(p, prior, data)
round(cbind(p, prior, post), 2)

## 事前確率・事後確率を比較
library(lattice)
PRIOR <- data.frame("Prior", p, prior)
POST <- data.frame("Posterior", p, post)
names(PRIOR) <- c("Type", "P", "Probability")
names(POST) <- c("Type", "P", "Probability")
data <- rbind(PRIOR, POST)
xyplot(Probability ~ P|Type, data=data, layout=c(1,2), type="h", lwd=3, col="black")

## ベータ関数を事前分布として利用する
quantile2 <- list(p=.9, x=.5)
quantile1 <- list(p=.5, x=.3)
beta.select(quantile1, quantile2)
a <- 3.26
b <- 7.19
s <- 11
f <- 16
curve(dbeta(x, a+s, b+f), from=0, to=1,
      xlab="p", ylab="Density", lty=1, lwd=4)
curve(dbeta(x, s+1, f+1), add=TRUE, lty=2, lwd=4)
curve(dbeta(x,a,b), add=TRUE, lty=3, lwd=4)
legend(.65, 3, c("Prior", "Likedlihood", "Postterior"), lty=c(3,2,1), lwd=c(3,3,3))

## 睡眠の十分な学生の割合が0.5より大きいか？
1 - pbeta(0.5, a+s, b+f)
qbeta(c(0.05, 0.95), a+s, b+f)

## シミュレーションで事後分布を要約してみる
ps <- rbeta(1000, a+s, b+f)
hist(ps, xlab="p", main="")
sum(ps>=0.5)/1000
quantile(ps, c(0.05,0.95))

## ヒストグラム事前分布の利用
midpt <- seq(0.05, 0.95, by=0.1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior/sum(prior)
curve(histprior(x, midpt, prior), from=0, to=1,
      ylab = "Prior density", ylim = c(0, .3))
curve(histprior(x, midpt, prior) * dbeta(x, s+1, f+1), from=0, to=1,
      ylab = "Posterior density")

p <- seq(0, 1, length=500)
post <- histprior(p, midpt, prior) * dbeta(p, s+1, f+1)
post = post/sum(post)
ps <- sample(p, replace=TRUE, prob=post)
hist(ps, xlab="p", main="")

## 予測
## 事前予測密度を離散分布として求める
p <- seq(0.05, 0.95, by=0.1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior/sum(prior)
m <- 20; ys <- 0:20
pred <- pdiscp(p, prior, m, ys)
round(cbind(0:20, pred), 3)

## ベータ事前密度を利用する
ab <- c(3.26, 7.19)
m <- 20; ys <- 0:20
pred <- pbetap(ab, m, ys)

## 事前ベータ密度を利用して、追加20人分の予測を行う
p <- rbeta(1000, 3.26, 7.19)
y <- rbinom(1000, 20, p)
table(y)
freq <- table(y)
ys <- as.integer(names(freq))
predprob <- freq/sum(freq)
plot(ys, predprob, type="h", xlab="y",
     ylab = "Predictive Probability")

dist <- cbind(ys, predprob)
covprob <- .9
discint(dist, covprob)

### discintによって得られた信頼区間は、母集団の確率区間よりも相対的に広くなる。
### これはpの不確実性と、yの値の不確実性による。

# 2.9 練習問題
## 1. 離散事前分布による割合の推定
p <- seq(0, 1, by=0.125)
prior <- c(.001, .001, .950, .008, .008, .008, .008, .008, .008)
data <- c(6, 4)
post <- pdisc(p, prior, data)
round(cbind(p, prior, post), 3)

PRIOR <- data.frame("Prior", p, prior)
POST <- data.frame("Posterior", p, post)
names(PRIOR) <- c("Type", "P", "Probability")
names(POST) <- c("Type", "P", "Probability")
data <- rbind(PRIOR, POST)
xyplot(Probability ~ P|Type, data=data, layout=c(1,2), type="h", lwd=3, col="black")


## 2. ヒストグラム事前分布による割合の推定
midpt <- seq(0.05, 0.95, by=0.1)
prior <- c(0.005, 0.005, 0.01, 0.03, 0.45, 0.45, 0.03, 0.01, 0.005, 0.005)
s <- 12
f <- 8
curve(histprior(x, midpt, prior), from=0, to=1,
      ylab = "Prior density", ylim = c(0, .5))
curve(histprior(x, midpt, prior) * dbeta(x, s+1, f+1), from=0, to=1,
      ylab = "Posterior density")

p <- seq(0, 1, length=500)
post <- histprior(p, midpt, prior) * dbeta(p, s+1, f+1)
post = post/sum(post)
ps <- sample(p, replace=TRUE, prob=post)
hist(ps, xlab="p", main="")


## 3. 将来の標本の割合と予測分布の推定
a = 1
b = 1
s = 22
f = 7
qbeta(c(0.05, 0.95), a+s, b+f)
1 - pbeta(0.6, a+s, b+f)
ps <- rbeta(1000, a+s, b+f)
hist(ps, xlab="p", main="")
sum(ps>=0.5)/1000
quantile(ps, c(0.05, 0.95))

y <- rbinom(1000, 0:10, ps)
table(y)
freq <- table(y)
ys <- as.integer(names(freq))
predprob <- freq/sum(freq)
plot(ys, predprob, type="h", xlab="y",
     ylab = "Predictive Probability")
predprob[10]
predprob[11]


## 4. 二つの異なる事前分布による予測の比較
library(LearnBayes)
p <- seq(0.1, 0.5, by=0.1)
prior_j <- c(0.5, 0.2, 0.2, 0.05, 0.05)
plot(p, prior_j, type="h")
quantile1 <- list(p=0.5, x=0.1)
quantile2 <- list(p=0.9, x=0.3)
beta.select(quantile1, quantile2)
a_j <- 0.97
b_j <- 6.33
a_s <- 3
b_s <- 12

beta_mv <- function(a, b){
   beta_mean <- a/(a+b)
   beta_variance <- beta_mean*(1-beta_mean)/(a+b+1)
   return(list(beta_mean, beta_variance))
}
j_mv <- beta_mv(a_j, b_j)
s_mv <- beta_mv(a_s, b_s)
j_mv
s_mv

p
prior_j
m <- 12; ys <- 0:12
pred_j <- pdiscp(p, prior_j, m, ys)
round(cbind(0:12, pred_j), 3)

ab_s <- c(3, 12) 
pred_s <- pbetap(ab_s, m, ys)
round(cbind(0:12, pred_s), 3)


## 5. 離散事前分布を使った正規分布の平均推定
mu <- seq(20, 70, by=10)
prior <- c(0.1, 0.15, 0.25, 0.25, 0.15, 0.1)
y <- c(38.6, 42.4, 57.5, 40.5, 51.7, 67.1, 33.4, 60.9, 64.1, 40.1, 40.7, 6.4)
ybar <- mean(y)
yvar <- var(y)
n <- length(y)
like <- exp(-n*(mu-ybar)^2/(2*yvar))
post <- prior * like / sum(prior*like)
round(cbind(mu, post), 3)
dist <- cbind(mu, post)
discint(dist, prob=.8)


## 6. 離散事前密度を利用したポアソン平均の推定
la <- seq(0.5, 3, by=0.5)
prior <- c(0.1, 0.2, 0.3, 0.2, 0.15, 0.05)
t <- 6
y <- 12
post <- prior * exp(-t*la)*((t*la)^y)
post <- post/sum(post)
round(cbind(la, prior, post), 3)
sum(post * exp(-7*la))
