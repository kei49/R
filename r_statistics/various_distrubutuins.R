# 二項分布
dbinom(7, 10, p=0.6)
dbinom(c(2, 5, 8), 10, p=0.6)

pbinom(3, 10, p=0.6)
sum(dbinom(1:3, 10, p=0.6))

barplot(dbinom(0:10, 10, 0.65), names=0:10, xlab="x")


# ポワソン分布
barplot(dpois(0:10, lambda=2.3), names=0:10, xlab="x")

years <- c(1, 6, 6, 8, 5, 7, 0, 1, 0)
m <- sum((0:8)*years)/sum(years)
m

data <- data.frame(years, sum(years)*dpois(0:8, lambda=m))
barplot(t(data), col=c("black", "gray"), beside=TRUE, names.arg=0:8, xlab="number of accidents")
legend("topleft", legend=c("real", "theory"), bg="transparent", fill=c("black", "gray"))


# 幾何分布
first <- rgeom(100000, prob=1/6)
hist(first, prob=TRUE)


# 負の二項分布
k <- 0:40
barplot(dnbinom(k, size=3, prob=0.2), names.arg=k)


k <- 0:20
barplot(dpois(k, lambda=5), names=k, xlab="x")
dpois(k, lambda=5)

k <- 0:40
barplot(dnbinom(k, size=10, prob=0.6), names.arg=k)


# 正規分布
curve(dnorm(x, mean=5, sd=1), 1, 9, type="l")
1-pnorm(9, mean=8, sd=2)

k <- 0:20
plot(k, dbinom(k, 20, prob=0.5), xlab="", ylab="")
par(new=TRUE)
curve(dnorm(x, mean=10, sd=sqrt(20*0.5*(1-0.5))), 0, 20, xlab="x", ylab="", axes=FALSE)

# 対数正規分布
curve(dlnorm(x, meanlog=1, sdlog=1), 0, 10, type="l")

# 指数分布
curve(dexp(x, rate=2.3))

n <- 10^3
r <- rexp(n, rate=2.3)
x <- 0
xnum <- 0
count <- 0
time <- 0
for(i in 1:n){
  time <- time + r[i]
  if(time < 1) count <- count + 1
  else {
    x[xnum] <- count
    xnum <- xnum + 1
    time <- 0
    count <- 0
  }
}
barplot(table(x)/xnum)

plot(c(0, 150), c(0, 1), type="n", axes=FALSE, xlab="", ylab="")
axis(1)
n <- 50
r <- rexp(n, rate=0.5)
pos <- numeric(n)
for(i in 1:n) pos[i] <- sum(r[1:i])
segments(pos, 0.2, pos, 0.8)

# コーシー分布
x <- seq(-3, 5, 0.1)
curve(dcauchy(x, location=1, scale=2), -3, 5)

# ワイブル分布
x <- seq(0, 5, by=0.01)
curve(dweibull(x, shape=5, scale=3), 0, 5)


# 対数正規分布を自作
rlognormal <- function(nsize=10, mean=1, sd=1) {
  a <- log(1+sd^2/mean^2)
  mean_log <- log(mean) - 0.5*a
  sd_log <- sqrt(a)
  return(rlnorm(nsize, meanlog=mean_log, sdlog=sd_log))
}
x <- rlognormal(10000, mean=10, sd=3)
hist(x, prob=TRUE)
