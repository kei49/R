##1.2.1 LearnBayesのライブラリをインストールし、データセットを利用する
library(LearnBayes)
data('studentdata')

# データの1行目表示
studentdata[1, ]

# 学生が夕食に飲みたいドリンクを棒グラフで表示
table(Drink)
barplot(table(Drink), xlab='Drink', ylab='Count')

# 睡眠時間のヒストグラムをプロット
hours.of.sleep <- WakeUp - ToSleep
hist(hours.of.sleep, main='')
# 男女別に表示
boxplot(hours.of.sleep ~ Gender, ylab='Hours of Sleep')

# 散髪データを男女別に集計する
female.Haircut <- Haircut[Gender=='female']
male.Haircut <- Haircut[Gender=='male']
summary(female.Haircut)
summary(male.Haircut)

# 睡眠時間と、就寝時間の相関をみてみる
plot(jitter(ToSleep), jitter(hours.of.sleep))
# lmで最小二乗法による当てはめをする
fit <- lm(hours.of.sleep ~ ToSleep)
fit
# ablineで散布図上に直線を表示
abline(fit)
 

## 1.3.2 t統計量を計算するための関数を作成する
# t統計量を計算することで、2つの正規分布x, yの平均が同じ稼働かを検定することができる
# 正規分布データを作成
x <- rnorm(10, mean=50, sd=10)
y <- rnorm(10, mean=50, sd=10)

m <- length(x)
n <- length(y)
sp <- sqrt(((m-1)*sd(x)^2 + (n-1)*sd(y)^2)/(m+n-2))
t.stat <- (mean(x)-mean(y))/(sp*sqrt(1/m + 1/n))

tstatistic <- function(x, y){
  m <- length(x)
  n <- length(y)
  sp <- sqrt(((m-1)*sd(x)^2 + (n-1)*sd(y)^2)/(m+n-2))
  t.stat <- (mean(x)-mean(y))/(sp*sqrt(1/m + 1/n))
  return(t.stat)
}

data.x <- c(1,4,3,6,5)
data.y <- c(5,4,7,6,10)
tstatistic(data.x, data.y)


## 1.3.3 モンテカルロシミュレーションを実装する
# 1.3.4 異なる仮説下での真の有意水準の挙動
alpha = .1
N <- 10000
n.reject <- 0
monte <- function(m,n){
  for(i in 1:N){
    ## 色んなx,yの組み合わせでシミュレーションする
    #x <- rnorm(m, mean=0, sd=1)
    #y <- rnorm(n, mean=0, sd=1)
    #x <- rnorm(m, mean=0, sd=1)
    #y <- rnorm(n, mean=0, sd=10)
    #x <- rt(m, df=4)
    #y <- rt(n, df=4)
    #x <- rexp(m, rate=1)
    #y <- rexp(n, rate=1)
    x <- rnorm(m, mean=10, sd=2)
    y <- rexp(n, rate=1/10)
    t.stat <- tstatistic(x, y)
    if(abs(t.stat) > qt(1-alpha/2, n+m-2))
      n.reject <- n.reject+1
  }
  true.sig.level <- n.reject/N
  return(true.sig.level)
}
monte(10,10)

# 正規母集団と指数母集団のケースをやり直す, 単なる棄却率ではなく、t検定量の分布を見る
m <- 10; n <- 10
my.tsimulation <- function()
  + tstatistic(rnorm(m, mean=10, sd=2), rexp(n, rate=1/10))
tstat.vector <- replicate(10000, my.tsimulation())
plot(density(tstat.vector), xlim=c(-5,8), ylim=c(0,.4),lwd=3)
curve(dt(x, df=18), add=TRUE)
legend(4, .3, c("exact", "t(18)"), lwd=c(3,1))


## 1.6練習問題
# 1
hist(Dvds)
summary(Dvds)
table(Dvds)
barplot(table(Dvds))

# 2
boxplot(Height ~ Gender)
output <- boxplot(Height ~ Gender)
output
male.Height <- Height[Gender=='male']
female.Height <- Height[Gender=='female']
mean(male.Height, na.rm=TRUE) - mean(female.Height, na.rm=TRUE)

# 3
plot(jitter(ToSleep), jitter(WakeUp))
fit <- lm(WakeUp ~ ToSleep)
abline(fit)
fit

# 4
binomial.conf.interval <- function(y,n){
  z <- qnorm(.95)
  phat <- y/n
  se <- sqrt(phat * (1-phat)/n)
  return(c(phat-z*se, phat+z*se))
}
y <- rbinom(20,20,.5)
binomial.conf.interval(y,20)