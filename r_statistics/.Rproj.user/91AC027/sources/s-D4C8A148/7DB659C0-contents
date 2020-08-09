create_sample <- function(n) {
  sample_mean <- numeric(length=10000)
  for(i in 1:10000) {
    sample <- rnorm(n=n, mean=0, sd=1)
    sample_mean[i] <- mean(sample)
  }
  sample_mean
}

t <- numeric(length=10000)
sup <- 0
for(i in 1:10000){
  sample1 <- rnorm(n=10, mean=0, sd=1)
  sample2 <- rnorm(n=10, mean=0.5, sd=1)
  result <- t.test(sample1, sample2, var.equal=TRUE)
  t[i] <- result[[1]]
  sup <- sup + ifelse(result[[3]]<0.05, 1, 0)
}


x <- runif(100000, min=-pi/2, max=pi/2)
y <- sin(x)
hist(y, prob=TRUE)
curve(1/(pi*sqrt(1-x^2)), -1, 1, add=TRUE)
