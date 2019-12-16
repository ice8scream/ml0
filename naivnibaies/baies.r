# source('naivnibaies/baies.r')
library("MASS")

n <- 300
sig1 <- matrix(c(1, 0, 0, 2), 2, 2)
sig2 <- matrix(c(1, 0, 0, 1), 2, 2)

print(sig1)
print(sig2)

mu1 <- c(5, 5)
mu2 <- c(10, 5)

xc1 <- mvrnorm(n=n, mu = mu1, Sigma = sig1)
xc2 <- mvrnorm(n=n, mu = mu2, Sigma = sig2)

xl <- min(xc1[,1], xc2[,1]) - 1
yl <- min(xc1[,2], xc2[,2]) - 1
xm <- max(xc1[,1], xc2[,1]) + 1
ym <- max(xc1[,2], xc2[,2]) + 1
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(xl, xm), ylim = c(yl, ym), main="Наивный байесовский классификатор")

colors <- c("deepskyblue1", "brown1")
points(xc1, pch=21, col=colors[1], bg=colors[1])
points(xc2, pch=21, col=colors[2], bg=colors[2])

# Мат ожидание
Mathw <- function(xs) {
  l <- dim(xs)[2]
  res <- matrix(NA, 1, l)
  for (i in seq(l)) {
    res[1, i] <- mean(xs[,i])
  }
  return(c(res))
}

# Дисперсия
Disp <- function(xs, mu) {
  rows <- dim(xs)[1]
  cols <- dim(xs)[2]
  
  res <- matrix(0, 1, cols)
  for (i in seq(rows)) {
    res <- res + (xs[i,] - mu)^2
  }
  
  return(c(res/(rows-1)))
}

# Плотность
Pyj <- function(x, M, D){
  return( (1/(D*sqrt(2*pi))) * exp(-1 * ((x - M)^2)/(2*D^2)) )
}

naivniBayes <- function(x, Mathw, Disp, Prob, Prior) {
  res <- log(Prob * Prior)
  l <- length(x)
  
  for (i in seq(l)) {
    p <- Pyj(x[i], Mathw[i], Disp[i])
    res <- res + log(p)
  }
  
  return(res)
}

m1 <- Mathw(xc1)
m2 <- Mathw(xc2)
d1 <- Disp(xc1, m1)
d2 <- Disp(xc2, m2)

l <- max(xm - xl, ym - yl)
x <- seq(xl, xm, l/50)
y <- seq(yl, ym, l/50)

for (i in x) {
  for (j in y) {
    res1 <- naivniBayes(c(i, j), m1, d1, 0.5, 1)
    res2 <- naivniBayes(c(i, j), m2, d2, 0.5, 1000)
    color <- ifelse(res1 > res2, colors[1], colors[2])
    points(i, j, pch = 21, col = color)
  }
}