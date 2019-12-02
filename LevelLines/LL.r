# source('LevelLines/LL.r')

LevelLines <- function(mx, C = c(0,0), limits = matrix(c(-5, -5, 5, 5), nrow = 2, ncol = 2)) {
    print("------------------------------------------------\\")
    print("Matrix:")
    print("------------\\")
    print(mx)
    print("------------//")

    det <- det(paste("det = ", mx)
    message(det)

    a <- mx[2, 2] / det
    b <- -mx[1, 2] / det
    c <- -mx[2, 1] / det
    d <- mx[1, 1] / det

    A1 <- d
    A2 <- a
    A3 <- -c -b
    A4 <- -2*d*C[1] + C[2]*(c+b)
    A5 <- -2*a*C[2] + C[1]*(c+b)
    A6 <- d*C[1]^2 + a*C[2]^2 + C[1]*C[2]*(-c-b)

    f <- function(x, y) {
    1 / (2*pi*sqrt(det)) * exp(-0.5 * (A1*(x^2) + A2*(y^2) + A3*(x*y) + A4*(x) + A5*(y) + A6))
    }
    
    print("Limits:")
    print("------------\\")
    print(limits)
    print("------------//")

    X <- seq(limits[1, 1]-0.1, limits[1, 2]+0.1, 0.1)
    Y <- seq(limits[2, 1]-0.1, limits[2, 2]+0.1, 0.1)
    Z <- outer(X, Y, f)

    contour(X, Y, Z)
    print("------------------------------------------------//")
}

MX1 <- matrix(c(3, 0, 0, 3), nrow = 2, ncol = 2)
MX2 <- matrix(c(6, 0, 0, 2), nrow = 2, ncol = 2)
MX3 <- matrix(c(4, -4, 0, 4), nrow = 2, ncol = 2)

 svg('nocor.svg')
 LevelLines(MX1)
 dev.off()

 svg('eq.svg')
 LevelLines(MX2)
 dev.off()

 svg('cor.svg')
 LevelLines(MX3)
 dev.off()
