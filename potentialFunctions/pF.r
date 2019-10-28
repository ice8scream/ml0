# source('potentialFunctions/pF.r')
# install.packages("plotrix")
library("plotrix")
# source('helpF/Distanse.r', chdir = TRUE)
Distanse <- function(u,v) sqrt(sum((u - v)^2))
source('parsen/parsen.r', chdir = TRUE)

potentialF <- function(x, z, g, kerF, h=c()) {
    m <- dim(x)[1]
    n <- dim(x)[2]-1
    
    if(sum(h) == 0) h <- c(rep(1, m/3), rep(0.25,(m - m / 3)))

    classes <- rep(0, length(names(table(x[,n+1]))))

    names(classes) <- names(table(x[,n+1]))
    for(i in 1:m) {
        y <- x[i, n+1]
        dist <- Distanse(x[i,1:n],z)
        w <- kerF(dist/h[i]) * g[i]
        classes[y] <- classes[y] + w
    }

    if(sum(classes) > 0) {
        class <- names(which.max(classes))
    } else {
        class <- "unknown"
    }
    return(class)
}

pError <- function(x,g,kerF, h) {
    error <- 0
    m <- dim(x)[1]
    n <- dim(x)[2]-1
    for(i in 1:m) {
        class1 <- potentialF(x,x[i,1:n],g,kerF,h)
        class2 <- x[i,n+1]
        if(class1 != class2) {
            error <- error + 1
        }
    }
    return(error)
}

Gamma <- function(x, kerF=GausKer, h=c(), delta = 10) {
    m <- dim(x)[1]
    n <- dim(x)[2]-1
    
    if(sum(h) == 0) h <- c(rep(1, m/3), rep(0.25,(m - m / 3)))

    g <- rep(0,m)
    i <- 1
    while(pError(x,g,kerF,h) > delta) {
        class1 <- potentialF(x,x[i,1:n],g,kerF,h)
        class2 <- x[i,n+1]
        if(class1 != class2) g[i] <- g[i] + 1
        i <- ((40 + sample(1:110, 1)[1])%%m) + 1
    }
    return(g)
}

# svg('PF.svg')

# plot(iris[, 3:4], main=paste("Potential Functions\nwith Gauss kern"), pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp='1') 

m <- dim(iris)[1]
gamma <- Gamma(iris[,3:5])
maxG = max(gamma)
opacity <- gamma / maxG
h <- c(rep(1, m/3), rep(0.5, (m-m/3)))

# for(i in 1:m) {
#     if(gamma[i] > 0) {
#         color = adjustcolor(colors[iris[i,5]], opacity[i] / 2)
#         draw.circle(iris[i,3], iris[i,4], opacity[i], 40, border = color, col = color)
#         points(iris[i,3],iris[i,4],col="black",bg=color,pch=21)
#         points(iris[i,3],iris[i,4],col="white",pch=13)
#     } else {
#         h <- c(rep(1, m/3), rep(0.5, (m-m/3)))
#         if(iris[i,5] != potentialF(iris[,3:5],iris[i,3:4],gamma,GausKer,h)) {
#             points(iris[i,3],iris[i,4],col="black",pch=22)
#         }
#     }
# }
# dev.off()

svg('PFMapG.svg')
plot(iris[, 3:4], main=paste("Potential Functions classification Map\nwith Gauss kern"), pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp='1') 

 for (i in seq(0,7,0.1)) {
    for (j in seq(0,2.5,0.1)){
        color <- potentialF(iris[,3:5],c(i,j),gamma,GausKer,h)
        if(color != "unknown") {
            points(i,j,pch=21, col = colors[color])
        }
    }
  }

for (i in 1:m) {
        if(iris[i,5] != potentialF(iris[,3:5],iris[i,3:4],gamma,GausKer,h)){
             points(iris[i,3],iris[i,4],col="black",pch=22)
        }
    }
dev.off()