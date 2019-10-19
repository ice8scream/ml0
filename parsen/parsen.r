# source('parsen/parsen.r')
# Функция расстояния
Distanse <- function(u,v) sqrt(sum((u - v)^2))

# Функции ядер
RectKer <- function(r) (abs(r) <= 1) * 0.5
TriaKer <- function(r) (abs(r) <= 1) * (1 - abs(r))
QuarKer <- function(r) (abs(r) <= 1) * (1 - r^2)^2
EpanKer <- function(r) (abs(r) <= 1) * (1 - r^2)
GausKer <- function(r) dnorm(r)

# Функция парзеновского окна
parsen <- function(x, z, h, kerF) {
    m <- dim(x)[1]
    n <- dim(x)[2]-1
    classes <- rep(0, length(names(table(x[,n+1]))))
    names(classes) <- names(table(x[,n+1]))
    for(i in 1:m){
        y <- x[i, n+1]
        dist <- Distanse(x[i,1:n],z)
        w <- kerF(dist/h)
        classes[y] <- classes[y] + w
    }
    if(sum(classes) > 0) {
        class <- names(which.max(classes))
    } else {
        class <- "unknown"
    }
    return(class)
}

# Leve One Out для парзеновского окна
LOO <- function(x, kerF, kerN="") {
    m <- dim(x)[1]
    n <- dim(x)[2] - 1
    params <- seq(0.1,2,0.05)
    mark <- rep(0,length(params))
    for(h in 1:length(params)) {
        for(i in 1:m) {
            x1 <- x[-i,]
            class1 <- parsen(x1,x[i,1:n], params[h], kerF)
            class2 <- x[i,n+1]
            if(class1 != class2)  mark[h] <- mark[h] + 1/m
        }
    }
    plot(params, mark, type="l",xlab="Ширина окна", ylab="Оценка", main=paste("LOO для парзеновского окна c ", kerN))
    minPoint <- c(params[which.min(mark)], round(min(mark),4))
    text <- paste("h = ",minPoint[1],"\nLOO = ",minPoint[2])
    points(minPoint[1], minPoint[2], pch=19, col="firebrick1", bg="black")
    text(minPoint[1] + 0.1,minPoint[2] + 0.1,labels=text, col="firebrick1")
}

xl <- iris[, 3:5]
colors <- c( "setosa"="red", "versicolor"="green3", "virginica"="blue" )
FindParsen <- function(z1, z2, h, kerF) {
    points(z1,z2, pch = 21, col=colors[parsen(xl,c(z1,z2),h,kerF)])
}

PlotMapParsen <- function(h = 0.1, kerF = GausKer, label="Гаусовским ядром") {
    plot(iris[, 3:4], main=paste("Карта классификации\nдля парзеновского окна c ", label), pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp='1') 
    for (i in seq(0,7,0.1)) {
        for (j in seq(0,3,0.1)){
         FindParsen(i,j,h,kerF)
        }
    }
}

# svg('RectKerLOO.svg')
#     LOO(iris[,3:5],RectKer,"прямоугольным ядром");
# dev.off()
# svg('TriaKerLOO.svg')
#     LOO(iris[,3:5],TriaKer,"треугольным ядром");
# dev.off()
# svg('QuarKerLOO.svg')
#     LOO(iris[,3:5],QuarKer,"квартическим ядром");
# dev.off()
# svg('EpanKerLOO.svg')
#     LOO(iris[,3:5],EpanKer,"ядром Епанечникова");
# dev.off()
# svg('GausKerLOO.svg'))
#     LOO(iris[,3:5],GausKer,"Гаусовским ядром");
# dev.off()

# svg('RectKerMap.svg')
#     PlotMapParsen(h = 0.35, kerF = RectKer, label="прямоугольным ядром")
# dev.off()
# svg('TriaKerMap.svg')
#     PlotMapParsen(h = 0.35, kerF = RectKer, label="треугольным ядром")
# dev.off()
# svg('QuarKerMap.svg')
#     PlotMapParsen(h = 0.35, kerF = RectKer, label="квартическим ядром")
# dev.off()
# svg('EpanKerMap.svg')
#     PlotMapParsen(h = 0.35, kerF = RectKer, label="ядром Епанечникова")
# dev.off()
# svg('GausKerMap.svg')
#     PlotMapParsen()
# dev.off()