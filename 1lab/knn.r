xPoint <- 3
yPoint <- 0

yourPoint <- c(xPoint, yPoint)

# 
euclideanDistance <- function(u, v) {
	sqrt( sum( (u - v)^2 ) )
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance) {
	l <- dim(xl)[1]
	n <- dim(xl)[2] - 1

	distances <- matrix(NA, l, 2)
	
	for (i in 1:l) {
		distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
	}

	orderedXl <- xl[order(distances[, 2]), ]
	return (orderedXl);
}


kNN <- function(xl, z, k) {

	orderedXl <- sortObjectsByDist(xl, z)
	n <- dim(orderedXl)[2] - 1

	classes <- orderedXl[1:k, n + 1]

	counts <- table(classes)

	class <- names(which.max(counts))
	return(class)
	
}

colors <- c(  "setosa" = "red", "versicolor"  = "green3", "virginica"   = "blue"  )
xl <- iris[, 3:5]

source('LOO.r')       

svg("LOO knn.svg")
loo <- LOO(xl, iris[, 5], sortObjectsByDist, kNN, function(a, b) {
	a != b
 }, "LOO for knn");
 dev.off()

svg("knn.svg")

plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], 
      col = colors[iris$Species], asp = 1)


class <- kNN(xl, yourPoint, k=6)

points(yourPoint[1], yourPoint[2], pch = 22, bg = colors[class], asp = 1)
dev.off()

svg("knn with loo.svg")
text <- paste("knn with k = ", loo)
plot(iris[, 3:4], main = text, pch = 21, bg = colors[iris$Species], 
      col = colors[iris$Species], asp = 1)


class <- kNN(xl, yourPoint, loo)

points(yourPoint[1], yourPoint[2], pch = 22, bg = colors[class], asp = 1)
dev.off()

svg("knn map.svg")
text <- paste("Map classificaton for knn with k = ", loo) 
plot(iris[, 3:4], main=text, pch = 21, bg = colors[xl$Species], col = colors[xl$Species], asp='1') 

for(i in seq(0, 7, 0.1)) {

    for(j in seq(0, 3, 0.08)) {
        mapPoint <- c(i, j) 
		orderedXl <- sortObjectsByDist(xl, mapPoint) 
		class <- kNN(xl, mapPoint,loo) 
		points(i, j, pch = 1, bg = colors[class],col=colors[class])
    }
}
dev.off()