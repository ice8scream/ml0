source('knn/LOO.r')       
source('helpF/Distanse.r', chdir = TRUE)      
xPoint <- 3
yPoint <- 0

yourPoint <- c(xPoint, yPoint)

colors <- c(  "setosa" = "red", "versicolor"  = "green3", "virginica"   = "blue"  )

euclideanDistance <- function(u, v) {
	sqrt(sum((u - v)^2))
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance) {
	l <- dim(xl)[1]
	n <- dim(xl)[2] - 1
	distances <- rep(0, 1)
	for (i in 1:l) {
		distances[i] <- c(metricFunction(xl[i, 1:n], z))
	}
	orderedXl <- xl[order(distances), ]
	return (orderedXl);
}

weight <- function(i, k) {
  return((k + 1 - i) / k)
}

kWNN <- function(xl, z, k) {
	orderedXl <- sortObjectsByDist(xl, z)
	n <- dim(orderedXl)[2] - 1
	weights <- rep(0,3)
  	names(weights) <- c("setosa", "versicolor", "virginica")
	classes <- orderedXl[1:k, n + 1]
	for(i in 1:k) {
		weights[classes[i]] <- weight(i,k) + weights[classes[i]];
	}
	class <- names(which.max(weights))
	return(class)
}

xl <- iris[, 3:5]

svg("LOO_kwnn.svg")
	loo <- LOO(xl, iris[, 5], sortObjectsByDist, kWNN, function(a, b) {
		return(a != b)
	}, "LOO for kwnn");
dev.off()

svg("kwnn_with_loo.svg")
	plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], 
		col = colors[iris$Species], asp = 1)
	class <- kWNN(xl, yourPoint, loo)
	points(yourPoint[1], yourPoint[2], pch = 22, bg = colors[class], asp = 1)
dev.off()

svg("kwnn_map.svg")
	text <- paste("Map classificaton for kwnn with k = ", loo) 
	plot(iris[, 3:4], main=text, pch = 21, bg = colors[xl$Species], col = colors[xl$Species], asp='1') 
	for(i in seq(0, 7, 0.1)) {
		for(j in seq(0, 3, 0.1)) {
			mapPoint <- c(i, j) 
			orderedXl <- sortObjectsByDist(xl, mapPoint) 
			class <- kNN(xl, mapPoint,loo) 
			points(i, j, pch = 1, bg = colors[class],col=colors[class])
		}
	}
dev.off()