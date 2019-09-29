xPoint <- 3
yPoint <- 0

yourPoint <- c(xPoint, yPoint)

euclideanDistance <- function(u, v) {
	sqrt( sum( (u - v)^2 ) )
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

kNN <- function(xl, z, k) {

	orderedXl <- sortObjectsByDist(xl, z)
	n <- dim(orderedXl)[2] - 1

	weights <- rep(0,3)
	
  	names(weights) <- c("setosa", "versicolor", "virginica")
	classes <- orderedXl[1:k, n + 1]
	
	for(i in 1:k) {
		#print(classes[i])
		# print(weight(i,k))
		weights[classes[i]] <- weight(i,k) + weights[classes[i]];
	}
	# print(weights)
	# print(weights)
	class <- names(which.max(weights))
	# print(class)
	return(class)
	# source('kwnn.r')
}

colors <- c(  "setosa" = "red", "versicolor"  = "green3", "virginica"   = "blue"  )
xl <- iris[, 3:5]

source('LOO.r')       

svg("LOO kwnn.svg")
loo <- LOO(xl, iris[, 5], sortObjectsByDist, kNN, function(a, b) {
	return(a != b)
 }, "LOO for kwnn");
 dev.off()

svg("kwnn with loo.svg")

plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], 
      col = colors[iris$Species], asp = 1)


class <- kNN(xl, yourPoint, loo)

points(yourPoint[1], yourPoint[2], pch = 22, bg = colors[class], asp = 1)
dev.off()

svg("kwnn map.svg")
text <- paste("Map classificaton for kwnn with k = ", loo) 
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