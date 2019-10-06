LOO = function (xl, class, sortingFunction, task, isLooIncrease, text) {
    n = dim(xl)[1];
    loo = rep(0, n - 1)
    for (i in 1 : n) {
        X = xl[-i, 1:3]
        u = xl[i, 1 : 2]
        for (k in 1 : (n - 1)) {
            test = task(X, u, k)
            if (isLooIncrease(test, class[i])) {
                loo[k] = loo[k] + 1;
            }
            print(loo[k])
        }
    }
    loo = loo / n
    x = 1:length(loo)
    y = loo
    plot(x, y,main = text, xlab="k", ylab="LOO", type = "l")

    min = which.min(loo)
    minLoo=round(loo[min],3)
    points(min, loo[min], pch = 21, col = "red",bg = "red")
    label = paste("   K = ", min, "\n", "   LOO = ", minLoo, sep = "")
    minX = 3*min;
    text(minX, minLoo, labels = label, pos = 3, col = "red")
    return(min);
}