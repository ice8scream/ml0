# Системы и методы принятия решений
---

## Метрические алгоритмы классификации

>---
>## kNN
>
>Алгоритм ***kNN*** и ***kWNN*** - метрические алгоритмы классификации, основаный на оценке сходства объектов. Относит объект к тому классу, элементов которого больше среди ***k*** ближайших.
>
>Формула алгоритма **kNN** (*k* ближайших соседей) выглядит: ![knnF](imgs/knn.png)
>
>Для выбора оптимального k используем метод скользащего контроля (LOO). Применив kNN и LOO к выборке Ирисы Фишера получим результат:
>
>|График LOO(k) | Карта классификации kNN|
>|:------------:|:----------------------:|
>|![kNN](knn/LOO_knn.svg)|![kNN](knn/knn_map.svg)|
>
>Видно, что лучишй результат получаем при k = 6, с оценкой ошибки равной 0.33.
>
>kNN — один из простейших алгоритмов классификации, поэтому на реальных задачах он зачастую оказывается неэффективным. Помимо точности классификации, проблемой этого классификатора является скорость классификации: если в обучающей выборке N объектов, в тестовой выборе M объектов, а размерность пространства — K, то количество операций для классификации тестовой выборки может быть оценено как O(K\*M\*N).
>
>---
>
>#### Код алгоритма kNN:
>
>```r
># Алгоритм kNN
>kNN <- function(xl, z, k) {
>	orderedXl <- sortObjectsByDist(xl, z)
>	n <- dim(orderedXl)[2] - 1
>	classes <- orderedXl[1:k, n + 1]
>	counts <- table(classes)
>	class <- names(which.max(counts))
>	return(class)
>}
>```
>
>---
>
>#### Классификация объекта (3,0)
>
>![kNN](knn/knn_with_loo.svg)
>
>---

>---
>## kWNN
>Формула алгоритма ***kWNN*** (*k* взвешаных ближайших соседей) выглядит: ![kwnnF](imgs/kwnn1.png)
>Возьмем за вес ![kwnnF](imgs/kwnn2.png) и переберем по LOO
>
>
>**Карта классификации для kWNN:**
>
>|График LOO(k,q) | Карта классификации kWNN|
>|:------------:|:----------------------:|
>|![kNN](knn/LOO_kwnn.svg)|![kNN](knn/kwnn_map.svg)|
>
>Видим что лучший результат при **k = 4**. Равен **0.04**
>
>Зачем использовать ***kwNN*** если там больше расчетов? В задачах с числом классов **3** и более нечётность уже не помогает и сутации неодназначности могут возниктаь. Тогда на помошь приходят веса, и объект классифицируется к тому классу, чей суммарны вес больше среди ***k*** соседий.
>
>---
>
>#### Код алгоритма kNN:
>
>```r
># Алгоритм kNN
>kWNN <- function(xl, z, k) {
>	orderedXl <- sortObjectsByDist(xl, z)
>	n <- dim(orderedXl)[2] - 1
>	weights <- rep(0,3)
>  	names(weights) <- c("setosa", "versicolor", "virginica")
>	classes <- orderedXl[1:k, n + 1]
>	for(i in 1:k) {
>		weights[classes[i]] <- weight(i,k) + weights[classes[i]];
>	}
>	class <- names(which.max(weights))
>	return(class)
>}
>```
>
>---
>
>#### Классификация объекта (3,0)
>
>![kNN](knn/kwnn_with_loo.svg)
>
>---
> ## Преимущество kwnn над knn
>Предположим, что есть два класса объектов с разной плотностью распределения этих объектов. Тогда алгоритм **kNN** будет классифицировать объекты,  находящиеся на границе двух классов, к тому классу, чьи объекты расположены плотнее. Для более равномерной классификации объектов между двумя классами можно воспользоваться алгоритмом **kWNN**.
>
>Классификация объекта ***A*** **[2, 0,4]** при **k=7**:
>
>|kNN | kWNN|
>|:------------:|:----------------------:|
>|![kNN](knn/knn_diff.png)|![kNN](knn/kwnn_diff.png)|
>
>Как видим **kNN** в таком случае неправильно классифицирует объект ***A*** в отличе от **kWNN**.
>
>---

>---
>## Метод парзеновского окна
>
>**Метод парзеновского окна** — метод классификации, основанный на непараметрическом восстановлении плотности по имеющейся выборке. В основе подхода лежит идея о том, что плотность выше в тех точках, рядом с которыми находится большое количество объектов выборки. В данном алгоритме выбирается следующий способ задать веса соседям: определить ![parsenF](imgs/parsenwi.png), а не от ранга соседа. 
>
>> Введём функцию ядра ![parsenF](imgs/parsen1.png) - весовую функцию следующим образом: ![parsenF](imgs/parsenwui.png) = ![parsenF](imgs/parsenkf.png), 
>>где ***h***-ширина окна
>> Подберем ***h*** по оценке скользящего контроля LOO
>
> Реализация алгоритма на языке **R**:
>
> 
> ```
>parsen <- function(x, z, h, kerF) {
>    m <- dim(x)[1]
>    n <- dim(x)[2]-1
>    classes <- rep(0, length(names(table(x[,n+1]))))
>    names(classes) <- names(table(x[,n+1]))
>    for(i in 1:m){
>        y <- x[i, n+1]
>        dist <- Distanse(x[i,1:n],z)
>        w <- kerF(dist/h)
>        classes[y] <- classes[y] + w
>    }
>    if(sum(classes) > 0) {
>        class <- names(which.max(classes))
>    } else {
>        class <- "unknown"
>    }
>    return(class)
>}
> ```
> ---
>#### Прямоугольное ядро
>
> Формула: ![parsenF](imgs/RectKer.png)
>
> Функция ядра на языке **R**:
>
>|``` RectKer <- function(r) (abs(r) <= 1) * 0.5 ```|
>|:-:|
>
> Оптимальная ширина окна ***h = 0.35***, оценка ***LOO = 0.04***
> 
>
>|![parsen](parsen/RectKerLOO.svg)|![parsen](parsen/RectKerMap.svg)|
>|:------------------------------:|:------------------------------:|
>
> ---
>#### Треугольное ядро
>
> Формула: ![parsenF](imgs/TrianKer.png)
>
> Функция ядра на языке **R**:
>
>|``` TriaKer <- function(r) (abs(r) <= 1) * (1 - abs(r)) ```|
>|:-:|
>
> Оптимальная ширина окна ***h = 0.35***, оценка ***LOO = 0.04***
> 
>
>|![parsen](parsen/TriaKerLOO.svg)|![parsen](parsen/TriaKerMap.svg)|
>|:------------------------------:|:------------------------------:|
>
> ---
>#### Квартическое  ядро
>
> Формула: ![parsenF](imgs/QuarKer.png)
>
> Функция ядра на языке **R**:
>
>|``` QuarKer <- function(r) (abs(r) <= 1) * (1 - r^2)^2 ```|
>|:-:|
>
> Оптимальная ширина окна ***h = 0.35***, оценка ***LOO = 0.04***
> 
>
>|![parsen](parsen/QuarKerLOO.svg)|![parsen](parsen/QuarKerMap.svg)|
>|:------------------------------:|:------------------------------:|
>
> ---
>#### Ядро Епанечникова
>
> Формула: ![parsenF](imgs/EpanKer.png)
>
> Функция ядра на языке **R**:
>
>|``` EpanKer <- function(r) (abs(r) <= 1) * (1 - r^2) ```|
>|:-:|
>
> Оптимальная ширина окна ***h = 0.35***, оценка ***LOO = 0.04***
> 
>
>|![parsen](parsen/EpanKerLOO.svg)|![parsen](parsen/EpanKerMap.svg)|
>|:------------------------------:|:------------------------------:|
>
> ---
>#### Гауссовское ядро
>
> Формула: ![parsenF](imgs/GausKer.png)
>
> Функция ядра на языке **R**:
>
>|``` GausKer <- function(r) dnorm(r) ```|
>|:-:|
>
> Оптимальная ширина окна ***h = 0.1***, оценка ***LOO = 0.04***
> 
>
>|![parsen](parsen/GausKerLOO.svg)|![parsen](parsen/GausKerMap.svg)|
>|:------------------------------:|:------------------------------:|
>
>---
> **Плюсом** данного метода является хорошее качество классификации объекта при правильно выбраном ***h***, алгоритм прост в реализации и его сложность равна O(N)
> **Минусом** является малый набор настраиваемх параметров, что характеризуется не гибкостью алгоритма. Параметры ширины тебуется подбирать под конкретную обучающую выборку и хранить эту выборку целиком. Так же для всех ядер есть вероятность, что объект не будет классифицирован, так как не попадет в окно. Исключением является Гауссовское ядро.
>
>---

>---
> ## Метод потенциальных функций
>
> Для оценки близости объекта ***u*** к классу y алгоритм использует функцию:
>![potentialF](imgs/PotentialF.png)
> В реализуемом методе используется фиксированная ширина окна. Для первых **50** объектов ***(class=setosa) h=1***, так как объекты данного класса достаточно удалены от объектов других. Для остальных объектов ***h=100***.
> Изачально потенциалы заполняются нулями. Далее, пока количество ошибок классификации не достигнет нужного предела, выбираем случайно точку ***x*** из выборки. Если для нее классификация выполняется неверно, увеличиваем потенциал на **1** и пересчитываем общее количество ошибок.
>
> Объекты, потенциалы которых являются ***ненулевыми*** помечены ![img](imgs/NNP.png), объекты, на которых алгоритм ***ошибается*** ![img](imgs/Err.png). Радиус круга - сила потенциала.
>
>---
>### Прямоугольное ядро
>
>|Графическое изображение|Карта классификаций|
>|:-:|:-:|
>|![pF](potentialFunctions/PFR.png)|![pF](potentialFunctions/PFMapR.svg)|
>
>---
>### Треугольное ядро
>
>|Графическое изображение|Карта классификаций|
>|:-:|:-:|
>|![pF](potentialFunctions/PFT.png)|![pF](potentialFunctions/PFMapT.svg)|
>
>---
>### Квартическое ядро
>
>|Графическое изображение|Карта классификаций|
>|:-:|:-:|
>|![pF](potentialFunctions/PFQ.png)|![pF](potentialFunctions/PFMapQ.svg)|
>
>---
>### Ядро Епанечникова
>
>|Графическое изображение|Карта классификаций|
>|:-:|:-:|
>|![pF](potentialFunctions/PFE.png)|![pF](potentialFunctions/PFMapE.svg)|
>
>---
>### Ядро Гаусса
>
>|Графическое изображение|Карта классификаций|
>|:-:|:-:|
>|![pF](potentialFunctions/PFG3.png)|![pF](potentialFunctions/PFMapG3.png)|
>
>---
> **Плюсом** данного метода является богатый набор из 2l параметров, с помощью которых можно настроить алгоритм для классификации с высокой точностью.
>**Минусом** является сложность реализации, а также долгий процесс подбора параметров.
>Как итог мы можем классифицировать объекты с максимальной возможной точностью, которую мы можем задать вручную при подборе параметров.
>
>---

---
## Линии уровня
 
Вероятностное распределение с плотностью ![LL](LevelLines/f1.png)
называется n-мерным многомерным нормальным распределением  с математическим ожиданием ![LL](LevelLines/f2.png), и ковариационной матрицей  ![LL](LevelLines/f3.png). Предполагается, что матрица симметричная, невырожденная, положительно определенная

$N(x;\mu,\sum) = \dfrac{1}{\sqrt{(2\pi)^n|\sum|}}exp(-\dfrac{1}{2}(x-\mu)^t\sum^{-1}(x-\mu))$ $\mu \in R^n$$\sum \in R^{n\times n}$

---
 ### Листинг программы на языке R
```R
LevelLines <- function(mx, C = c(0,0), limits = matrix(c(-5, -5, 5, 5), nrow = 2, ncol = 2) {
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
```
---
## Ниже представлены графики лийний плотности в зависимости от признаков
|1. Одинаковая Дисперсия | 2. Признаки некоррелированы | 3. Признаки Коррелированы|
|:-:|:-:|:-:|
|![LL](LevelLines/nocor.svg)|![LL](LevelLines/eq.svg)|![LL](LevelLines/cor.svg)|

---

---
## Наивный байесовский классификатор

Пусть имеем набор некоторых объектов, имеющих **n** числовых признаков **f<sub>i</sub>**. Будем считать, что признаки независимы друг от друга. Тогда функция правдоподобия классов представима в виде: <sub><sub><a href="https://www.codecogs.com/eqnedit.php?latex=$P_y(x)=p_{y_1}(f_1)\dots&space;p_{y_n}(f_n),&space;y\in&space;Y$" target="_blank"><img src="https://latex.codecogs.com/gif.latex?$P_y(x)=p_{y_1}(f_1)\dots&space;p_{y_n}(f_n),&space;y\in&space;Y$" title="$P_y(x)=p_{y_1}(f_1)\dots p_{y_n}(f_n), y\in Y$" /></a></sub></sub>, где <sub><a href="https://www.codecogs.com/eqnedit.php?latex=$p_{y_i}(f_i)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?$p_{y_i}(f_i)" title="$p_{y_i}(f_i)" /></a></sub> - плотность распределения значений **i**-го признака. Подставив функцию правдоподобия классов в оптимальный байесовский классификатор получим: <sub><sub><sub><sub><sub><a href="https://www.codecogs.com/eqnedit.php?latex=$a(x)=&space;\arg\max\limits_{y\in&space;Y}(\lambda_y\rho_y&space;\prod\limits_{i=1}^{n}&space;P_{y_i}(f_i))" target="_blank"><img src="https://latex.codecogs.com/gif.latex?$a(x)=&space;\arg\max\limits_{y\in&space;Y}(\lambda_y\rho_y&space;\prod\limits_{i=1}^{n}&space;P_{y_i}(f_i))" title="$a(x)= \arg\max\limits_{y\in Y}(\lambda_y\rho_y \prod\limits_{i=1}^{n} P_{y_i}(f_i))" /></a></sub></sub></sub></sub></sub> - наивный нормальный байесовский классификатор.
Наивный из-за того, что предположение о независимости признаков является наивным.

Вычислим плотность с помощью формулы гауссовской плотности: 
<sub><sub><sub><sub><sub><sub><sub><sub><sub>![nb](naivnibaies/GausP.png)</sub></sub></sub></sub></sub></sub></sub></sub></sub> , где <sub><a href="https://www.codecogs.com/eqnedit.php?latex=$\mu_{y_i}$" target="_blank"><img src="https://latex.codecogs.com/png.latex?$\mu_{y_i}$" title="$\mu_{y_i}$" /></a></sub> - мат.ожидание **i**-того признака класса **y**.

Прологарифмируем максимизируемое выражение, в таком случае классификатор приобретает вид: <a href="https://www.codecogs.com/eqnedit.php?latex=$a(x)=\arg\max\limits_{y\in&space;Y}(\ln(\lambda_y\rho_y)&space;&plus;&space;\sum\limits_{i=1}^{n}\ln&space;P_{y_i}(f_i))$" target="_blank"><img src="https://latex.codecogs.com/png.latex?$a(x)=\arg\max\limits_{y\in&space;Y}(\ln(\lambda_y\rho_y)&space;&plus;&space;\sum\limits_{i=1}^{n}\ln&space;P_{y_i}(f_i))$" title="$a(x)=\arg\max\limits_{y\in Y}(\ln(\lambda_y\rho_y) + \sum\limits_{i=1}^{n}\ln P_{y_i}(f_i))$" /></a>

---
## Листинг программы на языке R
```R
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

```

---
## График  классификатора
<div align="center">
<img src="naivnibaies/plot.png">
</div>

---