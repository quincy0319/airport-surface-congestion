x <- read.csv("dep_feb_per60.csv", header = F)
x01 <- x[, 1]
x18l <- x[, 2]
x18r <- x[, 3]
x19 <- x[, 4]
x36l <- x[, 5]
x36r <- x[, 6]
y <- read.csv("arr_feb_per60.csv", header = F)
y01 <- y[, 1]
y18l <- y[, 2]
y18r <- y[, 3]
y19 <- y[, 4]
y36l <- y[, 5]
y36r <- y[, 6]

opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))
smoothScatter(x01, y01, xlab = "dep_01_per60", ylab = "arr_01_per60")
smoothScatter(x36l, y36r, xlab = "dep_36l_per60", ylab = "arr_36l_per60")
smoothScatter(x36r, y36r, xlab = "dep_36r_per60", ylab = "arr_36r_per60")
smoothScatter(x19, y19, xlab = "dep_19_per60", ylab = "arr_19_per60")
smoothScatter(x18r, y18r, xlab = "dep_18r_per60", ylab = "arr_18l_per60")
smoothScatter(x18l, y18l, xlab = "dep_18l_per60", ylab = "arr_18l_per60")

