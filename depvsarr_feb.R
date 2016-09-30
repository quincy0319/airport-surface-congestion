# put data into dataframe
dep_60 <- read.csv("dep_feb_per60.csv", header = F)
arr_60 <- read.csv("arr_feb_per60.csv", header = F)
dep_30 <- read.csv("dep_feb_per30.csv", header = F)
arr_30 <- read.csv("arr_feb_per30.csv", header = F)
dep_15 <- read.csv("dep_feb_per15.csv", header = F)
arr_15 <- read.csv("arr_feb_per15.csv", header = F)
names(dep_60) <- c("x01", "x18l", "x18r", "x19", "x36l", "x36r")
names(arr_60) <- c("y01", "y18l", "y18r", "y19", "y36l", "y36r")
names(dep_30) <- c("x01", "x18l", "x18r", "x19", "x36l", "x36r")
names(arr_30) <- c("y01", "y18l", "y18r", "y19", "y36l", "y36r")
names(dep_15) <- c("x01", "x18l", "x18r", "x19", "x36l", "x36r")
names(arr_15) <- c("y01", "y18l", "y18r", "y19", "y36l", "y36r")

# rwy configuration caculation
dep_60_matrix <- as.matrix(dep_60)
sum_dep <- rowSums(dep_60_matrix)
arr_60_matrix <- as.matrix(arr_60)
sum_arr <- rowSums(arr_60_matrix)
dep_data_summary <- colSums(dep_60_matrix) / sum(colSums(dep_60_matrix))
arr_data_summary <- colSums(arr_60_matrix) / sum(colSums(arr_60_matrix))
sum_per60 <- sum_dep + sum_arr
data_60 <- cbind(dep_60, sum_dep, arr_60, sum_arr, sum_per60)
attach(dep_60)
dep_prop_01 <- x01 / sum_per60
dep_prop_36l <- x36l / sum_per60
dep_prop_36r <- x36r / sum_per60
dep_prop_19 <- x19 / sum_per60
dep_prop_18r <- x18r / sum_per60
dep_prop_18l <- x18l / sum_per60
detach(dep_60)
attach(arr_60)
arr_prop_01 <- y01 / sum_per60
arr_prop_36l <- y36l / sum_per60
arr_prop_36r <- y36r / sum_per60
arr_prop_19 <- y19 / sum_per60
arr_prop_18r <- y18r / sum_per60
arr_prop_18l <- y18l / sum_per60
detach(arr_60)
data_60 <- cbind(data_60, dep_prop_01, dep_prop_36l, dep_prop_36r, dep_prop_19, 
                 dep_prop_18r, dep_prop_18l)
data_60 <- cbind(data_60, arr_prop_01, arr_prop_36l, arr_prop_36r, arr_prop_19, 
                 arr_prop_18r, arr_prop_18l)

dev.new()
rwy_36l_upper10 <- subset(data_60, ((x36l + y36l) > 10), select = c(5, 12))
smoothScatter(rwy_36l_upper10)
title("rwy_36l_upper10")
grid()
dev.new()
rwy_36r_upper10 <- subset(data_60, ((x36r + y36r) > 10), select = c(6, 13))
smoothScatter(rwy_36r_upper10)
title("rwy_36r_upper10")
grid()
dev.new()
rwy_01_upper10 <- subset(data_60, ((x01 + y01) > 10), select = c(1, 8))
smoothScatter(rwy_01_upper10)
title("rwy_01_upper10")
grid()
rwy_36l_cluster <- subset(data_60, ((x01 - 13) ^ 2 + (y01 - 13) ^ 2) <= 9,
                          select = c(1,8))


# scatter plot colored by smoothed densities
dev.new()
attach(dep_60)
attach(arr_60)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))
par(cex.axis = 1.2, cex.lab = 1.5)
smoothScatter(x01, y01, xlab = "dep_01_per60", ylab = "arr_01_per60")
title(main = "rwy_01", cex.main = 1.5)
smoothScatter(x36l, y36l, xlab = "dep_36l_per60", ylab = "arr_36l_per60")
title(main = "rwy_36l", cex.main = 1.5)
smoothScatter(x36r, y36r, xlab = "dep_36r_per60", ylab = "arr_36r_per60")
title(main = "rwy_36r", cex.main = 1.5)
smoothScatter(x19, y19, xlab = "dep_19_per60", ylab = "arr_19_per60")
title(main = "rwy_19", cex.main = 1.5)
smoothScatter(x18r, y18r, xlab = "dep_18r_per60", ylab = "arr_18r_per60")
title(main = "rwy_18r", cex.main = 1.5)
smoothScatter(x18l, y18l, xlab = "dep_18l_per60", ylab = "arr_18l_per60")
title(main = "rwy_18l", cex.main = 1.5)
par(opar)
detach(dep_60)
detach(arr_60)


dev.new()
attach(dep_30)
attach(arr_30)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))
par(cex.axis = 1.2, cex.lab = 1.5)
smoothScatter(x01, y01, xlab = "dep_01_per30", ylab = "arr_01_per30")
title(main = "rwy_01", cex.main = 1.5)
smoothScatter(x36l, y36l, xlab = "dep_36l_per30", ylab = "arr_36l_per30")
title(main = "rwy_36l", cex.main = 1.5)
smoothScatter(x36r, y36r, xlab = "dep_36r_per30", ylab = "arr_36r_per30")
title(main = "rwy_36r", cex.main = 1.5)
smoothScatter(x19, y19, xlab = "dep_19_per30", ylab = "arr_19_per30")
title(main = "rwy_19", cex.main = 1.5)
smoothScatter(x18r, y18r, xlab = "dep_18r_per30", ylab = "arr_18r_per30")
title(main = "rwy_18r", cex.main = 1.5)
smoothScatter(x18l, y18l, xlab = "dep_18l_per30", ylab = "arr_18l_per30")
title(main = "rwy_18l", cex.main = 1.5)
par(opar)
detach(dep_30)
detach(arr_30)

dev.new()
attach(dep_15)
attach(arr_15)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))
par(cex.axis = 1.2, cex.lab = 1.5)
smoothScatter(x01, y01, xlab = "dep_01_per15", ylab = "arr_01_per15")
title(main = "rwy_01", cex.main = 1.5)
smoothScatter(x36l, y36l, xlab = "dep_36l_per15", ylab = "arr_36l_per15")
title(main = "rwy_16l", cex.main = 1.5)
smoothScatter(x36r, y36r, xlab = "dep_36r_per15", ylab = "arr_36r_per15")
title(main = "rwy_36r", cex.main = 1.5)
smoothScatter(x19, y19, xlab = "dep_19_per15", ylab = "arr_19_per15")
title(main = "rwy_19", cex.main = 1.5)
smoothScatter(x18r, y18r, xlab = "dep_18r_per15", ylab = "arr_18r_per15")
title(main = "rwy_18r", cex.main = 1.5)
smoothScatter(x18l, y18l, xlab = "dep_18l_per15", ylab = "arr_18l_per15")
title(main = "rwy_18l", cex.main = 1.5)
par(opar)
detach(dep_15)
detach(arr_15)

# percentage of rwy usage
dev.new()
pie(dep_data_summary, radius = 1)
title("dep rwy usage")
dev.new()
pie(arr_data_summary, radius = 1)
title("arr rwy usage")

#wssplot function
wssplot <- function(data, nc = 10, seed = 1234){
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withiness)
  }
  plot(1:nc, wss, type = "b", xlab = "number of clusters",
       ylab = "within groups sum of squares")
}

# Kmeans-clusters
df <- rwy_36l_upper10
df <- scale(df[-1])
wssplot(df)
library(NbClust)
set.seed(1234)
devAskNewPage(ask = TRUE)
nc <- NbClust(df, min.nc = 2, max.nc = 10, method = "kmeans")
table(nc$Best.n[1, ])
barplot(table(nc$Best.n[1, ]), 
        xlab = "Number of Clusters", ylab = "Number of Criteria",
        main = "Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
fit.km <- kmeans(df, 3, nstart = 25)
fit.km$size
fit.km$centers
aggregate(df[-1], by = list(cluster = fit.km$cluster), mean)


# regression tree
library(rpart)
set.seed(1234)
dtree <- rpart(class ~ ., data = df, method = "class",
               parms = list(split = "information"))
dtree$cptable
plotcp(dtree)
dtree.pruned <- prune(dtree, cp = .0125)
library(rpart.plot)
prp(dtree.pruned, type = 2, extra = 104, 
    fallen.leaves = TRUE, main = "Decision Tree")
dtree.pred <- predict(dtree.pruned, df.validate, type = "class")
dtree.perf <- table(df.validate$class, dtree.pred,
                    dnn = c("Actual", "Predicted"))
dtree.perf

