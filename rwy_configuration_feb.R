# open origin data source
rwy_flow_per15 <- read.csv("rwy_flow_per15.csv")
rwy_flow_per60 <- read.csv("rwy_flow_per60.csv")
# smooth scatter pic per 60 minutes
# rwy 36l
dev.new()
rwy_36l_upper10 <- subset(rwy_flow_per60, ((dep36l + arr36l) > 10), select = c(1,4))
smoothScatter(rwy_36l_upper10)
title("rwy_36l_upper10")
grid()
a1 <- 4
a2 <- 18
b1 <- 15
b2 <- 11
c1 <- 6.3
c2 <- 27.4
d1 <- 17.3
d2 <- 20.4
lines(c(a1, b1), c(a2, b2), "l", lwd = 2)
lines(c(a1, c1), c(a2, c2), "l", lwd = 2)
lines(c(b1, d1), c(b2, d2), "l", lwd = 2)
lines(c(c1, d1), c(c2, d2), "l", lwd = 2)
# rwy 36l main configuration
rwy_36l_main <- subset(rwy_flow_per60, (arr36l >= 226 / 11 - (7 * dep36l) / 11)
                & (arr36l <= 691 / 22 -(7 * dep36l) / 11
                & (arr36l <= (94 * dep36l) / 23 + 38/23)
                & (arr36l >= (94 * dep36l) / 23 - 1157/23)),
                select = c(1, 4))

# rwy 36r
dev.new()
rwy_36r_upper10 <- subset(rwy_flow_per60, ((dep36r + arr36r) > 10), select = c(2, 5))
smoothScatter(rwy_36r_upper10)
title("rwy_36r_upper10")
grid()
a1 <- 28
a2 <- 0
b1 <- 37
b2 <- 0
c1 <- 11
c2 <- 15
d1 <- 20
d2 <- 15
lines(c(a1, b1), c(a2, b2), "l", lwd = 2)
lines(c(a1, c1), c(a2, c2), "l", lwd = 2)
lines(c(b1, d1), c(b2, d2), "l", lwd = 2)
lines(c(c1, d1), c(c2, d2), "l", lwd = 2)
# rwy 36r main configuration
rwy_36r_main <- subset(rwy_flow_per60, (arr36r >= 0)
                & (arr36r <= 555 / 17 -(15 * dep36r) / 17)
                & (arr36r <= 15)
                & (arr36r >= 420 / 17 - (15 * dep36r) / 17),
                select = c(2, 5))

# rwy 01
dev.new()
rwy_01_upper10 <- subset(rwy_flow_per60, ((dep01 + arr01) > 10), select = c(3, 6))
smoothScatter(rwy_01_upper10)
title("rwy_01_upper10")
grid()
a1 <- 5
a2 <- 22
b1 <- 17
b2 <- 13
c1 <- 7
c2 <- 27.6
d1 <- 19
d2 <- 18.6
lines(c(a1, b1), c(a2, b2), "l", lwd = 2)
lines(c(a1, c1), c(a2, c2), "l", lwd = 2)
lines(c(b1, d1), c(b2, d2), "l", lwd = 2)
lines(c(c1, d1), c(c2, d2), "l", lwd = 2)
# rwy 01 main configuration
rwy_01_main <- subset(rwy_flow_per60, (arr01 >= 103 / 4 - (3 * dep01) / 4)
                & (arr01 <= 657 / 20 -(3 * dep01) / 4
                & (arr01 <= (14 * dep01) / 5 + 8)
                & (arr01 >= (14 * dep01) / 5 - 173/5)),
                select = c(1, 8))

# main configuration summary
prob_36l_main <- nrow(rwy_36l_main) / nrow(rwy_36l_upper10)
prob_36r_main <- nrow(rwy_36r_main) / nrow(rwy_36r_upper10)
prob_01_main <- nrow(rwy_01_main) / nrow(rwy_01_upper10)
prob_36l_main
prob_36r_main
prob_01_main

################################################################################

# put data back into origin data
attach(rwy_flow_per60)
for (i in 1:672){ 
        rwy_36l_chosen_hour <- ifelse((arr36l >= 226 / 11 - (7 * dep36l) / 11)
                        & (arr36l <= 691 / 22 -(7 * dep36l) / 11
                        & (arr36l <= (94 * dep36l) / 23 + 38/23)
                        & (arr36l >= (94 * dep36l) / 23 - 1157/23)), 1, 0)
}
for (i in 1:672){ 
        rwy_36r_chosen_hour <- ifelse((arr36r >= 0)
                        & (arr36r <= 555 / 17 -(15 * dep36r) / 17)
                        & (arr36r <= 15)
                        & (arr36r >= 420 / 17 - (15 * dep36r) / 17), 1, 0)
}
for (i in 1:672){ 
        rwy_01_chosen_hour <- ifelse((arr01 >= 103 / 4 - (3 * dep01) / 4)
                        & (arr01 <= 657 / 20 -(3 * dep01) / 4
                        & (arr01 <= (14 * dep01) / 5 + 8)
                        & (arr01 >= (14 * dep01) / 5 - 173/5)), 1, 0)
}
detach(rwy_flow_per60)
data_main_configuration <- cbind(rwy_36l_chosen_hour, rwy_36r_chosen_hour, rwy_01_chosen_hour)

################################################################################










################################################################################
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

