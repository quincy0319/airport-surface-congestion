# taxi time caculation and analysis

dep_36l_feb_finished <- read.csv("dep_36l_feb_finished_per60.csv")
dep_36l_ols <- subset(dep_36l_feb_finished, dep_36l_feb_finished$dep_36l_in_cluster == 1,
                      select = c(15, 21, 22))
y <- as.numeric(dep_36l_ols$dep_taxi)
x1 <- dep_36l_ols$dep_36l_per_dep
x2 <- dep_36l_ols$arr_36l_per_dep
fit_36l_dep <- lm (y ~ x1)
summary(fit_36l_dep)

dep_36r_feb_finished <- read.csv("dep_36r_feb_finished_per60.csv")
dep_36r_ols <- subset(dep_36r_feb_finished, dep_36r_feb_finished$dep_36r_in_cluster == 1,
                      select = c(15, 21, 22))
y <- as.numeric(dep_36r_ols$dep_taxi)
x1 <- dep_36r_ols$dep_36r_per_dep
x2 <- dep_36r_ols$arr_36r_per_dep
fit_36r_dep <- lm (y ~ x1 + x2)
summary(fit_36r_dep)

dep_01_feb_finished <- read.csv("dep_01_feb_finished_per60.csv")
dep_01_ols <- subset(dep_01_feb_finished, dep_01_feb_finished$dep_01_in_cluster == 1,
                     select = c(15, 21, 22))
y <- as.numeric(dep_01_ols$dep_taxi)
x1 <- dep_01_ols$dep_01_per_dep
x2 <- dep_01_ols$arr_01_per_dep
fit_01_dep <- lm(y ~ x1 + x2)
summary(fit_01_dep)




dep_36l_feb_finished <- read.csv("dep_36l_feb_finished_per15.csv")
y <- as.numeric(dep_36l_feb_finished$dep_taxi)
x1 <- as.numeric(dep_36l_feb_finished$dep_36l_per_dep)
x2 <- as.numeric(dep_36l_feb_finished$arr_36l_per_dep)
fit_36l_dep <- lm (y ~ x1 )
summary(fit_36l_dep)

