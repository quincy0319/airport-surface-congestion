# data source 

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