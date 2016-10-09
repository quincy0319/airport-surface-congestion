# data source 60min
dep_origin <- na.omit(read.csv("dep_average_taxi.csv"))
arr_origin <- na.omit(read.csv("arr_average_taxi.csv"))
dep_window <- read.csv("dep_window.csv")
arr_window <- read.csv("arr_window.csv")

dep_origin <- cbind(dep_origin, dep_window)
arr_origin <- cbind(arr_origin, arr_window)

dep_count_per15 <- read.csv("dep_count_per15.csv", header = F)
arr_count_per15 <- read.csv("arr_count_per15.csv", header = F)
################################################################################

# single rwy data
dep_36l_origin <- subset(dep_origin, rwy == "36L")
dep_36l_origin_feb <- subset(dep_36l_origin, as.numeric(mission_date) <= 20140230)
dep_36r_origin <- subset(dep_origin, rwy == "36R")
dep_36r_origin_feb <- subset(dep_36r_origin, as.numeric(mission_date) <= 20140230)
dep_01_origin <- subset(dep_origin, rwy == "1")
dep_01_origin_feb <- subset(dep_01_origin, as.numeric(mission_date) <= 20140230)


arr_36l_origin <- subset(arr_origin, rwy == "36L")
arr_36l_origin_feb <- subset(arr_36l_origin, as.numeric(mission_date) <= 20140230)
arr_36r_origin <- subset(arr_origin, rwy == "36R")
arr_36r_origin_feb <- subset(arr_36r_origin, as.numeric(mission_date) <= 20140230)
arr_01_origin <- subset(arr_origin, rwy == "1")
arr_01_origin_feb <- subset(arr_01_origin, as.numeric(mission_date) <= 20140230)

# put data into dataframe
# each rwy usage data
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


################################################################################

# put data back into origin data
attach(data_60)
for (i in 1:672){ 
        rwy_36l_chosen_hour <- ifelse((y36l >= 226 / 11 - (7 * x36l) / 11)
                                & (y36l <= 691 / 22 -(7 * x36l) / 11
                                & (y36l <= (94 * x36l) / 23 + 38/23)
                                & (y36l >= (94 * x36l) / 23 - 1157/23)), 1, 0)
}
for (i in 1:672){ 
        rwy_36r_chosen_hour <- ifelse((y36r >= 0)
                                & (y36r <= 555 / 17 -(15 * x36r) / 17)
                                & (y36r <= 15)
                                & (y36r >= 420 / 17 - (15 * x36r) / 17), 1, 0)
}
for (i in 1:672){ 
        rwy_01_chosen_hour <- ifelse((y01 >= 103 / 4 - (3 * x01) / 4)
                                & (y01 <= 657 / 20 -(3 * x01) / 4
                                & (y01 <= (14 * x01) / 5 + 8)
                                & (y01 >= (14 * x01) / 5 - 173/5)), 1, 0)
}
detach(data_60)
data_main_configuration <- cbind(rwy_36l_chosen_hour, rwy_36r_chosen_hour,
                                 rwy_01_chosen_hour)

################################################################################
# 60 minutes interval
# dep 36l
attach(data_60)
dep_count_36l <- x36l
arr_count_36l <- y36l
detach(data_60)
dep_36l_in_cluster <- vector(mode = "numeric", length = 0)
dep_36l_per_dep <- vector(mode = "numeric", length = 0)
arr_36l_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(dep_36l_origin_feb)){
        a <- dep_window[i, 2]
        dep_36l_in_cluster[i] <- rwy_36l_chosen_hour[a]
        dep_36l_per_dep[i] <- dep_count_36l[a]
        arr_36l_per_dep[i] <- arr_count_36l[a]
}
dep_36l_feb_finished <- cbind(dep_36l_origin_feb, dep_36l_in_cluster,
                              dep_36l_per_dep, arr_36l_per_dep)


# dep 36r
attach(data_60)
dep_count_36r <- x36r
arr_count_36r <- y36r
detach(data_60)
dep_36r_in_cluster <- vector(mode = "numeric", length = 0)
dep_36r_per_dep <- vector(mode = "numeric", length = 0)
arr_36r_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(dep_36r_origin_feb)){
        a <- dep_window[i, 2]
        dep_36r_in_cluster[i] <- rwy_36r_chosen_hour[a]
        dep_36r_per_dep[i] <- dep_count_36r[a]
        arr_36r_per_dep[i] <- arr_count_36r[a]
}
dep_36r_feb_finished <- cbind(dep_36r_origin_feb, dep_36r_in_cluster,
                              dep_36r_per_dep, arr_36r_per_dep)

# dep 01
attach(data_60)
dep_count_01 <- x01
arr_count_01 <- y01
detach(data_60)
dep_01_in_cluster <- vector(mode = "numeric", length = 0)
dep_01_per_dep <- vector(mode = "numeric", length = 0)
arr_01_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(dep_01_origin_feb)){
        a <- dep_window[i, 2]
        dep_01_in_cluster[i] <- rwy_01_chosen_hour[a]
        dep_01_per_dep[i] <- dep_count_01[a]
        arr_01_per_dep[i] <- arr_count_01[a]
}
dep_01_feb_finished <- cbind(dep_01_origin_feb, dep_01_in_cluster,
                             dep_01_per_dep, arr_01_per_dep)

################################################################################
# 60 minutes interval
# arr 36l
attach(data_60)
dep_count_36l <- x36l
arr_count_36l <- y36l
detach(data_60)
arr_36l_in_cluster <- vector(mode = "numeric", length = 0)
dep_36l_per_arr <- vector(mode = "numeric", length = 0)
arr_36l_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(arr_36l_origin_feb)){
        a <- arr_window[i, 2]
        arr_36l_in_cluster[i] <- rwy_36l_chosen_hour[a]
        arr_36l_per_arr[i] <- arr_count_36l[a]
        dep_36l_per_arr[i] <- dep_count_36l[a]
}
arr_36l_feb_finished <- cbind(arr_36l_origin_feb, arr_36l_in_cluster,
                              dep_36l_per_arr, arr_36l_per_arr)

# arr 36r
attach(data_60)
dep_count_36l <- x36l
arr_count_36r <- y36r
detach(data_60)
arr_36r_in_cluster <- vector(mode = "numeric", length = 0)
dep_36r_per_arr <- vector(mode = "numeric", length = 0)
arr_36r_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(arr_36r_origin_feb)){
        a <- arr_window[i, 2]
        arr_36r_in_cluster[i] <- rwy_36r_chosen_hour[a]
        arr_36r_per_arr[i] <- arr_count_36r[a]
        dep_36r_per_arr[i] <- dep_count_36r[a]
}
arr_36r_feb_finished <- cbind(arr_36r_origin_feb, arr_36r_in_cluster,
                              dep_36r_per_arr, arr_36r_per_arr)

# arr 01
attach(data_60)
dep_count_01 <- x01
arr_count_01 <- y01
detach(data_60)
arr_01_in_cluster <- vector(mode = "numeric", length = 0)
dep_01_per_arr <- vector(mode = "numeric", length = 0)
arr_01_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(arr_01_origin_feb)){
        a <- arr_window[i, 2]
        arr_01_in_cluster[i] <- rwy_01_chosen_hour[a]
        arr_01_per_arr[i] <- arr_count_01[a]
        dep_01_per_arr[i] <- dep_count_01[a]
}
arr_01_feb_finished <- cbind(arr_01_origin_feb, arr_01_in_cluster,
                             dep_01_per_arr, arr_01_per_arr)

################################################################################

# output
write.csv(dep_36l_feb_finished, "dep_36l_feb_finished_per60.csv")
write.csv(dep_36r_feb_finished, "dep_36r_feb_finished_per60.csv")
write.csv(dep_01_feb_finished, "dep_01_feb_finished_per60.csv")

write.csv(arr_36l_feb_finished, "arr_36l_feb_finished_per60.csv")
write.csv(arr_36r_feb_finished, "arr_36r_feb_finished_per60.csv")
write.csv(arr_01_feb_finished, "arr_01_feb_finished_per60.csv")

################################################################################
################################################################################

# 15 minutes interval
dep_window_per15 <- read.csv("dep_window_per15.csv")
arr_window_per15 <- read.csv("arr_window_per15.csv")

# dep 36l
attach(data_60)
dep_count_36l <- x36l
arr_count_36l <- y36l
detach(data_60)
dep_36l_in_cluster <- vector(mode = "numeric", length = 0)
dep_36l_per_dep <- vector(mode = "numeric", length = 0)
arr_36l_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(dep_36l_origin_feb)){
  a <- dep_window_per15[i, 2]
  dep_36l_in_cluster[i] <- rwy_36l_chosen_hour[a]
  dep_36l_per_dep[i] <- dep_count_36l[a]
  arr_36l_per_dep[i] <- arr_count_36l[a]
}
dep_36l_feb_per15_finished <- cbind(dep_36l_origin_feb, dep_36l_in_cluster,
                              dep_36l_per_dep, arr_36l_per_dep)


# dep 36r
attach(data_60)
dep_count_36r <- x36r
arr_count_36r <- y36r
detach(data_60)
dep_36r_in_cluster <- vector(mode = "numeric", length = 0)
dep_36r_per_dep <- vector(mode = "numeric", length = 0)
arr_36r_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(dep_36r_origin_feb)){
  a <- dep_window_per15[i, 2]
  dep_36r_in_cluster[i] <- rwy_36r_chosen_hour[a]
  dep_36r_per_dep[i] <- dep_count_36r[a]
  arr_36r_per_dep[i] <- arr_count_36r[a]
}
dep_36r_feb_per15_finished <- cbind(dep_36r_origin_feb, dep_36r_in_cluster,
                              dep_36r_per_dep, arr_36r_per_dep)

# dep 01
attach(data_60)
dep_count_01 <- x01
arr_count_01 <- y01
detach(data_60)
dep_01_in_cluster <- vector(mode = "numeric", length = 0)
dep_01_per_dep <- vector(mode = "numeric", length = 0)
arr_01_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(dep_01_origin_feb)){
  a <- dep_window_per15[i, 2]
  dep_01_in_cluster[i] <- rwy_01_chosen_hour[a]
  dep_01_per_dep[i] <- dep_count_01[a]
  arr_01_per_dep[i] <- arr_count_01[a]
}
dep_01_feb_per15_finished <- cbind(dep_01_origin_feb, dep_01_in_cluster,
                             dep_01_per_dep, arr_01_per_dep)

################################################################################
# 15 minutes interval
# arr 36l
attach(data_60)
dep_count_36l <- x36l
arr_count_36l <- y36l
detach(data_60)
arr_36l_in_cluster <- vector(mode = "numeric", length = 0)
dep_36l_per_arr <- vector(mode = "numeric", length = 0)
arr_36l_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(arr_36l_origin_feb)){
  a <- arr_window_per15[i, 2]
  arr_36l_in_cluster[i] <- rwy_36l_chosen_hour[a]
  arr_36l_per_arr[i] <- arr_count_36l[a]
  dep_36l_per_arr[i] <- dep_count_36l[a]
}
arr_36l_feb_per15_finished <- cbind(arr_36l_origin_feb, arr_36l_in_cluster,
                              dep_36l_per_arr, arr_36l_per_arr)

# arr 36r
attach(data_60)
dep_count_36l <- x36l
arr_count_36r <- y36r
detach(data_60)
arr_36r_in_cluster <- vector(mode = "numeric", length = 0)
dep_36r_per_arr <- vector(mode = "numeric", length = 0)
arr_36r_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(arr_36r_origin_feb)){
  a <- arr_window_per15[i, 2]
  arr_36r_in_cluster[i] <- rwy_36r_chosen_hour[a]
  arr_36r_per_arr[i] <- arr_count_36r[a]
  dep_36r_per_arr[i] <- dep_count_36r[a]
}
arr_36r_feb_per15_finished <- cbind(arr_36r_origin_feb, arr_36r_in_cluster,
                              dep_36r_per_arr, arr_36r_per_arr)

# arr 01
attach(data_60)
dep_count_01 <- x01
arr_count_01 <- y01
detach(data_60)
arr_01_in_cluster <- vector(mode = "numeric", length = 0)
dep_01_per_arr <- vector(mode = "numeric", length = 0)
arr_01_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(arr_01_origin_feb)){
  a <- arr_window_per15[i, 2]
  arr_01_in_cluster[i] <- rwy_01_chosen_hour[a]
  arr_01_per_arr[i] <- arr_count_01[a]
  dep_01_per_arr[i] <- dep_count_01[a]
}
arr_01_feb_per15_finished <- cbind(arr_01_origin_feb, arr_01_in_cluster,
                             dep_01_per_arr, arr_01_per_arr)

################################################################################

# output
write.csv(dep_36l_feb_per15_finished, "dep_36l_feb_finished_per15.csv")
write.csv(dep_36r_feb_per15_finished, "dep_36r_feb_finished_per15.csv")
write.csv(dep_01_feb_per15_finished, "dep_01_feb_finished_per15.csv")

write.csv(arr_36l_feb_per15_finished, "arr_36l_feb_finished_per15.csv")
write.csv(arr_36r_feb_per15_finished, "arr_36r_feb_finished_per15.csv")
write.csv(arr_01_feb_per15_finished, "arr_01_feb_finished_per15.csv")


