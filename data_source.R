# data source 60min
# dep_origin arr_origin 起降原始数据，包括滑行时间、起降所属的时间段（15分钟、60分钟）
dep_origin <- na.omit(read.csv("dep_origin.csv"))
arr_origin <- na.omit(read.csv("arr_origin.csv"))

# 15分钟起飞率、降落率（2月份） 
dep_count_per15 <- read.csv("dep_count_per15.csv", header = F)
arr_count_per15 <- read.csv("arr_count_per15.csv", header = F)

# 60分钟起飞率、降落率（2月份）
dep_count_per60 <- read.csv("dep_count_per60.csv", header = F)
arr_count_per60 <- read.csv("arr_count_per60.csv", header = F)
################################################################################

# single rwy data
# 单一跑道的运行数据(dep_origin的子集)
dep_36l_origin <- subset(dep_origin, rwy == "36L")
dep_36l_origin_feb <- subset(dep_36l_origin, as.numeric(mission_full_date) <= 20140230)
dep_36r_origin <- subset(dep_origin, rwy == "36R")
dep_36r_origin_feb <- subset(dep_36r_origin, as.numeric(mission_full_date) <= 20140230)
dep_01_origin <- subset(dep_origin, rwy == "1")
dep_01_origin_feb <- subset(dep_01_origin, as.numeric(mission_full_date) <= 20140230)

arr_36l_origin <- subset(arr_origin, rwy == "36L")
arr_36l_origin_feb <- subset(arr_36l_origin, as.numeric(mission_full_date) <= 20140230)
arr_36r_origin <- subset(arr_origin, rwy == "36R")
arr_36r_origin_feb <- subset(arr_36r_origin, as.numeric(mission_full_date) <= 20140230)
arr_01_origin <- subset(arr_origin, rwy == "1")
arr_01_origin_feb <- subset(arr_01_origin, as.numeric(mission_full_date) <= 20140230)

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

################################################################################

# rwy configuration caculation
# 跑道运行模式的计算

# 计算所有跑道起降航班的和
dep_60_matrix <- as.matrix(dep_60)
sum_dep <- rowSums(dep_60_matrix)
arr_60_matrix <- as.matrix(arr_60)
sum_arr <- rowSums(arr_60_matrix)
# 每条跑道各有多少航班
dep_data_summary <- colSums(dep_60_matrix) / sum(colSums(dep_60_matrix))
arr_data_summary <- colSums(arr_60_matrix) / sum(colSums(arr_60_matrix))
# 起降流量之和
sum_per60 <- sum_dep + sum_arr
# 将数据集中到一个dataframe中
data_60 <- cbind(dep_60, sum_dep, arr_60, sum_arr, sum_per60)

# 计算各跑道流量占机场总流量的比例（60分钟）
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

# 每架航班在哪个时间间隔内执行，2月，60分钟间隔共60*28个，15分钟间隔共96*28个
# 算法是遍历每一架航班，已计算出每架航班所在的时间间隔，首先将该航班的时间间隔
# 赋值给a，选取是否在划定的区域内（0-1变量）、15分钟起飞率、15分钟接收率中第a个
# 值，将其赋值给该航班

# 60 minutes interval
# dep 36l

dep_36l_in_cluster <- vector(mode = "numeric", length = 0)
dep_36l_per_dep <- vector(mode = "numeric", length = 0)
arr_36l_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(dep_36l_origin_feb)){
        a <- dep_36l_origin_feb$dep_window_per60[i]
        dep_36l_in_cluster[i] <- rwy_36l_chosen_hour[a]
        dep_36l_per_dep[i] <- as.matrix(dep_count_per60)[a]
        arr_36l_per_dep[i] <- as.matrix(arr_count_per60)[a]
}
dep_36l_feb_per60_finished <- cbind(dep_36l_origin_feb, dep_36l_in_cluster,
                              dep_36l_per_dep, arr_36l_per_dep)


# dep 36r

dep_36r_in_cluster <- vector(mode = "numeric", length = 0)
dep_36r_per_dep <- vector(mode = "numeric", length = 0)
arr_36r_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(dep_36r_origin_feb)){
        a <- dep_36r_origin_feb$dep_window_per60[i]
        dep_36r_in_cluster[i] <- rwy_36r_chosen_hour[a]
        dep_36r_per_dep[i] <- as.matrix(dep_count_per60)[a]
        arr_36r_per_dep[i] <- as.matrix(arr_count_per60)[a]
}
dep_36r_feb_per60_finished <- cbind(dep_36r_origin_feb, dep_36r_in_cluster,
                              dep_36r_per_dep, arr_36r_per_dep)

# dep 01

dep_01_in_cluster <- vector(mode = "numeric", length = 0)
dep_01_per_dep <- vector(mode = "numeric", length = 0)
arr_01_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(dep_01_origin_feb)){
        a <- dep_01_origin_feb$dep_window_per60[i]
        dep_01_in_cluster[i] <- rwy_01_chosen_hour[a]
        dep_01_per_dep[i] <- as.matrix(dep_count_per60)[a]
        arr_01_per_dep[i] <- as.matrix(arr_count_per60)[a]
}
dep_01_feb_per60_finished <- cbind(dep_01_origin_feb, dep_01_in_cluster,
                             dep_01_per_dep, arr_01_per_dep)

################################################################################
# 60 minutes interval
# arr 36l

arr_36l_in_cluster <- vector(mode = "numeric", length = 0)
dep_36l_per_arr <- vector(mode = "numeric", length = 0)
arr_36l_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(arr_36l_origin_feb)){
        a <- arr_36l_origin_feb$arr_window_per60[i]
        arr_36l_in_cluster[i] <- rwy_36l_chosen_hour[a]
        arr_36l_per_arr[i] <- as.matrix(arr_count_per60)[a]
        dep_36l_per_arr[i] <- as.matrix(dep_count_per60)[a]
}
arr_36l_feb_per60_finished <- cbind(arr_36l_origin_feb, arr_36l_in_cluster,
                              dep_36l_per_arr, arr_36l_per_arr)

# arr 36r

arr_36r_in_cluster <- vector(mode = "numeric", length = 0)
dep_36r_per_arr <- vector(mode = "numeric", length = 0)
arr_36r_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(arr_36r_origin_feb)){
        a <- arr_36r_origin_feb$arr_window_per60[i]
        arr_36r_in_cluster[i] <- rwy_36r_chosen_hour[a]
        arr_36r_per_arr[i] <- as.matrix(arr_count_per60)[a]
        dep_36r_per_arr[i] <- as.matrix(dep_count_per60)[a]
}
arr_36r_feb_per60_finished <- cbind(arr_36r_origin_feb, arr_36r_in_cluster,
                              dep_36r_per_arr, arr_36r_per_arr)

# arr 01

arr_01_in_cluster <- vector(mode = "numeric", length = 0)
dep_01_per_arr <- vector(mode = "numeric", length = 0)
arr_01_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(arr_01_origin_feb)){
        a <- arr_01_origin_feb$arr_window_per60[i]
        arr_01_in_cluster[i] <- rwy_01_chosen_hour[a]
        arr_01_per_arr[i] <- as.matrix(arr_count_per60)[a]
        dep_01_per_arr[i] <- as.matrix(dep_count_per60)[a]
}
arr_01_feb_per60_finished <- cbind(arr_01_origin_feb, arr_01_in_cluster,
                             dep_01_per_arr, arr_01_per_arr)

################################################################################

# output
write.csv(dep_36l_feb_per60_finished, "dep_36l_feb_finished_per60.csv")
write.csv(dep_36r_feb_per60_finished, "dep_36r_feb_finished_per60.csv")
write.csv(dep_01_feb_per60_finished, "dep_01_feb_finished_per60.csv")

write.csv(arr_36l_feb_per60_finished, "arr_36l_feb_finished_per60.csv")
write.csv(arr_36r_feb_per60_finished, "arr_36r_feb_finished_per60.csv")
write.csv(arr_01_feb_per60_finished, "arr_01_feb_finished_per60.csv")

################################################################################
################################################################################

# 15 minutes interval
# 15分钟间隔中的“是否属于选定类别中”列和60分钟间隔中相同
# dep 36l

dep_36l_per_dep <- vector(mode = "numeric", length = 0)
arr_36l_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(dep_36l_origin_feb)){
        a <- dep_36l_origin_feb$dep_window_per15[i]
        dep_36l_per_dep[i] <- as.matrix(dep_count_per15)[a]
        arr_36l_per_dep[i] <- as.matrix(arr_count_per15)[a]
}
dep_36l_feb_per15_finished <- cbind(dep_36l_origin_feb,
                                    dep_36l_feb_per60_finished$dep_36l_in_cluster,
                                    dep_36l_per_dep, arr_36l_per_dep)


# dep 36r

dep_36r_per_dep <- vector(mode = "numeric", length = 0)
arr_36r_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(dep_36r_origin_feb)){
        a <- dep_36r_origin_feb$dep_window_per15[i]
        dep_36r_per_dep[i] <- as.matrix(dep_count_per15)[a]
        arr_36r_per_dep[i] <- as.matrix(arr_count_per15)[a]
}
dep_36r_feb_per15_finished <- cbind(dep_36r_origin_feb, 
                                    dep_36r_feb_per60_finished$dep_36r_in_cluster,
                                    dep_36r_per_dep, arr_36r_per_dep)

# dep 01

dep_01_per_dep <- vector(mode = "numeric", length = 0)
arr_01_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(dep_01_origin_feb)){
        a <- dep_01_origin_feb$dep_window_per15[i]
        dep_01_per_dep[i] <- as.matrix(dep_count_per15)[a]
        arr_01_per_dep[i] <- as.matrix(arr_count_per15)[a]
}
dep_01_feb_per15_finished <- cbind(dep_01_origin_feb, 
                                  dep_01_feb_per60_finished$dep_01_in_cluster,
                                  dep_01_per_dep, arr_01_per_dep)

################################################################################
# 15 minutes interval
# arr 36l

dep_36l_per_arr <- vector(mode = "numeric", length = 0)
arr_36l_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(arr_36l_origin_feb)){
        a <- arr_36l_origin_feb$arr_window_per15[i]
        arr_36l_per_arr[i] <- as.matrix(arr_count_per15)[a]
        dep_36l_per_arr[i] <- as.matrix(dep_count_per15)[a]
}
arr_36l_feb_per15_finished <- cbind(arr_36l_origin_feb,
                                    arr_36l_feb_per60_finished$arr_36l_in_cluster,
                                    dep_36l_per_arr, arr_36l_per_arr)

# arr 36r

dep_36r_per_arr <- vector(mode = "numeric", length = 0)
arr_36r_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(arr_36r_origin_feb)){
      a <- arr_36r_origin_feb$arr_window_per15[i]
      arr_36r_per_arr[i] <- as.matrix(arr_count_per15)[a]
      dep_36r_per_arr[i] <- as.matrix(dep_count_per15)[a]
}
arr_36r_feb_per15_finished <- cbind(arr_36r_origin_feb,
                                    arr_36r_feb_per60_finished$arr_36r_in_cluster,
                                    dep_36r_per_arr, arr_36r_per_arr)

# arr 01

dep_01_per_arr <- vector(mode = "numeric", length = 0)
arr_01_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:nrow(arr_01_origin_feb)){
  a <- arr_01_origin_feb$arr_window_per15[i]
  arr_01_per_arr[i] <- as.matrix(arr_count_per15)[a]
  dep_01_per_arr[i] <- as.matrix(dep_count_per15)[a]
}
arr_01_feb_per15_finished <- cbind(arr_01_origin_feb,
                                   arr_01_feb_per60_finished$arr_01_in_cluster,
                                   dep_01_per_arr, arr_01_per_arr)

################################################################################

# output
write.csv(dep_36l_feb_per15_finished, "dep_36l_feb_finished_per15.csv")
write.csv(dep_36r_feb_per15_finished, "dep_36r_feb_finished_per15.csv")
write.csv(dep_01_feb_per15_finished, "dep_01_feb_finished_per15.csv")

write.csv(arr_36l_feb_per15_finished, "arr_36l_feb_finished_per15.csv")
write.csv(arr_36r_feb_per15_finished, "arr_36r_feb_finished_per15.csv")
write.csv(arr_01_feb_per15_finished, "arr_01_feb_finished_per15.csv")

# just for test
