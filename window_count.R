# 计算各个时段的起飞、降落航班数（2月）
# 读取源数据
dep_origin <- read.csv("dep_origin.csv")
arr_origin <- read.csv("arr_origin.csv")

################################################################################

# 计算每个航班所属的window
# 15 minutes
dep_origin_feb <- subset(dep_origin, mission_month == 2)
arr_origin_feb <- subset(arr_origin, mission_month == 2)
attach(dep_origin_feb)
	dep_window_per15 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_origin_feb)

attach(arr_origin_feb)
	arr_window_per15 <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_origin_feb)

# 60 minutes
attach(dep_origin_feb)
	dep_window_per60 <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_origin_feb)

attach(arr_origin_feb)
	arr_window_per60 <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_origin_feb)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15 <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(dep_window_per15)) {
	a <- dep_window_per15[i]
	dep_count_per15[a] <- dep_count_per15[a] + 1
}

arr_count_per15 <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(arr_window_per15)) {
	a <- arr_window_per15[i]
	arr_count_per15[a] <- arr_count_per15[a] + 1
}

# 60 minutes
dep_count_per60 <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(dep_window_per60)) {
	a <- dep_window_per60[i]
	dep_count_per60[a] <- dep_count_per60[a] + 1
}

arr_count_per60 <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(arr_window_per60)) {
	a <- arr_window_per60[i]
	arr_count_per60[a] <- arr_count_per60[a] + 1
}

################################################################################

# 每架航班所在window的起降数（此处为全部起降航班计算得出的数字）
# 15 minutes
# 为起飞航班添加
dep_15_per_dep <- vector(mode = "numeric", length = 0)
arr_15_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:length(dep_window_per15)) {
	a <- dep_window_per15[i]
	dep_15_per_dep[i] <- dep_count_per15[a]
	arr_15_per_dep[i] <- arr_count_per15[a]
}
# 为降落航班添加
dep_15_per_arr <- vector(mode = "numeric", length = 0)
arr_15_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:length(arr_window_per15)) {
	b <- arr_window_per15[i]
	dep_15_per_arr[i] <- dep_count_per15[a]
	arr_15_per_arr[i] <- arr_count_per15[a]
}

# 60 minutes
# 为起飞航班添加
dep_60_per_dep <- vector(mode = "numeric", length = 0)
arr_60_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:length(dep_window_per60)) {
	a <- dep_window_per60[i]
	dep_60_per_dep[i] <- dep_count_per60[a]
	arr_60_per_dep[i] <- arr_count_per60[a]
}
# 为降落航班添加
dep_60_per_arr <- vector(mode = "numeric", length = 0)
arr_60_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:length(arr_window_per60)) {
	b <- arr_window_per60[i]
	dep_60_per_arr[i] <- dep_count_per60[a]
	arr_60_per_arr[i] <- arr_count_per60[a]
}

# 将每架航班所在window的起降数bind到源数据上
dep_feb <- cbind(dep_origin_feb,
		 data.frame(dep_15_per_dep),
		 data.frame(arr_15_per_dep),
		 data.frame(dep_60_per_dep),
		 data.frame(arr_60_per_dep))
arr_feb <- cbind(arr_origin_feb,
		 data.frame(dep_15_per_arr),
		 data.frame(arr_15_per_arr),
		 data.frame(dep_60_per_arr),
		 data.frame(arr_60_per_arr))

################################################################################
# 输出数据
write.csv(dep_feb, "dep_feb_processed.csv", row.names = F)
write.csv(arr_feb, "arr_feb_processed.csv", row.names = F)


################################################################################