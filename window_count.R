# 计算各个时段的起飞、降落航班数（2月）
# 读取源数据
dep_origin <- read.csv("dep_origin.csv")
arr_origin <- read.csv("arr_origin.csv")

################################################################################

# 计算每个航班属于哪个window
# 15 minutes
dep_origin_feb <- subset(dep_origin, mission_month == 2)
arr_origin_feb <- subset(arr_origin, mission_month == 2)
attach(dep_origin_feb)
	dep_window_per15 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15)
detach(dep_origin_feb)

attach(arr_origin_feb)
	arr_window_per15 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15)
detach(dep_origin_feb)

# 60 minutes
attach(dep_origin_feb)
	dep_window_per60 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 60)
detach(dep_origin_feb)

attach(arr_origin_feb)
	arr_window_per60 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 60)
detach(arr_origin_feb)

# 计算每个window有多少架航班（15分钟）
# 15 minutes
dep_count_per15 <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(dep_window_per15)) {
	a <- dep_window_per15[i]
	dep_count_per15[a] <- dep_count_per15[a] + 1
}

arr_count_per15 <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(arr_window_per15)) {
	a <- dep_window_per15[i]
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
	a <- dep_window_per60[i]
	arr_count_per60[a] <- arr_count_per60[a] + 1
}

################################################################################