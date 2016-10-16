# 将2月份的数据按跑道分成子集
# 读取2月数据
dep_feb <- read.csv("dep_feb_processed.csv")
arr_feb <- read.csv("arr_feb_processed.csv")
# 按跑道分
# 起飞
dep_feb_36l <- subset(dep_feb, rwy == '36L')
dep_feb_36r <- subset(dep_feb, rwy == '36R')
dep_feb_01 <- subset(dep_feb, rwy == "1")
# 降落
arr_feb_36l <- subset(arr_feb, rwy == '36L')
arr_feb_36r <- subset(arr_feb, rwy == '36R')
arr_feb_01 <- subset(arr_feb, rwy == '1')

# 计算每个window的航班数
# rwy 36l
# 计算每个航班所属的window
# 15 minutes

attach(dep_feb_36l)
	dep_window_per15_36l <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_feb_36l)

attach(arr_feb_36l)
	arr_window_per15_36l <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_feb_36l)

# 60 minutes
attach(dep_feb_36l)
	dep_window_per60_36l <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_feb_36l)

attach(arr_feb_36l)
	arr_window_per60_36l <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_feb_36l)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_36l <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_36l)) {
	a <- dep_window_per15_36l[i]
	dep_count_per15_36l[a] <- dep_count_per15_36l[a] + 1
}

arr_count_per15_36l <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_36l)) {
	a <- arr_window_per15_36l[i]
	arr_count_per15_36l[a] <- arr_count_per15_36l[a] + 1
}

# 60 minutes
dep_count_per60_36l <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_36l)) {
	a <- dep_window_per60_36l[i]
	dep_count_per60_36l[a] <- dep_count_per60_36l[a] + 1
}

arr_count_per60_36l <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_36l)) {
	a <- arr_window_per60_36l[i]
	arr_count_per60_36l[a] <- arr_count_per60_36l[a] + 1
}

# rwy 36r
# 计算每个航班所属的window
# 15 minutes

attach(dep_feb_36r)
	dep_window_per15_36r <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_feb_36r)

attach(arr_feb_36r)
	arr_window_per15_36r <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_feb_36r)

# 60 minutes
attach(dep_feb_36r)
	dep_window_per60_36r <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_feb_36r)

attach(arr_feb_36r)
	arr_window_per60_36r <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_feb_36r)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_36r <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_36r)) {
	a <- dep_window_per15_36r[i]
	dep_count_per15_36r[a] <- dep_count_per15_36r[a] + 1
}

arr_count_per15_36r <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_36r)) {
	a <- arr_window_per15_36r[i]
	arr_count_per15_36r[a] <- arr_count_per15_36r[a] + 1
}

# 60 minutes
dep_count_per60_36r <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_36r)) {
	a <- dep_window_per60_36r[i]
	dep_count_per60_36r[a] <- dep_count_per60_36r[a] + 1
}

arr_count_per60_36r <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_36r)) {
	a <- arr_window_per60_36r[i]
	arr_count_per60_36r[a] <- arr_count_per60_36r[a] + 1
}

# rwy 01
# 计算每个航班所属的window
# 15 minutes

attach(dep_feb_01)
	dep_window_per15_01 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_feb_01)

attach(arr_feb_01)
	arr_window_per15_01 <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_feb_01)

# 60 minutes
attach(dep_feb_01)
	dep_window_per60_01 <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_feb_01)

attach(arr_feb_01)
	arr_window_per60_01 <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_feb_01)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_01 <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_01)) {
	a <- dep_window_per15_01[i]
	dep_count_per15_01[a] <- dep_count_per15_01[a] + 1
}

arr_count_per15_01 <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_01)) {
	a <- arr_window_per15_01[i]
	arr_count_per15_01[a] <- arr_count_per15_01[a] + 1
}

# 60 minutes
dep_count_per60_01 <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_01)) {
	a <- dep_window_per60_01[i]
	dep_count_per60_01[a] <- dep_count_per60_01[a] + 1
}

arr_count_per60_01 <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_01)) {
	a <- arr_window_per60_01[i]
	arr_count_per60_01[a] <- arr_count_per60_01[a] + 1
}

# 将各跑道起降数据放到同一个矩阵中
rwy_flow_per15 <- data.frame(dep_count_per15_36l, dep_count_per15_36r, dep_count_per15_01,
	arr_count_per15_36l, arr_count_per15_36r, arr_count_per15_01)
rwy_flow_per60 <- data.frame(dep_count_per60_36l, dep_count_per60_36r, dep_count_per60_01,
	arr_count_per60_36l, arr_count_per60_36r, arr_count_per60_01)
names(rwy_flow_per15) <- c("dep36l", "dep36r", "dep01", "arr36l", "arr36r", "arr01")
names(rwy_flow_per60) <- c("dep36l", "dep36r", "dep01", "arr36l", "arr36r", "arr01")
write.csv(rwy_flow_per15, "rwy_flow_per15.csv", row.names = F)
write.csv(rwy_flow_per60, "rwy_flow_per60.csv", row.names = F)
