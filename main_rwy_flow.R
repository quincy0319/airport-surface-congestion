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
	dep_window_per15_feb_36l <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_feb_36l)

attach(arr_feb_36l)
	arr_window_per15_feb_36l <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_feb_36l)

# 60 minutes
attach(dep_feb_36l)
	dep_window_per60_feb_36l <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_feb_36l)

attach(arr_feb_36l)
	arr_window_per60_feb_36l <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_feb_36l)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_feb_36l <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_feb_36l)) {
	a <- dep_window_per15_feb_36l[i]
	dep_count_per15_feb_36l[a] <- dep_count_per15_feb_36l[a] + 1
}

arr_count_per15_feb_36l <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_feb_36l)) {
	a <- arr_window_per15_feb_36l[i]
	arr_count_per15_feb_36l[a] <- arr_count_per15_feb_36l[a] + 1
}

# 60 minutes
dep_count_per60_feb_36l <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_feb_36l)) {
	a <- dep_window_per60_feb_36l[i]
	dep_count_per60_feb_36l[a] <- dep_count_per60_feb_36l[a] + 1
}

arr_count_per60_feb_36l <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_feb_36l)) {
	a <- arr_window_per60_feb_36l[i]
	arr_count_per60_feb_36l[a] <- arr_count_per60_feb_36l[a] + 1
}

# rwy 36r
# 计算每个航班所属的window
# 15 minutes

attach(dep_feb_36r)
	dep_window_per15_feb_36r <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_feb_36r)

attach(arr_feb_36r)
	arr_window_per15_feb_36r <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_feb_36r)

# 60 minutes
attach(dep_feb_36r)
	dep_window_per60_feb_36r <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_feb_36r)

attach(arr_feb_36r)
	arr_window_per60_feb_36r <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_feb_36r)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_feb_36r <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_feb_36r)) {
	a <- dep_window_per15_feb_36r[i]
	dep_count_per15_feb_36r[a] <- dep_count_per15_feb_36r[a] + 1
}

arr_count_per15_feb_36r <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_feb_36r)) {
	a <- arr_window_per15_feb_36r[i]
	arr_count_per15_feb_36r[a] <- arr_count_per15_feb_36r[a] + 1
}

# 60 minutes
dep_count_per60_feb_36r <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_feb_36r)) {
	a <- dep_window_per60_feb_36r[i]
	dep_count_per60_feb_36r[a] <- dep_count_per60_feb_36r[a] + 1
}

arr_count_per60_feb_36r <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_feb_36r)) {
	a <- arr_window_per60_feb_36r[i]
	arr_count_per60_feb_36r[a] <- arr_count_per60_feb_36r[a] + 1
}

# rwy 01
# 计算每个航班所属的window
# 15 minutes

attach(dep_feb_01)
	dep_window_per15_feb_01 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_feb_01)

attach(arr_feb_01)
	arr_window_per15_feb_01 <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_feb_01)

# 60 minutes
attach(dep_feb_01)
	dep_window_per60_feb_01 <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_feb_01)

attach(arr_feb_01)
	arr_window_per60_feb_01 <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_feb_01)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_feb_01 <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_feb_01)) {
	a <- dep_window_per15_feb_01[i]
	dep_count_per15_feb_01[a] <- dep_count_per15_feb_01[a] + 1
}

arr_count_per15_feb_01 <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_feb_01)) {
	a <- arr_window_per15_feb_01[i]
	arr_count_per15_feb_01[a] <- arr_count_per15_feb_01[a] + 1
}

# 60 minutes
dep_count_per60_feb_01 <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_feb_01)) {
	a <- dep_window_per60_feb_01[i]
	dep_count_per60_feb_01[a] <- dep_count_per60_feb_01[a] + 1
}

arr_count_per60_feb_01 <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_feb_01)) {
	a <- arr_window_per60_feb_01[i]
	arr_count_per60_feb_01[a] <- arr_count_per60_feb_01[a] + 1
}

# 将各跑道起降数据放到同一个矩阵中
rwy_flow_feb_per15 <- data.frame(dep_count_per15_feb_36l, dep_count_per15_feb_36r, dep_count_per15_feb_01,
	arr_count_per15_feb_36l, arr_count_per15_feb_36r, arr_count_per15_feb_01)
rwy_flow_feb_per60 <- data.frame(dep_count_per60_feb_36l, dep_count_per60_feb_36r, dep_count_per60_feb_01,
	arr_count_per60_feb_36l, arr_count_per60_feb_36r, arr_count_per60_feb_01)
names(rwy_flow_feb_per15) <- c("dep36l", "dep36r", "dep01", "arr36l", "arr36r", "arr01")
names(rwy_flow_feb_per60) <- c("dep36l", "dep36r", "dep01", "arr36l", "arr36r", "arr01")
write.csv(rwy_flow_feb_per15, "rwy_flow_feb_per15.csv", row.names = F)
write.csv(rwy_flow_feb_per60, "rwy_flow_feb_per60.csv", row.names = F)


################################################################################

# 将3月份的数据按跑道分成子集
# 读取3月数据
dep_mar <- read.csv("dep_mar_processed.csv")
arr_mar <- read.csv("arr_mar_processed.csv")
# 按跑道分
# 起飞
dep_mar_36l <- subset(dep_mar, rwy == '36L')
dep_mar_36r <- subset(dep_mar, rwy == '36R')
dep_mar_01 <- subset(dep_mar, rwy == "1")
# 降落
arr_mar_36l <- subset(arr_mar, rwy == '36L')
arr_mar_36r <- subset(arr_mar, rwy == '36R')
arr_mar_01 <- subset(arr_mar, rwy == '1')

# 计算每个window的航班数
# rwy 36l
# 计算每个航班所属的window
# 15 minutes

attach(dep_mar_36l)
dep_window_per15_mar_36l <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_mar_36l)

attach(arr_mar_36l)
arr_window_per15_mar_36l <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_mar_36l)

# 60 minutes
attach(dep_mar_36l)
dep_window_per60_mar_36l <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_mar_36l)

attach(arr_mar_36l)
arr_window_per60_mar_36l <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_mar_36l)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_mar_36l <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_mar_36l)) {
	a <- dep_window_per15_mar_36l[i]
	dep_count_per15_mar_36l[a] <- dep_count_per15_mar_36l[a] + 1
}

arr_count_per15_mar_36l <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_mar_36l)) {
	a <- arr_window_per15_mar_36l[i]
	arr_count_per15_mar_36l[a] <- arr_count_per15_mar_36l[a] + 1
}

# 60 minutes
dep_count_per60_mar_36l <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_mar_36l)) {
	a <- dep_window_per60_mar_36l[i]
	dep_count_per60_mar_36l[a] <- dep_count_per60_mar_36l[a] + 1
}

arr_count_per60_mar_36l <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_mar_36l)) {
	a <- arr_window_per60_mar_36l[i]
	arr_count_per60_mar_36l[a] <- arr_count_per60_mar_36l[a] + 1
}

# rwy 36r
# 计算每个航班所属的window
# 15 minutes

attach(dep_mar_36r)
dep_window_per15_mar_36r <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_mar_36r)

attach(arr_mar_36r)
arr_window_per15_mar_36r <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_mar_36r)

# 60 minutes
attach(dep_mar_36r)
dep_window_per60_mar_36r <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_mar_36r)

attach(arr_mar_36r)
arr_window_per60_mar_36r <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_mar_36r)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_mar_36r <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_mar_36r)) {
	a <- dep_window_per15_mar_36r[i]
	dep_count_per15_mar_36r[a] <- dep_count_per15_mar_36r[a] + 1
}

arr_count_per15_mar_36r <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_mar_36r)) {
	a <- arr_window_per15_mar_36r[i]
	arr_count_per15_mar_36r[a] <- arr_count_per15_mar_36r[a] + 1
}

# 60 minutes
dep_count_per60_mar_36r <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_mar_36r)) {
	a <- dep_window_per60_mar_36r[i]
	dep_count_per60_mar_36r[a] <- dep_count_per60_mar_36r[a] + 1
}

arr_count_per60_mar_36r <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_mar_36r)) {
	a <- arr_window_per60_mar_36r[i]
	arr_count_per60_mar_36r[a] <- arr_count_per60_mar_36r[a] + 1
}

# rwy 01
# 计算每个航班所属的window
# 15 minutes

attach(dep_mar_01)
dep_window_per15_mar_01 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_mar_01)

attach(arr_mar_01)
arr_window_per15_mar_01 <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_mar_01)

# 60 minutes
attach(dep_mar_01)
dep_window_per60_mar_01 <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_mar_01)

attach(arr_mar_01)
arr_window_per60_mar_01 <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_mar_01)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_mar_01 <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_mar_01)) {
	a <- dep_window_per15_mar_01[i]
	dep_count_per15_mar_01[a] <- dep_count_per15_mar_01[a] + 1
}

arr_count_per15_mar_01 <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_mar_01)) {
	a <- arr_window_per15_mar_01[i]
	arr_count_per15_mar_01[a] <- arr_count_per15_mar_01[a] + 1
}

# 60 minutes
dep_count_per60_mar_01 <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_mar_01)) {
	a <- dep_window_per60_mar_01[i]
	dep_count_per60_mar_01[a] <- dep_count_per60_mar_01[a] + 1
}

arr_count_per60_mar_01 <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_mar_01)) {
	a <- arr_window_per60_mar_01[i]
	arr_count_per60_mar_01[a] <- arr_count_per60_mar_01[a] + 1
}

# 将各跑道起降数据放到同一个矩阵中
rwy_flow_mar_per15 <- data.frame(dep_count_per15_mar_36l, dep_count_per15_mar_36r, dep_count_per15_mar_01,
	arr_count_per15_mar_36l, arr_count_per15_mar_36r, arr_count_per15_mar_01)
rwy_flow_mar_per60 <- data.frame(dep_count_per60_mar_36l, dep_count_per60_mar_36r, dep_count_per60_mar_01,
	arr_count_per60_mar_36l, arr_count_per60_mar_36r, arr_count_per60_mar_01)
names(rwy_flow_mar_per15) <- c("dep36l", "dep36r", "dep01", "arr36l", "arr36r", "arr01")
names(rwy_flow_mar_per60) <- c("dep36l", "dep36r", "dep01", "arr36l", "arr36r", "arr01")
write.csv(rwy_flow_mar_per15, "rwy_flow_mar_per15.csv", row.names = F)
write.csv(rwy_flow_mar_per60, "rwy_flow_mar_per60.csv", row.names = F)


################################################################################

# 将4月份的数据按跑道分成子集
# 读取4月数据
dep_apr <- read.csv("dep_apr_processed.csv")
arr_apr <- read.csv("arr_apr_processed.csv")
# 按跑道分
# 起飞
dep_apr_36l <- subset(dep_apr, rwy == '36L')
dep_apr_36r <- subset(dep_apr, rwy == '36R')
dep_apr_01 <- subset(dep_apr, rwy == "1")
# 降落
arr_apr_36l <- subset(arr_apr, rwy == '36L')
arr_apr_36r <- subset(arr_apr, rwy == '36R')
arr_apr_01 <- subset(arr_apr, rwy == '1')

# 计算每个window的航班数
# rwy 36l
# 计算每个航班所属的window
# 15 minutes

attach(dep_apr_36l)
dep_window_per15_apr_36l <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_apr_36l)

attach(arr_apr_36l)
arr_window_per15_apr_36l <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_apr_36l)

# 60 minutes
attach(dep_apr_36l)
dep_window_per60_apr_36l <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_apr_36l)

attach(arr_apr_36l)
arr_window_per60_apr_36l <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_apr_36l)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_apr_36l <- matrix(0, nrow = 30 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_apr_36l)) {
	a <- dep_window_per15_apr_36l[i]
	dep_count_per15_apr_36l[a] <- dep_count_per15_apr_36l[a] + 1
}

arr_count_per15_apr_36l <- matrix(0, nrow = 30 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_apr_36l)) {
	a <- arr_window_per15_apr_36l[i]
	arr_count_per15_apr_36l[a] <- arr_count_per15_apr_36l[a] + 1
}

# 60 minutes
dep_count_per60_apr_36l <- matrix(0, nrow = 30 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_apr_36l)) {
	a <- dep_window_per60_apr_36l[i]
	dep_count_per60_apr_36l[a] <- dep_count_per60_apr_36l[a] + 1
}

arr_count_per60_apr_36l <- matrix(0, nrow = 30 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_apr_36l)) {
	a <- arr_window_per60_apr_36l[i]
	arr_count_per60_apr_36l[a] <- arr_count_per60_apr_36l[a] + 1
}

# rwy 36r
# 计算每个航班所属的window
# 15 minutes

attach(dep_apr_36r)
dep_window_per15_apr_36r <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_apr_36r)

attach(arr_apr_36r)
arr_window_per15_apr_36r <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_apr_36r)

# 60 minutes
attach(dep_apr_36r)
dep_window_per60_apr_36r <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_apr_36r)

attach(arr_apr_36r)
arr_window_per60_apr_36r <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_apr_36r)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_apr_36r <- matrix(0, nrow = 30 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_apr_36r)) {
	a <- dep_window_per15_apr_36r[i]
	dep_count_per15_apr_36r[a] <- dep_count_per15_apr_36r[a] + 1
}

arr_count_per15_apr_36r <- matrix(0, nrow = 30 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_apr_36r)) {
	a <- arr_window_per15_apr_36r[i]
	arr_count_per15_apr_36r[a] <- arr_count_per15_apr_36r[a] + 1
}

# 60 minutes
dep_count_per60_apr_36r <- matrix(0, nrow = 30 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_apr_36r)) {
	a <- dep_window_per60_apr_36r[i]
	dep_count_per60_apr_36r[a] <- dep_count_per60_apr_36r[a] + 1
}

arr_count_per60_apr_36r <- matrix(0, nrow = 30 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_apr_36r)) {
	a <- arr_window_per60_apr_36r[i]
	arr_count_per60_apr_36r[a] <- arr_count_per60_apr_36r[a] + 1
}

# rwy 01
# 计算每个航班所属的window
# 15 minutes

attach(dep_apr_01)
dep_window_per15_apr_01 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_apr_01)

attach(arr_apr_01)
arr_window_per15_apr_01 <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_apr_01)

# 60 minutes
attach(dep_apr_01)
dep_window_per60_apr_01 <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_apr_01)

attach(arr_apr_01)
arr_window_per60_apr_01 <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_apr_01)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_apr_01 <- matrix(0, nrow = 30 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_apr_01)) {
	a <- dep_window_per15_apr_01[i]
	dep_count_per15_apr_01[a] <- dep_count_per15_apr_01[a] + 1
}

arr_count_per15_apr_01 <- matrix(0, nrow = 30 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_apr_01)) {
	a <- arr_window_per15_apr_01[i]
	arr_count_per15_apr_01[a] <- arr_count_per15_apr_01[a] + 1
}

# 60 minutes
dep_count_per60_apr_01 <- matrix(0, nrow = 30 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_apr_01)) {
	a <- dep_window_per60_apr_01[i]
	dep_count_per60_apr_01[a] <- dep_count_per60_apr_01[a] + 1
}

arr_count_per60_apr_01 <- matrix(0, nrow = 30 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_apr_01)) {
	a <- arr_window_per60_apr_01[i]
	arr_count_per60_apr_01[a] <- arr_count_per60_apr_01[a] + 1
}

# 将各跑道起降数据放到同一个矩阵中
rwy_flow_apr_per15 <- data.frame(dep_count_per15_apr_36l, dep_count_per15_apr_36r, dep_count_per15_apr_01,
	arr_count_per15_apr_36l, arr_count_per15_apr_36r, arr_count_per15_apr_01)
rwy_flow_apr_per60 <- data.frame(dep_count_per60_apr_36l, dep_count_per60_apr_36r, dep_count_per60_apr_01,
	arr_count_per60_apr_36l, arr_count_per60_apr_36r, arr_count_per60_apr_01)
names(rwy_flow_apr_per15) <- c("dep36l", "dep36r", "dep01", "arr36l", "arr36r", "arr01")
names(rwy_flow_apr_per60) <- c("dep36l", "dep36r", "dep01", "arr36l", "arr36r", "arr01")
write.csv(rwy_flow_apr_per15, "rwy_flow_apr_per15.csv", row.names = F)
write.csv(rwy_flow_apr_per60, "rwy_flow_apr_per60.csv", row.names = F)

################################################################################
# 将5月份的数据按跑道分成子集
# 读取5月数据
dep_may <- read.csv("dep_may_processed.csv")
arr_may <- read.csv("arr_may_processed.csv")
# 按跑道分
# 起飞
dep_may_36l <- subset(dep_may, rwy == '36L')
dep_may_36r <- subset(dep_may, rwy == '36R')
dep_may_01 <- subset(dep_may, rwy == "1")
# 降落
arr_may_36l <- subset(arr_may, rwy == '36L')
arr_may_36r <- subset(arr_may, rwy == '36R')
arr_may_01 <- subset(arr_may, rwy == '1')

# 计算每个window的航班数
# rwy 36l
# 计算每个航班所属的window
# 15 minutes

attach(dep_may_36l)
dep_window_per15_may_36l <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_may_36l)

attach(arr_may_36l)
arr_window_per15_may_36l <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_may_36l)

# 60 minutes
attach(dep_may_36l)
dep_window_per60_may_36l <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_may_36l)

attach(arr_may_36l)
arr_window_per60_may_36l <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_may_36l)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_may_36l <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_may_36l)) {
	a <- dep_window_per15_may_36l[i]
	dep_count_per15_may_36l[a] <- dep_count_per15_may_36l[a] + 1
}

arr_count_per15_may_36l <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_may_36l)) {
	a <- arr_window_per15_may_36l[i]
	arr_count_per15_may_36l[a] <- arr_count_per15_may_36l[a] + 1
}

# 60 minutes
dep_count_per60_may_36l <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_may_36l)) {
	a <- dep_window_per60_may_36l[i]
	dep_count_per60_may_36l[a] <- dep_count_per60_may_36l[a] + 1
}

arr_count_per60_may_36l <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_may_36l)) {
	a <- arr_window_per60_may_36l[i]
	arr_count_per60_may_36l[a] <- arr_count_per60_may_36l[a] + 1
}

# rwy 36r
# 计算每个航班所属的window
# 15 minutes

attach(dep_may_36r)
dep_window_per15_may_36r <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_may_36r)

attach(arr_may_36r)
arr_window_per15_may_36r <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_may_36r)

# 60 minutes
attach(dep_may_36r)
dep_window_per60_may_36r <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_may_36r)

attach(arr_may_36r)
arr_window_per60_may_36r <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_may_36r)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_may_36r <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_may_36r)) {
	a <- dep_window_per15_may_36r[i]
	dep_count_per15_may_36r[a] <- dep_count_per15_may_36r[a] + 1
}

arr_count_per15_may_36r <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_may_36r)) {
	a <- arr_window_per15_may_36r[i]
	arr_count_per15_may_36r[a] <- arr_count_per15_may_36r[a] + 1
}

# 60 minutes
dep_count_per60_may_36r <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_may_36r)) {
	a <- dep_window_per60_may_36r[i]
	dep_count_per60_may_36r[a] <- dep_count_per60_may_36r[a] + 1
}

arr_count_per60_may_36r <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_may_36r)) {
	a <- arr_window_per60_may_36r[i]
	arr_count_per60_may_36r[a] <- arr_count_per60_may_36r[a] + 1
}

# rwy 01
# 计算每个航班所属的window
# 15 minutes

attach(dep_may_01)
dep_window_per15_may_01 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(dep_may_01)

attach(arr_may_01)
arr_window_per15_may_01 <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_may_01)

# 60 minutes
attach(dep_may_01)
dep_window_per60_may_01 <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_may_01)

attach(arr_may_01)
arr_window_per60_may_01 <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_may_01)

# 计算每个window有多少架航班
# 15 minutes
dep_count_per15_may_01 <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(dep_window_per15_may_01)) {
	a <- dep_window_per15_may_01[i]
	dep_count_per15_may_01[a] <- dep_count_per15_may_01[a] + 1
}

arr_count_per15_may_01 <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(arr_window_per15_may_01)) {
	a <- arr_window_per15_may_01[i]
	arr_count_per15_may_01[a] <- arr_count_per15_may_01[a] + 1
}

# 60 minutes
dep_count_per60_may_01 <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(dep_window_per60_may_01)) {
	a <- dep_window_per60_may_01[i]
	dep_count_per60_may_01[a] <- dep_count_per60_may_01[a] + 1
}

arr_count_per60_may_01 <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(arr_window_per60_may_01)) {
	a <- arr_window_per60_may_01[i]
	arr_count_per60_may_01[a] <- arr_count_per60_may_01[a] + 1
}

# 将各跑道起降数据放到同一个矩阵中
rwy_flow_may_per15 <- data.frame(dep_count_per15_may_36l, dep_count_per15_may_36r, dep_count_per15_may_01,
	arr_count_per15_may_36l, arr_count_per15_may_36r, arr_count_per15_may_01)
rwy_flow_may_per60 <- data.frame(dep_count_per60_may_36l, dep_count_per60_may_36r, dep_count_per60_may_01,
	arr_count_per60_may_36l, arr_count_per60_may_36r, arr_count_per60_may_01)
names(rwy_flow_may_per15) <- c("dep36l", "dep36r", "dep01", "arr36l", "arr36r", "arr01")
names(rwy_flow_may_per60) <- c("dep36l", "dep36r", "dep01", "arr36l", "arr36r", "arr01")
write.csv(rwy_flow_may_per15, "rwy_flow_may_per15.csv", row.names = F)
write.csv(rwy_flow_may_per60, "rwy_flow_may_per60.csv", row.names = F)