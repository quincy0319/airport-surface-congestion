# 计算各个时段的起飞、降落航班数（2月）
# 读取源数据
dep_origin <- read.csv("dep_tower.csv")
arr_origin <- read.csv("arr_tower.csv")

################################################################################

# 计算每个航班所属的window
# feb
# 15 minutes
dep_origin_feb <- subset(dep_origin, mission_month == 2)
arr_origin_feb <- subset(arr_origin, mission_month == 2)
attach(dep_origin_feb)
	dep_window_feb_per15 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
	pb_window_feb_per15 <- (mission_date - 1) * 96 + floor(time_pushback / 15) + 1
detach(dep_origin_feb)

attach(arr_origin_feb)
	arr_window_feb_per15 <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_origin_feb)

# 60 minutes
attach(dep_origin_feb)
	dep_window_feb_per60 <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
	pb_window_feb_per60 <- (mission_date - 1) * 24 + floor(time_pushback / 60) + 1
detach(dep_origin_feb)

attach(arr_origin_feb)
	arr_window_feb_per60 <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_origin_feb)
################################################################################

# 计算每个航班所属的window
# mar
# 15 minutes
dep_origin_mar <- subset(dep_origin, mission_month == 3)
arr_origin_mar <- subset(arr_origin, mission_month == 3)
attach(dep_origin_mar)
dep_window_mar_per15 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
pb_window_mar_per15 <- (mission_date - 1) * 96 + floor(time_pushback / 15) + 1
detach(dep_origin_mar)

attach(arr_origin_mar)
arr_window_mar_per15 <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_origin_mar)

# 60 minutes
attach(dep_origin_mar)
dep_window_mar_per60 <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
pb_window_mar_per60 <- (mission_date - 1) * 24 + floor(time_pushback / 60) + 1
detach(dep_origin_mar)

attach(arr_origin_mar)
arr_window_mar_per60 <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_origin_mar)
################################################################################

# 计算每个航班所属的window
# apr
# 15 minutes
dep_origin_apr <- subset(dep_origin, mission_month == 4)
arr_origin_apr <- subset(arr_origin, mission_month == 4)
attach(dep_origin_apr)
dep_window_apr_per15 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
pb_window_apr_per15 <- (mission_date - 1) * 96 + floor(time_pushback / 15) + 1
detach(dep_origin_apr)

attach(arr_origin_apr)
arr_window_apr_per15 <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_origin_apr)

# 60 minutes
attach(dep_origin_apr)
dep_window_apr_per60 <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
pb_window_apr_per60 <- (mission_date - 1) * 24 + floor(time_pushback / 60) + 1
detach(dep_origin_apr)

attach(arr_origin_apr)
arr_window_apr_per60 <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_origin_apr)
################################################################################

# 计算每个航班所属的window
# may
# 15 minutes
dep_origin_may <- subset(dep_origin, mission_month == 5)
arr_origin_may <- subset(arr_origin, mission_month == 5)
attach(dep_origin_may)
dep_window_may_per15 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
pb_window_may_per15 <- (mission_date - 1) * 96 + floor(time_pushback / 15) + 1
detach(dep_origin_may)

attach(arr_origin_may)
arr_window_may_per15 <- (mission_date - 1) * 96 + floor(time_wheelon_real / 15) + 1
detach(arr_origin_may)

# 60 minutes
attach(dep_origin_may)
dep_window_may_per60 <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
pb_window_may_per60 <- (mission_date - 1) * 24 + floor(time_pushback / 60) + 1
detach(dep_origin_may)

attach(arr_origin_may)
arr_window_may_per60 <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_origin_may)

################################################################################
################################################################################

# 计算每个window有多少架航班
# feb
# 15 minutes
dep_count_feb_per15 <- matrix(0, nrow = 28 * 96, ncol = 1)
pb_count_feb_per15 <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(dep_window_feb_per15)) {
	a <- dep_window_feb_per15[i]
	b <- pb_window_feb_per15[i]
	dep_count_feb_per15[a] <- dep_count_feb_per15[a] + 1
	pb_count_feb_per15[b] <- pb_count_feb_per15[b] + 1
}

arr_count_feb_per15 <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(arr_window_feb_per15)) {
	a <- arr_window_feb_per15[i]
	arr_count_feb_per15[a] <- arr_count_feb_per15[a] + 1
}

# 60 minutes
dep_count_feb_per60 <- matrix(0, nrow = 28 * 24, ncol = 1)
pb_count_feb_per60 <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(dep_window_feb_per60)) {
	a <- dep_window_feb_per60[i]
	b <- pb_window_feb_per60[i]
	dep_count_feb_per60[a] <- dep_count_feb_per60[a] + 1
	pb_count_feb_per60[b] <- pb_count_feb_per60[b] + 1
}

arr_count_feb_per60 <- matrix(0, nrow = 28 * 24, ncol = 1)
for (i in 1:length(arr_window_feb_per60)) {
	a <- arr_window_feb_per60[i]
	arr_count_feb_per60[a] <- arr_count_feb_per60[a] + 1
}

window_count_feb_per15 <- data.frame(dep_count_feb_per15, arr_count_feb_per15, pb_count_feb_per15)
window_count_feb_per60 <- data.frame(dep_count_feb_per60, arr_count_feb_per60, pb_count_feb_per60)
write.csv(window_count_feb_per15, "window_count_feb_per15.csv", row.names = F)
write.csv(window_count_feb_per60, "window_count_feb_per60.csv", row.names = F)

################################################################################

# 每架航班所在window的起降数（此处为全部起降航班计算得出的数字）
# 15 minutes
# 为起飞航班添加
dep_feb_15_per_dep <- vector(mode = "numeric", length = 0)
arr_feb_15_per_dep <- vector(mode = "numeric", length = 0)
pb_feb_15_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:length(dep_window_feb_per15)) {
	a <- dep_window_feb_per15[i]
	dep_feb_15_per_dep[i] <- dep_count_feb_per15[a]
	arr_feb_15_per_dep[i] <- arr_count_feb_per15[a]
	pb_feb_15_per_dep[i] <- pb_count_feb_per15[a]
}
# 为降落航班添加
dep_feb_15_per_arr <- vector(mode = "numeric", length = 0)
arr_feb_15_per_arr <- vector(mode = "numeric", length = 0)
pb_feb_15_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:length(arr_window_feb_per15)) {
	b <- arr_window_feb_per15[i]
	dep_feb_15_per_arr[i] <- dep_count_feb_per15[b]
	arr_feb_15_per_arr[i] <- arr_count_feb_per15[b]
	pb_feb_15_per_arr[i] <- pb_count_feb_per15[b]
}

# 60 minutes
# 为起飞航班添加
dep_feb_60_per_dep <- vector(mode = "numeric", length = 0)
arr_feb_60_per_dep <- vector(mode = "numeric", length = 0)
pb_feb_60_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:length(dep_window_feb_per60)) {
	a <- dep_window_feb_per60[i]
	dep_feb_60_per_dep[i] <- dep_count_feb_per60[a]
	arr_feb_60_per_dep[i] <- arr_count_feb_per60[a]
	pb_feb_60_per_dep[i] <- pb_count_feb_per60[a]
}
# 为降落航班添加
dep_feb_60_per_arr <- vector(mode = "numeric", length = 0)
arr_feb_60_per_arr <- vector(mode = "numeric", length = 0)
pb_feb_60_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:length(arr_window_feb_per60)) {
	b <- arr_window_feb_per60[i]
	dep_feb_60_per_arr[i] <- dep_count_feb_per60[b]
	arr_feb_60_per_arr[i] <- arr_count_feb_per60[b]
	pb_feb_60_per_arr[i] <- pb_count_feb_per60[a]
}

# 将每架航班所在window的起降数bind到源数据上
dep_feb <- cbind(dep_origin_feb,
		 data.frame(dep_feb_15_per_dep),
		 data.frame(arr_feb_15_per_dep),
		 data.frame(dep_feb_60_per_dep),
		 data.frame(arr_feb_60_per_dep),
		 data.frame(pb_feb_15_per_dep),
		 data.frame(pb_feb_60_per_dep))
arr_feb <- cbind(arr_origin_feb,
		 data.frame(dep_feb_15_per_arr),
		 data.frame(arr_feb_15_per_arr),
		 data.frame(dep_feb_60_per_arr),
		 data.frame(arr_feb_60_per_arr),
		 data.frame(pb_feb_15_per_arr),
		 data.frame(pb_feb_60_per_arr))


################################################################################
# 输出数据
write.csv(dep_feb, "dep_feb_processed.csv", row.names = F)
write.csv(arr_feb, "arr_feb_processed.csv", row.names = F)


################################################################################

################################################################################

# 计算每个window有多少架航班
# mar
# 15 minutes
dep_count_mar_per15 <- matrix(0, nrow = 31 * 96, ncol = 1)
pb_count_mar_per15 <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(dep_window_mar_per15)) {
	a <- dep_window_mar_per15[i]
	b <- pb_window_mar_per15[i]
	dep_count_mar_per15[a] <- dep_count_mar_per15[a] + 1
	pb_count_mar_per15[b] <- pb_count_mar_per15[b] + 1
}

arr_count_mar_per15 <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(arr_window_mar_per15)) {
	a <- arr_window_mar_per15[i]
	arr_count_mar_per15[a] <- arr_count_mar_per15[a] + 1
}

# 60 minutes
dep_count_mar_per60 <- matrix(0, nrow = 31 * 24, ncol = 1)
pb_count_mar_per60 <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(dep_window_mar_per60)) {
	a <- dep_window_mar_per60[i]
	b <- pb_window_mar_per60[i]
	dep_count_mar_per60[a] <- dep_count_mar_per60[a] + 1
	pb_count_mar_per60[b] <- pb_count_mar_per60[b] + 1
}

arr_count_mar_per60 <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(arr_window_mar_per60)) {
	a <- arr_window_mar_per60[i]
	arr_count_mar_per60[a] <- arr_count_mar_per60[a] + 1
}

window_count_mar_per15 <- data.frame(dep_count_mar_per15, arr_count_mar_per15, pb_count_mar_per15)
window_count_mar_per60 <- data.frame(dep_count_mar_per60, arr_count_mar_per60, pb_count_mar_per60)
write.csv(window_count_mar_per15, "window_count_mar_per15.csv", row.names = F)
write.csv(window_count_mar_per60, "window_count_mar_per60.csv", row.names = F)

################################################################################

# 每架航班所在window的起降数（此处为全部起降航班计算得出的数字）
# 15 minutes
# 为起飞航班添加
dep_mar_15_per_dep <- vector(mode = "numeric", length = 0)
arr_mar_15_per_dep <- vector(mode = "numeric", length = 0)
pb_mar_15_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:length(dep_window_mar_per15)) {
	a <- dep_window_mar_per15[i]
	dep_mar_15_per_dep[i] <- dep_count_mar_per15[a]
	arr_mar_15_per_dep[i] <- arr_count_mar_per15[a]
	pb_mar_15_per_dep[i] <- pb_count_mar_per15[a]
}
# 为降落航班添加
dep_mar_15_per_arr <- vector(mode = "numeric", length = 0)
arr_mar_15_per_arr <- vector(mode = "numeric", length = 0)
pb_mar_15_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:length(arr_window_mar_per15)) {
	b <- arr_window_mar_per15[i]
	dep_mar_15_per_arr[i] <- dep_count_mar_per15[b]
	arr_mar_15_per_arr[i] <- arr_count_mar_per15[b]
	pb_mar_15_per_arr[i] <- pb_count_mar_per15[b]
}

# 60 minutes
# 为起飞航班添加
dep_mar_60_per_dep <- vector(mode = "numeric", length = 0)
arr_mar_60_per_dep <- vector(mode = "numeric", length = 0)
pb_mar_60_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:length(dep_window_mar_per60)) {
	a <- dep_window_mar_per60[i]
	dep_mar_60_per_dep[i] <- dep_count_mar_per60[a]
	arr_mar_60_per_dep[i] <- arr_count_mar_per60[a]
	pb_mar_60_per_dep[i] <- pb_count_mar_per60[a]
}
# 为降落航班添加
dep_mar_60_per_arr <- vector(mode = "numeric", length = 0)
arr_mar_60_per_arr <- vector(mode = "numeric", length = 0)
pb_mar_60_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:length(arr_window_mar_per60)) {
	b <- arr_window_mar_per60[i]
	dep_mar_60_per_arr[i] <- dep_count_mar_per60[b]
	arr_mar_60_per_arr[i] <- arr_count_mar_per60[b]
	pb_mar_60_per_arr[i] <- pb_count_mar_per60[a]
}

# 将每架航班所在window的起降数bind到源数据上
dep_mar <- cbind(dep_origin_mar,
		 data.frame(dep_mar_15_per_dep),
		 data.frame(arr_mar_15_per_dep),
		 data.frame(dep_mar_60_per_dep),
		 data.frame(arr_mar_60_per_dep),
		 data.frame(pb_mar_15_per_dep),
		 data.frame(pb_mar_60_per_dep))
arr_mar <- cbind(arr_origin_mar,
		 data.frame(dep_mar_15_per_arr),
		 data.frame(arr_mar_15_per_arr),
		 data.frame(dep_mar_60_per_arr),
		 data.frame(arr_mar_60_per_arr),
		 data.frame(pb_mar_15_per_arr),
		 data.frame(pb_mar_60_per_arr))


################################################################################
# 输出数据
write.csv(dep_mar, "dep_mar_processed.csv", row.names = F)
write.csv(arr_mar, "arr_mar_processed.csv", row.names = F)


################################################################################

################################################################################

# 计算每个window有多少架航班
# apr
# 15 minutes
dep_count_apr_per15 <- matrix(0, nrow = 30 * 96, ncol = 1)
pb_count_apr_per15 <- matrix(0, nrow = 30 * 96, ncol = 1)
for (i in 1:length(dep_window_apr_per15)) {
	a <- dep_window_apr_per15[i]
	b <- pb_window_apr_per15[i]
	dep_count_apr_per15[a] <- dep_count_apr_per15[a] + 1
	pb_count_apr_per15[b] <- pb_count_apr_per15[b] + 1
}

arr_count_apr_per15 <- matrix(0, nrow = 30 * 96, ncol = 1)
for (i in 1:length(arr_window_apr_per15)) {
	a <- arr_window_apr_per15[i]
	arr_count_apr_per15[a] <- arr_count_apr_per15[a] + 1
}

# 60 minutes
dep_count_apr_per60 <- matrix(0, nrow = 30 * 24, ncol = 1)
pb_count_apr_per60 <- matrix(0, nrow = 30 * 24, ncol = 1)
for (i in 1:length(dep_window_apr_per60)) {
	a <- dep_window_apr_per60[i]
	b <- pb_window_apr_per60[i]
	dep_count_apr_per60[a] <- dep_count_apr_per60[a] + 1
	pb_count_apr_per60[b] <- pb_count_apr_per60[b] + 1
}

arr_count_apr_per60 <- matrix(0, nrow = 30 * 24, ncol = 1)
for (i in 1:length(arr_window_apr_per60)) {
	a <- arr_window_apr_per60[i]
	arr_count_apr_per60[a] <- arr_count_apr_per60[a] + 1
}

window_count_apr_per15 <- data.frame(dep_count_apr_per15, arr_count_apr_per15, pb_count_apr_per15)
window_count_apr_per60 <- data.frame(dep_count_apr_per60, arr_count_apr_per60, pb_count_apr_per60)
write.csv(window_count_apr_per15, "window_count_apr_per15.csv", row.names = F)
write.csv(window_count_apr_per60, "window_count_apr_per60.csv", row.names = F)

################################################################################

# 每架航班所在window的起降数（此处为全部起降航班计算得出的数字）
# 15 minutes
# 为起飞航班添加
dep_apr_15_per_dep <- vector(mode = "numeric", length = 0)
arr_apr_15_per_dep <- vector(mode = "numeric", length = 0)
pb_apr_15_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:length(dep_window_apr_per15)) {
	a <- dep_window_apr_per15[i]
	dep_apr_15_per_dep[i] <- dep_count_apr_per15[a]
	arr_apr_15_per_dep[i] <- arr_count_apr_per15[a]
	pb_apr_15_per_dep[i] <- pb_count_apr_per15[a]
}
# 为降落航班添加
dep_apr_15_per_arr <- vector(mode = "numeric", length = 0)
arr_apr_15_per_arr <- vector(mode = "numeric", length = 0)
pb_apr_15_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:length(arr_window_apr_per15)) {
	b <- arr_window_apr_per15[i]
	dep_apr_15_per_arr[i] <- dep_count_apr_per15[b]
	arr_apr_15_per_arr[i] <- arr_count_apr_per15[b]
	pb_apr_15_per_arr[i] <- pb_count_apr_per15[b]
}

# 60 minutes
# 为起飞航班添加
dep_apr_60_per_dep <- vector(mode = "numeric", length = 0)
arr_apr_60_per_dep <- vector(mode = "numeric", length = 0)
pb_apr_60_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:length(dep_window_apr_per60)) {
	a <- dep_window_apr_per60[i]
	dep_apr_60_per_dep[i] <- dep_count_apr_per60[a]
	arr_apr_60_per_dep[i] <- arr_count_apr_per60[a]
	pb_apr_60_per_dep[i] <- pb_count_apr_per60[a]
}
# 为降落航班添加
dep_apr_60_per_arr <- vector(mode = "numeric", length = 0)
arr_apr_60_per_arr <- vector(mode = "numeric", length = 0)
pb_apr_60_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:length(arr_window_apr_per60)) {
	b <- arr_window_apr_per60[i]
	dep_apr_60_per_arr[i] <- dep_count_apr_per60[b]
	arr_apr_60_per_arr[i] <- arr_count_apr_per60[b]
	pb_apr_60_per_arr[i] <- pb_count_apr_per60[a]
}

# 将每架航班所在window的起降数bind到源数据上
dep_apr <- cbind(dep_origin_apr,
		 data.frame(dep_apr_15_per_dep),
		 data.frame(arr_apr_15_per_dep),
		 data.frame(dep_apr_60_per_dep),
		 data.frame(arr_apr_60_per_dep),
		 data.frame(pb_apr_15_per_dep),
		 data.frame(pb_apr_60_per_dep))
arr_apr <- cbind(arr_origin_apr,
		 data.frame(dep_apr_15_per_arr),
		 data.frame(arr_apr_15_per_arr),
		 data.frame(dep_apr_60_per_arr),
		 data.frame(arr_apr_60_per_arr),
		 data.frame(pb_apr_15_per_arr),
		 data.frame(pb_apr_60_per_arr))


################################################################################
# 输出数据
write.csv(dep_apr, "dep_apr_processed.csv", row.names = F)
write.csv(arr_apr, "arr_apr_processed.csv", row.names = F)


################################################################################

################################################################################

# 计算每个window有多少架航班
# may
# 15 minutes
dep_count_may_per15 <- matrix(0, nrow = 31 * 96, ncol = 1)
pb_count_may_per15 <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(dep_window_may_per15)) {
	a <- dep_window_may_per15[i]
	b <- pb_window_may_per15[i]
	dep_count_may_per15[a] <- dep_count_may_per15[a] + 1
	pb_count_may_per15[b] <- pb_count_may_per15[b] + 1
}

arr_count_may_per15 <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(arr_window_may_per15)) {
	a <- arr_window_may_per15[i]
	arr_count_may_per15[a] <- arr_count_may_per15[a] + 1
}

# 60 minutes
dep_count_may_per60 <- matrix(0, nrow = 31 * 24, ncol = 1)
pb_count_may_per60 <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(dep_window_may_per60)) {
	a <- dep_window_may_per60[i]
	b <- pb_window_may_per60[i]
	dep_count_may_per60[a] <- dep_count_may_per60[a] + 1
	pb_count_may_per60[b] <- pb_count_may_per60[b] + 1
}

arr_count_may_per60 <- matrix(0, nrow = 31 * 24, ncol = 1)
for (i in 1:length(arr_window_may_per60)) {
	a <- arr_window_may_per60[i]
	arr_count_may_per60[a] <- arr_count_may_per60[a] + 1
}

window_count_may_per15 <- data.frame(dep_count_may_per15, arr_count_may_per15, pb_count_may_per15)
window_count_may_per60 <- data.frame(dep_count_may_per60, arr_count_may_per60, pb_count_may_per60)
write.csv(window_count_may_per15, "window_count_may_per15.csv", row.names = F)
write.csv(window_count_may_per60, "window_count_may_per60.csv", row.names = F)

################################################################################

# 每架航班所在window的起降数（此处为全部起降航班计算得出的数字）
# 15 minutes
# 为起飞航班添加
dep_may_15_per_dep <- vector(mode = "numeric", length = 0)
arr_may_15_per_dep <- vector(mode = "numeric", length = 0)
pb_may_15_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:length(dep_window_may_per15)) {
	a <- dep_window_may_per15[i]
	dep_may_15_per_dep[i] <- dep_count_may_per15[a]
	arr_may_15_per_dep[i] <- arr_count_may_per15[a]
	pb_may_15_per_dep[i] <- pb_count_may_per15[a]
}
# 为降落航班添加
dep_may_15_per_arr <- vector(mode = "numeric", length = 0)
arr_may_15_per_arr <- vector(mode = "numeric", length = 0)
pb_may_15_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:length(arr_window_may_per15)) {
	b <- arr_window_may_per15[i]
	dep_may_15_per_arr[i] <- dep_count_may_per15[b]
	arr_may_15_per_arr[i] <- arr_count_may_per15[b]
	pb_may_15_per_arr[i] <- pb_count_may_per15[b]
}

# 60 minutes
# 为起飞航班添加
dep_may_60_per_dep <- vector(mode = "numeric", length = 0)
arr_may_60_per_dep <- vector(mode = "numeric", length = 0)
pb_may_60_per_dep <- vector(mode = "numeric", length = 0)
for (i in 1:length(dep_window_may_per60)) {
	a <- dep_window_may_per60[i]
	dep_may_60_per_dep[i] <- dep_count_may_per60[a]
	arr_may_60_per_dep[i] <- arr_count_may_per60[a]
	pb_may_60_per_dep[i] <- pb_count_may_per60[a]
}
# 为降落航班添加
dep_may_60_per_arr <- vector(mode = "numeric", length = 0)
arr_may_60_per_arr <- vector(mode = "numeric", length = 0)
pb_may_60_per_arr <- vector(mode = "numeric", length = 0)
for (i in 1:length(arr_window_may_per60)) {
	b <- arr_window_may_per60[i]
	dep_may_60_per_arr[i] <- dep_count_may_per60[b]
	arr_may_60_per_arr[i] <- arr_count_may_per60[b]
	pb_may_60_per_arr[i] <- pb_count_may_per60[a]
}

# 将每架航班所在window的起降数bind到源数据上
dep_may <- cbind(dep_origin_may,
		 data.frame(dep_may_15_per_dep),
		 data.frame(arr_may_15_per_dep),
		 data.frame(dep_may_60_per_dep),
		 data.frame(arr_may_60_per_dep),
		 data.frame(pb_may_15_per_dep),
		 data.frame(pb_may_60_per_dep))
arr_may <- cbind(arr_origin_may,
		 data.frame(dep_may_15_per_arr),
		 data.frame(arr_may_15_per_arr),
		 data.frame(dep_may_60_per_arr),
		 data.frame(arr_may_60_per_arr),
		 data.frame(pb_may_15_per_arr),
		 data.frame(pb_may_60_per_arr))


################################################################################
# 输出数据
write.csv(dep_may, "dep_may_processed.csv", row.names = F)
write.csv(arr_may, "arr_may_processed.csv", row.names = F)


################################################################################