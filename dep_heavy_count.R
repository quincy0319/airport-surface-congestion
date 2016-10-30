# dep heavy count 
# 每个window起飞的重型机
heavy_origin <- read.csv("dep_heavy_ac.csv")
heavy_feb <- subset(heavy_origin, mission_month == 2)
heavy_mar <- subset(heavy_origin, mission_month == 3)
heavy_apr <- subset(heavy_origin, mission_month == 4)
heavy_may <- subset(heavy_origin, mission_month == 5)

# 计算每个航班所属的window
# feb 15 minutes
attach(heavy_feb)
heavy_window_feb_per15 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(heavy_feb)

# mar 15 minutes
attach(heavy_mar)
heavy_window_mar_per15 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(heavy_mar) 

# apr 15 minutes
attach(heavy_apr)
heavy_window_apr_per15 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(heavy_apr) 

# may 15 minutes
attach(heavy_may)
heavy_window_may_per15 <- (mission_date - 1) * 96 + floor(time_takeoff_real / 15) + 1
detach(heavy_may)

################################################################################

# 计算每个window有多少架航班
# feb
# 15 minutes
heavy_count_feb_per15 <- matrix(0, nrow = 28 * 96, ncol = 1)
for (i in 1:length(heavy_window_feb_per15)) {
	a <- heavy_window_feb_per15[i]
	heavy_count_feb_per15[a] <- heavy_count_feb_per15[a] + 1
}

# mar
# 15 minutes
heavy_count_mar_per15 <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(heavy_window_mar_per15)) {
	a <- heavy_window_mar_per15[i]
	heavy_count_mar_per15[a] <- heavy_count_mar_per15[a] + 1
}

# apr
# 15 minutes
heavy_count_apr_per15 <- matrix(0, nrow = 30 * 96, ncol = 1)
for (i in 1:length(heavy_window_apr_per15)) {
	a <- heavy_window_apr_per15[i]
	heavy_count_apr_per15[a] <- heavy_count_apr_per15[a] + 1
}

# may
# 15 minutes
heavy_count_may_per15 <- matrix(0, nrow = 31 * 96, ncol = 1)
for (i in 1:length(heavy_window_may_per15)) {
	a <- heavy_window_may_per15[i]
	heavy_count_may_per15[a] <- heavy_count_may_per15[a] + 1
}

