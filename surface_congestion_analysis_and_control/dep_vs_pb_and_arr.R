# Departure through as a function of departure demand and arrival throughput
# dep_count作为因变量，pb_count和arr_count作为自变量
# 即在不同arr的情况下pb和dep的关系&在不同pb的情况下arr和dep的关系

# 设置路径
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/surface_congestion_analysis_and_control")

# read origin data
window_count_per15 <- read.csv("window_count_feb2may.csv")
# 不同arr下的子集，arr最大值为25
# 用循环的方式，

window_sub_0arr <- subset(window_count_per15, arr_count_per15 == 0)

# 0arr
pb_max_per15_0arr <- max(window_sub_0arr$pb_count_per15)
dep_max_per15_0arr <- max(window_sub_0arr$dep_count_per15)
arr_max_per15_0arr <- max(window_sub_0arr$arr_count_per15)
# 做曲线前准备的数据
dep_mean_0arr <- vector(mode = "numeric", length = pb_max_per15_0arr)
dep_median_0arr <- vector(mode = "numeric", length = pb_max_per15_0arr)
dep_max_0arr <- vector(mode = "numeric", length = pb_max_per15_0arr)
dep_min_0arr <- vector(mode = "numeric", length = pb_max_per15_0arr)
dep_sd_0arr <- vector(mode = "numeric", length = pb_max_per15_0arr)
for (i in 1:pb_max_per15_0arr) {
	dep_mean_0arr[i] <- mean(as.matrix(subset(window_sub_0arr,
		pb_count_per15 == i, select = 1)))
	dep_median_0arr[i] <- mean(as.matrix(subset(window_sub_0arr,
		pb_count_per15 == i, select = 1)))
	dep_max_0arr[i] <- mean(as.matrix(subset(window_sub_0arr,
		pb_count_per15 == i, select = 1)))
	dep_min_0arr[i] <- mean(as.matrix(subset(window_sub_0arr,
		pb_count_per15 == i, select = 1)))
	dep_sd_0arr[i] <- mean(as.matrix(subset(window_sub_0arr,
		pb_count_per15 == i, select = 1)))
}
# 做点图及误差线
x <- c(1:27)
# 同一推出率下起飞率的平均数、中位数、标准差
# 只取推出1至24架的情况
y1 <- floor(dep_mean_0arr[c(1:27)])
y2 <- floor(dep_median_0arr[c(1:27)])
err_sd <- floor(dep_sd_0arr[c(1:27)])
# 做图(平均数图）
# 平均数（实心三角）
plot(x, y1, lty = 1, cex = 0)
################################################################################
# 回归分析
# 2次
fit_mean_2_0arr <- lm(y1 ~ x + I(x ^ 2))
lines(x, fitted(fit_mean_2_0arr))

################################################################################
# 1-25 arr
for (j in 1:15) {
	window_sub <- subset(window_count_per15, arr_count_per15 == j)
	pb_max_per15 <- max(window_sub$pb_count_per15)
	dep_max_per15 <- max(window_sub$dep_count_per15)
	arr_max_per15 <- max(window_sub$arr_count_per15)
	dep_mean <- vector(mode = "numeric", length = pb_max_per15)
	dep_median <- vector(mode = "numeric", length = pb_max_per15)
	dep_max <- vector(mode = "numeric", length = pb_max_per15)
	dep_min <- vector(mode = "numeric", length = pb_max_per15)
	dep_sd <- vector(mode = "numeric", length = pb_max_per15)
	for (i in 1:pb_max_per15) {
		dep_mean[i] <- mean(as.matrix(subset(window_sub,
			pb_count_per15 == i, select = 1)))
		dep_median[i] <- mean(as.matrix(subset(window_sub,
			pb_count_per15 == i, select = 1)))
		dep_max[i] <- mean(as.matrix(subset(window_sub,
			pb_count_per15 == i, select = 1)))
		dep_min[i] <- mean(as.matrix(subset(window_sub,
			pb_count_per15 == i, select = 1)))
		dep_sd[i] <- mean(as.matrix(subset(window_sub,
			pb_count_per15 == i, select = 1)))
	}
	x <- c(1:length(dep_mean))
	# 同一推出率下起飞率的平均数、中位数、标准差
	y_mean <- floor(dep_mean[c(1:length(dep_mean))])
	x <- x[is.na(y_mean) == F]
	y_mean <- na.omit(y_mean)
################################################################################
	# 回归分析
	# 2次
	fit_mean_2 <- lm(y_mean ~ x + I(x ^ 2))
	lines(x, fitted(fit_mean_2))
}
