# 推出率-起飞率曲线
# 以下均为2月份的数据
dep_feb <- read.csv("dep_feb_processed.csv")
arr_feb <- read.csv("arr_feb_processed.csv")

# 15分钟推出率、起飞率、接收率最大值
window_count_per15 <- read.csv("window_count_per15.csv")
pb_max_per15 <- max(window_count_per15$pb_count_per15)
dep_max_per15 <- max(window_count_per15$dep_count_per15)
arr_max_per15 <- max(window_count_per15$arr_count_per15)

# 做曲线前准备的数据
dep_mean <- vector(mode = "numeric", length = pb_max_per15)
dep_median <- vector(mode = "numeric", length = pb_max_per15)
dep_max <- vector(mode = "numeric", length = pb_max_per15)
dep_min <- vector(mode = "numeric", length = pb_max_per15)
dep_sd <- vector(mode = "numeric", length = pb_max_per15)
for (i in 1:pb_max_per15) {
	dep_mean[i] <- mean(as.matrix(subset(window_count_per15, 
		pb_count_per15 == i, select = 1)))
	dep_median[i] <- median(as.matrix(subset(window_count_per15, 
		pb_count_per15 == i, select = 1)))
	dep_max[i] <- max(as.matrix(subset(window_count_per15, 
		pb_count_per15 == i, select = 1)))
	dep_min[i] <- min(as.matrix(subset(window_count_per15, 
		pb_count_per15 == i, select = 1)))
	dep_sd[i] <- sd(as.matrix(subset(window_count_per15, 
		pb_count_per15 == i, select = 1)))
}

# 做点图及误差线
x <- c(1:24)
# 同一推出率下起飞率的平均数、中位数、标准差
# 只取推出1至24架的情况
y1 <- floor(dep_mean[c(1:24)])
y2 <- floor(dep_median[c(1:24)])
err_sd <- floor(dep_sd[c(1:24)])
# 调用errorbar函数
# 平均数（实心三角）
library(pracma)
errorbar(x, y1, yerr = err_sd, grid = TRUE, pch = 17, lty = 1)
# 将中位数（实心方形）绘制到图上
points(x, y2, pch = 15, lty = 1)

########################################################

# 回归分析
# 1次
fit_mean_1 <- lm(y1 ~ x)
fit_median_1 <- lm(y2 ~ x)
# 2次
fit_mean_2 <- lm(y1 ~ x + I(x ^ 2))
fit_median_2 <- lm(y2 ~ x + I(x ^ 2))
