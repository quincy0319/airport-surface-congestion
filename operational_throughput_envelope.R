# 推出率-起飞率曲线
# 读取2-5月数据
dep_feb <- read.csv("dep_feb_processed.csv")
arr_feb <- read.csv("arr_feb_processed.csv")
dep_mar <- read.csv("dep_mar_processed.csv")
arr_mar <- read.csv("dep_mar_processed.csv")
dep_apr <- read.csv("dep_apr_processed.csv")
arr_apr <- read.csv("dep_apr_processed.csv")
dep_may <- read.csv("dep_may_processed.csv")
arr_may <- read.csv("dep_may_processed.csv")

# 将五个月输入放入一张表中
# dep
names(dep_feb)[17:22] <- c("dep_15_per_dep", "arr_15_per_dep", "dep_60_per_dep",
	"arr_60_per_dep", "pb_15_per_dep", "pb_60_per_dep")
names(dep_mar)[17:22] <- c("dep_15_per_dep", "arr_15_per_dep", "dep_60_per_dep",
	"arr_60_per_dep", "pb_15_per_dep", "pb_60_per_dep")
names(dep_apr)[17:22] <- c("dep_15_per_dep", "arr_15_per_dep", "dep_60_per_dep",
	"arr_60_per_dep", "pb_15_per_dep", "pb_60_per_dep")
names(dep_may)[17:22] <- c("dep_15_per_dep", "arr_15_per_dep", "dep_60_per_dep",
	"arr_60_per_dep", "pb_15_per_dep", "pb_60_per_dep")
dep_full <- rbind(dep_feb, dep_mar, dep_apr, dep_may)


# 15分钟推出率、起飞率、接收率最大值
window_count_feb_per15 <- read.csv("window_count_feb_per15.csv")
window_count_mar_per15 <- read.csv("window_count_mar_per15.csv")
window_count_apr_per15 <- read.csv("window_count_apr_per15.csv")
window_count_may_per15 <- read.csv("window_count_may_per15.csv")
# 将window_count结合成一个表
names(window_count_feb_per15) <- c("dep_count_per15", "arr_count_per15", "pb_count_per15")
names(window_count_mar_per15) <- c("dep_count_per15", "arr_count_per15", "pb_count_per15")
names(window_count_apr_per15) <- c("dep_count_per15", "arr_count_per15", "pb_count_per15")
names(window_count_may_per15) <- c("dep_count_per15", "arr_count_per15", "pb_count_per15")
window_count_per15 <- rbind(window_count_feb_per15, window_count_mar_per15,
	window_count_apr_per15, window_count_may_per15)

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
x <- c(1:27)
# 同一推出率下起飞率的平均数、中位数、标准差
# 只取推出1至24架的情况
y1 <- floor(dep_mean[c(1:27)])
y2 <- floor(dep_median[c(1:27)])
err_sd <- floor(dep_sd[c(1:27)])
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
# 观察回归效果
summary(fit_mean_1)
summary(fit_mean_2)
summary(fit_median_1)
summary(fit_median_2)
