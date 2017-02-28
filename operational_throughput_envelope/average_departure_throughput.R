setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/operational_throughput_envelope")
# paper chapter 4
# dep_rate~dep_demand关系图、误差线、拟合曲线
source("deal_with_data.R")
library(ggplot2)
library(reshape2)
library(grid)
demand_statistical_data_long <- melt(demand_statistical_data, id = "dep_demand")

# plot 1 
# 均值图（mean plot）
# 底层赋值给变量
dep_mean_plot <- ggplot(demand_statistical_data_long,
			aes(x = dep_demand, y = value,
			shape = variable,
			colour = variable))
# 添加图层
# 绘制errorbar、二次拟合曲线
plot1 <- dep_mean_plot +
	geom_errorbar(data = demand_statistical_data_long,
		aes(ymin = value - demand_sd,
			ymax = value + demand_sd),
			size = 1.1, width = .7, alpha = .5) +
	geom_point(size = 3.5, alpha = .8) + 
	labs(x = "15分钟场面离场航班数", y = "15分钟起飞率", 
		title = "离场航班数-起飞率变化图(a)", size = 2) +
	scale_x_discrete(limits = c(0, 60)) +
	scale_x_continuous(limits = c(0, 60), breaks = c(0, 5, 10, 15, 20, 25,
		30, 35, 40, 45, 50, 55, 60),
		labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) +
	scale_colour_discrete(name = "统计量", labels = c("平均数", "中位数")) +
	scale_shape_discrete(name = "统计量", labels = c("平均数", "中位数")) +
	scale_linetype_discrete(name = "统计量", labels = c("平均数", "中位数"))


# plot 2
# mean和median的拟合曲线
demand_regression_plot <- ggplot(demand_statistical_data_long,
				aes(x = dep_demand, y = value,
				colour = variable,
				shape = variable,
				linetype = variable))
plot2 <- demand_regression_plot +
	geom_point(size = 2) +
	geom_smooth(method = 'lm', formula = y ~ poly(x, 2),
			size = 1.2, alpha = .1) +
	labs(x = "15分钟场面离场航班数", y = "15分钟起飞率", title = "(b)拟合曲线",
		shape = "11", colour = "11", size = 2) +
	scale_x_continuous(limits = c(0, 60), breaks = c(0, 5, 10, 15, 20, 25,
		30, 35, 40, 45, 50, 55, 60),
		labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) +
# 由于颜色、点形状、线性均改变了，所以改变图例时也要将他们同时修改
	scale_colour_discrete(name = "统计量", 
		labels = c("平均数拟合线", "中位数拟合线")) +
	scale_shape_discrete(name = "统计量", 
		labels = c("平均数拟合线", "中位数拟合线")) +
	scale_linetype_discrete(name = "统计量", 
		labels = c("平均数拟合线", "中位数拟合线"))

# 一页多图设置
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(1, 2))

# 将dep_mean和dep_median中的NA值变为前一个值
demand_mean[is.na(demand_mean)] <- demand_mean[as.numeric(which(is.na(demand_mean)) - 1)]
demand_median[is.na(demand_median)] <- demand_median[as.numeric(which(is.na(demand_median)) - 1)]

# 回归分析
fit_mean <- lm(demand_mean ~ dep_demand + I(dep_demand ^ 2))
summary(fit_mean)
fit_median <- lm(demand_median ~ dep_demand + I(dep_demand ^ 2))
summary(fit_median)

qqPlot(fit_mean, id.method = "identify", simulate = TRUE, main = "qqplot")

###############################################################################
# dep_rate ~ (dep_demand, arr_rate)

# read origin data
window_count_per15 <- read.csv("window_count_feb2may.csv")
# 不同arr下的子集，arr最大值为25
# 用循环的方式，

window_sub_0arr <- subset(window_count_per15, arr_count_per15 == 0)

# 0arr
demand_max_per15_0arr <- max(window_sub_0arr$dep_demand_per15)
dep_max_per15_0arr <- max(window_sub_0arr$dep_count_per15)
arr_max_per15_0arr <- max(window_sub_0arr$arr_count_per15)
# 做曲线前准备的数据
dep_mean_0arr <- vector(mode = "numeric", length = demand_max_per15_0arr)
dep_median_0arr <- vector(mode = "numeric", length = demand_max_per15_0arr)
dep_max_0arr <- vector(mode = "numeric", length = demand_max_per15_0arr)
dep_min_0arr <- vector(mode = "numeric", length = demand_max_per15_0arr)
dep_sd_0arr <- vector(mode = "numeric", length = demand_max_per15_0arr)
for (i in 1:demand_max_per15_0arr) {
	dep_mean_0arr[i] <- mean(as.matrix(subset(window_sub_0arr,
		dep_demand_per15 == i, select = 1)))
	dep_median_0arr[i] <- median(as.matrix(subset(window_sub_0arr,
		dep_demand_per15 == i, select = 1)))
	dep_max_0arr[i] <- max(as.matrix(subset(window_sub_0arr,
		dep_demand_per15 == i, select = 1)))
	dep_min_0arr[i] <- min(as.matrix(subset(window_sub_0arr,
		dep_demand_per15 == i, select = 1)))
	dep_sd_0arr[i] <- sd(as.matrix(subset(window_sub_0arr,
		dep_demand_per15 == i, select = 1)))
}
# 做点图及误差线
x <- c(1:61)
# 同一推出率下起飞率的平均数、中位数、标准差
# 只取推出1至24架的情况
y1 <- dep_mean_0arr[c(1:61)]
# 去除y1中的na值，同时去除相应的x值
x_y1 <- x[-which(is.na(y1))]
y1 <- y1[-which(is.na(y1))]
y2 <- dep_median_0arr[c(1:61)]
err_sd <- dep_sd_0arr[c(1:61)]
# 做图(平均数图）
# 平均数（实心三角）
plot(x_y1, y1, lty = 1, cex = 1)
################################################################################
# 回归分析
# 2次拟合
fit_mean_2_0arr <- lm(y1 ~ x_y1 + I(x_y1 ^ 2))
lines(x_y1, fitted(fit_mean_2_0arr))

###############################################################################
# different arr
# 1-25 arr
window_count_per15 <- read.csv("window_count_feb2may.csv")
plot(x_y1, y1, lty = 1, cex = 0, xlab = "", ylab = "")
grid()
title(xlab = "离场场面航班数（架次/15分钟）", ylab = "起飞率平均数（架次/15分钟）")
for (j in c(1, 3, 5, 7, 9, 10, 11, 13, 15, 17, 19, 20)) {
	window_sub <- subset(window_count_per15, arr_count_per15 == j)
	demand_max_per15 <- max(window_sub$dep_demand_per15)
	dep_max_per15 <- max(window_sub$dep_count_per15)
	arr_max_per15 <- max(window_sub$arr_count_per15)
	dep_mean <- vector(mode = "numeric", length = demand_max_per15)
	dep_median <- vector(mode = "numeric", length = demand_max_per15)
	dep_max <- vector(mode = "numeric", length = demand_max_per15)
	dep_min <- vector(mode = "numeric", length = demand_max_per15)
	dep_sd <- vector(mode = "numeric", length = demand_max_per15)
	for (i in 1:demand_max_per15) {
		dep_mean[i] <- mean(as.matrix(subset(window_sub,
			dep_demand_per15 == i, select = 1)))
		dep_median[i] <- median(as.matrix(subset(window_sub,
			dep_demand_per15 == i, select = 1)))
		dep_max[i] <- max(as.matrix(subset(window_sub,
			dep_demand_per15 == i, select = 1)))
		dep_min[i] <- min(as.matrix(subset(window_sub,
			dep_demand_per15 == i, select = 1)))
		dep_sd[i] <- sd(as.matrix(subset(window_sub,
			dep_demand_per15 == i, select = 1)))
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
	lines(x, fitted(fit_mean_2), lwd = 1.8)
}

# different demand 
window_count_per15 <- read.csv("window_count_feb2may.csv")
plot(y1, x_y1, lty = 1, cex = 0, xlab = "", ylab = "",
	xlim = c(0, 20), ylim = c(0, 30))
grid()
title(xlab = "接收率（架次/15分钟）", ylab = "起飞率平均数（架次/15分钟）")
for (j in c(1, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) {
	window_sub <- subset(window_count_per15, dep_demand_per15 == j)
	demand_max_per15 <- max(window_sub$dep_demand_per15)
	dep_max_per15 <- max(window_sub$dep_count_per15)
	arr_max_per15 <- max(window_sub$arr_count_per15)
	dep_mean <- vector(mode = "numeric", length = arr_max_per15)
	dep_median <- vector(mode = "numeric", length = arr_max_per15)
	dep_max <- vector(mode = "numeric", length = arr_max_per15)
	dep_min <- vector(mode = "numeric", length = arr_max_per15)
	dep_sd <- vector(mode = "numeric", length = arr_max_per15)
	for (i in 1:arr_max_per15) {
		dep_mean[i] <- mean(as.matrix(subset(window_sub,
			arr_count_per15 == i, select = 1)))
		dep_median[i] <- median(as.matrix(subset(window_sub,
			arr_count_per15 == i, select = 1)))
		dep_max[i] <- max(as.matrix(subset(window_sub,
			arr_count_per15 == i, select = 1)))
		dep_min[i] <- min(as.matrix(subset(window_sub,
			arr_count_per15 == i, select = 1)))
		dep_sd[i] <- sd(as.matrix(subset(window_sub,
			arr_count_per15 == i, select = 1)))
	}
	x <- c(1:length(dep_mean))
	# 同一推出率下起飞率的平均数、中位数、标准差
	y_mean <- floor(dep_mean[c(1:length(dep_mean))])
	x <- x[is.na(y_mean) == F]
	y_mean <- na.omit(y_mean)
	################################################################################
	# 回归分析
	# 2次
	fit_mean_3 <- lm(y_mean ~ x + I(x ^ 2))
	lines(x, fitted(fit_mean_3), lwd = 1.8)
}
# not 