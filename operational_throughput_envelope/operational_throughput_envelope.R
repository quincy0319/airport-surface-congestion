# operational throughput envelope
# x为降y为起，做拟合图、散点图
# read data
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/operational_throughput_envelope")
window_count_per15 <- read.csv("window_count_feb2may.csv")

# 计算每个数据点出现的次数
fre <- matrix(0, nrow = 30, ncol = 30)
for (i in 1:(dim(window_count_per15)[1])) {
	a <- window_count_per15[i, 1]
	b <- window_count_per15[i, 2]
	fre[a + 1 , b + 1] <- fre[a + 1, b + 1] + 1
}
prob <- fre / (dim(window_count_per15)[1])
# 计算点出现的频率，为做图做准备
node_prob <- vector(mode = "numeric", length = 0)
attach(window_count_per15)
for (j in 1:(dim(window_count_per15)[1])) {
	x1 <- arr_count_per15[j]
	y1 <- dep_count_per15[j]
	node_prob[j] <- prob[x1 + 1, y1 + 1]
}
detach(window_count_per15)
node_prob <- node_prob * 100
window_count_per15 <- cbind(window_count_per15, node_prob )
# 取起降和大于10的点
window_count_upper10 <- subset(window_count_per15,
	window_count_per15$dep_count_per15 +
	window_count_per15$arr_count_per15 >= 10)
# 计算1:max(arr)的dep平均数和中位数
attach(window_count_per15)
arr_max <- max(arr_count_per15)
dep_mean <- vector(mode = "numeric", length = arr_max)
dep_median <- vector(mode = "numeric", length = arr_max)
dep_max <- vector(mode = "numeric", length = arr_max)
dep_min <- vector(mode = "numeric", length = arr_max)
dep_sd <- vector(mode = "numeric", length = arr_max)
detach(window_count_per15)

for (k in 1:arr_max) {
	dep_mean[k] <- floor(mean(as.matrix(subset(window_count_upper10,
		arr_count_per15 == k, select = 1))))
	dep_median[k] <- floor(mean(as.matrix(subset(window_count_upper10,
		arr_count_per15 == k, select = 1))))
	dep_max[k] <- mean(as.matrix(subset(window_count_upper10,
		arr_count_per15 == k, select = 1)))
	dep_min[k] <- mean(as.matrix(subset(window_count_upper10,
		arr_count_per15 == k, select = 1)))
	dep_sd[k] <- mean(as.matrix(subset(window_count_upper10,
		arr_count_per15 == k, select = 1)))
}

################################################################################
# 做图
# 使用ggplot2包进行画图，点的大小表示出现频率的高低
library(ggplot2)
ggplot(window_count_upper10, aes(x = window_count_upper10$arr_count_per15, 
	y = window_count_upper10$dep_count_per15, 
	size = window_count_upper10$node_prob)) +
geom_point()

# 图1
# 使用高密度散点图
smoothScatter(window_count_upper10$arr_count_per15, window_count_upper10$dep_count_per15)
# 叠加mean、median、errorbar
xl <- c(1:18)
library(pracma)
errorbar(xl, dep_mean[1:18], yerr = dep_sd[1:18], pch = 15, cex = 1.2, add = TRUE)
points(xl, dep_mean[1:18], pch = 15, cex = 0.9)
lines(xl, dep_mean[1:18])

# 图2
dev.new()
smoothScatter(window_count_upper10$arr_count_per15, window_count_upper10$dep_count_per15)
y_fit <- dep_mean[1:18]
x_fit <- c(1:18)
lines(x_fit, fitted(lm(y_fit ~ x_fit)))