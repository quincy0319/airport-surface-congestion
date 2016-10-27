# Variance of departure throughput
# 通过箱线图观察偏差值(all arrivals	和10 arrivals）
# 表明除了推出率和到达率之外还有其他的影响因素

setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/
	operational_throughput_envelope")
window_count_per15 <- read.csv("window_count_feb2may.csv")
par(mfrow = c(2, 1))
# 绘制箱线图
boxplot(window_count_per15$dep_count_per15 ~ window_count_per15$pb_count_per15,
	main = "All Arrivals")
# 取子集，绘制箱线图
window_count_per15_10arr <- subset(window_count_per15, arr_count_per15 == 10)
boxplot(window_count_per15_10arr$dep_count_per15 ~ 
	window_count_per15_10arr$pb_count_per15, main = "10 Arrivals")
