#起降散点图 ，点的大小与出现概率成正比
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/operational_throughput_envelope")
# 四个月所有15分钟窗口的数据
# 选取起飞大于6架次的时刻用来做average throughput envelope
window_count <- read.csv("window_count_feb2may.csv")
window_count_6dep <- subset(window_count, dep_count_per15 > 6)

dep_mean <- vector(mode = "numeric", length = 0)
dep_sd <- vector(mode = "numeric", length = 0)
for (i in 1:max(window_count$arr_count_per15)) {
	dep_flight <- subset(window_count, arr_count_per15 == i,
			select = 1)
	dep_mean[i] <- mean(as.matrix(dep_flight))
	dep_sd[i] <- sd(as.matrix(dep_flight))
}
upper <- dep_mean[1:20] + dep_sd[1:20]
lower <- dep_mean[1:20] - dep_sd[1:20]
library(ggplot2)
dep_arr_plot <- ggplot(data = window_count_6dep,
			aes(x = arr_count_per15, y = dep_count_per15))
dep_arr_plot +
	geom_point(colour = "black", stat = "sum", alpha = .3) +
# 均值、中位数连线的做图：
	stat_summary(fun.y = "mean", geom = "line",
	colour = "red", size = 1.2, linetype = "dashed") +
	stat_summary(fun.y = "median", geom = "line",
	colour = "blue", size = 1.2, linetype = "solid") +
	labs(x = "15分钟降落率", y = "15分钟起飞率", title = "起飞-降落分布图") +
	scale_x_continuous(limits = c(0, 20)) +
	scale_y_continuous(limits = c(0, 30),
		breaks = seq(from = 0, to = 30, by = 5)) +
	theme_bw() 






################################################################################
# 分跑道起降流量对比图
rwy_flow_60 <- read.csv("rwy_flow_per60.csv")
# rwy 36l flow in 60 mins interval
rwy_flow_36l_60 <- subset(rwy_flow_60, dep36l > 0 & arr36l > 0)
plot_36l_60 <- ggplot(data = rwy_flow_36l_60, aes(x = arr36l, y = dep36l))
plot_36l <- plot_36l_60 +
	geom_point(colour = "blue", stat = "sum", alpha = .6)

# rwy 36r flow in 60 mins interval
rwy_flow_36r_60 <- subset(rwy_flow_60, dep36r > 0 & arr36r > 0)
plot_36r_60 <- ggplot(data = rwy_flow_36r_60, aes(x = arr36r, y = dep36r))
plot_36r <- plot_36r_60 +
	geom_point(colour = "blue", stat = "sum", alpha = .6)

# rwy 01 flow in 60 mins interval
rwy_flow_60 <- read.csv("rwy_flow_per60.csv")
rwy_flow_01_60 <- subset(rwy_flow_60, dep01 > 0 & arr01 > 0)
plot_01_60 <- ggplot(data = rwy_flow_01_60, aes(x = arr01, y = dep01))
plot_01 <- plot_01_60 +
	geom_point(colour = "blue", stat = "sum", alpha = .6)

