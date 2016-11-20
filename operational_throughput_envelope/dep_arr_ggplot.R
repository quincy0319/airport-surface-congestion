# 起降散点图，点的大小与出现概率成正比
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/operational_throughput_envelope")
# 四个月所有15分钟窗口的数据
window_count <- read.csv("window_count_feb2may.csv")
window_count_6dep <- subset(window_count, dep_count_per15 > 6)
library(ggplot2)
dep_arr_plot <- ggplot(data = window_count_6dep, 
			aes(x = arr_count_per15, y = dep_count_per15))
dep_arr_plot + 
	geom_point(colour = "darkred", stat = "sum", alpha = .6) +
	labs(x = "15分钟降落率", y = "15分钟起飞率", title = "起飞-降落分布图") +
	scale_y_continuous(limits = c(0, 30),
		breaks = seq(from = 0, to = 30, by = 5))

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

