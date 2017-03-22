# 小时流量分布
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/surface_congestion_analysis_and_control")
window_count_feb <- read.csv("window_count_feb_per60.csv")
window_count_mar <- read.csv("window_count_mar_per60.csv")
window_count_apr <- read.csv("window_count_apr_per60.csv")
window_count_may <- read.csv("window_count_may_per60.csv")
names(window_count_feb) <- c("dep_count", "arr_count", "pb_count")
names(window_count_mar) <- c("dep_count", "arr_count", "pb_count")
names(window_count_apr) <- c("dep_count", "arr_count", "pb_count")
names(window_count_may) <- c("dep_count", "arr_count", "pb_count")
window_count_per60 <- rbind(window_count_feb, window_count_mar,
			window_count_apr, window_count_may)
time_seq <- strptime("2014-02-01 00:00:00", "%Y-%m-%d %H:%M:%S") + 3600 * 0:2879
window_count_per60 <- cbind(time_seq, window_count_per60)
rwy_flow <- window_count_per60$dep_count + window_count_per60$arr_count
pek_flow_per_hour <- data.frame(time_seq, rwy_flow)

library(ggplot2)
# 分布直方图
rwy_flow_hist <- pek_flow_per_hour$rwy_flow
# 剔除4月1日（数据为空）
rwy_flow_hist <- rwy_flow_hist[-c(1417:1440)]
time_seq <- time_seq[-c(1417:1440)]
hour_seq <- rep(c(0:23), 119)
rwy_flow_plot <- ggplot(NULL, aes(x = rwy_flow_hist, y = ..density..))
win.graph(width = 10, height = 5)
rwy_flow_plot +
geom_histogram(binwidth = 1) +
geom_density(size = 1.2, alpha = .6) +
labs(x = "小时流量（架次）", y = "概率密度", size = 20) + 
geom_vline(aes(xintercept = 88), size = 1.2, colour = "red", 
	linetype = "dashed") +
theme_bw()

# 各时刻航班数均值折线图
timeHM_formatter <- function(x) {
	h <- floor(x / 60)
	m <- floor(x %% 60)
	lab <- sprintf("%02d:%02d", h, m) # Format the strings as HH:MM:SS
	return(lab)
}
hourly_flow_mean <- ggplot(NULL, aes(x = hour_seq, y = rwy_flow_hist))
win.graph(width = 10, height = 5)
hourly_flow_mean +
scale_x_continuous(breaks = seq(from = 0, to = 23, by = 1), 
		labels = c(timeHM_formatter(seq(from = 0, to = 23, by = 1) * 60))) +
stat_summary(fun.y = "mean", geom = "line", size = 1.5, colour = "red") +
labs(x = "时刻", y = "平均流量(架次)") +
theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25)) +
theme_bw() 

