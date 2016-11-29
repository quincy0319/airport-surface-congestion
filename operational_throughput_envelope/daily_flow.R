# 小时流量分布
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/operational_throughput_envelope")
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
rwy_flow_hist <- pek_flow_per_hour$rwy_flow
# 剔除4月1日
rwy_flow_hist <- rwy_flow_hist[ - c(1417:1440)]
time_seq <- time_seq[ - c(1417:1440)]
rwy_flow_plot <- ggplot(NULL, aes(x = rwy_flow_hist, y = ..density..))
rwy_flow_plot +
geom_histogram(binwidth = 1) +
geom_density(size = 1.2) +
labs(x = "rwy flow") + 
geom_vline(aes(xintercept = 88), size = 1.2, colour = "red", 
	linetype = "dashed")
