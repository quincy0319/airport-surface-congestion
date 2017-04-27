# 基于地面航班数的畅通滑行时间计算
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/surface_congestion_analysis_and_control")
adjusted_traffic <- read.csv("adjusted_traffic.csv")
dep_processed <- read.csv("dep_processed_feb2may.csv")

dep_processed <- data.frame(dep_processed, adjusted_traffic)
###############################################################################
###############################################################################
###############################################################################
# plot adj_traffic vs dep_taxi 
# rwy 36L
adj_vs_taxi_36l <- subset(dep_processed, rwy == "36L", select = c(25, 14, 6))
names(adj_vs_taxi_36l) <- c("adjusted_traffic", "dep_taxi", "ramp")
# 作图前准备
adj_max <- max(adj_vs_taxi_36l$adjusted_traffic)
taxi_mean <- vector(mode = "numeric", length = adj_max)
taxi_median <- vector(mode = "numeric", length = adj_max)
taxi_max <- vector(mode = "numeric", length = adj_max)
taxi_min <- vector(mode = "numeric", length = adj_max)
taxi_sd <- vector(mode = "numeric", length = adj_max)

for (i in 1:adj_max) {
	taxi_mean[i] <- mean(as.matrix(subset(adj_vs_taxi_36l,
		adjusted_traffic == i, select = 2)))
	taxi_median[i] <- median(as.matrix(subset(adj_vs_taxi_36l,
		adjusted_traffic == i, select = 2)))
	taxi_max[i] <- max(as.matrix(subset(adj_vs_taxi_36l,
		adjusted_traffic == i, select = 2)))
	taxi_min[i] <- min(as.matrix(subset(adj_vs_taxi_36l,
		adjusted_traffic == i, select = 2)))
	taxi_sd[i] <- sd(as.matrix(subset(adj_vs_taxi_36l,
		adjusted_traffic == i, select = 2)))
}
adj_taxi_num <- c(1:max(adj_max))
# mean regression fit data
adj_statistical_data <- data.frame(taxi_mean, adj_taxi_num)

###############################################################################
# 做图
library(ggplot2)
plot_taxi_adj_36l <- ggplot(adj_statistical_data,
			aes(x = adj_taxi_num, y = taxi_mean))

plot_output_36l <- plot_taxi_adj_36l +
	geom_errorbar(data = adj_statistical_data,
		aes(ymin = taxi_mean - taxi_sd,
			ymax = taxi_mean + taxi_sd),
			size = 0.9, width = .7, alpha = .5) +
	geom_point(size = 3.5, alpha = .8) +
	labs(x = "地面航班数（架次）", y = "离场滑行时间（分钟）", size = 2) +
	scale_x_discrete(limits = c(0, 60)) +
	scale_x_continuous(limits = c(0, 60), breaks = c(0, 5, 10, 15, 20, 25,
		30, 35, 40, 45, 50, 55, 60),
		labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) +
	scale_y_discrete(limits = c(0, 40)) +
	scale_y_continuous(limits = c(0, 40), breaks = c(0, 5, 10, 15, 20, 25,
		30, 35, 40),
		labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40)) +
	theme_bw()

win.graph(width = 8, height = 5)
plot_output_36l

###############################################################################
###############################################################################
###############################################################################
# plot adj_traffic vs dep_taxi 
# rwy 36R
adj_vs_taxi_36r <- subset(dep_processed, rwy == "36R", select = c(25, 14, 6))
names(adj_vs_taxi_36r) <- c("adjusted_traffic", "dep_taxi", "ramp")
# 作图前准备
adj_max <- max(adj_vs_taxi_36r$adjusted_traffic)
taxi_mean <- vector(mode = "numeric", length = adj_max)
taxi_median <- vector(mode = "numeric", length = adj_max)
taxi_max <- vector(mode = "numeric", length = adj_max)
taxi_min <- vector(mode = "numeric", length = adj_max)
taxi_sd <- vector(mode = "numeric", length = adj_max)

for (i in 1:adj_max) {
	taxi_mean[i] <- mean(as.matrix(subset(adj_vs_taxi_36r,
		adjusted_traffic == i, select = 2)))
	taxi_median[i] <- median(as.matrix(subset(adj_vs_taxi_36r,
		adjusted_traffic == i, select = 2)))
	taxi_max[i] <- max(as.matrix(subset(adj_vs_taxi_36r,
		adjusted_traffic == i, select = 2)))
	taxi_min[i] <- min(as.matrix(subset(adj_vs_taxi_36r,
		adjusted_traffic == i, select = 2)))
	taxi_sd[i] <- sd(as.matrix(subset(adj_vs_taxi_36r,
		adjusted_traffic == i, select = 2)))
}
adj_taxi_num <- c(1:max(adj_max))
# mean regression fit data
adj_statistical_data <- data.frame(taxi_mean, adj_taxi_num)

###############################################################################
# 做图
library(ggplot2)
plot_taxi_adj_36r <- ggplot(adj_statistical_data,
			aes(x = adj_taxi_num, y = taxi_mean))

plot_output_36r <- plot_taxi_adj_36r +
	geom_errorbar(data = adj_statistical_data,
		aes(ymin = taxi_mean - taxi_sd,
			ymax = taxi_mean + taxi_sd),
			size = 0.9, width = .7, alpha = .5) +
	geom_point(size = 3.5, alpha = .8) +
	labs(x = "地面航班数（架次）", y = "离场滑行时间（分钟）", size = 2) +
	scale_x_discrete(limits = c(0, 60)) +
	scale_x_continuous(limits = c(0, 60), breaks = c(0, 5, 10, 15, 20, 25,
		30, 35, 40, 45, 50, 55, 60),
		labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) +
	scale_y_discrete(limits = c(0, 40)) +
	scale_y_continuous(limits = c(0, 40), breaks = c(0, 5, 10, 15, 20, 25,
		30, 35, 40),
		labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40)) +
	theme_bw()

win.graph(width = 8, height = 5)
plot_output_36r

###############################################################################
###############################################################################
###############################################################################
# plot adj_traffic vs dep_taxi 
# rwy 01
adj_vs_taxi_01 <- subset(dep_processed, rwy == "1", select = c(25, 14, 6))
names(adj_vs_taxi_01) <- c("adjusted_traffic", "dep_taxi", "ramp")
# 作图前准备
adj_max <- max(adj_vs_taxi_01$adjusted_traffic)
taxi_mean <- vector(mode = "numeric", length = adj_max)
taxi_median <- vector(mode = "numeric", length = adj_max)
taxi_max <- vector(mode = "numeric", length = adj_max)
taxi_min <- vector(mode = "numeric", length = adj_max)
taxi_sd <- vector(mode = "numeric", length = adj_max)

for (i in 1:adj_max) {
	taxi_mean[i] <- mean(as.matrix(subset(adj_vs_taxi_01,
		adjusted_traffic == i, select = 2)))
	taxi_median[i] <- median(as.matrix(subset(adj_vs_taxi_01,
		adjusted_traffic == i, select = 2)))
	taxi_max[i] <- max(as.matrix(subset(adj_vs_taxi_01,
		adjusted_traffic == i, select = 2)))
	taxi_min[i] <- min(as.matrix(subset(adj_vs_taxi_01,
		adjusted_traffic == i, select = 2)))
	taxi_sd[i] <- sd(as.matrix(subset(adj_vs_taxi_01,
		adjusted_traffic == i, select = 2)))
}
adj_taxi_num <- c(1:max(adj_max))
# mean regression fit data
adj_statistical_data <- data.frame(taxi_mean, adj_taxi_num)

###############################################################################
# 做图
library(ggplot2)
plot_taxi_adj_01 <- ggplot(adj_statistical_data,
			aes(x = adj_taxi_num, y = taxi_mean))

plot_output_01 <- plot_taxi_adj_01 +
	geom_errorbar(data = adj_statistical_data,
		aes(ymin = taxi_mean - taxi_sd,
			ymax = taxi_mean + taxi_sd),
			size = 0.9, width = .7, alpha = .5) +
	geom_point(size = 3.5, alpha = .8) +
	labs(x = "地面航班数（架次）", y = "离场滑行时间（分钟）", size = 2) +
	scale_x_discrete(limits = c(0, 60)) +
	scale_x_continuous(limits = c(0, 60), breaks = c(0, 5, 10, 15, 20, 25,
		30, 35, 40, 45, 50, 55, 60),
		labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) +
	scale_y_discrete(limits = c(0, 40)) +
	scale_y_continuous(limits = c(0, 40), breaks = c(0, 5, 10, 15, 20, 25,
		30, 35, 40),
		labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40)) +
	theme_bw()

win.graph(width = 8, height = 5)
plot_output_01


###############################################################################
###############################################################################
###############################################################################
# 计算各停机位到跑道36l的畅通滑行时间
unimpeded_taxi_time_36l <- vector(mode = "numeric", length = 33)
for (i in 1:33) {
	dep_ramp_i <- subset(adj_vs_taxi_36l, ramp == i)
	adj_taxi_ramp_i_unimpeded <- subset(dep_ramp_i,
		(adjusted_traffic < 25) & (adjusted_traffic > 5))
	unimpeded_taxi_time_36l[i] <- mean(adj_taxi_ramp_i_unimpeded$dep_taxi)
}

###############################################################################
###############################################################################
###############################################################################
# 计算各停机位到跑道36r的畅通滑行时间
unimpeded_taxi_time_36r <- vector(mode = "numeric", length = 33)
for (i in 1:33) {
	dep_ramp_i <- subset(adj_vs_taxi_36r, ramp == i)
	adj_taxi_ramp_i_unimpeded <- subset(dep_ramp_i,
		(adjusted_traffic < 16)&(adjusted_traffic > 6))
	unimpeded_taxi_time_36r[i] <- mean(adj_taxi_ramp_i_unimpeded$dep_taxi)
}

###############################################################################
###############################################################################
###############################################################################
# 计算各停机位到跑道01的畅通滑行时间
unimpeded_taxi_time_01 <- vector(mode = "numeric", length = 33)
for (i in 1:33) {
	dep_ramp_i <- subset(adj_vs_taxi_01, ramp == i)
	adj_taxi_ramp_i_unimpeded <- subset(dep_ramp_i, adjusted_traffic < 12)
	unimpeded_taxi_time_01[i] <- mean(adj_taxi_ramp_i_unimpeded$dep_taxi)
}


###############################################################################
# 本文计算的畅通滑行时间
unimpeded_taxi_time <- data.frame(unimpeded_taxi_time_36l,
	unimpeded_taxi_time_36r, unimpeded_taxi_time_01)
names(unimpeded_taxi_time) <- c("36L", "36R", "01")

# 唐华龙计算的畅通滑行时间
unimpeded_taxi_time_tang_36l <- c(NA, NA, 10.53, 12.62, NA, 10.34, NA, NA, NA,
	10.44, NA, 12.14, 9.44, 7.72, 6.62, 5.56, 5.82, 6.94, 5.41, 6.23, 7.02,
	6.80, 5.64, 6.26, 5.23, 4.90, 5, NA, 7.76, 11.56, NA, NA, NA)
unimpeded_taxi_time_tang_36r <- c(4.22, 5.24, 3.67, 3.45, 4.52, 3.95, 5.7, 7.11,
	6.16, 4.82, 8.64, 5.33, 4.19, 3.02, 3.06, 3.98, 5.08, 5.24, 7.79, 5.58,
	4.89, 5.51, 5.87, 4.79, 5.27, 6.05, 6.31, NA, 6.47, 9.58, 7.22, NA, NA)
unimpeded_taxi_time_tang_01 <- c(5.13, 4.7, 5.97, 5.99, 5.53, 5.63, 6.39, 6.75,
	7.69, 6.46, 6.99, 8.23, NA, NA, NA, NA, NA, 11.79, NA, NA, 10.62, 12.01,
	NA, NA, NA, NA, NA, NA, NA, 10.08, NA, NA, NA)
unimpeded_taxi_time_tang <- data.frame(unimpeded_taxi_time_tang_36l,
	unimpeded_taxi_time_tang_36r, unimpeded_taxi_time_tang_01)
names(unimpeded_taxi_time_tang) <- c("36L", "36R", "01")

###############################################################################
# 滑行延误计算
# 首先计算各停机位到各跑道的平均滑行时间
average_taxi_36l <- vector(mode = "numeric", length = 33)
for (i in 1:33) {
	dep_ramp_i <- subset(adj_vs_taxi_36l, ramp == i)
	average_taxi_36l[i] <- mean(dep_ramp_i$dep_taxi)
}
average_taxi_36r <- vector(mode = "numeric", length = 33)
for (i in 1:33) {
	dep_ramp_i <- subset(adj_vs_taxi_36r, ramp == i)
	average_taxi_36r[i] <- mean(dep_ramp_i$dep_taxi)
}
average_taxi_01 <- vector(mode = "numeric", length = 33)
for (i in 1:33) {
	dep_ramp_i <- subset(adj_vs_taxi_01, ramp == i)
	average_taxi_01[i] <- mean(dep_ramp_i$dep_taxi)
}

average_taxi <- data.frame(average_taxi_36l, average_taxi_36r, average_taxi_01)
names(average_taxi) <- c("36L", "36R", "01")

# 分别计算两种滑行延误
taxi_delay_qu <- average_taxi - unimpeded_taxi_time
taxi_delay_tang <- average_taxi - unimpeded_taxi_time_tang

###############################################################################
# 输出畅通滑行时间与滑行延误
write.csv(unimpeded_taxi_time, "unimpeded_taxi_time_qu.csv", row.names = F)
write.csv(unimpeded_taxi_time_tang, "unimpeded_taxi_time_tang.csv", row.names = F)
write.csv(taxi_delay_qu, "taxi_delay_qu.csv", row.names = F)
write.csv(taxi_delay_tang, "taxi_delay_tang.csv", row.names = F)

