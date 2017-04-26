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
unimpeded_taxi <- vector(mode = "numeric", length = nrow(dep_processed))
for (j in 1:nrow(dep_processed)) {
	ramp_num <- dep_processed$ramp[j]
	unimpeded_taxi[j] <- unimpeded_taxi_time[ramp_num]
}
dep_processed <- data.frame(dep_processed, unimpeded_taxi)

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
unimpeded_taxi <- vector(mode = "numeric", length = nrow(dep_processed))
for (j in 1:nrow(dep_processed)) {
	ramp_num <- dep_processed$ramp[j]
	unimpeded_taxi[j] <- unimpeded_taxi_time[ramp_num]
}
dep_processed <- data.frame(dep_processed, unimpeded_taxi)

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
unimpeded_taxi <- vector(mode = "numeric", length = nrow(dep_processed))
for (j in 1:nrow(dep_processed)) {
	ramp_num <- dep_processed$ramp[j]
	unimpeded_taxi[j] <- unimpeded_taxi_time[ramp_num]
}
dep_processed <- data.frame(dep_processed, unimpeded_taxi)


###############################################################################
unimpeded_taxi_time <- data.frame(unimpeded_taxi_time_36l,
	unimpeded_taxi_time_36r, unimpeded_taxi_time_01)
names(unimpeded_taxi_time) <- c("36L", "36R", "01")