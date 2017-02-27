# 推出率-起飞率曲线
# 读取2-5月数据
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/operational_throughput_envelope")

dep_feb <- read.csv("dep_feb_processed.csv")
arr_feb <- read.csv("arr_feb_processed.csv")
dep_mar <- read.csv("dep_mar_processed.csv")
arr_mar <- read.csv("arr_mar_processed.csv")
dep_apr <- read.csv("dep_apr_processed.csv")
arr_apr <- read.csv("arr_apr_processed.csv")
dep_may <- read.csv("dep_may_processed.csv")
arr_may <- read.csv("arr_may_processed.csv")

# 整合四个月数据
names(dep_feb)[17:24] <- c("dep_15_per_dep", "arr_15_per_dep", "dep_60_per_dep",
		"arr_60_per_dep", "pb_15_per_dep", "pb_60_per_dep",
		"heavy_dep_15_per_dep", "heavy_arr_15_per_dep")
names(dep_mar)[17:24] <- c("dep_15_per_dep", "arr_15_per_dep", "dep_60_per_dep",
		"arr_60_per_dep", "pb_15_per_dep", "pb_60_per_dep",
		"heavy_dep_15_per_dep", "heavy_arr_15_per_dep")
names(dep_apr)[17:24] <- c("dep_15_per_dep", "arr_15_per_dep", "dep_60_per_dep",
		"arr_60_per_dep", "pb_15_per_dep", "pb_60_per_dep",
		"heavy_dep_15_per_dep", "heavy_arr_15_per_dep")
names(dep_may)[17:24] <- c("dep_15_per_dep", "arr_15_per_dep", "dep_60_per_dep",
		"arr_60_per_dep", "pb_15_per_dep", "pb_60_per_dep",
		"heavy_dep_15_per_dep", "heavy_arr_15_per_dep")

names(arr_feb)[16:23] <- c("dep_15_per_arr", "arr_15_per_arr", "dep_60_per_arr",
		"arr_60_per_arr", "pb_15_per_arr", "pb_60_per_arr",
		"heavy_dep_15_per_arr", "heavy_arr_15_per_arr")
names(arr_mar)[16:23] <- c("dep_15_per_arr", "arr_15_per_arr", "dep_60_per_arr",
		"arr_60_per_arr", "pb_15_per_arr", "pb_60_per_arr",
		"heavy_dep_15_per_arr", "heavy_arr_15_per_arr")
names(arr_apr)[16:23] <- c("dep_15_per_arr", "arr_15_per_arr", "dep_60_per_arr",
		"arr_60_per_arr", "pb_15_per_arr", "pb_60_per_arr",
		"heavy_dep_15_per_arr", "heavy_arr_15_per_arr")
names(arr_may)[16:23] <- c("dep_15_per_arr", "arr_15_per_arr", "dep_60_per_arr",
		"arr_60_per_arr", "pb_15_per_arr", "pb_60_per_arr",
		"heavy_dep_15_per_arr", "heavy_arr_15_per_arr")

dep_full <- rbind(dep_feb, dep_mar, dep_apr, dep_may)
arr_full <- rbind(arr_feb, arr_mar, arr_apr, arr_may)
# 输出数据
write.csv(dep_full, "dep_processed_feb2may.csv", row.names = F)
write.csv(arr_full, "arr_processed_feb2may.csv", row.names = F)

# 
# 15分钟推出率、起飞率、接收率、重型机（起降）
window_count_feb_per15 <- read.csv("window_count_feb_per15.csv")
window_count_mar_per15 <- read.csv("window_count_mar_per15.csv")
window_count_apr_per15 <- read.csv("window_count_apr_per15.csv")
window_count_may_per15 <- read.csv("window_count_may_per15.csv")
# 将window_count结合成一个表
names(window_count_feb_per15) <- c("dep_count_per15", "arr_count_per15",
	"pb_count_per15", "heavy_dep_per15", "heavy_arr_per15")
names(window_count_mar_per15) <- c("dep_count_per15", "arr_count_per15",
	"pb_count_per15", "heavy_dep_per15", "heavy_arr_per15")
names(window_count_apr_per15) <- c("dep_count_per15", "arr_count_per15",
	"pb_count_per15", "heavy_dep_per15", "heavy_arr_per15")
names(window_count_may_per15) <- c("dep_count_per15", "arr_count_per15",
	"pb_count_per15", "heavy_dep_per15", "heavy_arr_per15")
dep_demand_per15 <- as.matrix(read.csv("dep_demand_per15.csv", header = F))
dep_demand_per15 <- as.vector(dep_demand_per15)
window_count_per15 <- rbind(window_count_feb_per15, window_count_mar_per15,
	window_count_apr_per15, window_count_may_per15)
window_count_per15 <- cbind(window_count_per15, dep_demand_per15)
write.csv(window_count_per15, "window_count_feb2may.csv", row.names = F)


pb_max_per15 <- max(window_count_per15$pb_count_per15)
dep_max_per15 <- max(window_count_per15$dep_count_per15)
arr_max_per15 <- max(window_count_per15$arr_count_per15)
dep_demand_max_per15 <- max(window_count_per15$dep_demand_per15)
# 做曲线前准备的数据
dep_mean <- vector(mode = "numeric", length = pb_max_per15)
dep_median <- vector(mode = "numeric", length = pb_max_per15)
dep_max <- vector(mode = "numeric", length = pb_max_per15)
dep_min <- vector(mode = "numeric", length = pb_max_per15)
dep_sd <- vector(mode = "numeric", length = pb_max_per15)

demand_mean <- vector(mode = "numeric", length = dep_demand_max_per15)
demand_median <- vector(mode = "numeric", length = dep_demand_max_per15)
demand_max <- vector(mode = "numeric", length = dep_demand_max_per15)
demand_min <- vector(mode = "numeric", length = dep_demand_max_per15)
demand_sd <- vector(mode = "numeric", length = dep_demand_max_per15)

###############################################################################
# pushback
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
dep_pb <- c(1:max(pb_max_per15))
dep_statistical_data <- data.frame(dep_mean, dep_median, dep_pb)

###############################################################################
# dep_demand
for (i in 1:dep_demand_max_per15) {
	demand_mean[i] <- mean(as.matrix(subset(window_count_per15,
		dep_demand_per15 == i, select = 1)))
	demand_median[i] <- median(as.matrix(subset(window_count_per15,
		dep_demand_per15 == i, select = 1)))
	demand_max[i] <- max(as.matrix(subset(window_count_per15,
		dep_demand_per15 == i, select = 1)))
	demand_min[i] <- min(as.matrix(subset(window_count_per15,
		dep_demand_per15 == i, select = 1)))
	demand_sd[i] <- sd(as.matrix(subset(window_count_per15,
		dep_demand_per15 == i, select = 1)))
}
dep_demand <- c(1:max(dep_demand_per15))
demand_statistical_data <- data.frame(demand_mean, demand_median, dep_demand)
