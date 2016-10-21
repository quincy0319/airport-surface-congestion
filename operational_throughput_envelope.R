# 推出率-起飞率曲线
# 以下均为2月份的数据
dep_feb <- read.csv("dep_feb_processed.csv")
arr_feb <- read.csv("arr_feb_processed.csv")

# 15分钟推出率、起飞率、接收率最大值
window_count_per15 <- read.csv("window_count_per15.csv")
pb_max_per15 <- max(window_count_per15$pb_count_per15)
dep_max_per15 <- max(window_count_per15$dep_count_per15)
arr_max_per15 <- max(window_count_per15$arr_count_per15)

# 做曲线前准备的数据
dep_mean <- vector(mode = "numeric", length = pb_max_per15)
dep_median <- vector(mode = "numeric", length = pb_max_per15)
dep_max <- vector(mode = "numeric", length = pb_max_per15)
dep_min <- vector(mode = "numeric", length = pb_max_per15)

for (i in 1:pb_max_per15) {
	dep_mean[i] <- mean(as.matrix(subset(window_count_per15, pb_count_per15 == i, select = 1)))
	dep_median[i] <- median(as.matrix(subset(window_count_per15, pb_count_per15 == i, select = 1)))
	dep_max[i] <- max(as.matrix(subset(window_count_per15, pb_count_per15 == i, select = 1)))
	dep_min[i] <- min(as.matrix(subset(window_count_per15, pb_count_per15 == i, select = 1)))
}
