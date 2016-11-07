# 北京机场运行模式分析
# 论文需要的图表

# 图2-2
dep_num <- c(21778, 5852, 1548, 2458, 20363, 40662)
arr_num <- c(29803, 1142, 4939, 4886, 33569, 19002)
dep_freq <- dep_num / sum(dep_num)
arr_freq <- arr_num / sum(arr_num)
rwy_num <- c("01", "18L", "18R", "19", "36L", "36R")
names(dep_num) <- rwy_num
names(arr_num) <- rwy_num
names(dep_freq) <- rwy_num
names(arr_freq) <- rwy_num

# 小时流量图


# 图3-1、3-2、3-3
# open origin data source
# feb
setwd("C:\\Users\\QYF\\Documents\\Visual Studio 2015\\Projects\\airport_congestion\\rwy_configuration")
rwy_flow_feb_per15 <- read.csv("rwy_flow_feb_per15.csv")
rwy_flow_feb_per60 <- read.csv("rwy_flow_feb_per60.csv")

################################################################################
# 图3-1
# rwy 36l
# 改变画布尺寸的代码
win.graph(width = 15, height = 15, pointsize = 8)
rwy_36l_feb <- subset(rwy_flow_feb_per60, dep36l + arr36l > 0, select = c(4, 1))
smoothScatter(rwy_36l_feb, xlim = c(0, 30), ylim = c(0, 30),
		xlab = "降落航班数（架次/60分钟）",
		ylab = "起飞航班数（架次/60分钟）",
		font.lab = 2)
title("跑道36L起降航班高密度散点图")
grid()
points(c(18, 4), c(9, 8), pch = 3, cex = 3)
# 主运行模式
rwy_36l_main_config <- subset(rwy_36l_feb, 
		(((dep36l - 9) ^ 2) + ((arr36l - 18) ^ 2) <= 25))
library(plotrix)
draw.circle(x = 18, y = 10, radius = 5, lwd = 2)

################################################################################
# 图3-2
# rwy 36r
# 改变画布尺寸的代码
win.graph(width = 15, height = 15, pointsize = 8)
rwy_36r_feb <- subset(rwy_flow_feb_per60, dep36r + arr36r > 0, select = c(5, 2))
smoothScatter(rwy_36r_feb, xlim = c(0, 30), ylim = c(0, 30),
		xlab = "降落航班数（架次/60分钟）",
		ylab = "起飞航班数（架次/60分钟）",
		font.lab = 2)
title("跑道36R起降航班高密度散点图")
grid()
points(c(11, 6), c(6, 24), pch = 3, cex = 3)

# 主运行模式
rwy_36r_main_config <- subset(rwy_36r_feb,
		(((dep36r - 24) ^ 2) + ((arr36r - 6) ^ 2) <= 25))
library(plotrix)
draw.circle(x = 6, y = 24, radius = 5, lwd = 2)

################################################################################
# 图3-3
# rwy 01
# 改变画布尺寸的代码
win.graph(width = 15, height = 15, pointsize = 8)
rwy_01_feb <- subset(rwy_flow_feb_per60, dep01 + arr01 > 0, select = c(6, 3))
smoothScatter(rwy_01_feb, xlim = c(0, 30), ylim = c(0, 30),
		xlab = "降落航班数（架次/60分钟）",
		ylab = "起飞航班数（架次/60分钟）",
		font.lab = 2)
title("跑道01起降航班高密度散点图")
grid()
points(c(5, 19), c(10, 11), pch = 3, cex = 3)

# 主运行模式
rwy_01_main_config <- subset(rwy_01_feb,
		(((dep01 - 11) ^ 2) + ((arr01 - 19) ^ 2) <= 25))
library(plotrix)
draw.circle(x = 19, y = 11, radius = 5, lwd = 2)

################################################################################
# 属于主运行模式的航班
dep_feb <- read.csv("dep_feb_processed.csv")
arr_feb <- read.csv("arr_feb_processed.csv")
dep_feb_36l <- subset(dep_feb, rwy == "36L")
arr_feb_36l <- subset(arr_feb, rwy == "36L")
dep_feb_36r <- subset(dep_feb, rwy == "36R")
arr_feb_36r <- subset(arr_feb, rwy == "36R")
dep_feb_01 <- subset(dep_feb, rwy == "1")
arr_feb_01 <- subset(arr_feb, rwy == "1")
attach(rwy_flow_feb_per60)
rwy_36l_chosen <- ifelse(((dep36l - 9) ^ 2) + ((arr36l - 18) ^ 2) <= 25, 1, 0)
rwy_36r_chosen <- ifelse(((dep36r - 24) ^ 2) + ((arr36r - 6) ^ 2) <= 25, 1, 0)
rwy_01_chosen <- ifelse(((dep01 - 11) ^ 2) + ((arr01 - 19) ^ 2) <= 25, 1, 0)
detach(rwy_flow_feb_per60)

# 36l analysis 
attach(dep_feb_36l)
dep_window_per60_36l <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_feb_36l)
attach(arr_feb_36l)
arr_window_per60_36l <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_feb_36l)
lth <- nrow(dep_feb_36l)
flight_chosen_dep_36l <- vector(mode = "numeric", length = lth)
for (i in 1:lth) {
	a <- dep_window_per60_36l[i]
	flight_chosen_dep_36l[i] <- rwy_36l_chosen[a]
}
lth <- nrow(arr_feb_36l)
flight_chosen_arr_36l <- vector(mode = "numeric", length = lth)
for (i in 1:lth) {
	a <- arr_window_per60_36l[i]
	flight_chosen_arr_36l[i] <- rwy_36l_chosen[a]
}
dep_feb_36l <- cbind(dep_feb_36l, flight_chosen_dep_36l)
arr_feb_36l <- cbind(arr_feb_36l, flight_chosen_arr_36l)
dep_feb_36l_main <- subset(dep_feb_36l, flight_chosen_dep_36l == 1)
arr_feb_36l_main <- subset(arr_feb_36l, flight_chosen_arr_36l == 1)

pairs( ~ dep_feb_60_per_dep + arr_feb_60_per_dep + pb_feb_60_per_dep + dep_taxi,
	data = dep_feb_36l_main)
pairs( ~ dep_feb_60_per_arr + arr_feb_60_per_arr + pb_feb_60_per_arr + arr_taxi,
	data = arr_feb_36l_main)



# 36r analysis 
attach(dep_feb_36r)
dep_window_per60_36r <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_feb_36r)
attach(arr_feb_36r)
arr_window_per60_36r <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_feb_36r)
lth <- nrow(dep_feb_36r)
flight_chosen_dep_36r <- vector(mode = "numeric", length = lth)
for (i in 1:lth) {
	a <- dep_window_per60_36r[i]
	flight_chosen_dep_36r[i] <- rwy_36r_chosen[a]
}
lth <- nrow(arr_feb_36r)
flight_chosen_arr_36r <- vector(mode = "numeric", length = lth)
for (i in 1:lth) {
	a <- arr_window_per60_36r[i]
	flight_chosen_arr_36r[i] <- rwy_36r_chosen[a]
}
dep_feb_36r <- cbind(dep_feb_36r, flight_chosen_dep_36r)
arr_feb_36r <- cbind(arr_feb_36r, flight_chosen_arr_36r)
dep_feb_36r_main <- subset(dep_feb_36r, flight_chosen_dep_36r == 1)
arr_feb_36r_main <- subset(arr_feb_36r, flight_chosen_arr_36r == 1)

pairs( ~ dep_feb_60_per_dep + arr_feb_60_per_dep + pb_feb_60_per_dep + dep_taxi,
	data = dep_feb_36r_main)
pairs( ~ dep_feb_60_per_arr + arr_feb_60_per_arr + pb_feb_60_per_arr + arr_taxi,
	data = arr_feb_36r_main)

# 01 analysis 
attach(dep_feb_01)
dep_window_per60_01 <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_feb_01)
attach(arr_feb_01)
arr_window_per60_01 <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_feb_01)
lth <- nrow(dep_feb_01)
flight_chosen_dep_01 <- vector(mode = "numeric", length = lth)
for (i in 1:lth) {
	a <- dep_window_per60_01[i]
	flight_chosen_dep_01[i] <- rwy_01_chosen[a]
}
lth <- nrow(arr_feb_01)
flight_chosen_arr_01 <- vector(mode = "numeric", length = lth)
for (i in 1:lth) {
	a <- arr_window_per60_01[i]
	flight_chosen_arr_01[i] <- rwy_01_chosen[a]
}
dep_feb_01 <- cbind(dep_feb_01, flight_chosen_dep_01)
arr_feb_01 <- cbind(arr_feb_01, flight_chosen_arr_01)
dep_feb_01_main <- subset(dep_feb_01, flight_chosen_dep_01 == 1)
arr_feb_01_main <- subset(arr_feb_01, flight_chosen_arr_01 == 1)

dev.new()
dep_lab = c("离场架次/60分钟", "到场架次/60分钟", "推出架次/60分钟", "离场滑行时间")
pairs( ~ dep_feb_60_per_dep + arr_feb_60_per_dep + pb_feb_60_per_dep + dep_taxi,
	data = dep_feb_01_main,
	labels = dep_lab)
arr_lab = c("离场架次/60分钟", "到场架次/60分钟", "推出架次/60分钟", "到场滑行时间")
pairs( ~ dep_feb_60_per_arr + arr_feb_60_per_arr + pb_feb_60_per_arr + arr_taxi,
	data = arr_feb_01_main,
	labels = arr_lab)

# kmeans
# 36l
library(fpc)
set.seed(1403018)
fit_kmeans_36l <- kmeans(na.omit(rwy_36l_feb), centers = 2)
plotcluster(na.omit(rwy_36l_feb), fit_kmeans_36l$cluster)
fit_kmeans_36l$centers
# 36r
fit_kmeans_36r <- kmeans(na.omit(rwy_36r_feb), centers = 2)
fit_kmeans_36r$centers
# 01
fit_kmeans_01 <- kmeans(na.omit(rwy_01_feb), centers = 2)
fit_kmeans_01$centers

# 柱状图
dep_feb_flight <- c(4982, 10484, 5318)
dep_feb_main <- c(2289, 4655, 2617)
dep_feb_not_main <- dep_feb_flight - dep_feb_main
dep_barplot <- rbind(dep_feb_main, dep_feb_not_main)
colnames(dep_barplot) <- c("36L", "36R", "01")
win.graph(width = 30, height = 18, pointsize = 8)
par(mfrow = c(1, 2))
barplot(dep_barplot,
	ylim = c(0, 12000),
	main = "离场航班运行模式",
	cex.main = 2, 
	xlab = "跑道", ylab = "航班架次",
	col = c("blue", "yellow"),
	cex.axis = 2, cex.names = 2, cex.lab = 1.1)
legend("topleft", c("单一运行模式", "其他模式"), cex = 1.5, 
	fill = c("blue", "yellow"))

arr_feb_flight <- c(8365, 4799, 7793)
arr_feb_main <- c(4624, 1269, 4369)
arr_feb_not_main <- arr_feb_flight - arr_feb_main
arr_barplot <- rbind(arr_feb_main, arr_feb_not_main)
colnames(arr_barplot) <- c("36L", "36R", "01")
barplot(arr_barplot,
	ylim = c(0, 12000),
	main = "进场航班运行模式",
	cex.main = 2,
	xlab = "跑道", ylab = "航班架次", 
	col = c("blue", "yellow"),
	cex.axis = 2, cex.names = 2, cex.lab = 1.1)
legend("topleft", c("单一运行模式", "其他模式"), cex = 1.5,
	fill = c("blue", "yellow"))

# 选定模式下航班滑行时间分析
# 36l
mean(dep_feb_36l_main$dep_taxi)
sd(dep_feb_36l_main$dep_taxi)
mean(arr_feb_36l_main$arr_taxi)
sd(arr_feb_36l_main$arr_taxi)

mean(dep_feb_36l$dep_taxi)
sd(dep_feb_36l$dep_taxi)
mean(arr_feb_36l$arr_taxi)
sd(arr_feb_36l$arr_taxi)

# 36r
mean(dep_feb_36r_main$dep_taxi)
sd(dep_feb_36r_main$dep_taxi)
mean(arr_feb_36r_main$arr_taxi)
sd(arr_feb_36r_main$arr_taxi)

mean(dep_feb_36r$dep_taxi)
sd(dep_feb_36r$dep_taxi)
mean(arr_feb_36r$arr_taxi)
sd(arr_feb_36r$arr_taxi)

# 01
mean(dep_feb_01_main$dep_taxi)
sd(dep_feb_01_main$dep_taxi)
mean(arr_feb_01_main$arr_taxi)
sd(arr_feb_01_main$arr_taxi)

mean(dep_feb_01$dep_taxi)
sd(dep_feb_01$dep_taxi)
mean(arr_feb_01$arr_taxi)
sd(arr_feb_01$arr_taxi)
