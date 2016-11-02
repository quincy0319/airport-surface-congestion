# open origin data source
# feb
setwd("C:\\Users\\QYF\\Documents\\Visual Studio 2015\\Projects\\airport_congestion\\rwy_configuration")
rwy_flow_feb_per15 <- read.csv("rwy_flow_feb_per15.csv")
rwy_flow_feb_per60 <- read.csv("rwy_flow_feb_per60.csv")

# 各跑道的60分钟高密度散点图
# smooth scatter pic per 60 minutes
# rwy 36l
dev.new()
rwy_36l_feb_upper10 <- subset(rwy_flow_feb_per60, ((dep36l + arr36l) > 10), select = c(1,4))
smoothScatter(rwy_36l_feb_upper10)
title("rwy_36l_feb_upper10")
grid()
a1 <- 4
a2 <- 18
b1 <- 15
b2 <- 11
c1 <- 6.3
c2 <- 27.4
d1 <- 17.3
d2 <- 20.4
lines(c(a1, b1), c(a2, b2), "l", lwd = 2)
lines(c(a1, c1), c(a2, c2), "l", lwd = 2)
lines(c(b1, d1), c(b2, d2), "l", lwd = 2)
lines(c(c1, d1), c(c2, d2), "l", lwd = 2)
# rwy 36l main configuration in feb
rwy_36l_feb_main <- subset(rwy_flow_feb_per60, (arr36l >= 226 / 11 - (7 * dep36l) / 11)
		& (arr36l <= 691 / 22 -(7 * dep36l) / 11
		& (arr36l <= (94 * dep36l) / 23 + 38/23)
		& (arr36l >= (94 * dep36l) / 23 - 1157/23)),
		select = c(1, 4))

# rwy 36r
dev.new()
rwy_36r_feb_upper10 <- subset(rwy_flow_feb_per60, ((dep36r + arr36r) > 10), select = c(2, 5))
smoothScatter(rwy_36r_feb_upper10)
title("rwy_36r_feb_upper10")
grid()
a1 <- 28
a2 <- 0
b1 <- 37
b2 <- 0
c1 <- 11
c2 <- 15
d1 <- 20
d2 <- 15
lines(c(a1, b1), c(a2, b2), "l", lwd = 2)
lines(c(a1, c1), c(a2, c2), "l", lwd = 2)
lines(c(b1, d1), c(b2, d2), "l", lwd = 2)
lines(c(c1, d1), c(c2, d2), "l", lwd = 2)
# rwy 36r main configuration in feb
rwy_36r_feb_main <- subset(rwy_flow_feb_per60, (arr36r >= 0)
		& (arr36r <= 555 / 17 -(15 * dep36r) / 17)
		& (arr36r <= 15)
		& (arr36r >= 420 / 17 - (15 * dep36r) / 17),
		select = c(2, 5))

# rwy 01
dev.new()
rwy_01_feb_upper10 <- subset(rwy_flow_feb_per60, ((dep01 + arr01) > 10), select = c(3, 6))
smoothScatter(rwy_01_feb_upper10)
title("rwy_01_feb_upper10")
grid()
a1 <- 5
a2 <- 22
b1 <- 17
b2 <- 13
c1 <- 7
c2 <- 27.6
d1 <- 19
d2 <- 18.6
lines(c(a1, b1), c(a2, b2), "l", lwd = 2)
lines(c(a1, c1), c(a2, c2), "l", lwd = 2)
lines(c(b1, d1), c(b2, d2), "l", lwd = 2)
lines(c(c1, d1), c(c2, d2), "l", lwd = 2)
# rwy 01 main configuration in feb
rwy_01_feb_main <- subset(rwy_flow_feb_per60, (arr01 >= 103 / 4 - (3 * dep01) / 4)
		& (arr01 <= 657 / 20 -(3 * dep01) / 4
		& (arr01 <= (14 * dep01) / 5 + 8)
		& (arr01 >= (14 * dep01) / 5 - 173/5)),
		select = c(3, 6))

# feb main configuration summary
# 计算主运行模式的占比
prob_36l_feb_main <- nrow(rwy_36l_feb_main) / nrow(rwy_36l_feb_upper10)
prob_36r_feb_main <- nrow(rwy_36r_feb_main) / nrow(rwy_36r_feb_upper10)
prob_01_feb_main <- nrow(rwy_01_feb_main) / nrow(rwy_01_feb_upper10)
prob_36l_feb_main
prob_36r_feb_main
prob_01_feb_main

################################################################################

# put data back into origin data
# 各个60分钟window是否属于主运行模式（属于为1不属于为0）
attach(rwy_flow_feb_per60) 
for (i in 1:672){ 
	rwy_36l_chosen_hour_feb <- ifelse((arr36l >= 226 / 11 - (7 * dep36l) / 11)
			& (arr36l <= 691 / 22 -(7 * dep36l) / 11
			& (arr36l <= (94 * dep36l) / 23 + 38/23)
			& (arr36l >= (94 * dep36l) / 23 - 1157/23)), 1, 0)
}
for (i in 1:672){ 
	rwy_36r_chosen_hour_feb <- ifelse((arr36r >= 0)
			& (arr36r <= 555 / 17 -(15 * dep36r) / 17)
			& (arr36r <= 15)
			& (arr36r >= 420 / 17 - (15 * dep36r) / 17), 1, 0)
}
for (i in 1:672){ 
	rwy_01_chosen_hour_feb <- ifelse((arr01 >= 103 / 4 - (3 * dep01) / 4)
			& (arr01 <= 657 / 20 -(3 * dep01) / 4
			& (arr01 <= (14 * dep01) / 5 + 8)
			& (arr01 >= (14 * dep01) / 5 - 173/5)), 1, 0)
}
detach(rwy_flow_feb_per60)
data_main_configuration <- cbind(rwy_36l_chosen_hour_feb, rwy_36r_chosen_hour_feb, rwy_01_chosen_hour_feb)

# 读取起降源数据
dep_feb <- subset(read.csv("dep_origin.csv"), mission_month == 2)
arr_feb <- subset(read.csv("arr_origin.csv"), mission_month == 2)
# 读取包含所在60 15分钟window的航班数据
# 起飞
dep_feb_36l <- subset(dep_feb, rwy == '36L')
dep_feb_36r <- subset(dep_feb, rwy == '36R')
dep_feb_01 <- subset(dep_feb, rwy == "1")
# 降落
arr_feb_36l <- subset(arr_feb, rwy == '36L')
arr_feb_36r <- subset(arr_feb, rwy == '36R')
arr_feb_01 <- subset(arr_feb, rwy == '1')

# origin_data 是源数据，用上边的6个量
# data_window 是通过origin_data中日期、时刻算出的window（60分）
# chosen_ornot 是上一个部分算出的，每个window是否为主运行模式

#flight_chosen_ornot <- function(origin_data, chosen_ornot, flight_chosen) {
	#attach(origin_data)
		#data_window <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
	#detach(origin_data)
	#lth <- nrow(origin_data)
	#flight_chosen <- vector(mode = "numeric", length = lth)
	#for (i in 1:lth) {
		#a <- data_window[i]
		#flight_chosen[i] <- chosen_ornot[a]
	#}
#}
#flight_chosen_ornot(dep_feb_36l, rwy_36l_chosen_hour, chosen_dep_36l)

# Function 功能有问题！！！！
################################################################################

# 数据准备（36l、36r、01三条跑道）
# 36l
attach(dep_feb_36l)
dep_window_per60_36l <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_feb_36l)
attach(arr_feb_36l)
arr_window_per60_36l <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_feb_36l)
# 36r
attach(dep_feb_36r)
dep_window_per60_36r <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_feb_36r)
attach(arr_feb_36r)
arr_window_per60_36r <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_feb_36r)
# 01
attach(dep_feb_01)
dep_window_per60_01 <- (mission_date - 1) * 24 + floor(time_takeoff_real / 60) + 1
detach(dep_feb_01)
attach(arr_feb_01)
arr_window_per60_01 <- (mission_date - 1) * 24 + floor(time_wheelon_real / 60) + 1
detach(arr_feb_01)

# 分别计算每个航班是否在选出的时刻中
# 36l
lth <- nrow(dep_feb_36l)
flight_chosen_dep_36l <- vector(mode = "numeric", length = lth)
for (i in 1:lth) {
	a <- dep_window_per60_36l[i]
	flight_chosen_dep_36l[i] <- rwy_36l_chosen_hour[a]
}

lth <- nrow(arr_feb_36l)
flight_chosen_arr_36l <- vector(mode = "numeric", length = lth)
for (i in 1:lth) {
	a <- arr_window_per60_36l[i]
	flight_chosen_arr_36l[i] <- rwy_36l_chosen_hour[a]
}

# 36r
lth <- nrow(dep_feb_36r)
flight_chosen_dep_36r <- vector(mode = "numeric", length = lth)
for (i in 1:lth) {
	a <- dep_window_per60_36r[i]
	flight_chosen_dep_36r[i] <- rwy_36r_chosen_hour[a]
}

lth <- nrow(arr_feb_36r)
flight_chosen_arr_36r <- vector(mode = "numeric", length = lth)
for (i in 1:lth) {
	a <- arr_window_per60_36r[i]
	flight_chosen_arr_36r[i] <- rwy_36r_chosen_hour[a]
}

# 01
lth <- nrow(dep_feb_01)
flight_chosen_dep_01 <- vector(mode = "numeric", length = lth)
for (i in 1:lth) {
	a <- dep_window_per60_01[i]
	flight_chosen_dep_01[i] <- rwy_01_chosen_hour[a]
}

lth <- nrow(arr_feb_01)
flight_chosen_arr_01 <- vector(mode = "numeric", length = lth)
for (i in 1:lth) {
	a <- arr_window_per60_01[i]
	flight_chosen_arr_01[i] <- rwy_01_chosen_hour[a]
}

dep_feb_36l_processed <- data.frame(dep_feb_36l, flight_chosen_dep_36l)
dep_feb_36r_processed <- data.frame(dep_feb_36r, flight_chosen_dep_36r)
dep_feb_01_processed <- data.frame(dep_feb_01, flight_chosen_dep_01)
arr_feb_36l_processed <- data.frame(arr_feb_36l, flight_chosen_arr_36l)
arr_feb_36r_processed <- data.frame(arr_feb_36r, flight_chosen_arr_36r)
arr_feb_01_processed <- data.frame(arr_feb_01, flight_chosen_arr_01)

write.csv(dep_feb_36l_processed, "dep_feb_36l_processed.csv", row.names = F)
write.csv(dep_feb_36r_processed, "dep_feb_36r_processed.csv", row.names = F)
write.csv(dep_feb_01_processed, "dep_feb_01_processed.csv", row.names = F)
write.csv(arr_feb_36l_processed, "arr_feb_36l_processed.csv", row.names = F)
write.csv(arr_feb_36r_processed, "arr_feb_36r_processed.csv", row.names = F)
write.csv(arr_feb_01_processed, "arr_feb_01_processed.csv", row.names = F)

################################################################################
# scatter plot colored by smoothed densities
# 高密度散点图
# 本段代码不使用，前边已经有这段的功能
dev.new()
attach(dep_60)
attach(arr_60)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))
par(cex.axis = 1.2, cex.lab = 1.5)
smoothScatter(x01, y01, xlab = "dep_01_per60", ylab = "arr_01_per60")
title(main = "rwy_01", cex.main = 1.5)
smoothScatter(x36l, y36l, xlab = "dep_36l_per60", ylab = "arr_36l_per60")
title(main = "rwy_36l", cex.main = 1.5)
smoothScatter(x36r, y36r, xlab = "dep_36r_per60", ylab = "arr_36r_per60")
title(main = "rwy_36r", cex.main = 1.5)
smoothScatter(x19, y19, xlab = "dep_19_per60", ylab = "arr_19_per60")
title(main = "rwy_19", cex.main = 1.5)
smoothScatter(x18r, y18r, xlab = "dep_18r_per60", ylab = "arr_18r_per60")
title(main = "rwy_18r", cex.main = 1.5)
smoothScatter(x18l, y18l, xlab = "dep_18l_per60", ylab = "arr_18l_per60")
title(main = "rwy_18l", cex.main = 1.5)
par(opar)
detach(dep_60)
detach(arr_60)


dev.new()
attach(dep_30)
attach(arr_30)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))
par(cex.axis = 1.2, cex.lab = 1.5)
smoothScatter(x01, y01, xlab = "dep_01_per30", ylab = "arr_01_per30")
title(main = "rwy_01", cex.main = 1.5)
smoothScatter(x36l, y36l, xlab = "dep_36l_per30", ylab = "arr_36l_per30")
title(main = "rwy_36l", cex.main = 1.5)
smoothScatter(x36r, y36r, xlab = "dep_36r_per30", ylab = "arr_36r_per30")
title(main = "rwy_36r", cex.main = 1.5)
smoothScatter(x19, y19, xlab = "dep_19_per30", ylab = "arr_19_per30")
title(main = "rwy_19", cex.main = 1.5)
smoothScatter(x18r, y18r, xlab = "dep_18r_per30", ylab = "arr_18r_per30")
title(main = "rwy_18r", cex.main = 1.5)
smoothScatter(x18l, y18l, xlab = "dep_18l_per30", ylab = "arr_18l_per30")
title(main = "rwy_18l", cex.main = 1.5)
par(opar)
detach(dep_30)
detach(arr_30)

dev.new()
attach(dep_15)
attach(arr_15)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))
par(cex.axis = 1.2, cex.lab = 1.5)
smoothScatter(x01, y01, xlab = "dep_01_per15", ylab = "arr_01_per15")
title(main = "rwy_01", cex.main = 1.5)
smoothScatter(x36l, y36l, xlab = "dep_36l_per15", ylab = "arr_36l_per15")
title(main = "rwy_16l", cex.main = 1.5)
smoothScatter(x36r, y36r, xlab = "dep_36r_per15", ylab = "arr_36r_per15")
title(main = "rwy_36r", cex.main = 1.5)
smoothScatter(x19, y19, xlab = "dep_19_per15", ylab = "arr_19_per15")
title(main = "rwy_19", cex.main = 1.5)
smoothScatter(x18r, y18r, xlab = "dep_18r_per15", ylab = "arr_18r_per15")
title(main = "rwy_18r", cex.main = 1.5)
smoothScatter(x18l, y18l, xlab = "dep_18l_per15", ylab = "arr_18l_per15")
title(main = "rwy_18l", cex.main = 1.5)
par(opar)
detach(dep_15)
detach(arr_15)

# percentage of rwy usage
dev.new()
pie(dep_data_summary, radius = 1)
title("dep rwy usage")
dev.new()
pie(arr_data_summary, radius = 1)
title("arr rwy usage")




