# rwy usage
# feb to may
rwy_number <- c("01", "18L", "18R", "19", "36L", "36R", "01",
		"18L", "18R", "19", "36L", "36R")
rwy_stat <- c("dep", "dep", "dep", "dep", "dep", "dep",
		"arr", "arr", "arr", "arr", "arr", "arr")
rwy_dep <- c(21778, 5852, 1548, 2458, 20363, 40662)
rwy_arr <- c(29803, 1142, 4939, 4886, 33569, 19002)
rwy_flow <- c(21778, 5852, 1548, 2458, 20363, 40662,
		29803, 1142, 4939, 4886, 33569, 19002)
# feb
rwy_dep_feb <- c(5318, 450, 141, 180, 4982, 10484)
rwy_arr_feb <- c(7793, 195, 302, 325, 8365, 4799)
rwy_flow_feb <- c(5318, 450, 141, 180, 4982, 10484,
		7793, 195, 302, 325, 8365, 4799)

# dataframe
rwy_usage <- data.frame(rwy_number, rwy_flow, rwy_stat)
rwy_usage_feb <- data.frame(rwy_number, rwy_flow_feb, rwy_stat)
# 使用plyr数据中的arrange函数对每组条形对应的数据进行累计求和
library(plyr)
ce <- arrange(rwy_usage, rwy_number, rwy_stat)
# 计算y轴的位置，将数据标签置于条形中部
ce <- ddply(ce, "rwy_number", transform, label_y = cumsum(rwy_flow) - 0.5 * rwy_flow)
# plot of runway usage
library(ggplot2)
win.graph(width = 10, height = 8)
plot1 <- ggplot(ce,
	aes(x = rwy_number, y = rwy_flow, fill = rwy_stat))
plot1 + geom_bar(stat = "identity", colour = "black") +
geom_text(aes(y = label_y, label = rwy_flow), colour = "black") +
labs(x = "跑道", y = "流量（架次）", fill = "航班", size = 20) +
scale_fill_discrete(labels = c("到场", "离场")) +
theme_bw()