setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/operational_throughput_envelope")
# paper chapter 2
# dep_rate~dep_demand关系图、误差线、拟合曲线
source("deal_with_data.R")
library(ggplot2)
library(reshape2)
library(grid)
demand_statistical_data_long <- melt(demand_statistical_data, id = "dep_demand")

# plot 1 
# 均值图（mean plot）
# 底层赋值给变量
dep_mean_plot <- ggplot(demand_statistical_data_long,
			aes(x = dep_demand, y = value,
			shape = variable,
			colour = variable))
# 添加图层
# 绘制errorbar、二次拟合曲线
plot1 <- dep_mean_plot +
	geom_errorbar(data = demand_statistical_data_long,
		aes(ymin = value - demand_sd,
			ymax = value + demand_sd),
			size = 1.1, width = .7, alpha = .5) +
	geom_point(size = 3.5, alpha = .8) + 
	labs(x = "15分钟场面离场航班数", y = "15分钟起飞率", 
		title = "离场航班数-起飞率变化图(a)", size = 2) +
	scale_x_discrete(limits = c(0, 60)) +
	scale_x_continuous(limits = c(0, 60), breaks = c(0, 5, 10, 15, 20, 25,
		30, 35, 40, 45, 50, 55, 60),
		labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) +
	scale_colour_discrete(name = "统计量", labels = c("平均数", "中位数")) +
	scale_shape_discrete(name = "统计量", labels = c("平均数", "中位数")) +
	scale_linetype_discrete(name = "统计量", labels = c("平均数", "中位数"))


# plot 2
# mean和median的拟合曲线
demand_regression_plot <- ggplot(demand_statistical_data_long,
				aes(x = dep_demand, y = value,
				colour = variable,
				shape = variable,
				linetype = variable))
plot2 <- demand_regression_plot +
	geom_point(size = 2) +
	geom_smooth(method = 'lm', formula = y ~ poly(x, 2),
			size = 1.2, alpha = .1) +
	labs(x = "15分钟场面离场航班数", y = "15分钟起飞率", title = "(b)拟合曲线",
		shape = "11", colour = "11", size = 2) +
	scale_x_continuous(limits = c(0, 60), breaks = c(0, 5, 10, 15, 20, 25,
		30, 35, 40, 45, 50, 55, 60),
		labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) +
# 由于颜色、点形状、线性均改变了，所以改变图例时也要将他们同时修改
	scale_colour_discrete(name = "统计量", 
		labels = c("平均数拟合线", "中位数拟合线")) +
	scale_shape_discrete(name = "统计量", 
		labels = c("平均数拟合线", "中位数拟合线")) +
	scale_linetype_discrete(name = "统计量", 
		labels = c("平均数拟合线", "中位数拟合线"))

# 一页多图设置
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(1, 2))

# 将dep_mean和dep_median中的NA值变为前一个值
demand_mean[is.na(demand_mean)] <- demand_mean[as.numeric(which(is.na(demand_mean)) - 1)]
demand_median[is.na(demand_median)] <- demand_median[as.numeric(which(is.na(demand_median)) - 1)]

# 回归分析
fit_mean <- lm(demand_mean ~ dep_demand + I(dep_demand ^ 2))
summary(fit_mean)
fit_median <- lm(demand_median ~ dep_demand + I(dep_demand ^ 2))
summary(fit_median)

qqPlot(fit_mean, id.method = "identify", simulate = TRUE, main = "qqplot")