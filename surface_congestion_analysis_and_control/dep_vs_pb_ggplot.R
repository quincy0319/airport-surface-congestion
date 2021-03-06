# 起飞率-推出率关系图、误差线、拟合曲线
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/surface_congestion_analysis_and_control")
source("deal_with_data.R")
library(ggplot2)
library(reshape2)
library(grid)
dep_statistical_data_long <- melt(dep_statistical_data, id = "dep_pb")

# plot 1 
# 均值图（mean plot）
# 底层赋值给变量
dep_mean_plot <- ggplot(dep_statistical_data_long,
			aes(x = dep_pb, y = value,
			shape = variable,
			colour = variable))
# 添加图层
# 绘制errorbar、二次拟合曲线
plot1 <- dep_mean_plot +
geom_errorbar(data = dep_statistical_data_long,
	aes(ymin = value - dep_sd, ymax = value + dep_sd),
		size = 1.1, width = .7, alpha = .5) +
geom_point(size = 3.5, alpha = .8) + 
labs(x = "推出率（架次/15分钟）", y = "起飞率（架次/15分钟）", size = 6) +
scale_x_discrete(limits = c(0, 30)) +
scale_x_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30),
		labels = c(0, 5, 10, 15, 20, 25, 30)) +
scale_colour_discrete(name = "统计量", labels = c("平均数", "中位数")) +
scale_shape_discrete(name = "统计量", labels = c("平均数", "中位数")) +
scale_linetype_discrete(name = "统计量", labels = c("平均数", "中位数")) +
theme_bw()



# plot 2
# mean和median的拟合曲线
dep_regression_plot <- ggplot(dep_statistical_data_long,
				aes(x = dep_pb, y = value,
				colour = variable,
				shape = variable,
				linetype = variable))
plot2 <- dep_regression_plot +
geom_point(size = 2) +
geom_smooth(method = 'lm', formula = y ~ poly(x, 2), size = 1.2, alpha = .1) +
labs(x = "推出率（架次/15分钟）", y = "起飞率（架次/15分钟）",
	shape = "11", colour = "11", size = 6) +
scale_x_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30),
	labels = c(0, 5, 10, 15, 20, 25, 30)) +
# 由于颜色、点形状、线性均改变了，所以改变图例时也要将他们同时修改
scale_colour_discrete(name = "统计量", 
	labels = c("平均数拟合线", "中位数拟合线")) +
scale_shape_discrete(name = "统计量", 
	labels = c("平均数拟合线", "中位数拟合线")) +
scale_linetype_discrete(name = "统计量", 
	labels = c("平均数拟合线", "中位数拟合线")) +
theme_bw()

# 一页多图设置
win.graph(width = 14, height = 6)
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(1, 2))

