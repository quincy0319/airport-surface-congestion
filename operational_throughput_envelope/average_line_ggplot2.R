setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/operational_throughput_envelope")
library(ggplot2)
library(reshape2)
dep_statistical_data_long <- melt(dep_statistical_data, id = "dep_pb")

# 底层赋值给变量
dep_plot <- ggplot(dep_statistical_data, aes(dep_pb, dep_mean)) +
		labs(x = "推出率", y = "起飞率", title = "推出率-起飞率曲线") +
		xlim(0, 30)
# 添加图层
# 绘制errorbar
dep_plot +
theme(text = element_text(family = "SimSun", size = 12)) + 
geom_errorbar(data = dep_statistical_data, 
	aes(ymin = dep_mean - dep_sd, ymax = dep_mean + dep_sd),
	width = .5) + 
geom_point(size = 3) +
geom_smooth() 
