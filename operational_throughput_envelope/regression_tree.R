# regression tree
# 利用决策树选择N*和Nmax

# 设置路径
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/operational_throughput_envelope")
# read data
window_count_per15 <- read.csv("window_count_feb2may.csv")
# regression tree package 调用分类树
library(rpart)
set.seed(1234)
rt_fit <- rpart(dep_count_per15 ~ arr_count_per15 + pb_count_per15, 
		data = window_count_per15, method = "anova")
rt_fit$cptable

# 可读性更强的画树包
library(maptree)
draw.tree(rt_fit, cex = 0.8, nodeinfo = TRUE, col = gray(0:8 / 8))