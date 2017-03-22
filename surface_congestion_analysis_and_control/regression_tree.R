# regression tree
# ���þ�����ѡ��N*��Nmax

# ����·��
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/surface_congestion_analysis_and_control")
# read data
window_count_per15 <- read.csv("window_count_feb2may.csv")
# regression tree package ���÷�����
library(rpart)
set.seed(1234)
rt_fit <- rpart(dep_count_per15 ~ arr_count_per15 + pb_count_per15, 
		data = window_count_per15, method = "anova")
rt_fit$cptable

# �ɶ��Ը�ǿ�Ļ�����
library(maptree)
draw.tree(rt_fit, cex = 0.8, nodeinfo = TRUE, col = gray(0:8 / 8))