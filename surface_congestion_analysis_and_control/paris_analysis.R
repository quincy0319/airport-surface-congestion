# 分析dep arr pb heavy的关系
# 使用散点图矩阵 pairs()函数
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/surface_congestion_analysis_and_control")
window_count <- read.csv("window_count_feb2may.csv")
pairs(window_count)