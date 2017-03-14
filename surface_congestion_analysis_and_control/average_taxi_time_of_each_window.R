# average taxi time in each window
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/surface_congestion_analysis_and_control")
dep_processed <- read.csv("dep_processed.csv")
dep_taxi_per_window <- vector(mode = "numeric", length = 96*120)
for (i in 1:120) {
	dep_each_day <- subset(dep_processed, day_of_case == i)
	for (j in 1:96) {
		dep_each_window <- subset(dep_each_day, window_of_day == j)
		average_taxi_time_in_window <- mean(dep_each_window$dep_taxi)
		dep_taxi_per_window[(i - 1) * 96 + j] <- average_taxi_time_in_window
	}
}
dep_taxi_per_window[is.na(dep_taxi_per_window)] <- 0
window_count <- data.frame(window_count, dep_taxi_per_window)
write.csv(window_count, "window_count.csv", row.names = F)