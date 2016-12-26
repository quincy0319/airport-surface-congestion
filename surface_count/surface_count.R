# airport surface count 
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/surface_count")
# read surface count data
surface_count <- read.csv("surface_count_feb2may.csv", header = FALSE)
surface_count <- as.vector(as.matrix(surface_count))
head(surface_count)
# read dep arr pb data
window_count_feb2may <- read.csv("window_count_feb2may_per15.csv", header = TRUE)
dep_count <- window_count_feb2may[, 1]
arr_count <- window_count_feb2may[, 2]
pb_count <- window_count_feb2may[, 3]
serial_num <- seq(from = 1, to = 11520, by = 1)
# surface count vs time plot
day1 <- rep(c("day1"), times = 96)
day2 <- rep(c("day2"), times = 96)
day3 <- rep(c("day3"), times = 96)
day4 <- rep(c("day4"), times = 96)
window_day1 <- seq(from = 1, to = 96, by = 1)
window_day2 <- seq(from = 1, to = 96, by = 1)
window_day3 <- seq(from = 1, to = 96, by = 1)
window_day4 <- seq(from = 1, to = 96, by = 1)
surface_count_per_window <- surface_count[1:(96 * 4)]
day1_4 <- c(day1, day2, day3, day4)
window1_4 <- c(window_day1, window_day2, window_day3, window_day3)
surface_count_plot_day1_4 <- data.frame(window1_4, 
				surface_count_per_window, day1_4)
library(ggplot2)
plot1 <- ggplot(surface_count_plot_day1_4, 
		aes(x = window1_4, y = surface_count_per_window, 
			colour = day1_4))
plot1 + geom_line()