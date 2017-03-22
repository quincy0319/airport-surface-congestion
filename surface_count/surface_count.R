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
# data preparation
day1 <- rep(c("day1"), times = 96)
day2 <- rep(c("day2"), times = 96)
day3 <- rep(c("day3"), times = 96)
day4 <- rep(c("day4"), times = 96)
window_day1 <- seq(from = 1, to = 96, by = 1)
window_day2 <- seq(from = 1, to = 96, by = 1)
window_day3 <- seq(from = 1, to = 96, by = 1)
window_day4 <- seq(from = 1, to = 96, by = 1)
surface_count_per_window <- surface_count[c(1:96, (96 * 30 + 1):(96 * 31), 
					(96 * 66 + 1):(96 * 67),
					(96 * 99 + 1):(96 * 100))]
day1_4 <- c(day1, day2, day3, day4)
window1_4 <- c(window_day1, window_day2, window_day3, window_day3)
surface_count_plot_day1_4 <- data.frame(window1_4, 
				surface_count_per_window, day1_4)
# time window ~ surface count plot
library(ggplot2)
plot1 <- ggplot(surface_count_plot_day1_4, 
		aes(x = window1_4, y = surface_count_per_window, 
			linetype = day1_4, colour = day1_4))
win.graph(width = 10, height = 6)
plot1 + geom_line(size = 1.2, alpha = .8) +
labs(x = "时刻", y = "场面航班数/15分钟", linetype = "日期", colour = "日期" , size = 8) +
theme_bw() +
scale_x_continuous(limits = c(0, 96), breaks = c(0, 24, 48, 72, 96),
		labels = c("0000", "0600", "1200", "1800", "2400"))

###############################################################################

# max surface count distribution
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/surface_count")
surface_count_max <- data.matrix(read.csv("surface_count_max.csv", header = FALSE))
serial_num <- seq(from = 1, to = 129, by = 1)
surface_count_analysis <- data.frame(serial_num, surface_count_max)
# exclude empty data from 1st apr 
surface_count_analysis <- surface_count_analysis[-60,]
names(surface_count_analysis) <- c("serial_num", "surface_count_maxx")
a <- rep("field operation", times = 126)
b <- rep("standard normal", times = 126)
library(ggplot2)
plot2 <- ggplot(surface_count_analysis, 
	aes(x = surface_count_maxx, y = ..density..))
win.graph(width = 7, height = 3)
plot2 +
geom_histogram(binwidth = 2, alpha = .6, colour = "black") +
geom_line(stat = "density", size = 1.2, 
	position = "identity", colour = "red") +
xlim(0, 85) +
xlab("地面航班数最大值（架次/15分钟）") +
ylab("频率") +
theme_bw()

# distribution analysis
# add random disturbance
# k-s test
set.seed(2333)
ks.test(jitter(surface_count_max), "pnorm", 
	mean(surface_count_max), sd(surface_count_max))

###############################################################################

# standard normal distribution
library(ggplot2)
plot_area <- function(min,max){
function(x){
	y <- dnorm(x)
	y[x < min | x > max] <- NA
	return(y)
	}
}

pnorm(2.983)  #p-value

ggplot(data.frame(x = c(-4, 4)), aes(x = x)) + 
stat_function(fun = plot_area(3, 4), 
	geom = "area", fill = "red", alpha = 0.2) +
stat_function(fun=dnorm) + 
geom_text(aes(y=0, x=c(3),label=paste("x=", "μ + 3σ"), vjust=1))


###############################################################################

# daily surface count 
setwd("C:/Users/QYF/Documents/Visual Studio 2015/Projects/airport_congestion/surface_count")
surface_count_max <- data.matrix(read.csv("surface_count_max.csv", header = FALSE))
# delete fake data
daily_max <- surface_count_max[1:120]
daily_max <- daily_max[-60]
# time seq
date_seq_analysis <- strptime("2014-02-01", "%Y-%m-%d") + (3600 * 24) * (0:119)
date_seq_analysis <- date_seq_analysis[-60]
## remember to delete data from 1st apr.
daily_surface_max <- data.frame(date_seq_analysis, daily_max)
aa <- floor(mean(daily_max))
bb <- floor(sd(daily_max))
library(ggplot2)
plot3 <- ggplot(daily_surface_max, aes(x = date_seq_analysis, y = daily_max))
plot3 + geom_line(size = 1.1, colour = "red", alpha = .7) +
geom_point(size = 2, shape = 17) +
geom_hline(yintercept = c(72, 66), colour = "blue", size = 1.3, linetype = 2)
