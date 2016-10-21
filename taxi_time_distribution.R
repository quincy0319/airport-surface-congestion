# 不同跑道航班在主运行模式下的滑行时间研究
# taxi time distribution
# load dep taxi time data
taxi_dep_36l <- as.numeric(subset((read.csv("dep_feb_36l_processed.csv")),
	flight_chosen_dep_36l == 1)$dep_taxi)
taxi_dep_36r <- as.numeric(subset((read.csv("dep_feb_36r_processed.csv")), 
	flight_chosen_dep_36r == 1)$dep_taxi)
taxi_dep_01 <- as.numeric(subset((read.csv("dep_feb_01_processed.csv")), 
	flight_chosen_dep_01 == 1)$dep_taxi)
# load arr taxi time data
taxi_arr_36l <- as.numeric(subset((read.csv("arr_feb_36l_processed.csv")), 
	flight_chosen_arr_36l == 1)$arr_taxi)
taxi_arr_36r <- as.numeric(subset((read.csv("arr_feb_36r_processed.csv")), 
	flight_chosen_arr_36r == 1)$arr_taxi)
taxi_arr_01 <- as.numeric(subset((read.csv("arr_feb_01_processed.csv")), 
	flight_chosen_arr_01 == 1)$arr_taxi)

# taxi_hist函数,用来做滑行时间的直方图及概率密度
taxi_hist <- function(x) {
	hist(x, probability = T)
	lines(density(x), col = 'red', lwd = 2)
	rug(x)
}
taxi_hist(taxi_dep_36l)
taxi_hist(taxi_dep_36r)
taxi_hist(taxi_dep_01)
taxi_hist(taxi_arr_36l)
taxi_hist(taxi_arr_36r)
taxi_hist(taxi_arr_01)