# rwy usage
# feb to may
rwy_number <- c("01", "18L", "18R", "19", "36L", "36R", "01", 
		"18L", "18R", "19", "36L", "36R")
rwy_stat <- c("dep", "dep", "dep", "dep", "dep", "dep", 
		"arr","arr","arr","arr","arr","arr" )
rwy_dep <- c(21778, 5852, 1548, 2458, 20363, 40662)
rwy_arr <- c(29803, 1142, 4939, 4886, 33569, 19002)
rwy_flow <- c(21778, 5852, 1548, 2458, 20363, 40662,
		29803, 1142, 4939, 4886, 33569, 19002)

# feb
rwy_dep_feb <- c(5318, 450, 141, 180, 4982, 10484)
rwy_arr_feb <- c(7793, 195, 302, 325, 8365, 4799)
rwy_flow_feb <- c(5318, 450, 141, 180, 4982, 10484,
		7793, 195, 302, 325, 8365, 4799)

# dataframe
rwy_usage <- data.frame(rwy_number, rwy_flow, rwy_stat)
rwy_usage_feb <- data.frame(rwy_number, rwy_flow_feb, rwy_stat)

# plot of runway usage
library(ggplot2)
plot1 <- ggplot(rwy_usage, 
	aes(x = rwy_number, y = rwy_flow, fill = rwy_stat))
plot1 + geom_bar(stat = "identity", position = "dodge")