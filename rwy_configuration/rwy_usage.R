# rwy usage
# feb to may
rwy_number <- c("01", "18L", "18R", "19", "36L", "36R")
rwy_dep <- c(21778, 5852, 1548, 2458, 20363, 40662)
rwy_arr <- c(29803, 1142, 4939, 4886, 33569, 19002)

# feb
rwy_dep_feb <- c(5318, 450, 141, 180, 4982, 10484)
rwy_arr_feb <- c(7793, 195, 302, 325, 8365, 4799)

rwy_usage <- data.frame(rwy_number, rwy_dep, rwy_arr)
rwy_usage_feb <- data.frame(rwy_number, rwy_dep_feb, rwy_arr_feb)