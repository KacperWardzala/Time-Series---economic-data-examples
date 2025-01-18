data <- read.csv2("C:/Users/kacwa/Downloads/CENY_2967_CREL_20250118181146/CENY_2967_CREL_20250118181146.csv")
head(data)

# Installing the tidyr package if it's not already installed
if (!require("tidyr")) install.packages("tidyr")
if (!require("timeSeries")) install.packages("timeSeries")
if (!require("forecast")) install.packages("forecast")

# Loading the package
library(tidyr)
library(timeSeries)
library(forecast)

# Transforming the table: each agricultural product as a separate column
transformed_data <- data %>%
  pivot_wider(
    names_from = Produkty.rolne,  
    values_from = Wartosc         
  )

# Saving the results to a new CSV file (optional)
write.csv(transformed_data, "transformed_data.csv", row.names = FALSE)

# Displaying the transformed data
print(transformed_data)

agriculture = cbind(transformed_data[,3:4], transformed_data[,8:13])
agriculture = sort_by(agriculture, agriculture[2])

write.csv2(agriculture, 'agricultural_product_prices.csv')

barley = agriculture[,3]
triticale = agriculture[,4]
oats = agriculture[,5]
corn = agriculture[,6]
milk = agriculture[,8]


ts_milk = ts(milk, start = c(2018, 1), end = c(2024, 12), frequency = 12)

monthplot(ts_milk)

seasonplot(ts_milk)

Acf(ts_milk)

ts_milk = tsclean(ts_milk)

ts_milk = log(ts_milk)

ts_milk.diff12 = diff(ts_milk, lag = 12)

plot(ts_milk.diff12)

Acf(ts_milk.diff12, lag.max = 120)
Pacf(ts_milk.diff12, lag.max = 120)

ts_milk.diff12.diff1 = diff(ts_milk.diff12, lag = 1)

plot(ts_milk.diff12.diff1)
Acf(ts_milk.diff12.diff1, lag.max = 120)
Pacf(ts_milk.diff12.diff1, lag.max = 120)

lag.plot(ts_milk.diff12.diff1, lags = 12, do.lines = F, pch = 20)

# v2

ts_milk.diff12.diff1.diff1 = diff(ts_milk.diff12.diff1, lag = 1)
Acf(ts_milk.diff12.diff1.diff1, lag.max = 120)
Pacf(ts_milk.diff12.diff1.diff1, lag.max = 120)

lag.plot(ts_milk.diff12.diff1.diff1, lags = 12, do.lines = F, pch = 20)

# MA 36

# Without trend and seasonality
MA36.ts_milk.diff12.diff1.diff1 = Arima(ts_milk.diff12.diff1.diff1, order = c(0, 0, 36), seasonal = c(0, 0, 0))
summary(MA36.ts_milk.diff12.diff1.diff1)

auto.arima(ts_milk)
