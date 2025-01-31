# Ładowanie pakietu
library(tidyr)
library(timeSeries)
library(forecast)

rola = read.csv2('https://raw.githubusercontent.com/KacperWardzala/Time-Series---economic-data-examples/refs/heads/main/ceny_produktow_rolniczych.csv')

trening_rola = rola[1:67,]
test_rola =  rola[68:83,] # Usuwamy dane z grudnia 2024 - są puste

jeczmien = trening_rola[,3]
pszenzyto = trening_rola[,4]
owies = trening_rola[,5]
kukurydza = trening_rola[,6]
mleko = trening_rola[,8]

test_mleko = test_rola[,8]

cor.test(jeczmien,mleko)

model = lm(mleko~jeczmien+pszenzyto+owies+kukurydza)
model2 = lm(mleko~pszenzyto+owies+kukurydza)
model3 = lm(mleko~pszenzyto+owies)
model4 = lm(mleko~owies)
 
summary(model)
summary(model2)
summary(model3)
summary(model4)

plot(mleko, owies)
cor(mleko,owies)

#szeregi czasowe testowe, poprawne analizy w pliku forecast -------------------------------------------

#install.packages('timeSeries')



#install.packages("forecast")



ts_mleko = ts(mleko, start = c(2018, 1), end = c(2023, 7), frequency = 12)
ts_test = ts(mleko, start = c(2023, 8), end = c(2024, 11), frequency = 12)

ts_mleko = tsclean(ts_mleko)
ts_test = tsclean(ts_test)

monthplot(ts_mleko)

seasonplot(ts_mleko)

Acf(ts_mleko, lag=144)
Pacf(ts_mleko, lag=144)

plot(ts_mleko) #nie widać tutaj wyraźnej sezonowości. Nie ma takich powtarzalnych zmian

lag.plot(ts_mleko, lags = 12, do.lines = F, pch = 20) #w lag 12 nie ma korelacji wiec nie mamy tutaj sezonowsoci. Za to silny trend
#usuwanie trendu

ts_mleko.diff1 = diff(ts_mleko, lag = 1)

Acf(ts_mleko.diff1, lag.max = 144)
Pacf(ts_mleko.diff1, lag.max = 144)

ts_mleko.diff1.diff1 = diff(ts_mleko.diff1, lag = 1)

Acf(ts_mleko.diff1.diff1, lag.max = 144)
Pacf(ts_mleko.diff1.diff1, lag.max = 144)


lag.plot(ts_mleko.diff1, lags = 12, do.lines = F, pch = 20)


#ts_mleko = log(ts_mleko)

ts_mleko.diff12 = diff(ts_mleko, lag = 12)

plot(ts_mleko.diff12)

Acf(ts_mleko.diff12, lag.max = 120)
Pacf(ts_mleko.diff12, lag.max = 120)

ts_mleko.diff12.diff1 = diff(ts_mleko.diff12, lag = 1)

plot(ts_mleko.diff12.diff1)
Acf(ts_mleko.diff12.diff1, lag.max = 120)
Pacf(ts_mleko.diff12.diff1, lag.max = 120)

lag.plot(ts_mleko.diff12.diff1, lags = 12, do.lines = F, pch = 20)

#v2


ts_mleko.diff12.diff1.diff1  = diff(ts_mleko.diff12.diff1 , lag = 1)
Acf(ts_mleko.diff12.diff1.diff1, lag.max = 120)
Pacf(ts_mleko.diff12.diff1.diff1, lag.max = 120)

lag.plot(ts_mleko.diff12.diff1.diff1, lags = 12, do.lines = F, pch = 20)

#MA 12 to raczej nie, bo mamy zanikanie w ACF ----------------------

#bez trendu i sezonowosci
MA12.ts_mleko.diff12.diff1.diff1 = Arima(ts_mleko, order = c(0,0,12), seasonal = c(0,1,0))
summary(MA12.ts_mleko.diff12.diff1.diff1)


auto.arima(ts_mleko)

# model AR ---------------------------------------
# dla AR 10
AR12.ts_mleko.diff1.diff1 = Arima(ts_mleko, order = c(10,2,0), seasonal = c(0,0,0)) 
for.AR12.ts_mleko.diff1.diff1 = forecast(AR12.ts_mleko.diff1.diff1, h=12)
plot(for.AR12.ts_mleko.diff1.diff1)
plot(tsclean(ts(rola[,8])))
plot(ts_test)

#reszty

coefs = AR12.ts_mleko.diff1.diff1$coef
coefs.se = sqrt(diag(AR12.ts_mleko.diff1.diff1$var.coef))
ind = abs(coefs/(coefs.se*1.96))

signif = which(ind >= 1)
signif

temp.fixed = numeric(10)
temp.fixed[signif] = NA

temp.fixed

AR12.ts_mleko.diff1.diff1.signif = Arima(ts_mleko, order = c(10,2,0), seasonal = c(0,0,0), fixed = temp.fixed)


fore.AR12.ts_mleko.diff1.diff1.signif = forecast(AR12.ts_mleko.diff1.diff1.signif, h=12)

plot(fore.AR12.ts_mleko.diff1.diff1.signif)

plot(fore.AR12.ts_mleko.diff1.diff1.signif$residuals)

Acf(fore.AR12.ts_mleko.diff1.diff1.signif$residuals, lag.max=144)
Pacf(fore.AR12.ts_mleko.diff1.diff1.signif$residuals, lag.max=144) # tutaj w obu mamy biały szum - mieszczą się w przedziałach istotnosci









AR12.ts_mleko.diff12.diff1.diff1 = Arima(ts_mleko, order = c(12,2,0), seasonal = c(0,1,0)) # teraz to jest dobrze
summary(AR12.ts_mleko.diff12.diff1.diff1)

for.AR12.ts_mleko.diff12.diff1.diff1 = forecast(AR12.ts_mleko.diff12.diff1.diff1, h=12)

plot(for.AR12.ts_mleko.diff12.diff1.diff1)


coefs = AR12.ts_mleko.diff12.diff1.diff1$coef
coefs.se = sqrt(diag(AR12.ts_mleko.diff12.diff1.diff1$var.coef))
ind = abs(coefs/(coefs.se*1.96))

signif = which(ind >= 1)
signif

temp.fixed = numeric(12)
temp.fixed[signif] = NA

temp.fixed

#AR12.ts_mleko.diff12.diff1.diff1.signif = Arima(ts_mleko, order = c(12,1,0), seasonal = c(0,1,0), fixed = temp.fixed)
AR12.ts_mleko.diff12.diff1.diff1.signif = Arima(ts_mleko, order = c(12,2,0), seasonal = c(0,1,0), fixed = temp.fixed)


fore.AR12.ts_mleko.diff12.diff1.diff1.signif = forecast(AR12.ts_mleko.diff12.diff1.diff1.signif, h=12)

plot(fore.AR12.ts_mleko.diff12.diff1.diff1.signif$residuals)

Acf(fore.AR12.ts_mleko.diff12.diff1.diff1.signif$residuals, lag.max=144)
Pacf(fore.AR12.ts_mleko.diff12.diff1.diff1.signif$residuals, lag.max=144)

Box.test(fore.AR12.ts_mleko.diff12.diff1.diff1.signif$residuals) #ho: jest losowość. Nie odrzucamy H0

shapiro.test((fore.AR12.ts_mleko.diff12.diff1.diff1.signif$residuals)) #nie ma normalności

#t.test((fore.AR12.ts_mleko.diff12.diff1.diff1.signif$residuals)) nie robimy t testu, bo nie ma normalności


# na podstawie auto.arimy ------------------------
auto.arima(ts_mleko)
#ARIMA(1,1,0)(1,0,0)

auto.ts_mleko = Arima(ts_mleko, order = c(1,1,0), seasonal = c(1,0,0))
summary(auto.ts_mleko)



for.auto.ts_mleko = forecast(auto.ts_mleko, h=12)

plot(for.auto.ts_mleko)


coefs = auto.ts_mleko$coef
coefs.se = sqrt(diag(auto.ts_mleko$var.coef))
ind = abs(coefs/(coefs.se*1.96))

signif = which(ind >= 1)
signif

temp.fixed = numeric(2)
temp.fixed[signif] = NA

temp.fixed

#AR12.ts_mleko.diff12.diff1.diff1.signif = Arima(ts_mleko, order = c(12,1,0), seasonal = c(0,1,0), fixed = temp.fixed)
auto.ts_mleko.signif = Arima(ts_mleko, order = c(1,1,0), seasonal = c(1,0,0), fixed = temp.fixed)


fore.auto.ts_mleko.signif = forecast(auto.ts_mleko.signif, h=12)

plot(fore.auto.ts_mleko.signif$residuals)

Acf(fore.auto.ts_mleko.signif$residuals, lag.max=144)
Pacf(fore.auto.ts_mleko.signif$residuals, lag.max=144)

Box.test(fore.auto.ts_mleko.signif$residuals) #ho: jest losowość. Nie odrzucamy H0

shapiro.test(fore.auto.ts_mleko.signif$residuals) #Mamy normalnosc
