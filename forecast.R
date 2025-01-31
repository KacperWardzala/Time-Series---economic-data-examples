library(tidyr)
library(timeSeries)
library(forecast)

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

ts_mleko = ts(mleko, start = c(2018, 1), end = c(2023, 7), frequency = 12)

full_mleko = ts(rola[8], start = c(2018, 1), end = c(2024, 11), frequency = 12)

full_mleko = tsclean(full_mleko)

#szukamy wartości odstających

tsoutliers(ts_mleko) # dwa outliery w indexach 59. 60. 

tsclean(ts_mleko)

plot(ts_mleko, main = 'Ceny mleka') # nie widać tutaj sezonowości, widać pewien trend wzrostowy

monthplot(ts_mleko, main = 'Monthplot cen mleka') # nie widać sezonowości za bardzo

seasonplot(ts_mleko, year.labels = T, year.labels.left = T ,col = rainbow(6), main  = 'Seasonplot cen mleka') #linie łamią się dosyć losowo, ciężko tutaj doszukiwać sie sezonowości. Wyraźny trend wzrostowy w latach. Inflacja?

Acf(ts_mleko, lag.max = 144)
Pacf(ts_mleko, lag.max=144)

lag.plot(ts_mleko, lags = 12, do.lines = F, pch = 20, main = 'lagplot cen mleka') #widać korelację liniową dla opóźnienia rzędu 1

#Mamy zauważalny trend. Sezonowości właściwie nie ma, może lekko zauważalna w okresie 2018-2020

#Usuwamy trend poprzez różnicowanie z opóźnieniem 1

ts_mleko.diff1 = diff(ts_mleko, lag = 1)

#obserwujemy dane po różnicowaniu

plot(ts_mleko.diff1)
Acf(ts_mleko.diff1, lag.max = 144) 
Pacf(ts_mleko.diff1, lag.max = 144)# na wykresie pierwsze opóźnienie jest nadal mocno zauważalne ponad przedział ufnosci. Sugeruje to nam potrzebę ponownego zróżnicowania z opóźnieniem 1


#potwierdzamy przypuszczenia o trendzie na lagplot
lag.plot(ts_mleko.diff1, lags = 12, do.lines = F, pch = 20, main = 'lagplot cen mleka po różnicowaniu z opóźniem 1') #nadal rozproszona,a le rozpoznawalna korelacja w opoznieniu rzedun 1

#przeprowadzamy ponowne różnicowanie z opóźnieniem 1
ts_mleko.diff1.diff1 = diff(ts_mleko.diff1, lag = 1)

#obserwujemy dane po ponownym różnicowaniu

plot(ts_mleko.diff1.diff1)

Acf(ts_mleko.diff1.diff1, lag.max = 144)
Pacf(ts_mleko.diff1.diff1, lag.max = 144)

lag.plot(ts_mleko.diff1.diff1, lags = 12, do.lines = F, pch = 20, main = 'lagplot cen mleka po ponownym różnicowaniu z opóźniem 1') #nadal rozproszona,a le rozpoznawalna korelacja w opoznieniu rzedun 1
# tutaj całkowicie nie widać korelacji w opóźnieniu 1, co świadczy o tym, że dzięki podwójnemu różnicowaniu z opóźnieniem 1 pozbyliśmy się całkowicie trendu


# Dobieranie modelu stacjonarnego

#wybieramy model AR. Na wykresie ACF jest widoczne stopniowe zanikanie, dlatego nie dobieramy modelu MA

#patrzymy na PACF - pik jest teraz na opóźnieniu 2. Więc bierzemy p = 1

AR2.ts_mleko.diff1.diff1 = Arima(ts_mleko, order = c(2,2,0), seasonal = c(0,0,0)) 

summary(AR2.ts_mleko.diff1.diff1)

for.AR2.ts_mleko.diff1.diff1 = forecast(AR2.ts_mleko.diff1.diff1, h=16)
plot(for.AR2.ts_mleko.diff1.diff1, ylim = c(0,400))


# Sprwadzamy jaki model zasugeruje autoarima i porównamy jego kryteria

auto.ts_mleko = auto.arima(ts_mleko) #sugeruje nam model ARIMA(0,1,3)(1,0,0), czyli MA
summary(auto.ts_mleko)

for.auto.ts_mleko = forecast(auto.ts_mleko, h=16)
plot(for.auto.ts_mleko, ylim = c(0,400))

#Na podstawie tej sugestii sprawdzimy jednak model MA12 - na podstawie ACF. To opóźnienie 3 z autoarimy też miałoby sens orzy jednokrotnym różnicowaniu. My jednak robimy dwukrotne, bo ten trend na podstawie lag1, jest nadal mocno wystrzelony

MA12.ts_mleko.diff1.diff1 = Arima(ts_mleko, order = c(0,2,12), seasonal = c(0,0,0))
summary(MA12.ts_mleko.diff1.diff1)

for.MA12.ts_mleko.diff1.diff1 = forecast(MA12.ts_mleko.diff1.diff1, h=16)
plot(for.MA12.ts_mleko.diff1.diff1, ylim = c(0,400))



#tslm trend + AR(2)     oraz      trend + ARIMA(0,1,3)(1,0,0)

ts_mleko.tslm  = tslm(ts_mleko ~ trend)
summary(ts_mleko.tslm)

ts_mleko.tslm.res = ts_mleko.tslm$residuals


par(mfrow = c(3,1))
plot(ts_mleko.tslm.res)
Acf(ts_mleko.tslm.res, lag.max = 144)
Pacf(ts_mleko.tslm.res, lag.max = 144)


auto.arima(ts_mleko.tslm.res) #taki sam model jak wcześniej (0,1,3)(1,0,0)

#prognoza

forecast.trend = forecast(ts_mleko.tslm, h = 16)
plot(forecast.trend.season)

ts_mleko.tslm.AR2 = Arima(ts_mleko.tslm.res, order = c(2,2,0), seasonal = c(0,0,0))
forecast.tslm.AR2 = forecast(ts_mleko.tslm.AR2, h = 16)
forecast.tslm.AR2


forecast.tslm = forecast.trend$mean + forecast.tslm.AR2$mean

plot(forecast.tslm, ylim = c(0,400)) #Tak jakoś strasznie lipnie te wykresy i prognozy wyglądają. To coś może nzaczyć, że ten model AR faktycznie nie jest najlepszy

# sprawdzamy trend trend + ARIMA(0,1,3)(1,0,0)

forecast.trend = forecast(ts_mleko.tslm, h = 16)
plot(forecast.trend.season)

ts_mleko.tslm.auto = Arima(ts_mleko.tslm.res, order = c(0,1,3), seasonal = c(1,0,0))

forecast.tslm.auto = forecast(ts_mleko.tslm.auto , h = 16)
forecast.tslm.auto


forecast.tslm.auto = forecast.trend$mean + forecast.tslm.auto$mean

#plot(forecast.tslm, ylim = c(0,400)) # no tutaj to wygląda już lepiej


#I nie zabardzo mam pomysł co sądzić teraz o tej sezonowości. Sprawdzę tslm z sezonowością

# plot do porównania trening + test

par(mfrow = c(1,1))

plot(full_mleko, ylim = c(0,400))
plot(for.AR2.ts_mleko.diff1.diff1, ylim = c(0,400))
plot(for.auto.ts_mleko, ylim = c(0,400))
plot(for.MA12.ts_mleko.diff1.diff1, ylim = c(0,400))

ts.plot(full_mleko, 
        for.AR2.ts_mleko.diff1.diff1$mean, 
        for.auto.ts_mleko$mean, 
        for.MA12.ts_mleko.diff1.diff1$mean,
        forecast.tslm,
        forecast.tslm.auto,
        gpars=list(xlab="Year", ylab="Cena", 
                   lty=c(1, 2, 3, 4, 5, 6), col=c('black', 'red', 'blue', 'green', 'pink', 'magenta')))
legend("topright", 
       legend=c("Pełny szereg", "AR2 Forecast(2,2,0)(0,0,0)", "Auto Forecast(0,1,3)(1,0,0)", "MA12 Forecast(0,2,12)(0,0,0)", "tslm: trend + AR2", "tslm: trend + auto"),
       col=c('black', 'red', 'blue', 'green', 'pink', 'magenta'), 
       lty=c(1, 2, 3, 4, 5, 6), 
       cex=0.5)

#Na razie wniosek jest taki - lepiej spisuje się nam MA, a nie AR.
#sprawdzimy jeszcze tslm + MA12

