library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)

temps <- read_delim("GaTech/ISYE 6501/Week 3/temps.txt", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)




temps

#days <- temps$DAY
#averages_day <- tibble(days, averages_day)
#colnames(averages_day) <- c('Day', 'Temperature')
#averages_day$Day <- as.Date(averages_day$Day, '%d-%b')



# example data frame
x = data.frame(
  id   = c(1, 1, 2, 2),
  blue = c(1, 0, 1, 0),
  red  = c(0, 1, 0, 1)
)

x
# collapse the data frame
melt(data = x, id.vars = "id", measure.vars = c("blue", "red"))



names <- as.vector(as.character(colnames(temps[,-1])))
temps_agg <- melt(data = temps, id.vars = 'DAY', measure.vars = names)
colnames(temps_agg) <- c('Day', 'Year', 'Temperature')
#new %>%  unite(DAY, variable, COUNTY, TRACT, BLOCK, sep = "", remove = FALSE)
temps_agg$Day <- as.Date(temps_agg$Day, '%d-%b')











colnames(temps) <- c('Day', 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)
temps$Day <- as.Date(temps$Day, '%d-%b')

rownames(temps) <- temps$Day
temps <- as.data.frame(temps)
temps <- temps %>% select(-Day)


sample <- new[1:123,]
rownames(sample) <- sample$DAY
sample <- sample %>% select(value)


value <- new %>% select(value, DAY)
value <- value[1:123,]
rownames(value) <- new[1:123,]$DAY
value <- value %>% select(value)
value <- ts(value)




library(lubridate)
new$DAY <- with(new, DAY + years(2))
new$variable <- as.numeric(as.character(new$variable))
2020 - new$variable[1]


for (i in seq(1:nrow(new))){
  a <- 2020 - new$variable[i]
  new$DAY[i] <- with(new[i], DAY - years(a))
}


new$DAY[2460] <- with(new[2460,], DAY + years(48))
new <- new %>% select(-variable)
colnames(new) <- c('Day', 'Temp')
new[1:369,]
ts <- ts(new[1:369,]$Temp, frequency =123, start = c(1995, 1))



ts1 <- ts(temps, frequency=123, start = c(1996, 1))
ts2 <- ts(t(temps), frequency=123, start = c(1996, 1))
ts3 <- ts(new$Temp, frequency=123, start = c(1996, 1))



temps_agg$Temperature[247:616]

ts4 <- ts(temps_agg$Temperature[1:492], frequency=123, start = c(1996, 1))
m1 <- HoltWinters(ts4, seasonal = 'additive')
plot(fitted(m1)[247:369,'season'])

ts5 <- ts(temps_agg$Temperature[1969:2460], frequency=123, start = c(1996, 1))
m2 <- HoltWinters(ts5, seasonal = 'additive')
plot(fitted(m2)[247:369,'season'])
          
temp1 <- (fitted(m1)[247:369,'season'])
temp2 <- (fitted(m2)[247:369,'season'])
Day <- temps_agg$Day[1:123]

mean(temp1)
model1 <- cusum(temp1, mean(temp1[1:31]), 5, 25)[3]
print(as.Date(model1, origin = "1970-01-01"))

model2 <- cusum(temp2, mean(temp2[1:31]), 5, 25)[3]
print(as.Date(model2, origin = "1970-01-01"))







plot(fitted(m)[1846:1967,'season'])
plot(fitted(m)[1:616,'season'])
plot(fitted(m))

m$gamma
m$call
plot(m$x)

m$seasonal
m$SSE

plot.ts(ts4)

comp <- decompose(ts4)
plot(comp)

plot(comp$seasonal)


adj <- ts4 - comp$seasonal - comp$trend
comp
comp$seasonal
plot(adj)

m$fitted


install.packages('forecast')
library(forecast)
forecast(m, h= 123)


temps_agg$Temperature[1:615]
ts5 <- ts(temps_agg$Temperature[1:615], frequency=123, start = c(1996, 1))
n <- HoltWinters(ts5, seasonal = 'additive')
n$SSE


ts6 <- ts(temps_agg$Temperature[1846:2460], frequency=123, start = c(1996, 1))
n1 <- HoltWinters(ts6, seasonal = 'additive')
n1$SSE


forecast <- predict(n, n.ahead = 123, prediction.interval = T, level = 0.95)
forecast1 <- predict(n1, n.ahead = 123, prediction.interval = T, level = 0.95)
plot(forecast[,'fit'])
lines(forecast1[,'fit'])

plot(cbind(forecast[,'fit'], forecast1[,'fit']))
ts.plot(forecast[,'fit'], forecast1[,'fit'], gpars = list(col = c("black", "red")))


plot(forecast)
plot(forecast1)
plot(temps_agg$Temperature[1:123])
plot(temps_agg$Temperature[1969:2091])

#Use cusum on first three years of seasonality
#Use cusum on last three years of seasonality. 





as.vector(forecast1[,'fit'])


temp <- as.vector(forecast[,'fit'])
Day <- temps_agg$Day[1:123]

#Function that performs the CUSUM
cusum <- function(average, cost, threshold){
  
  count <- 0 
  for (i in 1:length(temp)){
    count <- min(temp[i] - average + cost + count, 0)
    if (abs(count) > abs(threshold)){
      Day <- Day[i]
      break
    }
  }
  
  if (i != length(temp)){
    return(c(cost, threshold, Day))
  } else{
    return ('No change detected')
  }
}
#Example with mu set to 88.8 
example <- cusum(88.75, 5, 25)
print(as.Date(example[3], origin = "1970-01-01"))


















aust <- window(ts3,start=1996)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
autoplot(aust)

autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts") +
  autolayer(fit2, series="HW multiplicative forecasts")


















qcement
plot(qcement)
class(qcement)

qcement.train <- window(qcement, end = c(2012, 4))
qcement.test <- window(qcement, start = c(2013, 1))
qcement.hw <- ets(qcement.train, model = "AAM")
autoplot(forecast(qcement.hw))


install.packages('fpp2')
library(fpp2)  

install.packages('fpp')
library(fpp)

window(sample)
aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")

autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))







install.packages('stats')
library(stats)

# NOT RUN {
require(graphics)

## Seasonal Holt-Winters
co2
class(co2)
class(temps)
(m <- HoltWinters(co2))
plot(m)
plot(fitted(m))


(m <- HoltWinters(AirPassengers, seasonal = "mult"))
plot(m)

## Non-Seasonal Holt-Winters
x <- uspop + rnorm(uspop, sd = 5)
m <- HoltWinters(x, gamma = FALSE)
plot(m)

## Exponential Smoothing
m2 <- HoltWinters(x, gamma = FALSE, beta = FALSE)
lines(fitted(m2)[,1], col = 3)
# }














































