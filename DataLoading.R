#Delete everything until now
rm(list=ls())
library('ggplot2')
library("runner")

setwd("S:/Acctn/Trinity/Modules/Dissertation/Dissertation/Factor_Model")
fileName <- './data/BTC_Data.csv'

data <- read.csv(fileName, header=TRUE, sep=",")
colnames(data) = c('Date','Close','Log Price') # Change column names
##Bitcoin_data$Date <- as.Date(Bitcoin_data$Date, format = "%m/%d/%Y")

data$Date <- as.Date(data$Date, format = "%d/%m/%Y") #Convert string dates into R dates
key_window = as.Date('2017-07-01') #set critical window
Bitcoin_data <- subset(data,data$Date >= key_window)
summary(Bitcoin_data)

#Setting t value for model
gtd <- as.Date("2016-12-31")
Bitcoin_data$t <- 2017 + as.numeric(difftime(Bitcoin_data$Date,gtd)/365)

#Plotting all price data
p <- ggplot(data = data,aes(x=Date,y=Close))
p + geom_line(size=1) + xlab('Date') + ylab('Price') +  theme_bw() 

#### Calculating volatility of close price

results <- vector(mode = "integer", length = nrow(Bitcoin_data))
for (i in 1:nrow(Bitcoin_data)){
  results[i]<-sd(Bitcoin_data$`Log Price`[i:1], na.rm=TRUE)
}
results[is.na(results)] <- 0
results <- mean_run(results,k=2)
Bitcoin_data<-cbind(Bitcoin_data, results)
colnames(Bitcoin_data)[5] <- "volatility"
summary(Bitcoin_data)
head(Bitcoin_data)
#Plotting key window price data
p <- ggplot(data = Bitcoin_data,aes(x=Date,y=Close))
p + geom_line(size=1) + xlab('Date') + ylab('Price') + theme_bw()




###################Below data analysis specific to 2020 bubble and crash

rm(list=ls())
library("runner")
setwd("S:/Acctn/Trinity/Modules/Dissertation/Dissertation/Factor_Model")
#fileName <- './data/BTC_USD_2013-10-01_2020-05-26-CoinDesk.csv'
fileName <- './data/BTC_USD_2013-10-01_2020-08-13-CoinDesk.csv'

Bitcoin_data <- read.csv(fileName, header=TRUE, sep=",")
colnames(Bitcoin_data) <- c("Currency", "Date","Close", "Open","High", "Low")
Bitcoin_data$Date <- as.Date(Bitcoin_data$Date, format= "%Y-%m-%d")

######New window for 2019
key_window = as.Date('2019-10-31') #set critical window for 2019(for testing only)
Bitcoin_data <- subset(Bitcoin_data,Bitcoin_data$Date >= key_window)

summary(Bitcoin_data)

#Setting t value for model
gtd <- as.Date("2018-12-31")
Bitcoin_data$t <- 2019 + as.numeric(difftime(Bitcoin_data$Date,gtd)/365)

Bitcoin_data$`Log Price` <- log(Bitcoin_data$Close)

## Calculating volatility of bitcoin
results <- vector(mode = "integer", length = nrow(Bitcoin_data))
for (i in 1:nrow(Bitcoin_data)){
  results[i]<-sd(Bitcoin_data$`Log Price`[i:1], na.rm=TRUE)
}
results[is.na(results)] <- 0
results <- mean_run(results,k=2)
Bitcoin_data<-cbind(Bitcoin_data, results)
colnames(Bitcoin_data)[9] <- "volatility"
summary(Bitcoin_data)
head(Bitcoin_data)

Bitcoin_data <- subset(Bitcoin_data,Bitcoin_data$Date >= as.Date("2017-07-01"))

#Plotting key window price data
p <- ggplot(data = Bitcoin_data,aes(x=Date,y=Close))
p + geom_line(size=1) + xlab('Date') + ylab('Price') + theme_bw() + scale_x_date(date_labels = "%Y-%m", date_breaks = "4 month") + geom_vline(xintercept = as.Date("2017-12-17"), color = "red", size=1) + geom_vline(xintercept = as.Date("2020-02-14"), color = "green", size=1) + geom_vline(xintercept = as.Date("2020-08-11"), color = "blue", size=1)+ geom_text(aes(x = as.Date("2017-12-17") , y= 5000, label = "tc1")) + geom_text(aes(x = as.Date("2020-02-14") , y= 5000, label = "tc2")) + geom_text(aes(x = as.Date("2020-08-11") , y= 5000, label = "tc3")) 



#########################
########################### Data Analysis for other assets

Bitcoin_data <- subset(Bitcoin_data,Bitcoin_data$Date >= as.Date("2017-01-01") & Bitcoin_data$Date <= as.Date("2020-08-10"))


install.packages("data.table")
library("data.table")

library('stats')
summary(Bitcoin_data$Close)
qqnorm(Bitcoin_data$Close)
qqline(Bitcoin_data$Close)
acf(Bitcoin_data$Close)

returns <- diff(Bitcoin_data$Close)/Bitcoin_data$Close[-length(Bitcoin_data$Close)]
returnBitcoin_data <- data.frame(Bitcoin_data$Date[-length(Bitcoin_data$Date)],returns)
plot(returnBitcoin_data$Bitcoin_data.Date..length.Bitcoin_data.Date.., returnBitcoin_data$returns, xlab="Date", ylab="Returns", typ='l')

summary(returnBitcoin_data$returns)
qqnorm(returnBitcoin_data$returns)
qqline(returnBitcoin_data$returns)
acf(returnBitcoin_data$returns)
sd(returnBitcoin_data$returns)

hist(returnBitcoin_data$returns)

#Comparison with Gold 
install.packages('PerformanceAnalytics')
install.packages('tseries')

library('PerformanceAnalytics')
library('zoo')
library('tseries')

GOLD.prices = get.hist.quote(instrument="GLD", start="2017-01-01",
                             end="2020-08-10", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="d", retclass="ts")
goldBitcoin_data <- data.frame(GOLD.prices)

GOLDreturns <- diff(GOLD.prices)/GOLD.prices[-length(GOLD.prices)]
GOLDreturnBitcoin_data <- data.frame(GOLDreturns)

install.packages("Quandl")

##Comparison with oil_data 
library(Quandl)
oil_data = Quandl("OPEC/ORB", order="asc", start_date="2017-01-01", end_date="2020-08-10")

alldates <- data.table(date=seq.Date(min(oil_data$Date), max(oil_data$Date), by="day"))
dt2 <- merge(oil_data, alldates, by.x='Date', by.y='date', all=TRUE)

oil_datareturns <- diff(dt2$Value)/dt2$Value[-length(dt2$Value)]
oil_datareturnBitcoin_data <- data.frame(oil_datareturns)

#Comparison with US Real Estate Index data
RealEstate_data = get.hist.quote(instrument="IYR", start="2017-01-01",
                                 end="2020-08-10", quote="AdjClose",
                                 provider="yahoo", origin="1970-01-01",
                                 compression="d", retclass="ts")
RealEstate_dataBitcoin_data <- data.frame(RealEstate_data)
RealEstate_datareturns <- diff(RealEstate_data)/RealEstate_data[-length(RealEstate_data)]
RealEstate_datareturnBitcoin_data <- data.frame(RealEstate_datareturns)


#Compare with S&P500 
SP500_data = get.hist.quote(instrument="^GSPC", start="2017-01-01",
                            end="2020-08-10", quote="AdjClose",
                            provider="yahoo", origin="1970-01-01",
                            compression="d", retclass="ts")
SP500_dataBitcoin_data <- data.frame(SP500_data)
SP500_datareturns <- diff(SP500_data)/SP500_data[-length(SP500_data)]
SP500_datareturnBitcoin_data <- data.frame(SP500_datareturns)

#Fix dataframes for corr analysis

oil_datapricefixed <- na.locf(dt2)
goldpricefixed <- na.locf(goldBitcoin_data)
RealEstate_datapricefixed <- na.locf(RealEstate_dataBitcoin_data)
SP500_datapricefixed <- na.locf(SP500_dataBitcoin_data)


tail(oil_datapricefixed)
goldpricefixed
RealEstate_datapricefixed
SP500_datapricefixed

### To plot oil_data Returns
n_plot = nrow(oil_datareturnBitcoin_data)
ss_plot <- as.Date('2017-01-01')
from_plot <- seq(from = ss_plot, by = 1, length.out = n_plot)
oil_datareturnBitcoin_data$Date <- from_plot

plot(oil_datareturnBitcoin_data$Date, oil_datareturnBitcoin_data$oil_datareturns, xlab="Date", ylab="Returns", typ='l')


### To plot gold Returns
n_plot = nrow(GOLDreturnBitcoin_data)
ss_plot <- as.Date('2017-01-01')
from_plot <- seq(from = ss_plot, by = 1, length.out = n_plot)
GOLDreturnBitcoin_data$Date <- from_plot

plot(GOLDreturnBitcoin_data$Date, GOLDreturnBitcoin_data$Adjusted, xlab="Date", ylab="Returns", typ='l')

### To plot US Real Estate Index Returns
n_plot = nrow(RealEstate_datareturnBitcoin_data)
ss_plot <- as.Date('2017-01-01')
from_plot <- seq(from = ss_plot, by = 1, length.out = n_plot)
RealEstate_datareturnBitcoin_data$Date <- from_plot

plot(RealEstate_datareturnBitcoin_data$Date, RealEstate_datareturnBitcoin_data$Adjusted, xlab="Date", ylab="Returns", typ='l')

### To plot S&P500 Returns
n_plot = nrow(SP500_datareturnBitcoin_data)
ss_plot <- as.Date('2017-01-01')
from_plot <- seq(from = ss_plot, by = 1, length.out = n_plot)
SP500_datareturnBitcoin_data$Date <- from_plot

plot(SP500_datareturnBitcoin_data$Date, SP500_datareturnBitcoin_data$Adjusted, xlab="Date", ylab="Returns", typ='l')


nrow(returnBitcoin_data)
nrow(SP500_datareturnBitcoin_data)
nrow(RealEstate_datareturnBitcoin_data)
nrow(GOLDreturnBitcoin_data) 
nrow(oil_datareturnBitcoin_data)
oil_datapricefixed
