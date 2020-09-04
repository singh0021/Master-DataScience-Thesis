library('cmaes')
library('quantmod')
library('zoo')
library('nortest')
library('Metrics')
library('stats')
library('dplyr')
library('reshape2')
library('grid')


#Creating time series for rolling t1 time window
ndays <- 20
ss <- as.Date('2017-08-01')
from <- seq(from = ss, by = 3, length.out = ndays)
to <- as.Date("2017-12-01")

result_test <- as.data.frame(NULL)
result_test1 <- as.data.frame(NULL)
result_test2 <- as.data.frame(NULL)
Expect_Price <- as.data.frame(NULL)
vec_control <- data.frame(maxit = c(100)) 

n_run = 10

for(i in 1:ndays){
  rBitcoin_data <- subset(Bitcoin_data, Bitcoin_data$Date >= from[i] & Bitcoin_data$Date <= to)
  result_test2 <- as.data.frame(NULL)
  for(j in 1:n_run){
    test <- cma_es(c(0.01, 6, max(rBitcoin_data$t)+0.01), residual_obj, lower=c(0.01,6 , max(rBitcoin_data$t)+0.01), upper=c(1, 16, max(rBitcoin_data$t)+0.25), control=vec_control)
    result_test2 <- rbind(result_test2, test$par)
    
  }
  result_test1 <- rbind(result_test1,apply(result_test2,2,mean))
  
}
colnames(result_test1) <- c('m','omega','tc')
result_test <- result_test1
last_row <- tail(rBitcoin_data, 1)
result_test$Date_to_Peak <- as.integer((result_test$tc-last_row$t)/(1/365))
View(result_test)

plot_long <- melt(result_test,by = '')

p1 <- ggplot(NULL,aes(x=from,y=result_test[,1])) + geom_point()+geom_line() + scale_x_date(breaks=as.Date(c("2017-09-01","2017-09-15", "2017-08-01","2017-08-15")))+
  ylim(0,1) + xlab('Date') + ylab('Beta') + theme_bw()
p1
p2 <- ggplot(NULL,aes(x=from,y=result_test[,2])) + geom_point()+geom_line() + scale_x_date(breaks=as.Date(c("2017-08-01","2017-08-15","2017-09-01","2017-09-15"))) +
  ylim(0,10) + xlab('Date') + ylab('Omega') + theme_bw()
p2
p3 <- ggplot(NULL,aes(x=from,y=result_test[,4])) + geom_point()+geom_line() + scale_x_date(breaks=as.Date(c("2017-08-01","2017-08-15","2017-09-01","2017-09-15"))) +
  ylim(0,40) + xlab('Date') + ylab('Days to Tc') + theme_bw()
p3


