install.packages("cmaes")
install.packages("quantmod")
install.packages("zoo")
install.packages("nortest")
install.packages("Metrics")
install.packages("stats")

## Fixing window for analysis
from <- as.Date("2019-12-01")
to <- as.Date("2020-02-01")


rBitcoin_dataPlot <- subset(Bitcoin_data, Bitcoin_data$Date >= from & Bitcoin_data$Date <=to )

p <- ggplot(data = rBitcoin_dataPlot,aes(x=Date,y=Close))
p + geom_line(size=1) + xlab('Date') + ylab('Price') + theme_bw()



library('cmaes')
library('quantmod')
library('zoo')
library('nortest')
library('Metrics')
library('stats')
library('dplyr')
library('reshape2')
library('ggplot2')

#Time window for estimation
from <- as.Date("2019-12-10")
to <- as.Date("2020-02-10")

rBitcoin_data <- subset(Bitcoin_data, Bitcoin_data$Date >= from & Bitcoin_data$Date <= to)

#Putting Linear Variables
NJLS <- function(data, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  data$Xm <- data$X ** m #B
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X)) #C1
  data$Xm.sin <- data$X ** m * sin(omega * log(data$X)) #C2
  data$logP <- log(data$Close)
  data$v <- data$volatility # C3
  return(lm(logP ~ Xm + Xm.cos + Xm.sin + v, data=data))
}

#Initial Estimates of A, B, C1, C2 and C3 through Least Squares
FittedNJLS <- function(data, lm.result, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  C3 <- lm.result$coefficients[5] # this parameters is for introducing volatility
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X)) + C3 * (data$volatility)) 
  return(result)
}

FittedNLJSPrice <- function(data,paras){
  m = as.numeric(paras[1])
  omega = as.numeric(paras[2])
  tc = as.numeric(paras[3])
  A = as.numeric(paras[4])
  B = as.numeric(paras[5])
  C1 = as.numeric(paras[6])
  C2 = as.numeric(paras[7])
  C3 = as.numeric(paras[8])
  data$X <- tc - data$t
  data <- subset(data,data$X >0)
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X)) + C3 * (data$volatility))
  #return(as.data.frame(result))
  return(result)
}


#Function for getting final values of A, B, C1, C2 and C3 parameters
getlinear_param <- function(m, omega, tc) {
  lm.result <- NJLS(rBitcoin_data, m, omega, tc)
  getcoeff_regNJLS <- c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4], lm.result$coefficients[5])
}

# Sum of squared residuals, to evaluate the fitness of m, omega, psi
residuals <- function(m, omega, tc) {
  lm.result <- NJLS(rBitcoin_data, m, omega, tc)
  return(sum((FittedNJLS(rBitcoin_data, lm.result, m, omega, tc) - rBitcoin_data$Close) ** 2))
}


residual_obj <- function(x) {
  return(residuals(x[1], x[2], x[3]))
}

vec_control <- data.frame(maxit = c(100))  
result_test <- as.data.frame(NULL)
result_test1 <- as.data.frame(NULL)
result_test2 <- as.data.frame(NULL)
Expected_BitPrice <- as.data.frame(NULL)
n <- 100 # times for estimation

for(i in 1:n){
  test <- cma_es(c(0.01, 6, max(rBitcoin_data$t)+0.01), residual_obj, lower=c(0.01, 6, max(rBitcoin_data$t)+0.01), upper=c(1, 16, max(rBitcoin_data$t)+0.25), control=vec_control)
  result_test1 <- rbind(result_test1, test$par)
  lm.result <-NJLS(rBitcoin_data,test$par[1],test$par[2], test$par[3])
  result_test2 <- rbind(result_test2,c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4], lm.result$coefficients[5]))
}
colnames(result_test1) <- c('m','omega','tc')
colnames(result_test2) <- c('A','B','C1','C2','C3')
result_test <- cbind(result_test1,result_test2)
last_row <- tail(rBitcoin_data, 1)
result_test$Date_to_Peak <- as.integer((result_test$tc-last_row$t)/(1/365))
summary(result_test)


#Plotting tc
p <- ggplot(data = result_test,aes(x=Date_to_Peak)) 
p + geom_histogram(binwidth = 3, fill = "lightblue", colour = "black") + xlim(0,30) +  theme_bw() +
  xlab( "Days to Peak date") + ylab("Frequency")

for (i in 1:n){
  Expected_BitPrice <- dplyr::bind_rows(Expected_BitPrice,data.frame(t(FittedNLJSPrice(Bitcoin_data,result_test[i,]))))
}
Expected_BitPrice <- data.frame(t(Expected_BitPrice))
colnames(Expected_BitPrice) <- c(1:n)

n_plot = nrow(Expected_BitPrice)
ss_plot <- as.Date('2019-10-31')
from_plot <- seq(from = ss_plot, by = 1, length.out = n_plot)
Expected_BitPrice$Date <- from_plot

Expected_BitPrice_long <- subset(Expected_BitPrice,Expected_BitPrice$Date>= as.Date('2019-11-15'))
Expected_BitPrice_long <- melt(Expected_BitPrice_long,id='Date')
colnames(Expected_BitPrice_long) <- c('Date','Number','Price')

#Plot the expected price
Bitcoin_data_plot <- subset(Bitcoin_data,Bitcoin_data$Date >= as.Date('2019-10-01') & Bitcoin_data$Date <= as.Date('2020-02-28'))

p  <- ggplot() + geom_line(aes(Bitcoin_data_plot$Date,Bitcoin_data_plot$`Log Price`),size=1)+
  geom_line(aes(Expected_BitPrice_long$Date,log(Expected_BitPrice_long$Price),colour = Expected_BitPrice_long$Number),size=0.8) +
  xlab('Date: 2019-2020')+ylab('ln[p(t)]')+ guides(color=FALSE) + theme_bw() 
p + scale_x_date(date_labels = "%b", date_breaks = "1 month") + geom_vline(xintercept = as.Date("2020-02-08"), color = "blue", size=1) + geom_vline(xintercept = as.Date("2020-02-14"), color = "red", size=1) +   geom_vline(xintercept = as.Date("2019-12-10"), color = "green", size=1) +  geom_text(aes(x = as.Date("2019-12-10") , y= 8, label = "t1")) +  geom_text(aes(x = as.Date("2020-02-08") , y= 8, label = "t2")) +  geom_text(aes(x = as.Date("2020-02-14") , y= 8.5, label = "tc"))

quantile(result_test$Date_to_Peak, c(0.05, 0.95))
quantile(result_test$Date_to_Peak, c(0.025, 0.975))

quantile(result_test$A, c(0.05, 0.95))
quantile(result_test$A, c(0.025, 0.975))
quantile(result_test$B, c(0.05, 0.95))
quantile(result_test$B, c(0.025, 0.975))
C <- result_test$C1**2 + result_test$C2**2
quantile(sqrt(result_test$C1**2 + result_test$C2**2), c(0.05, 0.95))
quantile(sqrt(result_test$C1**2 + result_test$C2**2), c(0.025, 0.975))

quantile(result_test$C3, c(0.05, 0.95))
quantile(result_test$C3, c(0.025, 0.975))
quantile(result_test$m, c(0.05, 0.95))
quantile(result_test$m, c(0.025, 0.975))

quantile(result_test$omega, c(0.05, 0.95))
quantile(result_test$omega, c(0.025, 0.975))

quantile(acos(result_test$C1/(sqrt(result_test$C1**2 + result_test$C2**2))), c(0.05, 0.95))
quantile(acos(result_test$C1/(sqrt(result_test$C1**2 + result_test$C2**2))), c(0.025, 0.975))

summary(result_test)

mean(sqrt(result_test$C1**2 + result_test$C2**2))
median(sqrt(result_test$C1**2 + result_test$C2**2))
sd(sqrt(result_test$C1**2 + result_test$C2**2))

mean(acos(result_test$C1/(sqrt(result_test$C1**2 + result_test$C2**2))))
median(acos(result_test$C1/(sqrt(result_test$C1**2 + result_test$C2**2))))
sd(acos(result_test$C1/(sqrt(result_test$C1**2 + result_test$C2**2))))


Expected_BitPrice_long_CF <- subset(Expected_BitPrice_long, Expected_BitPrice_long$Date >=as.Date('2020-02-05') & Expected_BitPrice_long$Date <=as.Date('2020-02-16'))
quantile(Expected_BitPrice_long_CF$Price, c(0.025, 0.975), na.rm=TRUE)

Expected_BitPrice_long_CF


Bitcoin_data$Close[which.max(Bitcoin_data$Close)]
