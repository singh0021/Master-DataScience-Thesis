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
# For 60 days forward rolling window, we can reset the below window
from <- as.Date("2020-05-10")
to <- as.Date("2020-08-10")

rBitcoin_data <- subset(Bitcoin_data, Bitcoin_data$Date >= from & Bitcoin_data$Date <= to)

#Defining Linear Variables
NJLS <- function(data, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  data$Xm <- data$X ** m #B
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X)) #C1
  data$Xm.sin <- data$X ** m * sin(omega * log(data$X)) #C2
  data$logP <- log(data$Close)
  data$v <- data$volatility
  return(lm(logP ~ Xm + Xm.cos + Xm.sin + v, data=data))
}

#Initial Estimates of A, B, C1, C2 and C3 through Least Squares
FittedNJLS <- function(data, lm.result, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  C3 <- lm.result$coefficients[5]
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X)) +  C3 * (data$volatility)) 
  return(result)
}

FittedNJLSPrice <- function(data,paras){
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
  
  return(result)
}


#Function to get final values of A, B, C1, C2 & C3 parameters
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
Expect_NJLSPrice <- as.data.frame(NULL)
n <- 100 # times for estimation

for(i in 1:n){
  test <- cma_es(c(0.01, 6, max(rBitcoin_data$t)+0.01), residual_obj, lower=c(0.01, 6, max(rBitcoin_data$t)+0.01), upper=c(1, 16, max(rBitcoin_data$t)+0.25), control=vec_control)
  result_test1 <- rbind(result_test1, test$par)
  lm.result <-NJLS(rBitcoin_data,test$par[1],test$par[2],test$par[3])
  result_test2 <- rbind(result_test2,c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4], lm.result$coefficients[5]))
}
colnames(result_test1) <- c('m','omega','tc')
colnames(result_test2) <- c('A','B','C1','C2', 'C3')
result_test <- cbind(result_test1,result_test2)
last_row <- tail(rBitcoin_data, 1)
result_test$Date_to_Peak <- as.integer((result_test$tc-last_row$t)/(1/365))
summary(result_test)

#Plotting tc
p <- ggplot(data = result_test,aes(x=Date_to_Peak)) 
p + geom_histogram(binwidth = 3, fill = "lightblue", colour = "black") +  theme_bw() +
  xlab( "Days to peak") + ylab("Frequency")

gtd <- as.Date("2019-12-31")
# plot expected time
plot_from <- as.Date('2020-01-01')
plot_to <- as.Date('2020-12-31')
plot_days <- as.data.frame(seq(plot_from, plot_to, by = 'days'))
colnames(plot_days) <- 'Date'
plot_days$t <- 2019 + as.numeric(difftime(plot_days$Date,gtd, unit = "days")/365)
plot_days<-subset(plot_days, plot_days$Date <=as.Date("2020-08-10"))
plot_days<-subset(plot_days, plot_days$Date != as.Date("2020-03-30") & plot_days$Date != as.Date("2020-04-09") & plot_days$Date != as.Date("2020-04-11")
                  & plot_days$Date != as.Date("2020-04-20") & plot_days$Date != as.Date("2020-04-27") & plot_days$Date != as.Date("2020-05-02")
                  & plot_days$Date != as.Date("2020-05-04") & plot_days$Date != as.Date("2020-05-06") & plot_days$Date != as.Date("2020-05-18"))
plot_days$volatility<-Bitcoin_data[Bitcoin_data$Date >= as.Date("2020-01-01") & Bitcoin_data$Date <=as.Date("2020-08-10"),]$volatility


for (i in 1:n){
  Expect_NJLSPrice <- dplyr::bind_rows(Expect_NJLSPrice,data.frame(t(FittedNJLSPrice(plot_days,result_test[i,]))))
}
Expect_NJLSPrice <- data.frame(t(Expect_NJLSPrice))
colnames(Expect_NJLSPrice) <- c(1:n)

n_plot = nrow(Expect_NJLSPrice)
ss_plot <- plot_from
from_plot <- seq(from = ss_plot, by = 1, length.out = n_plot)
Expect_NJLSPrice$Date <- from_plot

Expect_NJLSPrice_long <- subset(Expect_NJLSPrice,Expect_NJLSPrice$Date>= as.Date('2020-01-01'))

Expect_NJLSPrice_long <- melt(Expect_NJLSPrice_long,id='Date')
colnames(Expect_NJLSPrice_long) <- c('Date','Number','Price')

Bitcoin_data_plot <- subset(Bitcoin_data,Bitcoin_data$Date >= as.Date('2020-01-01'))
p  <- ggplot() + geom_line(aes(Bitcoin_data_plot$Date,Bitcoin_data_plot$`Log Price`),size=1)+
  geom_line(aes(Expect_NJLSPrice_long$Date,log(Expect_NJLSPrice_long$Price),colour = Expect_NJLSPrice_long$Number),size=0.8) +
  xlab('Date')+ylab('ln[p(t)]')+ guides(color=FALSE) + theme_bw() 
p 




quantile(result_test$C3, c(0.05, 0.95))
quantile(result_test$C3, c(0.025, 0.975))

