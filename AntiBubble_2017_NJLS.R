library('cmaes')
library('quantmod')
library('zoo')
library('nortest')
library('Metrics')
library('stats')
library('dplyr')
library('reshape2')
library('ggplot2')

#Window Setup
from <- as.Date("2017-12-18")
to <- as.Date("2018-03-28")

#Fixing Bitcoin_data for window of interest
rBitcoin_data <- subset(Bitcoin_data, Bitcoin_data$Date >= from & Bitcoin_data$Date <= to)

#getting critical time window
tc_anti = Bitcoin_data$t[which.max(Bitcoin_data$Close)]

#reducing the number of parameters: from 4 to 3
NJLS_anti <- function(data, m=1, omega=1) {
  data$X <- data$t - tc_anti
  data$Xm <- data$X ** m #B
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X)) #C1
  data$Xm.sin <- data$X ** m * sin(omega * log(data$X)) #C2
  data$logP <- log(data$Close)
  return(lm(logP ~ Xm + Xm.cos + Xm.sin, data=data))
}

#Initial Estimates of A, B, C1 and C2 through Least Squares
FittedNJLS_anti <- function(data, lm.result, m=1, omega=1) {
  data$X <- data$t - tc_anti
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X))) 
  return(result)
}


#Function for getting final values of A, B, C1 and C2 parameters
getlinear_param_anti <- function(m, omega) {
  lm.result <- NJLS_anti(rBitcoin_data, m, omega)
  getcoeff_regNJLS <- c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4])
}

FittedNJLSPrice_anti <- function(data,paras){
  m = as.numeric(paras[1])
  omega = as.numeric(paras[2])
  A = as.numeric(paras[3])
  B = as.numeric(paras[4])
  C1 = as.numeric(paras[5])
  C2 = as.numeric(paras[6])
  data$X <- data$t - tc_anti
  data <- subset(data,data$X >0)
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X)))
  
  return(result)
}

# Sum of squared residuals, to evaluate the fitness of m, omega, phi
residuals_anti <- function(m, omega) {
  lm.result <- NJLS_anti(rBitcoin_data, m, omega)
  return(sum((FittedNJLS_anti(rBitcoin_data, lm.result, m, omega) - rBitcoin_data$Close) ** 2))
}


residual_obj_anti <- function(x) {
  return(residuals_anti(x[1], x[2]))
}

#Optimisation Procedure using CMAES
vec_control <- data.frame(maxit = c(100))   

result_test <- as.data.frame(NULL)
result_test1 <- as.data.frame(NULL)
result_test2 <- as.data.frame(NULL)
Expect_NJLSPrice <- as.data.frame(NULL)
ts_plot <- as.data.frame(NULL)
n <- 100 # times for estimation

for(i in 1:n){
  
  test <- cma_es(c(0.01, 10), residual_obj_anti, lower=c(0.01, 7), upper=c(1, 16), control=vec_control)
  result_test1 <- rbind(result_test1, test$par)
  lm.result <-NJLS_anti(rBitcoin_data,test$par[1],test$par[2])
  result_test2 <- rbind(result_test2,c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4]))
}
colnames(result_test1) <- c('m','omega')
colnames(result_test2) <- c('A','B','C1','C2')
result_test <- cbind(result_test1,result_test2)
summary(result_test)

n_ts = 150
ts_Date <- seq(from = from, by = 1, length.out = n_ts)
gtd <- as.Date("2016-12-31")
ts_t <- 2017 + as.numeric(difftime(ts_Date,gtd)/365)
ts_plot <- cbind(as.Date(ts_Date),ts_t)
ts_plot <- as.data.frame(ts_plot)
colnames(ts_plot) <- c('Date','t')

for (i in 1:n){
  Expect_NJLSPrice <- dplyr::bind_rows(Expect_NJLSPrice,data.frame(t(FittedNJLSPrice_anti(ts_plot,result_test[i,]))))
}
Expect_NJLSPrice <- data.frame(t(Expect_NJLSPrice))
colnames(Expect_NJLSPrice) <- c(1:n)

n_plot = nrow(Expect_NJLSPrice)
ss_plot <- as.Date('2017-12-17')
from_plot <- seq(from = ss_plot, by = 1, length.out = n_plot)
Expect_NJLSPrice$Date <- from_plot

Expect_NJLSPrice_long <- melt(Expect_NJLSPrice,id='Date')
colnames(Expect_NJLSPrice_long) <- c('Date','Number','Price')
Expect_NJLSPrice_long <- subset(Expect_NJLSPrice_long,Expect_NJLSPrice_long$Date<= as.Date('2018-08-01'))


#Plot the expected price
Bitcoin_data_plot <- subset(Bitcoin_data,Bitcoin_data$Date >=as.Date('2017-12-01') & Bitcoin_data$Date <=as.Date('2018-08-01'))
p  <- ggplot() + geom_line(aes(Bitcoin_data_plot$Date,Bitcoin_data_plot$`Log Price`),size=1)+
  geom_line(aes(Expect_NJLSPrice_long$Date,log(Expect_NJLSPrice_long$Price)),size=0.8,colour = "#FF8C00") +
  xlab('Date: 2017-2018')+ylab('ln[p(t)]') + theme_bw() +guides(color = FALSE)
p + scale_x_date(date_labels = "%b", date_breaks = "1 month") + geom_vline(xintercept = as.Date("2017-12-18"), color = "red", size=1) + geom_vline(xintercept = as.Date("2018-03-28"), color = "green", size=1) + geom_text(aes(x = as.Date("2017-12-18") , y= 9, label = "t1")) + geom_text(aes(x = as.Date("2018-03-28") , y= 8.8, label = "t2")) 
