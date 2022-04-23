#Q1
# AR(p) process
# X_t = phi1 * X_(t-12) + e_t


# Define coefficients
phi1=-0.9 #By last digit

#We compute the roots of the AR polynomial as p=12
coeff <- c(1,0,0,0,0,0,0,0,0,0,0,0.9)
Q1a <- polyroot(coeff) #causal

#Define AR model
ar1 = c(0,0,0,0,0,0,0,0,0,0,0,phi1)
ar1_model = list(ar = ar1)

#Theoretical ACF 
ar1_tacf = ARMAacf(ar = ar1, lag.max=60)
#Theoretical ACF plot
plot(ar1_tacf ,  main ="Plot of the values of the theoretical ACF", xlab = "x" , ylab = "ACF" ) 

#Theoretical PACF 
ar1_tpacf = ARMAacf(ar = ar1, lag.max=60,pacf=TRUE)
#Theoretical PACF plot 
plot(ar1_tpacf, main ="Plot of the values of the theoretical PACF", xlab = "x" , ylab = "PACF" )


# Generate series
ar1_series = arima.sim(model = ar1_model, n=120)
#Plot series
plot(ar1_series, main = "Plot of the simulated series of the model", xlab = "t" , ylab = "Xt"  )

#Sample ACF
ar1_acf = acf(ar1_series, lag.max = 60)
#Sample ACF plot
plot(ar1_acf, main = "Sample ACF plot of the simulated series" , xlab = "Lag", ylab ="ACF")

#Sample PACF 
ar1_pacf = pacf(ar1_series, lag.max = 60,plot=FALSE)
#Sample PACF plot
plot(ar1_pacf ,main = "Sample PACF plot of the simulated series" , xlab = "Lag", ylab ="PACF")


#Q2
#Import data
mydata <- read.csv("assign_data_89.csv")
mydata
#Exclude the first column x=x
cols.dont.want <- "X"
mydata <- mydata[, ! names(mydata) %in% cols.dont.want, drop = F]

#Estimate the mean
mean(mydata$x)

#Estimate the sample autocovariance function
acvf = acf(mydata$x,type = 'covariance', lag.max = 19)

#The sample autocovariance function plot
plot(acvf, main = " Sample ACVF plot", xlab ="Lag" ,ylab ="ACVF" )


#Create 4x4 matrix
matrix1 <- matrix(0,4,4)
#Create time-series objects
t_data <- ts(mydata$x)


# Fit an ARMA(p, q) model + determine Q(p, q) := '-l'
for(p in 1:4){
for (q in 1:4){
ARMA <- arima(t_data, optim.control = list(maxit = 1000),  order = c(p,0,q))
matrix1[p,q]<- (-ARMA$loglik)
}
}

matrix1

#Divide Q(p,q) by n for the BIC equation 
matrix2 <- matrix(0,4,4)
for(p in 1:4){
for (q in 1:4){
matrix2[p,q] <- matrix1[p,q]/1001
}
}

#BIC formula
Q2b <- matrix(0,4,4)
for(p in 1:4){
for (q in 1:4){
Q2b[p,q] <- log (matrix2[p,q])+ ((p+q)/1001)* log(1001)
}
} 

#Create 4x4 matrix

matrix3 <- matrix(0,4,4)

# Fit an ARMA(p, q) model + determine Q(p, q) := '-l' 
for(p in 1:4){
for (q in 1:4){
ARIMA <- arima(t_data, optim.control = list(maxit = 1000),  order = c(p,1,q))
matrix3[p,q]<-(-ARIMA$loglik)
}
}

#Divide Q(p,q) by n for the BIC equation 
matrix4 <- matrix(0,4,4)
for(p in 1:4){
for (q in 1:4){
matrix4[p,q] <- matrix3[p,q]/1001
}
}

#BIC formula
Q2c <- matrix(0,4,4)
for(p in 1:4){
for (q in 1:4){
Q2c[p,q] <- log (matrix4[p,q])+ ((p+q)/1001)* log(1001)
}
} 

