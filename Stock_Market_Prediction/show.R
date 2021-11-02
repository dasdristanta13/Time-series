
library(prophet)
library(quantmod)
library(forecast)
library(tseries)
library(timeSeries)
library(dplyr)
library(fGarch)
library(gridExtra)
library(readxl)
library(astsa)
library(urca)
library(rugarch)

setwd("~/Sem_3/Time_Series/Data")

nifty <- read_excel("nifty.xlsx")
dax <- read_excel("dax.xlsx")
dow_jones <- read_excel("dow_jones.xlsx")
nikkei <- read_excel("nikkei.xlsx")

niftyw<-nifty[857:1234,]
daxw<-dax[878:1263,]
dow_jonesw<-dow_jones[960:1344,]
nikkeiw<-nikkei[849:1220,]

niftywc<- as.numeric(niftyw$Close)
daxwc<- as.numeric(daxw$Close)
dowwc<- as.numeric(dow_jonesw$Close)
nikc<- as.numeric(nikkeiw$Close)

plot.ts((as.numeric(niftyw$Close)-mean(as.numeric(niftyw$Close)))/(max(niftywc)-min(niftywc)),main="Nifty-Dax-Dow Jones-Nikkei",ylab="",xact="n")

lines((as.numeric(daxw$Close)-mean(as.numeric(daxw$Close)))/(max(daxwc)-min(daxwc)),col="red")
lines((as.numeric(dow_jonesw$Close)-mean(as.numeric(dow_jonesw$Close)))/(max(dowwc)-min(dowwc)),col="blue")
lines((as.numeric(nikkeiw$Close)-mean(as.numeric(nikkeiw$Close)))/(max(nikc)-min(nikc)),col="purple")
legend(0, 0.4, legend=c("Nifty", "Dax","Dow Jones","Nikkei"),
       col=c("black","red", "blue","purple"), lty=1:2, cex=0.8,text.font=4, bg='lightblue')

##############################










####################################

### Nifty

######################################





acf2(as.numeric(niftyw$Close))

niftyd=diff(log(as.numeric(niftyw$Close)), lag=1)

ts.plot(niftyd,ylab="Daily log returns")

ts.plot(niftyd**2,ylab="Daily Sq-log returns")

acf2(niftyd)

acf2(niftyd**2)

summary(ur.df(as.numeric(niftyw$Close), type="none",lag=1))    ####### Residual standard error: 0.01302 on 373 degrees of freedom
#                                                     Multiple R-squared:  0.5233,	Adjusted R-squared:  0.5208 
#                                                     F-statistic: 204.8 on 2 and 373 DF,  p-value: < 2.2e-16






nifty_ma= stats::filter(niftyw$Close , sides=2, c(0.5,rep(1,29),0.5)/30)
plot.ts(nifty_ma,main="Nifty MA",ylab="")

plot.ts(as.numeric(niftyw$Close)-nifty_ma)





niftyfit <- auto.arima(niftyd, lambda = "auto")
plot(resid(niftyfit),main="Residual Plot",ylab="")

acf2(resid(niftyfit))

Box.test(niftyfit$residuals, type="Ljung-Box")   # Null hypothesis is that our model does not show
                                                 # lack of fit.

summary(ur.df(resid(niftyfit),type="none",lag=1))

nifty_forecast <- forecast(niftyfit, h=30)
plot(nifty_forecast)
head(nifty_forecast$lower)
head(nifty_forecast$upper)


############################################
# Testing ARCH   ##########################

library(FinTS)

ArchTest(niftyd,lags=1,demean = TRUE)

## The result of this LM statistic is 9.2071 which if compared with df=1 and alpha =0.05
## chi-squraed(0.95,1)=3.84
## This indicates that the null hypothesis is rejected.
## Thus it can be concludes that the series has ARCH effects



#################################

## Estimating Best ARCH model #######

#################################

nif_AIC<-matrix(0*seq_len(10), nrow = 1, ncol = 10)



for (j in seq(1,10)){
  k <- garch(niftyd,c(0,j))
  #print(summary(k))
 # print("--------------------------------")
  nif_AIC[1,j]<- stats::AIC(k)
  #plot.ts(k$fitted.values[-1,1]**2,main=paste("Estimated GARCH(",paste(0,j,sep=","),") variance for the 'Nifty' dataset"))
}

nif_AIC

which(nif_AIC==min(nif_AIC),arr.ind = T)

nft_arch_main<-garch(niftyd,c(0,3))

summary(nft_arch_main)



plot.ts(nft_arch_main$fitted.values[-1,1]**2,main=paste("Estimated ARCH(3) for the 'Nifty' dataset"))


plot.ts(niftyd**2,main="Sq-Log Return")



#plot(nft_arch_main,which="all")



#################################

## Best GARCH modelling

#################################



NIF_AIC<-matrix(0*seq_len(10), nrow = 10, ncol = 10)



for (i in seq(1,10)){
  for (j in seq(1,10)){
    k <- garch(niftyd,c(i,j))
    #print(summary(k))
    #print("--------------------------------")
    NIF_AIC[i,j]<- stats::AIC(k)
    #plot.ts(k$fitted.values[-1,1]**2,main=paste("Estimated GARCH(",paste(i,j,sep=","),") variance for the 'Nifty' dataset"))
  }
}

NIF_AIC

which(NIF_AIC==min(NIF_AIC),arr.ind = T)

nft_garch_main<-garch(niftyd,c(1,2))

summary(nft_garch_main)
par(mfrow=c(1,1))

plot.ts(nft_garch_main$fitted.values[-1,1]**2,main=paste("Estimated GARCH(",paste(1,2,sep=","),") for the 'Nifty' dataset"))




###############################

### dax

##############################


ts.plot(daxw$Close)

summary(ur.df(daxw$Close))

acf2(as.numeric(daxw$Close), main = "Dax")

daxd=diff(log(as.numeric(daxw$Close)), lag=1)

ts.plot(daxd,ylab="Daily log returns")

ts.plot(daxd**2,ylab="Daily Sq-log returns")

acf2(daxd)

acf2(daxd**2)

summary(ur.df(daxd, type="none",lag=1))    #######


dax_ma= stats::filter(daxw$Close , sides=2, c(0.5,rep(1,29),0.5)/30)
plot.ts(dax_ma)

plot.ts(as.numeric(daxw$Close)-dax_ma)





daxfit <- auto.arima(daxd, lambda = "auto")
plot(resid(daxfit))

acf2(resid(daxfit))

Box.test(daxfit$residuals, type="Ljung-Box")

summary(ur.df(resid(daxfit),type="none",lag=1))

dax_forecast <- forecast(daxfit, h=30)
plot(dax_forecast)
head(dax_forecast$lower)
head(dax_forecast$upper)


############################################
# Testing ARCH   ##########################

############################################
library(FinTS)

ArchTest(daxd,lags=1,demean = TRUE)

## The result of this LM statistic is 0.18679 which if compared with df=1 and alpha =0.05
## chi-squraed(0.95,1)=3.84
## This indicates that the null hypothesis is not rejected.
## Thus it can be concludes that the series has no ARCH effects



#################################

## Estimating ARCH model #######

#################################

dax_archfit <- garch(daxd,c(0,1))

summary(dax_archfit)

plot(dax_archfit)


plot.ts(dax_archfit$fitted.values[-1,1]**2,main="Estimated ARCH(1) variance for the 'Dax' dataset",col="blue")

plot.ts(daxd**2,col="red")

acf2(ts(dax_archfit$residuals))

acf2(ts(dax_archfit$residuals**2))


#################################

## GARCH modelling

#################################



dax_garchFit <- garch(daxd,c(1,1))

summary(dax_garchFit)



plot.ts(dax_garchFit$fitted.values[-1,1]**2,main="Estimated GARCH(1,1)  for the 'Dax' dataset",col="blue")

plot.ts(daxd**2,col="red")

acf2(ts(dax_garchFit$residuals**2))
















######################

### Dow_jones

######################

ts.plot(dow_jonesw$Close)

summary(ur.df(dow_jonesw$Close))

acf2(as.numeric(dow_jonesw$Close), main = "Dow Jones")

dow_jonesd=diff(log(as.numeric(dow_jonesw$Close)), lag=1)

ts.plot(dow_jonesd,ylab="Daily log returns")

ts.plot(dow_jonesd**2,ylab="Daily Sq-log returns")

acf2(dow_jonesd)

acf2(dow_jonesd**2)

summary(ur.df(dow_jonesd, type="none",lag=1))    #######


dow_ma= stats::filter(dow_jonesw$Close , sides=2, c(0.5,rep(1,29),0.5)/30)
plot.ts(dow_ma)

plot.ts(as.numeric(dow_jonesw$Close)-dax_ma)





dowfit <- auto.arima(dow_jonesd, lambda = "auto")
plot(resid(dowfit))

acf2(resid(dowfit))

Box.test(dowfit$residuals, type="Ljung-Box")

summary(ur.df(resid(dowfit),type="none",lag=1))

dow_forecast <- forecast(dowfit, h=30)
plot(dow_forecast)
head(dow_forecast$lower)
head(dow_forecast$upper)


############################################
# Testing ARCH   ##########################

############################################
library(FinTS)

ArchTest(dow_jonesd,lags=1,demean = TRUE)

## The result of this LM statistic is 6.0705 which if compared with df=1 and alpha =0.05
## chi-squraed(0.95,1)=3.84
## This indicates that the null hypothesis is rejected.
## Thus it can be concludes that the series has ARCH effects



#################################

## Estimating ARCH model #######

#################################

dow_AIC<-matrix(0*seq_len(10), nrow = 1, ncol = 10)



for (j in seq(1,10)){
  k <- garch(dow_jonesd,c(0,j))
  #print(summary(k))
  #print("--------------------------------")
  dow_AIC[1,j]<- stats::AIC(k)
  #plot.ts(k$fitted.values[-1,1]**2,main=paste("Estimated GARCH(",paste(0,j,sep=","),") variance for the 'Nifty' dataset"))
}

dow_AIC

which(dow_AIC==min(dow_AIC),arr.ind = T)

dow_arch_main<-garch(dow_jonesd,c(0,4))

summary(dow_arch_main)

plot.ts(dow_arch_main$fitted.values[-1,1]**2,main=paste("Estimated ARCH(3) variance for the 'Dow Jones' dataset"))

plot.ts(dow_jonesd**2)

acf2(ts(dow_arch_main$residuals))

acf2(ts(dow_arch_main$residuals**2))


#################################

## GARCH modelling

#################################



DOW_AIC<-matrix(0*seq_len(10), nrow = 10, ncol = 10)



for (i in seq(1,10)){
  for (j in seq(1,10)){
    k <- garch(dow_jonesd,c(i,j))
    #print(summary(k))
    #print("--------------------------------")
    DOW_AIC[i,j]<- stats::AIC(k)
    #plot.ts(k$fitted.values[-1,1]**2,main=paste("Estimated GARCH(",paste(i,j,sep=","),") variance for the 'Nifty' dataset"))
  }
}

DOW_AIC

which(DOW_AIC==min(DOW_AIC),arr.ind = T)

DOW_garch_main<-garch(dow_jonesd,c(1,2))

summary(DOW_garch_main)

plot.ts(DOW_garch_main$fitted.values[-1,1]**2,main="Estimated GARCH(1,2) variance for 'Dow Jones' dataset")

plot.ts(dow_jonesd**2)

acf2(ts(DOW_garch_main$residuals**2))






#############################

### Nikkei (Non stationary)

##############################


ts.plot(nikkeiw$Close)

summary(ur.df(nikkeiw$Close))

acf2(as.numeric(nikkeiw$Close), main = "Nikkei")

nikkeid=diff(log(as.numeric(nikkeiw$Close)), lag=1)

ts.plot(nikkeid,ylab="Daily log returns")

ts.plot(nikkeid**2,ylab="Daily Sq-log returns")

acf2(nikkeid)

acf2(nikkeid**2)

summary(ur.df(nikkeid, type="none",lag=1))    #######


nikkei_ma= stats::filter(nikkeiw$Close , sides=2, c(0.5,rep(1,29),0.5)/30)
plot.ts(nikkei_ma)

plot.ts(as.numeric(nikkeiw$Close)-nikkei_ma)





nikkeifit <- auto.arima(nikkeid, lambda = "auto")
plot(resid(nikkeifit))

acf2(resid(nikkeifit))

Box.test(nikkeifit$residuals, type="Ljung-Box")

summary(ur.df(resid(nikkeifit),type="none",lag=1))

nikkei_forecast <- forecast(nikkeifit, h=30)
plot(nikkei_forecast)
head(nikkei_forecast$lower)
head(nikkei_forecast$upper)


############################################
# Testing ARCH   ##########################

############################################
library(FinTS)

ArchTest(nikkeid,lags=1,demean = TRUE)

## The result of this LM statistic is 130.63 which if compared with df=1 and alpha =0.05
## chi-squraed(0.95,1)=3.84
## This indicates that the null hypothesis is rejected.
## Thus it can be concludes that the series has ARCH effects



#################################

## Estimating ARCH model #######

#################################

nikkei_AIC<-matrix(0*seq_len(10), nrow = 1, ncol = 10)



for (j in seq(1,10)){
  k <- garch(nikkeid,c(0,j))
  #print(summary(k))
  #print("--------------------------------")
  nikkei_AIC[1,j]<- stats::AIC(k)
  #plot.ts(k$fitted.values[-1,1]**2,main=paste("Estimated GARCH(",paste(0,j,sep=","),") variance for the 'Nifty' dataset"))
}

dow_AIC

which(nikkei_AIC==min(nikkei_AIC),arr.ind = T)

nikkei_arch_main<-garch(nikkeid,c(0,4))

summary(nikkei_arch_main)

plot.ts(nikkei_arch_main$fitted.values[-1,1]**2,main=paste("Estimated ARCH(4) variance for the 'Nikkei' dataset"))

plot.ts(nikkeid**2)

acf2(ts(nikkei_arch_main$residuals))

acf2(ts(nikkei_arch_main$residuals**2))


#################################

## GARCH modelling

#################################



Nikkei_AIC<-matrix(0*seq_len(10), nrow = 10, ncol = 10)



for (i in seq(1,10)){
  for (j in seq(1,10)){
    k <- garch(dow_jonesd,c(i,j))
    #print(summary(k))
    #print("--------------------------------")
    Nikkei_AIC[i,j]<- stats::AIC(k)
    #plot.ts(k$fitted.values[-1,1]**2,main=paste("Estimated GARCH(",paste(i,j,sep=","),") variance for the 'Nifty' dataset"))
  }
}

Nikkei_AIC

which(Nikkei_AIC==min(Nikkei_AIC),arr.ind = T)

Nikkei_garch_main<-garch(nikkeid,c(1,2))

summary(Nikkei_garch_main)

plot.ts(Nikkei_garch_main$fitted.values[-1,1]**2,main="Estimated GARCH(1,2) variance for 'Nikkei' dataset")

plot.ts(nikkeid**2)

acf2(ts(Nikkei_garch_main$residuals**2))

