library(quantmod)
library(dplyr)
library(tidyverse)
library(tseries)
library(rugarch)
library(xts)
library(PerformanceAnalytics)
library(lubridate)

setwd("~/Sem_3/Time_Series/Data")

nifty <- read_excel("nifty.xlsx")
dax <- read_excel("dax.xlsx")
dow_jones <- read_excel("dow_jones.xlsx")
nikkei <- read_excel("nikkei.xlsx")

niftyw<-nifty[857:1234,]
daxw<-dax[878:1263,]
dow_jonesw<-dow_jones[960:1344,]
nikkeiw<-nikkei[849:1220,]



################################################################

##               Nifty

#########################################################

niftyxts<-xts(x=as.numeric(niftyw$Close),order.by = ymd(niftyw$Date))

chartSeries(niftyxts)

acf2(niftyxts)

summary(ur.df(niftyxts, type="none",lag=1)) 

niftyreturn = CalculateReturns(log(niftyxts))
niftyreturn=niftyreturn[-c(1),]

chartSeries(niftyreturn)
chartSeries(niftyreturn**2)

summary(ur.df(niftyreturn, type="none",lag=1)) 

chart.Histogram(niftyreturn,methods = c("add.density","add.normal"),colorset = c("grey","red","blue"))
legend("topright",legend=c("return","kernel","normal dist."),fill=c("grey","red","blue"))


acf2(niftyreturn)
acf2(niftyreturn**2)




auto.arima(niftyreturn)


###    3 4 3 4

###    0 0 1 1

### Normal ###
niftyspec_1<-ugarchspec(mean.model = list(armaOrder=c(3,4)),variance.model = list(model="sGARCH",garchOrder=c(3,4)),distribution.model="norm")

niftyfit_1<-ugarchfit(data=niftyreturn,spec = niftyspec_1,out.sample = 20)
niftyfit_1


niftyspec_2<-ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model="sGARCH",garchOrder=c(1,1)),distribution.model="norm")

niftyfit_2<-ugarchfit(data=niftyreturn,spec = niftyspec_2,out.sample = 20)
niftyfit_2



### 3 2 3 1

### 0 0 1 1


### Students' T ###


##

niftyspec_3<-ugarchspec(mean.model = list(armaOrder=c(3,2)),variance.model = list(model="sGARCH",garchOrder=c(2,1)),distribution.model="sstd")

niftyfit_3<-ugarchfit(data=niftyreturn,spec = niftyspec_3,out.sample = 20)
niftyfit_3




niftyspec_4<-ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model="sGARCH",garchOrder=c(1,1)),distribution.model="sstd")

niftyfit_4<-ugarchfit(data=niftyreturn,spec = niftyspec_4,out.sample = 20)
niftyfit_4

####


### Comparison ###
par(mfrow=c(2,2))
plot(niftyfit_1,which=8)
plot(niftyfit_2,which=8)
plot(niftyfit_3,which=8)
plot(niftyfit_4,which=8)

##################
par(mfrow=c(2,2))
plot(niftyfit_1,which=9)
plot(niftyfit_2,which=9)
plot(niftyfit_3,which=9)
plot(niftyfit_4,which=9)

##################

Box.test(niftyfit_1@fit$residuals, type="Ljung-Box")

Box.test(niftyfit_2@fit$residuals, type="Ljung-Box")

Box.test(niftyfit_3@fit$residuals, type="Ljung-Box")

Box.test(niftyfit_4@fit$residuals, type="Ljung-Box")
#############################
par(mfrow=c(2,1))

ts.plot(niftyfit_3@fit$sigma,xlab="",ylab="",main="Volatility of suggested model 1")
ts.plot(niftyfit_4@fit$sigma,xlab="",ylab="",main="Volatility of suggested model 2")

############## Forecast

nifty_forecast1<- ugarchforecast(niftyfit_3,n.ahead = 30)


nifty_forecast1@forecast$seriesFor


par(mfrow=c(1,1))
plot(nifty_forecast1,which=1)



exp(nifty_forecast1@forecast$seriesFor[2])


last= as.numeric(niftyw$Close[length(niftyw$Close)])
Update1<- c(as.numeric(niftyw$Close))

for (i in seq(1,30)){
  last= last*exp(nifty_forecast1@forecast$seriesFor[i])
  Update1 <- c(Update1,last)
}

Update1


ts(Update1)


###########################
plot(ts(Update1),col="red",main="Nifty Forecast",ylab="",lwd=2,type="l")
lines(niftyw$Close,lwd=2)
legend(0, 17999, legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')

##########################


nifty_forecast2<- ugarchforecast(niftyfit_4,n.ahead = 30)


nifty_forecast2@forecast$seriesFor

plot(nifty_forecast2,which=1)



exp(nifty_forecast2@forecast$seriesFor[2])


last= as.numeric(niftyw$Close[length(niftyw$Close)])
Update2<- c(as.numeric(niftyw$Close))

for (i in seq(1,30)){
  last= last*exp(nifty_forecast2@forecast$seriesFor[i])
  Update2 <- c(Update2,last)
}

Update2


ts(Update2)


###########################
plot(ts(Update2),col="red",main="Nifty Forecast",ylab="",lwd=2,type="l")
lines(niftyw$Close,lwd=2)
legend(0, 17999, legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')

##########################



par(mfrow=c(1,2))

plot(ts(Update1),col="red",main="Nifty Forecast with ARMA(3,2) and GARCH(3,1)",ylab="",lwd=2,type="l")
lines(niftyw$Close,lwd=2)
legend(0, 17999, legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')


plot(ts(Update2),col="red",main="Nifty Forecast with ARMA(0,0) and GARCH(1,1)",ylab="",lwd=2,type="l")
lines(niftyw$Close,lwd=2)
legend(0, 17999, legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')




##################################

#####       Dax

#############################################



daxxts<-xts(x=as.numeric(daxw$Close),order.by = ymd(daxw$Date))

chartSeries(daxxts)

acf2(daxxts)

summary(ur.df(daxxts,type="none",lag=1))


daxreturn = CalculateReturns(log(daxxts))
daxreturn=daxreturn[-c(1),]

chartSeries(daxreturn)

chartSeries(daxreturn**2)

chart.Histogram(daxreturn,methods = c("add.density","add.normal"),colorset = c("grey","red","blue"))
legend("topright",legend=c("return","kernel","normal dist."),fill=c("grey","red","blue"))


acf2(daxreturn)
acf2(daxreturn**2)

summary(ur.df(daxreturn,type="none",lag=1))


dax_arma<-auto.arima(daxreturn)


dax_arma_for<-forecast(dax_arma,h=30)

plot(dax_arma_for)

dax_arma_for[9]


par(mfrow=c(1,1))
plot(x=seq(1,length(daxreturn)),y=rep(0.001532645,length(daxreturn)),type="l",xlab="",ylab="",main="Constant volatility of selected model")
abline(h=0.001532645)


AIC(arima(dax_arma_for,order = c(4,0,1)))


last= as.numeric(daxw$Close[length(daxw$Close)])
Update1<- c(as.numeric(daxw$Close))

for (i in seq(1,30)){
  last= last*exp(dax_arma_for[4]$mean[i])
  Update1 <- c(Update1,last)
}




#### 3 2 2 1

#### 0 0 1 1



### Normal ###
daxspec1<-ugarchspec(mean.model = list(armaOrder=c(3,2)),variance.model = list(model="sGARCH",garchOrder=c(2,1)),distribution.model="norm")

daxfit1<-ugarchfit(data=daxreturn,spec = daxspec1,out.sample = 20)
daxfit1


daxspec2<-ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model="sGARCH",garchOrder=c(1,1)),distribution.model="norm")

daxfit2<-ugarchfit(data=daxreturn,spec = daxspec2,out.sample = 20)
daxfit2


####  3 4 2 1

####  0 0 1 1
### Students' T ###


##

daxspec3<-ugarchspec(mean.model = list(armaOrder=c(3,4)),variance.model = list(model="sGARCH",garchOrder=c(2,1)),distribution.model="sstd")

daxfit3<-ugarchfit(data=daxreturn,spec = daxspec3,out.sample = 20)
daxfit3


daxspec4<-ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model="sGARCH",garchOrder=c(1,1)),distribution.model="sstd")

daxfit4<-ugarchfit(data=daxreturn,spec = daxspec4,out.sample = 20)
daxfit4



### Comparison ###
par(mfrow=c(2,2))
plot(daxfit1,which=8)
plot(daxfit2,which=8)
plot(daxfit3,which=8)
plot(daxfit4,which=8)

##################
plot(daxfit1,which=9)
plot(daxfit2,which=9)
plot(daxfit3,which=9)
plot(daxfit4,which=9)

##################

Box.test(daxfit1@fit$residuals, type="Ljung-Box")

Box.test(daxfit2@fit$residuals, type="Ljung-Box")

Box.test(daxfit3@fit$residuals, type="Ljung-Box")

Box.test(daxfit4@fit$residuals, type="Ljung-Box")

############## Forecast

dax_forecast1<- ugarchforecast(daxfit3,n.ahead = 30)


dax_forecast1@forecast$seriesFor

plot(dax_forecast1,which=1)



exp(dax_forecast1@forecast$seriesFor[2])


last= as.numeric(daxw$Close[length(daxw$Close)])
Update1<- c(as.numeric(daxw$Close))

for (i in seq(1,30)){
  last= last*exp(nifty_forecast1@forecast$seriesFor[i])
  Update1 <- c(Update1,last)
}

Update1


ts(Update1)


###########################
plot(ts(Update1),col="red",main="Dax Forecast",ylab="",lwd=2,type="l")
lines(daxw$Close,lwd=2)
legend("topleft", legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')



############################


dax_forecast2<- ugarchforecast(daxfit4,n.ahead = 30)


dax_forecast2@forecast$seriesFor

plot(dax_forecast2,which=1)



exp(dax_forecast2@forecast$seriesFor[2])


last= as.numeric(daxw$Close[length(daxw$Close)])
Update2<- c(as.numeric(daxw$Close))

for (i in seq(1,30)){
  last= last*exp(nifty_forecast1@forecast$seriesFor[i])
  Update2 <- c(Update2,last)
}

Update2


ts(Update2)


###########################
plot(ts(Update2),col="red",main="Dax Forecast",ylab="",lwd=2,type="l")
lines(daxw$Close,lwd=2)
legend(0, 15999, legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')
############################


par(mfrow=c(2,1))

plot(ts(Update1),col="red",main="Dax Forecast with ARMA(3,4) and GARCH(2,1)",ylab="",lwd=2,type="l")
lines(daxw$Close,lwd=2)
legend("topleft", legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')



plot(ts(Update2),col="red",main="Dax Forecast with ARMA(0,0) and GARCH(1,1)",ylab="",lwd=2,type="l")
lines(daxw$Close,lwd=2)
legend("topleft", legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')


################################################################

##               Dow Jones

#########################################################

dowjonesxts<-xts(x=as.numeric(dow_jonesw$Close),order.by = ymd(dow_jonesw$Date))

chartSeries(dowjonesxts)
acf2(dowjonesxts)

summary(ur.df(dowjonesxts,type="none",lag=1))

dowjonesreturn = CalculateReturns(log(dowjonesxts))
dowjonesreturn=dowjonesreturn[-c(1),]

chartSeries(dowjonesreturn**2)

chart.Histogram(dowjonesreturn,methods = c("add.density","add.normal"),colorset = c("grey","red","blue"))
legend("topright",legend=c("return","kernel","normal dist."),fill=c("grey","red","blue"))


acf2(dowjonesreturn)
acf2(dowjonesreturn**2)

summary(ur.df(dowjonesreturn,type="none",lag=1))

chartSeries(dowjonesreturn**2)

auto.arima(dowjonesreturn)



##   0 0 1 1


### Normal ###
djspec1<-ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model="sGARCH",garchOrder=c(1,1)),distribution.model="norm")

djfit1<-ugarchfit(data=dowjonesreturn,spec = djspec1,out.sample = 20)
djfit1



##   4 4 1 2

##   4 2 1 1

##   0 0 1 1


### Students' T ###



djspec2<-ugarchspec(mean.model = list(armaOrder=c(4,4)),variance.model = list(model="sGARCH",garchOrder=c(1,2)),distribution.model="sstd")

djfit2<-ugarchfit(data=dowjonesreturn,spec = djspec2,out.sample = 20)
djfit2



djspec3<-ugarchspec(mean.model = list(armaOrder=c(4,2)),variance.model = list(model="sGARCH",garchOrder=c(1,1)),distribution.model="sstd")

djfit3<-ugarchfit(data=dowjonesreturn,spec = djspec3,out.sample = 20)
djfit3



djspec4<-ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model="sGARCH",garchOrder=c(1,1)),distribution.model="sstd")

djfit4<-ugarchfit(data=dowjonesreturn,spec = djspec4,out.sample = 20)
djfit4


### Comparison ###
par(mfrow=c(2,2))
plot(djfit1,which=8)
plot(djfit2,which=8)
plot(djfit3,which=8)
plot(djfit4,which=8)

##################
par(mfrow=c(2,2))
plot(djfit1,which=9)
plot(djfit2,which=9)
plot(djfit3,which=9)
plot(djfit4,which=9)

##################

Box.test(djfit1@fit$residuals, type="Ljung-Box")

Box.test(djfit2@fit$residuals, type="Ljung-Box")

Box.test(djfit3@fit$residuals, type="Ljung-Box")

Box.test(djfit4@fit$residuals, type="Ljung-Box")


###############################

par(mfrow=c(3,1))

ts.plot(djfit2@fit$sigma,xlab="",ylab="",main="Volatility of suggested model 1")
ts.plot(djfit3@fit$sigma,xlab="",ylab="",main="Volatility of suggested model 2")
ts.plot(djfit4@fit$sigma,xlab="",ylab="",main="Volatility of suggested model 3")

############## Forecast

dowjones_forecast2<- ugarchforecast(djfit2,n.ahead = 30)
dowjones_forecast3<- ugarchforecast(djfit3,n.ahead = 30)
dowjones_forecast4<- ugarchforecast(djfit4,n.ahead = 30)


dowjones_forecast@forecast$seriesFor

plot(dowjones_forecast2,which=1)
plot(dowjones_forecast3,which=1)
plot(dowjones_forecast4,which=1)



exp(dowjones_forecast2@forecast$seriesFor[2])


plot()




last= as.numeric(dow_jonesw$Close[length(dow_jonesw$Close)])
Update1<- c(as.numeric(dow_jonesw$Close))

for (i in seq(1,30)){
  last= last*exp(dowjones_forecast@forecast$seriesFor[i])
  Update1 <- c(Update1,last)
}

Update2


ts(Update2)


###########################
par(mfrow=c(1,3))

plot(ts(Update2),col="red",main="Dow Jones Forecast with ARMA(4,4) and GARCH(1,2)",ylab="",lwd=2,type="l")
lines(dow_jonesw$Close,lwd=2)
legend("topleft", legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')



plot(ts(Update1),col="red",main="Dow Jones Forecast with ARMA(4,2) and GARCH(1,2)",ylab="",lwd=2,type="l")
lines(dow_jonesw$Close,lwd=2)
legend("topleft", legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')

plot(ts(Update3),col="red",main="Dow Jones Forecast with ARMA(0,0) and GARCH(1,1)",ylab="",lwd=2,type="l")
lines(dow_jonesw$Close,lwd=2)
legend("topleft", legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')

############################


plot(Updat)











################################################################

##               Nikkei

#########################################################

nikkeixts<-xts(x=as.numeric(nikkeiw$Close),order.by = ymd(nikkeiw$Date))

chartSeries(nikkeixts)
acf2(nikkeixts)
summary(ur.df(nikkeixts,type="none",lag=1))

nkreturn = CalculateReturns(log(nikkeixts))
nkreturn=nkreturn[-c(1),]

chartSeries(nkreturn)

chart.Histogram(nkreturn,methods = c("add.density","add.normal"),colorset = c("grey","red","blue"))
legend("topright",legend=c("return","kernel","normal dist."),fill=c("grey","red","blue"))

chartSeries(nkreturn**2)


acf2(nkreturn)
acf2(nkreturn**2)


auto.arima(nkreturn)




####   1 1 1 1 
####   0 0 1 1

### Normal ###
nkspec1<-ugarchspec(mean.model = list(armaOrder=c(1,1)),variance.model = list(model="sGARCH",garchOrder=c(1,1)),distribution.model="norm")

nkfit1<-ugarchfit(data=nkreturn,spec = nkspec1,out.sample = 20)
nkfit1



nkspec2<-ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model="sGARCH",garchOrder=c(1,1)),distribution.model="norm")

nkfit2<-ugarchfit(data=nkreturn,spec = nkspec2,out.sample = 20)
nkfit2




####    2 4 1 4
####    0 0 1 1



### Students' T ###


##

nkspec3<-ugarchspec(mean.model = list(armaOrder=c(2,4)),variance.model = list(model="sGARCH",garchOrder=c(1,4)),distribution.model="sstd")

nkfit3<-ugarchfit(data=nkreturn,spec = nkspec3,out.sample = 20)
nkfit3

nkspec4<-ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model="sGARCH",garchOrder=c(1,1)),distribution.model="sstd")

nkfit4<-ugarchfit(data=nkreturn,spec = nkspec4,out.sample = 20)
nkfit4




### Comparison ###
par(mfrow=c(2,2))
plot(nkfit1,which=8)
plot(nkfit2,which=8)
plot(nkfit3,which=8)
plot(nkfit4,which=8)

##################
plot(nkfit1,which=9)
plot(nkfit2,which=9)
plot(nkfit3,which=9)
plot(nkfit4,which=9)

##################

Box.test(nkfit1@fit$residuals, type="Ljung-Box")

Box.test(nkfit2@fit$residuals, type="Ljung-Box")

Box.test(nkfit3@fit$residuals, type="Ljung-Box")

Box.test(nkfit4@fit$residuals, type="Ljung-Box")

##################

par(mfrow=c(2,1))

ts.plot(nkfit3@fit$sigma,xlab="",ylab="",main="Volatility of suggested model 1")
ts.plot(nkfit4@fit$sigma,xlab="",ylab="",main="Volatility of suggested model 2")




############## Forecast

nk_forecast3<- ugarchforecast(nkfit3,n.ahead = 30)

nk_forecast4<- ugarchforecast(nkfit4,n.ahead = 30)


nk_forecast@forecast$seriesFor

par(mfrow=c(1,2))
plot(nk_forecast3,which=1)
plot(nk_forecast4,which=1)



exp(nk_forecast@forecast$seriesFor[2])


last= as.numeric(nikkeiw$Close[length(nikkeiw$Close)])
Update4<- c(as.numeric(nikkeiw$Close))

for (i in seq(1,30)){
  last= last*exp(nk_forecast4@forecast$seriesFor[i])
  Update4 <- c(Update4,last)
}

Update


ts(Update)


###########################
plot(ts(Update3),col="red",main="Nikkei Forecast with ARMA(2,4) and GARCH(1,4)",ylab="",lwd=2,type="l")
lines(nikkeiw$Close,lwd=2)
legend("topleft", legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')

plot(ts(Update4),col="red",main="Nikkei Forecast with ARMA(0,0) and GARCH(1,1)",ylab="",lwd=2,type="l")
lines(nikkeiw$Close,lwd=2)
legend("topleft", legend=c("Forecast","Actual"),
       col=c("red","black"), lty=1, cex=0.8,text.font=4, bg='lightblue')

##############################









data<-data.frame(NIFTY=as.numeric(niftyw$Close[1:372]),DAX=as.numeric(daxw$Close[1:372]),DOW_JONES=as.numeric(dow_jonesw$Close[1:372]),NIKKEI=as.numeric(nikkeiw$Close[1:372]))

correl<-cor(data)
library(corrplot)
corrplot(correl, type = "upper", method="square", order = "hclust",
         tl.col = "black", tl.srt = 30,addCoef.col = "white")





d1=data$NIFTY-data$DAX
d_hat=mean(d1)
d_sd=(var(d1))**0.5
T_d= (length(d1)**0.5)*(d_hat/d_sd)
T_d
