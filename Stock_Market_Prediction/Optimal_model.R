AICC <- function(n,k,AIC){
  return(AIC+ (2*((k)**2+k)/(n-k-1)))
}










Nif_norm<-read.csv("Sem_3/Time_Series/Data/AIC_BIC/Nif_norm_AIC_BIC.csv")

for (i in seq(1,length(Nif_norm$AIC))){
  Nif_norm$AICC[i] <- AICC(length(niftyw$Close),sum(as.numeric(str_split(Nif_norm$AMG[i]," ")[[1]])),Nif_norm$AIC[i])
  
}




par(mfrow=c(3,1))
ts.plot(Nif_norm$AIC,main=paste(Nif_norm$AMG[which(Nif_norm==min(Nif_norm$AIC),arr.ind = T)[1]],"ARMA + GARCH for AIC"),xlab="",ylab="")
abline(v=which(Nif_norm==min(Nif_norm$AIC),arr.ind = T)[1],col="red")
ts.plot(Nif_norm$BIC,main=paste(Nif_norm$AMG[which(Nif_norm==min(Nif_norm$BIC),arr.ind = T)[1]],"ARMA + GARCH for BIC"),xlab="",ylab="")
abline(v=which(Nif_norm==min(Nif_norm$BIC),arr.ind = T)[1],col="red")
ts.plot(Nif_norm$AICC,main=paste(Nif_norm$AMG[which(Nif_norm==min(Nif_norm$AICC),arr.ind = T)[1]],"ARMA + GARCH for AICC"),xlab="",ylab="")
abline(v=which(Nif_norm==min(Nif_norm$AICC),arr.ind = T)[1],col="red")



str_split(Nif_norm$AMG[which(Nif_norm==min(Nif_norm$AIC),arr.ind = T)[1]]," ")[[1]]





Nif_sstd<-read.csv("Sem_3/Time_Series/Data/AIC_BIC/Nif_sstd_AIC_BIC.csv")


for (i in seq(1,length(Nif_sstd$AIC))){
  Nif_sstd$AICC[i] <- AICC(length(niftyw$Close),sum(as.numeric(str_split(Nif_sstd$AMG[i]," ")[[1]])),Nif_sstd$AIC[i])
  
}




par(mfrow=c(3,1))
ts.plot(Nif_sstd$AIC,main=paste(Nif_sstd$AMG[which(Nif_sstd==min(Nif_sstd$AIC),arr.ind = T)[1]],"ARMA + GARCH for AIC"),xlab="",ylab="")
abline(v=which(Nif_sstd==min(Nif_sstd$AIC),arr.ind = T)[1],col="red")
ts.plot(Nif_sstd$BIC,main=paste(Nif_sstd$AMG[which(Nif_sstd==min(Nif_sstd$BIC),arr.ind = T)[1]],"ARMA + GARCH for BIC"),xlab="",ylab="")
abline(v=which(Nif_sstd==min(Nif_sstd$BIC),arr.ind = T)[1],col="red")
ts.plot(Nif_sstd$AICC,main=paste(Nif_sstd$AMG[which(Nif_sstd==min(Nif_sstd$AICC),arr.ind = T)[1]],"ARMA + GARCH for AICC"),xlab="",ylab="")
abline(v=which(Nif_sstd==min(Nif_sstd$AICC),arr.ind = T)[1],col="red")





#########   DAX        ###############


Dax_norm <- read.csv("Sem_3/Time_Series/Data/AIC_BIC/Dax_norm_AIC_BIC.csv")




for (i in seq(1,length(Dax_norm$AIC))){
  Dax_norm$AICC[i] <- AICC(length(daxw$Close),sum(as.numeric(str_split(Dax_norm$AMG[i]," ")[[1]])),Dax_norm$AIC[i])
  
}




par(mfrow=c(3,1))
ts.plot(Dax_norm$AIC,main=paste(Dax_norm$AMG[which(Dax_norm==min(Dax_norm$AIC),arr.ind = T)[1]],"ARMA + GARCH for AIC"),xlab="",ylab="")
abline(v=which(Dax_norm==min(Dax_norm$AIC),arr.ind = T)[1],col="red")
ts.plot(Dax_norm$BIC,main=paste(Dax_norm$AMG[which(Dax_norm==min(Dax_norm$BIC),arr.ind = T)[1]],"ARMA + GARCH for BIC"),xlab="",ylab="")
abline(v=which(Dax_norm==min(Dax_norm$BIC),arr.ind = T)[1],col="red")
ts.plot(Dax_norm$AICC,main=paste(Dax_norm$AMG[which(Dax_norm==min(Dax_norm$AICC),arr.ind = T)[1]],"ARMA + GARCH for AICC"),xlab="",ylab="")
abline(v=which(Dax_norm==min(Dax_norm$AICC),arr.ind = T)[1],col="red")




Dax_sstd <- read.csv("Sem_3/Time_Series/Data/AIC_BIC/DAX_sstd_AIC_BIC.csv")

for (i in seq(1,length(Dax_sstd$AIC))){
  Dax_sstd$AICC[i] <- AICC(length(daxw$Close),sum(as.numeric(str_split(Dax_sstd$AMG[i]," ")[[1]])),Dax_sstd$AIC[i])
  
}




par(mfrow=c(3,1))
ts.plot(Dax_sstd$AIC,main=paste(Dax_sstd$AMG[which(Dax_sstd==min(Dax_sstd$AIC),arr.ind = T)[1]],"ARMA + GARCH for AIC"),xlab="",ylab="")
ts.plot(Dax_sstd$BIC,main=paste(Dax_sstd$AMG[which(Dax_sstd==min(Dax_sstd$BIC),arr.ind = T)[1]],"ARMA + GARCH for BIC"),xlab="",ylab="")
ts.plot(Dax_sstd$AICC,main=paste(Dax_sstd$AMG[which(Dax_sstd==min(Dax_sstd$AICC),arr.ind = T)[1]],"ARMA + GARCH for AICC"),xlab="",ylab="")




















Dow_norm <- read.csv("Sem_3/Time_Series/Data/AIC_BIC/Dow_norm_AIC_BIC.csv")

for (i in seq(1,length(Dow_norm$AIC))){
  Dow_norm$AICC[i] <- AICC(length(dow_jonesw$Close),sum(as.numeric(str_split(Dow_norm$AMG[i]," ")[[1]])),Dow_norm$AIC[i])
  
}




par(mfrow=c(3,1))
ts.plot(Dow_norm$AIC,main=paste(Dow_norm$AMG[which(Dow_norm==min(Dow_norm$AIC),arr.ind = T)[1]],"ARMA + GARCH for AIC"),xlab="",ylab="")
abline(v=which(Dow_norm==min(Dow_norm$AIC),arr.ind = T)[1],col="red")
ts.plot(Dow_norm$BIC,main=paste(Dow_norm$AMG[which(Dow_norm==min(Dow_norm$BIC),arr.ind = T)[1]],"ARMA + GARCH for BIC"),xlab="",ylab="")
abline(v=which(Dow_norm==min(Dow_norm$BIC),arr.ind = T)[1],col="red")
ts.plot(Dow_norm$AICC,main=paste(Dow_norm$AMG[which(Dow_norm==min(Dow_norm$AICC),arr.ind = T)[1]],"ARMA + GARCH for AICC"),xlab="",ylab="")
abline(v=which(Dow_norm==min(Dow_norm$AICC),arr.ind = T)[1],col="red")



Dow_sstd <- read.csv("Sem_3/Time_Series/Data/AIC_BIC/Dow_sstd_AIC_BIC.csv")

for (i in seq(1,length(Dow_sstd$AIC))){
  Dow_sstd$AICC[i] <- AICC(length(dow_jonesw$Close),sum(as.numeric(str_split(Dow_sstd$AMG[i]," ")[[1]])),Dow_sstd$AIC[i])
  
}




par(mfrow=c(3,1))
ts.plot(Dow_sstd$AIC,main=paste(Dow_sstd$AMG[which(Dow_sstd==min(Dow_sstd$AIC),arr.ind = T)[1]],"ARMA + GARCH for AIC"),xlab="",ylab="")
abline(v=which(Dow_sstd==min(Dow_sstd$AIC),arr.ind = T)[1],col="red")
ts.plot(Dow_sstd$BIC,main=paste(Dow_sstd$AMG[which(Dow_sstd==min(Dow_sstd$BIC),arr.ind = T)[1]],"ARMA + GARCH for BIC"),xlab="",ylab="")
abline(v=which(Dow_sstd==min(Dow_sstd$BIC),arr.ind = T)[1],col="red")
ts.plot(Dow_sstd$AICC,main=paste(Dow_sstd$AMG[which(Dow_sstd==min(Dow_sstd$AICC),arr.ind = T)[1]],"ARMA + GARCH for AICC"),xlab="",ylab="")
abline(v=which(Dow_sstd==min(Dow_sstd$AICC),arr.ind = T)[1],col="red")















Nik_norm <- read.csv("Sem_3/Time_Series/Data/AIC_BIC/Nik_norm_AIC_BIC.csv")

for (i in seq(1,length(Nik_norm$AIC))){
  Nik_norm$AICC[i] <- AICC(length(nikkeiw$Close),sum(as.numeric(str_split(Nik_norm$AMG[i]," ")[[1]])),Nik_norm$AIC[i])
  
}




par(mfrow=c(3,1))
ts.plot(Nik_norm$AIC,main=paste(Nik_norm$AMG[which(Nik_norm==min(Nik_norm$AIC),arr.ind = T)[1]],"ARMA + GARCH for AIC"),xlab="",ylab="")
abline(v=which(Nik_norm==min(Nik_norm$AIC),arr.ind = T)[1],col="red")
ts.plot(Nik_norm$BIC,main=paste(Nik_norm$AMG[which(Nik_norm==min(Nik_norm$BIC),arr.ind = T)[1]],"ARMA + GARCH for BIC"),xlab="",ylab="")
abline(v=which(Nik_norm==min(Nik_norm$BIC),arr.ind = T)[1],col="red")
ts.plot(Nik_norm$AICC,main=paste(Nik_norm$AMG[which(Nik_norm==min(Nik_norm$AICC),arr.ind = T)[1]],"ARMA + GARCH for AICC"),xlab="",ylab="")
abline(v=which(Nik_norm==min(Nik_norm$AICC),arr.ind = T)[1],col="red")




Nik_sstd <- read.csv("Sem_3/Time_Series/Data/AIC_BIC/NIK_sstd_AIC_BIC.csv")

for (i in seq(1,length(Nik_norm$AIC))){
  Nik_sstd$AICC[i] <- AICC(length(nikkeiw$Close),sum(as.numeric(str_split(Nik_sstd$AMG[i]," ")[[1]])),Nik_sstd$AIC[i])
  
}




par(mfrow=c(3,1))
ts.plot(Nik_sstd$AIC,main=paste(Nik_sstd$AMG[which(Nik_sstd==min(Nik_sstd$AIC),arr.ind = T)[1]],"ARMA + GARCH for AIC"),xlab="",ylab="")
abline(v=which(Nik_sstd==min(Nik_sstd$AIC),arr.ind = T)[1],col="red")
ts.plot(Nik_sstd$BIC,main=paste(Nik_sstd$AMG[which(Nik_sstd==min(Nik_sstd$BIC),arr.ind = T)[1]],"ARMA + GARCH for BIC"),xlab="",ylab="")
abline(v=which(Nik_sstd==min(Nik_sstd$BIC),arr.ind = T)[1],col="red")
ts.plot(Nik_sstd$AICC,main=paste(Nik_sstd$AMG[which(Nik_sstd==min(Nik_sstd$AICC),arr.ind = T)[1]],"ARMA + GARCH for AICC"),xlab="",ylab="")
abline(v=which(Nik_sstd==min(Nik_sstd$AICC),arr.ind = T)[1],col="red")




