rm(list=ls())
setwd("C:/Users/hbhaskarrajusd/Desktop/New model failure")

Rel_Data <- read.csv("NewModelData.csv",h=T)

Rel_Data$production_date = NULL
Rel_Data$sold_date = NULL
Rel_Data$Repair_date = NULL
#Rel_Data = Rel_Data[Rel_Data$CFT_Engine == "Yes" | Rel_Data$CFT_Gats == "Yes",]
cutoff= 4
req= 4
Monthly_Repairs = numeric(req-cutoff+1)

Rel_Data1 = Rel_Data[which(Rel_Data$Production_Month<cutoff & Rel_Data$Sold_Month<cutoff & Rel_Data$Repair_Month<cutoff),]
# actual=Rel_Data[which(Rel_Data$Repair_Month == cutoff),]
actual=Rel_Data[which(Rel_Data$Repair_Month>=cutoff & Rel_Data$Repair_Month< (req+1)),]
sum(Rel_Data1$SN)+sum(actual$SN)

Run_Base <- data.frame(Repair_days=1:((cutoff-1)*30),Repairs = 0)
#Run_Final <- subset(Run_Final,select = -c(Repairs.x))

Run_Agg <- aggregate(Rel_Data1$SN, by = list(Rel_Data1$Repair_days), FUN = sum)
names(Run_Agg) = c("Repair_days","Repairs")
library(dplyr)

Run_Final <- left_join(Run_Base,Run_Agg, by = "Repair_days",copy=T)
Run_Final[is.na(Run_Final)] = 0

names(Run_Final)[2] = "Repairs"



for(i in cutoff:req) {
  #
  #i = 4
  j = (((i-1)*30)+1)
  k = (i*30)
  
  
  startW<-as.numeric(strftime(head(as.Date('2015-01-01'),1),format="%W"))
  startD<-as.numeric(strftime(head(as.Date('2015-01-01'),1)+1,format="%w"))
  times = ts(Run_Final$Repairs,frequency=7,start=c(startW,startD))#,calendar=T) 
  
  library(forecast)
  hw=HoltWinters(times)
  
  arm=auto.arima(times,ic="bic")
  
  forecast_period=30
  
  fhw=forecast.HoltWinters(hw,forecast_period)
  farm=forecast.Arima(arm,forecast_period)
  ed=nrow(Run_Final)
  ssehw=sum((fhw$fitted-times[8:ed])^2)
  sseam=sum((farm$fitted[8:ed]-times[8:ed])^2)
  
  library(MLmetrics)
  if (sseam < ssehw & any(grepl("ar1",names(arm$coef))) != F ) {
    print("ARIMA")
    pred=farm$mean
  } else  {
    print("HOLT-WINTERS")
    pred=fhw$mean
  }
  pred=ifelse(pred < 0,0,pred)
  
  Run_Final = rbind(Run_Final,data.frame(Repair_days=c(j:k),Repairs=pred,stringsAsFactors = FALSE))
  Monthly_Repairs[i-cutoff+1] = round(sum(pred))
}
total_repairs = sum(Monthly_Repairs)
actualrepairs = sum(actual$SN)
RMSPE(total_repairs,actualrepairs)*100
total_repairs + sum(Rel_Data1$SN)


VM_Data <- read.csv("VM_HB10UF.csv",h=T)

cutoff=10
req=12
strt_date = '2015-01-01'
Monthly_Vehicles_Sold = numeric(req-cutoff+1)

actual_VS=VM_Data[which(VM_Data$Repair_days>((cutoff-1)*30) & VM_Data$Repair_days<=(req*30)),]
# actual_VS <- VM_Data[which(VM_Data$Repair_days > ((cutoff-1)*30) 
#                           & VM_Data$Repair_days <= (cutoff*30)),]
Rel_Data1 <- VM_Data[which(VM_Data$Repair_days <= ((cutoff-1)*30)),]
sum(Rel_Data1$VINS)+sum(actual_VS$VINS)
Run_Base <- data.frame(Repair_days=1:((cutoff-1)*30),VINS = 0)

Run_Agg <- aggregate(Rel_Data1$VINS, by = list(Rel_Data1$Repair_days), FUN = sum)
names(Run_Agg) = c("Repair_days","VINS")


Run_Final <- left_join(Run_Base,Run_Agg, by = "Repair_days",copy=T)
Run_Final[is.na(Run_Final)] = 0

Run_Final <- subset(Run_Final,select = -c(VINS.x))
names(Run_Final)[2] = "VINS"

Pos_Rep = which(Run_Final$VINS >= 2*sd(Run_Final$VINS))
Run_Final[Pos_Rep,2]=  mean(Run_Final$VINS)

mfrow=c(1,1)
for(i in cutoff:req) {
  # i=4
  j = (((i-1)*30)+1)
  k = (i*30)
  
  
  startW<-as.numeric(strftime(head(as.Date(strt_date),1),format="%W"))
  startD<-as.numeric(strftime(head(as.Date(strt_date),1)+1,format="%w"))
  # times_VS = ts(Run_Final$Repairs,frequency=7,start=c(startW,startD))#,calendar=T)
  times_VS = ts(Run_Final$VINS,frequency=7,start=c(startW,startD))
  
  
  hw_VS=HoltWinters(times_VS)
  arm_VS=auto.arima(times_VS,ic="bic")
  
  forecast_period=30
  fhw_VS=forecast.HoltWinters(hw_VS,forecast_period)
  farm_VS=forecast.Arima(arm_VS,forecast_period)
  
  ed=nrow(Run_Final)
  ssehw_VS=sum((fhw_VS$fitted-times_VS[8:(ed)])^2)
  sseam_VS=sum((farm_VS$fitted[8:ed]-times_VS[8:ed])^2)
  library(MLmetrics)
  if (sseam_VS < ssehw_VS & any(grepl("ar1",names(arm_VS$coef))) != F ) {
    print("ARIMA")
    pred_VS=farm_VS$mean
  } else  {
    print("HOLT-WINTERS")
    pred_VS=fhw_VS$mean
  }
  pred_VS=ifelse(pred_VS < 0,0,pred_VS)
  Run_Final = rbind(Run_Final,data.frame(Repair_days=c(j:k)
                                         ,VINS=pred_VS
                                         ,stringsAsFactors = FALSE))
  
  Monthly_Vehicles_Sold[i-cutoff+1] = round(sum(pred_VS))
}
pred_vehiclessold = sum(Monthly_Vehicles_Sold)
actual_vehiclessold = sum(actual_VS$VINS)
RMSPE(pred_vehiclessold,actual_vehiclessold)*100
sum(Rel_Data1$VINS) + pred_vehiclessold 




pred_permillion = (1000000*total_repairs)/pred_vehiclessold
  
actual_permillion = (1000000*actualrepairs)/actual_vehiclessold
RMSPE(pred_permillion,actual_permillion)*100
