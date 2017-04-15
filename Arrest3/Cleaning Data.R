#Data Recleaning
rm(list=ls())
setwd("C:/Users/dengj/Desktop/2016-2017 Spring/R/data")
dat=read.csv("ArrestDataFinal.csv")

#Data Recleaning

library(lubridate)
idx<-which(dat$AGE<100&dat$AGE>0)
dat1<-dat[idx,]
idx1<-which(year(dat1$ARRESTTIME)==2014|year(dat1$ARRESTTIME)==2015)
dat1<-dat1[-idx1,]
ArrestTeen<-data.frame(dat1$X_id,dat1$AGE,dat1$GENDER,dat1$RACE,dat1$ARRESTTIME,dat1$OFFENSES,dat1$ARRESTLOCATION,dat1$PK,dat1$CCR)
ArrestTeen$TENN<-ArrestTeen$dat1.AGE<22
ArrestTeen$TENN[which(ArrestTeen$TENN=="TRUE")]<-1
ArrestTeen$TENN[which(ArrestTeen$TENN=="FALSE")]<-0

library(lubridate)
arresttime<-ArrestTeen$dat1.ARRESTTIME
arresttime2<-sub("T", " ", arresttime)
arresttime3<-as.POSIXlt(arresttime2,format = "%Y-%m-%d %H:%M:%S")
hour2<-as.character(hour(arresttime3))
months2<-as.character(months(arresttime3))
weekdays2<-as.character(weekdays(arresttime3))
ArrestTeen$dat1.ARRESTLOCATION<-hour2
ArrestTeen$dat1.PK<-months2
ArrestTeen$dat1.CCR<-weekdays2
colnames(ArrestTeen)<-c("ID","AGE","GENDER","RACE","ArrestTime","OFFENSE","Hour","Month","Weekday","Teen")
ArrestTeenForVincent<-ArrestTeen

#Data Recleaning
rm(list=ls())
setwd("C:/Users/dengj/Desktop/2016-2017 Spring/R/data")
dat=read.csv("ArrestDataFinal.csv")

#Data Recleaning

library(lubridate)
idx<-which(dat$AGE<100&dat$AGE>0)
dat1<-dat[idx,]
idx1<-which(year(dat1$ARRESTTIME)==2014|year(dat1$ARRESTTIME)==2015)
dat1<-dat1[-idx1,]
ArrestTeen<-data.frame(dat1$X_id,dat1$AGE,dat1$GENDER,dat1$RACE,dat1$ARRESTTIME,dat1$OFFENSES,dat1$ARRESTLOCATION,dat1$PK,dat1$CCR)
ArrestTeen$TENN<-ArrestTeen$dat1.AGE<22
ArrestTeen$TENN[which(ArrestTeen$TENN=="TRUE")]<-1
ArrestTeen$TENN[which(ArrestTeen$TENN=="FALSE")]<-0

library(lubridate)
arresttime<-ArrestTeen$dat1.ARRESTTIME
arresttime2<-sub("T", " ", arresttime)
arresttime3<-as.POSIXlt(arresttime2,format = "%Y-%m-%d %H:%M:%S")
hour2<-as.character(hour(arresttime3))
months2<-as.character(months(arresttime3))
weekdays2<-as.character(weekdays(arresttime3))
ArrestTeen$dat1.ARRESTLOCATION<-hour2
ArrestTeen$dat1.PK<-months2
ArrestTeen$dat1.CCR<-weekdays2
colnames(ArrestTeen)<-c("ID","AGE","GENDER","RACE","ArrestTime","OFFENSE","Hour","Month","Weekday","Teen")
ArrestTeenForVincent<-ArrestTeen


#Data for Yilin
y2<-regexpr(" ",ArrestTeen$OFFENSE)
code<-NULL
i=0
for(n in 1:length(y2)){
  i=i+1
  code[i]<-substr(ArrestTeen$OFFENSE[n],1,y2[n]-1)
}
ArrestTeen$Code<-code

y4<-regexpr("[(]",ArrestTeen$Code)
idx4<-which(y4>0)
code2<-NULL
i=0
for(n in 1:length(idx4)){
  i=i+1
  code2[i]<-substr(ArrestTeen$Code[idx4[n]],1,y4[idx4[n]]-1)
}
dat1<-ArrestTeen[idx4,]
dat1$ShortCode<-code2
dat2<-ArrestTeen[-idx4,]
dat2$ShortCode<-dat2$Code
ArrestTeen<-rbind(dat1,dat2)
ArrestTeen$Code<-ArrestTeen$ShortCode
idx5<-which(ArrestTeen$Code==13)
ArrestTeen$Code[idx5]<-"13(a)"
ArrestTeen$ShortCode[idx5]<-"13(a)"

y5<-regexpr("[.]",ArrestTeen$Code)
idx6<-which(y5>0)
code3<-NULL
i=0
for(n in 1:length(idx6)){
  i=i+1
  code3[i]<-substr(ArrestTeen$Code[idx6[n]],1,y5[idx6[n]]-1)
}
arrest1<-ArrestTeen[idx6,]
arrest1$Code<-code3
arrest2<-ArrestTeen[-idx6,]
ArrestTeen<-rbind(arrest1,arrest2)
ArrestTeen$ShortCode<-ArrestTeen$Code

idx7<-which(nchar(ArrestTeen$ShortCode)==3)
idx8<-which(nchar(ArrestTeen$ShortCode)==4)
ArrestTeen$ShortCode[idx7]<-substr(ArrestTeen$ShortCode[idx7],1,1)
ArrestTeen$ShortCode[idx8]<-substr(ArrestTeen$ShortCode[idx8],1,2)

ArrestTeenForYilin<-ArrestTeen