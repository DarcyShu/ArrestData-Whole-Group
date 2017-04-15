rm(list=ls())
setwd("C:/Users/dengj/Desktop/2016-2017 Spring/R/data")
dat=read.csv("ArrestData.csv")
y<-regexpr("/",dat$OFFENSES)
idx<-which(y>0)
offense<-NULL
i=0
for(n in 1:length(idx)){
  i=i+1
  offense[i]<-substr(dat$OFFENSES[idx[n]],1,y[idx[n]]-1)
}
dat1<-dat[idx,]
dat1$OFFENSES<-offense
dat2<-dat[-idx,]
arrest<-rbind(dat1,dat2)

y6<-regexpr(" ",arrest$OFFENSES)
code<-NULL
i=0
for(n in 1:length(y6)){
  i=i+1
  code[i]<-substr(arrest$OFFENSES[n],1,y6[n]-1)
}
shortcode<-code
arrest<-data.frame(arrest$AGE,arrest$GENDER,arrest$ARRESTTIME,arrest$OFFENSES,arrest$RACE,code,shortcode)

y1<-regexpr("[(]",arrest$code)
idx1<-which(y1>0)
code1<-NULL
i=0
for(n in 1:length(idx1)){
  i=i+1
  code1[i]<-substr(arrest$code[idx1[n]],1,y1[idx1[n]]-1)
}
arrest1<-arrest[idx1,]
arrest1$code<-code1
arrest2<-arrest[-idx1,]
arrest<-rbind(arrest1,arrest2)

y2<-regexpr("[.]",arrest$code)
idx2<-which(y2>0)
code2<-NULL
i=0
for(n in 1:length(idx2)){
  i=i+1
  code2[i]<-substr(arrest$code[idx2[n]],1,y2[idx2[n]]-1)
}
arrest1<-arrest[idx2,]
arrest1$code<-code2
arrest2<-arrest[-idx2,]
arrest<-rbind(arrest1,arrest2)

idx5<-which(arrest$code==13)
arrest$code[idx5]<-"13(a)"
arrest$shortcode<-arrest$code

idx6<-which(nchar(arrest$shortcode)==3)
idx7<-which(nchar(arrest$shortcode)==4)

arrest$shortcode[idx6]<-substr(arrest$shortcode[idx6],1,1) 
arrest$shortcode[idx7]<-substr(arrest$shortcode[idx7],1,2) 
arrest<-na.omit(arrest)
table(arrest$arrest.AGE)

idx8<-which(arrest$arrest.AGE==0|arrest$arrest.AGE==999)
arrest<-arrest[-idx8,]

idx9<-which(arrest$shortcode=="27"|arrest$shortcode=="13(a)"|arrest$shortcode=="9"|arrest$shortcode=="39")
ArrestTopFour<-arrest[idx9,]
sort(table(ArrestTopFour$shortcode))

library(lubridate)
table(year(ArrestTopFour$arrest.ARRESTTIME))

idx10<-which(year(ArrestTopFour$arrest.ARRESTTIME)==2014)
ArrestTopFour<-ArrestTopFour[-idx10,]
sort(table(ArrestTopFour$shortcode))

ArrestTopFour<-data.frame(ArrestTopFour$arrest.AGE,ArrestTopFour$arrest.GENDER,ArrestTopFour$arrest.RACE,ArrestTopFour$arrest.OFFENSES,ArrestTopFour$code,ArrestTopFour$shortcode)

colnames(ArrestTopFour)<-c("AGE","GENDER","RACE","OFFENSE","CODE","SHORTCODE")


#ggplot2
install.packages("ggplot2")
library(ggplot2)
library(plyr)

ggplot(data=ArrestTopFour,aes(x=AGE,fill=GENDER)) + 
  geom_bar(data=subset(ArrestTopFour,GENDER=="F")) + 
  geom_bar(data=subset(ArrestTopFour,GENDER=="M"),aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-100,100,10),labels=abs(seq(-100,100,10))) + 
  coord_flip()+
  facet_grid(SHORTCODE~.)



datgraph<-dat
idx3<-which(datgraph$AGE>0 & datgraph$AGE<100)
datgraph<-datgraph[idx3,]

ggplot(data=datgraph,aes(x=AGE,fill=RACE)) + 
  geom_bar(data=subset(datgraph,RACE=="B")) + 
  geom_bar(data=subset(datgraph,RACE=="W"),aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-200,200,40),labels=abs(seq(-200,200,40))) + 
  coord_flip()





#Data Recleaning
rm(list=ls())
setwd("C:/Users/dengj/Desktop/2016-2017 Spring/R/data")
dat=read.csv("ArrestDataFinal.csv")

#Data Recleaning

library(lubridate)
idx<-which(dat$AGE<=21&dat$AGE>0)
dat1<-dat[idx,]
idx1<-which(year(dat1$ARRESTTIME)==2014|year(dat1$ARRESTTIME)==2015)
dat1<-dat1[-idx1,]
ArrestTeen<-data.frame(dat1$X_id,dat1$AGE,dat1$GENDER,dat1$RACE,dat1$ARRESTTIME,dat1$OFFENSES,dat1$ARRESTLOCATION,dat1$PK,dat1$CCR)
library(lubridate)
arresttime<-ArrestTeen$dat1.ARRESTTIME
arresttime2<-sub("T", " ", arresttime)
arresttime3 <- as.POSIXct(arresttime2)
hour2<-as.character(hour(arresttime3))
months2<-as.character(months(arresttime3))
weekdays2<-as.character(weekdays(arresttime3))
ArrestTeen$dat1.ARRESTLOCATION<-hour2
ArrestTeen$dat1.PK<-months2
ArrestTeen$dat1.CCR<-weekdays2
colnames(ArrestTeen)<-c("ID","AGE","GENDER","RACE","ArrestTime","OFFENSE","Hour","Month","Weekday")

library(dplyr)
library(tidyr)
ArrestTeen<-ArrestTeen%>%mutate(OFFENSE=strsplit(as.character(OFFENSE),"/"))%>%
  unnest(OFFENSE)

#LongCode and ShortCode
y1<-regexpr("[0-9]+",ArrestTeen$OFFENSE)
idx2<-which(y1>0)
ArrestTeen<-ArrestTeen[idx2,]

y2<-regexpr(" ",ArrestTeen$OFFENSE)
idx3<-which(y2==1)
code<-NULL
i=0
for(n in 1:length(idx3)){
  i=i+1
  code[i]<-substr(ArrestTeen$OFFENSE[idx3[n]],2,nchar(ArrestTeen$OFFENSE[idx3[n]]))
}

dat1<-ArrestTeen[idx3,]
dat2<-ArrestTeen[-idx3,]
dat1$OFFENSE<-code
ArrestTeen<-rbind(dat1,dat2)

y3<-regexpr(" ",ArrestTeen$OFFENSE)
code1<-NULL
i=0
for(n in 1:length(y3)){
  i=i+1
  code1[i]<-substr(ArrestTeen$OFFENSE[n],1,y3[n]-1)
}

ArrestTeen$Code<-code1

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


#Get Unique ShortCode for EACH One Offender
ArrestTeen1<-distinct(ArrestTeen,ID,AGE,GENDER,RACE,ArrestTime,Hour,Month,Weekday,ShortCode)

#write CSV
setwd("C:/Users/dengj/Desktop/2016-2017 Spring/R/data")
write.csv(ArrestTeen,"C:/Users/dengj/Desktop/2016-2017 Spring/R/data/ArrestTeen.csv")
write.csv(ArrestTeen1,"C:/Users/dengj/Desktop/2016-2017 Spring/R/data/ArrestTeen1.csv")



#Data Cleaning Rewite
rm(list=ls())
setwd("C:/Users/dengj/Desktop/2016-2017 Spring/R/data")
dat=read.csv("ArrestDataFinal.csv")

#as.date  Extract only the crimes happening in 2016&2017
library(lubridate)
idx<-which(year(dat$ARRESTTIME)==2014|year(dat$ARRESTTIME)==2015)
dat<-dat[-idx,]

#Remove Outliers
idx1<-which(dat$AGE>0 & dat$AGE<100)
dat<-dat[idx1,]

#Creat Binary Variable
Arrest<-data.frame(dat$X_id,dat$AGE,dat$GENDER,dat$RACE,dat$ARRESTTIME,dat$OFFENSES)
Arrest$TENN<-Arrest$dat.AGE<22
table(Arrest$TENN)
