#Arrest Team 3
#Team Member:
# Jinshi Deng
# Vincent Fan
# Yilin Lu

#1. Data Recleaning: Jinshi Deng
rm(list=ls())
setwd("/Users/apple/Desktop/GSPIA/GSPIA/study/2017 Spring/R/Class Materials")
dat=read.csv("ArrestData.csv")

#Data Recleaning

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
install.packages("tidyr")
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

#Graph 2.1 & 2.2: Yilin Lu

#Graph 2.1: Type bar plot including all crimes 
table(ArrestTeen$ShortCode)   #Crime type 27 ranks first as 604 cases.
library(ggplot2)
ArrestTeenG1<-sort(ArrestTeen$ShortCode,decreasing = TRUE)
Gender<-ArrestTeen$GENDER
a<-ggplot(dat=ArrestTeen,aes(x=ArrestTeen$ShortCode,fill=Gender))
a+geom_bar(stat="count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Short Offense Code")+
  geom_text(stat="count",aes(label=..count..), check_overlap = TRUE,position = position_stack(vjust = 0.5),size=3)+
  ggtitle("Distribution of All Crime Types Among Teenagers")   


#Graph 2.2: Top crime types distribution among teenagers from 10 to 21
idx27<-which(ArrestTeen$ShortCode==27)
Age27<-ArrestTeen[idx27,]
idx13<-which(ArrestTeen$ShortCode=="13(a)")
Age13<-ArrestTeen[idx13,]
AgeTopCrime<-rbind(Age27,Age13)

##Divide age by age group: elementary school, junior high school, senior high school
agegroup<-ifelse(AgeTopCrime$AGE<=13,"10-13",ifelse(AgeTopCrime$AGE<=18,"14-18","19-21"))
AgeTopCrime<-cbind(AgeTopCrime,agegroup)
agegroup<-AgeTopCrime$agegroup
Weekday<-AgeTopCrime$Weekday

###Every age
b<-ggplot(dat=AgeTopCrime,aes(x=AgeTopCrime$AGE,fill=Weekday))
b+geom_bar(stat="count")+
  xlab("Age")+
  scale_fill_brewer(palette = "Blues")+
  geom_text(stat="count",aes(label=..count..),check_overlap = TRUE,size=3,position = position_stack(vjust = 0.5),angle=45)+
  scale_x_continuous(breaks=c(10,12,15,18,21))+
  facet_grid(.~AgeTopCrime$ShortCode)+
  ggtitle("Top 2 Crimes Among Teenagers")

####Age group
c<-ggplot(dat=AgeTopCrime,aes(x=agegroup,fill=Weekday))
c+geom_bar(stat="count")+
  xlab("Age")+
  scale_fill_brewer(palette = "Blues")+
  geom_text(stat="count",aes(label=..count..),check_overlap = TRUE,size=3,position = position_stack(vjust = 0.5),angle=45)+
  facet_grid(.~AgeTopCrime$ShortCode)+
  ggtitle("Top 2 Crimes Among Teenagers")


# Graph 2.3 & 2.4: Vincent Fan
rm(list=ls())
setwd("C:/Users/yacheng/Desktop/Courses/2nd semester/R/Final project/Group5 Data Visulization/ArrestData-Whole-Group")
teen<-read.csv("ArrestTeen.csv")

#library useful packages
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(dplyr)

####By Weekday
teen1<-teen
par(family = "Microsoft Accor black")
#Overall Cases
dat<-teen1 %>%
  select(Hour,Month,Weekday,ShortCode) %>% 
  group_by(Weekday) %>% 
  summarize(N=n())
dat$Weekday <- factor(dat$Weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
p<-ggplot(dat,aes(x=Weekday,y=N))
p+geom_bar(stat = "identity",fill="#084594")+xlab("")+ylab("")+
  ggtitle("")+
  theme_classic()+theme(axis.ticks = element_blank(),axis.line = element_blank())+
  geom_hline(aes(yintercept=mean(N)), colour="Black", linetype="dashed",cex=1)

#27,assault
dat1<-teen1 %>%
  filter(ShortCode==27)%>% 
  select(Hour,Month,Weekday,ShortCode) %>% 
  group_by(Weekday) %>% 
  summarize(N=n())
dat1$Weekday <- factor(dat$Weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
p<-ggplot(dat1,aes(x=Weekday,y=N))
p+geom_bar(stat = "identity",fill="#2171B5")+xlab("")+ylab("")+
  ggtitle("Assault")+
  theme_classic()+theme(axis.ticks = element_blank(),axis.line = element_blank())+
  geom_hline(aes(yintercept=mean(N)), colour="Black", linetype="dashed",cex=1)
View(dat)
#13(a),drug issues
dat2<-teen1%>%
  filter(ShortCode=="13(a)") %>%
  select(Hour,Month,Weekday,ShortCode) %>% 
  group_by(Weekday) %>% 
  summarize(N=n())
dat2$Weekday <- factor(dat$Weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
p<-ggplot(dat2,aes(x=Weekday,y=N))
p+geom_bar(stat = "identity",fill="#4292C6")+xlab("")+ylab("")+
  ggtitle("Drug Issues")+
  theme_classic()+theme(axis.ticks = element_blank(),axis.line = element_blank())+
  geom_hline(aes(yintercept=mean(N)), colour="Dark Grey", linetype="dashed",cex=1)
###line graph
weekday<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
co<-c(96,111,122,84,80,51,59,83,89,137,103,79,62,29)
newdat<-data.frame(c(rep("Assault",7),rep("Drug Issues",7)),rep(weekday,2),co)
colnames(newdat)<-c("type","day","count")
newdat$day<-factor(dat$Weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
ggplot(newdat, aes(x=factor(day), y=count, colour=type,group=type)) + geom_line(size=1.5)+
  theme_classic()+theme(axis.ticks = element_blank(),axis.line = element_blank())+xlab("")+geom_point(size=4)

#Overall by time
View(dat)
dat<-teen1 %>%
  select(Hour,Month,Weekday,ShortCode) %>% 
  group_by(Hour) %>% 
  summarize(N=n())
p<-ggplot(dat,aes(x=Hour,y=N))
p<-p+geom_bar(stat = "identity",fill="#084594", alpha=.5)+xlab("")+ylab("")+
  ggtitle("")+
  theme_classic()+theme(axis.ticks = element_blank(),axis.line = element_blank())
p+geom_line(data=dat,aes(x=Hour,y=N),colour="pink",size=2)

brewer.pal(3,"Set1")

#barplot of age distribution FILLED by gender or race
#1. pure age distribution
p<-ggplot(data=teen,aes(x=AGE))
p+geom_bar()
#2. filled by gender
p<-ggplot(data=teen,aes(x=AGE))
p+geom_bar(aes(fill=GENDER))
#3.filled by race
p<-ggplot(data=teen,aes(x=AGE))
p+geom_bar(aes(fill=RACE))

#divide age into three groups: 10-13(middle school),14-18(high school),19-21(University Student OR else)
teen1$AGE<-ifelse(teen$AGE<=13,"10-13",ifelse(teen$AGE>18,"19-21","14-18"))
####By month
dat3<-teen1 %>%
  select(Hour,Month,ShortCode) %>%
  group_by(Month) %>%
  summarise(N=n())

View(dat3)
dat3$Month<-factor(dat3$Month,levels=c("July","August","September","October","November","December","January","February","March","April"))
p<-ggplot(dat3,aes(x=Month,y=N))
p+geom_bar(stat = "identity",fill="#C6DBEF")+xlab("")+ylab("")+
  ggtitle("")+
  theme_classic()+theme(axis.ticks = element_blank(),axis.line = element_blank())+
  geom_hline(aes(yintercept=mean(N)), colour="Dark Grey", linetype="dashed",cex=1)


brewer.pal(7,"Blues")

View(dat)
p<-ggplot(teen1,aes(x=Weekday))
p+geom_bar()+geom_density(aes(y=table(Weekday)))


#distribution of teen crime cases (time continuous)
table(teen$Month)
teen$Month<-factor(teen$Month,levels=c('July','August','September','October','November','December','January','February','March','April'))
p<-ggplot(teen,aes(x=Month))
p+geom_bar()+ ylab("Count")+ggtitle("Juvenile Delinquency 2016.07~2017.03")

#creating subsets of crime types
#1. ShortCode=27,assault
tassasult<-teen1[teen1$ShortCode==27,]
tmatrix1<-tassasult[,c("AGE","Hour","Weekday")]
write.csv(tmatrix1,"1.csv")

#Dataframe shows the count by age and hour.
a<-read.csv("1.csv")
b<-attr(a$AGE,"levels")
Count<-NULL
Hour<-NULL
Age<-NULL
for (z in 0:23) {
  for (i in b) {
    temp<-length(which(as.character(a$AGE) == i & a$Hour == z))
    Hour<-c(Hour,z)
    Age<-c(Age,i)
    Count<-c(Count,temp)
    d<-data.frame(Hour,Age,Count)
  }
}
p<-ggplot(data=d,aes(x=Hour,y=Age,fill=Count))+geom_tile(color="grey", size=0.02)
p+theme_classic()+theme(axis.ticks = element_blank(),axis.line = element_blank())+
  scale_fill_gradient2('Number of Assault',
                       low = 'white', high ="#2980B9")+geom_text(aes(label = Count))

#Dataframe shows the count by age and day.
c<-attr(a$Weekday,"levels")
Count<-NULL
Day<-NULL
Age<-NULL
for (z in c) {
  for (i in b) {
    temp<-length(which(as.character(a$AGE) == i & a$Weekday== z))
    Day<-c(Day,z)
    Age<-c(Age,i)
    Count<-c(Count,temp)
    m<-data.frame(Day,Age,Count)
  }
}

levels(m$Day)<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
p<-ggplot(data=m,aes(x=Day,y=Age,fill=Count))+geom_tile(color="grey", size=0.02)
p+theme_classic()+theme(axis.ticks = element_blank(),axis.line = element_blank())+
  scale_fill_gradient2('Assault Cases',low = 'white', high ="#2980B9")+
  geom_text(aes(label = Count))



#2. ShortCode=13(a),drug issues
tdrug<-teen1[teen1$ShortCode=="13(a)",]
j<-tdrug[,c("AGE","Hour","Weekday")]
View(h)
b<-as.character(unique(j$AGE))
Count<-NULL
Hour<-NULL
Age<-NULL
for (z in 0:23) {
  for (i in b) {
    temp<-length(which(as.character(j$AGE) == i & j$Hour == z))
    Hour<-c(Hour,z)
    Age<-c(Age,i)
    Count<-c(Count,temp)
    h<-data.frame(Hour,Age,Count)
  }
}
p<-ggplot(data=h,aes(x=Hour,y=Age,fill=Count))+geom_tile(color="grey", size=0.02)
p+theme_classic()+theme(axis.ticks = element_blank(),axis.line = element_blank())+
  scale_fill_gradient2('Drug Issues',
                       low = 'white', high ="#2980B9")+geom_text(aes(label = Count))

#2
c<-attr(j$Weekday,"levels")
Count<-NULL
Day<-NULL
Age<-NULL
for (z in c) {
  for (i in b) {
    temp<-length(which(as.character(j$AGE) == i & j$Weekday== z))
    Day<-c(Day,z)
    Age<-c(Age,i)
    Count<-c(Count,temp)
    u<-data.frame(Day,Age,Count)
  }
}
levels(u$Day)<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
p<-ggplot(data=u,aes(x=Day,y=Age,fill=Count))+geom_tile(color="grey", size=0.02)
p+theme_classic()+theme(axis.ticks = element_blank(),axis.line = element_blank())+
  scale_fill_gradient2('Drug Issues',low = 'white', high ="#2980B9")+
  geom_text(aes(label = Count))

