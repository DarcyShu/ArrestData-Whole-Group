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











