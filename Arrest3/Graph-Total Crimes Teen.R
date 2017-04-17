rm(list=ls())
setwd("/Users/apple/Desktop/GSPIA/GSPIA/study/2017 Spring/R/Class Materials")
ArrestTeen=read.csv("ArrestTeenForYilin.csv")


###test

str(ArrestTeen)
summary(ArrestTeen)
ArrestTeen[1:6,]
####For Teen:
Teen<-which(ArrestTeen$Teen==1)
Teendt<-ArrestTeen[Teen,]
count <- table(Teendt$ShortCode)
subdat <- data.frame(shortCode=names(count),value=as.vector(count))
subdat[1:6,]
subdatTeen<-subdat[which(subdat$value>0),]

###For Adult:
Adult<-which(ArrestTeen$Teen==0)
Adultdt<-ArrestTeen[Adult,]
count2 <- table(Adultdt$ShortCode)
subdat2 <- data.frame(shortCode=names(count2),value=as.vector(count2))
subdat2[1:6,]
subdatAdult<-subdat2[which(subdat2$value>0),]

###Graph For Teenager(1):
library(ggplot2)
Teen<-ggplot(data=subdatTeen, aes(x=reorder(shortCode, value), y=value)) 
Teen+geom_bar(stat="identity",fill='blue')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Short Offense Code")+
  scale_fill_manual(values=Blue)+
  geom_text(aes(label=value), check_overlap = TRUE,vjust=2, colour="black")+
  ggtitle("Distribution of All Crime Types Among Teenagers")+
  coord_flip()


###Graph For Adult(2):
Adult<-ggplot(data=subdatAdult, aes(x=reorder(shortCode, value), y=value)) 
Adult+geom_bar(stat="identity",fill='steelblue')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Short Offense Code")+
  scale_fill_manual(values=Blue)+
  geom_text(aes(label=value), check_overlap = TRUE,vjust=2, colour="black")+
  ggtitle("Distribution of All Crime Types Among Adults")+
  coord_flip()

####Graph Comparison For Top 4 (3):
idx<-which(ArrestTeen$ShortCode==27|ArrestTeen$ShortCode==9|ArrestTeen$ShortCode=="13(a)"|ArrestTeen$ShortCode==39)
TopFour<-ArrestTeen[idx,]
TopFour$Teen[which(TopFour$Teen=="1")]<-"Teen"
TopFour$Teen[which(TopFour$Teen=="0")]<-"Adult"

library(scales)
Four<-ggplot(TopFour,aes(x=ShortCode,fill=Teen))
Four+geom_bar(aes(y = (..count..)/sum(..count..)),position="dodge") + 
  scale_fill_manual(values = c("darkblue", "#6698FF"))+
  ggtitle("Top 4 Crimes--Adult vs. Teen")+
  ylab("Percentage %")+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", position = position_dodge(0.9),vjust =-0.5)


