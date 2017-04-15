rm(list=ls())
dat=read.csv("ArrestData.csv")

#Data Cleaning
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

#Using chapter number to category crime types:
idx5<-which(arrest$code==13)
arrest$code[idx5]<-"13(a)"
arrest$shortcode<-arrest$code   

idx6<-which(nchar(arrest$shortcode)==3)
idx7<-which(nchar(arrest$shortcode)==4)

arrest$shortcode[idx6]<-substr(arrest$shortcode[idx6],1,1) 
arrest$shortcode[idx7]<-substr(arrest$shortcode[idx7],1,2) 
arrest<-na.omit(arrest)
table(arrest$arrest.AGE)

#Take out outliers:
idx8<-which(arrest$arrest.AGE==0|arrest$arrest.AGE==999)
arrest<-arrest[-idx8,]

#Extract top four crimes:
idx9<-which(arrest$shortcode=="27"|arrest$shortcode=="13(a)"|arrest$shortcode=="9"|arrest$shortcode=="39")
ArrestTopFour<-arrest[idx9,]
sort(table(ArrestTopFour$shortcode))

library(lubridate)
table(year(ArrestTopFour$arrest.ARRESTTIME))

#Take out year 2014 and 2015
idx10<-which(year(ArrestTopFour$arrest.ARRESTTIME)==2014|year(ArrestTopFour$arrest.ARRESTTIME)==2015)
ArrestTopFour<-ArrestTopFour[-idx10,]
sort(table(ArrestTopFour$shortcode))

#Create data frame of Arrest top four:
ArrestTopFour<-data.frame(ArrestTopFour$arrest.AGE,ArrestTopFour$arrest.GENDER,ArrestTopFour$arrest.RACE,ArrestTopFour$arrest.OFFENSES,ArrestTopFour$code,ArrestTopFour$shortcode)

colnames(ArrestTopFour)<-c("AGE","GENDER","RACE","OFFENSE","CODE","SHORTCODE")

##########Graph
four<-ArrestTopFour
library(ggplot2)
library(RColorBrewer)

#Clean age group
idx1<-which(four$AGE<=18)
idx2<-which(four$AGE>18 & four$AGE<=25)
idx3<-which(four$AGE<=34&four$AGE>25)
idx4<-which(four$AGE<=44&four$AGE>34)
idx5<-which(four$AGE<=54&four$AGE>44)
idx6<-which(four$AGE>54)
four$AGE[idx1]<-"Under18"
four$AGE[idx2]<-"18to24"
four$AGE[idx3]<-"25to34"
four$AGE[idx4]<-"35to44"
four$AGE[idx5]<-"44to54"
four$AGE[idx6]<-"above55"
four$AGE<-factor(four$AGE, levels=c('Under18','18to24','25to34','35to44','44to54','above55'))

#Create function to plot distribution of crimes over age and race
plotbyage<-function(x){
  a<-ggplot(x,aes(AGE,fill=factor(RACE)))
  a+geom_bar(stat="count")+
    facet_grid(.~SHORTCODE)+ylab("Number of Crime")+
    xlab("Age")+ggtitle("Distribution of Number of Incidents Over Race and Age")+
    guides(fill=guide_legend(title="Race",reverse = F))+
    theme(panel.background = element_rect(colour="Black")) +scale_fill_brewer(palette = 'Set1')}
plotbyage(four)

#Create function to plot distribution of crimes over age and gender
plotbygender<-function(x){
  a<-ggplot(x,aes(AGE,fill=factor(GENDER)))
  a+geom_bar(stat="count")+
    facet_grid(.~SHORTCODE)+ylab("Number of Crime")+
    xlab("Age")+ggtitle("Distribution of Number of Incidents Over Age and Gender")+
    guides(fill=guide_legend(title="Gender",reverse = T))+
    theme(panel.background = element_rect(colour="Black")) +scale_fill_brewer(palette = 'Set1')}
plotbygender(four)

##
ggplot(data=datgraph,aes(x=AGE,fill=RACE)) + 
  geom_bar(data=subset(datgraph,RACE=="B")) + 
  geom_bar(data=subset(datgraph,RACE=="W"),aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-200,200,40),labels=abs(seq(-200,200,40))) + 
  coord_flip()