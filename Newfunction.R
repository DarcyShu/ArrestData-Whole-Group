library(dplyr)
library(tidyr)
dat<-ArrestData
datwhole<-dat%>%mutate(OFFENSES=strsplit(as.character(OFFENSES),"/"))%>%
  unnest(OFFENSES)

CleanFun<-function(frame,x){
  matches<-regexpr("[0~9]+",x)
  idx<-attr(matches,"match.length")
  idx<-idx<=2&idx>0
  x<-x[idx]#11688 offenses
  framn<-frame[idx,]#11688 offenses
  matches<-gregexpr("[0-9]+",x)
  FullCode<-regmatches(x,matches)
  FullCode<-sapply(FullCode,"[[",1)
  framn<-cbind(framn,FullCode)
  FullCode[which(nchar(FullCode) == 2)]<-"13(a)"
  FullCode[which(nchar(FullCode) == 4)]<-substr(FullCode[which(nchar(FullCode) == 4)],1,2)
  FullCode[which(nchar(FullCode) == 3)]<-substr( FullCode[which(nchar(FullCode) == 3)],1,1)
  ShortCode<-FullCode
  framn<-cbind(framn,ShortCode)
  return(framn)
}

dat<-CleanFun(datwhole,datwhole$OFFENSES)
sort(table(datwhole$ShortCode),decreasing = T)