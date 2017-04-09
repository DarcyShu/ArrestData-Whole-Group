datwhole<-dat%>%mutate(OFFENSES=strsplit(as.character(OFFENSES),"/"))%>%
  unnest(OFFENSE)
matches<-regexpr("[0~9]+",datwhole$OFFENSES)
idx<-attr(matches,"match.length")
idx<-idx<=2&idx>0
datwhole<-datwhole[idx,]#11688 offenses
matches<-gregexpr("[0-9]+",datwhole$OFFENSES)
ShortCode<-regmatches(datwhole$OFFENSES,matches)
datwhole$ShortCode<-sapply(ShortCode,"[[",1)
datwhole$ShortCode[which(nchar(datwhole$ShortCode) == 2)]<-"13(a)"
datwhole$ShortCode[which(nchar(datwhole$ShortCode) == 4)]<-substr( datwhole$ShortCode[which(nchar(datwhole$ShortCode) == 4)],1,2)
datwhole$ShortCode[which(nchar( datwhole$ShortCode) == 3)]<-substr( datwhole$ShortCode[which(nchar(datwhole$ShortCode) == 3)],1,1)

sort(table(datwhole$ShortCode),decreasing = T)