datwhole<-dat%>%mutate(OFFENSES=strsplit(as.character(OFFENSES),"/"))%>%
  unnest(OFFENSE