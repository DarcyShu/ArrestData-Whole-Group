
four<-read.csv("Four.csv")
library(ggplot2)
library(RColorBrewer)

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

plotbyage<-function(x){
  a<-ggplot(x,aes(AGE,fill=factor(RACE)))
  a+geom_bar(stat="count")+
    facet_grid(.~SHORTCODE)+ylab("Number of Crime")+
    xlab("Age")+ggtitle("Distribution of Number of Incidents Over Race and Age")+
    guides(fill=guide_legend(title="Race",reverse = F))+
    theme(panel.background = element_rect(colour="Black")) +scale_fill_brewer(palette = 'Set1')}
plotbyage(four)

plotbygender<-function(x){
  a<-ggplot(x,aes(AGE,fill=factor(GENDER)))
  a+geom_bar(stat="count")+
    facet_grid(.~SHORTCODE)+ylab("Number of Crime")+
    xlab("Age")+ggtitle("Distribution of Number of Incidents Over Age and Gender")+
    guides(fill=guide_legend(title="Gender",reverse = T))+
    theme(panel.background = element_rect(colour="Black")) +scale_fill_brewer(palette = 'Set1')}