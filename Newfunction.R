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
  framn<-cbind(framn,FullCode,stringsAsFactors=F)
  FullCode[which(nchar(FullCode) == 2)]<-"13(a)"
  FullCode[which(nchar(FullCode) == 4)]<-substr(FullCode[which(nchar(FullCode) == 4)],1,2)
  FullCode[which(nchar(FullCode) == 3)]<-substr( FullCode[which(nchar(FullCode) == 3)],1,1)
  ShortCode<-FullCode
  framn<-cbind(framn,ShortCode,stringsAsFactors=F)
  return(framn)
}

dat<-CleanFun(datwhole,datwhole$OFFENSES)
sort(table(dat$ShortCode),decreasing = T)
#27    55    39     9    38    35    95 13(a)    61    33 
#2666  1119  1114   724   699   643   629   566   521   441 
#49    51    37    90    43    59    41    15     6    47 
#430   376   312   261   238   175   148   132   128    82 
#13    29    63    25     4     7    91    26     1    19 
#81    74    63    22    14     6     5     4     3     2 
#31    65    94    45    57    73    75 
#2     2     2     1     1     1     1 

#To decode we look the information on website http://www.legis.state.pa.us/wu01/li/li/ct/htm/18/18.htm
#, which give us the information about Title 18 in Consolidated Statutes. 
names(table(dat$OFFENSES[dat$ShortCode=="27"]))
#27 represents chapter 27 of Title 18 with the title "Assult"
names(table(dat$OFFENSES[dat$ShortCode=="55"]))
#"55" represents chapter 55 of Title 18 with the title "Riot, Disorderly Conduct and Related Offenses"
names(table(dat$OFFENSES[dat$ShortCode=="39"]))
#39 represents chapter 39 of Title 18 with the title "Theft and Related Offenses"
names(table(dat$OFFENSES[dat$ShortCode=="9"]))
#"9" represents chapter 9 of Title 18 with the tile "Inchoate Crimes"
names(table(dat$OFFENSES[dat$ShortCode=="38"]))
#"38" represents chapter 38 of Title 75 Driving After Imbibing Alcohol or Ultilizing Drugs
names(table(dat$OFFENSES[dat$ShortCode=="35"]))
#"35" represents chapter 35 of Title 18 with the title "Burglary and Other Criminal Intrusion"
names(table(dat$OFFENSES[dat$ShortCode=="95"]))
#"95" represents Bench Warrant
names(table(dat$OFFENSES[dat$ShortCode=="13(a)"]))
#13(a) represents "Controlled Substance, Drug, Device and Cosmetic Act"
names(table(dat$OFFENSES[dat$ShortCode=="61"]))
#"61" represents chapter 61 of Title 18 with the title "Firearms and Other Dangerous Articles"
names(table(dat$OFFENSES[dat$ShortCode=="33"]))
#messed situation for the short code "33"
#" 3301 Driving on Right Side of Roadway. "  
#" 3306 Limitations on Driving on Left Side of Roadway. " 
# " 3308 One-Way Roadways and Rotary Traffic Islands. " 
#" 3309 Driving on Roadways Laned for Traffic. " 
#" 3310 Following Too Closely. " 
#"3301 Driving on Right Side of Roadway. "  
#"3308 One-Way Roadways and Rotary Traffic Islands. "
#"3309 Driving on Roadways Laned for Traffic. " 
#"3310 Following Too Closely. "
#The Statutes above are included in Title 75 Vehicles chapter 33 Rules of the Road in General
#Others are included in Title 18 chapter 33 Arson, Criminal Mischief and Other Property Destruction
names(table(dat$OFFENSES[dat$ShortCode=="49"]))
#"49" represents chapter 49 of Title 18 with the title "Falsification and Intimidation"
names(table(dat$OFFENSES[dat$ShortCode=="51"]))
#"37" represents chapter 51 of Title 18 with the title "Obstructing Governmental Operations"
names(table(dat$OFFENSES[dat$ShortCode=="37"]))
#" 3701 Robbery." 
#" 3701 Robbery. "
#" 3702 Robbery of Motor Vehicle."    
#" 3702 Robbery of Motor Vehicle. "
# "3701 Robbery." 
#"3701 Robbery. " 
#"3702 Robbery of Motor Vehicle."
#All above represents chapter 37 of Title 18 with the title "Robbery"
#" 3701 Unattended Motor Vehicle. "   
#" 3702 Limitations on Backing. "  
#" 3703 Driving Upon Sidewalk. " 
#" 3709 Dep. Waste and Other Material"
#All above represents chapter 37 of Title 75 with the title "Miscellaneous Provisions"
names(table(dat$OFFENSES[dat$ShortCode=="90"]))
#"90" represents Faliure to Appear
names(table(dat$OFFENSES[dat$ShortCode=="43"]))
#" 4302 Incest."                                                           
#" 4302 Incest. "     
# " 4304(a)(1) Endangering Welfare of Children."                            
#" 4304(a)(1) Endangering Welfare of Children. "                           
#" 4304(a)(1) Enhanced Endangering Welfare of Children - Course of Conduct"
#"4302 Incest. "      
#"4304(a)(1) Endangering Welfare of Children."
#"4304(a)(1) Endangering Welfare of Children. "
#All above represents chapter 43 of Title 18 with the title "Offenses Against the Family"
#" 4302 Periods for Requiring Lighted Lamps."                              
#" 4302 Periods for Requiring Lighted Lamps. "                             
#" 4303 General Lighting Requirements."                                    
#" 4303 General Lighting Requirements. "  
#" 4306 Use of Multiple-Beam Road Lighting Equipment. "                    
#"4302 Incest. "                                                           
#"4302 Periods for Requiring Lighted Lamps. "                              
#"4303 General Lighting Requirements. "   
#All above represents chapter 43 of Title 75 with the title "Lighting Equipment"
names(table(dat$OFFENSES[dat$ShortCode=="59"]))
#"59" represents chapter 59 of Title 18 with the title "Public Indecency"
names(table(dat$OFFENSES[dat$ShortCode=="41"]))
#" 4101 Forgery."                                  
#" 4101 Forgery. "                                 
#" 4104 Tampering with Records or Identification. "
#" 4105(a)(1) Bad Checks"                          
#" 4105(a)(1) Bad Checks "                         
#" 4106 Access Device Fraud"                       
#" 4106 Access Device Fraud "                      
#" 4107 Deceptive Business Practices."             
#" 4107 Deceptive Business Practices. "     
#" 4119 Trademark Counterfeiting"                  
#" 4120 Identity Theft"                            
#" 4120 Identity Theft "                           
#"4101 Forgery."                                   
#"4101 Forgery. "                                  
#"4104 Tampering with Records or Identification. " 
#"4106 Access Device Fraud"                        
#"4106 Access Device Fraud "                       
#"4107 Deceptive Business Practices."              
#"4120 Identity Theft "
#Above belongs to chapter 41 of title 18 named Forgery and Fraudulent Practices
#" 4107(b)(2) Vehicle Equipment Standards"         
#" 4107(b)(2) Vehicle Equipment Standards "
#Above belongs to chapter 41 of title 75 named  Equipment Standards
names(table(dat$OFFENSES[dat$ShortCode=="15"]))
#Above belongs to chapter 41 of title 75 named  Licensing of Drivers
names(table(dat$OFFENSES[dat$ShortCode=="6"]))
#"6" represents Regulated Rights and Actions from Home Rule Chapter of City of Pittsburgh
names(table(dat$OFFENSES[dat$ShortCode=="47"]))
#" 4702 Requirement for Periodic Inspection of Vehicles "            
#" 4703 Operation of Vehicle without Official Certif. of Inspection "
#" 4706 Prohib. on Expend. for Emission Insp Prog.."                 
#" 4706 Prohib. on Expend. for Emission Insp Prog.. "                
#" 4730 Violations of Use of Certificate of Inspection. "   
#"4703 Operation of Vehicle without Official Certif. of Inspection " 
#"4703 Operation of Vehicle without Official Certif. of Inspection "
#Above belongs to chapter 47 of title 75 named Inspection of Vehicles
#" 4701 Bribery in Official and Political Matters. "
#"4701 Bribery in Official and Political Matters. " 
#Above belongs to chapter 47 of title 18 named Bribery and Corrupt Influence
names(table(dat$OFFENSES[dat$ShortCode=="13"]))
#"13" represents chapter 13 of title 75 named Registration of Vehcles
names(table(dat$OFFENSES[dat$ShortCode=="29"]))
#"29" represents chapter 29 of title 75 named  Kidnapping
names(table(dat$OFFENSES[dat$ShortCode=="63"]))
#"29" represents chapter 63 of title 75 named  Minors
names(table(dat$OFFENSES[dat$ShortCode=="25"]))
#"25" represents chapter 63 of title 75 named  Cirminal Homocide
names(table(dat$OFFENSES[dat$ShortCode=="4"]))
#" 493(1) Furnishing Liquor"
#"493(1) Furnishing Liquor" 
#Above belong to Pennsylvania Statutes Title 47 P.S. Liquor
# " 419.01 Obstruction of Street, Sidewalk, or Public Way "
#" 473.07 (a) Hours of Parks 0600-2300 hrs."              
#"419.01 Obstruction of Street, Sidewalk, or Public Way "
#Above belong to Home Rule of City of Pittsburgh, Title Four UBLIC PLACES AND PROPERTY
names(table(dat$OFFENSES[dat$ShortCode=="7"]))
#" 761.01 License required from Police"
#From TITLE SEVEN: - BUSINESS LICENSING of Home Rule of City of Pittsburgh
#" 780-113(a39) Prohibited Acts - Possession Of Substances For Methamphetamine Manufacture"
#Pennsylvania Statutes Title 35 P.S. Health and Safety
#"701.14 License Revocation or Suspension " 
##From TITLE SEVEN: - BUSINESS LICENSING of Home Rule of City of Pittsburgh
names(table(dat$OFFENSES[dat$ShortCode=="91"]))
#"9136 Commitment to Await Requisition"
#comes from chapter 91 of title detainers and extradition
#other two are just typoes
names(table(dat$OFFENSES[dat$ShortCode=="26"]))
#"26" represents Chapter 26 - Crimes Against Unborn Child under title 18
names(table(dat$OFFENSES[dat$ShortCode=="1"]))
#This is a typo.
names(table(dat$OFFENSES[dat$ShortCode=="19"]))
#"19" represents Chapter 19 -Fees  under title 75
names(table(dat$OFFENSES[dat$ShortCode=="31"]))
#"31" represents Chapter 31 -General Provisions  under title 75
names(table(dat$OFFENSES[dat$ShortCode=="65"]))
#6501(a)(1) Scattering Rubbish; Any waste, dangerous or detrimental substance upon public property or waters"
#from chapter 65 under title 18 named NUISANCES
#" 6503.1 Habitual Offenders"
#from chapter 65 under title 75 named PENALTIES AND DISPOSITION OF FINES
names(table(dat$OFFENSES[dat$ShortCode=="94"]))
#" 9497 Aided Case"
#" 9498 302 (Mental) "
names(table(dat$OFFENSES[dat$ShortCode=="45"]))
#"45" represents Other Required Equipment under title 75
names(table(dat$OFFENSES[dat$ShortCode=="57"]))
#"57" represents Wiretapping and Electronic Surveillance under title 18
names(table(dat$OFFENSES[dat$ShortCode=="73"]))
#"73" represents chapter 73  Trade and Commerce  under title 18
names(table(dat$OFFENSES[dat$ShortCode=="75"]))
#"75" represents chapter 75 Other Offenses  under title 18