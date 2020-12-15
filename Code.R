dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) 


library("tidyverse")
library("imputeTS")
library("RCurl")
library("jsonlite")
setwd("C:\Users\lalit\Desktop\ISTX87\Homework\New folder")
getwd()
#Loading the dataset from URL:
dataset <- getURL("https://s3.us-east-1.amazonaws.com/blackboard.learn.xythos.prod/5956621d575cd/8606160?response-content-disposition=inline%3B%20filename%2A%3DUTF-8%27%27fall2019-survey-M09.json&response-content-type=application%2Fjson&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20191202T025918Z&X-Amz-SignedHeaders=host&X-Amz-Expires=21600&X-Amz-Credential=AKIAIL7WQYDOOHAZJGWQ%2F20191202%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=533445b1b87d80d643b29e0292c23bbdc6ebc93777009d6693d4a5590a76e7c8")

SouthEast2 <- jsonlite::fromJSON(dataset)


view(SouthEast2)


#Coercing the numeric attributes into numbers:
SouthEast2$Departure.Delay.in.Minutes <- as.numeric(SouthEast2$Departure.Delay.in.Minutes)
SouthEast2$Arrival.Delay.in.Minutes <- as.numeric(SouthEast2$Arrival.Delay.in.Minutes)
SouthEast2$Flight.time.in.minutes <- as.numeric(SouthEast2$Flight.time.in.minutes)
SouthEast2$Likelihood.to.recommend <- as.numeric(SouthEast2$Likelihood.to.recommend)

#Replacing NAs with average of values in two adjecent cells:
SouthEast <- na_interpolation(SouthEast2)

#Creating a separate dataframe with relevant information to calculate NPS:
NPS <- SouthEast[ ,c(2,3,14,17,22,23,24,25,26,27)]
NPS$totalDelay <- NPS$Departure.Delay.in.Minutes+NPS$Arrival.Delay.in.Minutes

#NPS for airline partner
#Total respondents for each airline partner
Partner <- NPS%>%
  group_by(Partner.Code) %>%
  summarise(count=n())

#Promoters for airline partners:
AAPromoters <- NPS%>%
  filter(Partner.Code=="AA")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
ASPromoters <- NPS%>%
  filter(Partner.Code=="AS")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
B6Promoters <- NPS%>%
  filter(Partner.Code=="B6")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
EVPromoters <- NPS%>%
  filter(Partner.Code=="EV")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
DLPromoters <- NPS%>%
  filter(Partner.Code=="DL")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
F9Promoters <- NPS%>%
  filter(Partner.Code=="F9")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
FLPromoters <- NPS%>%
  filter(Partner.Code=="FL")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
HAPromoters <- NPS%>%
  filter(Partner.Code=="HA")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
MQPromoters <- NPS%>%
  filter(Partner.Code=="MQ")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
OOPromoters <- NPS%>%
  filter(Partner.Code=="OO")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
OUPromoters <- NPS%>%
  filter(Partner.Code=="OU")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
USPromoters <- NPS%>%
  filter(Partner.Code=="US")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
VXPromoters <- NPS%>%
  filter(Partner.Code=="VX")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
WNPromoters <- NPS%>%
  filter(Partner.Code=="WN")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())

#List of airline promoters:
PartnerPromoterList <- c(AAPromoters, ASPromoters, B6Promoters, DLPromoters, EVPromoters, F9Promoters, FLPromoters, HAPromoters, MQPromoters, OOPromoters, OUPromoters, USPromoters, VXPromoters, WNPromoters)
Partner$Promoters <- PartnerPromoterList

#Detractors for airline partners:
AADetractors <- NPS%>%
  filter(Partner.Code=="AA")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
ASDetractors <- NPS%>%
  filter(Partner.Code=="AS")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
B6Detractors <- NPS%>%
  filter(Partner.Code=="B6")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
DLDetractors <- NPS%>%
  filter(Partner.Code=="DL")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
EVDetractors <- NPS%>%
  filter(Partner.Code=="EV")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
FLDetractors <- NPS%>%
  filter(Partner.Code=="FL")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
F9Detractors <- NPS%>%
  filter(Partner.Code=="FL")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
HADetractors <- NPS%>%
  filter(Partner.Code=="HA")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
MQDetractors <- NPS%>%
  filter(Partner.Code=="MQ")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
OODetractors <- NPS%>%
  filter(Partner.Code=="OO")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
OUDetractors <- NPS%>%
  filter(Partner.Code=="OU")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
USDetractors <- NPS%>%
  filter(Partner.Code=="US")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
VXDetractors <- NPS%>%
  filter(Partner.Code=="VX")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
WNDetractors <- NPS%>%
  filter(Partner.Code=="WN")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())

#List of airline detractors:
PartnerDetractorList <- c(AADetractors, ASDetractors, B6Detractors, DLDetractors, EVDetractors, F9Detractors, FLDetractors, HADetractors, MQDetractors, OODetractors, OUDetractors, USDetractors, VXDetractors, WNDetractors)
Partner$Detractors <- PartnerDetractorList

#Calculation of NPS for each airline partner:
Partner$count <- as.numeric(Partner$count)
Partner$Promoters <- as.numeric(Partner$Promoters)
Partner$Detractors <- as.numeric(Partner$Detractors)
Partner$NPS <- ((Partner$Promoters/Partner$count*100)-(Partner$Detractors/Partner$count*100))

##################################
#NPS for airline status:
Status <- NPS%>%
  group_by(Airline.Status) %>%
  summarise(count=n())

#Promoters: airline status
BluePromoters <- NPS%>%
  filter(Airline.Status=="Blue")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
GoldPromoters <- NPS%>%
  filter(Airline.Status=="Gold")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
PlatinumPromoters <- NPS%>%
  filter(Airline.Status=="Platinum")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
SilverPromoters <- NPS%>%
  filter(Airline.Status=="Silver")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())

#List of Status promoters:
StatusPromoterList <- c(BluePromoters, GoldPromoters, PlatinumPromoters, SilverPromoters)
Status$Promoters <- StatusPromoterList

#Detractors for airline status:
BlueDetractors <- NPS%>%
  filter(Airline.Status=="Blue")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
GoldDetractors <- NPS%>%
  filter(Airline.Status=="Gold")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
PlatinumDetractors <- NPS%>%
  filter(Airline.Status=="Platinum")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
SilverDetractors <- NPS%>%
  filter(Airline.Status=="Silver")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())

#List of Status detractors:
StatusDetractorsList <- c(BlueDetractors, GoldDetractors, PlatinumDetractors, SilverDetractors)
Status$Detractors <- StatusDetractorsList

#Calculation of NPS for each airline status:
Status$count <- as.numeric(Status$count)
Status$Promoters <- as.numeric(Status$Promoters)
Status$Detractors <- as.numeric(Status$Detractors)
Status$NPS <- ((Status$Promoters/Status$count*100)-(Status$Detractors/Status$count*100))

######################################
#NPS for class:
Class <- NPS%>%
  group_by(Class) %>%
  summarise(count=n())

#Promoters: class
BusinessPromoters <- NPS%>%
  filter(Class=="Business")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
EcoPromoters <- NPS%>%
  filter(Class=="Eco")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
EcoPlusPromoters <- NPS%>%
  filter(Class=="Eco Plus")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())

#List of Class promoters:
ClassPromoterList <- c(BusinessPromoters, EcoPromoters, EcoPlusPromoters)
Class$Promoters <- ClassPromoterList

#Detractors for class:
BusinessDetractors <- NPS%>%
  filter(Class=="Business")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
EcoDetractors <- NPS%>%
  filter(Class=="Eco")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
EcoPlusDetractors <- NPS%>%
  filter(Class=="Eco Plus")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())

#List of Class detractors:
ClassDetractorsList <- c(BusinessDetractors, EcoDetractors, EcoPlusDetractors)
Class$Detractors <- ClassDetractorsList

#Calculation of NPS for each airline class:
Class$count <- as.numeric(Class$count)
Class$Promoters <- as.numeric(Class$Promoters)
Class$Detractors <- as.numeric(Class$Detractors)
Class$NPS <- ((Class$Promoters/Class$count*100)-(Class$Detractors/Class$count*100))

###################################
#NPS for flight cancellation
Cancellation <- NPS%>%
  group_by(Flight.cancelled) %>%
  summarise(count=n())

#Promoters: flight cancellation
CanceledPromoters <- NPS%>%
  filter(Flight.cancelled=="Yes")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())
NotCanceledPromoters <- NPS%>%
  filter(Flight.cancelled=="No")%>%
  filter(Likelihood.to.recommend>8)%>%
  summarise(count=n())

#Detractors: flight cancellation
CanceledDetractors <- NPS%>%
  filter(Flight.cancelled=="Yes")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())
NotCanceledDetractors <- NPS%>%
  filter(Flight.cancelled=="No")%>%
  filter(Likelihood.to.recommend<7)%>%
  summarise(count=n())

#List of Canceled promoters:
CanceledPromotersList <- c(NotCanceledPromoters, CanceledPromoters)
Cancellation$Promoters <- CanceledPromotersList

#List of Canceled detractors:
CanceledDetractorsList <- c(NotCanceledDetractors, CanceledDetractors)
Cancellation$Detractors <- CanceledDetractorsList

#Calculation of NPS for each flight cancellation:
Cancellation$count <- as.numeric(Cancellation$count)
Cancellation$Promoters <- as.numeric(Cancellation$Promoters)
Cancellation$Detractors <- as.numeric(Cancellation$Detractors)
Cancellation$NPS <- ((Cancellation$Promoters/Cancellation$count*100)-(Cancellation$Detractors/Cancellation$count*100))










