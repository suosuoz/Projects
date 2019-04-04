library(readr)
library(dplyr)
library(corrplot)
library(car)
library(ggplot2)
library(MASS)
library(alr3)
setwd("C:/Users/Matt Matsuo/Google Drive/Fifa")
setwd("C:/Users/mattm/Google Drive/Fifa")
train <- read.csv("FifatrainNew.csv",header=TRUE,sep=",")
test <- read_csv("FifaTestNoYValues.csv")



xd <- as.data.frame(log(train$WageNew))
#Check density of our Y var - WageNew
ggplot(data = xd, aes(x=log(train$WageNew))) +
  geom_histogram(aes(y = ..density..), binwidth = .5) +
  geom_density(alpha = .2, fill = "antiquewhite3")


#Account for NA values
#train$Position[is.na(train$Position)]= "LB"
train$International.Reputation[is.na(train$International.Reputation)]=median(train$International.Reputation,na.rm=T)
train$Potential[is.na(train$Potential)]=median(train$Potential,na.rm=T)
train$Skill.Moves[is.na(train$Skill.Moves)]=median(train$Skill.Moves,na.rm=T)



#Add new columns to train by position
train <- train %>% 
  mutate(GKtotal = GKDiving + GKHandling + GKKicking + GKPositioning + GKReflexes,
         MFp = as.numeric((Position=="CDM")+(Position=="CM")+(Position=="LAM")+(Position=="LCM")
                          +(Position=="LDM")+(Position=="LM")+(Position=="LWB")+(Position=="RAM")+
                            (Position=="RCM")+(Position=="RDM")+(Position=="RM")+(Position=="RWB")),
         FWp = as.numeric((Position=="CAM")+(Position=="CF")+(Position=="LF")+(Position=="RF"))+as.numeric((Position=="ST")
                                                                                                           +(Position=="LS")+(Position=="RS")+(Position=="RW")+(Position=="LW")),
         BKp = as.numeric((Position=="LB")+(Position=="CB")+(Position=="RB")+(Position=="LCB")+(Position=="RCB")),
         GKp = as.numeric(Position=="GK")
  )


#Add new variable realface = true
train <- train %>% 
  mutate(realfaceyes = as.numeric(Real.Face == "Yes"))
train <- train %>% 
  mutate(
    Small = as.numeric((Club=="Manchester United")+(Club=="Arsenal")+(Club=="Real Madrid")+(Club=="Juventus")+(Club=="Chelsea")+(Club=="Tottenham Hotspur")+(Club=="Manchester City")+(Club=="Atl<e9>tico Madrid")+(Club=="Inter")+(Club=="Milan")+(Club=="Valencia CF")+(Club=="FC Bayern M<fc>nchen")+(Club=="Liverpool")+(Club=="Olympique Lyonnais")+(Club=="Paris Saint-Germain")+(Club=="Guangzhou Evergrande Taobao FC")+(Club=="FC Barcelona")+(Club=="Rayo Vallecano")+(Club=="Everton")+(Club=="West Ham United")+(Club=="Napoli")+(Club=="Borussia Dortmund")+(Club=="AS Monaco")+(Club=="Roma")
    ),
  aa = as.numeric((Nationality=="Afghanistan")),ab=as.numeric(Nationality=="Belize"),ac=as.numeric(Nationality=="Bolivia"),ad=as.numeric(Nationality=="Botswana"),ae=as.numeric(Nationality=="Bulgaria"),af=as.numeric(Nationality=="Colombia"),ag=as.numeric(Nationality=="Greece"),ah=as.numeric(Nationality=="Guatemala"),ai=as.numeric(Nationality=="Hong Kong"),aj=as.numeric(Nationality=="India"),ak=as.numeric(Nationality=="Indonesia"),al=as.numeric(Nationality=="Jordan"),am=as.numeric(Nationality=="Kenya"),an=as.numeric(Nationality=="New Zealand"),ao=as.numeric(Nationality=="Norway"),ap=as.numeric(Nationality=="Qatar"),aq=as.numeric(Nationality=="Russia"),ar=as.numeric(Nationality=="South Africa"),ass=as.numeric(Nationality=="Sweden"),att=as.numeric(Nationality=="Ukraine"),  
    low = as.numeric(Overall <= 55)
    
    )
train$Small[is.na(train$Small)]=0
train$nation[is.na(train$nation)]=0
train$realfaceyes[is.na(train$realfaceyes)]=0
train$Overall[is.na(train$Overall)]=median(train$Overall)

# zz <- train %>% 
#   group_by(Nationality) %>% 
#   summarize(median(WageNew))
# 
# which(zz$`median(WageNew)` < 50000)
# zz$Nationality[17]
# zz$Nationality[42]
# zz$Nationality[87]
# zz$Nationality[118]
# zz$Nationality[126]
# zz$Nationality[145]
# zz$Nationality[151]





myvars = c("Overall","International.Reputation","Skill.Moves","Potential","realfaceyes","Small","nation","low","aa","ab","ac","ad","ae","af","ag","ah","ai","aj","ak","al","am","an","ao","ap","aq","ar","ass","att") 
train2 = train[myvars]
tWage <- log(train$WageNew)
WageNewModel <- lm(data=train2, tWage~.)
summary(WageNewModel)
par(mfrow=c(2,2))
plot(WageNewModel)
rmse <- function(error){  sqrt(mean(error^2))}
y_hat <- fitted.values(WageNewModel)
error <- train$WageNew - y_hat
rmse(error)







#Setup Model This Model has R2 of 64 MSE of 27000
myvars = c("Overall","International.Reputation","Potential","Skill.Moves","realfaceyes","Small") 
train2 = train[myvars]

WageNewModel <- lm(data=train2, train$WageNew~.)
summary(WageNewModel)

inverseResponsePlot(WageNewModel)
tWage <- log(train$WageNew)
WageNewModel <- lm(data=train2, tWage~.)
summary(WageNewModel)

boxcox(WageNewModel)

library(alr3)
attach(train)
summary(powerTransform(cbind(WageNew,Overall,International.Reputation,Potential,Skill.Moves)~1))
detach(train)

tOverall <- train$Overall^3
tInt <- train$International.Reputation^-14
tPotential <- train$Potential^.75
tWage <- log(train$WageNew)
myvars = c("International.Reputation","Potential","Skill.Moves","realfaceyes","Small","Overall") 
train2 = train[myvars]
WageNewModel <- lm(data=train2, tWage~tOverall+tInt+tPotential+realfaceyes+Small+Skill.Moves)
summary(WageNewModel)
par(mfrow=c(2,2))
plot(WageNewModel)
inverseResponsePlot(WageNewModel)

rmse <- function(error){  sqrt(mean(error^2))}
y_hat <- fitted.values(WageNewModel)
error <- train$WageNew - y_hat
rmse(error)
#

#interaction

WageNewModel <- lm(data=train2,log(train$WageNew)~I(Overall^3)+I(International.Reputation^-14)+I(Potential^.75)+realfaceyes+Small+Skill.Moves)
#
































#Account for NA values
test$Position[is.na(test$Position)]= "LB"
test$'International Reputation'[is.na(test$'International Reputation')]=median(test$'International Reputation',na.rm=T)
test$Potential[is.na(test$Potential)]=median(test$Potential,na.rm=T)
test$'Skill Moves'[is.na(test$'Skill Moves')]=median(test$'Skill Moves',na.rm=T)
test$realfaceyes[is.na(test$realfaceyes)]= 0


#Add new columns to test by position
test <- test %>% 
  mutate(GKtotal = GKDiving + GKHandling + GKKicking + GKPositioning + GKReflexes,
         MFp = as.numeric((Position=="CDM")+(Position=="CM")+(Position=="LAM")+(Position=="LCM")
                          +(Position=="LDM")+(Position=="LM")+(Position=="LWB")+(Position=="RAM")+
                            (Position=="RCM")+(Position=="RDM")+(Position=="RM")+(Position=="RWB")),
         FWp = as.numeric((Position=="CAM")+(Position=="CF")+(Position=="LF")+(Position=="RF"))+as.numeric((Position=="ST")
                                                                                                           +(Position=="LS")+(Position=="RS")+(Position=="RW")+(Position=="LW")),
         BKp = as.numeric((Position=="LB")+(Position=="CB")+(Position=="RB")+(Position=="LCB")+(Position=="RCB")),
         GKp = as.numeric(Position=="GK")
  )


#Add new variable realface = true
test <- test %>% 
  mutate(realfaceyes = as.numeric(test$`Real Face` == "Yes"))
test <- test %>% 
  mutate(
    Small = as.numeric((Club=="Manchester United")+(Club=="Arsenal")+(Club=="Real Madrid")+(Club=="Juventus")+(Club=="Chelsea")+(Club=="Tottenham Hotspur")+(Club=="Manchester City")+(Club=="Atl<e9>tico Madrid")+(Club=="Inter")+(Club=="Milan")+(Club=="Valencia CF")+(Club=="FC Bayern M<fc>nchen")+(Club=="Liverpool")+(Club=="Olympique Lyonnais")+(Club=="Paris Saint-Germain")+(Club=="Guangzhou Evergrande Taobao FC")+(Club=="FC Barcelona")+(Club=="Rayo Vallecano")+(Club=="Everton")+(Club=="West Ham United")+(Club=="Napoli")+(Club=="Borussia Dortmund")+(Club=="AS Monaco")+(Club=="Roma")
    ),
    nation = as.numeric((Nationality=="Afghanistan")+(Nationality=="Belize")+(Nationality=="Bolivia")+(Nationality=="Botswana")+(Nationality=="Bulgaria")+(Nationality=="Colombia")+(Nationality=="Greece")+(Nationality=="Guatemala")+(Nationality=="Hong Kong")+(Nationality=="India")+(Nationality=="Indonesia")+(Nationality=="Jordan")+(Nationality=="Kenya")+(Nationality=="New Zealand")+(Nationality=="Norway")+(Nationality=="Qatar")+(Nationality=="Russia")+(Nationality=="South Africa")+(Nationality=="Sweden")+(Nationality=="Ukraine"))  
    
    
  )
test$Small[is.na(test$Small)]=0
test$nation[is.na(test$nation)]=0
test$realfaceyes[is.na(test$realfaceyes)]=0






#Create Prediction to Upload
myvars = c("Overall","International Reputation","Potential","Skill Moves","realfaceyes","Small","nation") 
test2 = test[myvars]
#colnames(test2)[3] <- "International Reputation"
#WageNewModel5 <- lm(data=test2, I(train$WageNew^.66) ~.)


apply(test2,2,function(x)sum(is.na(x)))
test2$realfaceyes[is.na(test2$realfaceyes)]=0

colnames(test2)[2] <- "International.Reputation"
colnames(test2)[4] <- "Skill.Moves"
prediction <- predict(WageNewModel, newdata = test2)
write.csv(prediction, file = "MyData2.csv")
