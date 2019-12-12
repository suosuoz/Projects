library(glmnet)
library(readr)
library(dplyr)
library(car)

setwd("C:/Users/Matt Matsuo/Google Drive/a Stat 101C/Final Project")
train <- read_csv("train.csv")
test <- read_csv("test.csv")
train$HTWins <- as.factor(train$HTWins)

setwd("C:/Users/mattm/Google Drive/a Stat 101C/Final Project")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#HT.TA == VT.OTA
#HT.TS == VT.OTS
#HT.OTA == VT.TA
#HT.OTS == VT.TS



#EXPLORATION New Variables
#eFG%
# train <- train %>% mutate(htEFG = (HT.TS.fgm + 1.5*HT.TS.tpm) / (HT.TS.fga))
# train <- train %>% mutate(ht.def.EFG = (HT.TA.fgm+1.5*HT.TA.tpm) / (HT.TA.fga))
# train <- train %>% mutate(vtEFG = (HT.OTS.fgm+1.5*HT.OTS.tpm) / (VT.TS.fga))
# train <- train %>% mutate(vt.def.EFG = (HT.OTA.fgm+1.5*HT.OTA.tpm) / (VT.TA.fga))
# 
# test <- test %>% mutate(htEFG = (HT.TS.fgm + 1.5*HT.TS.tpm) / (HT.TS.fga))
# test <- test %>% mutate(ht.def.EFG = (HT.TA.fgm+1.5*HT.TA.tpm) / (HT.TA.fga))
# test <- test %>% mutate(vtEFG = (HT.OTS.fgm+1.5*HT.OTS.tpm) / (VT.TS.fga))
# test <- test %>% mutate(vt.def.EFG = (HT.OTA.fgm+1.5*HT.OTA.tpm) / (VT.TA.fga))



#TOV%
#train <- train %>% 
#          mutate(htTOV = 100 * HT.TS.to/(HT.TS.fga + .44*HT.TS.fta + HT.TS.to))
#train <- train %>% 
#          mutate(ht.def.TOV = 100 * HT.TA.to/(HT.TA.fga + .44*HT.TA.fta + HT.TA.to))
#train <- train %>% 
#          mutate(vtTOV = 100 * HT.OTS.to/(HT.OTS.fga + .44*HT.OTS.fta + HT.OTS.to))
#train <- train %>% 
#          mutate(vt.def.TOV = 100 * HT.OTA.to/(HT.OTA.fga + .44*HT.OTA.fta + HT.OTA.to))



#Team Days Off Between Games Using HTcumrest, VTcumrest
# VT.rest <- rep(0,nrow(train))
# i <- 1
# for(i in 1:nrow(train)){
#   current <- train$VT[i]
#   range <- train[1:i-1,]
#   last.home <- max(which(range$HT == train$VT[i], arr.ind = TRUE))
#   last.visit <- max(which(range$VT == train$VT[i], arr.ind = TRUE))
#   if(last.home > last.visit){
#     days.since <- train$VTcumRest[i] - train$HTcumRest[last.home]
#   } else{
#     days.since <- train$VTcumRest[i] - train$VTcumRest[last.visit]
#   }
#   VT.rest[i] <- days.since
# }
# VT.rest
# for(i in 1:nrow(train)){
#   if(is.na(VT.rest[i])){
#     VT.rest[i] <- 0
#   }
# }
# 
# HT.rest <- rep(0,nrow(train))
# i <- 1
# for(i in 1:nrow(train)){
#   current <- train$HT[i]
#   range <- train[1:i-1,]
#   last.home <- max(which(range$HT == train$HT[i], arr.ind = TRUE))
#   last.visit <- max(which(range$VT == train$HT[i], arr.ind = TRUE))
#   if(last.home > last.visit){
#     days.since <- train$HTcumRest[i] - train$HTcumRest[last.home]
#   } else{
#     days.since <- train$HTcumRest[i] - train$VTcumRest[last.visit]
#   }
#   HT.rest[i] <- days.since
# }
# HT.rest
# for(i in 1:nrow(train)){
#   if(is.na(HT.rest[i])){
#     HT.rest[i] <- 0
#   }
# }
# train$VT.rest <- VT.rest
# train$HT.rest <- HT.rest
# train <- train %>%
#   mutate(VT.rest2 = ifelse(VT.rest == 1, 0, 1))
# train <- train %>%
#   mutate(HT.rest2 = ifelse(HT.rest == 1, 0, 1))
# 
# VT2.rest <- rep(0,nrow(test))
# i <- 1
# for(i in 1:nrow(test)){
#   current <- test$VT[i]
#   range <- test[1:i-1,]
#   last.home <- max(which(range$HT == test$VT[i], arr.ind = TRUE))
#   last.visit <- max(which(range$VT == test$VT[i], arr.ind = TRUE))
#   if(last.home > last.visit){
#     days.since <- test$VTcumRest[i] - test$HTcumRest[last.home]
#   } else{
#     days.since <- test$VTcumRest[i] - test$VTcumRest[last.visit]
#   }
#   VT2.rest[i] <- days.since
# }
# VT2.rest
# for(i in 1:nrow(test)){
#   if(is.na(VT2.rest[i])){
#     VT2.rest[i] <- 0
#   }
# }
# 
# HT2.rest <- rep(0,nrow(test))
# i <- 1
# for(i in 1:nrow(test)){
#   current <- test$HT[i]
#   range <- test[1:i-1,]
#   last.home <- max(which(range$HT == test$HT[i], arr.ind = TRUE))
#   last.visit <- max(which(range$VT == test$HT[i], arr.ind = TRUE))
#   if(last.home > last.visit){
#     days.since <- test$HTcumRest[i] - test$HTcumRest[last.home]
#   } else{
#     days.since <- test$HTcumRest[i] - test$VTcumRest[last.visit]
#   }
#   HT2.rest[i] <- days.since
# }
# HT2.rest
# for(i in 1:nrow(test)){
#   if(is.na(HT2.rest[i])){
#     HT2.rest[i] <- 0
#   }
# }
# test$VT.rest <- VT2.rest
# test$HT.rest <- HT2.rest
# test <- test %>%
#   mutate(VT.rest2 = ifelse(VT.rest == 1, 0, 1))
# test <- test %>%
#   mutate(HT.rest2 = ifelse(HT.rest == 1, 0, 1))



#######FUNCTIONS
getconf <- function(model){
  pred <- predict(model, train, type = "response")
  pred <- data.frame(pred)
  colnames(pred) <- c("first")
  pred <- pred %>%
    mutate(second = ifelse(first > 0.5,"Yes","No"))
  confmatrix <- table(Actual = train$HTWins, Predicted = pred$second)
  #accuracy
  cat(confmatrix)
  (confmatrix[1,1] + confmatrix[2,2]) / sum(confmatrix)
}

getsub <- function(model, number){
  pred <- predict(model, test, type = "response")
  pred <- data.frame(pred)
  colnames(pred) <- c("first")
  pred <- pred %>%
    mutate(second = ifelse(first > 0.5,"Yes","No"))
  submission4 <- cbind(test$id, pred$second)
  colnames(submission4) <- c("id","HTWins")
  write.csv(submission4, file=paste("submit",number,".csv",sep=""), row.names=FALSE)
}



##Variable Selection by Random Forest
# library(tree)
# library(randomForest)
# mod <- tree(HTWins~HT + VT.rest2 + HT.rest2  + sqrt(VT.TS.fgm) + VT.TS.fga +
#               VT.TS.tpm + VT.TS.tpa + sqrt(VT.TS.fta) + VT.TS.oreb + sqrt(VT.TS.stl) +
#               VT.TS.pts + sqrt(VT.TA.fgm) + sqrt(VT.TA.tpm) + VT.TA.tpa + VT.TA.fta +
#               VT.TA.oreb + sqrt(VT.TA.dreb) + sqrt(VT.TA.ast) + sqrt(VT.TA.pts) + VT.OTS.fgm +
#               VT.OTS.fga + VT.OTS.tpm + VT.OTS.tpa + VT.OTS.stl + VT.OTS.pts +
#               VT.OTA.tpa + VT.OTA.oreb + VT.OTA.ast + VT.OTA.blk + VT.S1.plmin +
#               VT.S1.pts + VT.S1.ast + VT.S2.pts + VT.S2.ast + VT.S3.pts +
#               VT.S3.min + VT.S3.stl + VT.S3.ast + VT.S4.pts + VT.S4.min +
#               VT.S4.ast + VT.S5.pts + VT.S5.min + VT.S5.stl + VT.OS1.plmin +
#               VT.OS1.dreb + VT.OS2.plmin + VT.OS2.dreb + VT.OS2.fgm + VT.OS3.plmin +
#               VT.OS3.dreb + VT.OS3.to + VT.OS4.dreb + VT.OS5.to + VT.OS5.oreb, data=train)
# 
# summary(mod)
# plot(mod)
# text(mod, pretty=0)
# 
# p <- dim(train)[2]
# rf <- randomForest(HTWins~HT + VT.rest2 + HT.rest2  + VT.TS.fgm + VT.TS.fga +
#                      VT.TS.tpm + VT.TS.tpa + VT.TS.fta + VT.TS.oreb + VT.TS.stl +
#                      VT.TS.pts + VT.TA.fgm + VT.TA.tpm + VT.TA.tpa + VT.TA.fta +
#                      VT.TA.oreb + VT.TA.dreb + VT.TA.ast + VT.TA.pts + VT.OTS.fgm +
#                      VT.OTS.fga + VT.OTS.tpm + VT.OTS.tpa + VT.OTS.stl + VT.OTS.pts +
#                      VT.OTA.tpa + VT.OTA.oreb + VT.OTA.ast + VT.OTA.blk + VT.S1.plmin +
#                      VT.S1.pts + VT.S1.ast + VT.S2.pts + VT.S2.ast + VT.S3.pts +
#                      VT.S3.min + VT.S3.stl + VT.S3.ast + VT.S4.pts + VT.S4.min +
#                      VT.S4.ast + VT.S5.pts + VT.S5.min + VT.S5.stl + VT.OS1.plmin +
#                      VT.OS1.dreb + VT.OS2.plmin + VT.OS2.dreb + VT.OS2.fgm + VT.OS3.plmin +
#                      VT.OS3.dreb + VT.OS3.to + VT.OS4.dreb + VT.OS5.to + VT.OS5.oreb, data=train, mtry=p-1, importance = TRUE)
# importance(rf)




#Random Forest Model: #64.68 final
train2 <- train[,1:114]
p <- dim(train2)[2]
library(tree)
library(randomForest)

rf <- randomForest(HTWins~.,data=train2, mtry=p-1,importance=TRUE)
imp <- as.data.frame(importance(rf))
sort(imp$MeanDecreaseAccuracy, decreasing=T)


library(caret)
varImpPlot(rf)

xxx <- glm(HTWins~HT+VT+
             VT.S1.plmin+VT.S2.plmin+VT.S3.plmin+VT.S4.plmin+VT.S5.plmin+
             VT.OS1.plmin+VT.OS2.plmin+VT.OS3.plmin+VT.OS4.plmin+VT.OS5.plmin+
             VT.OTA.ast+VT.OTA.dreb+VT.TA.ast+
             VT.TS.ast+VT.S1.pts+HT.ptsdif+VT.ptsdif+
             VT.OTA.blk
           ,data=train, family=binomial)
summary(xxx)
getconf(xxx)
vif(xxx)
getsub(xxx,31)



#Stepwise Model
step(xxx, direction=c("both"))

xxx2 <- glm(formula = HTWins ~ HT + VT.S1.plmin + VT.S2.plmin + VT.S3.plmin + 
              VT.OS1.plmin + VT.OS2.plmin + VT.OS3.plmin + VT.OS4.plmin + 
              VT.OTA.ast + VT.OTA.dreb + VT.TA.ast + VT.OTS.to + 
              VT.TS.ast + VT.OTS.pts + VT.S1.pts + 
              VT.OTA.blk + VT.OTS.tpm, family = binomial, data = train)
summary(xxx2)
getconf(xxx2)

#68.035 -> 66.262
# onex <- glm(HTWins~
#               VT.TS.fgp+VT.TS.tpm+VT.TS.fta+VT.TS.ast+VT.TS.pts+
#               VT.TA.fgp+VT.TA.tpm+VT.TA.fta+VT.TA.ast+VT.TA.pts+
#               VT.OTS.fgp+VT.OTS.tpm+VT.OTS.fta+VT.OTS.ast+VT.OTS.pts+
#               VT.OTA.fgp+VT.OTA.tpm+VT.OTA.fta+VT.OTA.ast+VT.OTA.pts+
#               VT.S1.plmin+VT.S1.pts+
#               VT.OS1.plmin+VT.OS2.plmin+VT.OS3.plmin
#             ,data=train,family=binomial)
# summary(onex)
# getconf(onex)
# plot(onex)
# 
# 
# 
# twox <- glm(HTWins ~ VT.TS.fgp + VT.TA.fgp
#             ,data=train,family=binomial)
# summary(twox)
# getconf(twox)




#Testing New Variables
train <- train %>%
  mutate(VT.TS.fgp = VT.TS.fgm / VT.TS.fga)
train <- train %>%
  mutate(VT.TA.fgp = VT.TA.fgm / VT.TA.fga)
train <- train %>%
  mutate(VT.OTS.fgp = VT.OTS.fgm / VT.OTS.fga)
train <- train %>%
  mutate(VT.OTA.fgp = VT.OTA.fgm / VT.OTA.fga)
test <- test %>%
  mutate(VT.TS.fgp = VT.TS.fgm / VT.TS.fga)
test <- test %>%
  mutate(VT.TA.fgp = VT.TA.fgm / VT.TA.fga)
test <- test %>%
  mutate(VT.OTS.fgp = VT.OTS.fgm / VT.OTS.fga)
test <- test %>%
  mutate(VT.OTA.fgp = VT.OTA.fgm / VT.OTA.fga)
train <- train %>% 
  mutate(HT.ptsdif = HT.TS.pts-HT.TA.pts)
train <- train %>% 
  mutate(VT.ptsdif = HT.OTS.pts-HT.OTA.pts)
train <- train %>% 
  mutate(HT.totreb = HT.TS.dreb+HT.TS.oreb-HT.TA.dreb-HT.TS.oreb)
train <- train %>% 
  mutate(VT.totreb = HT.OTS.dreb+HT.OTS.oreb-HT.OTA.dreb-HT.OTS.oreb)
train <- train %>% 
  mutate(HT.astdif = HT.TS.ast-HT.TA.ast)
train <- train %>% 
  mutate(VT.astdif = HT.OTS.ast-HT.OTA.ast)
train <- train %>% 
  mutate(HT.tovdif = HT.TS.to-HT.TA.to)
train <- train %>% 
  mutate(VT.tovdif = HT.OTS.to-HT.OTA.to)
test <- test %>% 
  mutate(HT.ptsdif = HT.TS.pts-HT.TA.pts)
test <- test %>% 
  mutate(VT.ptsdif = HT.OTS.pts-HT.OTA.pts)
test <- test %>% 
  mutate(HT.totreb = HT.TS.dreb+HT.TS.oreb-HT.TA.dreb-HT.TS.oreb)
test <- test %>% 
  mutate(VT.totreb = HT.OTS.dreb+HT.OTS.oreb-HT.OTA.dreb-HT.OTS.oreb)
test <- test %>% 
  mutate(HT.astdif = HT.TS.ast-HT.TA.ast)
test <- test %>% 
  mutate(VT.astdif = HT.OTS.ast-HT.OTA.ast)
test <- test %>% 
  mutate(HT.tovdif = HT.TS.to-HT.TA.to)
test <- test %>% 
  mutate(VT.tovdif = HT.OTS.to-HT.OTA.to)




####MODELS
#68.34 --> 64.07 #28
twox <- glm(HTWins ~ HT.ptsdif + VT.ptsdif+HT.totreb+VT.totreb+HT.astdif+VT.astdif+
              
            
              VT.TS.tpa + VT.OTS.fgm + 
              VT.OTS.fga + VT.OTS.stl +  
              VT.OTA.blk +  
              VT.S1.plmin + VT.S1.ast + 
              VT.S2.pts + VT.S2.ast +  
              VT.S3.ast +
              VT.S4.ast + 
              VT.S5.pts + VT.S5.min + VT.S5.stl +
              VT.OS1.plmin + VT.OS1.dreb + 
              VT.OS2.dreb + 
              VT.OS3.plmin + 
              VT.OS3.dreb + 
              VT.OS4.dreb + 
              VT.OS5.to + VT.OS5.oreb
              
            ,data=train,family=binomial)
summary(twox)
getconf(twox)
getsub(twox,28)




#68.60294 -->  #29
twox <- glm(HTWins ~ HT.ptsdif + VT.ptsdif+HT.totreb+VT.totreb+HT.astdif+VT.astdif+
              I(VT.TS.fgm^.25) + 
              I(VT.TS.fta^-.5) + I(VT.TS.stl^.5) + 
              VT.OTS.fgm + 
              VT.OTS.fga + VT.OTS.stl + 
              VT.OTA.blk + VT.S1.plmin + 
              VT.S1.pts + VT.S1.ast + VT.S2.pts + VT.S2.ast + I(VT.S3.pts^.5) + 
              VT.S3.min + I(VT.S3.stl^.5) + VT.S3.ast + VT.S4.pts + VT.S4.min + 
              VT.S4.ast + VT.S5.pts + VT.S5.min + VT.S5.stl + VT.OS1.plmin + 
              VT.OS1.dreb + VT.OS2.dreb + VT.OS3.plmin + 
              VT.OS3.dreb + VT.OS4.dreb + VT.OS5.to + VT.OS5.oreb
            ,data=train,family=binomial)
summary(twox)
getconf(twox)

attach(train)
summary(powerTransform(cbind(VT.TS.fgm , VT.TS.fta , VT.TS.stl , 
                       VT.OTS.fgm , 
                       VT.OTS.fga , VT.OTS.stl , 
                       VT.OTA.blk ,  
                       VT.S1.pts , VT.S1.ast , VT.S2.pts , VT.S2.ast , VT.S3.pts , 
                       VT.S3.min , VT.S3.stl , VT.S3.ast , VT.S4.pts , VT.S4.min , 
                       VT.S4.ast , VT.S5.pts , VT.S5.min ,
                       VT.OS1.dreb , VT.OS2.dreb , 
                       VT.OS3.dreb , VT.OS4.dreb , VT.OS5.to , VT.OS5.oreb)))
detach(train)

  


  
#68.72899 --> 66.868 #27
stuff <- glm(HTWins ~  
               VT.TS.tpa + VT.TS.stl + 
               VT.TS.pts +  
               VT.TA.oreb + VT.TA.dreb + VT.TA.ast + VT.TA.pts + VT.OTS.fgm + 
               VT.OTS.fga + VT.OTS.stl + VT.OTS.pts + 
               VT.OTA.tpa + VT.OTA.oreb + VT.OTA.ast + VT.OTA.blk + VT.S1.plmin + 
               VT.S1.pts + VT.S1.ast + VT.S2.pts + VT.S2.ast +  
               VT.S3.stl + VT.S3.ast +
               VT.S4.ast + VT.S5.pts + VT.S5.min + VT.S5.stl + VT.OS1.plmin + 
               VT.OS1.dreb + VT.OS2.plmin + VT.OS2.dreb + VT.OS3.plmin + 
               VT.OS3.dreb + VT.OS4.dreb + VT.OS5.to + VT.OS5.oreb
             , data = train, family = binomial)
summary(stuff)
vif(stuff)
getconf(stuff)
getsub(stuff,27)
#




#sub 18 69.3 --> 65.776
mod3 <- glm(HTWins ~ HT + VT.rest2 + HT.rest2  + sqrt(VT.TS.fgm) + VT.TS.fga + 
              VT.TS.tpm + VT.TS.tpa + sqrt(VT.TS.fta) + VT.TS.oreb + sqrt(VT.TS.stl) + 
              VT.TS.pts + sqrt(VT.TA.fgm) + sqrt(VT.TA.tpm) + VT.TA.tpa + VT.TA.fta + 
              VT.TA.oreb + sqrt(VT.TA.dreb) + sqrt(VT.TA.ast) + sqrt(VT.TA.pts) + VT.OTS.fgm + 
              VT.OTS.fga + VT.OTS.tpm + VT.OTS.tpa + VT.OTS.stl + VT.OTS.pts + 
              VT.OTA.tpa + VT.OTA.oreb + VT.OTA.ast + VT.OTA.blk + VT.S1.plmin + 
              VT.S1.pts + VT.S1.ast + VT.S2.pts + VT.S2.ast + VT.S3.pts + 
              VT.S3.min + VT.S3.stl + VT.S3.ast + VT.S4.pts + VT.S4.min + 
              VT.S4.ast + VT.S5.pts + VT.S5.min + VT.S5.stl + VT.OS1.plmin + 
              VT.OS1.dreb + VT.OS2.plmin + VT.OS2.dreb + VT.OS2.fgm + VT.OS3.plmin + 
              VT.OS3.dreb + VT.OS3.to + VT.OS4.dreb + VT.OS5.to + VT.OS5.oreb, data = train, family = binomial)
summary(mod3)
getconf(mod3)
getsub(mod3,18)

#sub 17 68.64496 --> 66.019
mod3 <- glm(HTWins ~ HT + VT.rest2 + HT.rest2  + htEFG + ht.def.EFG + vtEFG + vt.def.EFG +
                     sqrt(VT.TS.fta) + VT.TS.oreb + sqrt(VT.TS.stl) + 
                     VT.TS.pts + VT.TA.fta + 
                     VT.TA.oreb + sqrt(VT.TA.dreb) + sqrt(VT.TA.ast) + VT.OTS.stl + VT.OTA.oreb + VT.OTA.ast + VT.OTA.blk + VT.S1.plmin + 
                     VT.S1.pts + VT.S1.ast + VT.S2.pts + VT.S2.ast + VT.S3.pts + 
                     VT.S3.min + VT.S3.stl + VT.S3.ast + VT.S4.pts + VT.S4.min + 
                     VT.S4.ast + VT.S5.pts + VT.S5.min + VT.S5.stl + VT.OS1.plmin + 
                     VT.OS1.dreb + VT.OS2.plmin + VT.OS2.dreb + VT.OS2.fgm + VT.OS3.plmin + 
                     VT.OS3.dreb + VT.OS3.to + VT.OS4.dreb + VT.OS5.to + VT.OS5.oreb, data = train, family = binomial)
summary(mod3)
getconf(mod3)

#sub 18 68.5 --> 65.169
mod18 <- glm(HTWins ~ VT.rest2 + HT.rest2 + htEFG + ht.def.EFG + vtEFG + vt.def.EFG +
               VT.TS.fta + VT.TS.oreb + VT.TS.stl + 
               VT.TA.fta + 
               VT.TA.oreb + VT.TA.dreb + VT.TA.ast + 
               VT.OTS.stl + 
               VT.OTA.oreb + VT.OTA.ast + VT.OTA.blk + VT.S1.plmin + 
               VT.S1.pts + VT.S1.ast + VT.S2.pts + VT.S2.ast + VT.S3.pts + 
               VT.S3.min + VT.S3.stl + VT.S3.ast + VT.S4.pts + VT.S4.min + 
               VT.S4.ast + VT.S5.pts + VT.S5.min + VT.S5.stl + VT.OS1.plmin + 
               VT.OS1.dreb + VT.OS2.plmin + VT.OS2.dreb + VT.OS2.fgm + VT.OS3.plmin + 
               VT.OS3.dreb + VT.OS3.to + VT.OS4.dreb + VT.OS5.to + VT.OS5.oreb, data = train, family = binomial)
getconf(mod18)
getsub(mod18, 19)



#BASE MODEL 68.5 --> 67.111
stuff <- glm(HTWins ~ VT.TS.fgm + 
               VT.TS.tpa + VT.TS.fta + VT.TS.oreb + VT.TS.stl + 
               VT.TS.pts + VT.TA.tpm + VT.TA.fta + 
               VT.TA.oreb + VT.TA.dreb + VT.TA.ast + VT.TA.pts + VT.OTS.fgm + 
               VT.OTS.fga + VT.OTS.tpa + VT.OTS.stl + VT.OTS.pts + 
               VT.OTA.tpa + VT.OTA.oreb + VT.OTA.ast + VT.OTA.blk + VT.S1.plmin + 
               VT.S1.pts + VT.S1.ast + VT.S2.pts + VT.S2.ast + VT.S3.pts + 
               VT.S3.min + VT.S3.stl + VT.S3.ast + VT.S4.pts + VT.S4.min + 
               VT.S4.ast + VT.S5.pts + VT.S5.min + VT.S5.stl + VT.OS1.plmin + 
               VT.OS1.dreb + VT.OS2.plmin + VT.OS2.dreb + VT.OS2.fgm + VT.OS3.plmin +
               VT.OS3.dreb + VT.OS3.to + VT.OS4.dreb + VT.OS5.to + VT.OS5.oreb, data = train, family = binomial)
summary(stuff)
getconf(stuff)
#





#68.70798
train <- train %>% 
  mutate(VT.TS.tpp = VT.TS.tpa / VT.TS.tpm)
train <- train %>% 
  mutate(VT.TA.tpp = VT.TA.tpa / VT.TA.tpm)
train <- train %>% 
  mutate(VT.OTS.tpp = VT.OTS.tpa / VT.OTS.tpm)
train <- train %>% 
  mutate(VT.OTA.tpp = VT.OTA.tpa / VT.OTA.tpm)
stuff <- glm(HTWins ~ 
                           VT.TS.tpp + VT.TS.fta + VT.TS.stl + VT.TS.pts + 
               VT.TA.fgp+  VT.TA.tpp + VT.TA.oreb + VT.TA.dreb + VT.TA.ast + VT.TA.pts + 
               VT.OTS.fgp +                        VT.OTS.stl + VT.OTS.pts + 
                                                   VT.OTA.oreb + VT.OTA.ast + VT.OTA.blk + 
               VT.S1.plmin + VT.S1.pts + VT.S1.ast + 
               VT.S2.pts + VT.S2.ast + 
               VT.S3.stl + VT.S3.ast + 
               VT.S4.pts + VT.S4.min + VT.S4.ast + 
               VT.S5.pts + VT.S5.min + VT.S5.stl + 
               VT.OS1.plmin + VT.OS1.dreb + 
               VT.OS2.plmin + VT.OS2.dreb +  
               VT.OS3.plmin + VT.OS3.dreb + 
               VT.OS4.dreb + 
               VT.OS5.to + VT.OS5.oreb
               , data = train, family = binomial)
summary(stuff)
getconf(stuff)
vif(stuff)
#




#68.77101
train <- train %>% 
  mutate(VT.TS.tpp = VT.TS.tpa / VT.TS.tpm)
train <- train %>% 
  mutate(VT.TA.tpp = VT.TA.tpa / VT.TA.tpm)
train <- train %>% 
  mutate(VT.OTS.tpp = VT.OTS.tpa / VT.OTS.tpm)
train <- train %>% 
  mutate(VT.OTA.tpp = VT.OTA.tpa / VT.OTA.tpm)
test <- test %>% 
  mutate(VT.TS.tpp = VT.TS.tpa / VT.TS.tpm)
test <- test %>% 
  mutate(VT.TA.tpp = VT.TA.tpa / VT.TA.tpm)
test <- test %>% 
  mutate(VT.OTS.tpp = VT.OTS.tpa / VT.OTS.tpm)
test <- test %>% 
  mutate(VT.OTA.tpp = VT.OTA.tpa / VT.OTA.tpm)
stuff <- glm(HTWins ~ 
                           VT.TS.tpp + VT.TS.fta + VT.TS.pts + 
                           VT.TA.tpp + VT.TA.oreb + VT.TA.ast + VT.TA.pts + 
               VT.OTS.fgp +                        VT.OTS.stl + VT.OTS.pts + 
                                       VT.OTA.oreb + VT.OTA.ast + VT.OTA.blk + 
               VT.S1.plmin + VT.S1.pts + VT.S1.ast + 
               VT.S2.pts + VT.S2.ast + 
               VT.S3.stl + VT.S3.ast + 
               VT.S4.pts + VT.S4.min + VT.S4.ast + 
               VT.S5.pts + VT.S5.min + VT.S5.stl + 
               VT.OS1.plmin + VT.OS1.dreb + 
               VT.OS2.plmin + VT.OS2.dreb +  
               VT.OS3.plmin + VT.OS3.dreb + 
               VT.OS4.dreb + 
               VT.OS5.to + VT.OS5.oreb
             , data = train, family = binomial)
summary(stuff)
getconf(stuff)
vif(stuff)
getsub(stuff,33)
#





#20 68.81 --> 65.533
stuff <- glm(HTWins ~ VT.TS.fgm + VT.rest2 + HT.rest2 +
               VT.TS.tpa + VT.TS.fta + VT.TS.oreb + VT.TS.stl + 
               VT.TS.pts + VT.TA.tpm + VT.TA.fta + 
               VT.TA.oreb + VT.TA.dreb + VT.TA.ast + VT.TA.pts + VT.OTS.fgm + 
               VT.OTS.fga + VT.OTS.tpa + VT.OTS.stl + VT.OTS.pts + 
               VT.OTA.tpa + VT.OTA.oreb + VT.OTA.ast + VT.OTA.blk + VT.S1.plmin + 
               VT.S1.pts + VT.S1.ast + VT.S2.pts + VT.S2.ast + VT.S3.pts + 
               VT.S3.min + VT.S3.stl + VT.S3.ast + VT.S4.pts + VT.S4.min + 
               VT.S4.ast + VT.S5.pts + VT.S5.min + VT.S5.stl + VT.OS1.plmin + 
               VT.OS1.dreb + VT.OS2.plmin + VT.OS2.dreb + VT.OS2.fgm + VT.OS3.plmin + 
               VT.OS3.dreb + VT.OS3.to + VT.OS4.dreb + VT.OS5.to + VT.OS5.oreb, data = train, family = binomial)
summary(stuff)
getconf(stuff)
getsub(stuff, 20)
#


#points/game
#assists
#rebounds
#fg percentage
#free throw percentage
#3 point percentage
#effective field goal percentage
#true shooting percentage

#cubic
#step functions
#splines
#ridge lasso