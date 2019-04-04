#Libraries ----
library(readr)
library(dplyr)
library(corrplot)
library(car)
library(ggplot2)
library(MASS)
library(alr3)
library(leaps)
library(lm.beta)

setwd("C:/Users/mattm/Google Drive/a Stat 101a/Project")

train <- read_csv("HTrainW19Final.csv")
test <- read_csv("HTestW19Final No Y values.csv")


#Data Manipulation----
#apply(train,2,function(x)sum(is.na(x)))
train <- train %>% mutate(BsmtQual = ifelse(TotalBsmtSF == 0, "None", BsmtQual))
train <- train %>% mutate(GarageQual = ifelse(GarageCars == 0, "None", GarageQual))

train$MSZoning[is.na(train$MSZoning)]= "RL"
train$LotFrontage[is.na(train$LotFrontage)]= median(train$LotFrontage,na.rm=T)
train$BsmtQual[is.na(train$BsmtQual)] = "TA"
train$BsmtCond[is.na(train$BsmtCond)] = "TA"
train$BsmtExposure[is.na(train$BsmtExposure)] = "No"
train$BsmtFinType1[is.na(train$BsmtFinType1)] = "Unf"
train$BsmtFinType2[is.na(train$BsmtFinType2)] = "Unf"
train$BsmtFinSF1[is.na(train$BsmtFinSF1)] = 0
train$BsmtFinSF2[is.na(train$BsmtFinSF2)] = 0
train$BsmtUnfSF[is.na(train$BsmtUnfSF)]= 0
train$TotalBsmtSF[is.na(train$TotalBsmtSF)]=0
train$KitchenQual[is.na(train$KitchenQual)] = "TA"
train$Functional[is.na(train$Functional)] = "Typ"
train$BedroomAbvGr[train$BedroomAbvGr==0] = 1
train$YearBuilt[is.na(train$YearBuilt)]=round(mean(train$YearBuilt,na.rm=T),0)
train$ExterQual[is.na(train$ExterQual)]= "TA"
train$SaleType[is.na(train$SaleType)]= "WD"


#New Vars----
#ggplot(data = train, aes(x=reorder(SaleCondition,-log(SalePrice)), y=log(SalePrice), group=SaleCondition)) + geom_boxplot()
#ggplot(data = train, aes(x=Condition1, y=log(SalePrice), group=Condition1)) + geom_boxplot()
#ggplot(data = train, aes(x=TotRmsAbvGrd, y=log(SalePrice))) + geom_point()

A = 107847.3
B = 331984.7
medN <- medN %>%
  mutate(scaled = (1 + (medN-A)*(10-1)/(B-A)))
#ggplot(data = medN, aes(x=ob ,y=scaled)) + geom_point()

medN <- train %>% 
  group_by(Neighborhood) %>%
  summarise(medN = median(SalePrice)) %>% 
  arrange(medN)
medN <- cbind(medN, ob=1:25)
train <- train %>% mutate(scaledNmean = ifelse(Neighborhood=="MeadowV",1,ifelse(Neighborhood=="IDOTRR",1.296448,ifelse(Neighborhood=="BrDale",1.40778,ifelse(Neighborhood=="BrkSide",1.4408,ifelse(Neighborhood=="OldTown",1.648863,ifelse(Neighborhood=="Edwards",1.826226,ifelse(Neighborhood=="Sawyer",2.172936,ifelse(Neighborhood=="SWISU",2.391245,ifelse(Neighborhood=="NAmes",2.454361,ifelse(Neighborhood=="Blueste",2.616718,ifelse(Neighborhood=="NPkVill",2.918449,ifelse(Neighborhood=="Mitchel",3.316729,ifelse(Neighborhood=="SawyerW",3.942787,ifelse(Neighborhood=="Gilbert",4.078011,ifelse(Neighborhood=="NWAmes",4.260111,ifelse(Neighborhood=="ClearCr",4.357136,ifelse(Neighborhood=="Crawfor",4.363573,ifelse(Neighborhood=="CollgCr",4.9048,ifelse(Neighborhood=="Somerst",5.483287,ifelse(Neighborhood=="Blmngtn",5.836831,ifelse(Neighborhood=="Veenker",6.47508,ifelse(Neighborhood=="Timber",6.86613,ifelse(Neighborhood=="NoRidge",9.14501,ifelse(Neighborhood=="StoneBr",10,5)))))))))))))))))))))))))

train <- train %>% 
  mutate( sqqq = BsmtFinSF1+BsmtFinSF2+(.5*BsmtUnfSF)+train$`1stFlrSF`+train$`2ndFlrSF`,
          
          kitchenq = ifelse(KitchenQual=="Ex",5,ifelse(KitchenQual=="Gd",4,ifelse(KitchenQual=="TA",3,2))),
          garageq = ifelse(GarageQual=="Ex",5,ifelse(GarageQual=="Gd",4,ifelse(GarageQual=="TA",3,ifelse(GarageQual=="Fa",2,1)))),
          bsmtq = ifelse(BsmtQual=="Ex",5,ifelse(BsmtQual=="Gd",4,ifelse(BsmtQual=="TA",3,ifelse(BsmtQual=="Fa",2,1)))),
          exterq = ifelse(ExterQual=="Ex",5,ifelse(ExterQual=="Gd",4,ifelse(ExterQual=="TA",3,2))),
          combinedq = kitchenq+garageq+OverallQual^2+bsmtq+exterq)


#----
rm <- c("KitchenQual","GarageQual","BsmtQual","ExterQual","MSZoning","MSSubClass","LotFrontage","Street","Alley","LandContour","LotShape","Utilities","LotConfig","LandSlope","Condition1","Condition2","Fence","MiscFeature","MiscVal","PoolArea","PoolQC","MoSold","YrSold","RoofStyle","RoofMatl","MasVnrType","MasVnrArea","Foundation","GarageType","GarageYrBlt","GarageFinish","PavedDrive","WoodDeckSF","OpenPorchSF","EnclosedPorch","ScreenPorch","3SsnPorch","FireplaceQu","Heating","HeatingQC","CentralAir","Electrical","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","KitchenAbvGr","Functional","BsmtCond","BsmtExposure","Exterior1st","Exterior2nd","BldgType","HouseStyle","ExterCond","Ob","BsmtFinType1","BsmtFinType2","BsmtFinType1a","BsmtFinSF1","BsmtFinType2a","BsmtFinSF2","BsmtUnfSF","bsmtsqft","1stFlrSF","2ndFlrSF","sqft","tot","ttt","LowQualFinSF","GarageCond","SaleType","SaleCondition","YearRemodAdd","Neighborhood","GarageArea","bsmtsqq","GrLivArea","OverallQual","kitchenq","garageq","bsmtq","exterq","TotRmsAbvGrd")
train68 <- train[, !(names(train) %in% rm)]
aabb <- regsubsets(log(SalePrice)~., data=train68, method="exhaustive",really.big = T)
summary(aabb)

t9 <- lm(log(train$SalePrice)~.-sqqq+log(sqqq),data=train68)
summary(t9)
t9a <- summary(t9)

t7  <- lm(log(train$SalePrice)~(OverallCond+YearBuilt+GarageCars+log(sqqq)+combinedq+scaledNmean+BedroomAbvGr+Fireplaces+TotalBsmtSF),data=train)
summary(t7)
t7a <- summary(t7)

t10  <- lm(log(train$SalePrice)~(OverallCond+YearBuilt+GarageCars+log(sqqq)+combinedq+scaledNmean+TotalBsmtSF),data=train)
summary(t10)
t10a <- summary(t10)

t11  <- lm(log(train$SalePrice)~(OverallCond+YearBuilt+GarageCars+log(sqqq)+combinedq+scaledNmean+TotalBsmtSF+log(LotArea)),data=train)
summary(t11)
t11a <- summary(t11)


t7a$adj.r.squared
t9a$adj.r.squared
t10a$adj.r.squared
t11a$adj.r.squared

n <- length(t7$residuals)
npar <- length(t7$coefficients) +1
extractAIC(t7,k=2)
extractAIC(t7,k=2)+2*npar*(npar+1)/(n-npar-1)
extractAIC(t7,k=log(n))

n3 <- length(t9$residuals)
npar3 <- length(t9$coefficients) +1
extractAIC(t9,k=2)
extractAIC(t9,k=2)+2*npar3*(npar3+1)/(n3-npar3-1)
extractAIC(t9,k=log(n3))

n4 <- length(t10$residuals)
npar3 <- length(t10$coefficients) +1
extractAIC(t10,k=2)
extractAIC(t10,k=2)+2*npar3*(npar3+1)/(n4-npar3-1)
extractAIC(t10,k=log(n4))

n5 <- length(t11$residuals)
npar3 <- length(t11$coefficients) +1
extractAIC(t11,k=2)
extractAIC(t11,k=2)+2*npar3*(npar3+1)/(n5-npar3-1)
extractAIC(t11,k=log(n5))


par(mfrow=c(2,2))
plot(t7)

par(mfrow=c(2,2))
plot(t9)


t11  <- lm(log(train$SalePrice)~(OverallCond+YearBuilt+GarageCars+log(sqqq)+combinedq+scaledNmean+TotalBsmtSF+log(LotArea)),data=train)
summary(t11)
t11a <- summary(t11)
vif(t11)
par(mfrow=c(2,2))
plot(t11)

t11  <- lm(log(train$SalePrice)~(OverallCond+YearBuilt+GarageCars+log(sqqq)+combinedq+log(scaledNmean)+TotalBsmtSF+log(LotArea))^2,data=train)
summary(t11)

attach(train)
summary(powerTransform(cbind(SalePrice,OverallCond,YearBuilt,sqqq,combinedq,scaledNmean,LotArea)~1))
detach(train)

#Final .9238 ->>  .93025 ----
t11  <- lm(log(train$SalePrice)~(OverallCond+YearBuilt+GarageCars+log(sqqq)+combinedq+log(scaledNmean)+TotalBsmtSF+log(LotArea)+Fireplaces+
                                   OverallCond:YearBuilt+YearBuilt:log(sqqq)+combinedq:log(scaledNmean)+TotalBsmtSF:log(LotArea)  ),data=train68)
summary(t11)
par(mfrow=c(2,2))
plot(t11)

anova(t11)
aov(t11)


forwardAIC <- step(t11,direction="forward", data=train)
summary(forwardAIC)
forwardBIC <- step(t11,direction="forward", data=train,k=log(nrow(train)))
summary(forwardBIC)


attach(train68)
summary(powerTransform(cbind(SalePrice,OverallCond,YearBuilt,sqqq,combinedq,scaledNmean,LotArea)~1))
detach(train68)



attach(train68)
par(mfrow=c(2,2))
mmp(t11,OverallCond)
mmp(t11,YearBuilt)
mmp(t11,GarageCars)
mmp(t11,log(sqqq))
mmp(t11,combinedq)
mmp(t11,scaledNmean)
mmp(t11,TotalBsmtSF)
mmp(t11,log(LotArea))
mmp(t11,train$Fireplaces)

par(mfrow=c(4,2))
avPlot(t11,OverallCond)
avPlot(t11,YearBuilt)
avPlot(t11,GarageCars)
avPlot(t11,log(sqqq))
avPlot(t11,combinedq)
avPlot(t11,scaledNmean)
avPlot(t11,TotalBsmtSF)
avPlot(t11,log(LotArea))
detach(train68)



#Test Data----
test <- test %>% mutate(scaledNmean = ifelse(Neighborhood=="MeadowV",1,ifelse(Neighborhood=="IDOTRR",1.296448,ifelse(Neighborhood=="BrDale",1.40778,ifelse(Neighborhood=="BrkSide",1.4408,ifelse(Neighborhood=="OldTown",1.648863,ifelse(Neighborhood=="Edwards",1.826226,ifelse(Neighborhood=="Sawyer",2.172936,ifelse(Neighborhood=="SWISU",2.391245,ifelse(Neighborhood=="NAmes",2.454361,ifelse(Neighborhood=="Blueste",2.616718,ifelse(Neighborhood=="NPkVill",2.918449,ifelse(Neighborhood=="Mitchel",3.316729,ifelse(Neighborhood=="SawyerW",3.942787,ifelse(Neighborhood=="Gilbert",4.078011,ifelse(Neighborhood=="NWAmes",4.260111,ifelse(Neighborhood=="ClearCr",4.357136,ifelse(Neighborhood=="Crawfor",4.363573,ifelse(Neighborhood=="CollgCr",4.9048,ifelse(Neighborhood=="Somerst",5.483287,ifelse(Neighborhood=="Blmngtn",5.836831,ifelse(Neighborhood=="Veenker",6.47508,ifelse(Neighborhood=="Timber",6.86613,ifelse(Neighborhood=="NoRidge",9.14501,ifelse(Neighborhood=="StoneBr",10,5)))))))))))))))))))))))))

A = 107847.3
B = 331984.7
medN <- medN %>% 
  mutate(scaled = (1 + (medN-A)*(10-1)/(B-A)))
ggplot(data = medN, aes(x=ob ,y=scaled)) + geom_point()


test <- test %>% mutate(GarageQual = ifelse(GarageCars == 0, "None", GarageQual))
test <- test %>% mutate(BsmtQual = ifelse(TotalBsmtSF == 0, "None", BsmtQual))
test$GarageQual[is.na(test$GarageQual)]= "TA"
test$TotalBsmtSF[is.na(test$TotalBsmtSF)]=0
test$YearBuilt[is.na(test$YearBuilt)]=round(mean(test$YearBuilt,na.rm=T),0)
test$MasVnrType[is.na(test$MasVnrType)]= "None"
test$ExterQual[is.na(test$ExterQual)]= "TA"
test$SaleType[is.na(test$SaleType)]= "WD"
test$BsmtUnfSF[is.na(test$BsmtUnfSF)]= 0
#test$finBsmt[is.na(test$finBsmt)]= 0
test$LotFrontage[is.na(test$LotFrontage)]= median(test$LotFrontage,na.rm=T)
test$MSZoning[is.na(test$MSZoning)]= "RL"
test$BedroomAbvGr[test$BedroomAbvGr==0] = 1
test$Utilities[is.na(test$Utilities)] = "AllPub"
test$MasVnrArea[is.na(test$MasVnrArea)] = 0
test$BsmtQual[is.na(test$BsmtQual)] = "TA"
test$BsmtCond[is.na(test$BsmtCond)] = "TA"
test$BsmtExposure[is.na(test$BsmtExposure)] = "No"
test$BsmtFinType1[is.na(test$BsmtFinType1)] = "Unf"
test$BsmtFinType2[is.na(test$BsmtFinType2)] = "Unf"
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] = 0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] = 0
test$BsmtFullBath[is.na(test$BsmtFullBath)] = 0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] = 0
test$KitchenQual[is.na(test$KitchenQual)] = "TA"
test$Functional[is.na(test$Functional)] = "Typ"

apply(test,2,function(x)sum(is.na(x)))
test$GarageCars[is.na(test$GarageCars)]= 0
test$GarageArea[is.na(test$GarageArea)]= 0

test <- test %>% 
  mutate( sqft = test$'1stFlrSF' + test$'2ndFlrSF',
          bsmtsqft = BsmtFinSF1 + BsmtFinSF2,
          bath = BsmtFullBath + .5*BsmtHalfBath + FullBath + .5*HalfBath,
          tot = sqft + bsmtsqft,
          ttt = bsmtsqft+sqft+.5*BsmtUnfSF)
test <- test %>%
  mutate(kitchenq = ifelse(KitchenQual=="Ex",5,ifelse(KitchenQual=="Gd",4,ifelse(KitchenQual=="TA",3,2))),
         garageq = ifelse(GarageQual=="Ex",5,ifelse(GarageQual=="Gd",4,ifelse(GarageQual=="TA",3,ifelse(GarageQual=="Fa",2,1)))),
         bsmtq = ifelse(BsmtQual=="Ex",5,ifelse(BsmtQual=="Gd",4,ifelse(BsmtQual=="TA",3,ifelse(BsmtQual=="Fa",2,1)))),
         exterq = ifelse(ExterQual=="Ex",5,ifelse(ExterQual=="Gd",4,ifelse(ExterQual=="TA",3,2)))
  )
test <- test %>% 
  mutate(combinedq = kitchenq+garageq+OverallQual^2+bsmtq+exterq)







test <- test %>% 
  mutate( sqft = test$'1stFlrSF' + test$'2ndFlrSF',
          bsmtsqft = BsmtFinSF1 + BsmtFinSF2,
          bath = BsmtFullBath + .5*BsmtHalfBath + FullBath + .5*HalfBath,
          tot = test$'1stFlrSF' + test$'2ndFlrSF' + BsmtFinSF1 + BsmtFinSF2,
          ttt = bsmtsqft+sqft+.5*BsmtUnfSF,
          sqqq = BsmtFinSF1+BsmtFinSF2+(.5*BsmtUnfSF)+test$`1stFlrSF`+test$`2ndFlrSF`+(.5*LowQualFinSF),
          
          kitchenq = ifelse(KitchenQual=="Ex",5,ifelse(KitchenQual=="Gd",4,ifelse(KitchenQual=="TA",3,2))),
          garageq = ifelse(GarageQual=="Ex",5,ifelse(GarageQual=="Gd",4,ifelse(GarageQual=="TA",3,ifelse(GarageQual=="Fa",2,1)))),
          bsmtq = ifelse(BsmtQual=="Ex",5,ifelse(BsmtQual=="Gd",4,ifelse(BsmtQual=="TA",3,ifelse(BsmtQual=="Fa",2,1)))),
          exterq = ifelse(ExterQual=="Ex",5,ifelse(ExterQual=="Gd",4,ifelse(ExterQual=="TA",3,2))),
          zone = ifelse(MSZoning== "FV",5,ifelse(MSZoning== "RL",4,ifelse(MSZoning== "RH",3,ifelse(MSZoning== "RM",2,1)))),
          bldgt = ifelse(BldgType== "TwnhsE",5,ifelse(BldgType== "1Fam",4,ifelse(BldgType== "Duplex",3,ifelse(BldgType== "Twnhs",2,1))))
  )

test <- test %>% 
  mutate(
    houses = ifelse(HouseStyle== "2Story",10,ifelse(HouseStyle== "2.5Fin",9,ifelse(HouseStyle== "1Story",8,ifelse(HouseStyle== "SLvl",7,ifelse(HouseStyle=="2.5Unf",6,ifelse(HouseStyle=="SFoyer",5,ifelse(HouseStyle=="1.5Fin",4,3)))))))
  )

test <- test %>% 
  mutate(
    BsmtFinType1a = ifelse(BsmtFinType1=="Unf",.5,1),
    BsmtFinType2a = ifelse(BsmtFinType2=="Unf",.5,1)
  )
test <- test %>% 
  mutate(bsmtsqq = BsmtFinType1a * BsmtFinSF1 + BsmtFinType2a * BsmtFinSF2 + .5 * BsmtUnfSF)
test <- test %>% 
  mutate(combinedq = kitchenq+garageq+OverallQual^2+bsmtq+exterq)

test <- test %>% 
  mutate(salet = ifelse(SaleType=="New",10,ifelse(SaleType=="Con",9,ifelse(SaleType=="WD",8,ifelse(SaleType=="CWD",7,ifelse(SaleType=="Oth",6,ifelse(SaleType=="ConLI",5,ifelse(SaleType=="COD",4,ifelse(SaleType=="ConLD",3,2)))))))))

test <- test %>% 
  mutate(salec = ifelse(SaleCondition=="Partial",10,ifelse(SaleCondition=="Normal",9,ifelse(SaleCondition=="Alloca",8,ifelse(SaleCondition=="Family",7,ifelse(SaleCondition=="Abnorml",6,5))))))




#Predict ----
prediction7 <- predict(t11, newdata=test)
prediction7 <- exp(prediction7)
write.csv(prediction7, file="pred99.csv")
