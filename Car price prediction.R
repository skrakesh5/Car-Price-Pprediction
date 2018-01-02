#---- Assignment - Linear regression 
# libraries ----  
library(tidyr)
library(dplyr)
library(MASS)
library(car)
library(plotrix)

#import ---- 
cars <- read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)
View(cars)
str(cars)

#EDA and data correction ----
#checking for duplicates
unique(cars)
unique(cars$car_ID)
# no duplicates

#seperate cars company and car model
cars <- separate(cars,CarName,into = c("Company","Model"),sep = " ")
cars <- cars[,-4]
#droped model name

#changing factor values
cars$symboling <- as.factor(cars$symboling)
cars$fueltype <- as.factor(cars$fueltype)
cars$aspiration <- as.factor(cars$aspiration)
cars$doornumber <- as.factor(cars$doornumber)
cars$carbody <- as.factor(cars$carbody)
cars$drivewheel <- as.factor(cars$drivewheel)
cars$enginelocation <- as.factor(cars$enginelocation)
cars$enginetype <- as.factor(cars$enginetype)
cars$cylindernumber <- as.factor(cars$cylindernumber)
cars$fuelsystem <- as.factor(cars$fuelsystem)
cars$Company <- as.factor(cars$Company)

str(cars)

#few company names are mis spelt correcting those
cars$Company <- gsub("maxda","mazda",cars$Company)
cars$Company <- gsub("Nissan","nissan",cars$Company)
cars$Company <- gsub("toyouta","toyota",cars$Company)
cars$Company <- gsub("vokswagen","volkswagen",cars$Company)
cars$Company <- gsub("porcshce","porsche",cars$Company)
cars$Company <- gsub("vw","volkswagen",cars$Company)

#checking missing values
sum(is.na(cars))

#outlier treatment
quantile(cars$wheelbase,seq(0,1,0.01))
cars$wheelbase[which(cars$wheelbase >110)]<- 110

quantile(cars$carlength,seq(0,1,0.01))
cars$carlength[which(cars$carlength < 155)] <- 155.900
cars$carlength[which(cars$carlength > 202.480)] <- 202.480

quantile(cars$carwidth,seq(0,1,0.01))
#though small jump of 2 values is present ignoring it, asuming the jump to be too small

quantile(cars$carheight,seq(0,1,0.01))
#No outliers found

quantile(cars$curbweight,seq(0,1,0.01))
cars$curbweight[which(cars$curbweight < 1819.72)] <- 1819.72

quantile(cars$enginesize,seq(0,1,0.01))
cars$enginesize[which(cars$enginesize < 90 )] <- 90.00
cars$enginesize[which(cars$enginesize > 209.00)] <- 209.00

quantile(cars$boreratio,seq(0,1,0.01))
#no outliers

quantile(cars$stroke,seq(0,1,0.01))
cars$stroke[which(cars$stroke < 2.6400 )] <- 2.6400

quantile(cars$compressionratio,seq(0,1,0.01))
cars$compressionratio[which(cars$compressionratio > 10.9400)] <- 10.9400

quantile(cars$horsepower,seq(0,1,0.01))
cars$horsepower[which(cars$horsepower > 207.00)] <- 207.00

quantile(cars$peakrpm,seq(0,1,0.01))
#not required jumps are significantly small

quantile(cars$citympg,seq(0,1,0.01))
cars$citympg[which(cars$citympg > 38.00)] <- 38.00

quantile(cars$highwaympg,seq(0,1,0.01))
cars$highwaympg[which(cars$highwaympg > 46.92)] <- 46.92


View(cars)

# creating dummy variables ----

dummy <- model.matrix(~symboling - 1, data = cars)
dummy<-dummy[,-1]
cars_1 <- cbind(cars[,-2],dummy)

levels(cars_1$fueltype)<-c(1,0)
cars_1$fueltype <- as.numeric(cars_1$fueltype)
levels(cars_1$aspiration) <- c(1,0)
cars_1$aspiration <- as.numeric(cars_1$aspiration)
levels(cars_1$doornumber) <- c(4,2)
cars_1$doornumber <- as.numeric(cars_1$doornumber)
levels(cars_1$enginelocation) <- c(1,0)
cars_1$enginelocation <- as.numeric(cars_1$enginelocation)


dummy <- model.matrix(~carbody,data = cars_1)
dummy <- dummy[,-1]
cars_1 <- cbind(cars_1[,-6],dummy)

dummy <- model.matrix(~drivewheel,data = cars_1)
dummy <- dummy[,-1]
cars_1 <- cbind(cars_1[,-6],dummy)

dummy <- model.matrix(~enginetype,data = cars_1)
dummy <- dummy[,-1]
cars_1 <- cbind(cars_1[,-12])

dummy <- model.matrix(~cylindernumber,data = cars_1)
dummy <- dummy[,-1]
cars_1 <- cbind(cars_1[,-12],dummy)

dummy <- model.matrix(~fuelsystem,data = cars_1)
dummy <- dummy[,-1]
cars_1 <- cbind(cars_1[,-13],dummy)

dummy <- model.matrix(~Company,data = cars_1)
dummy <- dummy[,-1]
cars_1 <- cbind(cars_1[,-2],dummy)

#creating new variables ---- 
#as part of car asthetics
cars_1$carWLratio <- cars_1$carwidth/cars_1$carlength
cars_1$carHWratio <- cars_1$carheight/cars_1$carwidth
cars_1$carHLratio <- cars_1$carheight/cars_1$carlength

#as part of performance
cars_1$citytohighwaympg <- cars_1$citympg/cars_1$highwaympg
cars_1$powtoweightratio <- cars_1$horsepower/cars_1$curbweight





#model building ----
set.seed(100)
trainindices= sample(1:nrow(cars_1), 0.7*nrow(cars_1))
train = cars_1[trainindices,]
test = cars_1[-trainindices,]
#considering 205 is big enogh data set to take part of it as training and test

#deleting car index
#Deleting unnecessary columns
train <- train[,-1]

#building first model 
model_1 <- lm(price~.,data = train)
summary(model_1)
#Adjusted R-squared:  0.9674
cor(cars_1)
#view cor to relate the variable and have a brief understanding of relation with variables

#using Step AIC to identify insignificant columns ----
step <- stepAIC(model_1, direction="both")
step

model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                enginesize + stroke + horsepower + peakrpm + carbodywagon + 
                drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                fuelsystemmpfi + Companybmw + Companybuick + Companydodge + 
                Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                Companymercury + Companymitsubishi + Companynissan + Companypeugeot + 
                Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                Companytoyota + Companyvolkswagen + Companyvolvo + carWLratio + 
                carHWratio + carHLratio + powtoweightratio, data = train)


summary(model_2)
#Adjusted R-squared:  0.9722
sort(vif(model_2))

#backward selection method ----
#looking at the VIF and p values for model_2, looks like we can afford to drop fuelsystemmpfi
#VIF 9.022946 P value 0.056578

model_3 <-  lm(formula = price ~ aspiration + enginelocation + carlength + 
                 enginesize + stroke + horsepower + peakrpm + carbodywagon + 
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + Companypeugeot + 
                 Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                 Companytoyota + Companyvolkswagen + Companyvolvo + carWLratio + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_3)
#Adjusted R-squared:  0.9715 
sort (vif (model_3))


#can affor to drop fuelsystem2bbl
#VIF 3.533380 p value 0.500140
model_4 <-  lm(formula = price ~ aspiration + enginelocation + carlength + 
                 enginesize + stroke + horsepower + peakrpm + carbodywagon + 
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + Companypeugeot + 
                 Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                 Companytoyota + Companyvolkswagen + Companyvolvo + carWLratio + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_4)
#Adjusted R-squared:  0.9716 
sort (vif (model_4))


#can afford to drop peakrpm
#VIF  5.199874 p value 0.057343
model_5 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                enginesize + stroke + horsepower +  carbodywagon + 
                drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree  + 
                Companybmw + Companybuick + Companydodge + 
                Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                Companymercury + Companymitsubishi + Companynissan + Companypeugeot + 
                Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                Companytoyota + Companyvolkswagen + Companyvolvo + carWLratio + 
                carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_5)
#Adjusted R-squared:  0.9709
sort(vif(model_5))


#can afford to drop carbodywagon
#p value 0.106391
model_6 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                enginesize + stroke + horsepower +  
                drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree  + 
                Companybmw + Companybuick + Companydodge + 
                Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                Companymercury + Companymitsubishi + Companynissan + Companypeugeot + 
                Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                Companytoyota + Companyvolkswagen + Companyvolvo + carWLratio + 
                carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_6)
#Adjusted R-squared:  0.9705
sort(vif(model_6))


#companyisuzu has least VIF yet going for to drop it because of its insignificance
model_7 <-lm(formula = price ~ aspiration + enginelocation + carlength + 
               enginesize + stroke + horsepower +  
               drivewheelrwd + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + cylindernumberthree  + 
               Companybmw + Companybuick + Companydodge + 
               Companyhonda +  Companyjaguar + Companymazda + 
               Companymercury + Companymitsubishi + Companynissan + Companypeugeot + 
               Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
               Companytoyota + Companyvolkswagen + Companyvolvo + carWLratio + 
               carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_7)
#Adjusted R-squared:  0.9701 
sort(vif(model_7))


#companyvolvo can be afforded to be dropped
model_8 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                enginesize + stroke + horsepower +  
                drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree  + 
                Companybmw + Companybuick + Companydodge + 
                Companyhonda +  Companyjaguar + Companymazda + 
                Companymercury + Companymitsubishi + Companynissan + Companypeugeot + 
                Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                Companytoyota + Companyvolkswagen  + carWLratio + 
                carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_8)
#Adjusted R-squared:  0.9695
sort(vif(model_8))


#can try droping companyhonda, but adjusted r square has to be checked again 
model_9 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                enginesize + stroke + horsepower +  
                drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree  + 
                Companybmw + Companybuick + Companydodge + 
                  Companyjaguar + Companymazda + 
                Companymercury + Companymitsubishi + Companynissan + Companypeugeot + 
                Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                Companytoyota + Companyvolkswagen  + carWLratio + 
                carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_9)
#Adjusted R-squared:  0.9687 little drop but not very huge
sort(vif(model_9))

#as seen adjusted r square hasnt changed much, droping companyhonda has not costed us much

#from model_9 companypeugeot seems to be affordable to be dropped
model_10 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 enginesize + stroke + horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                 Companytoyota + Companyvolkswagen  + carWLratio + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_10)
#Adjusted R-squared:  0.9679
sort(vif(model_10))


#companysabaru can be droped
model_11 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 enginesize + stroke + horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + 
                 Companytoyota + Companyvolkswagen  + carWLratio + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_11)
#Adjusted R-squared:  0.9566 
sort(vif(model_11))

#adjusted r square has dropped by almost 1 , considering this as comparable to previous model adjusted r square value
#carwlratio can be dropped
model_12 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 enginesize + stroke + horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_12)
#Adjusted R-squared:  0.9559
sort(vif(model_12))

#no significant drop in adjusted r square
#enginesize can be dropped
model_13 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 stroke + horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_13)
#Adjusted R-squared:  0.956
sort(vif(model_13))

#adjusted r square increased by 0.001 
#stroke can be removed
model_14 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_14)
#Adjusted R-squared:  0.9562 
sort(vif(model_14))

#aspiration can be removed
model_15 <- lm(formula = price ~ enginelocation + carlength + 
                 horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_15)
#Adjusted R-squared:  0.9566
sort(vif(model_15))

#dropingcylindernumber3
model_16 <- lm(formula = price ~ enginelocation + carlength + 
                 horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_16)
#Adjusted R-squared:  0.956
sort(vif(model_16))

#droping companysaab
model_17 <- lm(formula = price ~ enginelocation + carlength + 
                 horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault  + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_17)
#Adjusted R-squared:  0.9564
sort(vif(model_17))

#removing companynissan
model_18 <- lm(formula = price ~ enginelocation + carlength + 
                 horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + 
                 Companyplymouth + Companyrenault  + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_18)
#Adjusted R-squared:  0.9565 
sort(vif(model_18))

#droping company volkswagen
model_19 <- lm(formula = price ~ enginelocation + carlength + 
                 horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + 
                 Companyplymouth + Companyrenault  + 
                 Companytoyota  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_19)
#Adjusted R-squared:  0.9568 
sort(vif(model_19))

#droping company toyota
model_20 <- lm(formula = price ~ enginelocation + carlength + 
                 horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + 
                 Companyplymouth + Companyrenault  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_20)
#Adjusted R-squared:  0.9558
sort(vif(model_20))

#removing company mazda
model_21 <- lm(formula = price ~ enginelocation + carlength + 
                 horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar  + 
                 Companymercury + Companymitsubishi + 
                 Companyplymouth + Companyrenault  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_21)
#Adjusted R-squared:  0.9551
sort(vif(model_21))

#removing companymercury
model_22 <- lm(formula = price ~ enginelocation + carlength + 
                 horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar  + 
                 Companymitsubishi + 
                 Companyplymouth + Companyrenault  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_22)
#Adjusted R-squared:  0.9548 
sort(vif(model_22))

#removing companyplymouth
model_23 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  + Companybmw + Companybuick + Companydodge + 
                 Companyjaguar  +  Companymitsubishi + Companyrenault  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_23)
#Adjusted R-squared:  0.9546
sort(vif(model_23))

#removing companyrenault
model_24 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  + Companybmw + Companybuick + Companydodge + 
                 Companyjaguar  +  Companymitsubishi  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_24)
#Adjusted R-squared:  0.9536
sort(vif(model_24))

#removing companydodge
model_25 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  + Companybmw + Companybuick +  
                 Companyjaguar  +  Companymitsubishi  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_25)
#Adjusted R-squared:  0.9533
sort(vif(model_25))

#removing companymitsubishi
model_26 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  + Companybmw + Companybuick +  Companyjaguar  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_26)
#Adjusted R-squared:  0.952
sort(vif(model_26))

#removing drivewheelrwd
model_27 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  + Companybmw + Companybuick +  Companyjaguar  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)
summary(model_27)
#Adjusted R-squared:  0.9511


#most variables are significant ie, has p < 0.05


#predicting price using model_27 ----
predict_1 <- predict(model_27,test[,-18])
test$test_price <- predict_1
r <- cor(test$test_price,test$price)
rsquared <- r^2

#we have got r squared as  0.877 which is comparable with our initial models 0.9511 difference of approx 7%
summary(model_27)
#the estimate column gives the betai values intercept is -71548.19
#the adjusted r square and the r square are similar and approximate to each other
#R-squared:  0.9552,	Adjusted R-squared:  0.9511 which goes on to show none of my variables are redundant
#most models attempted had good adjusted r square value, hence alternate model and choosing of best one was not attempted


#white noise check
test$error <- test$price - test$test_price
ggplot(test,aes(x=car_ID,y=error))+geom_point()
#error is scattered and dosenot follow any trend/pattern

#to show that predicted and actual are approximately the same
twoord.plot(lx=test$car_ID,ly = test$price,ry=test$test_price,rx =test$car_ID,ylab = "Price",xlab = "Car_ID")

#conclusion ----
#there are 12 main  variables that predict the car pricing enginelocation,carlength,horsepower, no of cylinders is 4,5 or 6
#Companies like BMW,Buick,Jaguar, and carHeightweight ratio,carheight length ratio, and finally power to weight ratio

#Variables like cylinder number,car height weight ratio and power to weight ratio are negatively dependeant ie, increase 
#in these vairiables decrease the price of cars, other variable are positively dependant, ie they add up for the cost

#depending on these variables the manufacturer can develop his car models for a reasonable pricing