#' Instructor: Ted Kwartler
#' Date: 08-01-2018 Wednesday
#' Group Assignment: Group 3
#' Members: Yuting Li, Yifan Kang, Huaiqian Ye and Abhay Kavi Prakash 

#Clearing environment
rm(list=ls(all=TRUE))

#Set Working Directory
setwd(" Have all the requires csv files in this folder only as we only do this once ")

#Libraries
library(vtreat)
library(lubridate)
library(ggplot2)
library(caret)
library(rpart.plot)
library(randomForest)
library(MLmetrics)
library(e1071)
library(plyr)
library(stats)
library(ggthemes)
library(dplyr)
options(scipen=999)

#### pre-processing data ####
A<-read.csv('CurrentCustomerMktgResults.csv')
B<-read.csv('householdAxiomData.csv')
C<-read.csv('householdCreditData.csv')
D<-read.csv('householdVehicleData.csv')
trainingIDs<-unique(A$HHuniqueID)
idx<-which(B$HHuniqueID %in% trainingIDs)
tB<-B[idx,]
idxc<-which(C$HHuniqueID %in% trainingIDs)
tC<-C[idxc,]
idxd<-which(D$HHuniqueID %in% trainingIDs)
tD<-D[idxd,]
trainingDF<-merge(A,tB,by='HHuniqueID')
trainingDF<-merge(trainingDF,tC,by='HHuniqueID')
trainingDF<-merge(trainingDF,tD,by='HHuniqueID')
rm(tB,tC,tD)


attach(trainingDF)
levels(Communication)<-c(levels(Communication),'unknown')
Communication[is.na(Communication)]<-'unknown'
trainingDF$Communication<-Communication
levels(Education)<-c(levels(Education),'unspecified')
Education[is.na(Education)]<-'unspecified'
trainingDF$Education<-Education
levels(Job)<-c(levels(Job),'Non_given')
Job[is.na(Job)]<-'Non_given'
trainingDF$Job<-Job
carYr<-as.numeric(carYr)
carYr[is.na(carYr)]<-round(mean(carYr[is.na(carYr)==FALSE]))
trainingDF$carYr<-carYr
detach()
trainingDF<-trainingDF[,-c(28,15,4)]


#### processing with Vtreat function ####
informativeVars <- names(trainingDF)[-c(1,2,9,10,11,13)]
target<-names(trainingDF)[11]
plan <- designTreatmentsC(trainingDF, 
                          informativeVars,
                          target,1)
TT <- prepare(plan, trainingDF)

##calculate time
CS<-hms(trainingDF$CallStart)
CE<-hms(trainingDF$CallEnd)
DUR<-CE-CS
calltime<-as.duration(DUR)
TT<-cbind(TT,trainingDF[,c(1,13)])
TT<-cbind(TT,calltime)

##removed the catP and catB variables as we have the categories already
TT<-TT[,-c(1,2,3,4,8,9,14:19,24,25,40,45)]
TT$Y_AccetpedOffer<-as.factor(TT$Y_AccetpedOffer)

##fill the blanks in TT$annualDonation with 0
TT$annualDonations<-as.numeric(TT$annualDonations)
TT$annualDonations[is.na(TT$annualDonations)]<-0


####start of EDAs####

##distribution of calltime for successed and faied customers
TT$calltime <- as.numeric(TT$calltime)
plotit <- TT[,c(64,67)]
plotit$calltime <- ifelse(plotit$calltime>=2000,2000,plotit$calltime)
mu <- ddply(plotit, "Y_AccetpedOffer", summarise, grp.mean=mean(calltime))
ggplot(plotit,aes(x=calltime,color=Y_AccetpedOffer, fill=Y_AccetpedOffer))+
  geom_histogram(aes(y=..density..),alpha=0.5,
                 position = "identity",binwidth = 20)+
  geom_density(alpha=.2)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Y_AccetpedOffer),
             linetype="dashed")

##Plot out different car makers
CarMlist<-unique(trainingDF$carMake,incomparables = FALSE,fromLast = FALSE, MARGIN = 1)
SucRate<-data.frame(rep(0,nlevels(CarMlist)))
CountN<-data.frame(rep(0,nlevels(CarMlist)))
for(i in 1:nrow(SucRate)){
  temp<- trainingDF[which(trainingDF$carMake==CarMlist[i]),]
  SucRate[i,]=round(sum(temp$Y_AccetpedOffer)/nrow(temp),2)
  CountN[i,]=nrow(temp)
}
SucRate<-SucRate[is.na(SucRate)==0,]
data.frame(CountN)
CountN<-CountN[-which(CountN==0),]
Catgories<-data.frame(CarMlist,SucRate,CountN)
new<-Catgories[order(SucRate),]
new$CarMlist<-factor(new$CarMlist, levels=new$CarMlist)
ggplot(new, aes(y=CarMlist)) +
  geom_point(aes(x=SucRate), data=new, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=SucRate,label=SucRate), colour="red",hjust=-.25, size=3)


########################################## significance test of CarMakers
phat <- (Catgories$SucRate * Catgories$CountN + 1604)/(4000 + Catgories$CountN)
zvalue <- (Catgories$SucRate - 0.401)/sqrt(phat*(1-phat)*((Catgories$CountN)^(-1)+(4000)^(-1)))
carp.df <- data.frame(p=pnorm(zvalue,mean = 0,sd = 1),CarMak=CarMlist)
carp.df$CarMak[head(order(carp.df$p,decreasing = F),16)]
#Catgories[which(pnorm(zvalue,mean=0,sd=1)<=0.2),1]

## Similar Analysis of (MONTH)
countMon<- data.frame(rep(0,nlevels(trainingDF$LastContactMonth)))
Monlist<-unique(trainingDF$LastContactMonth,incomparables = FALSE,fromLast = FALSE, MARGIN = 1)
Monlist1<- factor(Monlist,order=TRUE,levels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
Monlist2<-sort(Monlist1)
SucRateMon<-data.frame(rep(0,nlevels(trainingDF$LastContactMonth)))
for(i in 1:nrow(SucRateMon)){
  temp<- trainingDF[which(trainingDF$LastContactMonth==Monlist[i]),]
  countMon[i,]<- dim(temp)[1]
  SucRateMon[i,]=round(sum(temp$Y_AccetpedOffer)/nrow(temp),2)
}
countMon
SucRateMon<-SucRateMon[is.na(SucRateMon)==0,]
Catgories<-data.frame(Monlist2,SucRateMon)
newMon<-Catgories[order(SucRateMon),]
newMon$Monlist2<-factor(newMon$Monlist2, levels=newMon$Monlist2)
ggplot(newMon, aes(y=Monlist2)) +
  geom_point(aes(x=SucRateMon), data=newMon, col='#2F99E0') +
  theme_gdocs() + 
  geom_text(aes(x=SucRateMon,label=SucRateMon), colour="#2F99E0",hjust=-.25, size=3)

##factors irrelevant droped
TT$headOfhouseholdGender_lev_x_F<-NULL
TT$Communication_lev_NA<-NULL
TT$Communication_lev_x_cellular<-NULL
TT$Communication_lev_x_telephone<-NULL
TT$calltime<-NULL

####modellings####

##partition
set.seed(2018)
trainPercent <- round(nrow(TT) %*% .6)
validPercent <- round(nrow(TT) %*% .2)
trainIdx <- sample(1:nrow(TT), trainPercent)
remainRows <- setdiff(1:nrow(TT),trainIdx)
validIdx <- sample(remainRows,validPercent)
trainDat <- TT[trainIdx, ]
validDat <- TT[validIdx, ]
testDat <- TT[-c(trainIdx,validIdx),]

##Random forest
trainDatRF <- trainDat[,-62]
RF_Fit <- train(Y_AccetpedOffer~ .,
                       data = trainDatRF,
                       method = "rf",
                       verbose = FALSE,
                       ntree = 128)

predProbs <- predict(RF_Fit, trainDatRF, type = c("prob"))
predClasses <-predict(RF_Fit, trainDatRF)

RF_Fit
varImp(RF_Fit)
plot(varImp(RF_Fit), top = 20,col='#A51C30')

trainClass_RF<-predict(RF_Fit, trainDatRF)
confusionMatrix(trainClass_RF, trainDatRF$Y_AccetpedOffer)
testClass_RF<-predict(RF_Fit, validDat)
confusionMatrix(testClass_RF,validDat$Y_AccetpedOffer)


##KNN model

knnFit <- train(Y_AccetpedOffer ~ . -HHuniqueID, 
                data = trainDat, 
                method = "knn", 
                preProcess = c("center","scale"))

knnFit
plot(knnFit)

trainPreds.kNN<-predict(knnFit,trainDat)
confusionMatrix(trainPreds.kNN, trainDat$Y_AccetpedOffer)
testClass_KNN<-predict(knnFit, validDat)
confusionMatrix(testClass_KNN,validDat$Y_AccetpedOffer)

##decision tree
DTfit <- rpart(Y_AccetpedOffer ~. -HHuniqueID, 
             data = trainDat, method = "class",  
             control = rpart.control(minsplit = 1, minbucket = 2),cp=c(0.01,0.05))

prp(DTfit,extra=1)
trainPreds.DT<-predict(DTfit,trainDat, type = c("prob"))
trainPreds.DT<-predict(DTfit,trainDat, type = c("class"))
confusionMatrix(trainPreds.DT, trainDat$Y_AccetpedOffer)
testClass_DT<-predict(DTfit, validDat,type = c("class"))
confusionMatrix(testClass_DT,validDat$Y_AccetpedOffer)

##logistic regression
train.lm <- glm(Y_AccetpedOffer~. -HHuniqueID,data=trainDat,family = "binomial")
summary(train.lm)
glm.step<- step(train.lm,direction = "both")
summary(glm.step)

trainPreds.log<-predict(glm.step,trainDat,type = 'response')
confusionMatrix(factor(ifelse(trainPreds.log>0.5,1,0)),factor(trainDat$Y_AccetpedOffer))
testPreds.log<-predict(glm.step,validDat,type = 'response')
confusionMatrix(factor(ifelse(testPreds.log>0.5,1,0)),factor(validDat$Y_AccetpedOffer))


####After deciding on random forest we apply the trained model to the test set####

Final_model<-predict(RF_Fit, testDat)
confusionMatrix(Final_model,testDat$Y_AccetpedOffer)

####After test trial we apply the model in the real potential customers####

rm(list = setdiff(ls(), c("B","C","D","RF_Fit","plan")))
E<-read.csv('ProspectiveCustomers.csv')
predictIDs<-unique(E$HHuniqueID)
idx<-which(B$HHuniqueID %in% predictIDs)
tB<-B[idx,]
idxc<-which(C$HHuniqueID %in% predictIDs)
tC<-C[idxc,]
idxd<-which(D$HHuniqueID %in% predictIDs)
tD<-D[idxd,]
predictDF<-merge(E,tB,by='HHuniqueID')
predictDF<-merge(predictDF,tC,by='HHuniqueID')
predictDF<-merge(predictDF,tD,by='HHuniqueID')
rm(tB,tC,tD)

attach(predictDF)
levels(Communication)<-c(levels(Communication),'unknown')
Communication[is.na(Communication)]<-'unknown'
predictDF$Communication<-Communication
levels(Education)<-c(levels(Education),'unspecified')
Education[is.na(Education)]<-'unspecified'
predictDF$Education<-Education
levels(Job)<-c(levels(Job),'Non_given')
Job[is.na(Job)]<-'Non_given'
predictDF$Job<-Job
carYr<-as.numeric(carYr)
carYr[is.na(carYr)]<-round(mean(carYr[is.na(carYr)==FALSE]))
predictDF$carYr<-carYr
detach()

TT <- prepare(plan, predictDF)
TT$HHuniqueID <- B[4001:5000,1]
TT$annualDonations <- B[4001:5000,3]
##removed the catP and catB variables as we have the categories already
TT<-TT[,-c(1:4,8,9,14:19,24,25,40,45)]
TT$Y_AccetpedOffer<-as.factor(TT$Y_AccetpedOffer)

##fill the blanks in TT$annualDonation with 0
TT$annualDonations<-as.numeric(TT$annualDonations)
TT$annualDonations[is.na(TT$annualDonations)]<-0

##factors irrelevant droped
TT$headOfhouseholdGender_lev_x_F<-NULL
TT$Communication_lev_NA<-NULL
TT$Communication_lev_x_cellular<-NULL
TT$Communication_lev_x_telephone<-NULL

# run the model 
FinalPred<-predict(RF_Fit, TT)
predProbs <- predict(RF_Fit, TT, type = c("prob"))
predClasses <-predict(RF_Fit, TT)
result <- data.frame(predProbs[,2])
result$HHuniqueID <- TT[,62]
cust <- head(order(result$predProbs...2.,decreasing = T),100)
ids <- result$HHuniqueID[cust]

####Obtainig information on finalized customers####
ids<-data.frame(ids)
prosIDs<-unique(E$HHuniqueID)
idx<-which(E$HHuniqueID %in% ids$ids)
pros<-E[idx,]

####Get the file in csv format which the bank can actually understand
####write.csv(pros, file = "C:/ mention the destination address....... /filename.csv")
