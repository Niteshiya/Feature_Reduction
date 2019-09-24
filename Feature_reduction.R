#Libraries
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

#Data Set
data("Sonar")
data <- Sonar
str(data)

#Feature Selection
set.seed(123)
boruta <- Boruta(Class ~.,data=data,doTrace=2,maxRuns=300)
print(boruta)
plot(boruta,las=2,cex.axis=0.7)
plotImpHistory(boruta)

#Tentetive fix
ten.fix <- TentativeRoughFix(boruta)
ten.fix
attStats(boruta)
#data partition
ind <- sample(2,nrow(data),replace=T,prob=c(0.7,0.3))
train <- data[ind==1,]
test <- data[ind==2,]

#Random Forest model
rf60 <- randomForest(Class~.,data=train)
rf60

#Predict data on Test
p <- predict(rf60,test)
head(p)
confusionMatrix(p,test$Class)
acc_all <- 75.41

#using boruta things
getNonRejectedFormula(boruta)
boruta
rf39 <- randomForest(Class ~ V1 + V4 + V5 + V8 + V9 + V10 + V11 + V12 + V13 + V15 + 
                       V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V26 + V27 + 
                       V28 + V29 + V30 + V31 + V32 + V35 + V36 + V37 + V39 + V42 + 
                       V43 + V44 + V45 + V46 + V47 + V48 + V49 + V51 + V52,train)
rf39
p1 <- predict(rf39,test)
head(p1)
confusionMatrix(p1,test$Class)
acc_non.rejec <- 72.13
#---------------------------------------
boruta
getConfirmedFormula(boruta)
rf33 <- randomForest(Class ~ V1 + V4 + V5 + V9 + V10 + V11 + V12 + V13 + V15 + V16 + 
                       V17 + V18 + V19 + V20 + V21 + V22 + V23 + V26 + V27 + V28 + 
                       V31 + V35 + V36 + V37 + V43 + V44 + V45 + V46 + V47 + V48 + 
                       V49 + V51 + V52,train)
rf33
p2 <- predict(rf33,test)
head(p2)
confusionMatrix(p2,test$Class)
acc_imp <- 75.41
