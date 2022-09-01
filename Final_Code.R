rm(list=ls())
#___________________________________________________________________________________________________
#Importing Data/
library(readr)
Employee_Attrition <- read_csv("Desktop/Capstone Project/Attrition.csv")
View(Employee_Attrition)
#___________________________________________________________________________________________________
Employee_Attrition <- read.csv("C:\\Users\\rabiya\\Downloads\\New folder\\Attrition.csv")
nrow(Employee_Attrition)
ncol(Employee_Attrition)

#---------------Structure and summary of data----------------
str(Employee_Attrition)
summary(Employee_Attrition)


# Install necessary packages

install.packages('ggplot2') # For plotting
install.packages('class')
install.packages("pROC")
install.packages('rpart') # For Decision Tree
install.packages('rpart.plot')
install.packages('caret')
install.packages('nnet') # Forlogistic Regression
install.packages('randomForest') # For Random Forest
install.packages('class')   # For KNN calssifier
install.packages('e1071')  # For naive bayes
install.packages("ROCR")

#___________________________________________________________________________________________________
##Installing necessary libraries
#___________________________________________________________________________________________________

library(ggplot2) # For plotting
library(class)
library("pROC")
library(rpart) # For Decision Tree
library(rpart.plot)
library(caret)
library(nnet) # Forlogistic Regression
library(randomForest) # For Random Forest
library(class)   # For KNN calssifier
library(e1071)  # For naive bayes
library("ROCR")


#___________________________________________________________________________________________________
#Balancing The data
#___________________________________________________________________________________________________

# Class imbalance check
barplot(prop.table(table(Employee_Attrition$Attrition)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")
# Observation : Attrition Yes > Attirition No
# Data in not balanced so We will use UpSampling Techique

#--------------UP Sampling---------------------#
yes<- which(Employee_Attrition$Attrition=="Yes")
No<- which(Employee_Attrition$Attrition=="No")
length(yes)
length(No)
Yes.upsam <- sample(yes, length(No), replace= T)
Employee_Attrition1<- Employee_Attrition[c(No, Yes.upsam),]
table(Employee_Attrition1$Attrition)

#___________________________________________________________________________________________________
#importance of variables in dataset 
#___________________________________________________________________________________________________

####usinng decision tree model to find out the importance###

df<-rpart( Attrition~.,data=Employee_Attrition1,control=rpart.control(minsplit = 10))
df$variable.importance

#other way of doing it by Droping the the columns with no variability.
drop_var<-names(Employee_Attrition1[, nearZeroVar(Employee_Attrition1)])
drop_var #it will show the variable names with zero variability

#Note:As we can notice EmployeeCount, Over18, Standhours has no variable importance


#___________________________________________________________________________________________________
## Cleaning the data
#___________________________________________________________________________________________________

# checking null values in the dataset 
sum(is.na.data.frame(Employee_Attrition1))
# so there are no missing values which is good

# Removing coloumns that are not needed for the analysis
Employee_Attrition1$Over18 <- NULL
Employee_Attrition1$EmployeeCount <- NULL
Employee_Attrition1$StandardHours <- NULL
Employee_Attrition1$EmployeeNumber<-NULL
Employee_Attrition1$DistanceFromHome<-NULL

#------Check the dataset again--------
#View(Employee_Attrition1)
# the coloumns have been removed



#___________________________________________________________________________________________________
#Data Visulization 
#___________________________________________________________________________________________________

table(Employee_Attrition$Attrition)
ggplot(Employee_Attrition, aes(x=Attrition, fill=Attrition)) + geom_bar()

#-------Visulizing Factors having Higher Variability----------#

#1) Age
summary(Employee_Attrition1$Age)
Age_plot <- cut(Employee_Attrition$Age, 8, include.lowest = TRUE)
ggplot(Employee_Attrition, aes(Age_plot, ..count.., 
                        fill = factor(Attrition))) + geom_bar(position="dodge")

#2) Hourly Rate
summary(Employee_Attrition$HourlyRate)
HourlyRate_Plot<- cut(Employee_Attrition$HourlyRate, 7, include.lowest = TRUE)
ggplot(Employee_Attrition, aes(HourlyRate_Plot, ..count..,
                        fill = factor(Attrition))) + geom_bar(position="dodge")


# 3) Job Role
table(Employee_Attrition$JobRole, Employee_Attrition$Attrition)
ggplot(Employee_Attrition, aes(JobRole, ..count.., 
                        fill = factor(Attrition))) + geom_bar(position="dodge")

# 4) Total working years
summary(Employee_Attrition$TotalWorkingYears)
TotalWorkingYears_plot <- cut(Employee_Attrition$TotalWorkingYears,
                              10, include.lowest = TRUE)
ggplot(Employee_Attrition, aes(TotalWorkingYears_plot, ..count.., 
                        fill = factor(Attrition))) + geom_bar(position="dodge")

# 5) Monlthy Income
table(Employee_Attrition$MonthlyIncome, Employee_Attrition$Attrition)
ggplot(Employee_Attrition, aes(MonthlyIncome, ..count.., 
                      fill = factor(Attrition))) + geom_bar(position="dodge")

#  6) Overtime
table(Employee_Attrition$OverTime, Employee_Attrition$Attrition)
ggplot(Employee_Attrition, aes(OverTime, ..count.., 
                      fill = factor(Attrition))) + geom_bar(position="dodge")
#___________________________________________________________________________________________________
#Balancing The data
#___________________________________________________________________________________________________

# Class imbalance check
barplot(prop.table(table(Employee_Attrition$Attrition)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")
# Observation : Attrition Yes > Attirition No
# Data in not balanced so We will use UpSampling Techique

#--------------UP Sampling---------------------#
yes<- which(Employee_Attrition$Attrition=="Yes")
No<- which(Employee_Attrition$Attrition=="No")
length(yes)
length(No)
Yes.upsam <- sample(yes, length(No), replace= T)
Employee_Attrition1<- Employee_Attrition[c(No, Yes.upsam),]
table(Employee_Attrition1$Attrition)

#Prparing the data for ML Algorithms
#___________________________________________________________________________________________________

# convert certain integer variable to factor variable  as they make more sense as factor data type
convert_factor<- c("Education", "EnvironmentSatisfaction", "JobInvolvement", 
                   "JobLevel","Department","EducationField","JobSatisfaction", 
                   "PerformanceRating", "RelationshipSatisfaction",
                   "StockOptionLevel","TrainingTimesLastYear","WorkLifeBalance",
                   "BusinessTravel","Gender","JobRole","MaritalStatus","OverTime" )

Employee_Attrition1[, convert_factor] <- lapply((Employee_Attrition1
                                                 [, convert_factor]), as.factor)
Employee_Attrition1[, convert_factor] <- lapply((Employee_Attrition1
                                                 [, convert_factor]), unclass)

#___________________________________________________________________________________________________
#Data partition
#___________________________________________________________________________________________________
#Dividing the data into test and train
p <- 0.30
set.seed(123)
ind<- sample(c(rep(0, (1-p) * nrow(Employee_Attrition1)), rep(1, p * nrow(Employee_Attrition1))))
X1 <-Employee_Attrition1[ind==0,]  #Training
X2 <- Employee_Attrition1[ind==1,]  #Test

#___________________________________________________________________________________________________
#Machine learning Models
#___________________________________________________________________________________________________

#=======================
# 1) Decision Tree Model
#=======================

DT_model<- rpart(Attrition~., data = X1, method = 'class')
rpart.plot(DT_model, extra = 106)

# Prediction
ypred <- predict(DT_model, newdata= X2, type ="class")

#Confusion Matrix of Decision Tree
T <- table(X2$Attrition, ypred)
#Prediction Accuracy
acu<- sum(diag(T))/ sum(T) *100

#Prediction Error
Pre <- 100- acu

# Percision
pe <- T[1,1]/sum(T[,1])

#Recall
Re <- T[1,1]/sum(T[1,])

cat("For Desicion Tree","\n", "Accuracy =", acu,"\n", "Prediction Error=", Pre,
"\n", "Percision=", pe ,"\n", "Recall= ", Re, "\n")
 
#Predict the probability For test data
Prob_DT <- predict(DT_model, newdata= X2[,-2], type ="prob")
Prob_DT

#AUC value
AUC <- auc(X2$Attrition,Prob_DT[,2])
AUC

# Roc curve
plot(roc(X2$Attrition,Prob_DT[,2]), main="ROC Curve of Decision Tree",col="blue")


#=======================
# 2) Logistic Regression
#=======================

set.seed(123)
Logistic_Model <- glm(as.factor(Attrition)~., data=X1, family = 'binomial')
pred <- predict(Logistic_Model, newdata = X2, type="response")

#Predictions
test <- vector()
test[pred > mean(Logistic_Model$y==1)] <- "Yes"
test[pred<= mean(Logistic_Model$y==0)] <- "No"

#Confusion matrix
T1 <- table(test, X2[,2])

#Prediction Accuracy
acu1 <- 100*sum(diag(T1))/sum(T1)

# Prediction Error
Pre1 <- 100-acu1

# Percision
pe1 <- T1[1,1]/sum(T1[,1])

#Recall
Re1 <- T1[1,1]/sum(T1[1,])

cat("For Logistic Regression","\n", "Accuracy =", acu1,"\n", "Prediction Error=", Pre1,
"\n", "Percision=", pe1 ,"\n", "Recall= ", Re1, "\n")

#ROC and AUC
pred_resp <- predict(Logistic_Model ,X2,type="response")
pred <- prediction(pred_resp, X2[,2])
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#
AUC1<-auc(X2[,2], pred_resp)

#==================
# 3) Random Forest
#==================

model_RF <- randomForest(x= X1[,-2], y=as.factor(X1[,2]),
                         ntree=500,
                         mtry=2,
                         importance=TRUE,
                         replace=FALSE)

# Predicting the Test set results
y_pred = predict(model_RF, newdata = X2[-2])


#Confusion matrix
T2 <- table(y_pred, X2[,2])

#Prediction Accuracy
acu2 <- 100*sum(diag(T2))/sum(T2)

# Prediction Error
Pre2 <- 100-acu2

# Percision
pe2 <- T2[1,1]/sum(T2[,1])

#Recall
Re2 <- T2[1,1]/sum(T2[1,])


cat("For Random forest","\n", "Accuracy =", acu2,"\n", "Prediction Error=", Pre2,
"\n", "Percision=", pe2 ,"\n", "Recall= ", Re2, "\n")

# Generate a textual view of the Random Forest
model_RF
summary(model_RF)
plot(model_RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest")

# ROC and AUC

Prob_RF <- (predict(model_RF, X2, type = 'prob'))
#AUC value
AUC2 <- auc(X2$Attrition,Prob_RF[,2])
AUC2
# Roc curve
plot(roc(X2$Attrition,Prob_RF[,2]), main="ROC Curve of Random Forest",col="Green")



#=============
#4) KNN
#=============


x1<- X1[,-2]
x2<- X2[,-2]

KN<-knn(train=x1, test= x2, cl=X1[,2], k = 3)


#Confusion matrix
T3 <- table( KN, X2[,2])

#Prediction Accuracy
acu3 <- 100*sum(diag(T3))/sum(T3)

# Prediction Error
Pre3 <- 100-acu3

# Percision
pe3 <- T3[1,1]/sum(T3[,1])

#Recall
Re3 <- T3[1,1]/sum(T3[1,])


cat("For KNN Classifier","\n", "Accuracy =", acu3,"\n", "Prediction Error=", Pre3,
"\n", "Percision=", pe3 ,"\n", "Recall= ", Re3, "\n")


#AUC and ROC

KN_prob<-knn(train=x1, test= x2, cl=X1[,2], k = 3, prob= T)

prob <- attr(KN_prob, "prob")
prob <- 2*ifelse(KN_prob == "-1", 1-prob, prob) - 1

pred_knn <- prediction(prob, X2[,2])
pred_knn <- performance(pred_knn, "tpr", "fpr")
plot(pred_knn, col="blue", lwd=3, main="ROC curve Using KNN Classifier")
AUC3<-auc(X2[,2], prob)


#==============
#5)Naive Bayes
#==============

classifier_cl <- naiveBayes( X1[,2]~ ., data = X1)

 
# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = X2[,-2])

#Confusion matrix
T4 <- table( y_pred, X2[,2])

#Prediction Accuracy
acu4 <- 100*sum(diag(T4))/sum(T4)

# Prediction Error
Pre4 <- 100-acu4

# Percision
pe4 <- T4[1,1]/sum(T4[,1])

#Recall
Re4 <- T4[1,1]/sum(T4[1,])


cat("For Naive bayes","\n", "Accuracy =", acu4,"\n", "Prediction Error=", Pre4,
"\n", "Percision=", pe4 ,"\n", "Recall= ", Re4, "\n")


#AUC and ROC

y_pred_prob <- predict(classifier_cl, newdata = X2[,-2], type = 'raw')
#AUC value
AUC4 <- auc(X2$Attrition,y_pred_prob[,2])
AUC4
# Roc curve
plot(roc(X2$Attrition,y_pred_prob[,2]), main="ROC Curve of Navie bayes",col="green")

# combain Roc  Curve
par(mfrow=c(2,3))
plot(roc(X2$Attrition,Prob_DT[,2]), main="ROC Curve of Decision Tree",col="blue")
plot(perf, colorize=TRUE, main= "ROC Curve for Logistic regression")
plot(roc(X2$Attrition,Prob_RF[,2]), main="ROC Curve of Random Forest",col="Green")
plot(pred_knn, col="blue", lwd=3, main="ROC curve Using KNN Classifier")
plot(roc(X2$Attrition,y_pred_prob[,2]), main="ROC Curve of Navie bayes",col="green")




# Combine table
acuracy <- c(acu,acu1,acu2,acu3,acu4)
Prediction_error<- c(Pre,Pre1, Pre2, Pre3, Pre4)
Percision <- c(pe, pe1, pe2, pe3, pe4)
Recall <- c(Re, Re1, Re2, Re3, Re4)
AUCC <- c(AUC, AUC1, AUC2, AUC3, AUC4)
c.table <- cbind(acuu, Prediction_error ,Percision,Recall ,AUCC)
rownames(c.table)<- c("Decision Tree", "Logistic Regression", "Random Forest","KNN","Naive bayes")


# From the table It can be observed that accuracy of Random forest is Highest,
#Prediction_error is lowest and other measures are also good so we use Random forest
#for further predictions


#_______________________________________________
#Final Predictions On real data
#_______________________________________________

y_predF = predict(model_RF, newdata = Employee_Attrition[-2])


#Confusion matrix
Tf <- table(y_predF, Employee_Attrition[,2])

#Prediction Accuracy
acuF <- 100*sum(diag(Tf))/sum(Tf)

# Prediction Error
Pref <- 100-acuF

# Percision
peF <- Tf[1,1]/sum(Tf[,1])

#Recall
Ref <- Tf[1,1]/sum(Tf[1,])


cat("Final Predictions","\n", "Accuracy =", acuF,"\n", "Prediction Error=", Pref,
"\n", "Percision=", peF ,"\n", "Recall= ", Ref, "\n")




