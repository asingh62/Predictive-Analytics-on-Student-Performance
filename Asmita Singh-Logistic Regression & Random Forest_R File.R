library(tidyverse)
library(GGally)
library(corrplot)
library(ISLR)
require(caTools)
library(pscl)
library(randomForest)
setwd("C:/Users/Amrita/Documents/GMU/STAT 515/Final Project")
studentmath.data<-read.csv(file='student-math.csv')
studentpor.data<-read.csv(file='student-por.csv')
students.data=merge(studentmath.data,studentpor.data,all=TRUE)


#****************Logistic Regression**********************************#

#Create dichotomous variable for overall grade G3
students.data$pooroverallgrade <- NULL
students.data$pooroverallgrade <- factor(ifelse(students.data$G3 <= 10, 1, 0))
                                         
#Create dichotomous variable for family size                       
students.data$largefamily<-NULL
students.data$largefamily <- factor(ifelse(students.data$famsize == "GT3", 1, 0))
                                     
#Create dichotomous variable for Parental Status
students.data$SeperatedParents<-NULL
students.data$SeperatedParents<- factor(ifelse(students.data$Pstatus == "A", 1, 0))

#Create dichotomous variable for Mother's level of Education                                   
students.data$welleducatedmother<-NULL
students.data$welleducatedmother <- factor(ifelse(students.data$Medu == 4, 1, 0))

#Create dichotomous variable for Mother's job type                                      
students.data$workingmother<-NULL
students.data$workingmother <- factor(ifelse(students.data$Mjob != "at_home", 1, 0))

#Create dichotomous variable for father's level of education                                  
students.data$welleducatedfather<-NULL
students.data$welleducatedfather <- factor(ifelse(students.data$Fedu == 4, 1, 0))
                                          
#Create dichotomous variable for father's job type
students.data$workingfather<-NULL
students.data$workingfather <- factor(ifelse(students.data$Fjob != "at_home", 1, 0))
                                      
#Create dichotomous variable for travel time
students.data$hightraveltime<-NULL
students.data$hightraveltime <- factor(ifelse(students.data$traveltime >= 3, 1, 0))
                                           
#Create dichotomous variable for study time
students.data$lessstudytime<-NULL
students.data$lessstudytime <- factor(ifelse(students.data$studytime <= 3, 1, 0))
                                      
#Create dichotomous variable for failures
students.data$highpastfailures<-NULL
students.data$highpastfailures <- factor(ifelse(students.data$failures >= 3, 1, 0))
                                     
#Create dichotomous variable for family relationships
students.data$poorfamilyrel<-NULL
students.data$poorfamilyrel <- factor(ifelse(students.data$famrel <= 3, 1, 0))
                                         
#Create dichotomous variable for free time
students.data$lessfreetime<-NULL
students.data$lessfreetime <- factor(ifelse(students.data$freetime <=3, 1, 0))
              
#Create dichotomous variable for social life
students.data$highsociallife<-NULL
students.data$highsociallife <- factor(ifelse(students.data$goout >=3, 1, 0))
                                
#Create dichotomous variable for alcohol consumption
students.data$highalcholconsumption<-NULL
students.data$highalcoholconsumption <- factor(ifelse(students.data$Dalc >= 3, 1, 0))
                                       
#Create dichotomous variable for health
students.data$poorhealth<-NULL
students.data$poorhealth <- factor(ifelse(students.data$health <= 3, 1, 0))
                                    
#Create dichotomous variable for absenteeism
students.data$highabsenteeism<-NULL
students.data$highabsenteeism <- factor(ifelse(students.data$absences > 15, 1, 0))
                                 
#Create dichotomous variable for first period grade
students.data$lowG1grade<-NULL
students.data$lowG1grade <- factor(ifelse(students.data$G1 <=10, 1, 0))

#Create dichotomous variable for second period grade 
students.data$lowG2grade<-NULL
students.data$lowG2grade <- factor(ifelse(students.data$G2 <=10, 1, 0))

#(a)Using the whole data to develop the model
#Using all the above dichotomous variables to develop the model 
Students.glm<-glm(pooroverallgrade~largefamily+SeperatedParents+welleducatedmother+workingmother+welleducatedfather+workingfather+hightraveltime+lessstudytime+highpastfailures+poorfamilyrel+lessfreetime+highsociallife+highalcoholconsumption+poorhealth+highabsenteeism+lowG1grade+lowG2grade,
                  data=students.data,family=binomial)
summary(Students.glm)
#Determining Analysis of Variance(ANOVA)
anova(Students.glm)
#Determining coefficients of the model
coef(Students.glm)
summary(Students.glm)$coef[,4]
#Calculate McFadden's psuedo R sqaured to assess the model fit
pR2(Students.glm)

#Predicting the probabilities 
Studentglm.probs=predict(Students.glm,type="response")
summary(Studentglm.probs)
#Compute the average prediction for each of the true outcomes
tapply(Studentglm.probs, students.data$pooroverallgrade, mean)
#Confusion Matrix for a threshold of 0.5
table(students.data$pooroverallgrade, Studentglm.probs > 0.5)

#(b)Developing the model by splitting data into train and test sets
#  set seed to ensure you always have same random numbers generated
set.seed(123)   
# splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
sample = sample.split(students.data,SplitRatio = 0.7) 
# creates a training dataset named train with rows which are marked as TRUE
train =subset(students.data,sample ==TRUE) 
test=subset(students.data, sample==FALSE)
#Creating model using the dichotomous variables of the train set
Students.glm<-glm(pooroverallgrade~largefamily+SeperatedParents+welleducatedmother+workingmother+welleducatedfather+workingfather+hightraveltime+lessstudytime+highpastfailures+poorfamilyrel+lessfreetime+highsociallife+highalcoholconsumption+poorhealth+highabsenteeism+lowG1grade+lowG2grade,data=train,family=binomial)
#Determining if the model fits well on the test set
summary(Students.glm)
Studentglm.probs=predict(Students.glm,test,type="response")
#Predicting Probabilities
Studentglm.probs <- ifelse(Studentglm.probs > 0.5,1,0)
#Determining Analysis of Variance(ANOVA)
anova(Students.glm)
#Determining the accuracy of fit 
misClasificError <- mean(Studentglm.probs != test$pooroverallgrade)
print(paste('Accuracy',1-misClasificError))
#Calculate McFadden's psuedo R sqaured to assess the model fit
pR2(Students.glm)

#Confusion Matrix for a threshold of 0.5
table(test$pooroverallgrade, Studentglm.probs > 0.5)
#Compute the average prediction for each of the true outcomes
tapply(Studentglm.probs, test$pooroverallgrade, mean)

#(b)Developing the model using significant predictors
set.seed(123)   
# splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
sample = sample.split(students.data,SplitRatio = 0.7) 
# creates a training dataset named train with rows which are marked as TRUE
train =subset(students.data,sample ==TRUE) 
test=subset(students.data, sample==FALSE)
#Creating model using the dichotomous variables of the train set
Students.glm<-glm(pooroverallgrade~hightraveltime+highpastfailures+lowG1grade+lowG2grade,data=train,family=binomial)
summary(Students.glm)
#Determining if the model fits well on the test set
Studentglm.probs=predict(Students.glm,test,type="response")
#Predicting Probabilities
Studentglm.probs <- ifelse(Studentglm.probs > 0.5,1,0)
#Determining Analysis of Variance(ANOVA)
anova(Students.glm)
#Determining the accuracy of fit 
misClasificError <- mean(Studentglm.probs != test$pooroverallgrade)
print(paste('Accuracy',1-misClasificError))
#Calculate McFadden's psuedo R sqaured to assess the model fit
pR2(Students.glm)


#===========================================================================#

###Random Forest ####

# Create a random forest with 500 trees for mtry=6
set.seed(123)
sample = sample.split(students.data,SplitRatio = 0.7) 
# creates a training dataset named train with rows which are marked as TRUE
train =subset(students.data,sample ==TRUE) 
test=subset(students.data, sample==FALSE)
#Create random forest with 500 trees and mtry=6
rf.Studentsdata1<-randomForest(G3~Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+Dalc+health+absences+G1+G2,data=train,mtry=6,importance=TRUE, na.action=na.exclude)
rf.Studentsdata1
#Predicting on Test set 
Studentspredict1.rf = predict(rf.Studentsdata1,test,type="class")
Grades.test=test$G3
mean((Studentspredict1.rf-Grades.test)^2)

# Print and plot the variable-importance measures
importance(rf.Studentsdata1)
varImpPlot(rf.Studentsdata1)

# Increase mtry to 8
set.seed(123)
rf.Studentsdata2<-randomForest(G3~Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+Dalc+health+absences+G1+G2,data=train,mtry=8,importance=TRUE, na.action=na.exclude)
rf.Studentsdata2
#Predicting on test set 
Studentspredict2.rf = predict(rf.Studentsdata2,test,type="class")
mean((Studentspredict2.rf-Grades.test)^2)

# Increase mtry to 13
set.seed(123)
rf.Studentsdata3<-randomForest(G3~Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+Dalc+health+absences+G1+G2,data=train,mtry=13,importance=TRUE, na.action=na.exclude)
rf.Studentsdata3
#Predicting on test set 
Studentspredict3.rf = predict(rf.Studentsdata3,test,type="class")
mean((Studentspredict3.rf-Grades.test)^2)

# Increase mtry to 12
set.seed(123)
rf.Studentsdata4<-randomForest(G3~Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+Dalc+health+absences+G1+G2,data=train,mtry=12,importance=TRUE, na.action=na.exclude)
rf.Studentsdata4
#Predicting on test set 
Studentspredict4.rf = predict(rf.Studentsdata4,test,type="class")
mean((Studentspredict4.rf-Grades.test)^2)


# Create a random forest with the four most important variables
# and let mtry=2
students.data2 <- dplyr::select(students.data,G3,G1,G2,absences,failures)
set.seed(123)
sample = sample.split(students.data2,SplitRatio = 0.7) 
# creates a training dataset named train with rows which are marked as TRUE
train2 =subset(students.data2,sample ==TRUE) 
test2=subset(students.data2, sample==FALSE)
#train = sample(1:nrow(students.data2), nrow(students.data2)/2)
rf.Studentsdata4=randomForest(G3~G1+G2+absences+failures,data=train2,
                        mtry=2,importance=TRUE)
rf.Studentsdata4
Studentspredict4.rf = predict(rf.Studentsdata4,test2,type="class")
Grades.test2=test2$G3
mean((Studentspredict4.rf -Grades.test2)^2)

##########################################################################


