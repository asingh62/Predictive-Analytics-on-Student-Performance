library(ggplot2)
library(corrplot)
library(dplyr)
library(GGally)
library(caTools)
library(coefplot)
library(rpart)
library(rpart.plot)
library(caret)
library(rattle)
setwd("C:/Users/Asmita Singh/Documents/GMU/STAT 515/Files")
studentmath.data<-read.csv(file='student-math.csv')
studentpor.data<-read.csv(file='student-por.csv')
students.data=merge(studentmath.data,studentpor.data,all=TRUE)
head(students.data)

# Check for NA values
any(is.na(students.data))

# Check the datatypes for categorical variables
str(students.data)

# Summary statistics of the data
summary(students.data)

#****************Exploratory Data Analysis**********************************#
# Store only numeric columns
num.cols <- sapply(students.data, is.numeric)

# Filter to numeric columns for correlation
correlation <- round(cor(students.data[sapply(students.data, is.numeric)],use="complete.obs",method="pearson"),4)
correlation

# Plot for data
corrplot::corrplot(correlation, type = "full", order = "hclust", 
         tl.col = "black")

# GGPLOT Pair plot
ggpairs(students.data, columns = c("age", "Medu", "Fedu", "traveltime", "failures", "studytime", "paid", "absences", "G1", "G2", "G3"))

# Histogram for response variable G3
ggplot(students.data, aes(x=G3))+
    geom_histogram(size=1, color = "black", binwidth = 1, fill="#43a2ca") +
    ggtitle("Histogram for Response Variable G3")

# Realtionship between G3 and G1, G2
ggplot(students.data,aes(x=G1,y=G3)) +
    geom_point() + geom_smooth(method = 'lm') +
    ggtitle("Relationship between response Variable G3 and G1")

ggplot(students.data,aes(x=G2,y=G3)) +
    geom_point() + geom_smooth(method = 'lm') +
    ggtitle("Relationship between response Variable G3 and G2")

# Histogram for failures
ggplot(students.data,aes(x=failures))+
    geom_histogram(size=1, color = "black", binwidth = 1,fill="#43a2ca") +
    ggtitle("Histogram for Failures")

# Histogram for absences
ggplot(students.data,aes(x=absences))+
    geom_histogram(size=1, color = "black", binwidth = 2,fill="#43a2ca") +
    ggtitle("Histogram for Absences")

#****************Linear Regression**********************************#
# Set the seed
set.seed(101)

# Split the data into test and train sets
data.split <- sample.split(students.data$age, SplitRatio = 0.7)

# Training data
train <- subset(students.data,data.split==TRUE)

# Test data
test <- subset(students.data,data.split==FALSE)

# Linear Regression for G3 on predictor variables (train the model)
model.lm <- lm(G3~famsize+Pstatus+Medu+Mjob+Fedu+Fjob+traveltime+
                   studytime+failures+famrel+freetime+Dalc+Walc+health+absences+G1+G2, train)
summary(model.lm)

# Accuracy of the model with all numeric predictors

# Residual standard error or sigma
RSE <- round(sqrt( sum(residuals(model.lm)^2) / model.lm$df.residual),2)
print(paste0("RSE: ", RSE))

# Error Rate
ErrorRate <- round(sigma(model.lm)/mean(students.data$G3),2)
print(paste0("ErrorRate: ", ErrorRate))

# drop insignificant predictors and rerun regression with significant predictors
model2.lm <- lm(G3~traveltime+failures+absences+G1+G2, data=train)
summary(model2.lm) 

#Examine residuals and response vs. predictor (to verify that assumptions hold)
par(mfrow=c(2,2))
plot(model2.lm)

# Cook's distance
plot(model2.lm, 4)

# Accuracy of the model with significant predictors

# Residual standard error or sigma
RSE <- round(sqrt( sum(residuals(model2.lm)^2) / model2.lm$df.residual),2)
print(paste0("RSE: ", RSE))

ErrorRate <- round(sigma(model2.lm)/mean(students.data$G3),2)
print(paste0("ErrorRate: ", ErrorRate))

# Model performance and accracy calculation on test data

# Make predictions on test data
predictions <- model2.lm %>% predict(test)

# Compute the prediction error, RMSE on test data
RMSE(predictions, test$G3)

# Compute R-square of test data
R2(predictions, test$G3)

# Error Rate of test data
ErrorRate <- round(sigma(model2.lm)/mean(test$G3),2)
print(paste0("ErrorRate: ", ErrorRate))

# Predict G3 scores
G3.predictions <- predict(model2.lm, test)
summary(G3.predictions)


#****************Classification Tree**********************************#

# Shuffle the data using function sample()
shuffle_index <- sample(1:nrow(students.data))
head(students.data)

# use the above index to shuffle the dataset.
students.data <- students.data[shuffle_index, ]
head(students.data)

# Add categorical variable High to dataframe using G3
High=ifelse(students.data$G3 < median(students.data$G3),"No","Yes")
students.data2=data.frame(students.data,High)
str(students.data2)

# Split the data into test and train sets
data.split <- sample.split(students.data2$age, SplitRatio = 0.7)

# Training data
train <- subset(students.data2,data.split==TRUE)

# Test data
test <- subset(students.data2,data.split==FALSE)

# Build the model, using class method because we are predicting a class
set.seed(678)
fit <- rpart(High~.-G3, data = train, method = 'class')
rpart.plot(fit, extra = 106)

# display the important variables in decreasing order
imp <- varImp(fit)
rownames(imp)[order(imp$Overall, decreasing=TRUE)]

# Make a prediction using test data
predict_unseen <-predict(fit, test, type = 'class')

# Measure performance
table_mat <- table(test$High, predict_unseen)
table_mat 

# Calculate accuracy
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

# Tune the parameters to improve the model over default value
accuracy_tune <- function(fit) {
    predict_unseen <- predict(fit, test, type = 'class')
    table_mat <- table(test$High, predict_unseen)
    accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
    accuracy_Test
}

control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(High~.-G3, data = train, method = 'class', control = control)

accuracy_tune(tune_fit)
