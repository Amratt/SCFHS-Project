# Chaning directory 
setwd("~/Desktop/Data Science (Google)/Other/Data Science Interview/scfhs-project/data")
## Loading The data

emp <- read.csv("employees_data.csv")

# Objective

# from the exploratory analysis done in Tableau, I have concluded 2 things:
# 1. Women with technical degrees working in R&D earn more money than men 
#    with the same criteria 
# 2. There is a relationship between work-life balance and job satisfaction if
#    the employee was a female working as a sales rep

# To be able to make these conclusions, one must use inferential statistcs 
# and see if the averages is significantly different in (1)
# and see if a relationship (correlation) exists in (2)

# BONUS Question

# 3. I would like to build a machine learning model that aim to predict if 
#    an employee is Happy (Job Satisfaction Score = 3,4) or Unhappy (Job Satisfaction Score = 1,2)
#    based on some of the variables in the dataset, and evaluate the performance of the model



# Collect data with SQL
library(sqldf)
## Data will be used to answer Question 1. 
# I am trying to get the monthly incomes of technical degrees working in R&D
data.1 <- sqldf('SELECT Gender, MonthlyIncome 
                FROM emp
                WHERE Department = "Research & Development"
                AND EducationField = "Technical Degree"') 



## Data will be used to answer Question 2. 
# here I am getting work life balance and job satisfaction score for Females working as Sales Reps
# so we can figure out the correlation between them
data.2<- sqldf('SELECT WorkLifeBalance, JobSatisfaction 
                FROM emp
                WHERE Gender = "Female"
                AND JobRole = "Sales Representative"') 



## Data will be used to answer Question 3. 
# The data used for question 3 is the same as the original 1470 record dataset
# However, there are some cleaning that needs to be made to ensure that
# that the model is not misleading


# Data Exploration and Data Cleaning 

## Analysis For Question 1
males = which(data.1$Gender=='Male')
females = which(data.1$Gender=='Female')
t.test(data.1$MonthlyIncome[males],data.1$MonthlyIncome[females]) 
# P Value = 0.002 (with alpha = 0.05) we reject the null and conclude that the pay is significantly different



## Analysis For Question 2
library(corrplot)
M = cor(data.2)
M # the correlation coefficient = -0.22
#corrplot(M, method = "number")




## Exploratory Analysis/Data Cleaning For Question 3
# The first step in data cleaning is checking if there are missing values 
sum(is.na(emp))
# removed varibles that does not add value or might hurt the model
# that includes EmployeeCount, EmployeeNumber (ID),Over18 ,StandardHours
data.3<- emp[,-c(9,10)] 

# There are also varibles that seem to have the same value repeated for most rows
# We can used Near Zero Variance to detect those varibles and remove them
# Applying the near zero variance on every column in that data frame will help us detect the variables

library(caret)
apply(X = data.3, FUN = nearZeroVar, MARGIN = 2) 
# it looks like StandardHours, Over18 has the same values across all rows, so we remove them
data.3<- data.3[,-c(20,25)]

# multicollinearity is another issues that needs to be looked at
# it is when the variables (Features) are correlated with each other
# Therefore it is important to remove one of the correlated variables 
# or apply models that handles such scenarios.

# I will create a correlation matrix that includes all of the numeric variables
data.3.num = data.3[,-c(2,3,5,8,10,14,16,20)]
M2 = cor(data.3.num)
corrplot(M2, method = "circle")
# there seems to be some correlated features that needs to be removed

# TotalWorkingYears ~ Age , TotalWorkingYears ~ JobLevel
# MonthlyIncome ~ JobLevel, PerformanceRating ~ PercentSalaryHike
# So here I will remove Age, JobLevel, and PercentSalaryHike

# Also YearsAtCompany and YearsInCurrentRole and YearsSinceLastPromotion and YearsWithcurrManger
# are all correlated so I will remove all of them except for YearsAtCompany
data.3 = data.3[,-c(1,13,21,28)]

# here I am converting 1 & 2 to unhappy employees, and 3 &4 to happy to make modeling binary and cleaner and much more interpretable
#data.3$JobSatisfaction[data.3$JobSatisfaction==1 | data.3$JobSatisfaction==2]<-"Unappy"
# data.3$JobSatisfaction[data.3$JobSatisfaction==3 | data.3$JobSatisfaction==4]<-"Happy"


# now that the data is ready, we can start modeling


# Modeling (For Question 3 only)
# I will build a Random Forest Model using the Caret Library
# Before doing this I need to split the data intro training and testing 

set.seed(1)

n <- nrow(data.3)  
t <- 0.8*n            #Set the size of the training set
set.seed(1)        #Setting a fix seed to make the results reproducible
trainIndex <- sample(1:n,t)

# Training Dataset
training<- data.3[trainIndex,]
training$JobSatisfaction<-as.factor(training$JobSatisfaction)
# Testing Dataset
testing<- data.3[-trainIndex,]
testing$JobSatisfaction<-as.factor(testing$JobSatisfaction)


# to help reduce the variance, I will use 10 fold cross validation

train_control<- trainControl(method="cv", number=10)

# train the model 
clasmodel<- train(JobSatisfaction~., data=training, trControl=train_control, method="rf")

# now we can evaluate it against the testing dataset

# make predictions
predictions<- predict(clasmodel,testing)


# append predictions
mydat<- cbind(testing,predictions)

# Confusion Matrix
confusionMatrix(predictions,testing$JobSatisfaction)

# Summarize your results
## Analysis For Question 1
# Females working for R&D with technical degrees earn an average of $7671
# While males working for R&D with technical degrees earn an average of $4574
# In our analysis I conducted a T test to see if the difference is statisticly significant
# The results showed that with a P value of 0.002 we can comfortably say that 
# women working for R&D with technical degrees earn more than men with the same criteria





## Analysis For Question 2
# The correlation coefficient was -0.22. The closer the number in magnitude to 0
# The less the more you know that there is no relationship
# In this case, although the Tableau treemap suggested that there might be a a relationship
# between work-life balance and job satisfaction for female working as sales rep,
# there is not enough evidance to support that claim statisticaly 





# List any resource you used (all libraries used)
