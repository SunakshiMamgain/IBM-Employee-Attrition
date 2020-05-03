data<-read.csv('dataset.csv')
summary(data)
colnames(data)
data$Attrition <- factor(data$Attrition)
data$BusinessTravel <- factor(data$BusinessTravel)
data$Department <- factor(data$Department)
data$Education <- factor(data$Education)
data$EducationField <- factor(data$EducationField)
data$EnvironmentSatisfaction <- factor(data$EnvironmentSatisfaction)
data$Gender <- factor(data$Gender)
data$JobInvolvement <- factor(data$JobInvolvement)
data$JobLevel <- factor(data$JobLevel)
data$JobRole <- factor(data$JobRole)
data$JobSatisfaction <- factor(data$JobSatisfaction)
data$MaritalStatus <- factor(data$MaritalStatus)
data$Over18 <- factor(data$Over18)
data$OverTime <- factor(data$OverTime)
data$PerformanceRating <- factor(data$PerformanceRating)
data$RelationshipSatisfaction <- factor(data$RelationshipSatisfaction)
data$StockOptionLevel <- factor(data$StockOptionLevel)
data$WorkLifeBalance <- factor(data$WorkLifeBalance)

data
data <- subset( data, select = -c(EmployeeCount, EmployeeNumber, StandardHours) )
View(data)


# bi-variate AND uNIVARIATE aNALYSIS


t.test(Age~Attrition,data=data)
# Welch Two Sample t-test
# 
# data:  Age by Attrition
# t = 5.828, df = 316.93, p-value = 1.38e-08
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   2.618930 5.288346
# sample estimates:
#   mean in group No mean in group Yes 
# 37.56123          33.60759
#My interpretation: age has significant impact on attrition

t.test(DailyRate~Attrition,data=data)
# Welch Two Sample t-test
# 
# data:  DailyRate by Attrition
# t = 2.1789, df = 333.76, p-value = 0.03004
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   6.040083 118.243100
# sample estimates:
#   mean in group No mean in group Yes 
# 812.5045          750.3629 
#My interpretation: DailyRate has impact on attrition

t.test(DistanceFromHome~Attrition,data=data)
# Welch Two Sample t-test
# 
# data:  DistanceFromHome by Attrition
# t = -2.8882, df = 322.72, p-value = 0.004137
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.8870025 -0.5475146
# sample estimates:
#   mean in group No mean in group Yes 
# 8.915653         10.632911 
#has impact

t.test(MonthlyIncome~Attrition,data=data)
# data:  MonthlyIncome by Attrition
# t = 7.4826, df = 412.74, p-value = 4.434e-13
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1508.244 2583.050
# sample estimates:
#   mean in group No mean in group Yes 
# 6832.740          4787.093 
#has impact

t.test(MonthlyRate~Attrition,data=data)
# data:  MonthlyRate by Attrition
# t = -0.5755, df = 330.1, p-value = 0.5653
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1296.8656   709.8084
# sample estimates:
#   mean in group No mean in group Yes 
# 14265.78          14559.31 
# does not have an impact

t.test(NumCompaniesWorked~Attrition,data=data)
# data:  NumCompaniesWorked by Attrition
# t = -1.5747, df = 317.14, p-value = 0.1163
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.66437603  0.07367926
# sample estimates:
#   mean in group No mean in group Yes 
# 2.645580          2.940928 
# does not impact


t.test(PercentSalaryHike~Attrition,data=data)
# data:  PercentSalaryHike by Attrition
# t = 0.50424, df = 326.11, p-value = 0.6144
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3890709  0.6572652
# sample estimates:
#   mean in group No mean in group Yes 
# 15.23114          15.09705 
# does not impact

t.test(TotalWorkingYears~Attrition,data=data)
# data:  TotalWorkingYears by Attrition
# t = 7.0192, df = 350.88, p-value = 1.16e-11
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   2.604401 4.632019
# sample estimates:
#   mean in group No mean in group Yes 
# 11.862936          8.244726 
# has an impact

t.test(TrainingTimesLastYear~Attrition,data=data)
# data:  TrainingTimesLastYear by Attrition
# t = 2.3305, df = 339.56, p-value = 0.02036
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.03251776 0.38439273
# sample estimates:
#   mean in group No mean in group Yes 
# 2.832928          2.624473 
# has impact

t.test(YearsAtCompany~Attrition,data=data)
# data:  YearsAtCompany by Attrition
# t = 5.2826, df = 338.21, p-value = 2.286e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.404805 3.071629
# sample estimates:
#   mean in group No mean in group Yes 
# 7.369019          5.130802 
# has impact

t.test(YearsInCurrentRole~Attrition,data=data)
# data:  YearsInCurrentRole by Attrition
# t = 6.8471, df = 366.57, p-value = 3.187e-11
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.127107 2.035355
# sample estimates:
#   mean in group No mean in group Yes 
# 4.484185          2.902954 
# has an impact

t.test(YearsSinceLastPromotion~Attrition,data=data)
# data:  YearsSinceLastPromotion by Attrition
# t = 1.2879, df = 338.49, p-value = 0.1987
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.1525043  0.7309843
# sample estimates:
#   mean in group No mean in group Yes 
# 2.234388          1.945148 
#has no impact

t.test(YearsWithCurrManager~Attrition,data=data)
# data:  YearsWithCurrManager by Attrition
# t = 6.6334, df = 365.1, p-value = 1.185e-10
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.065929 1.964223
# sample estimates:
#   mean in group No mean in group Yes 
# 4.367397          2.852321 
#has an impact



#Applying chi-square test on categorical variables with respect to Attrition


chisq.test(data$BusinessTravel, data$Attrition, correct=FALSE)
# data:  data$BusinessTravel and data$Attrition
# X-squared = 24.182, df = 2, p-value = 5.609e-06
#dependent (p-value less than 0.05)
#install.packages("corrplot")
library(corrplot)
corrplot(chisq.test(data$BusinessTravel, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# Non-travel has a positive association with No and negative association with Yes -> Non-travel do not go for attrittion.
# Similarly Travel_Frequently has more possibility to go for attrittion and Travel_Rarely has less possibilty to go for attrition.



chisq.test(data$Department, data$Attrition, correct=FALSE)
# data:  data$Department and data$Attrition
# X-squared = 10.796, df = 2, p-value = 0.004526
# dependent 
corrplot(chisq.test(data$Department, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
#HR has positive association with Yes; RnD has postive association with No; Sales has positve association with Yes.
# HR and Sales people are more inclined towards attrition as compared to RnD.

chisq.test(data$Education, data$Attrition, correct=FALSE)
# data:  data$Education and data$Attrition
# X-squared = 3.074, df = 4, p-value = 0.5455
#independent
corrplot(chisq.test(data$Education, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
#1 has positive impact on Yes; 3 has impact on Yes

chisq.test(data$EducationField, data$Attrition, correct=FALSE)
# data:  data$EducationField and data$Attrition
# X-squared = 16.025, df = 5, p-value = 0.006774
#dependent
corrplot(chisq.test(data$EducationField, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association to Yes: HR, Marketing and Technical Degree

chisq.test(data$EnvironmentSatisfaction, data$Attrition, correct=FALSE)
# data:  data$EnvironmentSatisfaction and data$Attrition
# X-squared = 22.504, df = 3, p-value = 5.123e-05
corrplot(chisq.test(data$EnvironmentSatisfaction, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association to Yes: 1

chisq.test(data$Gender, data$Attrition, correct=FALSE)
# data:  data$Gender and data$Attrition
# X-squared = 1.2752, df = 1, p-value = 0.2588
corrplot(chisq.test(data$Gender, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association to Yes: Male

chisq.test(data$JobInvolvement, data$Attrition, correct=FALSE)
# data:  data$JobInvolvement and data$Attrition
# X-squared = 28.492, df = 3, p-value = 2.863e-06
#dependent
corrplot(chisq.test(data$JobInvolvement, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association with Yes: 1 and 2

chisq.test(data$JobLevel, data$Attrition, correct=FALSE)
# data:  data$JobLevel and data$Attrition
# X-squared = 72.529, df = 4, p-value = 6.635e-15
# dependent
corrplot(chisq.test(data$JobLevel, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association with Yes: 1 

chisq.test(data$JobRole, data$Attrition, correct=FALSE)
# data:  data$JobRole and data$Attrition
# X-squared = 86.19, df = 8, p-value = 2.752e-15
corrplot(chisq.test(data$JobRole, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association with Yes: HR, Laboratory Technician, Sales, Sales Representatives


chisq.test(data$JobSatisfaction, data$Attrition, correct=FALSE)
# 
# Pearson's Chi-squared test
# 
# data:  data$JobSatisfaction and data$Attrition
# X-squared = 17.505, df = 3, p-value = 0.0005563
corrplot(chisq.test(data$JobSatisfaction, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association with Yes:1


chisq.test(data$MaritalStatus, data$Attrition, correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  data$MaritalStatus and data$Attrition
# X-squared = 46.164, df = 2, p-value = 9.456e-11
corrplot(chisq.test(data$MaritalStatus, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association with Yes: Single


chisq.test(data$Over18, data$Attrition, correct=FALSE)
# data:  data$JobRole and data$Attrition
# X-squared = 86.19, df = 8, p-value = 2.752e-15
corrplot(chisq.test(data$Over18, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association with Yes: HR, Laboratory Technician, Sales, Sales Representatives


chisq.test(data$OverTime, data$Attrition, correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  data$OverTime and data$Attrition
# X-squared = 89.044, df = 1, p-value < 2.2e-16
corrplot(chisq.test(data$OverTime, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association with Yes: Yes


chisq.test(data$PerformanceRating, data$Attrition, correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  data$PerformanceRating and data$Attrition
# X-squared = 0.012267, df = 1, p-value = 0.9118
corrplot(chisq.test(data$PerformanceRating, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association with Yes: 4


chisq.test(data$RelationshipSatisfaction, data$Attrition, correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  data$RelationshipSatisfaction and data$Attrition
# X-squared = 5.2411, df = 3, p-value = 0.155
corrplot(chisq.test(data$RelationshipSatisfaction, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association with Yes: 1


chisq.test(data$StockOptionLevel, data$Attrition, correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  data$StockOptionLevel and data$Attrition
# X-squared = 60.598, df = 3, p-value = 4.379e-13
corrplot(chisq.test(data$StockOptionLevel, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association with Yes: 0


chisq.test(data$WorkLifeBalance, data$Attrition, correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  data$WorkLifeBalance and data$Attrition
# X-squared = 16.325, df = 3, p-value = 0.0009726
corrplot(chisq.test(data$WorkLifeBalance, data$Attrition, correct=FALSE)$residuals, is.cor = FALSE)
# positive association with Yes: 1,2,4

#########################################################################################\
# Decision Tree
# install.packages('splitstackshape')
# install.packages('ISLR')
# install.packages('caret')
# install.packages('rattle')
library(splitstackshape)
library(ISLR)
library(pdp)
library(rpart,quietly = TRUE)
library(caret)
library(rpart.plot,quietly = TRUE)
library(rattle)
data
data$Attrition <- factor(data$Attrition)
summary(data)
View(data)
evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  xtab = table(prediction, data$Attrition)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == data$Attrition)/length(data$Attrition)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy*100), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}


colnames(data)
# Usage
tree_with_params = rpart(formula = Attrition ~ .,data=data, method="class",control = rpart.control(cp =0.000009, maxdepth = 5,minsplit = 10,minbucket=4))
#rpart.plot(tree_with_params,extra=107) # Plot without labels
rpart.plot(tree_with_params) # Plot with labels
evaluation(tree_with_params, data, "class")
df <- data.frame(imp = tree_with_params$variable.importance)
df

# Removing features which are having variable importance less than 4 and performing stratified sampling.
data <- data[setdiff(colnames(data), c('NumCompaniesWorked', 'RelationshipSatisfaction', 'JobInvolvement', 'JobLevel', 'DistanceFromHome', 'PerformanceRating','TrainingTimesLastYear','Gender','BusinessTravel'))]
data1<-data[data$Attrition == 'No',]
data1
data2<-data[data$Attrition == 'Yes',]
colnames(data1)
set.seed(42)  # good idea to set the random seed for reproducibility
data1_<-stratified(data1, c("Age",'Attrition'), 0.22)
data1_


data<-rbind(data1_,data2)

plot(data$Attrition)

colnames(data)
# Usage
tree_with_params = rpart(formula = Attrition ~ .,data=data, method="class",control = rpart.control(cp =0.000009, maxdepth = 5,minsplit = 10,minbucket=4))
#rpart.plot(tree_with_params,extra=107) # Plot without labels
rpart.plot(tree_with_params) # Plot with labels
evaluation(tree_with_params, data, "class")



#install.packages('tidyverse')
library(tidyverse)
df <- data.frame(imp = tree_with_params$variable.importance)
df
#
df2 <- df %>%
  tibble::rownames_to_column() %>%
  dplyr::rename("variable" = rowname) %>%
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()
#
ggplot2::ggplot(df2) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp),
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable),
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()



#########################################################################################################

#########################################################################################################
#Multiple Linear Regreesion on YearsAtCompany
data<-read.csv('dataset.csv')
summary(data)
colnames(data)
data$Attrition <- factor(data$Attrition)
data$BusinessTravel <- factor(data$BusinessTravel)
data$Department <- factor(data$Department)
data$Education <- factor(data$Education)
data$EducationField <- factor(data$EducationField)
data$EnvironmentSatisfaction <- factor(data$EnvironmentSatisfaction)
data$Gender <- factor(data$Gender)
data$JobInvolvement <- factor(data$JobInvolvement)
data$JobLevel <- factor(data$JobLevel)
data$JobRole <- factor(data$JobRole)
data$JobSatisfaction <- factor(data$JobSatisfaction)
data$MaritalStatus <- factor(data$MaritalStatus)
data$Over18 <- factor(data$Over18)
data$OverTime <- factor(data$OverTime)
data$PerformanceRating <- factor(data$PerformanceRating)
data$RelationshipSatisfaction <- factor(data$RelationshipSatisfaction)
data$StockOptionLevel <- factor(data$StockOptionLevel)
data$WorkLifeBalance <- factor(data$WorkLifeBalance)

colnames(data)
data <- data[setdiff(colnames(data), c('NumCompaniesWorked', 'RelationshipSatisfaction', 'JobInvolvement', 'JobLevel', 'DistanceFromHome', 'PerformanceRating','TrainingTimesLastYear','Gender','BusinessTravel'))]

data <- subset( data, select = -c(Attrition,EmployeeCount, EmployeeNumber, StandardHours) )
View(data)
colnames(data)
model <- lm(YearsAtCompany~Age+MaritalStatus+MonthlyIncome+MonthlyRate+HourlyRate+JobSatisfaction+JobRole+DailyRate+Department+Education+EducationField+EnvironmentSatisfaction+StockOptionLevel+TotalWorkingYears+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data = data)
summary(model)


model <- lm(MonthlyIncome~Age+MaritalStatus+YearsAtCompany+MonthlyRate+HourlyRate+JobSatisfaction+JobRole+DailyRate+Department+Education+EducationField+EnvironmentSatisfaction+StockOptionLevel+TotalWorkingYears+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data = data)
summary(model)
########################################################################################################

library(splitstackshape)
library(ISLR)
library(pdp)
library(rpart,quietly = TRUE)
library(caret)
library(rpart.plot,quietly = TRUE)
library(rattle)
# data
# data$Attrition <- factor(data$Attrition)
# summary(data)
# View(data)
evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  xtab = table(prediction, data$OverTime)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == data$OverTime)/length(data$OverTime)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy*100), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}

plot(data$OverTime)

data1<-data[data$OverTime == 'No',]
data1
data2<-data[data$OverTime == 'Yes',]


set.seed(42)  # good idea to set the random seed for reproducibility
data1_<-stratified(data1, c("Age",'OverTime'), 0.40)
data1_


data<-rbind(data1_,data2)
plot(data$OverTime)
colnames(data)
# Usage
tree_with_params = rpart(formula = OverTime ~ .,data=data, method="class",control = rpart.control(cp =0.000009, maxdepth = 5,minsplit = 10,minbucket=4))
#rpart.plot(tree_with_params,extra=107) # Plot without labels
rpart.plot(tree_with_params) # Plot with labels
evaluation(tree_with_params, data, "class")
df <- data.frame(imp = tree_with_params$variable.importance)
df
###################################################################################################

###################################################################################################

data<-read.csv('dataset.csv')
summary(data)
colnames(data)
data$Attrition <- factor(data$Attrition)
data$BusinessTravel <- factor(data$BusinessTravel)
data$Department <- factor(data$Department)
data$Education <- factor(data$Education)
data$EducationField <- factor(data$EducationField)
data$EnvironmentSatisfaction <- factor(data$EnvironmentSatisfaction)
data$Gender <- factor(data$Gender)
data$JobInvolvement <- factor(data$JobInvolvement)
data$JobLevel <- factor(data$JobLevel)
data$JobRole <- factor(data$JobRole)
data$JobSatisfaction <- factor(data$JobSatisfaction)
data$MaritalStatus <- factor(data$MaritalStatus)
data$Over18 <- factor(data$Over18)
data$OverTime <- factor(data$OverTime)
data$PerformanceRating <- factor(data$PerformanceRating)
data$RelationshipSatisfaction <- factor(data$RelationshipSatisfaction)
data$StockOptionLevel <- factor(data$StockOptionLevel)
data$WorkLifeBalance <- factor(data$WorkLifeBalance)

colnames(data)
data <- data[setdiff(colnames(data), c('NumCompaniesWorked', 'RelationshipSatisfaction', 'JobInvolvement', 'JobLevel', 'DistanceFromHome', 'PerformanceRating','TrainingTimesLastYear','Gender','BusinessTravel'))]

data <- subset( data, select = -c(Attrition,EmployeeCount, EmployeeNumber, StandardHours) )
View(data)
colnames(data)


library(splitstackshape)
library(ISLR)
library(pdp)
library(rpart,quietly = TRUE)
library(caret)
library(rpart.plot,quietly = TRUE)
library(rattle)
# data
# data$Attrition <- factor(data$Attrition)
# summary(data)
# View(data)
evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  xtab = table(prediction, data$WorkLifeBalance)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == data$WorkLifeBalance)/length(data$WorkLifeBalance)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy*100), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}

plot(data$WorkLifeBalance)

data1<-data[data$WorkLifeBalance == '1',]
data2<-data[data$WorkLifeBalance == '2',]
data3<-data[data$WorkLifeBalance == '3',]
data4<-data[data$WorkLifeBalance == '4',]
set.seed(42)  # good idea to set the random seed for reproducibility
data2_<-stratified(data2, c("Age",'WorkLifeBalance'), 0.25)
View(data2_)
data3_<-stratified(data3, c("Age",'WorkLifeBalance'), 0.10)
View(data3_)
data4_<-stratified(data4, c("Age",'WorkLifeBalance'), 0.60)
View(data4_)


data<-rbind(data1,data2_,data3_,data4_)
plot(data$WorkLifeBalance)
colnames(data)
# Usage
tree_with_params = rpart(formula = WorkLifeBalance ~ .,data=data, method="class",control = rpart.control(cp =0.000009, maxdepth = 5,minsplit = 10,minbucket=4))
#rpart.plot(tree_with_params,extra=107) # Plot without labels
rpart.plot(tree_with_params) # Plot with labels
evaluation(tree_with_params, data, "class")
df <- data.frame(imp = tree_with_params$variable.importance)
df

###################################################################################################

###################################################################################################