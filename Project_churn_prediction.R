# remove all the objects stored
rm(list=ls())

# Installing and loading few libraries
library(ggplot2)
library(grid)
library(gtable)
install.packages("gridExtra")
library(gridExtra)
install.packages("corrgram")
library(corrgram)

# Set working directory
setwd("C:/Users/jatin/Videos/Music/Documents/project1_edwisor")

getwd()


df= read.csv("Train_data.csv",header=T, na.strings=c(" ","NA"))
str(df)

# We have 3333 observations of 21 variables

# We have area.code as int data type , converting it into factor
df$area.code = as.factor(df$area.code)
str(df)
df
# just saving a copy of dataset before pre-processing for comparison afterwards 
# processing of data
df_first = df
summary(df$Churn)
# We have 3333 observations of 21 variables and churn values
# 2850 False , 483 true

## Data pre-processing
# Preparing data for model building

# Checking missing values

missing_val = data.frame(apply(df,2,function(x){
  sum(is.na(x))
}))


# There are no missing values in the dataset , 

# Outlier analysis will be done on numeric data to see if dataset is containing some exceptional data

numeric_index = sapply(df,is.numeric)

numeric_data = df[,numeric_index]
numeric_data 

cnames = colnames(numeric_data)

cnames

for (i in 1:length(cnames))
{
     assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(df))+ 
              stat_boxplot(geom = "errorbar", width = 0.5) +
              geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                           outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
              labs(y=cnames[i],x="Churn")+
              ggtitle(paste("Box plot of responded for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn9,ncol=2)

gridExtra::grid.arrange(gn15,gn7,ncol=2)

#loop to remove outliers from all variables
 for(i in cnames){
   print(i)
   val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
   print(length(val))
   df = df[which(!df[,i] %in% val),]
 }

unique(df$number.customer.service.calls)
# We have removed a lot of observations from this variable

# Now going for feature selection 
# Correlation analysis among independent variables and chisquare test among
# categorical and independent variables

corrgram(df[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# total.day.charge , total.eve.charge, total.night.charge , total.intl.charge can be removed
# as they are redundant


## Chi-squared Test of Independence

factor_index = sapply(df,is.factor)
factor_data = df[,factor_index]

# we will apply chisquare on df_first as it contains 3333 observations
#choosing statistical significance as p = 0.05
for (i in 1:ncol(factor_data))
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn,factor_data[,i])))
}


# p>0.05 accept the null hypothesis variables are independent
# area.code, phone no  can be removed 

unique(df$state)

# We will remove State variable also as it not very less than 0.05 and has too many levels
# which is not helpful in model building
# Removing 7 variables
df = subset(df, select = -c(state,area.code,phone.number,total.day.charge,total.eve.charge,total.night.charge,total.intl.charge))
df

colnames(df)

#Normalisation
# for future use
# df_new frame
cnames = c("account.length","number.vmail.messages","total.day.minutes","total.day.calls","total.eve.minutes","total.eve.calls","total.night.calls",
           "total.night.minutes","total.intl.minutes","total.intl.calls" ,"number.customer.service.calls")

df_new = df
for(i in cnames){
  print(i)
  df_new[,i] = (df[,i] - min(df[,i]))/
    (max(df[,i]) - min(df[,i]))
}
##Data Manupulation; convert string categories into factor numeric

for(i in 1:ncol(df))
{
  if(class(df[,i]) == 'factor')
  {
    df[,i] = factor(df[,i], labels=(1:length(levels(factor(df[,i])))))
  }
}
df

# cHURN 1 = false and 2 = true in dataframe

# Implementing decision trees algorithm for classification pblm

install.packages("C50")
library(C50)

c50_model = C5.0 (Churn~.,df, trials = 10, rules = TRUE)
summary(c50_model)

# Reading the test data file and some data manipulation
test = read.csv("Test_data.csv")
# removing redudant variables from test data
test = subset(test, select = -c(state,area.code,phone.number,total.day.charge,total.eve.charge,total.night.charge,total.intl.charge))

for(i in 1:ncol(test)){
  
  if(class(test[,i]) == 'factor'){
    
    test[,i] = factor(test[,i], labels=(1:length(levels(factor(test[,i])))))
    
  }
}


str(test)

C50_Predictions = predict(c50_model, test[,-14], type = "class")
C50_Predictions

install.packages("caret",dependencies= TRUE )
library(caret)

ConfMatrix_C50 = table(test$Churn, C50_Predictions)
confusionMatrix(ConfMatrix_C50)



# Accuracy = 93.34
# FNR = FN/FN +TP = 101/101+123
#45.08


# Random Forest Model
install.packages("randomForest")
library(randomForest)
RF_model = randomForest(Churn ~ ., df, importance = TRUE, ntree = 50)

RF_Predictions = predict(RF_model, test[,-14])

ConfMatrix_RF = table(test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF)

# Accuracy = 93.34
# FNR = 109/(109 +115)
# 48.66

# Logistic regression

#Logistic Regression
logit_model = glm(Churn ~ ., data = df, family = "binomial")

#summary of the model
summary(logit_model)

test

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")
logit_Predictions
#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 2, 1)

logit_Predictions
ConfMatrix_logit = table(test$Churn, logit_Predictions)
test$Churn

confusionMatrix(ConfMatrix_logit)

# Accuracy = 89.02
# FNR = 73.2

library(class)
# We will use the scaled data for KNN 
#preparing the scaled test data
cnames

test_new = test
for(i in cnames){
  print(i)
  test_new[,i] = (test_new[,i] - min(test_new[,i]))/
    (max(test_new[,i]) - min(test_new[,i]))
}

test_new
KNN_Predictions = knn(df[, 1:13], test[, 1:13], df[,14], k = 5)

Conf_matrix = table(KNN_Predictions, test$Churn)

sum(diag(Conf_matrix))/nrow(test)
##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, KNN_Predictions)

confusionMatrix(ConfMatrix_RF)
# 156/156+68 
# FNR = 72.3
#Accuraacy=89.3



#naive Bayes
install.packages("e1071")
library(e1071)

#Develop model
NB_model = naiveBayes(Churn~ ., data = df)

#predict on test cases #raw
NB_Predictions = predict(NB_model, test[,1:13], type = 'class')

#Look at confusion matrix
Conf_matrix = table(observed = test[,14], predicted = NB_Predictions)
confusionMatrix(Conf_matrix)

#Accuracy: 88.84
#FNR: 174/174+50
#77.6

#statiscal way
mean(NB_Predictions == test$Churn)




