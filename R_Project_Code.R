install.packages("tidyverse")
#Tidyverse library contains packages like ggplot2, dplyr etc.
library(tidyverse)
list.files(path = "../input")
#Loading train and test dataset
train = read.csv("C:\\Users\\Sara\\Desktop\\R Project\\credit_train.csv")
test = read.csv("C:\\Users\\Sara\\Desktop\\R Project\\credit_test.csv")
head(train)
head(test)
library(tidyverse)
str(train)
#Checking class of all the columns in train
sapply(train, class)
colnames(train)
#Replace blank space as underscore in column names for processing
names(train)<-str_replace_all(names(train), c(" " = "_"))
names(test)<-str_replace_all(names(test), c(" " = "_"))
dim(train)
missing_data <- colSums(is.na(train))[colSums(is.na(train)) > 0] %>% sort(decreasing=T)
missing_data
#Calculating percentage of missing data
nrow(train[!complete.cases(train), ])/nrow(train)*100
data_subset <- train[ , c("Loan_ID")]  
train <- train[complete.cases(data_subset), ]
#Plotting Loan Status
ggplot(data = train) +
  geom_bar(mapping = aes(x = Loan_Status))
#Getting unique classes of categorical attributes
sort(unique(train$Term))
sort(unique(train$Years_in_current_job))
sort(unique(train$Home_Ownership))
sort(unique(train$Purpose))
table(train$Purpose, train$Loan_Status)
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(train, aes(x = reorder_size(Years_in_current_job))) +
  geom_bar() +
  xlab("Years_in_current_job") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(train, aes(x = reorder_size(Years_in_current_job))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Years_in_current_job") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~ Loan_Status) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(train, aes(x = reorder_size(Term))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Term") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~ Loan_Status) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(train, aes(x = reorder_size(Purpose))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Purpose") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(Term ~ Loan_Status) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data = train, mapping = aes(x = Years_of_Credit_History)) + 
  geom_histogram(binwidth = 0.25)
summary(train$Credit_Score)
train=train[!duplicated(train), ]
a=as.data.frame((table(train$Loan_ID)))
head(a)
names(a)[names(a) == "Var1"] <- "Loan_ID"
head(a)
train=merge(x = train, y = a, by = "Loan_ID", all.x = TRUE)
train$Credit_Score_flag = ifelse(is.na(train$Credit_Score),1,0)
head(train)
#train$Credit_Score_Freq_flag = ifelse(train$Credit_Score_flag == 1 & train$Freq > 1,1,0)
#train = train[train['Credit_Score_Freq_flag'] == 0,]
train$current_loan_amount_flag = ifelse(train$Current_Loan_Amount==99999999,1,0)
train$loan_Drop_flag = ifelse(train$current_loan_amount_flag == 1 & train$Freq > 1,1,0)
train = train[train['loan_Drop_flag'] == 0,]
drop <- c("Freq","Credit_Score_flag","Credit_Score_Freq_flag","current_loan_amount_flag","loan_Drop_flag")
train = train[,!(names(train) %in% drop)]
head(train)
#Changing the amount '999999999' to mean value of column current loan amount
s=train[train$Current_Loan_Amount != 99999999,]
mean(s$Current_Loan_Amount)
train$Current_Loan_Amount = ifelse(train$Current_Loan_Amount==99999999,308587,train$Current_Loan_Amount)
#Dividing train dataset based on Loan Status - fully paid and charged off
train1=train[train$Loan_Status=='Fully Paid',]
train2=train[train$Loan_Status=='Charged Off',]
#Replacing missing values by mean of column
train1$Current_Loan_Amount[is.na(train1$Current_Loan_Amount)] <- mean(train1$Current_Loan_Amount, na.rm = TRUE)
train1$Credit_Score[is.na(train1$Credit_Score)] <- mean(train1$Credit_Score, na.rm = TRUE)
train1$Annual_Income[is.na(train1$Annual_Income)] <- mean(train1$Annual_Income, na.rm = TRUE)
train1$Monthly_Debt[is.na(train1$Monthly_Debt)] <- mean(train1$Monthly_Debt, na.rm = TRUE)
train1$Years_of_Credit_History[is.na(train1$Years_of_Credit_History)] <- mean(train1$Years_of_Credit_History, na.rm = TRUE)
train1$Months_since_last_delinquent[is.na(train1$Months_since_last_delinquent)] <- mean(train1$Months_since_last_delinquent, na.rm = TRUE)
train1$Number_of_Open_Accounts[is.na(train1$Number_of_Open_Accounts)] <- mean(train1$Number_of_Open_Accounts, na.rm = TRUE)
train1$Number_of_Credit_Problems[is.na(train1$Number_of_Credit_Problems)] <- mean(train1$Number_of_Credit_Problems, na.rm = TRUE)
train1$Current_Credit_Balance[is.na(train1$Current_Credit_Balance)] <- mean(train1$Current_Credit_Balance, na.rm = TRUE)
train1$Maximum_Open_Credit[is.na(train1$Maximum_Open_Credit)] <- mean(train1$Maximum_Open_Credit, na.rm = TRUE)
train1$Bankruptcies[is.na(train1$Bankruptcies)] <- mean(train1$Bankruptcies, na.rm = TRUE)
train1$Tax_Liens[is.na(train1$Tax_Liens)] <- mean(train1$Tax_Liens, na.rm = TRUE)
head(train1)
train2$Current_Loan_Amount[is.na(train2$Current_Loan.Amount)] <- mean(train2$Current_Loan_Amount, na.rm = TRUE)
train2$Credit_Score[is.na(train2$Credit_Score)] <- mean(train2$Credit_Score, na.rm = TRUE)
train2$Annual_Income[is.na(train2$Annual_Income)] <- mean(train2$Annual_Income, na.rm = TRUE)
train2$Monthly_Debt[is.na(train2$Monthly_Debt)] <- mean(train2$Monthly_Debt, na.rm = TRUE)
train2$Years_of_Credit_History[is.na(train2$Years_of_Credit_History)] <- mean(train2$Years_of_Credit_History, na.rm = TRUE)
train2$Months_since_last_delinquent[is.na(train2$Months_since_last_delinquent)] <- mean(train2$Months_since_last_delinquent, na.rm = TRUE)
train2$Number_of_Open_Accounts[is.na(train2$Number.of_Open_Accounts)] <- mean(train2$Number_of_Open_Accounts, na.rm = TRUE)
train2$Number_of_Credit_Problems[is.na(train2$Number_of_Credit_Problems)] <- mean(train2$Number_of_Credit_Problems, na.rm = TRUE)
train2$Current_Credit_Balance[is.na(train2$Current_Credit_Balance)] <- mean(train2$Current_Credit_Balance, na.rm = TRUE)
train2$Maximum_Open_Credit[is.na(train2$Maximum_Open_Credit)] <- mean(train2$Maximum_Open_Credit, na.rm = TRUE)
train2$Bankruptcies[is.na(train2$Bankruptcies)] <- mean(train2$Bankruptcies, na.rm = TRUE)
train2$Tax_Liens[is.na(train2$Tax_Liens)] <- mean(train2$Tax_Liens, na.rm = TRUE)
head(train2)
#Round function rounds off the values in its first argument
train2$Current_Loan_Amoun=round(train2$Current_Loan_Amount,0)
train2$Credit_Score=round(train2$Credit_Score,0)
train2$Annual_Income=round(train2$Annual_Income,0)
train2$Monthly_Debt=round(train2$Monthly_Debt,0)
train2$Years_of_Credit_History=round(train2$Years_of_Credit_History,0)
train2$Months_since_last_delinquent=round(train2$Months_since_last_delinquent,0)
train2$Number_of_Open_Accounts=round(train2$Number_of_Open_Accounts,0)
train2$Number_of_Credit_Problems=round(train2$Number_of_Credit_Problems,0)
train2$Current_Credit_Balance=round(train2$Current_Credit_Balance,0)
train2$Maximum_Open_Credit=round(train2$Maximum_Open_Credit,0)
train2$Bankruptcies=round(train2$Bankruptcies,0)
train2$Tax_Liens=round(train2$Tax_Liens,0)

train1$Current_Loan_Amoun=round(train1$Current_Loan_Amount,0)
train1$Credit_Score=round(train1$Credit_Score,0)
train1$Annual_Income=round(train1$Annual_Income,0)
train1$Monthly_Debt=round(train1$Monthly_Debt,0)
train1$Years_of_Credit_History=round(train1$Years_of_Credit_History,0)
train1$Months_since_last_delinquent=round(train1$Months_since_last_delinquent,0)
train1$Number_of_Open_Accounts=round(train1$Number_of_Open_Accounts,0)
train1$Number_of_Credit_Problems=round(train1$Number_of_Credit_Problems,0)
train1$Current_Credit_Balance=round(train1$Current_Credit_Balance,0)
train1$Maximum_Open_Credit=round(train1$Maximum_Open_Credit,0)
train1$Bankruptcies=round(train1$Bankruptcies,0)
train1$Tax_Liens=round(train1$Tax_Liens,0)
#Using rbind function, which combines vector, matrix or data frame by rows, to combine train1 and train2 
final_train = rbind(train1, train2)
final_train$current_job_year <- ifelse((final_train$Years_in_current_job == ('< 1 year') | final_train$Years_in_current_job == ('1 year')
                                        | final_train$Years_in_current_job ==('2 years')
                                        | final_train$Years_in_current_job == ('3 years') 
                                        | final_train$Years_in_current_job == ('4 years')),'0-4 years',
                                       ifelse((final_train$Years_in_current_job == ('5 years') | final_train$Years_in_current_job ==('6 years')
                                               | final_train$Years_in_current_job == ('7 years') | final_train$Years_in_current_job ==('8 years')
                                               | final_train$Years_in_current_job == ('9 years') | final_train$Years_in_current_job == ('n/a')),'5-9 years','>=10 years'))
final_train$Credit_Type <- ifelse((final_train$Purpose == ('Business Loan') | final_train$Purpose == ('small_business')
                                   | final_train$Purpose ==('renewable_energy')),'Business Credit',
                                  ifelse((final_train$Purpose == ('Home Improvements') | final_train$Purpose ==('Buy House')
                                          | final_train$Purpose == ('moving')),'Mortgage Credit',
                                         ifelse((final_train$Purpose == ('Buy a Car')),'Vehicle Credit',
                                                ifelse((final_train$Purpose == ('Debt Consolidation')),'Debt Consolidation',
                                                       ifelse((final_train$Purpose == ('Educational Expenses') | final_train$Purpose ==('major_purchase')
                                                               | final_train$Purpose == ('Medical Bills') | final_train$Purpose ==('Take a Trip')
                                                               | final_train$Purpose == ('vacation') | final_train$Purpose == ('wedding')),'Consumer  Credit',
                                                              'Other')))))
df1=final_train
#Dummies library helps to create and manipulate dummy variables flexibly and efficiently
#This is done for categorical variables
library(dummies)
df1 <- cbind(df1, dummy(df1$Term, sep = "_"))
df1 <- cbind(df1, dummy(df1$Home_Ownership, sep = "_"))
df1 <- cbind(df1, dummy(df1$Credit_Type, sep = "_"))
df1 <- cbind(df1, dummy(df1$current_job_year, sep = "_"))
df1 = subset(df1, select = -c(Loan_ID, Customer_ID, Term, Years_in_current_job, Home_Ownership, Purpose,current_job_year,Credit_Type))
df1$Loan_Status=ifelse(df1$Loan_Status== 'Charged Off', 1,0)
install.packages("clusterSim")
#Installing clustersim for normalization
library(clusterSim)
normFunc=data.Normalization(df1, type="n4", normalization="column")
df1N=as.data.frame(lapply(1, function(x) normFunc))
install.packages("caret")
#Installing caret library
library(caret)
set.seed(1)
#Creating a train-test split with train = 90%, test = 10%
trainIndis=createDataPartition(y=df1$Loan_Status, p=.90, list=FALSE)

train=df1[trainIndis,]
test=df1[-trainIndis,]
train$Loan_Status=ifelse(train$Loan_Status== 1, 'Charged Off', 'Fully Paid')
train$Loan_Status <- as.factor(train$Loan_Status)
str(train)
testfeature=test[,-1]
testtarget=test[[1]]

trainfeature=train[,-1]
traintarget=train[[1]]

install.packages("RWeka")
library(RWeka)
sapply(train, class)
C45_model=J48(Loan_Status ~., data=train)
test %>% add_row(Current_Loan_Amount = 88572,	Credit_Score = 7510, Annual_Income =	1150374, Monthly_Debt =	18598, Years_of_Credit_History = 22, Months_since_last_delinquent = 34, Number_of_Open_Accounts = 12, Number_of_Credit_Problems = 0, Current_Credit_Balance = 680124, Maximum_Open_Credit = 855052, Bankruptcies = 0, Tax_Liens = 0, Current_Loan_Amount = 558074)  
C45_model_test.pred <- predict(C45_model, newdata = test)
table(test$Loan_Status, C45_model_test.pred)
summary(C45_model_test.pred)

