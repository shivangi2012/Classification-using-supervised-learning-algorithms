#Set working directory
getwd()
setwd("F:/")

#Load data
bank<-read.csv("bank.csv",sep=";")

#Keeping original data safe
data<-bank

#Explore data

#structure of data
str(data)

#print first 6 rows
head(data)

#summary of data
summary(data)

#Check dimensions of data
dim(data)

#Rename column names
colnames(data)[c(6,7,8,9,10,11,12,13,14,15,16,17)]<-c("Avg_Yearly_balance","Housing_loan","Personal_loan","Contact_type","Last_contact_day",
                                                    "Last_contact_month","last_contact_duration","contacts_for_this_campaign",
                                                    "days_passed_after_prev_campaign","contacts_for_prev_campaign",
                                                    "outcome_of_prev_campaign","outcome_term_deposit")
str(data)

#Check column wise missing values
colSums((is.na(data)))

#There is no missing data

#check class distribution of factor variables
apply(data[,c(2,3,4,5,7,8,9,11,16,17)],2,table)

#check correlation between numeric variables
cor(data[,c(1,6,10,12,13,14,15)])

#Visualize correlation between numeric variables
library(corrplot)
corrplot(cor(data[,c(1,6,10,12,13,14,15)]),method="number")

#Only variable days passed after previous campaign is correlated with Contacts for
#previous campaign

#load library
library(ggplot2)
a<-data$outcome_term_deposit

#Exploratory data analysis

#Relation between marital status of client and outcome term deposit
table(data$marital,data$outcome_term_deposit)
ggplot(data,aes(x=marital,fill=outcome_term_deposit))+geom_bar()+
labs(x="Marital status of client",y="Frequency",title="Relation between marital status of client and outcome term deposit")

#Mostly Married people(277 out of 4521) and then single people(167 out of 4521) has subscribed for the term deposit

#Frequency distribution of outcome term deposit
table(data$outcome_term_deposit)
ggplot(data,aes(x=outcome_term_deposit))+geom_bar()+
  labs(x="outcome_term_deposit",y="Frequency",title="Barplot of outcome term deposit")

#Very few people(521 out of 4521) has subscribed for term deposit.

#Relation between education of client and outcome term deposit
table(data$education,data$outcome_term_deposit)
ggplot(data,aes(x=education,fill=outcome_term_deposit))+geom_bar()+
  labs(x="Education of client",y="Frequency",title="Relation between education status of client and outcome term deposit")

#mostly people with secondary(245 out of 4521) and tertiary(193 out of 4521) education has subscribed for term deposit

#Relation between Job of client and outcome term deposit
table(data$job,data$outcome_term_deposit)
ggplot(data,aes(x=job,fill=outcome_term_deposit))+geom_bar()+
  labs(x="Job of client",y="Frequency",title="Relation between Job of client and outcome term deposit")

#People having management(131 out of 4521),technician(83 out of 4521),blue collar job(69 out of 4521) and admin(58 out of 4521) jobs has mostly 
#subscribed for term deposit

#Relation between default and outcome term deposit
table(data$default,data$outcome_term_deposit)
ggplot(data,aes(x=default,fill=outcome_term_deposit))+geom_bar()+
  labs(x="default",y="Frequency",title="Relation between default and outcome term deposit")

#People with no default has only subscribed for term deposit

#Relation between Housing loan and outcome term deposit
table(data$Housing_loan,data$outcome_term_deposit)
ggplot(data,aes(x=Housing_loan,fill=outcome_term_deposit))+geom_bar()+
  labs(x="Housing loan of client",y="Frequency",title="Relation between housing loan and outcome term deposit")

#People with no housing loan has mostly subscribed for term deposit but people having 
#housig loan has also subscribed for term deposit 

#Relation between Personal loan and outcome term deposit
table(data$Personal_loan,data$outcome_term_deposit)
ggplot(data,aes(x=Personal_loan,fill=outcome_term_deposit))+geom_bar()+
  labs(x="Personal loan of client",y="Frequency",title="Relation between personal loan and outcome term deposit")

#People with no personal loan has mostly subscribed for term deposit. very few having
#perosnal loan has subscribed.

#Relation between contact type  and outcome term deposit
table(data$Contact_type,data$outcome_term_deposit)
ggplot(data,aes(x=Contact_type,fill=outcome_term_deposit))+geom_bar()+
  labs(x="Contact type",y="Frequency",title="Relation between contact type and outcome term deposit")

#People with cellular communication has mostly subscribed for term deposit

#Relation between last contact month and outcome term deposit
table(data$Last_contact_month,data$outcome_term_deposit)
ggplot(data,aes(x=Last_contact_month,fill=outcome_term_deposit))+geom_bar()+
  labs(x="Last contact month",y="Frequency",title="Relation between last contact month and outcome term deposit")

#People who were contacted in the month of May>Aug>July>April>June has mostly 
#subscribed for term deposit

#Relation between outcome of previous campaign and outcome term deposit
table(data$outcome_of_prev_campaign,data$outcome_term_deposit)
ggplot(data,aes(x=outcome_of_prev_campaign,fill=outcome_term_deposit))+geom_bar()+
  labs(x="Last contact month",y="Frequency",title="Relation between outcome of previous campaign and outcome term deposit")

#People with unknown and success previous campaign has mostly subscribed for term deposit

#Relation between education of client and outcome term deposit, marital status wise
ggplot(data, aes(x = education, fill =outcome_term_deposit )) + 
  geom_bar()+facet_grid(.~marital)+
  labs(x="Education of client",y="Frequency")

#Married people having secondary and tertiary education and single people having secondary 
#education has mostly subscribed for term deposit

#Relation between housing loan and outcome term deposit, personal loan wise
ggplot(data, aes(x = Housing_loan, fill = outcome_term_deposit)) + 
  geom_bar()+facet_grid(.~Personal_loan)+
  labs(x="Housing loan",y="Frequency")

#People having housing loan and no personal loan has mostly subscribed for term deposit

#Relation between age and outcome term deposit
ggplot(data, aes(x = factor(age), fill = outcome_term_deposit)) + 
  geom_bar()+
  labs(x="Age",y="Frequency")

#People of age 27-38 41-49 has mostly subscribed for term deposit

#Relation between Average yearly balance and outcome term deposit
ggplot(data, aes(x =(Avg_Yearly_balance), color = outcome_term_deposit)) + 
  geom_density()+
  labs(x="Average yearly balance",y="Frequency")

#People having avg yearly balance less than 5000 has mostly subscribed for term deposit

#Relation between last contact day and outcome term deposit
ggplot(data, aes(x =(Last_contact_day), fill = outcome_term_deposit)) + 
  geom_histogram()+
  labs(x="Last contact day",y="Frequency")

#People getting calls on 10-20 of the month has mostly subscribed for term deposit

#Relation between contacts for this campaign and outcome term deposit
ggplot(data, aes(x =(contacts_for_this_campaign), fill = outcome_term_deposit)) + 
  geom_histogram()+
  labs(x="Contacts for this campaign",y="Frequency")

#People contacted for 1-4 times for this campaign has mostly subscribed for term deposit

#Relation between Contacts for previous campaign and outcome term deposit
ggplot(data, aes(x =(contacts_for_prev_campaign), fill = outcome_term_deposit)) + 
  geom_histogram()+
  labs(x="contacts for previous campaign",y="Frequency")

#People contacted for 1-3 times before this campaign has mostly subscribed for term deposit

#Modeling

# 1. Decision trees

#load library
library(caret)

#For reproducible results
set.seed(123)

#Prepare training scheme
control <- trainControl(method="cv", number=5)

#Split data into train and test set
index <- createDataPartition(data$outcome_term_deposit,p=0.7,list=FALSE)
train <- data[index,]
test <- data[-index,]

#check dimensions of train and test set
dim(train)
dim(test)

#check distribution of output variable in train and test set
table(train$outcome_term_deposit)
table(test$outcome_term_deposit)

#load library for decision tree
library(rpart)

str(train)
# fit model
model1<- train(outcome_term_deposit~.,data=train,method="rpart",trControl=control)

# summarize the model
print(model1)
summary(model1)

# make prediction on training set
model_train<-predict(model1,data=train)
table(model_train,train$outcome_term_deposit)

#check accuracy on train data
mean(model_train==train$outcome_term_deposit)

# make prediction on test set
model_test1<-predict(model1,newdata=test)
table(model_test1,test$outcome_term_deposit)

#check accuracy on test data
mean(model_test1==test$outcome_term_deposit)
head(cbind(model_test1,test$outcome_term_deposit))

#We got 90.5% accuracy on train data and 89.3% accuracy on test data

# 2. Random Forests

#Find random forest model with default parameters
model_rf<-train(outcome_term_deposit~.,data=train,method="rf",trControl=control)

#summarize model
summary(model_rf)
model_rf

#make prediction on train set
train_rf<-predict(model_rf,data=train)
table(train_rf,train$outcome_term_deposit)

#check accuracy on train set
mean(train_rf==train$outcome_term_deposit)

#make prediction on test set
test_rf<-predict(model_rf,newdata=test)
table(test_rf,test$outcome_term_deposit)

#check accuracy on test set
mean(test_rf==test$outcome_term_deposit)

#we got 100% accuracy on train set and 89.8% accuracy on test data

#Now we'll check for algorithms like knn,svm,logistic regression and neural networks. For this we
#need numeric attributes. Therefore, we are converting categorical variables to numeric variables
#using one hot encoding

str(data)

#keep original copy of data safe
data1<-data

#One hot encoding for Job variable
for(i in unique(data$job)){
  data[paste("job",i,sep="_")]<-ifelse(data$job==i,1,0)
}

#One hot encoding for marital status variable
for(i in unique(data1$marital)){
  data1[paste("marital",i,sep="_")]<-ifelse(data1$marital==i,1,0)
}

#One hot encoding for education variable
for(i in unique(data1$education)){
  data1[paste("education",i,sep="_")]<-ifelse(data1$education==i,1,0)
}

#One hot encoding for default variable
data1$default_yes<-ifelse(data1$default=="yes",1,0)

#One hot encoding for housing loan variable
data1$housing_loan_yes<-ifelse(data1$Housing_loan=="yes",1,0)

#One hot encoding for Personal loan variable
data1$personal_loan_yes<-ifelse(data1$Personal_loan=="yes",1,0)

#One hot encoding for outcome term deposit variable
data1$outcome_deposit_yes<-ifelse(data1$outcome_term_deposit=="yes",1,0)

#One hot encoding for contact type variable
for(i in unique(data1$Contact_type)){
  data1[paste("Contact_type",i,sep="_")]<-ifelse(data1$Contact_type==i,1,0)
}

#One hot encoding for last contact month variable
for(i in unique(data1$Last_contact_month)){
  data1[paste("Last_contact_month",i,sep="_")]<-ifelse(data1$Last_contact_month==i,1,0)
}

#One hot encoding for outcome of previous campaign variable
for(i in unique(data1$outcome_of_prev_campaign)){
  data1[paste("outcome_prev_campaign",i,sep="_")]<-ifelse(data1$outcome_of_prev_campaign==i,1,0)
}

#check structure of data
str(data1)

#check dimensions of data
dim(data1)

#remove job variable 
data1$job<-NULL

#remove marital status variable 
data1$marital<-NULL

#remove education variable 
data1$education<-NULL

#remove default variable 
data1$default<-NULL

#remove housing loan variable 
data1$Housing_loan<-NULL

#remove personal loan variable 
data1$Personal_loan<-NULL

#remove contact type variable 
data1$Contact_type<-NULL

#remove last contact month variable 
data1$Last_contact_month<-NULL

#remove outcome of previous campaign variable 
data1$outcome_of_prev_campaign<-NULL

#remove outcome term deposit variable 
data1$outcome_term_deposit<-NULL

str(data1)
dim(data1)

#converting outcome variable to factor
data1$outcome_deposit_yes<-as.factor(data1$outcome_deposit_yes)
#data1$outcome_deposit_yes<-as.numeric(data1$outcome_deposit_yes) # dont run

#3. K Nearest neighbour

#For reproducible results
set.seed(123)

#Sampling
control1 <- trainControl(method="cv", number=5)

#Split data into train and test set
index1 <- createDataPartition(data1$outcome_deposit_yes,p=0.7,list=FALSE)
train1 <- data1[index,]
test1 <- data1[-index,]

#check dimensions of train and test set
dim(train1)
dim(test1)

#normalize the values
train_x<-train1[,names(data1)!="outcome_deposit_yes"]
preproc<-preProcess(train_x,method=c("center","scale"))

#Normalized train and test set
train_norm<-predict(preproc,train1)
test_norm<-predict(preproc,test1)

str(train_norm)
str(test_norm)

#Fit model
model_knn<-train(outcome_deposit_yes~.,data=train_norm,method="knn",trControl=control1)

#summary of model
summary(model_knn)
model_knn

#make prediction on train set
train_knn<-predict(model_knn,data=train_norm)
table(train_knn,train_norm$outcome_deposit_yes)

#check accuracy on train set
mean(train_knn==train_norm$outcome_deposit_yes)

#make prediction on test set
test_knn<-predict(model_knn,newdata=test_norm)
head(test_knn)
table(test_knn,test_norm$outcome_deposit_yes)

#check accuracy on test set
mean(test_knn==test_norm$outcome_deposit_yes)

#we got 89.6% accuracy on train set and 88.6% accuracy on test data

#4. Logistic regression

#Fit model
model_log<-train(outcome_deposit_yes~.,data=train_norm,method="glm",trControl=control1)

#summary of model
summary(model_log)
model_log

#make prediction on train set
train_log<-predict(model_log,data=train_norm)
table(train_log,train_norm$outcome_deposit_yes)

#check accuracy on train set
mean(train_log==train_norm$outcome_deposit_yes)

#make prediction on test set
test_log<-predict(model_log,newdata=test_norm)
head(test_log)
table(test_log,test_norm$outcome_deposit_yes)

#check accuracy on test set
mean(test_log==test_norm$outcome_deposit_yes)

#we got 90.8% accuracy on train set and 89.8% accuracy on test data

#5. SVM

#SVM_Linear

#fit model
model_svm_linear<-train(outcome_deposit_yes~.,data=train_norm,method="svmLinear",trControl=control1)

#summary of model
summary(model_svm_linear)
model_svm_linear

#make prediction on train set
train_svm_linear<-predict(model_svm_linear,data=train_norm)
table(train_svm_linear,train_norm$outcome_deposit_yes)

#check accuracy on train set
mean(train_svm_linear==train_norm$outcome_deposit_yes)

#make prediction on test set
test_svm_linear<-predict(model_svm_linear,newdata=test_norm)
head(test_svm_linear)
table(test_svm_linear,test_norm$outcome_deposit_yes)

#check accuracy on test set
mean(test_svm_linear==test_norm$outcome_deposit_yes)

#we got 89.7% accuracy on train set and 89.89% accuracy on test data

#SVM_Radial

#fit model
model_svm_radial<-train(outcome_deposit_yes~.,data=train_norm,method="svmRadial",trControl=control1)
summary(model_svm_radial)
model_svm_radial

#make prediction on train set
train_svm_radial<-predict(model_svm_radial,data=train_norm)
table(train_svm_radial,train_norm$outcome_deposit_yes)

#check accuracy on train set
mean(train_svm_radial==train_norm$outcome_deposit_yes)

#make prediction on test set
test_svm_radial<-predict(model_svm_radial,newdata=test_norm)
head(test_svm_radial)
table(test_svm_radial,test_norm$outcome_deposit_yes)

#check accuracy on test set
mean(test_svm_radial==test_norm$outcome_deposit_yes)

#we got 91.6% accuracy on train set and 89.15% accuracy on test data

#6. Neural network

#fit model
model_nn<-train(outcome_deposit_yes~.,data=train_norm,method="nnet",trControl=control1)
summary(model_nn)
model_nn

#make prediction on train set
train_nn<-predict(model_nn,data=train_norm)
table(train_nn,train_norm$outcome_deposit_yes)

#check accuracy on train set
mean(train_nn==train_norm$outcome_deposit_yes)

#make prediction on test set
test_nn<-predict(model_nn,newdata=test_norm)
head(test_nn)
table(test_nn,test_norm$outcome_deposit_yes)

#check accuracy on test set
mean(test_nn==test_norm$outcome_deposit_yes)

#we got 90.8% accuracy on train set and 88.7% accuracy on test data

#7. Naive Bayes

#fit model
model_nb<-train(outcome_deposit_yes~.,data=train_norm,method="naive_bayes",trControl=control1)

#summary of model
summary(model_nb)
model_nb

#make prediction on train set
train_nb<-predict(model_nb,data=train_norm)
table(train_nb,train_norm$outcome_deposit_yes)

#check accuracy on train set
mean(train_nb==train_norm$outcome_deposit_yes)

#make prediction on test set
test_nb<-predict(model_nb,newdata=test_norm)
head(test_nb)
table(test_nb,test_norm$outcome_deposit_yes)

#check accuracy on test set
mean(test_nb==test_norm$outcome_deposit_yes)

#we got 88.5% accuracy on train set and 88.12% accuracy on test data

#8. Bagged CART(bagging)

#fit model
model_bc<-train(outcome_deposit_yes~.,data=train_norm,method="treebag",trControl=control1)

#summarize model
summary(model_bc)
model_bc

#make prediction on train set
train_bc<-predict(model_bc,data=train_norm)
table(train_bc,train_norm$outcome_deposit_yes)

#check accuracy on train set
mean(train_bc==train_norm$outcome_deposit_yes)

#make prediction on test set
test_bc<-predict(model_bc,newdata=test_norm)
head(test_bc)
table(test_bc,test_norm$outcome_deposit_yes)

#check accuracy on test set
mean(test_bc==test_norm$outcome_deposit_yes)

#we got 99.8% accuracy on train set and 88.93% accuracy on test data

#9. Boosted CART(boosting)

#fit model
model_boc<-train(outcome_deposit_yes~.,data=train_norm,method="ada",trControl=control1)

#summary of model
summary(model_boc)
model_boc

#make prediction on train set
train_boc<-predict(model_boc,data=train_norm)
table(train_boc,train_norm$outcome_deposit_yes)

#check accuracy on train set
mean(train_boc==train_norm$outcome_deposit_yes)

#make prediction on test set
test_boc<-predict(model_boc,newdata=test_norm)
head(test_boc)
table(test_boc,test_norm$outcome_deposit_yes)

#check accuracy on test set
mean(test_boc==test_norm$outcome_deposit_yes)

#we got 90.8% accuracy on train set and 89.1% accuracy on test data










