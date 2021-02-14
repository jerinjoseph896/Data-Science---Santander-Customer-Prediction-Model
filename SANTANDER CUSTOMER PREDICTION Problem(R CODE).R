rm(list=ls(all=T)) 
library(ggplot2)
library(tidyverse)
library(DataExplorer)
library(moments)
library(C50)
library(glmnet)
library(pROC)
library(lightgbm)




#load the data
setwd("C:/Users/jerin/Desktop/R work/EDWISOR PROJECT")
train_data= read.csv("train.csv", header = T)
test_data=read.csv("test.csv", header = T)
head(train_data)


#Dimension of train_data and test_data data
dim(train_data)
dim(test_data)

#storing ID_code  of test_data train_data data 
train_data_ID_code_orignal = train_data$ID_code
test_data_ID_code_orignal  = test_data$ID_code

#removing Idcode from orginal dataset 
train_data$ID_code=NULL
test_data$ID_code=NULL

#check dimension of dataset after removing column
(dim(train_data))
(dim(test_data))

#convert to factor
train_data$target<-as.factor(train_data$target)


#Target Value Distribution
table(train_data$target)/length(train_data$target)*100

plot1<-ggplot(train_data,aes(target))+theme_bw()+geom_bar(stat='count',fill='blue',alpha=0.5)
plot1


# Missing Value Analysis
missing_train_data_values= (apply(train_data,2,function(x)sum(is.na(x))))
sum(missing_train_data_values)
missing_test_data_values = (apply(test_data,2,function(x)sum(is.na(x))))
sum(missing_test_data_values)

#Summary of the dataset
str(train_data)
str(test_data)


########## Visualisations ###################

#Distribution of train_data attributes from 3 to 102
for (var in names(train_data)[c(3:102)]){
  target<-train_data$target
  plot<-ggplot(train_data, aes(x=.data[[var]],fill=target)) +
    geom_density(kernel='gaussian',color = "blue") + ggtitle(var)+theme_light()
  print(plot)
}

#Distribution of train_data attributes from 103 to 202
for (var in names(train_data)[c(103:202)]){
  target<-train_data$target
  plot<-ggplot(train_data, aes(x=.data[[var]], fill=target)) +
    geom_density(kernel='gaussian',color="red") + ggtitle(var)+theme_light()
  print(plot)
}


#Distribution of test_data attributes from 2 to 101
plot_density(test_data[,c(2:101)], ggtheme = theme_classic(),geom_density_args = list(color='red'))

#Distribution of test_data attributes from 102 to 201
plot_density(test_data[,c(102:201)], ggtheme = theme_classic(),geom_density_args = list(color='red'))


#Applying the function to find mean values per row in train_data and test_data data.
train_data_mean<-(apply(train_data[,-c(1,2)],MARGIN=1,FUN=mean))
test_data_mean<-(apply(test_data[,-c(1)],MARGIN=1,FUN=mean))
ggplot()+
  #Distribution of mean values per row in train_data data
  geom_density(data=train_data[,-c(1,2)],aes(x=train_data_mean),kernel='gaussian',show.legend=TRUE,fill="black")+theme_light()+
  #Distribution of mean values per row in test_data data
  geom_density(data=test_data[,-c(1)],aes(x=test_data_mean),kernel='gaussian',show.legend=TRUE,fill="#808000",alpha=0.5)+
  labs(x='mean values per row',title="Distribution of mean values per row in train_data and test_data")




#Applying the function to find mean values per column in train_data and test_data data.
train_data_mean<-(apply(train_data[,-c(1,2)],MARGIN=2,FUN=mean))
test_data_mean<-apply(test_data[,-c(1)],MARGIN=2,FUN=mean)
ggplot()+
  #Distribution of mean values per column in train_data data
  geom_density(aes(x=train_data_mean),kernel='gaussian',show.legend=TRUE,fill="black")+theme_light()+
  #Distribution of mean values per column in test_data data
  geom_density(aes(x=test_data_mean),kernel='gaussian',show.legend=TRUE,fill="#808000")+
  labs(x='mean values per column',title="Distribution of mean values per col in train_data and test_data")

#Applying the function to find standard deviation values per row in train_data and test_data.
train_data_sd<-apply(train_data[,-c(1,2)],MARGIN=1,FUN=sd)
test_data_sd<-apply(test_data[,-c(1)],MARGIN=1,FUN=sd)
ggplot()+
  #Distribution of sd values per row in train_data data
  geom_density(data=train_data[,-c(1,2)],aes(x=train_data_sd),kernel='gaussian',show.legend=TRUE,fill='black')+theme_light()+
  #Distribution of mean values per row in test_data data
  geom_density(data=test_data[,-c(1)],aes(x=test_data_sd),kernel='gaussian',show.legend=TRUE,fill='red',alpha=0.5)+
  labs(x='sd values per row',title="Distribution of sd values per row in train_data and test_data")

#Applying the function to find sd values per column in train_data and test_data data.
train_data_sd<-apply(train_data[,-c(1,2)],MARGIN=2,FUN=sd)
test_data_sd<-apply(test_data[,-c(1)],MARGIN=2,FUN=sd)
ggplot()+
  #Distribution of sd values per row in train_data data
  geom_density(aes(x=train_data_sd),kernel='gaussian',show.legend=TRUE,fill='black')+theme_light()+
  #Distribution of mean values per row in test_data data
  geom_density(aes(x=test_data_sd),kernel='gaussian',show.legend=TRUE,fill='red',alpha=0.7)+
  labs(x='sd values per column',title="Distribution of std values per column in train_data and test_data")

#Applying the function to find skewness values per row in train_data and test_data.
train_data_skew<-apply(train_data[,-c(1,2)],MARGIN=1,FUN=skewness)
test_data_skew<-apply(test_data[,-c(1)],MARGIN=1,FUN=skewness)
ggplot()+
  #Distribution of skewness values per row in train_data data
  geom_density(aes(x=train_data_skew),kernel='gaussian',show.legend=TRUE,fill='green')+theme_light()+
  #Distribution of skewness values per column in test_data data
  geom_density(aes(x=test_data_skew),kernel='gaussian',show.legend=TRUE,fill="#8b4513",alpha= 0.9)+
  labs(x='skewness values per row',title="Distribution of skewness values per row in train_data and test_data")

#Applying the function to find skewness values per column in train_data and test_data 
train_data_skew<-apply(train_data[,-c(1,2)],MARGIN=2,FUN=skewness)
test_data_skew<-apply(test_data[,-c(1)],MARGIN=2,FUN=skewness)
ggplot()+
  #Distribution of skewness values per column in train_data data
  geom_density(aes(x=train_data_skew),kernel='gaussian',show.legend=TRUE,fill='green')+theme_light()+
  #Distribution of skewness values per column in test_data data
  geom_density(aes(x=test_data_skew),kernel='gaussian',show.legend=TRUE,fill="#8b4513",alpha=0.9)+
  labs(x='skewness values per column',title="Distribution of skewness values per column in train_data and test_data")

#Applying the function to find kurtosis values per row in train_data and test_data.
train_data_kurtosis<-apply(train_data[,-c(1,2)],MARGIN=1,FUN=kurtosis)
test_data_kurtosis<-apply(test_data[,-c(1)],MARGIN=1,FUN=kurtosis)
ggplot()+
  #Distribution of kurtosis values per row in train_data data
  geom_density(aes(x=train_data_kurtosis),kernel='gaussian',show.legend=TRUE,fill='black')+theme_light()+
  #Distribution of kurtosis values per row in test_data data
  geom_density(aes(x=test_data_kurtosis),kernel='gaussian',show.legend=TRUE,fill='#005A9C',alpha=0.6)+
  labs(x='kurtosis values per row',title="Distribution of kurtosis values per row in train_data and test_data")

#Applying the function to find kurtosis values per column in train_data and test_data.
train_data_kurtosis<-apply(train_data[,-c(1,2)],MARGIN=2,FUN=kurtosis)
test_data_kurtosis<-apply(test_data[,-c(1)],MARGIN=2,FUN=kurtosis)
ggplot()+
  #Distribution of kurtosis values per column in train_data data
  geom_density(aes(x=train_data_kurtosis),kernel='gaussian',show.legend=TRUE,fill='black')+theme_light()+
  #Distribution of kurtosis values per column in test_data data
  geom_density(aes(x=test_data_kurtosis),kernel='gaussian',show.legend=TRUE,fill='#005A9C',alpha=0.6)+
  labs(x='kurtosis values per row',title="Distribution of kurtosis values column row in train_data and test_data")


#Correlations (method 1)
cormat <- cor(train_data[,-c(1,2)])
summary(cormat[upper.tri(cormat)]) #Correlations between features nearly zero.



#Correlations in train_data data(method 2)
#convert factor to int
train_data$target<-as.numeric(train_data$target)
train_data_correlations<-cor(train_data[,c(2:202)])
train_data_correlations


#Correlations in test_data data
test_data_correlations<-cor(test_data_df[,c(2:201)])
test_data_correlations



####### Modelling ###############

getmodel_accuracy=function(conf_matrix)
{
  model_parm =list()
  tn =conf_matrix[1,1]
  tp =conf_matrix[2,2]
  fp =conf_matrix[1,2]
  fn =conf_matrix[2,1]
  p =(tp)/(tp+fp)
  r =(fp)/(fp+tn)
  f1=2*((p*r)/(p+r))
  print(paste("accuracy",round((tp+tn)/(tp+tn+fp+fn),2)))
  print(paste("precision",round(p ,2)))
  print(paste("recall",round(r,2)))
}

##split the data into train and test
set.seed(1234)
require("caret")

train_data.index = createDataPartition(train_data$target, p = .80, list = FALSE)
train = train_data[ train_data.index,]
test  = train_data[-train_data.index,]

#dimension of train_data and validation data
dim(train)
dim(test)


##################### Logistic Regression Model ########################
logit_model =glm(target~. ,data =train ,family='binomial')
# model summary  
summary(logit_model)
#get model predicted  probality 
y_prob =predict(logit_model , test[,-1] ,type = 'response' )
# convert   probality to class according to thresshold
y_pred = ifelse(y_prob >0.5, 1, 0)
#create confusion matrix 
conf_matrix= table(test[,1] , y_pred)
#print model accuracy
getmodel_accuracy(conf_matrix)
confusionMatrix(conf_matrix)
# get auc 
roc=roc(test[,1], y_prob)
print(roc )
# plot roc _auc plot 
plot(roc ,main ="Logistic Regression Roc ")


############## Decision Tree ##############

#Develop Model on training data
C50_model = C5.0(target ~., train)

#Summary of DT model
summary(C50_model)


#Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-1],type='class')


##Evaluate the performance of classification model
ConfMatrix_C50 = table(test[,1], C50_Predictions)


#print model accuracy
getmodel_accuracy(ConfMatrix_C50)

#fucntion to calculate the different error metrics
confusionMatrix(ConfMatrix_C50)
#get Auc score 
C50_Predictions<-as.numeric(C50_Predictions)
roc=roc(test[,1], C50_Predictions )
print(roc)
# plot roc_auc curve 
plot(roc ,main="Decision tree Roc")



######### light gbm Model ########

X_train<-as.matrix(train[,-1])
y_train<-as.matrix(train$target)
X_valid<-as.matrix(test[,-1])
y_valid<-as.matrix(test$target)
test_set<-as.matrix(test_data[,-1])

#training data
lgb.train <- lgb.Dataset(data=X_train, label=y_train)
#Validation data
lgb.valid <- lgb.Dataset(data=X_valid,label=y_valid)

#Selecting best hyperparameters

lgb.grid = list(objective = "binary",
                metric = "auc",
                boost='gbdt',
                max_depth=-1,
                boost_from_average='false',
                min_sum_hessian_in_leaf = 12,
                feature_fraction = 0.05,
                bagging_fraction = 0.45,
                bagging_freq = 5,
                learning_rate=0.02,
                tree_learner='serial',
                num_leaves=20,
                num_threads=5,
                min_data_in_bin=150,
                min_gain_to_split = 30,
                min_data_in_leaf = 90,
                verbosity=-1,
                is_unbalance = TRUE)



lgbm.model <- lgb.train(params = lgb.grid, data = lgb.train, nrounds =10000,eval_freq =1000,
                        valids=list(val1=lgb.train,val2=lgb.valid),early_stopping_rounds = 5000)


#lgbm model performance on test data

lgbm_pred_prob <- predict(lgbm.model,as.matrix(test[,-1]))
print(lgbm_pred_prob)
#Convert to binary output (1 and 0) with threshold 0.5
lgbm_pred<-ifelse(lgbm_pred_prob>0.5,1,0)
print(lgbm_pred)

#create confusion matrix 
conf_matrix= table(test[,1] , lgbm_pred)
#print model accuracy
getmodel_accuracy(conf_matrix)
confusionMatrix(conf_matrix)

# get auc 
roc=roc(test[,1], lgbm_pred_prob)
print(roc )
# plot roc _auc plot 
plot(roc ,main ="Light Gradient Boost Roc ")
