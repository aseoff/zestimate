#Author: Jesse Aseoff, Spencer Carlson, Sindhu Iyengar
#Subject: MGSC310 - Final
#Desc. VC Models
#delete environment

#KEY:
#Model 1: Decision tree using all data (no clusters)
#Model 2: Decision tree using cluster data
#Model 3: Simple linear regression using cluster data

rm(list = ls())
par(mfrow=c(1,1))

#set working directory to current folder 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
set.seed(12345)

# libraries --------------------------------------------------------------------------------------
library(dplyr)
library(corrplot)
library(ggplot2)
library(GGally)
library(reshape)
library(stringr)
library(caret)
library(ISLR)
library(leaps)
library(VIM)
library(bnstruct)
library(class)
library(Metrics)
library(rpart)
library(rpart.plot)
library(randomForest)
library(factoextra)
library(fastDummies)
library(rattle)

# load dataset------------------------------------------------------------------------------------
data_VC = read.csv('data_VC_sample.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))

#remove the ID
data_VC = data_VC[,-1]

#------------------------------------------------------------------

#split data randomly------------------------------------------------------------------------------------
split <- 0.8
training_index <- sample(1:20000, split * 20000)

#================
#VC Models

data_VC$hashottuborspa = as.factor(data_VC$hashottuborspa)
data_VC$poolcnt = as.factor(data_VC$poolcnt)

training_VC <- data_VC[training_index,] 
testing_VC <- data_VC[-training_index,]

data_types <- sapply(data_VC, class)
data_types

write.csv(training_VC, "data_model1_VC.csv")

#------------------
#Model 1: Decision tree using all data (no clusters)

tree_model1_VC <- rpart(formula = taxvaluedollarcnt ~ ., data = training_VC, method = "anova")
rpart.plot(x = tree_model1_VC, yesno = 2, type = 0, extra = 0)
fancyRpartPlot(tree_model1_VC, caption = NULL)

#use model to predict training
tree_model1_VC_pred_training <- predict(tree_model1_VC, newdata = training_VC)

#calc RMSE 
rmse(actual = training_VC$taxvaluedollarcnt, predicted = tree_model1_VC_pred_training)

#use model to predict testing
tree_model1_VC_pred_testing <- predict(tree_model1_VC, newdata = testing_VC)

#calc RMSE (testing)
rmse(actual = testing_VC$taxvaluedollarcnt, predicted = tree_model1_VC_pred_testing)

#------------------
#K Means Clustering (preparation for models 2 and 3)

data_VC_cluster = select(data_VC, c(taxvaluedollarcnt, Longitude, Latitude))

#shrink down data just to see visualization of clusters
#data_VC_clusterplot = data_VC_cluster[1:3000,]

#fviz_nbclust(data_VC_clusterplot, kmeans, method = 'wss')
#fviz_nbclust(data_VC_clusterplot, kmeans, method = 'silhouette') #alternative method to find k

km_VC <- kmeans(data_VC_cluster, 2, nstart= 20)
summary(km_VC)
#fviz_cluster(km_VC, data = data_VC_cluster)

data_VC_for_cluster_models = data_VC
data_VC_for_cluster_models$cluster = km_VC$cluster
data_VC_for_cluster_models$cluster = as.factor(data_VC_for_cluster_models$cluster)
data_VC_for_cluster_models$poolcnt = as.factor(data_VC_for_cluster_models$poolcnt)
data_VC_for_cluster_models$hashottuborspa = as.factor(data_VC_for_cluster_models$hashottuborspa)

data_types <- sapply(data_VC_for_cluster_models, class)
data_types

#Decision tree to predict cluster (used in models 2 and 3)
#-------

data_for_VC_cluster_tree = select(data_VC_for_cluster_models, -c(taxvaluedollarcnt))

training_VC_cluster_tree <- data_for_VC_cluster_tree[training_index,] 

write.csv(training_VC_cluster_tree, "data_VC_predict_cluster.csv")

testing_VC_cluster_tree <- data_for_VC_cluster_tree[-training_index,]


#decision tree to predict cluster (without tax value)
tree_VC_cluster = rpart(cluster ~., data = training_VC_cluster_tree, method = 'class')
rpart.plot(x = tree_VC_cluster, yesno = 2, type = 0, extra = 0)
fancyRpartPlot(tree_VC_cluster, caption = NULL)

#use model to predict training
tree_cat_VC_pred_training <- predict(tree_VC_cluster, newdata = training_VC_cluster_tree, type = 'class')

#create confusion matrix (training data)
tree_cat_VC_train_conMat_training <- confusionMatrix(data = tree_cat_VC_pred_training, reference = training_VC_cluster_tree$cluster)
tree_cat_VC_train_conMat_training
tree_cat_VC_train_conMat_training$overall[1]

#use model to predict testing
tree_cat_VC_pred_testing <- predict(tree_VC_cluster, newdata = testing_VC_cluster_tree, type = 'class')

#create confusion matrix (testing data)
tree_cat_VC_train_conMat_testing <- confusionMatrix(data = tree_cat_VC_pred_testing, reference = testing_VC_cluster_tree$cluster)
tree_cat_VC_train_conMat_testing
tree_cat_VC_train_conMat_testing$overall[1]

#------------------
#Model 2: Decision tree using cluster data

data_for_model2_VC = data_VC_for_cluster_models
data_types = sapply(data_for_model2_VC, "class")
data_types
training_VC_model2_set <- data_for_model2_VC[training_index,] 
write.csv(training_VC_model2_set, "data_model2_VC.csv")
testing_VC_model2_set <- data_for_model2_VC[-training_index,]

tree_model2_VC <- rpart(formula = taxvaluedollarcnt ~ ., data = training_VC_model2_set, method = "anova")
rpart.plot(x = tree_model2_VC, yesno = 2, type = 0, extra = 0)
fancyRpartPlot(tree_model2_VC, caption = NULL)


#use model to predict training
tree_model2_VC_pred_training <- predict(tree_model2_VC, newdata = training_VC_model2_set)

#calc RMSE 
rmse(actual = training_VC_model2_set$taxvaluedollarcnt, predicted = tree_model2_VC_pred_training)

#use model to predict testing
tree_model2_VC_pred_testing <- predict(tree_model2_VC, newdata = testing_VC_model2_set)

#calc RMSE (testing)
rmse(actual = testing_VC_model2_set$taxvaluedollarcnt, predicted = tree_model2_VC_pred_testing)

#------------------
#Model 3: Simple linear regression using cluster data

data_for_model3_VC = select(data_VC_for_cluster_models, -c(Latitude, Longitude))
data_types <- sapply(data_for_model3_VC, class)
data_types

#convert factor variables to numeric for this model
?as.numeric
data_for_model3_VC$hashottuborspa = as.numeric(data_for_model3_VC$hashottuborspa)
data_for_model3_VC$poolcnt = as.numeric(data_for_model3_VC$poolcnt)


#data_cleaner$hashottuborspa[is.na(data_cleaner$hashottuborspa)] = 0

data_for_model3_VC$poolcnt[data_for_model3_VC$poolcnt == 1] = 0
data_for_model3_VC$poolcnt[data_for_model3_VC$poolcnt == 2] = 1
data_for_model3_VC$hashottuborspa[data_for_model3_VC$hashottuborspa == 1] = 0
data_for_model3_VC$hashottuborspa[data_for_model3_VC$hashottuborspa == 2] = 1

#convert cluster to dummy variable for this dataset
data_for_model3_VC = dummy_cols(
  data_for_model3_VC,
  select_columns = c("cluster"),
  remove_first_dummy = TRUE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = NULL,
  remove_selected_columns = TRUE
)

data_types <- sapply(data_for_model3_VC, class)
data_types

write.csv(data_for_model3_VC, "data_model3_VC.csv")

#Correlation Matrix
cor_matrix_VC_3 = cor(data_for_model3_VC)
corrplot::corrplot(cor_matrix_VC_3, method = "number", tl.cex = .8, number.cex = .6)

#run a regsubsets to see linear model drivers
reg_sub_VC = regsubsets(taxvaluedollarcnt ~ ., data = data_for_model3_VC)
summary(reg_sub_VC)
plot(reg_sub_VC, scale = "adjr2")

#split data randomly------------------------------------------------------------------------------------
training_VC_model3_set <- data_for_model3_VC[training_index,] 
testing_VC_model3_set <- data_for_model3_VC[-training_index,]

training_VC_model3 = lm(taxvaluedollarcnt ~ cluster_2 + calculatedfinishedsquarefeet + bedroomcnt + calculatedbathnbr + poolcnt + yearbuilt, data = training_VC_model3_set)
summary(training_VC_model3)
RMSE_training = sqrt(mean((training_VC_model3_set$taxvaluedollarcnt - predict(training_VC_model3, training_VC_model3_set))^2))
RMSE_training

testing_VC_model3 = lm(taxvaluedollarcnt ~ cluster_2 + calculatedfinishedsquarefeet + bedroomcnt + calculatedbathnbr + poolcnt + yearbuilt, data = testing_VC_model3_set)
summary(testing_VC_model3)
RMSE_testing = sqrt(mean((testing_VC_model3_set$taxvaluedollarcnt - predict(testing_VC_model3, testing_VC_model3_set))^2))
RMSE_testing

VC_model3 =  lm(taxvaluedollarcnt ~ cluster_2 + calculatedfinishedsquarefeet + bedroomcnt + calculatedbathnbr + poolcnt + yearbuilt, data = data_for_model3_VC)
summary(VC_model3)
RMSE = sqrt(mean((data_for_model3_VC$taxvaluedollarcnt - predict(VC_model3, data_for_model3_VC))^2))
RMSE
#===============================================================================================================

