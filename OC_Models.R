#Author: Jesse Aseoff, Spencer Carlson, Sindhu Iyengar
#Subject: MGSC310 - Final
#Desc. OC Models
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
data_OC = read.csv('data_OC_sample.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))

#remove the ID
data_OC = data_OC[,-1]

#------------------------------------------------------------------

#split data randomly------------------------------------------------------------------------------------
split <- 0.8
training_index <- sample(1:20000, split * 20000)

#================
#OC Models

data_OC$hashottuborspa = as.factor(data_OC$hashottuborspa)
data_OC$poolcnt = as.factor(data_OC$poolcnt)

training_OC <- data_OC[training_index,] 
testing_OC <- data_OC[-training_index,]

data_types <- sapply(data_OC, class)
data_types

write.csv(training_OC, "data_model1_OC.csv")

#------------------
#Model 1: Decision tree using all data (no clusters)

tree_model1_OC <- rpart(formula = taxvaluedollarcnt ~ ., data = training_OC, method = "anova")
rpart.plot(x = tree_model1_OC, yesno = 2, type = 0, extra = 0)
fancyRpartPlot(tree_model1_OC, caption = NULL)

#use model to predict training
tree_model1_OC_pred_training <- predict(tree_model1_OC, newdata = training_OC)

#calc RMSE 
rmse(actual = training_OC$taxvaluedollarcnt, predicted = tree_model1_OC_pred_training)

#use model to predict testing
tree_model1_OC_pred_testing <- predict(tree_model1_OC, newdata = testing_OC)

#calc RMSE (testing)
rmse(actual = testing_OC$taxvaluedollarcnt, predicted = tree_model1_OC_pred_testing)

#------------------
#K Means Clustering (preparation for models 2 and 3)

data_OC_cluster = select(data_OC, c(taxvaluedollarcnt, Longitude, Latitude))

#shrink down data just to see visualization of clusters
#data_OC_clusterplot = data_OC_cluster[1:3000,]

#fviz_nbclust(data_OC_clusterplot, kmeans, method = 'wss')
#fviz_nbclust(data_OC_clusterplot, kmeans, method = 'silhouette') #alternative method to find k

km_OC <- kmeans(data_OC_cluster, 2, nstart= 20)
summary(km_OC)
#fviz_cluster(km_OC, data = data_OC_cluster)

data_OC_for_cluster_models = data_OC
data_OC_for_cluster_models$cluster = km_OC$cluster
data_OC_for_cluster_models$cluster = as.factor(data_OC_for_cluster_models$cluster)
data_OC_for_cluster_models$poolcnt = as.factor(data_OC_for_cluster_models$poolcnt)
data_OC_for_cluster_models$hashottuborspa = as.factor(data_OC_for_cluster_models$hashottuborspa)

data_types <- sapply(data_OC_for_cluster_models, class)
data_types

#Decision tree to predict cluster (used in models 2 and 3)
#-------

data_for_OC_cluster_tree = select(data_OC_for_cluster_models, -c(taxvaluedollarcnt))

training_OC_cluster_tree <- data_for_OC_cluster_tree[training_index,] 

write.csv(training_OC_cluster_tree, "data_OC_predict_cluster.csv")

testing_OC_cluster_tree <- data_for_OC_cluster_tree[-training_index,]


#decision tree to predict cluster (without tax value)
tree_OC_cluster = rpart(cluster ~., data = training_OC_cluster_tree, method = 'class')
rpart.plot(x = tree_OC_cluster, yesno = 2, type = 0, extra = 0)
fancyRpartPlot(tree_OC_cluster, caption = NULL)

#use model to predict training
tree_cat_OC_pred_training <- predict(tree_OC_cluster, newdata = training_OC_cluster_tree, type = 'class')

#create confusion matrix (training data)
tree_cat_OC_train_conMat_training <- confusionMatrix(data = tree_cat_OC_pred_training, reference = training_OC_cluster_tree$cluster)
tree_cat_OC_train_conMat_training
tree_cat_OC_train_conMat_training$overall[1]

#use model to predict testing
tree_cat_OC_pred_testing <- predict(tree_OC_cluster, newdata = testing_OC_cluster_tree, type = 'class')

#create confusion matrix (testing data)
tree_cat_OC_train_conMat_testing <- confusionMatrix(data = tree_cat_OC_pred_testing, reference = testing_OC_cluster_tree$cluster)
tree_cat_OC_train_conMat_testing
tree_cat_OC_train_conMat_testing$overall[1]

#------------------
#Model 2: Decision tree using cluster data

data_for_model2_OC = data_OC_for_cluster_models
data_types = sapply(data_for_model2_OC, "class")
data_types
training_OC_model2_set <- data_for_model2_OC[training_index,] 
write.csv(training_OC_model2_set, "data_model2_OC.csv")
testing_OC_model2_set <- data_for_model2_OC[-training_index,]

tree_model2_OC <- rpart(formula = taxvaluedollarcnt ~ ., data = training_OC_model2_set, method = "anova")
rpart.plot(x = tree_model2_OC, yesno = 2, type = 0, extra = 0)
fancyRpartPlot(tree_model2_OC, caption = NULL)


#use model to predict training
tree_model2_OC_pred_training <- predict(tree_model2_OC, newdata = training_OC_model2_set)

#calc RMSE 
rmse(actual = training_OC_model2_set$taxvaluedollarcnt, predicted = tree_model2_OC_pred_training)

#use model to predict testing
tree_model2_OC_pred_testing <- predict(tree_model2_OC, newdata = testing_OC_model2_set)

#calc RMSE (testing)
rmse(actual = testing_OC_model2_set$taxvaluedollarcnt, predicted = tree_model2_OC_pred_testing)

#------------------
#Model 3: Simple linear regression using cluster data

data_for_model3_OC = select(data_OC_for_cluster_models, -c(Latitude, Longitude))
data_types <- sapply(data_for_model3_OC, class)
data_types

#convert factor variables to numeric for this model
?as.numeric
data_for_model3_OC$hashottuborspa = as.numeric(data_for_model3_OC$hashottuborspa)
data_for_model3_OC$poolcnt = as.numeric(data_for_model3_OC$poolcnt)


#data_cleaner$hashottuborspa[is.na(data_cleaner$hashottuborspa)] = 0

data_for_model3_OC$poolcnt[data_for_model3_OC$poolcnt == 1] = 0
data_for_model3_OC$poolcnt[data_for_model3_OC$poolcnt == 2] = 1
data_for_model3_OC$hashottuborspa[data_for_model3_OC$hashottuborspa == 1] = 0
data_for_model3_OC$hashottuborspa[data_for_model3_OC$hashottuborspa == 2] = 1

#convert cluster to dummy variable for this dataset
data_for_model3_OC = dummy_cols(
  data_for_model3_OC,
  select_columns = c("cluster"),
  remove_first_dummy = TRUE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = NULL,
  remove_selected_columns = TRUE
)

data_types <- sapply(data_for_model3_OC, class)
data_types

write.csv(data_for_model3_OC, "data_model3_OC.csv")

#Correlation Matrix
cor_matrix_OC_3 = cor(data_for_model3_OC)
corrplot::corrplot(cor_matrix_OC_3, method = "number", tl.cex = .8, number.cex = .6)

#run a regsubsets to see linear model drivers
reg_sub_OC = regsubsets(taxvaluedollarcnt ~ ., data = data_for_model3_OC)
summary(reg_sub_OC)
plot(reg_sub_OC, scale = "adjr2")

#split data randomly------------------------------------------------------------------------------------
training_OC_model3_set <- data_for_model3_OC[training_index,] 
testing_OC_model3_set <- data_for_model3_OC[-training_index,]

training_OC_model3 = lm(taxvaluedollarcnt ~ cluster_2 + calculatedfinishedsquarefeet + bedroomcnt + calculatedbathnbr + poolcnt + yearbuilt, data = training_OC_model3_set)
summary(training_OC_model3)
RMSE_training = sqrt(mean((training_OC_model3_set$taxvaluedollarcnt - predict(training_OC_model3, training_OC_model3_set))^2))
RMSE_training

testing_OC_model3 = lm(taxvaluedollarcnt ~ cluster_2 + calculatedfinishedsquarefeet + bedroomcnt + calculatedbathnbr + poolcnt + yearbuilt, data = testing_OC_model3_set)
summary(testing_OC_model3)
RMSE_testing = sqrt(mean((testing_OC_model3_set$taxvaluedollarcnt - predict(testing_OC_model3, testing_OC_model3_set))^2))
RMSE_testing

OC_model3 =  lm(taxvaluedollarcnt ~ cluster_2 + calculatedfinishedsquarefeet + bedroomcnt + calculatedbathnbr + poolcnt + yearbuilt, data = data_for_model3_OC)
summary(OC_model3)
RMSE = sqrt(mean((data_for_model3_OC$taxvaluedollarcnt - predict(OC_model3, data_for_model3_OC))^2))
RMSE
#===============================================================================================================

