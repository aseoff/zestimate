#Author: Jesse Aseoff, Spencer Carlson, Sindhu Iyengar
#Subject: MGSC310 - Final
#Desc. LA Models
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
data_LA = read.csv('data_LA_sample.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))

#remove the ID
data_LA = data_LA[,-1]

#------------------------------------------------------------------

#split data randomly------------------------------------------------------------------------------------
split <- 0.8
training_index <- sample(1:20000, split * 20000)

#================
#LA Models

data_LA$hashottuborspa = as.factor(data_LA$hashottuborspa)
data_LA$poolcnt = as.factor(data_LA$poolcnt)

training_LA <- data_LA[training_index,] 
testing_LA <- data_LA[-training_index,]

data_types <- sapply(data_LA, class)
data_types

write.csv(training_LA, "data_model1_LA.csv")

#------------------
#Model 1: Decision tree using all data (no clusters)

tree_model1_LA <- rpart(formula = taxvaluedollarcnt ~ ., data = training_LA, method = "anova")
rpart.plot(x = tree_model1_LA, yesno = 2, type = 0, extra = 0)
fancyRpartPlot(tree_model1_LA, caption = NULL)

#use model to predict training
tree_model1_LA_pred_training <- predict(tree_model1_LA, newdata = training_LA)

#calc RMSE 
rmse(actual = training_LA$taxvaluedollarcnt, predicted = tree_model1_LA_pred_training)

#use model to predict testing
tree_model1_LA_pred_testing <- predict(tree_model1_LA, newdata = testing_LA)

#calc RMSE (testing)
rmse(actual = testing_LA$taxvaluedollarcnt, predicted = tree_model1_LA_pred_testing)

#------------------
#K Means Clustering (preparation for models 2 and 3)

data_LA_cluster = select(data_LA, c(taxvaluedollarcnt, Longitude, Latitude))

#shrink down data just to see visualization of clusters
#data_LA_clusterplot = data_LA_cluster[1:3000,]

#fviz_nbclust(data_LA_clusterplot, kmeans, method = 'wss')
#fviz_nbclust(data_LA_clusterplot, kmeans, method = 'silhouette') #alternative method to find k

km_LA <- kmeans(data_LA_cluster, 2, nstart= 20)
summary(km_LA)
#fviz_cluster(km_LA, data = data_LA_cluster)

data_LA_for_cluster_models = data_LA
data_LA_for_cluster_models$cluster = km_LA$cluster
data_LA_for_cluster_models$cluster = as.factor(data_LA_for_cluster_models$cluster)
data_LA_for_cluster_models$poolcnt = as.factor(data_LA_for_cluster_models$poolcnt)
data_LA_for_cluster_models$hashottuborspa = as.factor(data_LA_for_cluster_models$hashottuborspa)

data_types <- sapply(data_LA_for_cluster_models, class)
data_types

#Decision tree to predict cluster (used in models 2 and 3)
#-------

data_for_LA_cluster_tree = select(data_LA_for_cluster_models, -c(taxvaluedollarcnt))

training_LA_cluster_tree <- data_for_LA_cluster_tree[training_index,] 

write.csv(training_LA_cluster_tree, "data_LA_predict_cluster.csv")

testing_LA_cluster_tree <- data_for_LA_cluster_tree[-training_index,]


#decision tree to predict cluster (without tax value)
tree_LA_cluster = rpart(cluster ~., data = training_LA_cluster_tree, method = 'class')
rpart.plot(x = tree_LA_cluster, yesno = 2, type = 0, extra = 0)
fancyRpartPlot(tree_LA_cluster, caption = NULL)

#use model to predict training
tree_cat_LA_pred_training <- predict(tree_LA_cluster, newdata = training_LA_cluster_tree, type = 'class')

#create confusion matrix (training data)
tree_cat_LA_train_conMat_training <- confusionMatrix(data = tree_cat_LA_pred_training, reference = training_LA_cluster_tree$cluster)
tree_cat_LA_train_conMat_training
tree_cat_LA_train_conMat_training$overall[1]

#use model to predict testing
tree_cat_LA_pred_testing <- predict(tree_LA_cluster, newdata = testing_LA_cluster_tree, type = 'class')

#create confusion matrix (testing data)
tree_cat_LA_train_conMat_testing <- confusionMatrix(data = tree_cat_LA_pred_testing, reference = testing_LA_cluster_tree$cluster)
tree_cat_LA_train_conMat_testing
tree_cat_LA_train_conMat_testing$overall[1]

#------------------
#Model 2: Decision tree using cluster data

data_for_model2_LA = data_LA_for_cluster_models
data_types = sapply(data_for_model2_LA, "class")
data_types
training_LA_model2_set <- data_for_model2_LA[training_index,] 
write.csv(training_LA_model2_set, "data_model2_LA.csv")
testing_LA_model2_set <- data_for_model2_LA[-training_index,]

tree_model2_LA <- rpart(formula = taxvaluedollarcnt ~ ., data = training_LA_model2_set, method = "anova")
rpart.plot(x = tree_model2_LA, yesno = 2, type = 0, extra = 0)
fancyRpartPlot(tree_model2_LA, caption = NULL)


#use model to predict training
tree_model2_LA_pred_training <- predict(tree_model2_LA, newdata = training_LA_model2_set)

#calc RMSE 
rmse(actual = training_LA_model2_set$taxvaluedollarcnt, predicted = tree_model2_LA_pred_training)

#use model to predict testing
tree_model2_LA_pred_testing <- predict(tree_model2_LA, newdata = testing_LA_model2_set)

#calc RMSE (testing)
rmse(actual = testing_LA_model2_set$taxvaluedollarcnt, predicted = tree_model2_LA_pred_testing)

#------------------
#Model 3: Simple linear regression using cluster data

data_for_model3_LA = select(data_LA_for_cluster_models, -c(Latitude, Longitude))
data_types <- sapply(data_for_model3_LA, class)
data_types

#convert factor variables to numeric for this model
?as.numeric
data_for_model3_LA$hashottuborspa = as.numeric(data_for_model3_LA$hashottuborspa)
data_for_model3_LA$poolcnt = as.numeric(data_for_model3_LA$poolcnt)


#data_cleaner$hashottuborspa[is.na(data_cleaner$hashottuborspa)] = 0

data_for_model3_LA$poolcnt[data_for_model3_LA$poolcnt == 1] = 0
data_for_model3_LA$poolcnt[data_for_model3_LA$poolcnt == 2] = 1
data_for_model3_LA$hashottuborspa[data_for_model3_LA$hashottuborspa == 1] = 0
data_for_model3_LA$hashottuborspa[data_for_model3_LA$hashottuborspa == 2] = 1

#convert cluster to dummy variable for this dataset
data_for_model3_LA = dummy_cols(
  data_for_model3_LA,
  select_columns = c("cluster"),
  remove_first_dummy = TRUE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = NULL,
  remove_selected_columns = TRUE
)

data_types <- sapply(data_for_model3_LA, class)
data_types

write.csv(data_for_model3_LA, "data_model3_LA.csv")

#Correlation Matrix
cor_matrix_LA_3 = cor(data_for_model3_LA)
corrplot::corrplot(cor_matrix_LA_3, method = "number", tl.cex = .8, number.cex = .6)

#run a regsubsets to see linear model drivers
reg_sub_LA = regsubsets(taxvaluedollarcnt ~ ., data = data_for_model3_LA)
summary(reg_sub_LA)
plot(reg_sub_LA, scale = "adjr2")

#split data randomly------------------------------------------------------------------------------------
training_LA_model3_set <- data_for_model3_LA[training_index,] 
testing_LA_model3_set <- data_for_model3_LA[-training_index,]

training_LA_model3 = lm(taxvaluedollarcnt ~ cluster_2 + calculatedfinishedsquarefeet + bedroomcnt + calculatedbathnbr + poolcnt + yearbuilt, data = training_LA_model3_set)
summary(training_LA_model3)
RMSE_training = sqrt(mean((training_LA_model3_set$taxvaluedollarcnt - predict(training_LA_model3, training_LA_model3_set))^2))
RMSE_training

testing_LA_model3 = lm(taxvaluedollarcnt ~ cluster_2 + calculatedfinishedsquarefeet + bedroomcnt + calculatedbathnbr + poolcnt + yearbuilt, data = testing_LA_model3_set)
summary(testing_LA_model3)
RMSE_testing = sqrt(mean((testing_LA_model3_set$taxvaluedollarcnt - predict(testing_LA_model3, testing_LA_model3_set))^2))
RMSE_testing

LA_model3 =  lm(taxvaluedollarcnt ~ cluster_2 + calculatedfinishedsquarefeet + bedroomcnt + calculatedbathnbr + poolcnt + yearbuilt, data = data_for_model3_LA)
summary(LA_model3)
RMSE = sqrt(mean((data_for_model3_LA$taxvaluedollarcnt - predict(LA_model3, data_for_model3_LA))^2))
RMSE
#===============================================================================================================

