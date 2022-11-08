#Author: Jesse Aseoff, Spencer Carlson, Sindhu Iyengar
#Subject: MGSC310 - Final
#Desc. Cleaning

#delete environment
rm(list = ls())
par(mfrow=c(1,1))

#set working directory to current folder 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

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
data_2017_raw = read.csv('properties_2017.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))

#display data types for each variable
data_types = sapply(data_2017_raw, class)
data_types

###### Observations ######
#58 variables and 2.9mn observations
#data types mostly consist of numeric or integer (categorical variables are displayed as numeric factors)
##########################

#establish y variable and delete na's from predictor variable column 
data_cleaner = data_2017_raw[!is.na(data_2017_raw$taxvaluedollarcnt),]
#removed about 3,000 observations, with a data set this large this is not a cause for concern


#total nas in entire dataset (all blanks were converted to NAs when read in)
sum_nas = sum(is.na(data_cleaner))
sum_nas
#83 mn na's, need to look further and see if there are columns we can remove

#this outputs the # of NAs for each individual variable.
for (i in 1:ncol(data_cleaner))
{
  print(colnames(data_cleaner[i]))
  print(sum(is.na(data_cleaner[,i])))
}


###### Observations ######
#29 columns that contain more than 1.5 mn na's
#Cannot assume all are invaluable
#Many columns seem repetitive and can be summed up by one or two columns
#Columns hottub/spa, poolcnt have millions of 0s and NAs, we will assume that na = 0 = nopool, no hottub/spa
##########################

#remove columns (see reasoning on chart)
data_cleaner = select(data_cleaner, c(bedroomcnt, calculatedbathnbr, calculatedfinishedsquarefeet, garagecarcnt, hashottuborspa, latitude, longitude, poolcnt, regionidcounty, regionidcity, regionidzip, yearbuilt, taxvaluedollarcnt, fips))

#removing garage car count for simplicity sake
#there are too many na's to try to predict
#we cannot assume that na's = 0 because zeros exist in the data
#too many zeros for this variable to be useful
data_cleaner = select(data_cleaner, -c(garagecarcnt))

#variable: hottub/spa
#Only true values, assumption that NA = 0 = no spa or hot tub
data_cleaner$hashottuborspa[is.na(data_cleaner$hashottuborspa)] = 0

#true values converted to 1s
data_cleaner$hashottuborspa[data_cleaner$hashottuborspa == "true"] = 1

#variable: poolcnt
#MAX and MIN poolcnt = 1, assumption that NA = 0 = no pool
data_cleaner$poolcnt[is.na(data_cleaner$poolcnt)] = 0

#convert hashottuborspa to numeric varialbe
data_cleaner$hashottuborspa = as.numeric(data_cleaner$hashottuborspa)

#remove remaining NAs
data_test1 = na.omit(data_cleaner)
#went from 2.95mn observations -> 2.81mn, this is a good sign that we didn't remove too much data

#rescale for z score manipulation
#this process will allow us to identify and remove outliers
data_test2 = select(data_test1, c(bedroomcnt, calculatedbathnbr, calculatedfinishedsquarefeet, yearbuilt, taxvaluedollarcnt, poolcnt, hashottuborspa, latitude, longitude, fips))

#new object containing z-scores of cleaned dataset
zscores = scale(data_test2)

#converting to a dataframe
zscores = as.data.frame(zscores)

#converting all numerical observations outside 3 standard deviations as NA values in our original dataset and will remove them 
data_test2$bedroomcnt[zscores$bedroomcnt > 3] = NA
data_test2$calculatedbathnbr[zscores$calculatedbathnbr > 3] = NA
data_test2$calculatedfinishedsquarefeet[zscores$calculatedfinishedsquarefeet > 3] = NA
data_test2$yearbuilt[zscores$yearbuilt > 3] = NA
data_test2$taxvaluedollarcnt[zscores$taxvaluedollarcnt > 3] = NA

sum(is.na(data_test2))
#we will lose 130k observations, this is OK with our large dataset

#new cleaned data object created
data_clean = na.omit(data_test2)

#Longitude and latitude observations are multiplied 1,000,000x so we will convert them to their standard notation
data_clean <- data_clean %>%
mutate(Longitude = longitude / 1000000) %>%
mutate(Latitude = latitude / 1000000)

data_clean = select(data_clean, -c(latitude, longitude))
#delete columns with old values

#get data_types
data_types = sapply(data_clean, class)
data_types
#data types consist of only numerical and integer

#FIPS is a unique identifier for COUNTY
#There are  counties - LA, Ventura, and Orange meaning that fips is a factor with 3 levels
#Convert fips to factor so that we can subest the data into 3 groups
data_clean$fips = as.factor(data_clean$fips)

data_types = sapply(data_clean, class)
data_types


#dataset for numerical values (with fips) removing longitude and latitude because were looking for linear relationships
data_clean_subsets = select(data_clean, -c(Latitude, Longitude))

#matrix
cor_matrix1 = cor(data_clean_subsets)
corrplot::corrplot(cor_matrix1, method = "number", tl.cex = .8, number.cex = .5)



reg_sub = regsubsets(taxvaluedollarcnt ~ ., data = data_clean_subsets)
#summary(reg_sub)
plot(reg_sub, scale = "adjr2")
#right now the adj r^2 is about .33, this is not great but understandably so because it is missing location - a factor we expect to contribute a lot in predicting value

#CREATE 3 datasets for clustering
#we are using a sample of 20,000 observations because we want to cluster within each location
#It would be way to computationally expensive to use the whole clean data set
#Our sample size of 20,000 is large enough to provide statistically significant results that represent the entire dataset

#split dataset into 3 - each dataset with only data from a single county
split_up_dataframe = split(data_clean, data_clean$fips)

#these are the three clean datasets (full - not sampled yet)
data_LA = as.data.frame(split_up_dataframe[1])
data_OC = as.data.frame(split_up_dataframe[2])
data_VC = as.data.frame(split_up_dataframe[3])

#samples created for each county
data_LA_sample = data_LA[sample(nrow(data_LA), 20000), ]
data_OC_sample = data_OC[sample(nrow(data_OC), 20000), ]
data_VC_sample = data_VC[sample(nrow(data_VC), 20000), ]

#remove the fips variable
data_LA_sample = data_LA_sample[,-8]
data_OC_sample = data_OC_sample[,-8]
data_VC_sample = data_VC_sample[,-8]

#saving files of our new subsetted datasets for simplicity, will be used for modeling, already done so I will comment this out
#write.csv(data_LA_sample, "data_LA_sample.csv")
#write.csv(data_OC_sample, "data_OC_sample.csv")
#write.csv(data_VC_sample, "data_VC_sample.csv")
#=================================================



?include_graphics

