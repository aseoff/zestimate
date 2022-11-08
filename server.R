library(shiny)
library(ISLR)
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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#==========================================================================================================================================================================================
#LA CODE for all models
data_LA_predict_cluster = read.csv('data_LA_predict_cluster.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))
data_LA_predict_cluster$poolcnt = as.factor(data_LA_predict_cluster$poolcnt)
data_LA_predict_cluster$hashottuborspa = as.factor(data_LA_predict_cluster$hashottuborspa)
data_LA_predict_cluster$cluster = as.factor(data_LA_predict_cluster$cluster)

data_model1_LA = read.csv('data_model1_LA.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))
data_model1_LA$poolcnt = as.factor(data_model1_LA$poolcnt)
data_model1_LA$hashottuborspa = as.factor(data_model1_LA$hashottuborspa)

data_model2_LA = read.csv('data_model2_LA.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))
data_model2_LA$poolcnt = as.factor(data_model2_LA$poolcnt)
data_model2_LA$hashottuborspa = as.factor(data_model2_LA$hashottuborspa)
data_model2_LA$cluster = as.factor(data_model2_LA$cluster)

data_model3_LA = read.csv('data_model3_LA.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))

predict_cluster_tree_LA = rpart(cluster ~., data = data_LA_predict_cluster, method = 'class')

LA_model1 = rpart(formula = taxvaluedollarcnt ~ ., data = data_model1_LA, method = "anova")

LA_model2 = rpart(formula = taxvaluedollarcnt ~ ., data = data_model2_LA, method = "anova")

LA_model3 =  lm(taxvaluedollarcnt ~ cluster_2 + calculatedfinishedsquarefeet + bedroomcnt + calculatedbathnbr + poolcnt + yearbuilt + cluster_2, data = data_model3_LA)
#==========================================================================================================================================================================================
#==========================================================================================================================================================================================
#OC CODE for all models
data_OC_predict_cluster = read.csv('data_OC_predict_cluster.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))
data_OC_predict_cluster$poolcnt = as.factor(data_OC_predict_cluster$poolcnt)
data_OC_predict_cluster$hashottuborspa = as.factor(data_OC_predict_cluster$hashottuborspa)
data_OC_predict_cluster$cluster = as.factor(data_OC_predict_cluster$cluster)

data_model1_OC = read.csv('data_model1_OC.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))
data_model1_OC$poolcnt = as.factor(data_model1_OC$poolcnt)
data_model1_OC$hashottuborspa = as.factor(data_model1_OC$hashottuborspa)

data_model2_OC = read.csv('data_model2_OC.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))
data_model2_OC$poolcnt = as.factor(data_model2_OC$poolcnt)
data_model2_OC$hashottuborspa = as.factor(data_model2_OC$hashottuborspa)
data_model2_OC$cluster = as.factor(data_model2_OC$cluster)

data_model3_OC = read.csv('data_model3_OC.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))

predict_cluster_tree_OC = rpart(cluster ~., data = data_OC_predict_cluster, method = 'class')

OC_model1 = rpart(formula = taxvaluedollarcnt ~ ., data = data_model1_OC, method = "anova")

OC_model2 = rpart(formula = taxvaluedollarcnt ~ ., data = data_model2_OC, method = "anova")

OC_model3 =  lm(taxvaluedollarcnt ~ cluster_2 + calculatedfinishedsquarefeet + bedroomcnt + calculatedbathnbr + poolcnt + yearbuilt + cluster_2, data = data_model3_OC)
#==========================================================================================================================================================================================
#==========================================================================================================================================================================================
#VC CODE for all models
data_VC_predict_cluster = read.csv('data_VC_predict_cluster.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))
data_VC_predict_cluster$poolcnt = as.factor(data_VC_predict_cluster$poolcnt)
data_VC_predict_cluster$hashottuborspa = as.factor(data_VC_predict_cluster$hashottuborspa)
data_VC_predict_cluster$cluster = as.factor(data_VC_predict_cluster$cluster)
data_VC_predict_cluster = select(data_VC_predict_cluster, -c(hashottuborspa))

data_model1_VC = read.csv('data_model1_VC.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))
data_model1_VC$poolcnt = as.factor(data_model1_VC$poolcnt)
data_model1_VC$hashottuborspa = as.factor(data_model1_VC$hashottuborspa)
data_model1_VC$hashottuborspa = as.factor(data_model1_VC$hashottuborspa)
data_model1_VC = select(data_model1_VC, -c(hashottuborspa))


data_model2_VC = read.csv('data_model2_VC.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))
data_model2_VC$poolcnt = as.factor(data_model2_VC$poolcnt)
data_model2_VC$hashottuborspa = as.factor(data_model2_VC$hashottuborspa)
data_model2_VC$cluster = as.factor(data_model2_VC$cluster)
data_model2_VC = select(data_model2_VC, -c(hashottuborspa))

data_model3_VC = read.csv('data_model3_VC.csv', stringsAsFactors = FALSE, na.strings=c("", "NA"))
data_model3_VC = select(data_model3_VC, -c(hashottuborspa))

predict_cluster_tree_VC = rpart(cluster ~., data = data_VC_predict_cluster, method = 'class')

VC_model1 = rpart(formula = taxvaluedollarcnt ~ ., data = data_model1_VC, method = "anova")

VC_model2 = rpart(formula = taxvaluedollarcnt ~ ., data = data_model2_VC, method = "anova")

VC_model3 =  lm(taxvaluedollarcnt ~ cluster_2 + calculatedfinishedsquarefeet + bedroomcnt + calculatedbathnbr + poolcnt + yearbuilt + cluster_2, data = data_model3_VC)
#==========================================================================================================================================================================================

# Define server logic for this application
shinyServer(function(input, output) {
  
  output$myTrans <- renderText({ input$Trans })
  
  
  #output county
  output$county <- renderText({ 
    input$actionButton
    isolate({
      # all variables
      county = as.characterinput$county
    })
  })
  
#==========================================================================================================================================================================================
#LA SERVER CODE
  #predict cluster
  output$cluster_prediction_LA <- renderText({ 
    input$actionButton
    isolate({
      # all variables
      newdata_cluster_LA = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, hashottuborspa = as.factor(input$hottuborspa), Latitude = input$latitude, Longitude = input$longitude, X=0.5)
      
      myp_LA_cluster = predict(predict_cluster_tree_LA, newdata_cluster_LA)
      
      cluster_prediction_LA = ifelse(myp_LA_cluster[1] >= 0.5, 1, 2)
            #output$myp_LA <- p[1]
    })
  })
  
  #model 1 - decision tree without cluster
  output$myp_LA1 <- renderText({ 
    input$actionButton
    isolate({
      # all variables
      newdata1_LA = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, hashottuborspa = as.factor(input$hottuborspa), Latitude = input$latitude, Longitude = input$longitude, X=0.5)
      myp_LA1 = predict(LA_model1, newdata1_LA)
      #output$myp_LA <- p[1]
    })
  })
  
  #model 2 - decision tree WITH CLUSTER
  output$myp_LA2 <- renderText({ 
    input$actionButton
    
    isolate({
      
      newdata_cluster_LA = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, hashottuborspa = as.factor(input$hottuborspa), Latitude = input$latitude, Longitude = input$longitude, X=0.5)
      myp_LA_cluster = predict(predict_cluster_tree_LA, newdata_cluster_LA)
      cluster_prediction_LA = ifelse(myp_LA_cluster[1] >= 0.5, 1, 2)
      
      # all variables
      newdata2_LA = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, hashottuborspa = as.factor(input$hottuborspa), Latitude = input$latitude, Longitude = input$longitude, cluster = as.factor(cluster_prediction_LA), X=0.5)
      myp_LA2 = predict(LA_model2, newdata2_LA)
      #output$myp_LA <- p[1]
    })
  })
  
  #model 3 - linear regression WITH CLUSTER
  output$myp_LA3 <- renderText({ 
    input$actionButton
    
    isolate({
      
      newdata_cluster_LA = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, hashottuborspa = as.factor(input$hottuborspa), Latitude = input$latitude, Longitude = input$longitude, X=0.5)
      myp_LA_cluster = predict(predict_cluster_tree_LA, newdata_cluster_LA)
      cluster_prediction_LA = ifelse(myp_LA_cluster[1] >= 0.5, 1, 2)
      
      # all variables
      newdata3_LA = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.numeric(input$pool), yearbuilt = input$YearBuilt, hashottuborspa = as.numeric(input$hottuborspa), Latitude = input$latitude, Longitude = input$longitude, cluster_2 = as.numeric(cluster_prediction_LA)-1, X=0.5)
      myp_LA3 = predict(LA_model3, newdata3_LA, interval = "predict")
      #output$myp_LA <- p[1]
    })
  })
#==========================================================================================================================================================================================

#==========================================================================================================================================================================================
#OC SERVER CODE
#predict cluster
output$cluster_prediction_OC <- renderText({ 
  input$actionButton
  isolate({
    # all variables
    newdata_cluster_OC = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, hashottuborspa = as.factor(input$hottuborspa), Latitude = input$latitude, Longitude = input$longitude, X=0.5)
    
    myp_OC_cluster = predict(predict_cluster_tree_OC, newdata_cluster_OC)
    
    cluster_prediction_OC = ifelse(myp_OC_cluster[1] >= 0.5, 1, 2)
    #output$myp_OC <- p[1]
  })
})

#model 1 - decision tree without cluster
output$myp_OC1 <- renderText({ 
  input$actionButton
  isolate({
    # all variables
    newdata1_OC = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, hashottuborspa = as.factor(input$hottuborspa), Latitude = input$latitude, Longitude = input$longitude, X=0.5)
    myp_OC1 = predict(OC_model1, newdata1_OC)
    #output$myp_OC <- p[1]
  })
})

#model 2 - decision tree WITH CLUSTER
output$myp_OC2 <- renderText({ 
  input$actionButton
  
  isolate({
    
    newdata_cluster_OC = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, hashottuborspa = as.factor(input$hottuborspa), Latitude = input$latitude, Longitude = input$longitude, X=0.5)
    myp_OC_cluster = predict(predict_cluster_tree_OC, newdata_cluster_OC)
    cluster_prediction_OC = ifelse(myp_OC_cluster[1] >= 0.5, 1, 2)
    
    # all variables
    newdata2_OC = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, hashottuborspa = as.factor(input$hottuborspa), Latitude = input$latitude, Longitude = input$longitude, cluster = as.factor(cluster_prediction_OC), X=0.5)
    myp_OC2 = predict(OC_model2, newdata2_OC)
    #output$myp_OC <- p[1]
  })
})

#model 3 - linear regression WITH CLUSTER
output$myp_OC3 <- renderText({ 
  input$actionButton
  
  isolate({
    
    newdata_cluster_OC = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, hashottuborspa = as.factor(input$hottuborspa), Latitude = input$latitude, Longitude = input$longitude, X=0.5)
    myp_OC_cluster = predict(predict_cluster_tree_OC, newdata_cluster_OC)
    cluster_prediction_OC = ifelse(myp_OC_cluster[1] >= 0.5, 1, 2)
    
    # all variables
    newdata3_OC = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.numeric(input$pool), yearbuilt = input$YearBuilt, hashottuborspa = as.numeric(input$hottuborspa), Latitude = input$latitude, Longitude = input$longitude, cluster_2 = as.numeric(cluster_prediction_OC)-1, X=0.5)
    myp_OC3 = predict(OC_model3, newdata3_OC, interval = "predict")
    #output$myp_OC <- p[1]
  })
})
#==========================================================================================================================================================================================

#==========================================================================================================================================================================================
#VC SERVER CODE
#predict cluster
output$cluster_prediction_VC <- renderText({ 
  input$actionButton
  isolate({
    # all variables
    newdata_cluster_VC = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, Latitude = input$latitude, Longitude = input$longitude, X=0.5)
    
    myp_VC_cluster = predict(predict_cluster_tree_VC, newdata_cluster_VC)
    
    cluster_prediction_VC = ifelse(myp_VC_cluster[1] >= 0.5, 1, 2)
    #output$myp_VC <- p[1]
  })
})

#model 1 - decision tree without cluster
output$myp_VC1 <- renderText({ 
  input$actionButton
  isolate({
    # all variables
    newdata1_VC = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, Latitude = input$latitude, Longitude = input$longitude, X=0.5)
    myp_VC1 = predict(VC_model1, newdata1_VC)
    #output$myp_VC <- p[1]
  })
})

#model 2 - decision tree WITH CLUSTER
output$myp_VC2 <- renderText({ 
  input$actionButton
  
  isolate({
    
    newdata_cluster_VC = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, Latitude = input$latitude, Longitude = input$longitude, X=0.5)
    myp_VC_cluster = predict(predict_cluster_tree_VC, newdata_cluster_VC)
    cluster_prediction_VC = ifelse(myp_VC_cluster[1] >= 0.5, 1, 2)
    
    # all variables
    newdata2_VC = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, Latitude = input$latitude, Longitude = input$longitude, cluster = as.factor(cluster_prediction_VC), X=0.5)
    myp_VC2 = predict(VC_model2, newdata2_VC)
    #output$myp_VC <- p[1]
  })
})

#model 3 - linear regression WITH CLUSTER
output$myp_VC3 <- renderText({ 
  input$actionButton
  
  isolate({
    
    newdata_cluster_VC = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.factor(input$pool), yearbuilt = input$YearBuilt, Latitude = input$latitude, Longitude = input$longitude, X=0.5)
    myp_VC_cluster = predict(predict_cluster_tree_VC, newdata_cluster_VC)
    cluster_prediction_VC = ifelse(myp_VC_cluster[1] >= 0.5, 1, 2)
    
    # all variables
    newdata3_VC = data.frame(calculatedfinishedsquarefeet = input$Squarefeet, bedroomcnt = input$Bedrooms, calculatedbathnbr = input$Bathrooms, poolcnt = as.numeric(input$pool), yearbuilt = input$YearBuilt, Latitude = input$latitude, Longitude = input$longitude, cluster_2 = as.numeric(cluster_prediction_VC)-1, X=0.5)
    myp_VC3 = predict(VC_model3, newdata3_VC, interval = "predict")
    #output$myp_VC <- p[1]
  })
})
#==========================================================================================================================================================================================




  # Generate diagnostic plots
  output$myplot <- renderPlot({
    
    # optional 4 graphs/page
    layout(matrix(c(1,2,3,4),2,2,byrow=T))
    plot(LA_model3)
    
  })
  
  # Generate a summary of the LA data
  output$summary_LA <- renderPrint({
    summary(data_model3_LA)
  })
  
  # Generate a summary of the OC data
  output$summary_OC <- renderPrint({
    summary(data_model3_OC)
  })
  
  # Generate a summary of the data
  output$summary_VC <- renderPrint({
    summary(data_model3_VC)
  })
  
  #LA MODEL VISUALS
  output$LA1_plot <- renderPlot({
    fancyRpartPlot(LA_model1, caption = "Model 1")
  })

  output$LA2_plot <- renderPlot({
  fancyRpartPlot(LA_model2, caption = "Model 2")
  })
  
  output$LA3_summary <- renderPrint({
    summary(LA_model3)
  })
  
  #OC MODEL VISUALS
  output$OC1_plot <- renderPlot({
    fancyRpartPlot(OC_model1, caption = "Model 1")
  })
  
  output$OC2_plot <- renderPlot({
    fancyRpartPlot(OC_model2, caption = "Model 2")
  })
  
  output$OC3_summary <- renderPrint({
    summary(OC_model3)
  })
  
  #VC MODEL VISUALS
  output$VC1_plot <- renderPlot({
    fancyRpartPlot(VC_model1, caption = "Model 1")
  })
  
  output$VC2_plot <- renderPlot({
    fancyRpartPlot(VC_model2, caption = "Model 2")
  })
  
  output$VC3_summary <- renderPrint({
    summary(VC_model3)
  })

})
