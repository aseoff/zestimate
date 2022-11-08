library(shiny)

# Define UI for Predicting Employed: display summary,table and plot using  longley dataset with tabs
shinyUI(
  pageWithSidebar(
  # Application title
  headerPanel("Predict Home Value"),
  
  
  #for linear regression
  #county, bedrooms, bathrooms, sqft, yearbuilt, poolcnt, hashottuborspa, latitude, longitude, cluster
  
  # Adding widgets
  sidebarPanel(
    h4("Help text"),
    helpText("Enter county, number of bedrooms, number of bathrooms, square footage, yearbuilt, latitude, longitude, pool, hot tub/spa and click on ",strong("Predict!")," to get predicted value of Tax Assessment Value"),
    
    #Input fields (9)
    selectInput("county", "Location:", choices = list("Los Angeles County" = 'LA', "Orange County" = 'OC', "Ventura County" = 'VC')),
    br(),
    numericInput("Bedrooms","Bedrooms",value=0,min=1,max=50),
    br(),
    numericInput("Bathrooms","Bathrooms",value=0,min=1,max=50),
    br(),
    numericInput("Squarefeet","SquareFeet",value=0,min=1,max=100000),
    br(),
    numericInput("YearBuilt","YearBuilt",value=0,min=1600,max=2020),
    br(),
    numericInput("latitude","latitude",value=0,min=-100000000,max=1000000000),
    br(),
    numericInput("longitude","longitude",value=0,min=-1000000000,max=1000000000),
    br(),
    radioButtons("hottuborspa","Is there a hot tub or spa?",choices = list("Yes" = 1, "No" = 0)),
    br(),
    radioButtons("pool","Is there a pool?",choices = list("Yes" = 1, "No" = 0)),
    br(),
    
    actionButton("actionButton","Predict!",align = "center"),
  ),
  
  # Prediction panel for housing valuation
  mainPanel(
    tabsetPanel(
      tabPanel("Predictions",
               
               h2("These are the three models we used to predict tax assessment value for each county:"),

                p("1. A decision tree that uses all variables to predict tax value dollar count"),
                    
               p("NOTE: Models 2 and 3 use clusters (kmeans method) comprised from latitude, longitude, and taxvaluedollarcount to predict the tax assessment value. The clusters are meant to separate the more expensive areas within a county (likely urban) and the relatively lower value areas (likely rural). Based on all of the input variables, a cluster (1 or 2) is predicted using a class decision tree. This predicted cluster is than incorporated into both models 2 and 3."),
                    
                p("2. A decision tree that uses all variables EXCEPT latitude and longitude - uses cluster data to account for location"),
               
               p("3. A simple linear regression model that uses all variables EXCEPT latitude and longitude - uses cluster data to account for location"),
                    
                    
               
               
               h2("Tax Assessment Value Predictions",align="center") ,
               

               p("The below values represent the Location Cluster assigned from our preliminary decision tree model and the 3 prediction values generated using all 3 county specific models (see descriptions above)."),
                
              #Output for LA
               conditionalPanel(condition = "input.county == 'LA'",
                                 p(strong("Cluster Prediction (Accuracy training/testing: 86.7%/87.0%):")),
                                 code(textOutput("cluster_prediction_LA")),
                                 p(strong("Model 1 Prediction (RMSE training/testing: 247,661/247,483):")),
                                 code(textOutput("myp_LA1")),
                                 p(strong("Model 2 Prediction (RMSE training/testing: 181,204/179,916):")),
                                 code(textOutput("myp_LA2")),
                                 p(strong("Model 3 Prediction with confidence interval (RMSE training/testing: 178,461/176,033):")),
                                 code(textOutput("myp_LA3"))),
              
              #Output for Orange county
               conditionalPanel(condition = "input.county == 'OC'",
                                p(strong("Cluster Prediction (Accuracy training/testing: 84.8%/83.8%):")),
                                code(textOutput("cluster_prediction_OC")),
                                p(strong("Model 1 Prediction (RMSE training/testing: 238,810/242,273):")),
                                code(textOutput("myp_OC1")),
                                p(strong("Model 2 Prediction (RMSE training/testing: 181,639/185,782):")),
                                code(textOutput("myp_OC2")),
                                p(strong("Model 3 Prediction with confidence interval (RMSE training/testing: 179,321/182,493):")),
                                code(textOutput("myp_OC3"))),
              
              #Output for Ventura county
               conditionalPanel(condition = "input.county == 'VC'",
                                p(strong("Cluster Prediction (Accuracy training/testing: 84.2%/83.9%):")),
                                code(textOutput("cluster_prediction_VC")),
                                p(strong("Model 1 Prediction (RMSE training/testing: 189,935/187,125):")),
                                code(textOutput("myp_VC1")),
                                p(strong("Model 2 Prediction (RMSE training/testing: 147,310/145,941):")),
                                code(textOutput("myp_VC2")),
                                p(strong("Model 3 Prediction with confidence interval (RMSE training/testing: 144,118/140,227):")),
                                code(textOutput("myp_VC3"))),
              

              
               
      ),
      
      #Other 3 tabs with information regarding descriptive statistics, HTML table, and diagnostic plots, all for the cleaned and complete dataset
      tabPanel("Summary", 
               h2("Summary of",strong("Los Angeles"),"dataset"),
               verbatimTextOutput("summary_LA"), 
               h2("Summary of",strong("Orange County"),"dataset"),
               verbatimTextOutput("summary_OC"), 
               h2("Summary of",strong("Ventura County"),"dataset"),
               verbatimTextOutput("summary_VC")), 
 
           tabPanel("LA Models", 
               h2("Model 1:"),
               plotOutput("LA1_plot"),
                h2("Model 2:"),
                plotOutput("LA2_plot"),
               h2("Model 3:"),
               verbatimTextOutput("LA3_summary")),
      
      tabPanel("OC Models", 
               h2("Model 1:"),
               plotOutput("OC1_plot"),
               h2("Model 2:"),
               plotOutput("OC2_plot"),
               h2("Model 3:"),
               verbatimTextOutput("OC3_summary")),
      
      tabPanel("VC Models", 
               h2("Model 1:"),
               plotOutput("VC1_plot"),
               h2("Model 2:"),
               plotOutput("VC2_plot"),
               h2("Model 3:"),
               verbatimTextOutput("VC3_summary"))
      
    )
  )
))

