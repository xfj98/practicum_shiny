##Project_Copied
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tm)
library(RColorBrewer)
library(plotly)

data <- read.csv("data.csv")
names(data)
#data <- data.frame(data[,-1])
# Define UI for application that draws a histogram
shinyUI(navbarPage("Impact Alpha Score For Healthcare Industry",
                   
                   tabPanel("Description",h1("Social Impact Alpha Score for Healthcare Industry"),
                            h2(textOutput("text")),
                            hr(),
                            tags$img(src = 'healthcare2.png',width = "1250px")
                            ),
                   tabPanel("Table",
                            fluidRow(
                                     column(4,
                                            selectInput("company","Select Company Name:",c('All',as.character(data$Company.Name)),selected = 'All')
                                     ),
                                     column(4,
                                            selectInput("company_type","Select Company Type:",c('All',as.character(data$Company.Type)),selected = 'All')
                                     )
                                     ),
                            DT::dataTableOutput("table")
                   ),
                   navbarMenu("Data Visualization",
                              tabPanel("Impact Alpha by Metric", 
                                       sidebarLayout(
                                         
                                         sidebarPanel(
                                           
                                           selectInput("metrics", "Choose a Metric to Visualize its Distribution:",
                                                       choices = c("GHG_Revenue","Women_Empls_Mgmt_Ratio",
                                                                   "Women_Employees_Percentage","Employee_Turnover_Percentage","Employee_Unionized_Percentage",
                                                                   "Lost_Time_Incident_Rate","Independent_Director_Percentage","Percenta_Board_Members_Woman",
                                                                   "Director_Avg_Age","Director_Meeting_Attd","Board_Size"),
                                                       selected = "Board_Size"),
        
                                           selectInput("company2", "Choose Five Companies to Visualize Metrics:",
                                                       choices = levels(data$Company.Name),
                                                       multiple =TRUE,
                                                       selected = "ZEAL",
                                                       selectize = TRUE)
                                           ),
                                         
                                         mainPanel("Histogram and Radar", 
                                                   fluidRow(
                                                     column(6,plotOutput(outputId="hist", width="450px",height="450px")),
                                                     column(6,plotlyOutput(outputId="radial", width="500px",height="500px"))
                                                     )
                                                   )
                                         )
                                       ),
                              tabPanel("Investment Return VS. Impact Alpha",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("companyType","Select company type",choices = c('All',as.character(data$Company.Type)),selected = 'All')
                                          ),
                                         mainPanel("Scatter",plotlyOutput( "scatter"))
                                        )
                                      )
                      ),
                   
                   tabPanel("Impact Alpha",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("company3","Select Company Name:",c('All',as.character(data$Company.Name)),selected = 'All'),
                                actionButton('go',"Update Weights"),
                                sliderInput("importance1","Importance of GHG_Revenue to Social Impact:",
                                            min= -10, max = 10,value = 1),
                                sliderInput("importance2","Importance of Women_Empls_Mgmt_Ratio to Social Impact:",
                                            min= -10, max = 10,value = 1),
                                sliderInput("importance3","Importance of Women_Employees_Percentage to Social Impact:",
                                            min= -10, max = 10,value = 1),
                                sliderInput("importance4","Importance of Employee_Turnover_Percentage to Social Impact:",
                                            min= -10, max = 10,value = 1),
                                sliderInput("importance5","Importance of Employee_Unionized_Percentage to Social Impact:",
                                            min= -10, max = 10,value = 1),
                                sliderInput("importance6","Importance of Lost_Time_Incident_Rate to Social Impact:",
                                            min= -10, max = 10,value = 1),
                                sliderInput("importance7","Importance of Independent_Director_Percentage to Social Impact:",
                                            min= -10, max = 10,value = 1),
                                sliderInput("importance8","Importance of Percenta_Board_Members_Woman to Social Impact:",
                                            min= -10, max = 10,value = 1),
                                sliderInput("importance9","Importance of Director_Avg_Age to Social Impact:",
                                            min= -10, max = 10,value = 1),
                                sliderInput("importance10","Importance of Director_Meeting_Attd to Social Impact:",
                                         min= -10, max = 10,value = 1),
                                sliderInput("importance11","Importance of Board_Size to Social Impact:",
                                            min= -10, max = 10,value = 1)
                          
                              ),
                              mainPanel("User-defined Impact Alpha",DT::dataTableOutput("alpha"))
                            ))
                   
                  
))
