#Copied
#Version2.0

library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tm)
library(RColorBrewer)
library(plotly)


data <- read.csv("data.csv")


shinyServer(function(input, output) {
    
    output$text <- renderText({
        
        "Background Introduction"
        
    })
    
    # output$table <- DT::renderDataTable(DT::datatable({
    #     data <- data[,c("Company.Name","Company.Type","Impact.Alpha","Total.Return")]
    #     if (input$company != "All") {
    #       data <- data[data$Company.Name == input$company,]
    #     }
    #     data
    # }))

    output$table <- DT::renderDataTable(DT::datatable({
      
      data <- data[,c("Company.Name","Company.Type","Impact.Alpha","Total.Return")]
      
      if (input$company == "All"){
        if(input$company_type == "All"){
          data <- data
        }
        else if(input$company_type != "All"){
          data <- data[data$Company.Type == input$company_type,]
        }
      }
      else if(input$company != "All"){
        if(input$company_type == "All"){
          data <- data[data$Company.Name == input$company,]
        }
        else if(input$company_type != 'All'){
          data1 <- data[data$Company.Name == input$company,]
          data <- data1[data1$Company.Type == input$company_type,]
        }
      }
      data
    }))
    
    output$hist <- renderPlot({
        
        ggplot(data = data, aes_string(x= input$metrics))+ geom_histogram(colour = "white",bins = 7,fill='#5DADE2') + 
            title(main = "Metric Distribution", ylab = "Histogram") 
    })
    
    output$radial <- renderPlotly({
      library(dplyr)
      
      inputcompany <- input$company2
      
      plot_ly(type = 'scatterpolar',
              mode = "markers",
              fill = 'toself') %>%
        
        add_trace(
          r = as.matrix(data[data$Company.Name == inputcompany[1],c("GHG_Revenue","Women_Empls_Mgmt_Ratio",
                                                                    "Women_Employees_Percentage","Employee_Turnover_Percentage","Employee_Unionized_Percentage",
                                                                    "Lost_Time_Incident_Rate","Independent_Director_Percentage","Percenta_Board_Members_Woman",
                                                                    "Director_Avg_Age","Director_Meeting_Attd","Board_Size")]),
          theta = c("GHG_Revenue","Women_Empls_Mgmt_Ratio",
                    "Women_Employees_Percentage","Employee_Turnover_Percentage","Employee_Unionized_Percentage",
                    "Lost_Time_Incident_Rate","Independent_Director_Percentage","Percenta_Board_Members_Woman",
                    "Director_Avg_Age","Director_Meeting_Attd","Board_Size"),
          showlegend = T,
          mode = "markers",
          name = inputcompany[1],
          color = brewer.pal(5,"Paired")[1]
          
        ) %>%  
        
        add_trace(
          r = as.matrix(data[data$Company.Name == inputcompany[2],c("GHG_Revenue","Women_Empls_Mgmt_Ratio",
                                                                    "Women_Employees_Percentage","Employee_Turnover_Percentage","Employee_Unionized_Percentage",
                                                                    "Lost_Time_Incident_Rate","Independent_Director_Percentage","Percenta_Board_Members_Woman",
                                                                    "Director_Avg_Age","Director_Meeting_Attd","Board_Size")]),
          theta = c("GHG_Revenue","Women_Empls_Mgmt_Ratio",
                    "Women_Employees_Percentage","Employee_Turnover_Percentage","Employee_Unionized_Percentage",
                    "Lost_Time_Incident_Rate","Independent_Director_Percentage","Percenta_Board_Members_Woman",
                    "Director_Avg_Age","Director_Meeting_Attd","Board_Size"),
          showlegend = T,
          mode = "markers",
          name = inputcompany[2],
          visible="legendonly",
          color = brewer.pal(5,"Paired")[2]
        ) %>% 
        
        add_trace(
          r = as.matrix(data[data$Company.Name == inputcompany[3],c("GHG_Revenue","Women_Empls_Mgmt_Ratio",
                                                                    "Women_Employees_Percentage","Employee_Turnover_Percentage","Employee_Unionized_Percentage",
                                                                    "Lost_Time_Incident_Rate","Independent_Director_Percentage","Percenta_Board_Members_Woman",
                                                                    "Director_Avg_Age","Director_Meeting_Attd","Board_Size")]),
          theta = c("GHG_Revenue","Women_Empls_Mgmt_Ratio",
                    "Women_Employees_Percentage","Employee_Turnover_Percentage","Employee_Unionized_Percentage",
                    "Lost_Time_Incident_Rate","Independent_Director_Percentage","Percenta_Board_Members_Woman",
                    "Director_Avg_Age","Director_Meeting_Attd","Board_Size"),
          showlegend = T,
          mode = "markers",
          name = inputcompany[3],
          visible="legendonly",
          color = brewer.pal(5,"Paired")[3]
        ) %>% 
        
        add_trace(
          r = as.matrix(data[data$Company.Name == inputcompany[4],c("GHG_Revenue","Women_Empls_Mgmt_Ratio",
                                                                    "Women_Employees_Percentage","Employee_Turnover_Percentage","Employee_Unionized_Percentage",
                                                                    "Lost_Time_Incident_Rate","Independent_Director_Percentage","Percenta_Board_Members_Woman",
                                                                    "Director_Avg_Age","Director_Meeting_Attd","Board_Size")]),
          theta = c("GHG_Revenue","Women_Empls_Mgmt_Ratio",
                    "Women_Employees_Percentage","Employee_Turnover_Percentage","Employee_Unionized_Percentage",
                    "Lost_Time_Incident_Rate","Independent_Director_Percentage","Percenta_Board_Members_Woman",
                    "Director_Avg_Age","Director_Meeting_Attd","Board_Size"),
          showlegend = T,
          mode = "markers",
          name = inputcompany[4],
          visible="legendonly",
          color = brewer.pal(5,"Paired")[4]
        ) %>% 
        
        add_trace(
          r = as.matrix(data[data$Company.Name == inputcompany[5],c("GHG_Revenue","Women_Empls_Mgmt_Ratio",
                                                                    "Women_Employees_Percentage","Employee_Turnover_Percentage","Employee_Unionized_Percentage",
                                                                    "Lost_Time_Incident_Rate","Independent_Director_Percentage","Percenta_Board_Members_Woman",
                                                                    "Director_Avg_Age","Director_Meeting_Attd","Board_Size")]),
          theta = c("GHG_Revenue","Women_Empls_Mgmt_Ratio",
                    "Women_Employees_Percentage","Employee_Turnover_Percentage","Employee_Unionized_Percentage",
                    "Lost_Time_Incident_Rate","Independent_Director_Percentage","Percenta_Board_Members_Woman",
                    "Director_Avg_Age","Director_Meeting_Attd","Board_Size"),
          showlegend = T,
          mode = "markers",
          name = inputcompany[5],
          visible="legendonly",
          color = brewer.pal(5,"Paired")[5]
        ) %>% 
        
        layout(
          width="400px",
          height="400px",
          polar = list(
            radialaxis = list(visible = T,range = c(0,1)),
            angularaxis = list(tickfont = list(size = 10))
          ),
          showlegend=TRUE)
    })
    
    update_data <- reactive({
      input$go
      importance<- c(as.numeric(isolate(input$importance1)),
                     as.numeric(isolate(input$importance2)),
                     as.numeric(isolate(input$importance3)),
                     as.numeric(isolate(input$importance4)),
                     as.numeric(isolate(input$importance5)),
                     as.numeric(isolate(input$importance6)),
                     as.numeric(isolate(input$importance7)),
                     as.numeric(isolate(input$importance8)),
                     as.numeric(isolate(input$importance9)),
                     as.numeric(isolate(input$importance10)),
                     as.numeric(isolate(input$importance11)))
      weight <- as.vector(importance/sum(importance))
      
      dataset <- data[,c("GHG_Revenue","Women_Empls_Mgmt_Ratio",
                         "Women_Employees_Percentage","Employee_Turnover_Percentage","Employee_Unionized_Percentage",
                         "Lost_Time_Incident_Rate","Independent_Director_Percentage","Percenta_Board_Members_Woman",
                         "Director_Avg_Age","Director_Meeting_Attd","Board_Size")]
      
      impact.alpha <- round(as.matrix(dataset) %*% weight,2)
      company.name <- data$Company.Name
      df <- data.frame("Impact_Alpha"=impact.alpha,"Company.Name"=company.name)
      update <- merge(data,df,by="Company.Name")
      update
    })
    
    
    output$scatter <- renderPlotly({
      
      df <- update_data()
      df <- df[,c("Company.Type","Impact_Alpha","Total.Return","Company.Name")]
      
      if (input$companyType == "All"){
        data = df
      }else{data = df[df$Company.Type == input$companyType,]}
      
      x <- list(title = "Impact Alpha")
      y <- list(title = "Investment Return")
      
      fig <- plot_ly(data,y= ~data$"Impact_Alpha",x= ~data$"Total.Return",type = "scatter",
                     text = ~paste("Company Nmae:",data$"Company.Name","Impact Alpha:",data$"Impact_Alpha","Investment Return:",data$"Total.Return"),
                     color = factor(data$"Company.Name"))
      fig <- fig %>% layout(xaxis = x, yaxis = y)
      fig    
    })
    
    output$alpha <- DT::renderDataTable(DT::datatable({
      input$go
      df <- update_data()
      df <- df[,c("Company.Name","Impact.Alpha")]
      if (isolate(input$company3) == "All"){
        data = df
      }else{data = df[df$Company.Name == isolate(input$company3),]}
      data
    }))
   
})


