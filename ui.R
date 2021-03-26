#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
require(shinydashboard)
ui <- dashboardPage(
    dashboardHeader(title = "dashboard for churn website"),
    dashboardSidebar(
        sidebarMenu(
        menuItem("Churn", tabName = "Data", icon = icon("dashboard")),
        menuItem("Forecast", tabName = "Forecast", icon = icon("dashboard")),
        menuItem("Plot", tabName = "Plot1", icon = icon("bar-chart-o")),
        menuItem("Table", tabName = "Table", icon = icon("dashboard")))
        
        ),
    dashboardBody(
        tabItems(
        tabItem(tabName = "Data", fluidRow(box(plotlyOutput("Churn_Percent")),box(plotlyOutput('tenure_churn'),title ='Churn trend by time')),
                fluidRow(box(plotlyOutput('affect'),title = 'Mayjor Influencing factors')) ),
        tabItem(tabName = "Forecast", 
                plotOutput("desicion") ),
        tabItem(tabName = "Plot1",
      fluidRow(box(plotOutput("entity"),width=6),box(plotOutput('otherdetail'),width=6)),      
      fluidRow(box(plotOutput('service'),width=6),box(plotOutput('tenure'),width=3),box(plotOutput('MonthlyCharges'),width=3)),
      fluidRow(box(plotlyOutput('tenure_bin'))) ),
      # fluidRow(),
      # fluidRow()
               
      tabItem(tabName = "Table",downloadButton("downloadData", "Download"),
              fluidRow(DTOutput('table1'),fluidRow(DTOutput('table2'),fluidRow(DTOutput('table3')))
              ))
      # fluidRow(formattableOutput('atable'))
              )
      
      ))
    
    
    

    
    
   
# box(plotlyOutput("Churn_Percent")),
# box(plotlyOutput("Churn_Percent")),
# box(plotOutput("entity")),
# box(plotOutput('service')),          
# box(plotOutput('otherdetail')),
# box(plotOutput('tenure')),
# box(plotOutput('MonthlyCharges')),
# box(plotOutput('correlation')),
# box(plotOutput('tenure_bin'),width = 8)