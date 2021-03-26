#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)
library(plotly)
library(formattable)
library(rpart)
library(rpart.plot)
if(!require(DT)){
    install.packages("DT")
    library(DT)
}
# install.packages("formattable")
setwd('E:/MscDA/MS984')
CompanyA=read.csv('CompanyA.csv')
CompanyA <- CompanyA[complete.cases(CompanyA),]
CompanyA$SeniorCitizen <- as.factor(ifelse(CompanyA$SeniorCitizen==1, 'YES', 'NO'))
theme1 <- theme_bw()+
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),legend.position="none")
theme2 <- theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="none")

CompanyA <- mutate(CompanyA, tenure_bin = tenure)
CompanyA$tenure_bin[CompanyA$tenure_bin >=0 & CompanyA$tenure_bin <= 12] <- '0-1 year'
CompanyA$tenure_bin[CompanyA$tenure_bin > 12 & CompanyA$tenure_bin <= 24] <- '1-2 years'
CompanyA$tenure_bin[CompanyA$tenure_bin > 24 & CompanyA$tenure_bin <= 36] <- '2-3 years'
CompanyA$tenure_bin[CompanyA$tenure_bin > 36 & CompanyA$tenure_bin <= 48] <- '3-4 years'
CompanyA$tenure_bin[CompanyA$tenure_bin > 48 & CompanyA$tenure_bin <= 60] <- '4-5 years'
CompanyA$tenure_bin[CompanyA$tenure_bin > 60 & CompanyA$tenure_bin <= 72] <- '5-6 years'
CompanyA$tenure_bin <- as.factor(CompanyA$tenure_bin)

dummy<- data.frame(sapply(CompanyA_cat,function(x) data.frame(model.matrix(~x-1,data =CompanyA_cat))[,-1]))
CompanyA_final <- cbind(CompanyA_int,dummy)
indices = sample.split(CompanyA_final$Churn, SplitRatio = 0.7)
train = CompanyA_final[indices,]
validation = CompanyA_final[!(indices),]
Dtree = rpart(Churn ~., data = train, method = "class")
DTPred <- predict(Dtree,type = "class", newdata = validation[,-24])

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$Churn_Percent <- renderPlotly({ggplotly(
        CompanyA %>% 
            group_by(Churn) %>% 
            summarise(Count = n())%>% 
            mutate(percent = prop.table(Count)*100)%>%
            ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
            geom_col(fill = c("red", "blue"))+
            geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
            theme_bw()+  
            xlab("Churn") + 
            ylab("Percent")+
            ggtitle("Churn Percent")
    )})
    
    output$entity <- renderPlot({
        options(repr.plot.width = 21, repr.plot.height = 8)
        
        plot_grid(ggplot(CompanyA, aes(x=gender,fill=Churn))+ geom_bar()+ theme1, 
                  ggplot(CompanyA, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme1,
                  ggplot(CompanyA, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme1,
                  ggplot(CompanyA, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme1,
                  ggplot(CompanyA, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme1,
                  ggplot(CompanyA, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                  align = "h")   
    
    })
    
    output$service=renderPlot({
        options(repr.plot.width = 15, repr.plot.height = 8)
        
        plot_grid(ggplot(CompanyA, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+ theme1+
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
                  ggplot(CompanyA, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme1+
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                  ggplot(CompanyA, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme1+
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                  ggplot(CompanyA, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+theme1+
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                  ggplot(CompanyA, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+theme1+
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                  ggplot(CompanyA, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                  align = "h")  
    })
    
    output$otherdetail=renderPlot({
 
        plot_grid(ggplot(CompanyA, aes(x=StreamingMovies,fill=Churn))+ 
                      geom_bar(position = 'fill')+ theme1+
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
                  ggplot(CompanyA, aes(x=Contract,fill=Churn))+ 
                      geom_bar(position = 'fill')+theme1+
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                  ggplot(CompanyA, aes(x=PaperlessBilling,fill=Churn))+ 
                      geom_bar(position = 'fill')+theme1+
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                  ggplot(CompanyA, aes(x=PaymentMethod,fill=Churn))+
                      geom_bar(position = 'fill')+theme_bw()+
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                  align = "h")  
    })
    
    output$tenure=renderPlot({
        options(repr.plot.width =6, repr.plot.height = 2)
        ggplot(CompanyA, aes(y= tenure, x = "", fill = Churn)) + 
            geom_boxplot()+ 
            xlab(" ")
    })
    
    output$MonthlyCharges=renderPlot({
        ggplot(CompanyA, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
            geom_boxplot()+ 
            theme_bw()+
            xlab(" ") 
    })
    
   
    output$correlation=renderPlot({
        options(repr.plot.width =6, repr.plot.height = 4)
        CompanyA_cor <- round(cor(CompanyA[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)
        ggcorrplot(CompanyA_cor, title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))
    
    })
    
    
    output$tenure_bin=renderPlotly({
       
        
        options(repr.plot.width =6, repr.plot.height = 3)
        ggplotly(ggplot(CompanyA, aes(tenure_bin, fill = tenure_bin)) + geom_bar()+ theme1)
        
    })     
    
    
    output$tenure_churn=renderPlotly({
       
        options(repr.plot.width =6, repr.plot.height = 3)
        ggplotly(ggplot(CompanyA, aes(tenure, fill = Churn)) +geom_area(stat = 'bin')+ theme_bw())
        
    })     
    
    
    
    output$table1=renderDT({
        datatable(CompanyA[,1:8],options =list(
            lengthMenu = c(5, 10, 15, 20)
            ))
        # test.table <- data.frame(lapply(1:8, function(x) {1:10}))
        # formattable(test.table, list())
    })
    
    output$table2=renderDT({
        datatable(CompanyA[,c(1,9:16)],options =list(
            lengthMenu = c(5, 10, 15, 20)
        ))
        # test.table <- data.frame(lapply(1:8, function(x) {1:10}))
        # formattable(test.table, list())
    })  
    
    
    output$table3=renderDT({
        datatable(CompanyA[,c(1,17:22)],options =list(
            lengthMenu = c(5, 10, 15, 20)
        ))
        # test.table <- data.frame(lapply(1:8, function(x) {1:10}))
        # formattable(test.table, list())
    })  
    
    output$desicion=renderPlot({
        rpart.plot(Dtree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
        
    })
    
    output$affect=renderPlotly({
        
        Dtree$variable.importance%>%as.data.frame()->variableimportance
        var=data.frame(name=rownames(variableimportance),value=as.numeric(variableimportance[,1]/sum(variableimportance)))
      ggplotly( var%>%
            arrange(value)%>%
            mutate(name=factor(name, levels=name))%>%
        ggplot(aes(x=value,y=name,fill=value))+
            geom_col())

   
    })
    
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(CompanyA, file, row.names = FALSE)
        }
    )
    
    
    
    
})
