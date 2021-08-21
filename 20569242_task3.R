library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(RColorBrewer)

#final 
covid_data<-read.csv(file = "C:/Users/dell/OneDrive - LA TROBE UNIVERSITY/Documents/vic_covid19_data.csv")
#covid_data<-read.csv(file = "vic_covid19_data.csv")

ui<-dashboardPage(
  dashboardHeader(title="Victoria Covid-19 data Analysis",titleWidth = 400),
    
    dashboardSidebar(
                     
    sidebarMenu(
   h4( selectInput("selectacquired", "Acquired ",
                  c("Travel overseas" = 1,"Acquired in Australia, unknown source" = 2,"Contact with a confirmed case"= 3)), 
      checkboxInput("checkbox", strong("Cases count based on local goverment area"),TRUE )
    ))),
    
    dashboardBody(
      
        
         
           tabBox(title = "Tab Box",id = "tabset1",width=1000,
     tabPanel("Task 3 (a)",plotlyOutput("plot1")),
     tabPanel("Task 3 (a)",plotlyOutput("plot2"))
      ))
)
#--------------------------------Server--------------------------------------
server<-function(input,output)
{
#-----------------------------------Task 3(a)------------------------------------  
  covid_data_count = covid_data %>%
    group_by(age_group,acquired) %>% 
    summarise(No_cases=n()) %>% 
    arrange(desc(No_cases))
  
  
  # No_cases<-covid_data_count$No_cases
  # age_group<-covid_data_count$age_group
  # acquired<-covid_data_count$acquired
  
  data1<-covid_data_count
  Filtereddata1<-filter(data1,acquired == "Travel overseas")
  Filtereddata2<-filter(data1,acquired == "Acquired in Australia, unknown source")
  Filtereddata3<-filter(data1,acquired == "Contact with a confirmed case")
  
  age_group<-covid_data_count$age_group
  
  
  
  output$plot1 <- renderPlotly({
    
    ggplotly({
      
    if(input$selectacquired==1)
   {      
    ggplot(Filtereddata1,
                  aes( x =reorder(age_group,No_cases),y = No_cases,))+
        geom_bar(stat = "identity",fill="#ED9871",color="black")  + ylab("Number of cases") +
        xlab("Age Group")+theme_classic()+ggtitle("Number of covid cases diagnosed by Travelling Overseas")+
        theme(plot.title = element_text(size = 13,face = "bold"))
    
      }
      
        else if (input$selectacquired==2)
        {
          ggplot(Filtereddata2,
                      aes(x =reorder(age_group,No_cases),y = No_cases,group=age_group))+
            geom_bar(stat="identity",fill="#ED9871",color="black")  + ylab("Number of cases") +
            xlab("Age Group")+theme_classic()+
          ggtitle("Number of covid cases acuired in Australia from unknown source")+
            theme(plot.title = element_text(size = 13,face = "bold"))
          }
      
    else if (input$selectacquired==3)
    {
      ggplot(Filtereddata3,
             aes(x =reorder(age_group,No_cases),y = No_cases))+
        geom_bar(stat = "identity",fill="#ED9871",color="black")  + ylab("Number of cases") +
        xlab("Age Group")+theme_classic()+
        ggtitle("Number of covid cases acquired by come in contact with confirmed cases")+
        theme(plot.title = element_text(size = 13,face = "bold"))
      
      }
      
    })   
    
  })
  
  #------------------------------------------TASK 3 (b)-----------------------------------------------------
  
  coviddata = covid_data%>%
  group_by(age_group,acquired,local_government_area,diagnosis_date) %>% 
    summarise(Number_of_cases=n()) %>% 
    arrange(desc(Number_of_cases)) 
  
  
 # Localgovtarea<- reactivecoviddata$local_government_area[order(Number_of_cases, decreasing = TRUE)]
  #diagnosisdate<-reactivecoviddata$diagnosis_date[order(Number_of_cases, decreasing = TRUE)]
  Number_of_casess<-as.character(coviddata$Number_of_cases [1:20])
  
 
  
  output$plot2<- renderPlotly({
    
    if (input$checkbox==TRUE)
    {
   
     r<- head(coviddata, 20)
      q <- ggplot(r, aes(x = diagnosis_date,y=Number_of_casess,
                      fill=local_government_area))+ 
      scale_fill_brewer(palette = "Reds",)+
       geom_bar(stat = "identity",position="dodge") + ylab("Number of cases") +xlab("Diagnosis date")+
        #coord_flip()+
      theme_classic()+ ggtitle("Covid Cases diagnosed over time based on local goverment areas")+
        theme(plot.title = element_text(size = 13,face = "bold"))
    ggplotly(q)
  
   } })
  
  #-------------------------------Run---------------------------------------
}

shinyApp(ui,server)