library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)

##################
#### D A T A #####
##################

mydata <- read.csv("crime.csv", header = T)
mydata$Date.Time <- lubridate::mdy(mydata$Date.Time)
mydata$Time <- lubridate::mdy_hms(mydata$Time)

##################
##### F N S ######
##################

leafPlot <- function(x = type){
  
  
  
  mydata <- filter(mydata, Call.Type == x)
  
  
  
  leaflet() %>% setView(lng = -96.85, lat = 46.85, zoom = 11) %>% addProviderTiles(providers$OpenStreetMap.DE) %>%
    
    leaflet::addMiniMap(position = "topleft") %>%
    
    leaflet::addCircleMarkers(
      lng = mydata$Lon,
      lat = mydata$Lat,
      label = mydata$Description,
      opacity = 0.2,
      stroke = F,
      color = "red")
  
  
  
  
  
}

plotMonth <- function(x = "type"){
  
  # Order data by Months
  mydata <- mydata
  mydata$Month <- factor(mydata$Month, c("Jan", "Feb", "Mar", "Apr", "May",
                                         "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  mydata <- dplyr::filter(mydata, Call.Type == x)
  
  ggplot(mydata,aes(x= mydata$Month, fill=..count..))+
    scale_fill_gradient(low = "green", high = "red")+
    geom_bar(width=0.7) +
    xlab("Month of Crime") +
    ylab("Number of Calls") +
    ggtitle("Number of Crimes by Month") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}

plotWeekday <- function(x = "type"){
  
  # Order data by Months
  mydata <- mydata
  mydata$Weekday <- factor(mydata$Weekday, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                             "Friday", "Saturday"))
  mydata <- dplyr::filter(mydata, Call.Type == x)
  
  ggplot(mydata,aes(x= mydata$Weekday, fill=..count..))+
    scale_fill_gradient(low = "green", high = "red")+
    geom_bar(width=0.7) +
    xlab("Month of Crime") +
    ylab("Number of Calls") +
    ggtitle("Number of Crimes by Weekday") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


##################
###### U I #######
##################

ui <- dashboardPage(
  

  ## H E A D E R
  dashboardHeader(title = "FARGO CRIME"),
  
  
  ## S I D E B A R
  dashboardSidebar(
    
      selectInput(inputId = "choose.crime",
                  label = "Choose a Crime to Explore",
                  choices = unique(mydata$Call.Type),
                  selected = "Impaired Person"),
      
      h4("An interactive dashboard tool for examining trends in Fargo crime. This tool plots the locations of calls made
         to Fargo police dispatch in 2017."),
      
      br(),
      br(),
      br(),
      br(),
      
      h6("Data: http://fargond.gov/city-government/departments \n /police/police-records-data/dispatch-logs"),
      
      h6("Built by Brad Lindblad, MBA. Fargo, North Dakota."),
      
      h6("bradley.lindblad@gmail.com")
      
      
      
      
  ),
  
  
  ## B O D Y
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    fluidRow(
      column(width = 9,
             
             
      #tags$style(type = "text/css", ".box-body {height:80vh}"),
      
      
      box(leafletOutput("choose.crime", 
                        width = "100%",
                        height = "740px"),
          width = "100%",
          height = "780px"
      )
      ),
      column(width = 3,
             
             box(
             plotOutput("weekday.bar",
                        height = "280px",
                        width = "100%"),
             height = "300px",
             width = "300px"
             ),
             
             
             box(
               plotOutput("month.bar",
                          height = "280px",
                          width = "100%"),
               height = "300px",
               width = "300px"
               
             )
             
      )
  )
  )
)


##################
## S E R V E R ###
##################


server <- function(input, output) {

  
  output$choose.crime <- renderLeaflet({
    
    leafPlot(input$choose.crime)
    
  })
  
  output$weekday.bar <- renderPlot({
    
    
    plotWeekday(input$choose.crime)
    
  })
  
  output$month.bar <- renderPlot({
    
    plotMonth(input$choose.crime)
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

