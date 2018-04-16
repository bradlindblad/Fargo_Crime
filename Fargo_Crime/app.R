# F I N A L

library(shiny)
library(tidyverse)
library(ggmap)
library(leaflet)


#################
##### DATA ######
#################


mydata <- read.csv("crime.csv", header = T)
mydata$Date.Time <- lubridate::mdy(mydata$Date.Time)
mydata$Time <- lubridate::mdy_hms(mydata$Time)



#################
## DEFINE FNs ###
#################


plotCrime <- function(title = "title", type = "type"){
  # title = The main title for the graph 
  # type = type of crime to filter on
  
  
  
  # Note center of Fargo using the median latitude and longitude
  myLocation_OD <- c(lon=median(mydata$Lon),
                     lat=median(mydata$Lat))
  # Build basemap
  myMAP_OD <- get_map(myLocation_OD,
                      source = "google",
                      maptype = "hybrid",
                      zoom = 12)
  
  # Subset the data
  mydata <- dplyr::filter(mydata, Call.Type == type)
  
  
  # Plot actual map
  ggmap(myMAP_OD, darken = c(0.0, "white")) +
    stat_density2d(aes(x = Lon, y = Lat, fill = -..level.., alpha = ..level..),
                   size = 1,
                   bins = 15,
                   data = mydata,
                   geom = "polygon") +
    scale_fill_gradientn(colors = topo.colors(4)) +
    scale_alpha(range = c(0.1, 0.2),
                guide = F) +
    ggtitle(title) +
    labs(subtitle = "") +
    theme(legend.position="none",
          plot.subtitle = element_text(color="#666666"),
          plot.caption = element_text(color="#AAAAAA", size = 6),
          plot.title = element_text(color = "#666666", size = 25),
          axis.text.x = element_text(angle = 90, hjust = 1),
          plot.margin = margin(0, 0, 0, 0, "cm")) 
  # ggsave(paste(title),'.png') 
  
  
}

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

#################
###### UI #######
#################

ui <- navbarPage("Explore Fargo Crime", #id="nav",
                 
                 tabPanel("Interactive map",
                          
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("my.css")
                              ),
                              
                              # Sidebar with a slider input for number of bins 
                              sidebarPanel(
                                
                                selectInput(inputId = "choose.crime",
                                            label = "Choose a Crime to Explore",
                                            choices = unique(mydata$Call.Type),
                                            selected = "Impaired Person"),
                                
                                hr(),
                                
                                
                                plotOutput("weekday.bar",
                                           height = 200),
                                
                                hr(),
                                
                                plotOutput("month.bar",
                                           height = 200),
                                
                                width = 3
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                
                                h1("FARGO CRIME ANALYZER"),
                                
                                h2("An interactive dashboard that plots dispatch calls"),
                                h2("from 2017, by Brad Lindblad, MBA"),
                                h3("bradley.lindblad@gmail.com"),
                                
                                hr(),
                                
                                leafletOutput("choose.crime",
                                              width = "100%",
                                              height = "550px"),
                                
                                hr(),
                                
                                h3("Data: http://fargond.gov/city-government/departments \n /police/police-records-data/dispatch-logs")
                              )
                          )
                 ),
                 tabPanel("Heatmap",
                          
                          
                          
                          fluidPage(
                            # Header
                            includeCSS("my.css"),
                            #headerPanel(h1("Fargo Crime - 2017 Interactive Dashboard")),
                            
                            
                            # Sidebar with a slider input for number of bins 
                            sidebarLayout(
                              sidebarPanel(
                                
                                
                                selectInput(inputId = "choose.crime1",
                                            label = "Pick a Crime",
                                            choices = unique(mydata$Call.Type),
                                            selected = "Impaired Person"),
                                width = 3
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                
                                h1("FARGO CRIME ANALYZER"),
                                
                                h2("An interactive dashboard that plots dispatch calls"),
                                h2("from 2017, by Brad Lindblad, MBA"),
                                h3("bradley.lindblad@gmail.com"),
                                
                                tags$hr(),
                                
                                plotOutput("choose.crime1",
                                           width = "1100px",
                                           height = "700px"),
                                
                                hr(),
                                
                                h3("Data: http://fargond.gov/city-government/departments \n /police/police-records-data/dispatch-logs")
                                
                              )
                            )
                          )
                          
                          
                 ))


#################
#### SERVER #####
#################


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
  
  
  output$choose.crime1 <- renderPlot({
    
    plotCrime(input$choose.crime1, input$choose.crime1)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


