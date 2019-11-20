library(shiny)
library(markdown)
library(ggplot2)
library(fs)
library(sf)
library(readr)
library(gganimate)
library(transformr)
library(lubridate)
library(shinythemes)
library(shinyBS)
library(tidyverse)

ui <- fluidPage(theme = shinytheme("flatly"),
                position = "static-top",
                
                br(),
                
    navbarPage("Conflict in the Yemeni Civil War, Summer 2019",
          
    #show pop-up about
    bsModal(
        id = "tutorialModal",
        title = "",
        trigger = "",
        #img(src = "whatever-image.jpg",
        # style = "display: block; margin-left: auto; margin-right: auto;",
        #height = "120",
        #width = "120"),
      htmlOutput("tutorial")),
              tabPanel("Fatalities",
                 titlePanel("Fatalities in the Yemeni Civil War, Summer 2019"),
                 imageOutput("map")),    
    
    
    #here's where the two interactive maps go
    
    navbarMenu("Interactive Maps",
               #this is the fat by actor drop down (1) 
               tabPanel("Fatalities by Actor",
                        titlePanel(
                          textOutput("FatalitiesbyactorTitle")
                        ),
                        sidebarLayout(position = "right",
                                      sidebarPanel(
                                        selectInput("recode_actor1",
                                                    "Actor:",
                                              choices = sort(unique(map_fat_actor1$fat_actor1)),
                                              selected = "Pro-Goverment Militias")),
                                      mainPanel(
                                        plotOutput("map_fat_actor1.rds")
                                        
                                      )
                        )
               ),
    #this is the fat by event type dropdown (2)
    
    tabPanel("Fatalities by Event Type",
             titlePanel(
               textOutput("FatalitiesbyeventTitle")
             ),
             sidebarLayout(position = "right",
                           sidebarPanel(
                             selectInput("event_type",
                                         "Event Type:",
                                         choices = sort(unique(map_fat_actor1$fat_actor1)),
                                         selected = "Air/drone strike")),
                           mainPanel(
                             plotOutput("map_fat_actor1.rds")
                             
                           )
             )
    ))))




              # tabPanel("Fatalities by Event Type"),
               # imageOutput("map_fat_actor1")),
                        
              # tabPanel("Fatalities by Actor"),
              #tabPanel("Analytics"))
                       #imageOutput("image"))))
  


server <- function(input, output, session) {
  toggleModal(session, "tutorialModal", toggle = "open")
  
  #This is for the pop-up 
  output$tutorial <- renderText({
    HTML("Text for About Page goes here"
         
    )
  })
  #this is where map goes 
    output$map <- renderImage({
        list(src = "map.gif",
             contentType = "image/gif")}, deleteFile = FALSE)


output$map_fat_actor1 <- renderImage({
  list(src = "map_fat_actor1.rds",
       contentType = "image/gif")}, deleteFile = FALSE)
}

shinyApp(ui, server)


