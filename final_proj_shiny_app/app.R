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
                
    navbarPage("Conflict in the Yemeni Civil War",
          
    #show pop-up about- is broken, fix later
   # bsModal(
    #    id = "tutorialModal",
     #   title = "",
      #  trigger = "",
        #img(src = "whatever-image.jpg",
        # style = "display: block; margin-left: auto; margin-right: auto;",
        #height = "120",
        #width = "120"),
      #htmlOutput("tutorial")),
    
              tabPanel("Fatalities",
                 titlePanel("Fatalities in the Yemeni Civil War, Summer 2019"),
                 imageOutput("map")),    
    
    
    #here's where the two interactive maps go
    
    navbarMenu("Interactive Maps",
               #this is the fat by actor drop down (1) 
               tabPanel("Fatalities by Actor",
                        titlePanel("Fatalities by Actor"),
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
             titlePanel("Fatalities by Event Type")
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
             )),
tabPanel("About",
         mainPanel(
           h2("The Data"),
           h5("The Armed Conflict Location & Event Data (ACLED) Project is a “disaggregated data collection, analysis and crisis mapping project” that has collected an incredibly detailed account of political violence and protest events. This site uses data from their collection on Yemen between 1 January 2015 (a few months before the official start of the civil war) and 8 October 2019. Please check out their work ", a("here.", href="https://www.acleddata.com/about-acled/")),
           h2("Connect"),
           h5("You can contact me at lizmasten@g.harvard.edu or connect with my on ", a("LinkedIn.", href="www.linkedin.com/in/elizabeth-masten-642567196")),
           h2("Technical"),
           h5("The source code for this site can be found at my ", a("GitHub.", href="https://github.com/LizMas")),
                                                
           
         ))))
         
         #titlePanel("Fatalities in the Yemeni Civil War, Summer 2019"),
         #imageOutput("map"))))


server <- function(input, output, session) {
  toggleModal(session, "tutorialModal", toggle = "open")
  
  #This is for the pop-up 
  output$tutorial <- renderText({
    HTML("Text for About Page goes here")})
  
  #this is where map goes 
    output$map <- renderImage({
        list(src = "map.gif",
             contentType = "image/gif")}, deleteFile = FALSE)

    #this is where map_fat_by_actor1 goes

output$fat_actor1 <- renderPlot({
  map_fat_actor1 <- ggplot(shap) +
    geom_sf(data = shap) +
    geom_sf(data = fat_actor1, aes(color = recode_actor1))
  map_fat_actor1
  
})

}

shinyApp(ui, server)


