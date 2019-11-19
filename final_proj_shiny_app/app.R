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

ui <- fluidPage(
               # h2(textOutput("currentTime")),
  
  
    navbarPage("Fatalities in the Yemeni Civil War, Summer 2019",
               theme = shinytheme("flatly"),
               position = "static-top",
               

               #I want to create a popup window, but that's not working
               #bsModal(
                   #id = "tutorialModal",
                #   title = "",
                 #  trigger = "",
                   #img(src = "unhcr_logo.jpg",
                  # style = "display: block; margin-left: auto; margin-right: auto;",
                   #height = "120",
                   #width = "120"),
               #htmlOutput("tutorial")
    #),
    #titlePanel(
     # textOutput("destTitle")
    #),
               tabPanel("Running map",
                        imageOutput("map")),
               tabPanel("Interactive maps",
                       imageOutput("image")),
                tabPanel("Regression stuff",
                         imageOutput("imagereg")),
                tabPanel("About",
                         #do I save an object for about text and then call it here? How do I do that? 
                         h5("about text"))))


server <- function(input, output, session) {
  #attempt to create a timer - need to figure out how to create timer from specific date
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The War in Yemen has been going on for:", Sys.time())
  })
  #this is where map will go 
    output$map <- renderImage({
        list(src = "map.gif",
             contentType = "image/gif")}, deleteFile = FALSE)}


shinyApp(ui, server)
