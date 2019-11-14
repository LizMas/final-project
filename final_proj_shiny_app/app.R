library(shiny)
library(markdown)
library(ggplot2)
library(fs)
library(sf)
library(readr)
library(gganimate)
library(transformr)
library(lubridate)
library(tidyverse)

ui <- fluidPage(
    navbarPage("Fatalities in the Yemeni Civil War",
               tabPanel("Running map",
                        h5("Source:")),
               tabPanel("Interactive maps",
                        imageOutput("image")),
                tabPanel("Regression stuff",
                         imageOutput("imagereg")),
                tabPanel("About",
                         h5("about text"))))


server <- function(input, output, session) {
    
    output$image <- renderImage({
        list(src = "loc.gif",
             contentType = "image/gif")}, deleteFile = FALSE)}


shinyApp(ui, server)
