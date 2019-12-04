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
library(ggridges)
library(plotly)
library(png)
library(leaflet)
library(tidyverse)

locate_all <- read_rds("clean-data_2/locations.rds")

shap <- read_rds("clean-data_2/shape.rds")

fat_sub_event <- locate_all %>% 
  select(sub_event_type, fatalities) %>% 
  dplyr::group_by(sub_event_type) %>% 
  summarise(sum_fatby_event = sum(fatalities)) %>% 
  filter(sum_fatby_event != 0)

map_fat_sub_event <- ggplot(shap) +
  geom_sf(data = shap) +
  geom_sf(data = fat_sub_event, aes(color = sub_event_type))

#recode actor1 

locate_all$actor1 <- as.factor(locate_all$actor1)

conflict_actor1_recode <- locate_all %>% 
  mutate(recode_actor1 = fct_recode(actor1,
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2016-) Supreme Political Council",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-)",
                                    "Saudi Coalition Operations" = "Operation Restoring Hope",
                                    "Unidentified Militias" = "Unidentified Armed Group (Yemen)",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) Security Belt Forces",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) Hadhrami Elite Forces",
                                    "Unidentified Militias" = "Unidentified Tribal Militia (Yemen)",
                                    "Pro-Government Militias" = "Hujur Tribal Militia (Yemen)",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2015-2016) Supreme Revolutionary Committee",
                                    "Pro-Government Militias" = "National Resistance Forces",
                                    "Pro-Government Militias" = "Popular Resistance",
                                    "Separatist Militias" = "Southern Resistance", 
                                    "Saudi Coalition Operations" = "Operation Decisive Storm",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) Security Belt Forces",
                                    "Pro-Government Militias" = "Militia (Abu al Abbas)", 
                                    "Military Forces of Yemen" = "Unidentified Military Forces",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) 35th Armored Brigade",
                                    "Police Forces of Yemen" = "Police Forces of Yemen (2012-)",
                                    "Pro-Government Militias" = "Bilharith Tribal Militia (Yemen)",
                                    "Separatist Militias" = "Houthi Movement - Ansar Allah", 
                                    "Pro-Government Militias" = "Militia (Pro-Government)",
                                    "Pro-Government Militias" = "Al Humayqani Tribal Militia (Yemen)",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) Shabwani Elite Forces",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2016-) Republican Guard",
                                    #Anwar al-Awlaki 
                                    "Al-Qaeda and Affiliates" = "Al Awlaki Tribal Militia (Yemen)",
                                    "Al-Qaeda and Affiliates" = "AQAP: Al Qaeda in the Arabian Peninsula	",
                                    #STC alligned
                                    "Separatist Militias" = "Subaihi Tribal Militia (Yemen)",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) Presidential Guard",
                                    "Military Forces of Yemen", "Military Forces of Yemen (2012-) Border Guard",
                                    "Police Forces of Yemen", "Police Forces of Yemen (2015-2016) Special Security Forces",
                                    "Military Forces of Yemen", "Military Forces of Yemen (2012-) 17th Infantry Brigade",
                                    "Pro-Government Militias" = "Al Islah Party",
                                    "Pro-Government Militias" = "Giants Brigade",
                                    "Islamic State and Affiliates" = "Ansar al Sharia (Yemen)",
                                    "Islamic State and Affiliates" = "Islamic State (Yemen)",
                                    "Pro-Government Militias" = "Tihama Resistance",
                                    "Police Forces of Yemen" = "Police Forces of Yemen (2016-) Supreme Political Council",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) 22nd Armored Brigade",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) 30th Brigade",
                                    "Pro-Government Militias" = "Militia (Hamoud Saeed al Mikhlafi)",
                                    "Protesters/ Rioters" = "Rioters (Yemen)",
                                    "Protesters/ Rioters" = "Protesters (Yemen)",
                                    "Unidentified Militias" = "Al al Amir Tribal Militia (Yemen)",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) 170th Air Defence Brigade",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) Saiqa Brigades",
                                    #was UAE backed before withdrawal 
                                    "Separatist Militias" = "Southern Movement",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2015-2016) Republican Guard", 
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) 102nd Brigade",
                                    #aligned with Hajouri against Houthis as of Mar 2019
                                    "Pro-Government Militias" = "Murad Tribal Militia (Yemen)",
                                    "Unidentified Militias" = "Unidentified Communal Militia (Yemen)",
                                    "Unknown Affiliation" = "Lawdar Communal Militia (Yemen)",
                                    "Unknown Affiliation" = "Azal Resistance",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) Political Security Organisation", 
                                    #Saudi-backed, Salafi, likely AQAP affiliated 
                                    "Pro-Government Militias" = "Militia (Al Mihdhar)",
                                    "Unknown Affiliation" = "Bani Khawlah Tribal Militia (Yemen)",
                                    "Unknown Affiliation" = "Riyam Tribal Militia (Yemen)",
                                    "Unknown Affiliation" = "Al Abyadh Communal Militia (Yemen)",
                                    "Unknown Affiliation" = "Al Ghanam Tribal Militia (Yemen)",
                                    "Unknown Affiliation" = "Al Jalal Tribal Militia (Yemen)",
                                    "Unknown Affiliation" = "Al Jalal Tribal Militia (Yemen)",
                                    "Unknown Affiliation" = "Al Jaradi Tribal Militia (Yemen)",
                                    "Unknown Affiliation" = "Al Udhr Tribal Militia (Yemen)",
                                    "Unknown Affiliation" = "Al Warawirah Tribal Militia (Yemen)",	
                                    "Unknown Affiliation" = "Ba Awdah Tribal Militia (Yemen)",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) 63rd Brigade",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) 9th Brigade",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) Coast Guard",
                                    "Unknown Affiliation" = "Militia (Mukhtar al Zurayqi)",
                                    "Unknown Affiliation" = "Ubaydah Tribal Militia (Yemen)",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) Support and Reinforcement Brigade",
                                    "Unknown Affiliation" = "As Silah Communal Militia (Yemen)",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) 4th Brigade",
                                    "Unknown Affiliation" = "Al Hussein bin Mohammed Tribal Militia (Yemen)",
                                    #I stopped with the Unknown Affiliations here because there were 20 more with only 1 reported fatality each 
                                    "Military Forces of Yemen" = "MIlitary Forces of Yemen (2012-)",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) Counter-Terrorism Unit",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) Republican Guard",
                                    "Police Forces of Yemen" = "Police Forces of Yemen (2015-2016) Prison Guards",
                                    "Military Forces of Yemen" = "Military Forces of Yemen (2012-) Tihami Elite Forces",
                                    "Police Forces of Yemen" = "Police Forces of Yemen (2016-) Prison Guards",
                                    "Police Forces of Yemen" = "Police Forces of Yemen (2016-) Special Security Forces" ))

#find total fatalities by actor1

fat_actor1 <- conflict_actor1_recode %>% 
  select(recode_actor1, event_date, fatalities) %>% 
  dplyr::group_by(recode_actor1) %>% 
  summarise(sum_fatby_actor1 = sum(fatalities)) %>% 
  filter(sum_fatby_actor1 != 0) %>% 
  arrange(desc(sum_fatby_actor1)) %>% 
  #head 9 because that's where it stops getting interesting number-wise
  head(9)

map_fat_actor1 <- ggplot(shap) +
  geom_sf(data = shap) +
  geom_sf(data = fat_actor1, aes(color = recode_actor1))

#for toggle jitter plot 

#xx <- fat_actor1 %>% 
#  filter(recode_actor1 %in% c("Military Forces of Yemen", "Saudi Coalition Operations", "Pro-Government Militias", "Unidentified Militias", "Islamic State and Affiliates", "Sepratist Militias", "Unidentified Militias")) %>% 
#  group_by(recode_actor1)

#this is for the slider graph 

#xx <- fat_actor1 %>% filter(recode_actor1 %in% c("Military Forces of Yemen", "Saudi Coalition Operations", "Pro-Government Militias", "Unidentified Militias", "Islamic State and Affiliates", "Sepratist Militias", "Unidentified Militias")) %>%
#  group_by(recode_actor1)

#xx$fatalities <- as.factor(xx$fatalities)

#xxyear <- xx %>% group_by(year)

#This is the data for the regression page 

pgm <- conflict_actor1_recode %>% 
  select(recode_actor1, event_date, fatalities) %>% 
  dplyr::group_by(recode_actor1) %>% 
  mutate(sum_fatby_actor1 = sum(fatalities)) %>% 
  filter(sum_fatby_actor1 != 0) %>% 
  arrange(desc(sum_fatby_actor1)) %>% 
  mutate(progov_militias = ifelse(recode_actor1 =="Pro-Government Militias", 1, 0))

saudi <- conflict_actor1_recode %>% 
  select(recode_actor1, event_date, fatalities) %>% 
  dplyr::group_by(recode_actor1) %>% 
  mutate(sum_fatby_actor1 = sum(fatalities)) %>% 
  filter(sum_fatby_actor1 != 0) %>% 
  arrange(desc(sum_fatby_actor1)) %>% 
  mutate(saudi_ops = ifelse(recode_actor1 =="Saudi Coalition Operations", 1, 0))

yemen_gov <- conflict_actor1_recode %>% 
  select(recode_actor1, event_date, fatalities) %>% 
  dplyr::group_by(recode_actor1) %>% 
  mutate(sum_fatby_actor1 = sum(fatalities)) %>% 
  filter(sum_fatby_actor1 != 0) %>% 
  arrange(desc(sum_fatby_actor1)) %>% 
  mutate(yemen_ops = ifelse(recode_actor1 =="Military Forces of Yemen", 1, 0))



 
#shiny app starts here --------------------------------

ui <- fluidPage(theme = shinytheme("flatly"),
                position = "static-top",
                
                
    navbarPage("Conflict in the Yemeni Civil War",
    
              tabPanel("Fatalities",
                 titlePanel("Fatalities in the Yemeni Civil War, Summer 2019"),
                 imageOutput("map"),
                 imageOutput("fig_fatalities")),  
    
    
    #here's where the two interactive maps go
    
    navbarMenu("Interactive Maps",
               #this is the fat by actor drop down (1) 
               tabPanel("Fatalities by Actor",
                        titlePanel("Fatalities by Actor"),
                        sidebarLayout(position = "right",
                                      sidebarPanel(
                                        selectInput("recode_actor1",
                                                    "Actor:",
                                              choices = sort(unique(fat_actor1$recode_actor1)),
                                              selected = "Pro-Goverment Militias",
                                              multiple = TRUE,
                                              )),
                                      mainPanel(
                                        plotOutput("fat_actor1"),
                       # sidebarLayout(position = "right",
                        #              sidebarPanel(
                         #               selectInput("year",
                          #                          "Year:",
                           #                         choices = sort(unique(fat_actor_slider$year)),
                            #                        selected = "2015")),
                             #         mainPanel(plotOutput("fat_actor_slider"),

                                        
                                        
                                      
                                        
                                      )
                                      
                        )),
    #this is the fat by event type dropdown (2)
    
    tabPanel("Fatalities by Event Type",
             titlePanel("Fatalities by Event Type"),
             sidebarLayout(position = "right",
                           sidebarPanel(
                             selectInput("map_fat_sub_event",
                                         "Event Type:",
                                         choices = sort(unique(fat_sub_event$sub_event_type)),
                                         selected = "Air/drone strike",
                                         multiple = TRUE)),
                           mainPanel(
                             plotOutput("fat_sub_event")
                             
                           )
             ))),

    
    #regression will go here: 
tabPanel("Analysis",
         imageOutput("pgm_model")),

tabPanel("About",
         mainPanel(
           h2("The Data"),
           h5("The Armed Conflict Location & Event Data (ACLED) Project is a “disaggregated data collection, analysis and crisis mapping project” that has collected an incredibly detailed account of political violence and protest events. This site uses data from their collection on Yemen between 1 January 2015 (a few months before the official start of the civil war) and 8 October 2019. Please check out their work ", a("here.", href="https://www.acleddata.com/about-acled/")),
           h2("Connect"),
           h5("You can contact me at lizmasten@g.harvard.edu or connect with my on ", a("LinkedIn.", href="www.linkedin.com/in/elizabeth-masten-642567196")),
           h2("Technical"),
           h5("The source code for this site can be found at my ", a("GitHub.", href="https://github.com/LizMas")),
                                                
           
         ))))


server <- function(input, output, session) {
  toggleModal(session, "tutorialModal", toggle = "open")
  
  #This is for the pop-up 
  output$tutorial <- renderText({
    HTML("Text for About Page goes here")})
  
  #this is where map goes 
    output$map <- renderImage({
        list(src = "map.gif",
             contentType = "image/gif")}, deleteFile = FALSE)
    
    #this is a plot, just uploading an image of a graph for ease
    
    output$fig_fatalities <- renderImage({
      filename <- "fig1update.png"
      list(src = filename)
    }, deleteFile = FALSE)

    output$pgm_model <- renderImage({
      filename2 <- "pgm_model.png"
      list(src = filename2)
    }, deleteFile = FALSE)

    #this is where map_fat_by_actor1 goes

output$fat_actor1 <- renderPlot({
  fat_actor1 <- fat_actor1 %>% filter(recode_actor1 == input$recode_actor1)

  map_fat_actor1 <- ggplot(shap) +
    geom_sf(data = shap) +
    geom_sf(data = fat_actor1, aes(color = recode_actor1, fill = recode_actor1))
  map_fat_actor1
  
})

#slider for fatalities by actor 

output$fat_actor_slider <- renderPlot({
  
  fat_actor_slider <- xxyear %>% filter(year == input$year)
  
  fat_slider_actor <- xxyear %>% 
    ggplot(aes(x = recode_actor1, y = xxyear$fatalities, color = recode_actor1, fill = recode_actor1)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
 slider_actor
  })


#ggplot vs plot_ly- watch datacamp 
output$fat_sub_event <- renderPlot({
  fat_sub_event <- fat_sub_event %>% filter(sub_event_type == input$map_fat_sub_event)
  map_fat_sub_event <- ggplot(shap) +
    geom_sf(data = shap) +
    geom_sf(data = fat_sub_event, aes(color = sub_event_type, fill = sub_event_type))
  map_fat_sub_event
  
 # ggplotly(map_fat_sub_event)
  #map_fat_sub_event
  
})

}

shinyApp(ui, server)


