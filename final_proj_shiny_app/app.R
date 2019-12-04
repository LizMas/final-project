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
                
                
    navbarPage("Fatalities in Yemen's Civil War",
    
              tabPanel("Fatalities",
                 titlePanel("Fatalities in the Yemeni Civil War"),
                 h5("Yemen’s Civil War began in March 2015 between the Yemeni Government, 
                    led by Abdrabbuh Mansur Hadi, and separatist rebels called the Houthis. 
                    Since then, Saudi Arabia, the UAE, Bahrain, the United States, the United Kingdom, France, Iran, and others have participated by providing material, logistic, intelligence, or other forms of support to certain factions. The war has plunged Yemen into the world’s largest humanitarian disaster. Between conflict and famine, ACLED estimates that the war has killed over 100,000 Yemenis."),
                 h4("Snapshot of Fatalities during Summer 2019"),
                 h6("Heightened geopolitical tensions corresponded with high intensity conflict."),
                 imageOutput("map"),
                 imageOutput("fig_fatalities")),  
    
    
    #here's where the two interactive maps go
    
    navbarMenu("Interactive Maps",

    #this is the fat by event type dropdown (2)
    
       tabPanel("Fatalities by Event Type",
             titlePanel("Fatalities by Event Type"),
             h5("In most asymmetric conflicts, actors employ different lethal methods depending on what they have at their disposal. The Saudi-led coalition mainly utilizes airpower like drone strikes, with support from their coalition partners, while less well-funded actors like militias resort to smaller artillery and bomb attacks. Select these attack types in the drop-down menu below to see where they occurred."),
             sidebarLayout(position = "right",
                           sidebarPanel(
                             radioButtons("map_fat_sub_event",
                                         "Event Type:",
                                         choices = sort(unique(fat_sub_event$sub_event_type)),
                                         selected = "Air/drone strike",
                                         )),
                           mainPanel(
                             plotOutput("fat_sub_event"),
                             imageOutput("col_event")
                           )
             )),

    #this is the fat by actor drop down (1) 
    tabPanel("Fatalities by Actor",
             titlePanel("Fatalities by Actor"),
             h5("The Yemeni civil war is incredibly complex; ACLED recorded 173 unique actors since 2015. These actors include governments, state-sanctioned proxies, militias, and terrorist organizations, with the lines between groups often blurry. To make sense of this, I sorted all 173 groups with attributable fatalities greater than 5 into 9 affiliations. Select these actors in the drop-down menu below to see where they committed attacks."),
             sidebarLayout(position = "right", 
                           sidebarPanel(
                             selectInput("recode_actor1",
                                         "Actor:",
                                         choices = sort(unique(fat_actor1$recode_actor1)),
                                         selected = "Pro-Goverment Militias"
                                         
                             )),
                           mainPanel(
                             plotOutput("fat_actor1"),
                             imageOutput("col_actor"),
                             br(),
                             br(),
                             br(), 
                             htmlOutput("model2"),
                           )
                           
             ))),
    #here are the regressions. The explanations take some work. 
    #I plan to incorporate some of my PDF work into the wording here.
    
tabPanel("Analysis",
         h5("The top three deadliest actors in Yemen’s Civil War are: Military Forces aligned with the recognized Yemeni Government, who are responsible for 68,104 fatalities; the Saudi-led Coalition, who are responsible for 17,717 fatalities; and Pro-Government Militias, who are responsible for 6,275. Below is a regression table of these three actors."), 
        htmlOutput("model"),
        br(), 
        br(),
        br(), 
         imageOutput("pgm_model"),
         br(),
         br(),
         br(),
         br(),
         h5("Pro-Government Militias have a slight negative correlation with respect to fatalities."),
         br(),
         br(),
         imageOutput("saudi_model"),
         br(),
         br(),
         br(),
         br(),
         h5("Saudi-Led Coalition Operations have a stronger negative correlation with respect to fatalities."),
         br(),
         br(),
         imageOutput("yemen_model"),
         br(),
         br(),
         br(),
         br(),
         h5("Yemen Government Operations have a positive correlation with respect to fatalities."),
         br(),
         br(),
         ),

tabPanel("About",
         mainPanel(
           h2("The Data"),
           imageOutput("acled"),
           div(style = "padding: 0px 0px; margin-top:-2em",
           h5("The Armed Conflict Location & Event Data (ACLED) Project is a “disaggregated data collection, analysis and crisis mapping project” that has collected an incredibly detailed account of political violence and protest events. This app uses data from their collection on Yemen between 1 January 2015 (a few months before the official start of the civil war) and 8 October 2019. Please check out their work ", a("here.", href="https://www.acleddata.com/about-acled/")),
           h2("Connect"),
           h5("You can contact me at lizmasten@g.harvard.edu or connect with my on ", a("LinkedIn.", href="www.linkedin.com/in/elizabeth-masten-642567196")),
           h2("Technical"),
           h5("The source code for this site can be found at my ", a("GitHub.", href="https://github.com/LizMas")),
                                                
           
         )))))


server <- function(input, output, session) {
  toggleModal(session, "tutorialModal", toggle = "open")

  
  #this is where map goes 
    output$map <- renderImage({
        list(src = "map.gif",
             contentType = "image/gif")}, deleteFile = FALSE)
    
    #this is a plot, just uploading an image of a graph for ease
    
    output$fig_fatalities <- renderImage({
      filename <- "fig1update3.png"
      list(src = filename)
    }, deleteFile = FALSE)
    
    #this image is a plot of fatalities by PGMs, just uploading a png

    output$pgm_model <- renderImage({
      filename2 <- "pgm_model2.png"
      list(src = filename2)
    }, deleteFile = FALSE)
    
    #this image is a plot of fatalities by Saudi Ops, just uploading a png
    
    output$saudi_model <- renderImage({
      filename3 <- "saudi_model2.png"
      list(src = filename3)
    }, deleteFile = FALSE)
    
    #this image is a plot of fatalities by Yemen Gov Ops, just uploading a png
    
    output$yemen_model <- renderImage({
      filename4 <- "yemen_gov_model2.png"
      list(src = filename4)
    }, deleteFile = FALSE)
    
    #this is an upload of the ACLED image in the About page 
    
    output$acled <- renderImage({
      filename5 <- "ACLED.png"
      list(src = filename5)
    }, deleteFile = FALSE)
    
    #This is a geom_col of fatalities by actor for interactive map tab 
    
    output$col_actor <- renderImage({
      filename6 <- "plot_actor_col.png"
      list(src = filename6, height="100%", width="100%", style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE)
    
    #This is geom_col of fatalities by event 
    
    output$col_event <- renderImage({
      filename7 <- "plot_event_col.png"
      list(src = filename7, height="100%", width="100%", style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE)
    
    #this is where map_fat_by_actor1 goes

output$fat_actor1 <- renderPlot({
  fat_actor1 <- fat_actor1 %>% filter(recode_actor1 == input$recode_actor1)
  group_colors <- c("Unknown Affiliation" = "#333BFF", "Unidentified Militias" = "#CC6600", "Pro-Government Militias" ="#9633FF", "Islamic State and Affiliates" = "#E2FF33", E = "#E3DB71")
  map_fat_actor1 <- ggplot(shap) +
    geom_sf(data = shap) +
    geom_sf(data = fat_actor1, aes(color = recode_actor1, fill = recode_actor1)) #+
    #scale_fill_manual(values=group_colors)
  map_fat_actor1
  
})

#here is where the html of my stargazer table is going 

getPage <- function() {
  return(includeHTML("models3.html"))
  
}

output$model <- renderUI({
  getPage()
  
})

#here is html of actor table for fatalities by actor tab
getPage2 <- function() {
  return(includeHTML("gt_actor2.html"))
  
}

output$model2 <- renderUI({
  getPage2()
  
})


#ggplot vs plot_ly- watch datacamp 
output$fat_sub_event <- renderPlot({
  fat_sub_event <- fat_sub_event %>% filter(sub_event_type == input$map_fat_sub_event)
  map_fat_sub_event <- ggplot(shap) +
    geom_sf(data = shap) +
    geom_sf(data = fat_sub_event, aes(color = sub_event_type, fill = sub_event_type))
  map_fat_sub_event
  
  
})

}

shinyApp(ui, server)


