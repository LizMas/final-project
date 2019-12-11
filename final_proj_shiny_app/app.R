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
library(shinyWidgets)
library(wesanderson)
library(leaflet)
library(tidyverse)

#this is all of the data needed for the Shiny app to run: 

#read in rds of location and shape data for the summer gif: 

locate_all <- read_rds("clean-data_2/locations.rds")

shap <- read_rds("clean-data_2/shape.rds")

#this is for the fatalities by event type section: 

fat_sub_event <- locate_all %>% 
  select(sub_event_type, fatalities) %>% 
  dplyr::group_by(sub_event_type) %>% 
  summarise(sum_fatby_event = sum(fatalities)) %>% 
  filter(sum_fatby_event != 0)

group_colors <- c("Air/drone strike" = "indianred4", "Armed clash" = "lightsalmon4", "Attack" = "#CC6600", "Chemical weapon" ="firebrick1", "Disrupted weapons use" = "plum4", "Excessive force against protesters" = "orangered4", "Government regains territory" = "mistyrose4", "Grenade" = "slategrey", "Mob violence" = "lavenderblush4", "Non-state actor overtakes territory" = "cadetblue4", "Remote explosive/landmine/IED" = "antiquewhite4", "Sexual violence" = "black", "Shelling/artillery/missile attack" = "darkgoldenrod3", "Suicide bomb" = "darkseagreen4", "Violent demonstration" = "goldenrod3")
map_fat_sub_event <- ggplot(shap) +
  geom_sf(data = shap) +
  geom_sf(data = fat_sub_event, aes(color = sub_event_type, fill = sub_event_type)) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) + labs(title = "Attack Locations in Yemen per Event Type, 1 Jan 2015 - 8 Oct 2019",
                                                   fill= "Event Type", color = "Event Type")

#this is for the fatalities by actor section: 

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
                                    "Military Forces of Yemen" = "Houthi Movement - Ansar Allah", 
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
                                    #Saudi-backed, Salafi, likely AQAP affiliated but not positive 
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

#find total fatalities by actor1: 

fat_actor1 <- conflict_actor1_recode %>% 
  select(recode_actor1, event_date, fatalities) %>% 
  dplyr::group_by(recode_actor1) %>% 
  summarise(sum_fatby_actor1 = sum(fatalities)) %>% 
  filter(sum_fatby_actor1 != 0) %>% 
  arrange(desc(sum_fatby_actor1)) %>% 
  #head 9 because that's where it stops getting interesting number-wise
  head(9)

group_colors <- c("Unknown Affiliation" = "lightsalmon4", "Unidentified Militias" = "#CC6600", "Pro-Government Militias" ="mistyrose4", "Islamic State and Affiliates" = "plum4", "AQAP: Al Qaeda in the Arabian Peninsula" = "orangered4", "Separatist Militias" = "sienna", "Military Forces of United States" = "slategrey", "Military Forces of Yemen" = "cadetblue4", "Saudi Coalition Operations" = "indianred4")
map_fat_actor1 <- ggplot(shap) +
  geom_sf(data = shap) +
  geom_sf(data = fat_actor1, aes(color = recode_actor1, fill = recode_actor1)) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  scale_color_manual(values = group_colors) + labs(title = "Attack Locations in Yemen per Actor, 1 Jan 2015 - 8 Oct 2019",
                                                   fill= "Actor", color = "Actor")

#this is the data for the regression page: 

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
                 h5("Yemen’s civil war began in March 2015 between Yemeni government forces aligned with President Abdrabbuh Mansur Hadi, and a rebel force called the Houthi Movement. Since then, the conflict has devolved into a chaotic web of government forces, militias, and foreign entities who form alliances of convenience one moment and then dissolve them the next. This project attempts to make sense of this chaos through mapping and analyzing instances and attributability of attacks."),
                 leafletOutput("leaf"),
                 imageOutput("fig_fatalities")),
                 
    
    #here's where the two interactive maps go
    
    navbarMenu("Interactive Maps",

    #this is the fat by event type dropdown (2)
    
       tabPanel("Fatalities by Event Type",
             titlePanel("Fatalities by Event Type"),
             h5("In most asymmetric conflicts, actors employ different lethal methods depending on what they have at their disposal. The Saudi-led coalition mainly utilizes air strikes, with support from their coalition partners, while less well-funded actors like militias resort to smaller artillery and bomb attacks. Select these attack types in the drop-down menu below to see where they occurred."),
             sidebarLayout(position = "right",
                           sidebarPanel(
                             prettyRadioButtons("map_fat_sub_event",
                                         "Event Type:",
                                         choices = sort(unique(fat_sub_event$sub_event_type)),
                                         selected = "Air/drone strike",
                                        outline = TRUE, fill = TRUE,
                                         )),
                           mainPanel(
                             plotOutput("fat_sub_event"),
                             imageOutput("col_event")
                           )
             )),

 
    #this is the fat by actor drop down (1) 
    tabPanel("Fatalities by Actor",
             titlePanel("Fatalities by Actor"),
             h5("The Yemeni civil war is incredibly complex. ACLED has aggregated 270 distinct actors operating in Yemen; this project uses 173 of them. These actors include governments, state-sanctioned proxies, militias, and terrorist organizations, with the lines between groups often blurry. To make sense of this, I sorted the 173 groups with attributable fatalities greater than 5 into 9 affiliations. Select these actors in the drop-down menu below to see where they committed attacks."),
             sidebarLayout(position = "right", 
                           sidebarPanel(
                             selectInput("recode_actor1",
                                         "Actor:",
                                         choices = sort(unique(fat_actor1$recode_actor1)),
                                         selected = "Pro-Goverment Militias"
                                         
                             )),
                           mainPanel(
                             plotOutput("fat_actor1"),
                             h4("Snapshot of fatal events during summer 2019"),
                             h6("Heightened geopolitical tensions corresponded with high intensity conflict."),
                             imageOutput("map")
                           )
                           
             ))),
    
    #regressions are here: 
    
navbarMenu("Discussion",
           tabPanel("Overview",
                    h2("Overview: Endemic instability"),
                    h5("The dynamics of Yemen’s civil war and the sometimes disparate and/or capricious alliances are hard to understand if you are unfamiliar with the history and endemic instability of Yemen. Similarly, the limitations of this project are difficult to appreciate without an understanding of the historical background."),
                    h5("Yemen was previously a British protectorate. In the 1960s, the southern part of the protectorate rebelled against British rule, which cumulated with Britain withdrawing from southern Yemen and consolidating their colony in the north. The south subsequently became the People’s Democratic Republic of Yemen, the Arab world’s only communist state, which enjoyed the support of the USSR.  Meanwhile, the north was embroiled in a civil war which pitted republican forces against royalists. The republican forces won, and northern Yemen became the Yemen Arab Republic, eventually aligned with Saudi Arabia.   Peace was short-lived; in 1972, a war broke out between the Yemen Arab Republic, supported by the Saudis, and the People’s Republic of South Yemen, supported by the USSR. In 1979, a similar conflict broke out, and in 1986, the south experienced yet another civil war."),
                    h2("Unification"),
                    h5("On 22 May 1990, North and South Yemen unified into a singular country, the Republic of Yemen, led by President Ali Abdullah Saleh. Another civil war ensued for the following four years. A movement opposed to Saleh’s rule emerged, calling themselves the Houthis. The following decade saw an uneasy peace, occasionally interrupted by high-profile terrorist attacks committed by Al-Qaeda’s affiliate in the Arabian Peninsula (AQAP). 
Partially emboldened by the US invasion of Iraq, the Houthi movement began an insurgency in 2004 aimed at ousting Saleh.  They were ultimately unsuccessful, but Saleh was eventually ousted after Arab Spring protests in November 2011 and replaced by Abdrabbuh Mansour Hadi. The Houthis were critical of Hadi, who they and most of the country saw as Riyadh’s puppet, and in 2014, they began working with Saleh to oust Hadi in a surprising reversal of alliances."),
                    h2("The current civil war begins"),
                    h5("In August 2014, the Houthis seized control of most of the capital, Sanaa. Iran, seeing an opportunity to counter their regional rival Saudi Arabia, aligned with the Houthis. They began offering material and personnel support, including resources from their Lebanese proxy, Hizballah.  In response, Saudi Arabia assembled a coalition to drive the Houthis out. The entrance of the Saudi-led coalition on 22 March 2015 marks the “official” beginning of the civil war."),
                     ),
tabPanel("Methodology",
         h2("Methodology and Limitations"),
         h5("The complex history outlined in the previous tab has created a mess of intertwined and competing allegiances that encourage alliances of convenience. These alliances break down once they are no longer expedient. This not only makes the conflict more volatile, but it makes coding actors across time incredibly difficult. 
ACLED has aggregated 270 distinct actors operating in Yemen; this project uses 173 of them. I sorted these 173 groups with attributable fatalities greater than five into nine affiliations:"),
         h5("-	Military Forces of Yemen: Variable includes attacks committed by Hadi government forces AND Houthi forces. This variable is the most problematic in this project. Due to the duality of governance between the Hadi-led government and the Houthis, the Houthi’s incorporation of the Supreme Revolutionary Council as an interim working governing body, and the fact that individual actor alliances between the two governing forces have a high degree of fractionalization across years, it is nearly impossible to attribute attacks to Hadi or Houthi forces with any meaningful consistency. This was a methodological decision made by ACLED and continued by me, but it is not an endorsement of Houthi legitimacy."),
         h5("-	Military Forces of the United States: Variable includes attacks committed directly by US forces."),
         h5("-	Saudi-led Coalition: Variable includes all members of the GCC (minus Oman) who joined the coalition at the outset of the war; Morocco, Egypt, Sudan, and Jordan, who pledged military support, and the United States when they operate within the coalition, usually within an intelligence or logistic capacity."),
         h5("-	Pro-Government Militias: Variable includes militias that align with either the Hadi-led government or the Houthis."),
         h5("-	Separatist Militias: Variable includes southern secessionists, including the Southern Transitional Council (STC)."),
         h5("-	Islamic State and Affiliates: Variable includes attacks by IS and affiliates that have directly pledged allegiance to the organization. This variable is coded differently than AQAP because, contrary to popular belief, they do not get along."),
         h5("-	AQAP: Variable includes attacks by al-Qaeda’s branch in the Arabian Peninsula."),
         h5("-	Unknown Affiliation: Variable includes actors that do not present a clear affiliation."),
         h5("-	Unidentified Militias: Variable includes attacks by militias that were not identified."
         )),

tabPanel("Analysis",
         h2("Analysis"), 
         h5("The three deadliest actors in Yemen’s Civil War are: Military Forces aligned with the Yemeni Government (remember, per the methodology section, this variable includes both Hadi government forces and Houthi forces), who are responsible for 68,104 fatalities; the Saudi-led Coalition, who are responsible for 17,717 fatalities; and Pro-Government Militias (PGMs), who are responsible for 6,275. To analyze these actors’ individual contribution to fatalities, I ran three Poisson regressions that show trends in lethality based on number of attacks per actor:"), 
         br(),
         br(),
        htmlOutput("model"),
        br(), 
        h5("The regression table above shows that Pro-Government Militias and Saudi-led Coalition operations have a negative correlation to fatalities, with the Coalition presenting a much stronger negative correlation. Meanwhile, Military Forces of Yemen present a positive correlation with respect to fatalities. Below, you can find individual visualizations of these findings with short discussions."),
        br(), 
         imageOutput("saudi_model"),
         br(),
         br(),
         br(),
         br(),
         h5("Saudi-Led Coalition Operations have a stronger negative correlation with respect to fatalities, presenting a regression coefficient of -1.303. This means that for each attack committed by members of the Coalition, the individual attacks were less deadly than the other actors. This is possibly due to the Coalition’s comparatively advanced weaponry and support, which makes targeted attacks more possible, thus reducing collateral damage."),
         br(),
         br(),
         imageOutput("yemen_model"),
         br(),
         br(),
         br(),
         br(),
         h5("Yemen Government Operations have a positive correlation with respect to fatalities, presenting a regression coefficient of 1.227. This means that attacks committed by these actors were more deadly, possibly because these actors do not have the same logistic, material, and intelligence support that the Saudi-led coalition does. This may impact their ability to perform accurate assessments of targets and increase their reliance on old and/or less advanced equipment. These factors may increase the likelihood of poorly targeted or indiscriminate attacks which lead to higher levels of collateral damage. These actors are by far the most active in the conflict, suggesting that their attack methods increase their lethality even more."),
         br(),
         br(),
         imageOutput("pgm_model"),
         br(),
         br(),
         br(),
         br(),
         h5("PGMs have a slight negative correlation with respect to fatalities, although this trend is hard to see based on the regression line above. Per the regression coefficient of -0.051, for each single increase in attacks committed by PGMs, the chance of those attacks being as fatal as if they were committed by the other actors in the data decreases by 0.051. You can also see in the visualization that PGMs committed less high-casualty attacks than other actors."),
         br(),
         br(),
          )),

tabPanel("About",
         mainPanel(
           #imageOutput("acled"),
           h2("The Data"),
           br(),
           br(),
           div(style = "padding: 0px 0px; margin-top:-2em",
           h5("The Armed Conflict Location & Event Data (ACLED) Project is a “disaggregated data collection, analysis and crisis mapping project” that has collected an incredibly detailed account of political violence and protest events. This app uses data from their collection on Yemen between 1 January 2015 and 8 October 2019, which they have generously made available for public use. Please check out their work ", a("here.", href="https://www.acleddata.com/about-acled/")),
           h2("Connect"),
           h5("You can contact me at lizmasten@g.harvard.edu or connect with my on ", a("LinkedIn.", href="www.linkedin.com/in/elizabeth-masten-642567196")),
           h2("Technical"),
           h5("The source code for this site can be found at my ", a("GitHub.", href="https://github.com/LizMas")),
               
           
         )))))


server <- function(input, output, session) {
  toggleModal(session, "tutorialModal", toggle = "open")

  #this is where the opening leaflet Yemen map goes 
  output$leaf <- renderLeaflet({
    leafy <- leaflet(states) %>%
      setView(lng = 48.5164,lat = 15.5527, zoom = 5.3) %>% 
      addTiles() 
    leafy
  })
  
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
    
    #this is an upload of the ACLED image in the About page, it's currently commented out of the UI pending a way to fix the margins 
    
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
  group_colors <- c("Unknown Affiliation" = "lightsalmon4", "Unidentified Militias" = "#CC6600", "Pro-Government Militias" ="mistyrose4", "Islamic State and Affiliates" = "plum4", "AQAP: Al Qaeda in the Arabian Peninsula" = "orangered4", "Separatist Militias" = "sienna", "Military Forces of United States" = "slategrey", "Military Forces of Yemen" = "cadetblue4", "Saudi Coalition Operations" = "indianred4")
  map_fat_actor1 <- ggplot(shap) +
    geom_sf(data = shap) +
    geom_sf(data = fat_actor1, aes(color = recode_actor1, fill = recode_actor1)) +
   scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
    scale_color_manual(values = group_colors) + labs(title = "Attack Locations in Yemen per Actor, 1 Jan 2015 - 8 Oct 2019",
                                                     fill= "Actor", color = "Actor")
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


#interactive map of event types 

output$fat_sub_event <- renderPlot({
  fat_sub_event <- fat_sub_event %>% filter(sub_event_type == input$map_fat_sub_event)
  group_colors <- c("Air/drone strike" = "indianred4", "Armed clash" = "lightsalmon4", "Attack" = "#CC6600", "Chemical weapon" ="firebrick1", "Disrupted weapons use" = "plum4", "Excessive force against protesters" = "orangered4", "Government regains territory" = "mistyrose4", "Grenade" = "slategrey", "Mob violence" = "lavenderblush4", "Non-state actor overtakes territory" = "cadetblue4", "Remote explosive/landmine/IED" = "antiquewhite4", "Sexual violence" = "black", "Shelling/artillery/missile attack" = "darkgoldenrod3", "Suicide bomb" = "darkseagreen4", "Violent demonstration" = "goldenrod3")
  map_fat_sub_event <- ggplot(shap) +
    geom_sf(data = shap) +
    geom_sf(data = fat_sub_event, aes(color = sub_event_type, fill = sub_event_type)) +
    scale_fill_manual(values = group_colors) +
    scale_color_manual(values = group_colors) + labs(title = "Attack Locations in Yemen per Event Type, 1 Jan 2015 - 8 Oct 2019",
    fill= "Event Type", color = "Event Type")
  map_fat_sub_event
  
  
})

}

#run the app: 

shinyApp(ui, server)


