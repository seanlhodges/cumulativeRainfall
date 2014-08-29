library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
headerPanel("Rainfall Summary"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    
 ##############################################
    selectInput("raingauge", "Location:",
                choices=c("Akitio at Toi Flat", "Forest Rd Drain at Drop Structure", 
                          "Hautapu at Alabasters", "Kahuterawa at Scotts Road", "Kawhatau Catchment at Upper Kawhatau", 
                          "Kumeti at Rua Roa", "Makino at Halcombe Road", "Makino at Cheltenham", 
                          "Makuri at Bee 4 Trig", "Makohine at Zohs Road", "Manawatu at Apiti Track", 
                          "Manawatu at Moutoa", "Mangaetoroa at Scarrows", "Mangahao at Kakariki", 
                          "Mangaone at Milson Line", "Mangaone at Valley Road", "Mangatainoka at Hillwood Hukanui", 
                          "Mangawhero at Aberfeldy", "Mangawhero at Bangonie", "Mangawhero at Raupiu Road", 
                          "Matarawa at Matarawa Valley", "Ohau at Makahika", "Ohura at Tokorima", 
                          "Ohura at Waitewhena Airstrip", "Oroua at Rangiwahia", "Pakihikura at Pakihikura Airstrip", 
                          "Pohangina at Alphabet Hut", "Pohangina at Delaware Ridge", "Pohangina at Makawakawa Divide", 
                          "Pohangina at Range View Farm", "Porewa Catchment at Tututotara", 
                          "Rangitikei at Erewhon Station", "Ruatiti at Ruatiti Station", 
                          "Tamaki at Tamaki Reserve", "Tiraumea at Alfredton", "Tiraumea at Ohehua Repeater", 
                          "Turakina at Koeke Airstrip", "Turakina at ONeills Bridge", "Turakina at Otairi", 
                          "Turakina at Ruanui", "Tutaenui at Ribby Farm", "Tutaenui at Green Haven Farm", 
                          "Waiharuru at S.H.49", "Whangaehu at Aranui", "Whangaehu at Kauangaroa", 
                          "Whangaehu at Kowhai St Mangamahu", "Whangaehu at Titoki", "Whangaehu at Tukino Repeater", 
                          "Whangamomona at Bridge to Somewhere", "Whangamomona at Marco Road", 
                          "Whanganui at Pipiriki", "Whanganui at Te Porere", "Whanganui at Te Rewa", 
                          "Waitarere Forest Climate Station", "Waimarino Forest Climate Station", 
                          "Upper Mangahao at No.1 Dam", "National Park Climate Station", 
                          "Lismore Forest Climate Station", "Lower Retaruke")),

   ##############################################
    selectInput("cumulymax", "Annual Max",
                choices=c(1500,2000,3000,4000,5000),
                selected="3000"),
    
    
   ##############################################
   selectInput("yoi", "Year of Interest",
               choices=c("None",as.character(seq(1980,2013))),
               selected="None"),

   ##############################################
   selectInput("recLength", "Compare rain to 30yr average or just use available data?",
               choices=c("Available record","30yr average"),
               selected="Available record")
   
),
 
 ##############################################
 # Show the caption and plot of the requested variable against mpg
  mainPanel(
    tabsetPanel(
      tabPanel("Barplot", plotOutput("dodgebarplot", width="600px", height="450px"))
      
    )
  )
))
