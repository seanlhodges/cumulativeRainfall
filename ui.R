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
                choices=c("Akitio at Toi Flat",  
                          "Kumeti at Rua Roa", 
                          "Manawatu at Apiti Track", 
                          "Mangaetoroa at Scarrows",
                          "Oroua at Feilding",
                          "Oroua at Rangiwahia",  
                          "Pohangina at Alphabet Hut", 
                          "Pohangina at Delaware Ridge", 
                          "Pohangina at Makawakawa Divide", 
                          "Pohangina at Range View Farm",  
                          "Tamaki at Tamaki Reserve", 
                          "Tiraumea at Alfredton",  
                          "Upper Mangahao at No.1 Dam")),

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
               selected="30yr average")
   
),
 
 ##############################################
 # Show the caption and plot of the requested variable against mpg
  mainPanel(
    tabsetPanel(
      tabPanel("Barplot", plotOutput("dodgebarplot", width="600px", height="450px"))
      
    )
  )
))
