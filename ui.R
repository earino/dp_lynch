
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(maps)


lynchings <- read.csv("lynchings.csv", strip.white=TRUE)

lynchings %>% 
  group_by(Offense) %>% 
  summarise(offense_count=n()) %>% 
  filter(offense_count > 10) -> offenses_to_keep

lynchings %>% filter(Offense %in% offenses_to_keep$Offense) -> lynchings

states_map <- map_data("state")
lynchings$State <- tolower(state.name[match(lynchings$State, state.abb)])
ls <- as.data.frame(table(lynchings$State))
lynch_map <- merge(states_map, ls, by.x="region", by.y="Var1")

offenses <- sort(unique(toupper(lynchings$Offense)))
                 
shinyUI(fluidPage(

  # Application title
  titlePanel("Lynchings in the South"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Create maps with Lynching data"),
      
      selectInput("offense", 
                  label = "Show by Offsense",
                  choices = c(NA, offenses))
      

    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("map"),
      plotOutput("trend"),
      plotOutput("genders")
    )
  )
))
