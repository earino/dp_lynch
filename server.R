
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(maps)
library(dplyr)
library(plyr)
library(ggplot2)
# lynchings <- read.csv("lynchings.csv")
# zip_codes_states <- read.csv("zip_codes_states.csv")


shinyServer(function(input, output) {

  output$map <- renderPlot({
    lynchings <- read.csv("lynchings.csv", strip.white=TRUE)
    #lynchings$Freq <- as.factor(lynchings$Freq)
    states_map <- map_data("state")
    lynchings$State <- tolower(state.name[match(lynchings$State, state.abb)])
    if (input$offense != "NA") {
      lynchings <- subset(lynchings, toupper(Offense) == input$offense )
    }
    ls <- as.data.frame(table(lynchings$State))
    lynch_map <- merge(states_map, ls, by.x="region", by.y="Var1", all.x=TRUE)
    lynch_map <- arrange(lynch_map, group, order)
    #lynch_map[is.na(lynch_map)] <- 0
    View(lynch_map)
    ggplot(lynch_map, aes(x=long, y=lat, group=group, fill=Freq)) + geom_polygon(colour="black") + 
      ggtitle("Lynchings by State") + scale_fill_gradient(na.value="grey50")
  })

  output$trend <- renderPlot({
    lynchings <- read.csv("lynchings.csv", strip.white=TRUE)
    #lynchings$Freq <- as.factor(lynchings$Freq)
    states_map <- map_data("state")
    lynchings$State <- tolower(state.name[match(lynchings$State, state.abb)])
    if (input$offense != "NA") {
      lynchings <- subset(lynchings, toupper(Offense) == input$offense )
    }

    lynchings %>% group_by(Year) %>% summarise(Freq = n()) -> by_year
    by_year$Year <- as.numeric(as.character(by_year$Year))
    
    ggplot(by_year, aes(x=Year, y=Freq)) + geom_line() +  ggtitle("Lynchings by Year")
    })
  
  output$genders <- renderPlot({
    lynchings <- read.csv("lynchings.csv", strip.white=TRUE)
    #lynchings$Freq <- as.factor(lynchings$Freq)
    states_map <- map_data("state")
    lynchings$State <- tolower(state.name[match(lynchings$State, state.abb)])
    if (input$offense != "NA") {
      lynchings <- subset(lynchings, toupper(Offense) == input$offense )
    }
    
    lynchings %>% group_by(Sex) %>% summarise(Freq = n()) -> by_sex
    
    ggplot(by_sex, aes(x=Sex, y=Freq)) + geom_bar(stat="identity") +  ggtitle("Lynchings by Gender")
  })
})
