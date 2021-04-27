#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Author: Joshua Gonzales 
# Project 3 - We've Got the Power

# Here we are going to look at data on electricity and gas usage by census block 
# in the city of Chicago to see how neighborhoods, types of buildings, 
# age of buildings can affect how much power is used throughout the year. 
# This project can also serve as a model for looking at other kinds of 
# census data related to the Chicago (or other areas of the US).

library(shiny)
library(shinydashboard)
library(tidyverse)
library(sp)
library(sf)
library(leaflet)
library(ggplot2)
library(geojsonio)
library(mapview)
library(tigris)
library(reshape)

df <- read.csv("energy-usage-2010.csv")
df <- subset(df, !is.na(df$BLOCK))

# Get Cook county dataset
cook_county <- blocks(state="IL", county="Cook", year=2010)
df_block <- df$BLOCK

# Subset of Cook county blocks, filters only blocks in dataset
chicago_blocks <- subset(cook_county, GEOID10 %in% df_block)
colnames(chicago_blocks)[which(names(chicago_blocks) == "GEOID10")] <- "BLOCKS"

blocks_simple <- subset(chicago_blocks, select = c("TRACTCE10", "BLOCKCE10", "BLOCKS", "INTPTLAT10", "INTPTLON10"))

initialView <- subset(df, COMMUNITY == "Near West Side")

#focusView <- subset(chicago_blocks, BLOCKS %in% initialView$BLOCK)
focusView <- subset(blocks_simple, BLOCKS %in% initialView$BLOCK)
coord <- subset(focusView, select = c("INTPTLAT10", "INTPTLON10"))

# Initialize coordinates for startup mapview
coord$INTPTLAT10 <- as.numeric(gsub("+", "", coord$INTPTLAT10))
coord$INTPTLON10 <- as.numeric(gsub("-", "", coord$INTPTLON10))
cntr_crds <- c(mean(coord$INTPTLAT10),
               -mean(coord$INTPTLON10))

test <- merge(focusView, initialView, by.x="BLOCKS", by.y="BLOCK")
test <- subset(test, select = c("COMMUNITY", "TRACTCE10", "BLOCKCE10", "BLOCKS", 
                                "INTPTLAT10", "INTPTLON10", "TOTAL_KWH", "TOTAL_THERM", 
                                "AVERAGE.HOUSESIZE", "BUILD_TYPE", "TOTAL.POPULATION", 
                                "AVERAGE.BUILDING.AGE", "KWH_JAN", "KWH_FEB", "KWH_MAR", 
                                "KWH_APR", "KWH_MAY", "KWH_JUN", "KWH_JUL", "KWH_AUG", 
                                "KWH_SEP", "KWH_OCT", "KWH_NOV", "KWH_DEC", 
                                "THERM_JAN", "THERM_FEB", "THERM_MAR", 
                                "THERM_APR", "THERM_MAY", "THERM_JUN", "THERM_JUL", 
                                "THERM_AUG", "THERM_SEP", "THERM_OCT", "THERM_NOV", "THERM_DEC"))

#=====================================================================================================================================
# Get total kwh by month and by census block

totalByMonth <- df %>%
  select(
    COMMUNITY,
    BLOCK,
    KWH_JAN,
    KWH_FEB,
    KWH_MAR,
    KWH_APR,
    KWH_MAY,
    KWH_JUN,
    KWH_JUL,
    KWH_AUG,
    KWH_SEP,
    KWH_OCT,
    KWH_NOV,
    KWH_DEC,
  )

months <- c("COMMUNITY", "BLOCKS", "JAN", "FEB", "MAR", "APR", "MAY", 
            "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
colnames(totalByMonth) <- months

totalByMonth <- subset(totalByMonth, !is.na(totalByMonth$JAN))
totalByMonth$COMMUNITY <- as.factor(totalByMonth$COMMUNITY)

reshapedTotal <- reshape(totalByMonth,
                         varying = c("JAN", "FEB", "MAR", "APR", 
                                     "MAY", "JUN", "JUL", "AUG", 
                                     "SEP", "OCT", "NOV", "DEC"
                                     ),
                         v.names = "KWH",
                         timevar = "MONTH",
                         times = c("JAN", "FEB", "MAR", "APR", 
                                   "MAY", "JUN", "JUL", "AUG", 
                                   "SEP", "OCT", "NOV", "DEC"),
                         new.row.names = sequence(prod(12, nrow(totalByMonth))),
                         direction = "long"
                         )
#reshapedTotal.sort <- reshapedTotal[order(reshapedTotal$COMMUNITY),]

totalByMonth2 <- df %>%
  select(
    COMMUNITY,
    BLOCK,
    THERM_JAN,
    THERM_FEB,
    THERM_MAR,
    THERM_APR,
    THERM_MAY,
    THERM_JUN,
    THERM_JUL,
    THERM_AUG,
    THERM_SEP,
    THERM_OCT,
    THERM_NOV,
    THERM_DEC,
  )
colnames(totalByMonth2) <- months

totalByMonth2 <- subset(totalByMonth2, !is.na(totalByMonth2$JAN))
totalByMonth2$COMMUNITY <- as.factor(totalByMonth2$COMMUNITY)

reshapedTotal2 <- reshape(totalByMonth2,
                         varying = c("JAN", "FEB", "MAR", "APR", 
                                     "MAY", "JUN", "JUL", "AUG", 
                                     "SEP", "OCT", "NOV", "DEC"
                         ),
                         v.names = "KWH",
                         timevar = "MONTH",
                         times = c("JAN", "FEB", "MAR", "APR", 
                                   "MAY", "JUN", "JUL", "AUG", 
                                   "SEP", "OCT", "NOV", "DEC"),
                         new.row.names = sequence(prod(12, nrow(totalByMonth))),
                         direction = "long"
)

#=====================================================================================================================================
datas <- c("Electricity", "Gas", "Building Age", "Building Type", "Building Height", "Total Population")
time <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
build_types <- c("Residential", "Commercial", "Industrial", "All")
community <- unique(df$COMMUNITY)

# Define UI for application that draws a histogram
ui <- navbarPage("CS424 Spring 2021 Project 3",
                 tabPanel("Heatmap for Chicago's blocks",
                       fluidRow(
                         column(6,
                                fluidRow( 
                                  box(title = "Heatmap", width=20, 
                                      leafletOutput("test"),
                                      actionButton("reset", "Reset Map")
                                  )
                                ),
                                fluidRow(
                                  box(status="primary", width=20, solidHeader=TRUE,
                                      selectInput("build_types", "Select building type(s):", build_types, selected = "All"),
                                      selectInput("selectData", "Select Data by:", datas, selected = "Electricity")
                                  )
                                ),
                                fluidRow( 
                                  box(title = "", status="primary", width=20, 
                                      selectInput("month", "Select time frame to display:", time,
                                                         selected = "Year",)
                                  )
                                )
                         ),
                         column(3,
                                fluidRow(
                                  box(title="Total Amount of Electricity Used in Census Blocks", solidHeader=TRUE, status="primary", width=20,    
                                      plotOutput("line1", height=270))
                                ),
                                fluidRow(
                                  box(title="Total Amount of Gas Used in Census Blocks", solidHeader=TRUE, status="primary", width=20,    
                                      plotOutput("line2", height=270))
                                ),
                         ),
                         column(3,
                                fluidRow(
                                  box(title="Table of Amount of Electricity Used in Census Blocks", solidHeader=TRUE, status="primary", width=20,
                                      DT::dataTableOutput("tab1"))
                                ),
                                fluidRow(
                                  box(title="Table of Amount of Gas Used in Census Blocks", solidHeader=TRUE, status="primary", width=20,
                                      DT::dataTableOutput("tab2"))
                                ),
                         ),
                        
                       )
                 ),
                 
                 #tabPanel("Compare two Community Areas")
                 tabPanel("About",
                          h1("Project 3: We've Got the Power"),
                          h3("Developed By: Joshua Gonzales"),
                          h4("Project 3 in CS 424 (Data Analytics / Visualization) at the University of Illinois at Chicago Spring 2021"),
                          
                          h5("________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________"),
                          
                          h3(""),
                          h4("This project contains data on the energy usage combined with some census data from 2010 in Chicago"),
                          h3(""),
                          h4("The data focuses on the census blocks of Chicago: "),
                          h4("Gas, Electricity, Building data, Population in that block"),
                          strong("The application helps visualize energy usage data by using leaflets and mapview, with part 1 being just the Near West Side Community Area, part 2 being comparisons between two areas, and part 3 involving displaying the whole of Chicago"),
                          h3(""),
                          
                          h5("* Libraries Used: shiny, tidyverse, sf, leaflet, ggplot2, geojsonio, mapview, reshape"),
                          
                          h5("* City of Chicago Energy Usage 2010: https://www.kaggle.com/chicago/chicago-energy-usage-2010"),
                          h5("Files used: energy-usage-2010.csv"),
                          
                          h5("* Created using R, RStudio, Shiny ")
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive df based on particular month/total for entire year to see electricity or gas usage
  #byMonth <- reactive({
  #  monthdf <- NULL
  #  
  #  # Get month if electricity
  #  if (input$selectData == "Electricity") {
  #    monthdf <- subset(reshapedTotal, MONTH %in% input$month)
  #  }
  #   
  #  
  #  # Get month if gas
  #  if (input$selectData == "Gas") {
  #    monthdf <- subset(reshapedTotal2, MONTH %in% input$month) 
  #  }
  #  
  #  return (monthdf)
  #})
  
  # Reactive df based on total electricity/gas used in current census blocks 
  
  # centered and scaled mapview of the Near West Side Community Area
  # Get total electric usage over the entire year 
  observe({
    output$test <- renderLeaflet({
      #mapview(chicago_blocks)@map
      
      #coord$INTPTLAT10 <- as.numeric(gsub("+", "", coord$INTPTLAT10))
      #coord$INTPTLON10 <- as.numeric(gsub("-", "", coord$INTPTLON10))
      #cntr_crds <- c(mean(coord$INTPTLAT10),
      #               -mean(coord$INTPTLON10))
      #mapview(focusView)@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13)
      
      if (input$build_types == "All") {
        newdf <- test
      } else {
        newdf <- subset(test, test$BUILD_TYPE == input$build_types)
      }
      
      if (input$selectData == "Electricity") {
        newdf <- subset(newdf, !is.na(newdf$TOTAL_KWH))
        switch(
          input$month,
          
          "Year" = mapview(newdf, zcol="TOTAL_KWH")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Jan" = mapview(newdf, zcol="KWH_JAN")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Feb" = mapview(newdf, zcol="KWH_FEB")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Mar" = mapview(newdf, zcol="KWH_MAR")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Apr" = mapview(newdf, zcol="KWH_APR")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "May" = mapview(newdf, zcol="KWH_MAY")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Jun" = mapview(newdf, zcol="KWH_JUN")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Jul" = mapview(newdf, zcol="KWH_JUL")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Aug" = mapview(newdf, zcol="KWH_AUG")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Sep" = mapview(newdf, zcol="KWH_SEP")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Oct" = mapview(newdf, zcol="KWH_OCT")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Nov" = mapview(newdf, zcol="KWH_NOV")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Dec" = mapview(newdf, zcol="KWH_DEC")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13)
        )
      } else if (input$selectData == "Gas") {
        newdf <- subset(newdf, !is.na(newdf$TOTAL_THERM))
        switch(
          input$month,
          
          "Year" = mapview(newdf, zcol="TOTAL_THERM")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Jan" = mapview(newdf, zcol="THERM_JAN")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Feb" = mapview(newdf, zcol="THERM_FEB")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Mar" = mapview(newdf, zcol="THERM_MAR")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Apr" = mapview(newdf, zcol="THERM_APR")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "May" = mapview(newdf, zcol="THERM_MAY")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Jun" = mapview(newdf, zcol="THERM_JUN")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Jul" = mapview(newdf, zcol="THERM_JUL")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Aug" = mapview(newdf, zcol="THERM_AUG")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Sep" = mapview(newdf, zcol="THERM_SEP")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Oct" = mapview(newdf, zcol="THERM_OCT")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Nov" = mapview(newdf, zcol="THERM_NOV")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13), 
          "Dec" = mapview(newdf, zcol="THERM_DEC")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13)
        )  
      } else if (input$selectData == "Building Age") {
        newdf <- subset(newdf, !is.na(newdf$AVERAGE.BUILDING.AGE))
        mapview(newdf, zcol="AVERAGE.BUILDING.AGE")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13)
      } else if (input$selectData == "Building Height") {
        newdf <- subset(newdf, !is.na(newdf$AVERAGE.HOUSESIZE))
        mapview(newdf, zcol="AVERAGE.HOUSESIZE")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13)
      } else if (input$selectData == "Building Type") {
        newdf <- subset(newdf, !is.na(newdf$BUILD_TYPE))
        mapview(newdf, zcol="BUILD_TYPE")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13)
      } else { # Total Population
        newdf <- subset(newdf, !is.na(newdf$TOTAL.POPULATION))
        mapview(newdf, zcol="TOTAL.POPULATION")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13)
      }
      
     # mapview(test, zcol="TOTAL_KWH")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13)
    })
  })
  
  
  
  
  # Reset map with reset button
  eventReactive(input$reset, {
      #proxy <- leafletProxy("test", data=chicago_blocks)
      coord$INTPTLAT10 <- as.numeric(gsub("+", "", coord$INTPTLAT10))
      coord$INTPTLON10 <- as.numeric(gsub("-", "", coord$INTPTLON10))
      cntr_crds <- c(mean(coord$INTPTLAT10),
                     -mean(coord$INTPTLON10))
      #mapview(focusView)@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13)
      mapview(test, zcol="TOTAL_KWH")@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13)
      #proxy %>% clearControls() 
      #%>% mapview(chicago_blocks)@map
  })
  
  # Total Electricity Line Graph
  output$line1 <- renderPlot({
    ggplot(reshapedTotal, aes(x=MONTH, y=KWH, fill=BLOCKS)) + 
      geom_bar(position="stack", stat="identity") + labs(x="Month", y="Energy (KWH)")   
  })
  
  # Total Gas Line Graph
  output$line2 <- renderPlot({
    ggplot(reshapedTotal2, aes(x=MONTH, y=KWH, fill=BLOCKS)) + 
      geom_bar(position="stack", stat="identity") + labs(x="Month", y="Energy (KWH)")   
  })
  
  output$tab1 <- DT::renderDataTable({
    DT::datatable(data=totalByMonth, options = list(pageLength=5))
  })
  
  output$tab2 <- DT::renderDataTable({
    DT::datatable(data=totalByMonth2, options = list(pageLength=5))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
