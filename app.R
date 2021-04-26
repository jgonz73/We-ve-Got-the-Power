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

#test <- merge(focusView, initialView, by="BLOCK")
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
datas <- c("Gas", "Electricity", "Building Age", "Building Type", "Building Height", "Total Population")
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
                                      selectInput("selectData", "Select Data by:", datas, selected = "Gas")
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
                 )
                 #tabPanel("Compare two Community Areas")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Reactive df based on particular month/total for entire year to see electricy or gas usage
  
  # Reactive df based on total electricity/gas used in current census blocks 
  
  # centered and scaled mapview of the Near West Side Community Area
  output$test <- renderLeaflet({
    #mapview(chicago_blocks)@map
    
    coord$INTPTLAT10 <- as.numeric(gsub("+", "", coord$INTPTLAT10))
    coord$INTPTLON10 <- as.numeric(gsub("-", "", coord$INTPTLON10))
    cntr_crds <- c(mean(coord$INTPTLAT10),
                   -mean(coord$INTPTLON10))
    mapview(focusView)@map %>% setView(cntr_crds[2], cntr_crds[1], zoom=13)
  })
  
  # Get total electric usage over the entire year 
  observeEvent(input$build_types, {
    
  })
  
  
  # Reset map with reset button
  eventReactive(input$reset, {
      proxy <- leafletProxy("test", data=chicago_blocks)
      
      proxy %>% clearControls() 
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
    DT::datatable(data=reshapedTotal, options = list(pageLength=5))
  })
  
  output$tab2 <- DT::renderDataTable({
    DT::datatable(data=reshapedTotal2, options = list(pageLength=5))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
