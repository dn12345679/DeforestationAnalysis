
library(shinydashboard)
library(tidyverse)
library(leaflet)

# additional packages
library(sf)
library(rstudioapi)
library(rmapshaper)
library(plotly)

living_index <- read.csv("data/global-living-planet-index/global-living-planet-index.csv")
location_data <- read.csv("data/world_country_and_usa_states_latitude_and_longitude_values.csv")
change_forest_area <- read.csv("data/annual-change-forest-area.csv")
forest_shares <- read.csv("data/archive/goal15.forest_shares.csv")

header <- dashboardHeader(title = "Exploring Deforestation related Habitat Loss")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Forest Shares",
             tabName = "fs",
             icon = icon("tree")),
    selectInput("iso", "Select a country",
                choices = unique(forest_shares$iso3c))
  ),
  sidebarMenu(
    menuItem("Living Planet Index",
             tabName = "lpi", 
             icon = icon("earth-africa")),
    selectInput("Entity", "Select a Region", 
                choices = unique(living_index$Entity))

  ),
  sidebarMenu(
    
    menuItem("Habitat Loss (2000) Map",
             tabName = "hab",
             icon = icon("map")),
    
    selectInput("country", "Select a country", 
                choices = unique(location_data$country))
  ),
  sidebarMenu(
    menuItem("Forest Change by Decade",
             tabName = "chn",
             icon = icon("chart-bar")),
    selectInput("Year", "Select a decade",
                choices = unique(change_forest_area$Year))
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(HTML(
      ".box {margin-bottom: 10px;}
      #hab_loss { height: calc(100vh - 150px) !important; }
      #lpi_yr { height: calc(100vh - 150px) !important; }
      #for_ch { height: calc(100vh - 100px) !important; }
      #for_area {height: calc(100vh - 100px) !important;}"
    ))
  ),
  tabItems(
    tabItem(tabName = "fs",
            fluidRow(
              infoBoxOutput(width = 3, "curr_country"),
              infoBoxOutput(width = 3, "curr_country_value2000"),
              infoBoxOutput(width = 3, "curr_country_value2020")
            ),
            fluidRow(
              box(width = 12, plotlyOutput("for_ch"))
            )),
    tabItem(tabName = "lpi",
            fluidRow(
              infoBoxOutput(width = 3, "region"),
              infoBoxOutput(width = 3, "net_change")),
            fluidRow(
              box(width = 12, plotlyOutput("lpi_yr")))
    ),
    tabItem(tabName = "hab",
            box(width = 12, leafletOutput("hab_loss"))
    ), 
    tabItem(tabName = "chn",
            box(width = 12, plotlyOutput("for_area")))
  )
)

dashboardPage(header, sidebar, body)
