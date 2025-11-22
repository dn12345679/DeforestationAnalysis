
library(tidyverse)
library(leaflet)

# additional packages
library(sf)
library(rstudioapi)
library(rmapshaper)
library(plotly)



function(input, output, session) {
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  change_forest_area <- read.csv("data/annual-change-forest-area.csv")
  forest_shares <- read.csv("data/archive/goal15.forest_shares.csv")
  living_index <- read.csv("data/global-living-planet-index/global-living-planet-index.csv")
  #food_deforestation <- read.csv("data/per-capita-co2-food-deforestation/per-capita-co2-food-deforestation.csv")
  habitat_loss <- st_read("data/Percent Habitat Loss by Terrestrial Ecoregion 2000/data/commondata/data0/wwf_ecos_hab_loss.shp")
  location_data <- read.csv("data/world_country_and_usa_states_latitude_and_longitude_values.csv")
  
  habitat_loss_simple <- rmapshaper::ms_simplify(habitat_loss, keep = 0.05)

  options(scipen = 999)
  
  thePoint = reactive({
    req(input$Entity)
    living_index[which(living_index$Entity == input$Entity),]
  })
  
  mapPoint = reactive({
    req(input$country)
    location_data[which(location_data$country == input$country), ]
  })
  
  changeYear = reactive({
    req(input$Year)
    change_forest_area %>%
      filter(Year == input$Year)
  })
  
  isoSelect = reactive({
    req(input$iso)
    forest_shares %>%
      filter(`ico3c` == input$iso)
  })

  output$for_ch = renderPlotly(
    {
      forest_long <- forest_shares %>%
        pivot_longer(cols = c("forests_2000", "forests_2020"),
                     names_to = "time_period",
                     values_to = "value") %>%
        mutate(highlight = (`iso3c` == input$iso))
      
      p <- ggplot(forest_long, aes(x = time_period, y = value, group = iso3c,
                                   text = paste0(
                                     "<b>ISO: </b>", iso3c, "<br>",
                                     "<b>Slope: </b>", trend
                                   ))) + 
        geom_line(aes(color = highlight), linewidth = 0.3) + 
        geom_point(aes(color = highlight), size = 0.8) + 
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray")) + 
        geom_text(data = subset(forest_long, time_period == "2000"),
                  aes(label = iso3c), hjust = 1.2, size = 4) +
        geom_text(data = subset(forest_long, time_period == "2020"),
                  aes(label = iso3c), hjust = -0.2, size = 4) +
        scale_x_discrete(labels = c("2000", "2020")) +
        labs(title = "Slope Plot: Forest Area Change between 2000 and 2020",
             x = "Year",
             y = "Forest Share (%)", 
             subtitle = "Hover an endpoint for more information") +
        theme(legend.position = "none")
      
      plotly_obj <- ggplotly(p, tooltip = "text")

      plotly_obj
    }
  )

  
  output$lpi_yr = renderPlotly({ 
    dat = living_index[which(living_index$Entity == input$Entity),]
    
    p <- ggplot(living_index, aes(x = Year, y = lpi_final, group=Entity,
                                  text = paste0(
                                    "<b>Entity: </b>", Entity, "<br>",
                                    "<b>Year: </b>", Year, "<br>",
                                    "<b>LPI: </b>", lpi_final
                                  ))) + 
      geom_point(size = 1) + 
      geom_line() + 
      geom_point(data = thePoint(), color = "red") +
      geom_line(data = thePoint(), color = "red") + 
      ggtitle("Living Planet Index by Year") + 
      xlab("Year (in Years)") +
      ylab("Living Planet Index") +
      labs(subtitle="Hover over a point to view additional information.")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  output$for_area = renderPlotly({
      p <- ggplot(changeYear() %>% filter( Annual.net.change.in.forest.area > 10000 |  
                                        Annual.net.change.in.forest.area < -10000, 
                                      Entity != "World"
                                      ), 
                              aes(x = Entity, y = Annual.net.change.in.forest.area, 
                             fill =  Annual.net.change.in.forest.area > 0,
                             text = paste0(
                               "<b>Entity: </b>", Entity, "<br>",
                               "<b>Change: </b>", Annual.net.change.in.forest.area
                             ))) +
      geom_col() + 
      scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "blue"),
                        guide = "none") +
      theme(axis.text.x = element_text(face = "bold", color = "993333", 
                                       size = 12, angle = 45), legend.position = "none") +
      labs(subtitle = "Hover over a bar for info.", title = paste("Annual Change in Forest Area,", input$Year, "(+/- 10,000"),
           x = "Country", y = "Forest Area Change in hectares (ha)")
      
      ggplotly(p, tooltip = "text")
  })
  
  
  
  output$hab_loss = renderLeaflet({
    valid_data <- habitat_loss_simple %>%
      filter(!is.na(habloss_p) & habloss_p >= 0 & habloss_p <= 1000)
    
    
    pal <- colorNumeric(
      palette = c("green", "white", "red"),
      domain = valid_data$habloss_p
    )
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addPolygons(data = valid_data, 
                  fillColor = ~pal(habloss_p),
                  color = "blue", 
                  stroke = TRUE,
                  weight = 0.05, 
                  opacity = 0.6,
                  popup = ~paste0(
                    "<b>Region:</b> ", ECO_NAME, "<br>",
                    "<b>Habitat Loss (%):</b> ", round(habloss_p, 3)
                  )) %>%
      addLegend(
        "bottomright", 
        pal = pal, values = valid_data$habloss_p,
        title = "Est. Habitat Loss % (2000)",
        labFormat = labelFormat(suffix = "%"),
        opacity = 0.9
      )
  })
  
  observe({
    req(input$country)
    points<- mapPoint()
    
    if(nrow(points) > 0){
      center_lng <- mean(points$longitude, na.rm = TRUE)
      center_lat <- mean(points$latitude, na.rm = TRUE)
      
      leafletProxy("hab_loss", session) %>%
        clearMarkers() %>%
        addMarkers(
          lng = points$longitude,
          lat = points$latitude,
          popup = paste0(
            "Location: ", points$country
          )) %>%
        setView(lng = center_lng, lat = center_lat, zoom = 6)
    }
  })
  

  # Infos
  
  output$net_change = renderInfoBox({
    req(input$Entity)
    dat = living_index[which(living_index$Entity == input$Entity),]
    val_end = dat[nrow(dat), ]$lpi_final 
    val_start = dat[1, ]$lpi_final
    infoBox(
      "Net Change", val_end - val_start,
      color = "blue")
  })
  output$region = renderInfoBox({
    req(input$Entity)
    infoBox(
      "Region", input$Entity,
      color = "blue"
    )
  })
  
  output$curr_country = renderInfoBox({
    req(input$iso)
    infoBox(
      "Country", input$iso,
      color = "blue"
    )
  })
  output$curr_country_value2000 = renderInfoBox({
    req(input$iso)
    infoBox(
      "2000 Value (% cover)", forest_long$value[forest_long$iso3c == input$iso & forest_long$time_period == "forests_2000"],
      color = "blue"
    )
  })
  output$curr_country_value2020 = renderInfoBox({
    req(input$iso)
    infoBox(
      "2020 Value (% cover)", forest_long$value[forest_long$iso3c == input$iso & forest_long$time_period == "forests_2020"],
      color = "blue"
    )
  })


  
}
