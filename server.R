## server.R ##
#source("./global.R")

function(input, output, session) {

  reactive_rc = reactive({
    req(input$mymap_shape_click)
    
    radarchart_data <- as.data.frame(reactive_db()) %>%
      filter(Country == country_selected()) %>%
      select("Country", ends_with("(Domain score)")) %>%
      rename_with(~ gsub(' (Domain score)', '', .x, fixed=TRUE))
    rownames(radarchart_data) <- radarchart_data[,1]
    radarchart_data[,1] <- NULL
    radarchart_data <- rbind(rep(100,6) , rep(0,6) , radarchart_data)
    radarchart_data
  })
  
  reactive_rc2 = reactive({
    #req(input$mymap_shape_click)
    
    radarchart_data <- as.data.frame(reactive_db()) %>%
      filter(`Country code` == country_selected()) %>%
      select("Country code",
             gei_indicators %>% 
               filter(`Parent Id` == "GEI") %>% 
               select("Indicator (s)") %>%
               unlist(use.names=FALSE)
      )
             
             #gei_indicators[gei_indicators$`Parent Id` == "GEI", ]$`Indicator (s)`) %>%
      #rename_with(~ gsub(' (Domain score)', '', .x, fixed=TRUE))
    rownames(radarchart_data) <- radarchart_data[,1]
    radarchart_data[,1] <- NULL
    radarchart_data <- t(radarchart_data)
    radarchart_data <- data.frame(Domain = row.names(radarchart_data),
                                  radarchart_data, row.names = NULL)
    #radarchart_data <- tibble::rownames_to_column(radarchart_data, "Country") 
  })
  
  country_selected <- reactive({
    input$mymap_shape_click$id
  })
  
  formatted_date = reactive({
    input$plot_date
  })
  
  reactive_db = reactive({
    gei_data %>% filter(Year == formatted_date())
  })
  
  reactive_polygons = reactive({
    geo_data[geo_data$iso_a2 %in% gei_data_year$`Country code`, ]
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(`Country code` %in% geo_data$iso_a2)
    worldcountry_subset = geo_data[geo_data$iso_a2 %in% large_countries$`Country code`, ]
    large_countries = large_countries[match(worldcountry_subset$iso_a2, large_countries$`Country code`), ]
    large_countries
  })
  
  reactive_chart_color = reactive({
    reactive_db_large() %>% filter(`Country code` == country_selected())
  })
  
  output$mymap <- renderLeaflet({basemap})
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(),
                  stroke = FALSE,
                  highlight = highlightOptions(
                    fillOpacity = 0.5,
                    bringToFront = TRUE),
                  smoothFactor = 0.1,
                  fillOpacity = 0.2,
                  fillColor = ~my_pal(reactive_db_large()$`Gender Equality Index`),
                  layerId = reactive_polygons()@data$iso_a2,
                  label = paste(
                    "Country: ", reactive_db_large()$Country,"<br/>", 
                    "Work: ", reactive_db_large()$`WORK`, "<br/>", 
                    "Money: ", reactive_db_large()$`MONEY`,
                    sep="") %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                  ))
  })
  
  observeEvent(input$mymap_shape_click,{
    click<-input$mymap_shape_click
    print(click$id)
    leafletProxy("mymap") %>%
      addPopups(click$lng, click$lat, click$id)
    
  })
  
  observeEvent(input$indicator, {
    if (input$indicator == 'GEI') {
      runjs(paste0('$(".selectize-input").css({"font-weight": "bold"})'))
    } else {
      runjs(paste0('$(".selectize-input").css({"font-weight": "bold"})'))
    }
  })
  
  output$radarPlot <- renderChartJSRadar({
    chartJSRadar(reactive_rc2(),
                 colMatrix = matrix(c(col2rgb(my_pal(reactive_chart_color()$`Gender Equality Index`)))),
                 polyAlpha = 0.4,
                 responsive = TRUE,
                 labelSize = 12,
                 showLegend = FALSE,
                 main = paste("Gender Equality Index: ", reactive_chart_color()$`Gender Equality Index`)
                 
               
    )})
}