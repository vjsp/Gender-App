## server.R ##
#source("./global.R")

function(input, output, session) {

  reactive_rc = reactive({
    req(input$mymap_shape_click)
    
    radarchart_data <- as.data.frame(reactive_db()) %>%
      filter(alpha.3 == country_selected()) %>%
      select("Country", ends_with("(Domain score)")) %>%
      rename_with(~ gsub(' (Domain score)', '', .x, fixed=TRUE))
    rownames(radarchart_data) <- radarchart_data[,1]
    radarchart_data[,1] <- NULL
    radarchart_data <- rbind(rep(100,6) , rep(0,6) , radarchart_data)
    radarchart_data
  })
  
  reactive_rc2 = reactive({
    req(input$mymap_shape_click)
    
    radarchart_data <- as.data.frame(reactive_db()) %>%
      filter(alpha.3 == country_selected()) %>%
      select("Country", ends_with("(Domain score)")) %>%
      rename_with(~ gsub(' (Domain score)', '', .x, fixed=TRUE))
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
    my_data %>% filter(Year == formatted_date())
  })
  
  reactive_polygons = reactive({
    world_countries[world_countries$adm0_a3 %in% cv_large_countries$alpha.3, ]
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha.3 %in% world_countries$adm0_a3)
    worldcountry_subset = world_countries[world_countries$adm0_a3 %in% large_countries$alpha.3, ]
    large_countries = large_countries[match(worldcountry_subset$adm0_a3, large_countries$alpha.3),]
    large_countries
  })
  
  reactive_chart_color = reactive({
    reactive_db_large() %>% filter(alpha.3 == country_selected())
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
                  fillColor = ~my_pal(reactive_db_large()$`Overall Gender Equality Index`),
                  layerId = reactive_polygons()@data$adm0_a3,
                  label = paste(
                    "Country: ", reactive_db_large()$Country,"<br/>", 
                    "Work: ", reactive_db_large()$`Work (Domain score)`, "<br/>", 
                    "Money: ", reactive_db_large()$`Money (Domain score)`,
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
    print(click$lng)
    print(click$lat)
    leafletProxy("mymap") %>%
      addPopups(click$lng, click$lat, click$id)
    
  })
  
  output$radarPlot <- renderPlot({
    radarchart(reactive_rc(),
               # Customize the polygon
               pcol = my_pal(reactive_chart_color()$`Overall Gender Equality Index`),
               pfcol = scales::alpha(my_pal(reactive_chart_color()$`Overall Gender Equality Index`),
                                     0.5), plwd = 2, plty = 1,
               # Customize the grid
               cglcol = "grey", cglty = 1, cglwd = 0.8,
               # Customize the axis
               axislabcol = "grey"
    )})
  
  output$radarPlot2 <- renderChartJSRadar({
    chartJSRadar(reactive_rc2(),
                 colMatrix = matrix(c(col2rgb(my_pal(reactive_chart_color()$`Overall Gender Equality Index`)))),
                 polyAlpha = 0.4,
                 responsive = TRUE,
                 labelSize = 12,
                 showLegend = FALSE,
                 main = paste("Gender Equality Index: ", reactive_chart_color()$`Overall Gender Equality Index`)
                 
               
    )})
}