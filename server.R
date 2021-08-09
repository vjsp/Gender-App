## server.R ##
#source("./global.R")

function(input, output, session) {
  
  ## Reactive expressions
  
  # Reactive expression for map radar chart
  # It returns domains values for the selected country and year
  map_rc_reactive = reactive({
    #req(input$mymap_shape_click)
    print(country_selected())
    radarchart_data <- as.data.frame(reactive_db()) %>%
      filter(`Country code` == country_selected()) %>%
      select("Country code",
             gei_indicators %>% 
               filter(`Parent Id` == "GEI") %>% 
               select("Indicator (s)") %>%
               unlist(use.names=FALSE)
      )
             
    rownames(radarchart_data) <- radarchart_data[,1]
    radarchart_data[,1] <- NULL
    radarchart_data <- t(radarchart_data)
    radarchart_data <- data.frame(Domain = row.names(radarchart_data),
                                  radarchart_data, row.names = NULL)
    }
    #radarchart_data <- tibble::rownames_to_column(radarchart_data, "Country") 
  )
  
  country_selected <- reactiveVal("EU28")
  
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
    reactive_db() %>% filter(`Country code` == country_selected())
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
  
  observeEvent(input$eu_button,
               country_selected("EU28")
  )
  
  observeEvent(input$mymap_shape_click,{
    click<-input$mymap_shape_click
    country_selected(click$id)
    print(country_selected)
    leafletProxy("mymap") %>%
      addPopups(click$lng, click$lat, click$id)
    
  })
  
  observeEvent(input$indicator, {
    if (input$indicator == 'GEI') {
      runjs(paste0('$("#map_indicator_panel .selectize-input").
        addClass("GEI_option")'))
      runjs(paste0('$("#map_indicator_panel .selectize-input").
        removeClass("Domain_option Subdomain_option Indicator_option")'))
    } else if (substr(input$indicator,1,1) == 'D'){
      runjs(paste0('$("#map_indicator_panel .selectize-input").
        addClass("Domain_option")'))
      runjs(paste0('$("#map_indicator_panel .selectize-input").
        removeClass("GEI_option Subdomain_option Indicator_option")'))
    } else if (substr(input$indicator,1,1) == 'S'){
      runjs(paste0('$("#map_indicator_panel .selectize-input").
        addClass("Subdomain_option")'))
      runjs(paste0('$("#map_indicator_panel .selectize-input").
        removeClass("GEI_option Domain_option Indicator_option")'))
    } else if (substr(input$indicator,1,1) == 'I'){
      runjs(paste0('$("#map_indicator_panel .selectize-input").
        addClass("Indicator_option")'))
      runjs(paste0('$("#map_indicator_panel .selectize-input").
        removeClass("GEI_option Domain_option Subdomain_option")'))
    } else {
      runjs(paste0('$("#map_indicator_panel .selectize-input").
        removeClass(
          "GEI_option Domain_option Subdomain_option Indicator_option"
        )'))
    }
  })
  
  output$radarPlot <- renderChartJSRadar({
    chartJSRadar(map_rc_reactive(),
                 colMatrix = matrix(c(col2rgb(my_pal(reactive_chart_color()$`Gender Equality Index`)))),
                 polyAlpha = 0.4,
                 responsive = TRUE,
                 labelSize = 12,
                 showLegend = FALSE,
                 main = paste(reactive_chart_color()$Country, reactive_chart_color()$`Gender Equality Index`)
    )
  })
}