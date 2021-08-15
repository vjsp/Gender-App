## server.R ##

function(input, output, session) {
  
  ## Reactive values ----------------------
  
  # Reactive value for the selected country
  # It defaults to European Union 28
  sel_country_r <- reactiveVal("EU28")
  
  # Reactive value for the selected year
  # It defaults to the current (latest) year
  sel_date_r <- reactiveVal(current_year)
  
  # Reactive value for the selected indicator
  # It defaults to Gender Equality Index
  sel_indicator_r <- reactiveVal("Gender Equality Index")

  
  ## Reactive expressions ----------------------
  
  # Reactive expression for the selected year GEI data
  # It is updated when the selected year changes
  gei_year_data_r <- reactive({
    gei_data %>% filter(Year == sel_date_r())
  })
  
  # Reactive expression for the selected year and country GEI data
  # It returns all indicators data for the selected year and country
  gei_year_country_data_r <- reactive({
    gei_year_data_r() %>% filter(`Country code` == sel_country_r())
  })
  
  # Reactive expression for the selected year and indicator GEI data
  # It returns all countries data for the selected year and indicator
  gei_year_indicator_data_r <- reactive({
    gei_year_data_r() %>% select(Country, sel_indicator_r())
  })
  
  # Reactive expression for the selected indicator value
  # It returns the selected indicator value for the selected year and country
  gei_indicator_value_r <- reactive({
    gei_year_country_data_r() %>% pull(sel_indicator_r())
  })
  
  # Reactive expression for the selected indicator value for European Union 28
  # It returns the selected indicator value for European Union 28
  eu_indicator_value_r <- reactive({
    gei_year_data_r() %>%
      filter(`Country code` == "EU28") %>%
      pull(sel_indicator_r())
  })
  
  # Reactive expression to obtain the map data
  # It only takes those countries that have data for the selected year and
  # can be represented in the map (European Union is excluded)
  map_data_r <- reactive({
    geo_data %>% subset(iso_a2 %in% gei_year_data_r()$`Country code`)
  })
  
  # Reactive expression for the selected year GEI data showed in the map
  # It preserves the order used in map data
  map_year_data_r <- reactive({
    gei_year_data_r() %>% slice(match(map_data_r()$iso_a2, .$`Country code`))
  })
  
  # Reactive expression for the selected indicator (and year) GEI data
  # showed in the map
  # It returns a vector with the indicator values
  map_indicator_data_r <- reactive({
    map_year_data_r() %>% pull(sel_indicator_r())
  })
  
  # Reactive expression for map radarchart data
  # It returns domains values for the selected country and year
  map_rc_data_r <- reactive({
    radarchart_data <- as.data.frame(gei_year_data_r()) %>%
      filter(`Country code` == sel_country_r()) %>%
      select("Country",
             gei_full_indicators %>% 
               filter(`Parent Id` == "GEI") %>% 
               select("Indicator (s)") %>%
               unlist(use.names=FALSE)
      ) %>%
      `rownames<-`(.$`Country`) %>%
      select(-`Country`) %>%
      t() %>%
      as_tibble(rownames = "Domain")
  })
  
  # Reactive expression for map indicators chart data
  # It returns indicators values for the selected country and year
  map_indicators_chart_data_r <- reactive({
    # Get the Id for the current indicator, since it is necessary to get 
    # its children
    indicator_id <- gei_full_indicators %>%
      filter(`Indicator (s)` == sel_indicator_r()) %>%
      pull(Id)
    
    domains_chart_data <- as.data.frame(gei_year_data_r()) %>%
      filter(`Country code` == sel_country_r()) %>%
      select("Country",
             gei_full_indicators %>% 
               filter(`Parent Id` == indicator_id) %>% 
               select("Indicator (s)") %>%
               unlist(use.names=FALSE)
      ) %>%
      gather("Indicator","Score",2:ncol(.))
    
    # Metrics are not limited to a score between 0 and 100, so the palette
    # can not be used
    if (substr(input$indicator,1,1) != 'I') {
      domains_chart_data %<>% mutate(color = my_pal(Score))
    } else {
      domains_chart_data
    }
  })


  ## Observers ----------------------
  
  # It updates leaflet map when selection is changed 
  observe(
    leafletProxy("myMap") %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons(data = map_data_r(),
                  stroke = FALSE,
                  highlight = highlightOptions(fillOpacity = 0.5, 
                                               bringToFront = TRUE),
                  smoothFactor = 0.1,
                  fillOpacity = 0.2,
                  fillColor = ~my_pal(map_indicator_data_r()),
                  layerId = map_data_r()@data$iso_a2,
                  label = sprintf("%s</br><text style='color:%s'> %s </text>",
                                  map_year_data_r()$Country,
                                  my_pal(map_indicator_data_r()),
                                  map_indicator_data_r()) %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    direction = "auto",
                    style = list(padding = "3px 8px",
                                 "text-align" = "center",
                                 "font-size" = "13px",
                                 "font-weight" = "normal"))
                  ) %>%
      addLegend("bottomleft",
                pal = my_pal,
                values = map_indicator_data_r())
  )

  
  ## Events observers ----------------------
  
  observeEvent(input$indicator, {
    # Set the indicator variable value
    sel_indicator_r(gei_full_indicators %>%
      filter(Id == input$indicator) %>%
      pull("Indicator (s)")
    )
    
    # Adapt the selector style taking into account the selected indicator
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
  
  observeEvent(input$plot_date, {
    sel_date_r(input$plot_date)
  })
  
  observeEvent(input$eu_button, {
    sel_country_r("EU28")
  })
  
  observeEvent(input$myMap_shape_click, {
    sel_country_r(input$myMap_shape_click$id)
  })

  
  ## Outputs ----------------------
  
  basemap <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron,
                     options = providerTileOptions(noWrap = TRUE)) %>%
    setView(30.44, 56.00, 3)
  output$myMap <- renderLeaflet({basemap})
  
  output$euHtmlValue <- renderUI({
    rgb <- col2rgb(my_pal(eu_indicator_value_r()))
    rgbStr <- sprintf('rgb(%d,%d,%d)', rgb[1], rgb[2], rgb[3])
    value <- eu_indicator_value_r()
    div(HTML(sprintf("European Union 28 <text style='color:%s'> %s </text>",
                     rgbStr, value)))
  })
  
  output$top3Table <- renderFormattable({
    my_format <- list(format_color())
    names(my_format) <- sel_indicator_r()
    formattable(
      gei_year_indicator_data_r() %>% 
        arrange_at(sel_indicator_r(), list(desc)) %>%
        head(3),
      align = c('l','r'),
      my_format
    )
  })
  
  output$bottom3Table <- renderFormattable({
    my_format <- list(format_color())
    names(my_format) <- sel_indicator_r()
    formattable(
      gei_year_indicator_data_r() %>% 
        arrange_at(sel_indicator_r()) %>%
        head(3),
      align = c('l','r'),
      my_format
    )
  })
  
  output$radarPlot <- renderChartJSRadar({
    chartJSRadar(map_rc_data_r(),
                 colMatrix = matrix(c(col2rgb(my_pal(gei_indicator_value_r())))),
                 polyAlpha = 0.4,
                 responsive = TRUE,
                 labelSize = 12,
                 showLegend = FALSE,
                 main = paste(gei_year_country_data_r()$Country,
                              gei_indicator_value_r())
    )
  })
  
  output$domainsChart <- renderHighchart({
    shiny::validate(need(input$indicator == 'GEI', message = FALSE))
    data = map_indicators_chart_data_r()
    highchart() %>%
      hc_chart(type = 'area', polar = TRUE) %>%
      hc_add_series(data$Score, name = unique(data$Country),
                    color = my_pal(gei_indicator_value_r()), fillOpacity = 0.4,
                    marker = list(radius = 3), showInLegend = FALSE) %>%
      hc_title(text = gei_year_country_data_r()$Country,
               style = list(fontSize = "14px")) %>%
      hc_subtitle(text = gei_indicator_value_r(),
                  style = list(color = my_pal(gei_indicator_value_r()), 
                               fontSize = "12px")) %>%
      hc_xAxis(categories = data$Indicator) %>%
      hc_yAxis(min = 0, max = 100, tickAmount = 5, showLastLabel = TRUE,
               labels = list(style = list(fontSize = "10px"))) %>%
      hc_add_theme(my_hc_theme)
  })
  
  output$subdomainsChart <- renderHighchart({
    shiny::validate(need(substr(input$indicator,1,1) == 'D', message = FALSE))
    data <- map_indicators_chart_data_r()
    highchart() %>%
      hc_chart(type = 'bar', polar = TRUE) %>%
      hc_add_series(data, type="bar", name = unique(data$Country),
                    hcaes(x = Indicator, y = Score, color = color),
                    showInLegend = FALSE) %>%
      hc_title(text =  gei_year_country_data_r()$Country,
               style = list(fontSize = "14px")) %>%
      hc_subtitle(text =  gei_indicator_value_r(),
                  style = list(color = my_pal(gei_indicator_value_r()), 
                               fontSize = "12px")) %>%
      hc_xAxis(categories = data$Indicator,
               labels = list(style = list(textOverflow = "none"))) %>%
      hc_yAxis(min = 0, max = 100, tickAmount = 5, showLastLabel = TRUE) %>%
      hc_pane(endAngle = 270) %>%
      hc_add_theme(my_hc_theme)
  })
  
  output$indicatorsChart <- renderHighchart({
    shiny::validate(need(substr(input$indicator,1,1) == 'S', message = FALSE))
    data <- map_indicators_chart_data_r()
    highchart() %>%
      hc_chart(type = 'bar', marginLeft = "160") %>%
      hc_add_series(data, type="bar", name = unique(data$Country),
                    hcaes(x = Indicator, y = Score, color = color),
                    showInLegend = FALSE) %>%
      hc_title(text =  gei_year_country_data_r()$Country,
               style = list(fontSize = "14px")) %>%
      hc_subtitle(text =  gei_indicator_value_r(),
                  style = list(color = my_pal(gei_indicator_value_r()), 
                               fontSize = "12px")) %>%
      hc_xAxis(categories = if(length(data$Indicator) == 1) {
                              list(data$Indicator)
                            } else {
                              data$Indicator
                            },
               labels = list(style = list(textOverflow = "none",
                                          fontSize = "10px"))) %>%
      hc_yAxis(min = 0, max = 100, tickAmount = 5, showLastLabel = TRUE) %>%
      hc_add_theme(my_hc_theme)
  })
  
  output$countryHtmlValue <- renderUI({
    rgb <- col2rgb(my_pal(gei_indicator_value_r()))
    rgbStr <- sprintf('rgb(%d,%d,%d)', rgb[1], rgb[2], rgb[3])
    value <- gei_indicator_value_r()
    HTML(sprintf(
      paste0("<text id = 'country_html_title'><tspan>%s</tspan></text>",
             "<text id = 'country_html_subtitle' style = 'color:%s'>",
             "<tspan>%s</tspan></text>"),
      gei_year_country_data_r()$Country, rgbStr, value))
  })
  
  output$totalChart <- renderHighchart({
    shiny::validate(need(substr(input$indicator,1,1) == 'I', message = FALSE))
    data <- map_indicators_chart_data_r()$Score[3]
    max_value <- get_max_value(sel_indicator_r())
    create_gauge(data, max_value, "Total", "green")
  })
  
  output$womenChart <- renderHighchart({
    shiny::validate(need(substr(input$indicator,1,1) == 'I', message = FALSE))
    data <- map_indicators_chart_data_r()$Score[1]
    max_value <- get_max_value(sel_indicator_r())
    create_gauge(data, max_value, "Women", "pink")
  })
  
  output$menChart <- renderHighchart({
    shiny::validate(need(substr(input$indicator,1,1) == 'I', message = FALSE))
    data <- map_indicators_chart_data_r()$Score[2]
    max_value <- get_max_value(sel_indicator_r())
    create_gauge(data, max_value, "Men", "blue")
  })
}