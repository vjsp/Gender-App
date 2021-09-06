################### server.R ###################

function(input, output, session) {
  
  ###============== Map Explorer ==============###
  
  ##------------- Reactive values -------------##
  
  # Reactive value for the selected country
  # It defaults to European Union 28
  sel_country_r <- reactiveVal("EU28")
  
  # Reactive value for the selected indicator
  # It defaults to Gender Equality Index
  sel_indicator_r <- reactiveVal("Gender Equality Index")

  
  ##----------- Reactive expressions -----------##
  
  # Reactive expression for the selected year GEI data
  # It returns all GEI data for the selected year
  gei_year_data_r <- reactive({
    gei_data %>% filter(Year == input$map_year)
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
    
    indicators_chart_data <- as.data.frame(gei_year_data_r()) %>%
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
      indicators_chart_data %<>% mutate(color = my_pal(Score))
    } else {
      indicators_chart_data
    }
  })


  ##---------------- Observers ----------------##
  
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


  ##------------- Event observers -------------##
  
  observeEvent(input$indicator, {
    # Set the indicator variable value
    sel_indicator_r(gei_full_indicators %>%
      filter(Id == input$indicator) %>%
      pull("Indicator (s)")
    )
    
    # Adapt the selector style taking into account the selected indicator
    set_selector_style(input$indicator, "#map_indicator_panel")
  })
  
  observeEvent(input$eu_button, {
    sel_country_r("EU28")
  })
  
  observeEvent(input$myMap_shape_click, {
    sel_country_r(input$myMap_shape_click$id)
  })


  ##----------------- Outputs -----------------##
  
  basemap <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron,
                     options = providerTileOptions(noWrap = TRUE)) %>%
    setView(30.44, 56.00, 3)
  output$myMap <- renderLeaflet({basemap})
  
  output$euHtmlValue <- renderUI({
    value <- eu_indicator_value_r()
    div(HTML(sprintf("European Union 28 <text style='color:%s'> %s </text>",
                     my_pal(value), value)))
  })
  
  output$top3Table <- renderFormattable({
    table_format <- list(format_color_formattable())
    names(table_format) <- sel_indicator_r()
    formattable(
      gei_year_indicator_data_r() %>% 
        arrange_at(sel_indicator_r(), list(desc)) %>%
        head(3),
      align = c('l','r'),
      table_format
    )
  })
  
  output$bottom3Table <- renderFormattable({
    table_format <- list(format_color_formattable())
    names(table_format) <- sel_indicator_r()
    formattable(
      gei_year_indicator_data_r() %>% 
        arrange_at(sel_indicator_r()) %>%
        head(3),
      align = c('l','r'),
      table_format
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
    shiny::validate(need(input$indicator == "GEI", message = FALSE))
    data = map_indicators_chart_data_r()
    highchart() %>%
      hc_chart(type = "area", polar = TRUE) %>%
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
    shiny::validate(need(substr(input$indicator,1,1) == "D", message = FALSE))
    data <- map_indicators_chart_data_r()
    highchart() %>%
      hc_chart(polar = TRUE) %>%
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
    shiny::validate(need(substr(input$indicator,1,1) == "S", message = FALSE))
    data <- map_indicators_chart_data_r()
    highchart() %>%
      hc_chart(marginLeft = "160") %>%
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
    shiny::validate(need(substr(input$indicator,1,1) == "I", message = FALSE))
    data <- map_indicators_chart_data_r()$Score[3]
    max_value <- get_max_value(sel_indicator_r())
    create_gauge(data, max_value, "Total", "green")
  })
  
  output$womenChart <- renderHighchart({
    shiny::validate(need(substr(input$indicator,1,1) == "I", message = FALSE))
    data <- map_indicators_chart_data_r()$Score[1]
    max_value <- get_max_value(sel_indicator_r())
    create_gauge(data, max_value, "Women", "pink")
  })
  
  output$menChart <- renderHighchart({
    shiny::validate(need(substr(input$indicator,1,1) == "I", message = FALSE))
    data <- map_indicators_chart_data_r()$Score[2]
    max_value <- get_max_value(sel_indicator_r())
    create_gauge(data, max_value, "Men", "blue")
  })
  
  
  
  ###============= Trend Explorer =============###

  ##------------- Reactive values -------------## 
  
  # Reactive value for the selected indicator
  # It defaults to Gender Equality Index
  trend_sel_indicator_r <- reactiveVal("Gender Equality Index")

  
  ##----------- Reactive expressions -----------##
  
  # Reactive expression for the selected indicator trend GEI data used in the
  # chart
  # It returns the GEI data for the indicator, years and countries specified
  # in the input selectors with the years as rows
  trend_indicator_chart_data_r <- reactive({
    gei_data %>%
      select(Year, Country, Indicator = trend_sel_indicator_r()) %>%
      filter(Year >= input$trend_years[1] & Year <= input$trend_years[2]) %>%
      filter(Country %in% input$trend_countries) %>%
      arrange_at(c("Year","Country"))
  })
  
  # Reactive expression for the selected indicator trend GEI data used in the
  # score table
  # It returns the GEI data for the indicator, years and countries specified
  # in the input selectors with the years as columns
  # It adds a column for the trend sparkline
  trend_indicator_table_data_r <- reactive({
    trend_indicator_chart_data_r() %>%
      spread(Year, Indicator) %>%
      mutate(Scores = NA)
  })
  
  # Reactive expression for the selected indicator trend GEI data used in the
  # variations table
  # It returns the GEI data for the indicator, years and countries specified
  # in the input selectors with the variation between years as columns
  # It also includes the cumulative variation between last and first years
  trend_indicator_var_table_data_r <- reactive({
    shiny::validate(need(input$trend_years[1] != input$trend_years[2],
                         message = "Variations only apply with ranges"))
    score_df <- trend_indicator_chart_data_r() %>%
      spread(Year, Indicator)
    n_cols <- length(score_df)
    cbind(score_df[1],
          score_df[-c(1,2)] - score_df[-c(1,n_cols)],
          setNames(score_df[n_cols] - score_df[2], "Cumulative")) %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      mutate(Trend = NA)
  })

  # Reactive expression for the sparkline data used in the scores table
  # It summarises the scores of the selected indicator 
  # It adds a column for the scores sparkline
  trend_sparkline_data_r <- reactive({
    trend_indicator_chart_data_r() %>%
      group_by(Country) %>%
      summarise(Scores = list(Indicator), .groups = "drop")
  })
  
  # Reactive expression for the sparkline data used in the variations table
  # It merges all years into a column for the variations sparkline
  trend_sparkline_var_data_r <- reactive({
    trend_indicator_var_table_data_r() %>%
      select(-c(Cumulative, Variations)) %>%
      gather("Year","Trend",2:ncol(.)) %>%
      group_by(Country) %>%
      summarise(Variations = list(Variations), .groups = "drop")
  })
  
    
  ##------------- Event observers -------------##
  
  observeEvent(input$trend_indicator, {
    # Set the indicator variable value
    trend_sel_indicator_r(gei_full_indicators %>%
                            filter(Id == input$trend_indicator) %>%
                            pull("Indicator (s)")
    )
    
    # Adapt the selector style taking into account the selected indicator
    set_selector_style(input$trend_indicator, "#trend_indicator_panel")
  })
  
  # Event to avoid that both years of the trend range are set to the same value
  observeEvent(input$trend_years, {
    if (input$trend_years[1] == input$trend_years[2]) {
      if (input$trend_years[1] == min(gei_years)) {
        updateSliderTextInput(session,"trend_years",
          selected = c(
            input$trend_years[1],
            gei_years[match(input$trend_years[2], gei_years) + 1]
          )
        )
      } else {
        updateSliderTextInput(session, "trend_years",
          selected = c(
            gei_years[match(input$trend_years[1], gei_years) - 1],
            input$trend_years[2]
          )
        )
      }
    }
  })

   
  ##----------------- Outputs -----------------##
  
  output$trendChart <- renderHighchart({
    shiny::validate(need(input$trend_countries != "",
                         message = "Please, select any country"))
    data <- trend_indicator_chart_data_r()
    highchart() %>%
      hc_chart(type = 'line') %>%
      hc_add_series(data, type="line", name = unique(data$Country),
                    hcaes(x = Year, y = Indicator, group = Country),
                    showInLegend = TRUE) %>%
      hc_xAxis(title = list(text = "Year"), allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = "Score")) %>%
      hc_legend(layout = "vertical", align = "right", padding = 20) %>%
      hc_exporting(enabled = TRUE,
                   buttons = list(
                     contextButton = list(
                       menuItems = list("viewFullscreen", "printChart",
                                        "separator", "downloadPNG",
                                        "downloadJPEG", "downloadPDF",
                                        "downloadSVG", "separator",
                                        "downloadCSV", "downloadXLS")))) %>%
      hc_add_theme(my_hc_theme)
  })
  
  output$trendScoreTable <- renderReactable({
    # Define data
    data <- trend_indicator_table_data_r()
    sparkline_data <- trend_sparkline_data_r()
    
    # Define column styles
    col_names <- data %>%
      select(-c(Country, Scores)) %>%
      colnames()
    country_col_def <- list(colDef(
      cell = function(value, index) {
        div(
          class = "team",
          img(class = "flag", alt = paste(value, "flag"),
              src = sprintf("images/%s.png", value)),
          div(class = "team_name", value),
        )
      },
      filterable = TRUE,
      minWidth = 110,
      style = list(position = "sticky", left = 0, background = "white",
                   zIndex = 1),
      headerStyle = list(position = "sticky", left = 0, background = "white",
                         zIndex = 1)
    )) %>%
      `names<-`("Country")
    scores_cols_defs <- list(colDef(
      cell = function(value) {
        div(class = "trend_table_score",
            style = list(backgroundColor = apply_alpha_to_color(my_pal(value),
                                                                0.7)),
            value)
      },
      align = "center",
      style = function(value) {
        list(display = "flex", flexDirection = "column", alignSelf = "center",
             alignItems = "center", color = my_pal(value))
      })) %>%
      rep(length(col_names)) %>%
      `names<-`(col_names)
    trend_col_def <- list(colDef(
      cell = function(value, index, name) {
        # By default bars are not properly centered, because they are
        # right aligned into the canvas and its width is not calculated
        # from bars width and spacing. In order to get this effect, chart
        # width is previously calculated.
        bar_width = 10
        bar_spacing = 2
        width = length(sparkline_data$Scores[[index]]) * bar_width +
          (length(sparkline_data$Scores[[index]] - 1) * bar_spacing)
        sparkline(sparkline_data$Scores[[index]],
                  type = "bar",
                  chartRangeMin = 0,
                  chartRangeMax = 100,
                  barWidth = bar_width,
                  barSpacing = bar_spacing,
                  colorMap = my_pal(sparkline_data$Scores[[index]]),
                  width = width
                  )
      },
    # trend_col_def <- list(colDef(
    #   cell = function(value, index) {
    #     last_value <- tail(sparkline_data$Scores[[index]],1)
    #     min_value <- min(sparkline_data$Scores[[index]])
    #     max_value <- max(sparkline_data$Scores[[index]])
    #     sparkline(sparkline_data$Scores[[index]],
    #               chartRangeMin = min(unlist(sparkline_data$Scores)),
    #               chartRangeMax = max(unlist(sparkline_data$Scores)),
    #               width = "100px",
    #               lineWidth = 2,
    #               lineColor = my_pal(last_value),
    #               fillColor = apply_alpha_to_color(my_pal(last_value),
    #                                              0.3),
    #               spotRadius = 3,
    #               spotColor = "undefined",
    #               minSpotColor = my_pal(min_value),
    #               maxSpotColor = my_pal(max_value),
    #               highlightSpotColor = rgb(240, 128, 0, maxColorValue = 255),
    #               highlightLineColor = rgb(240, 128, 0, maxColorValue = 255)
    #     ) 
    #   },
      align = "center",
      sortable = FALSE,
      style = list(alignSelf = 'center')
    )) %>%
      `names<-`("Scores")
    col_defs <- c(country_col_def, scores_cols_defs, trend_col_def)
    
    # Create table
    reactable(data,
              showSortable = TRUE,
              columns = col_defs)
  })
  
  output$trendVarTable <- renderReactable({
    # Define data
    data <- trend_indicator_var_table_data_r()
    #sparkline_data <- trend_sparkline_var_data_r()
    sparkline_data <- trend_sparkline_data_r()
    
    # Define column styles
    col_names <- data %>%
      select(-c(Country, Trend)) %>%
      colnames()
    country_col_def <- list(colDef(
      cell = function(value, index) {
        div(
          class = "team",
          img(class = "flag", alt = paste(value, "flag"),
              src = sprintf("images/%s.png", value)),
          div(class = "team_name", value),
        )
      },
      filterable = TRUE,
      minWidth = 110,
      style = list(position = "sticky", left = 0, background = "white",
                   zIndex = 1),
      headerStyle = list(position = "sticky", left = 0, background = "white",
                         zIndex = 1)
    )) %>%
      `names<-`("Country")
    variations_cols_defs <- list(colDef(
      cell = function(value) {
        if (value > 0) {
          paste0("+", value)
        } else {
          value
        }
      },
      align = "center",
      style = function(value, index, name) {
        list(alignSelf = "center", fontWeight = 600, color = trend_pal(value))
      }
    )) %>%
      rep(length(col_names)) %>%
      `names<-`(col_names)
    # Hay un bug: No se renderiza correctamente cuando solo hay un valor.
    # trend_col_def <- list(colDef(
    #   cell = function(value, index, name) {
    #     sparkline(c(10),
    #               type = "bar",
    #               chartRangeMin = min(unlist(sparkline_data$Variations)),
    #               chartRangeMax = max(unlist(sparkline_data$Variations)),
    #               barWidth = 5,
    #               barSpacing = 2,
    #               colorMap = trend_pal(sparkline_data$Variations[[index]]),
    #               resize = TRUE
    #     )
    #   },
    trend_col_def <- list(colDef(
      cell = function(value, index) {
        growth_value <- data$Cumulative[index]
        sparkline(sparkline_data$Scores[[index]],
                  chartRangeMin = min(unlist(sparkline_data$Scores)),
                  chartRangeMax = max(unlist(sparkline_data$Scores)),
                  width = "80px",
                  lineWidth = 2,
                  lineColor = trend_pal(growth_value),
                  fillColor = apply_alpha_to_color(trend_pal(growth_value),
                                                   0.3),
                  spotRadius = 3,
                  spotColor = "undefined",
                  minSpotColor = rgb(220, 28, 19, maxColorValue = 255),
                  maxSpotColor = rgb(46, 182, 44, maxColorValue = 255),
                  highlightSpotColor = rgb(149, 75, 146, maxColorValue = 255),
                  highlightLineColor = rgb(149, 75, 146, maxColorValue = 255)
        )
      },
      align = "center",
      sortable = FALSE,
      style = list(alignSelf = "center")
    )) %>%
      `names<-`("Trend")
    col_defs <- c(country_col_def, variations_cols_defs, trend_col_def)
    
    # Create table
    reactable(data,
              showSortable = TRUE,
              columns = col_defs)
  })



  ###============= Data Explorer =============###
  
  ##----------- Reactive expressions -----------##
  
  # Reactive expression for the data used in the GEI data table
  # It returns the GEI data for all the selected years and countries
  data_table_r <- reactive({
    gei_data %>%
      filter(Year %in% input$data_years) %>%
      filter(Country %in% input$data_countries)
  })
  
  
  ##---------------- Observers ----------------##
  
  # It disables download button when there is no data
  observe({
    shinyjs::toggleState(
      id = "downloadData",
      condition = !(input$data_tabset_panel == "Data table" &&
                      nrow(data_table_r()) == 0)
    )
  })


  ##----------------- Outputs -----------------##
  
  output$downloadData <- downloadHandler(
    filename = function() {
      extension <- ifelse (input$download_type == "CSV", "csv", "xlsx")
      
      ifelse(input$data_tabset_panel == "Data table",
                      paste0("gei_data.", extension),
                      paste0("gei_metadata.", extension))
    },
    content = function(file) {
      if (input$data_tabset_panel == "Data table") {
        data <- data_table_r()
      } else {
        data <- gei_metadata
      }
      
      if (input$Download_type == "CSV")  {
        write.csv2(data, file, row.names = FALSE)
      } else {
        write.xlsx(as.data.frame(data), file, row.names = FALSE)
      }
    }
  )
  
  # output$dataTable <- renderDataTable({
  #   datatable(gei_data,
  #             filter = 'top',
  #             extensions = c("FixedColumns","Buttons"),
  #             options = list(scrollX = T, autoWidth = TRUE,
  #                            fixedColumns = list(leftColumns = 2),
  #                            scrollY = "200px",
  #                            scrollCollapse = TRUE,
  #                            paging =  FALSE,
  #                            dom = 'Bfrtip',
  #                            buttons = c('csv','excel'),
  #                            columnDefs = list(
  #                              list(width = '200px',
  #                                   targets = "_all"
  #                              )
  #                            )),
  #             rownames = FALSE)
  # })
  output$dataTable <- renderReactable({
    col_defs <- list(
      Year = colDef(
        width = 60,
        style = list(position = "sticky", left = 0, background = "white",
                     zIndex = 1),
        headerStyle = list(position = "sticky", left = 0, background = "white",
                           zIndex = 1)
      ),
      "Country code" = colDef(show = FALSE),
      Country = colDef(
        width = 120,
        style = list(position = "sticky", left = "60px", background = "white",
                     zIndex = 1),
        headerStyle = list(position = "sticky", left = "60px",
                           background = "white", zIndex = 1)
      )
    )
    reactable(data_table_r(),
              showSortable = TRUE,
              #pagination = FALSE,
              defaultColDef = colDef(
                minWidth = 180
              ),
              columns = col_defs,
              style = list(
                fontSize = 12
              )
    )
  })
  
  # output$metadataTable <- renderDataTable({
  #   datatable(gei_metadata,
  #             options = list(scrollX = T, pageLength = 5),
  #             rownames = FALSE)
  # })
  output$metadataTable <- renderReactable({
    col_defs <- list(
      Domain = colDef(
        width = 80
      ),
      N = colDef(
        width = 50
      ),
      Subdomain = colDef(
        maxWidth = 120
      ),
      Indicator = colDef(
        maxWidth = 150
      )
    )
    reactable(gei_metadata,
              showSortable = TRUE,
              pagination = FALSE,
              columns = col_defs,
              height = "450px",
              style = list(
                fontSize = 12
              )
    )
  })
}
