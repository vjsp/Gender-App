################### server.R ###################
# @author Víctor Julio Sánchez Pollo           #
# @version 18/09/2021                          #
################################################

function(input, output, session) {

  ###================= Header =================###
  
  ##----------------- Outputs -----------------##
  
  output$info_menu <- renderMenu({
    # Code to use Github icon available in v.4.7
    icon_github <- icon("github")
    icon_github[["attribs"]][["class"]] <- "fa fa-github"
    
    # Code to open links in new tabs
    github_link <- "javascript:void(
      window.open('https://github.com/vjsp/Gender-App', '_blank')
    )"
    linkedin_link <- "javascript:void(
      window.open('https://www.linkedin.com/in/vj-sanchez-pollo/', '_blank')
    )"
    
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Author",
                   message = "Víctor Julio Sánchez Pollo",
                   href = linkedin_link),
                 messageItem(
                   from = "Contact",
                   message = "vjsanchez22411@alumnos.uemc.es",
                   icon("envelope"),
                   href = "mailto:vjsanchez22411@alumnos.uemc.es"),
                 messageItem(
                   from = "Code",
                   message = "https://github.com/vjsp/Gender-App",
                   icon = icon_github,
                   href = github_link),
                 badgeStatus = NULL,
                 icon = icon("info-circle fa-lg"),
                 headerText = "App Information"
    )
  })
  
  output$credits_menu <- renderMenu({
    # Code to open links in new tabs
    eige_link <- "javascript:void(
      window.open('https://eige.europa.eu', '_blank')
    )"
    freepik_link <- "javascript:void(
      window.open('https://www.freepik.com', '_blank')
    )"
    
    dropdownMenu(type = "notifications",
                 notificationItem(
                   text = div(
                     HTML("<img src='images/icons/eige.svg'
                          alt='EIGE Icon'>"),
                     div(class = "credit_text",
                         "All data and descriptions are property of the",
                         br(),
                         "European Institute for Gender Equality (EIGE)")
                   ),
                   icon = icon(NULL),
                   href = eige_link
                 ),
                 notificationItem(
                   text = div(
                     HTML("<img src='images/icons/freepik.svg'
                          alt='Freepik Icon'>"),
                     div(class = "credit_text",
                         "Country flag icons designed by Freepik")
                   ),
                   icon = icon(NULL),
                   href = freepik_link
                 ),
                 badgeStatus = NULL,
                 icon = icon("copyright fa-lg"),
                 headerText = "Credits"
    )
  })

  

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
      select(Country,
             gei_full_indicators %>% 
               filter(`Parent Id` == indicator_id) %>% 
               select("Indicator (s)") %>%
               unlist(use.names = FALSE)
      ) %>%
      gather("Indicator", "Score", 2:ncol(.))
    
    # Metrics are not limited to a score between 0 and 100, so the palette
    # can not be used
    if (substr(input$indicator,1,1) != 'I') {
      indicators_chart_data %<>% mutate(color = my_pal(Score))
    } else {
      indicators_chart_data
    }
  })


  ##---------------- Observers ----------------##
  
  # It updates the leaflet map when selection is changed 
  observe(
    leafletProxy("my_map") %>%
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
                                 "font-size" = "13px",
                                 "font-weight" = "normal",
                                 "text-align" = "center"))
                  ) %>%
      addLegend("bottomleft",
                pal = my_pal,
                values = map_indicator_data_r())
  )


  ##------------- Event observers -------------##
  
  observeEvent(input$indicator, {
    # Sets the indicator variable value
    sel_indicator_r(gei_full_indicators %>%
      filter(Id == input$indicator) %>%
      pull("Indicator (s)")
    )
    
    # Adapts the selector style taking into account the selected indicator
    set_selector_style(input$indicator, "#map_indicator_panel")
  })
  
  observeEvent(input$eu_button, {
    sel_country_r("EU28")
  })
  
  observeEvent(input$my_map_shape_click, {
    sel_country_r(input$my_map_shape_click$id)
  })


  ##----------------- Outputs -----------------##
  
  basemap <- leaflet(options = leafletOptions(zoomSnap = 0.5,
                                              zoomDelta = 0.5)) %>%
    addProviderTiles(providers$CartoDB.Positron,
                     options = providerTileOptions(noWrap = TRUE)) %>%
    setView(30.44, 56.00, 3.5)
  output$my_map <- renderLeaflet({basemap})
  
  output$eu_html_value <- renderUI({
    value <- eu_indicator_value_r()
    div(HTML(sprintf("European Union 28 <text style='color:%s'> %s </text>",
                     my_pal(value),
                     value)))
  })
  
  output$top_3_table <- renderFormattable({
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
  
  output$bottom_3_table <- renderFormattable({
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
  
  output$domains_chart <- renderHighchart({
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
  
  output$subdomains_chart <- renderHighchart({
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
  
  output$indicators_chart <- renderHighchart({
    shiny::validate(need(substr(input$indicator,1,1) == "S", message = FALSE))
    data <- map_indicators_chart_data_r()
    highchart() %>%
      hc_chart(marginLeft = "160") %>%
      hc_add_series(data, type = "bar", name = unique(data$Country),
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
               labels = list(style = list(fontSize = "10px",
                                          textOverflow = "none"))) %>%
      hc_yAxis(min = 0, max = 100, tickAmount = 5, showLastLabel = TRUE) %>%
      hc_add_theme(my_hc_theme)
  })
  
  output$country_html_value <- renderUI({
    value <- gei_indicator_value_r()
    HTML(sprintf(
      paste0("<text id = 'country_html_title'><tspan>%s</tspan></text>",
             "<text id = 'country_html_subtitle' style = 'color:%s'>",
             "<tspan>%s</tspan></text>"),
      gei_year_country_data_r()$Country,
      my_pal(gei_indicator_value_r()),
      value))
  })
  
  output$total_chart <- renderHighchart({
    shiny::validate(need(substr(input$indicator,1,1) == "I", message = FALSE))
    data <- map_indicators_chart_data_r()$Score[3]
    max_value <- get_max_value(sel_indicator_r())
    create_gender_gauge(data, max_value, "Total", "green")
  })
  
  output$women_chart <- renderHighchart({
    shiny::validate(need(substr(input$indicator,1,1) == "I", message = FALSE))
    data <- map_indicators_chart_data_r()$Score[1]
    max_value <- get_max_value(sel_indicator_r())
    create_gender_gauge(data, max_value, "Women", "pink")
  })
  
  output$men_chart <- renderHighchart({
    shiny::validate(need(substr(input$indicator,1,1) == "I", message = FALSE))
    data <- map_indicators_chart_data_r()$Score[2]
    max_value <- get_max_value(sel_indicator_r())
    create_gender_gauge(data, max_value, "Men", "blue")
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
          score_df[-c(1, 2)] - score_df[-c(1, n_cols)],
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
      gather("Year", "Trend", 2:ncol(.)) %>%
      group_by(Country) %>%
      summarise(Variations = list(Variations), .groups = "drop")
  })
  
    
  ##------------- Event observers -------------##
  
  observeEvent(input$trend_indicator, {
    # It sets the indicator variable value
    trend_sel_indicator_r(gei_full_indicators %>%
                            filter(Id == input$trend_indicator) %>%
                            pull("Indicator (s)")
    )
    
    # It adapts the selector style taking into account the selected indicator
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
  
  output$trend_chart <- renderHighchart({
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
  
  output$trend_score_table <- renderReactable({
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
          class = "flagged_country",
          img(class = "flag", alt = paste(value, "flag"),
              src = sprintf("images/flags/%s.png", value)),
          div(class = "country_name", value)
        )
      },
      filterable = TRUE,
      minWidth = 110,
      style = list(position = "sticky", zIndex = 1, left = 0,
                   background = "white"),
      headerStyle = list(position = "sticky", zIndex = 1, left = 0,
                         background = "white")
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
  
  output$trend_var_table <- renderReactable({
    # Define data
    data <- trend_indicator_var_table_data_r()
    sparkline_data <- trend_sparkline_data_r()
    
    # Define column styles
    col_names <- data %>%
      select(-c(Country, Trend)) %>%
      colnames()
    country_col_def <- list(colDef(
      cell = function(value, index) {
        div(
          class = "flagged_country",
          img(class = "flag", alt = paste(value, "flag"),
              src = sprintf("images/flags/%s.png", value)),
          div(class = "country_name", value),
        )
      },
      filterable = TRUE,
      minWidth = 110,
      style = list(position = "sticky", zIndex = 1, left = 0,
                   background = "white"),
      headerStyle = list(position = "sticky", zIndex = 1, left = 0,
                         background = "white")
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
        list(alignSelf = "center", color = trend_pal(value), fontWeight = 600)
      }
    )) %>%
      rep(length(col_names)) %>%
      `names<-`(col_names)
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
                  minSpotColor = default_red,
                  maxSpotColor = default_green,
                  highlightSpotColor = gei_color,
                  highlightLineColor = gei_color
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

  
  ###============ Country Explorer ============###
  
  ##------------- Reactive values -------------##
  
  # Reactive value for the selected domain in the country explorer tab
  country_domain_r <- reactiveVal("Gender Equality Index")
  
  ##----------- Reactive expressions -----------##
  
  # Reactive expression for the selected country trend data used in the chart
  # It returns the GEI and domains data for the selected country
  country_trend_chart_data_r <- reactive({
    gei_data %>%
      select(Year,
             Country,
             gei_full_indicators %>% 
               filter(Id == "GEI" | `Parent Id` == "GEI") %>% 
               select("Indicator (s)") %>%
               unlist(use.names = FALSE)) %>%
      filter(Country == input$country_country) %>%
      gather("Indicator", "Score", 3:ncol(.)) %>%
      arrange_at(c("Year", "Indicator"))
  })
  
  # Reactive expression for the selected domain data for EU-28
  # It returns the GEI or domain value for the EU-28 and the picked year 
  eu_domain_data_r <- reactive({
      gei_data %>%
        select(Year, Country, Score = country_domain_r()) %>%
        filter(Year == input$country_year) %>%
        filter(Country == "European Union 28")
  })
  
  # Reactive expression for the selected domain data used in the country
  # explorer
  # It returns the GEI or domain value along with its ranking for the picked
  # country and year 
  country_domain_data_r <- reactive({
    if (input$country_country != "European Union 28") {
      gei_data %>%
        select(Year, Country, Score = country_domain_r()) %>%
        filter(Year == input$country_year) %>%
        filter(Country != "European Union 28") %>%
        mutate(Rank = rank(-Score, ties.method = "min")) %>%
        filter(Country == input$country_country)
    } else {
      eu_domain_data_r()
    }
  })
  
  # Reactive expression for the country trend data used in the table
  # It returns each year score and the variatons vs. the previous year and vs.
  # the first edition (2013).
  # When GEI is selected, it returns GEI and domains data. If a domain is
  # selected, it returns that domain and its subdomains and indicators.
  country_trend_table_data_r <- reactive({
    if (country_domain_r() == "Gender Equality Index") {
      gei_data %>%
        filter(Country == input$country_country) %>%
        select(Year,
               gei_full_indicators %>% 
                 filter(Id == "GEI" | `Parent Id` == "GEI") %>% 
                 select("Indicator (s)") %>%
                 unlist(use.names = FALSE)) %>%
        `rownames<-`(.[,1]) %>%
        select(-Year) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Indicator")
    }
  })

    
  ##------------- Event observers -------------##
  
  # It sets the selected domain to the one clicked in the trend chart
  observeEvent(input$country_clicked_domain, {
    country_domain_r(input$country_clicked_domain)
  })
  
  
  ##----------------- Outputs -----------------##
  
  output$country_html_name <- renderUI({
    value <- input$country_country
    div(
      class = "flagged_country",
      img(class = "flag", alt = paste(value, "flag"),
          src = sprintf("images/flags/%s.png", value)),
      div(class = "country_name", value),
    )
  })
  
  output$country_trend_chart <- renderHighchart({
    data <- country_trend_chart_data_r()
    highchart() %>%
      hc_chart(type = 'line', marginLeft = "50") %>%
      hc_add_series(data, type="line", name = unique(data$Indicator),
                    hcaes(x = Year, y = Score, group = Indicator),
                    lineWidth = 1, marker = list(radius = 2),
                    showInLegend = FALSE) %>%
      hc_colors(c(gei_color, health_color, knowledge_color, money_color,
                  power_color, time_color, work_color)) %>%
      hc_xAxis(title = list(text = "Year"), allowDecimals = FALSE,
               tickPositions = gei_years) %>%
      hc_yAxis(title = list(text = "Score"), maxPadding = 0.005) %>%
      hc_pane(size = "100%") %>%
      hc_plotOptions(
        series = list(
          events = list(
            click = JS("function(event) {
              Shiny.onInputChange('country_clicked_domain',
                                  [event.point.series.name])
            }")))) %>%
      hc_credits(enabled = TRUE,
                 text = "*Click on lines to explore domains",
                 href = "",
                 position = list(x = 0, align = "left"),
                 style = list(cursor = "default"))%>%
      hc_add_theme(my_hc_theme)
  })
  
  output$country_domain_html_title <- renderUI({
    domain <- country_domain_r()
    year <- input$country_year
    HTML(sprintf(
      paste0("<text id = 'domain_html_title' style = 'color:%s'>
               <tspan>%s</tspan>
             </text>",
             "<text id = 'year_html_subtitle'>
               <tspan>%s</tspan>
             </text>"),
      ifelse(
        country_domain_r() == "Gender Equality Index",
        gei_color,
        domain_color_mapping[country_domain_r()]
      ),
      domain,
      year
    ))
  })
  
  output$country_domain_chart <- renderHighchart({
    data <- country_domain_data_r()$Score
    name <- country_domain_data_r()$Country
    highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_add_series(data, name = name, showInLegend = FALSE) %>%
      hc_yAxis(min = 0, max = 100, tickWidth = 0, minorTicks = FALSE,
               labels = list(enabled = FALSE)) %>%
      hc_pane(size = "100%",
              background = list(outerRadius = "100%", innerRadius = "75%",
                                shape = "arc")) %>%
      hc_plotOptions(
        solidgauge = list(
          innerRadius = "75%",
          dataLabels = list(y = -15, borderWidth = 0, useHTML = TRUE,
                            style = list(fontSize = "18px", 
                                         color = my_pal(data))))) %>%
      hc_colors(gei_domain_color_mapping[[country_domain_r()]]) %>%
      hc_tooltip(outside = TRUE) %>%
      hc_add_theme(my_hc_theme)
  })
  
  output$country_domain_ranking_text = renderText ({
    if (input$country_country != "European Union 28") {
      paste("Ranking:", country_domain_data_r()$Rank)
    } else {
      ""
    }
  })
  
  output$country_domain_html_detail = renderUI ({
    # Gather all the necessary data
    year <- input$country_year
    country <- input$country_country
    is_country <- input$country_country != "European Union 28"
    domain_score <- country_domain_data_r()$Score
    if (is_country) {
      domain_ranking <- country_domain_data_r()$Rank
    }
    eu_score <- eu_domain_data_r()$Score
    eu_difference <- domain_score - eu_score
    
    # Build the detail
    div(
      id = "country_domain_detail",
      HTML(
        paste0(
          sprintf("In the %s edition, %s<b>%s</b> scores %s points",
                  year,
                  ifelse(is_country, "", "the "),
                  ifelse(is_country, country, "European Union"),
                  domain_score),
          ifelse(is_country,
                 sprintf(
                   paste0(" and ranks in the %s position in the EU-28.",
                          " It's %s points %s the EU's score."),
                   domain_ranking,
                   abs(round(eu_difference, 2)),
                   ifelse(eu_difference >= 0, "above", "below")
                 ),
                 "."
          )
        )
      )
    )
  })
  
  output$country_trend_table <- renderReactable({
    data <- country_trend_table_data_r()
    print(data)
    reactable(data)
  })
  
  
  
  ###=========== Country Comparator ===========###
  
  ##------------- Reactive values -------------##
  
  # Reactive value for the selected subdomain in the comparator tab
  comp_subdomain_r <- reactiveVal(NULL)

  
  ##----------- Reactive expressions -----------##
  
  # Reactive expression for the first country-year pair selected
  # It builds the name used in data frames from inputs
  first_data_name_r <- reactive({
    paste(input$comp_first_country,
          input$comp_first_year,
          sep = " - ")
  })
  
  # Reactive expression for the second country-year pair selected
  # It builds the name used in data frames from inputs
  second_data_name_r <- reactive({
    paste(input$comp_second_country,
          input$comp_second_year,
          sep = " - ")
  })
  
  # Reactive expression for the data used in the comparison charts
  # It returns indicator values for the two selected country-year pairs
  comp_chart_data_r <- reactive({
    gei_data %>%
      as.data.frame() %>%
      filter(
        (Country == input$comp_first_country & Year == input$comp_first_year) |
          (Country == input$comp_second_country & Year == input$comp_second_year)
      ) %>%
      select(-"Country code") %>%
      unite("Country - Year", c(Country, Year), sep = " - ") %>%
      `rownames<-`(.[,1]) %>%
      select(-"Country - Year") %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Indicator")
  })
  
  # Reactive expression for comparison GEI charts data
  # It returns GEI values for the two selected country-year pairs
  comp_gei_chart_data_r <- reactive({
    comp_chart_data_r() %>%
      filter(Indicator == "Gender Equality Index")
  })
  
  # Reactive expression for the data used in the comparison polar chart
  # It returns domains values for the two selected country-year pairs
  comp_domains_chart_data_r <- reactive({
    comp_chart_data_r() %>%
      filter(Indicator %in% (gei_full_indicators %>% 
                               filter(`Parent Id` == "GEI") %>% 
                               select("Indicator (s)") %>%
                               unlist(use.names = FALSE)))
  })
  
  # Reactive expression for comparison domain charts data
  # It returns picked domain values for the two selected country-year pairs
  comp_domain_chart_data_r <- reactive({
    indicator_id <- gei_full_indicators %>%
      filter(`Indicator (s)` == input$comp_domain) %>%
      pull(`Indicator (s)`)
    
    comp_chart_data_r() %>%
      filter(Indicator == indicator_id)
  })
  
  # Reactive expression for the data used in the comparison bar chart
  # It returns picked domain subdomains values for the two selected country-year
  # pairs
  comp_subdomains_chart_data_r <- reactive({
    indicator_id <- gei_full_indicators %>%
      filter(`Indicator (s)` == input$comp_domain) %>%
      pull(Id)
    
    comp_chart_data_r() %>%
      filter(Indicator %in% (gei_full_indicators %>% 
                               filter(`Parent Id` == indicator_id) %>% 
                               select("Indicator (s)") %>%
                               unlist(use.names = FALSE)))
  })
  
  # Reactive expression for comparison subdomain charts data
  # It returns picked subdomain values for the two selected country-year pairs
  comp_subdomain_chart_data_r <- reactive({
    indicator_id <- gei_full_indicators %>%
      filter(`Indicator (s)` == comp_subdomain_r()) %>%
      pull(`Indicator (s)`)
    
    comp_chart_data_r() %>%
      filter(Indicator == indicator_id)
  })
  
  # Reactive expression for comparison indicators table data
  # It returns picked subdomain indicators values and the difference between 
  # them for the two selected country-year pairs
  comp_indicators_table_data_r <- reactive({
    indicator_id <- gei_full_indicators %>%
      filter(`Indicator (s)` == comp_subdomain_r()) %>%
      pull(Id)
    
    comp_chart_data_r() %>%
      filter(Indicator %in% (gei_full_indicators %>% 
                               filter(`Parent Id` == indicator_id) %>% 
                               select("Indicator (s)") %>%
                               unlist(use.names = FALSE))) %>%
      mutate(Difference = (!!as.name(first_data_name_r()) - 
                             !!as.name(second_data_name_r()))) %>%
      relocate(Indicator, first_data_name_r(), Difference, second_data_name_r())
  })
  
  
  ##------------- Event observers -------------##
  
  observeEvent(input$comp_domain, {
    # It sets the first subdomain as the selected one when a new domain is
    # picked 
    comp_subdomain_r((comp_subdomains_chart_data_r() %>%
                        pull(Indicator))[1])
    
    # It adapts the color of the domain comparator container taking into account
    # the selected domain
    runjs(paste0(
      '$("#domain_comparator_container .box").css("border-color", "',
      domain_color_mapping[input$comp_domain]
      ,'");'
    ))
  })
  
  # It sets the selected subdomain to the one clicked in the bar chart
  observeEvent(input$comp_clicked_subdomain, {
    comp_subdomain_r(input$comp_clicked_subdomain)
  })
  
  
  ##----------------- Outputs -----------------##
  
  output$comp_domains_chart <- renderHighchart({
    data <- comp_domains_chart_data_r()
    gei_difference <- comp_gei_chart_data_r()[[first_data_name_r()]] -
      comp_gei_chart_data_r()[[second_data_name_r()]]
    
    highchart() %>%
      hc_chart(type = "area", polar = TRUE, backgroundColor = "transparent") %>%
      hc_add_series(data[[first_data_name_r()]], name = first_data_name_r(),
                    color = first_country_color, fillOpacity = 0.4,
                    marker = list(radius = 3), showInLegend = FALSE) %>%
      hc_add_series(data[[second_data_name_r()]], name = second_data_name_r(),
                    color = second_country_color, fillOpacity = 0.4,
                    marker = list(radius = 3), showInLegend = FALSE) %>%
      hc_title(text = "GENDER EQUALITY INDEX",
               style = list(color = gei_color,
                            fontSize = "16px",
                            fontWeight = "bold")) %>%
      hc_subtitle(text = paste0(ifelse(gei_difference > 0, "\u2191 ", ""),
                                abs(round(gei_difference, 2)),
                                ifelse(gei_difference < 0, " \u2191", "")),
                  useHTML = TRUE,
                  style = list(color = ifelse(gei_difference > 0,
                                              first_country_color,
                                              second_country_color),
                               fontSize = "16px",
                               fontWeight = "bold")) %>%
      hc_xAxis(categories = data$Indicator) %>%
      hc_yAxis(min = 0, max = 100, tickAmount = 5, showLastLabel = TRUE,
               labels = list(style = list(fontSize = "10px"))) %>%
      hc_tooltip(outside = TRUE) %>%
      hc_responsive(
        rules = list(
          list(
            condition = list(maxWidth  = 220),
            chartOptions = list(xAxis = list(
              labels = list(
                distance = 10,
                formatter = JS("function () {
                  return this.value.toString().charAt(0)
                }")
              )
            ))
          ),
          list(
            condition = list(minWidth  = 220),
            chartOptions = list(xAxis = list(
              labels = list(
                distance = 15,
                formatter = JS("function () {
                  return this.value
                }")
              )
            ))
          )
        )
      ) %>%
      hc_add_theme(my_hc_theme)
  })
  
  output$first_gei_chart <- renderHighchart({
    data <- comp_gei_chart_data_r()[[first_data_name_r()]]
    star <- data > comp_gei_chart_data_r()[[second_data_name_r()]]
    create_indicator_gauge(data, first_data_name_r(), first_country_color,
                           star, "16px", -15)
  })
  
  output$second_gei_chart <- renderHighchart({
    data <- comp_gei_chart_data_r()[[second_data_name_r()]]
    star <- data > comp_gei_chart_data_r()[[first_data_name_r()]]
    create_indicator_gauge(data, second_data_name_r(), second_country_color,
                           star, "16px", -15)
  })
  
  output$first_domain_chart <- renderHighchart({
    data <- comp_domain_chart_data_r()[[first_data_name_r()]]
    star <- data > comp_domain_chart_data_r()[[second_data_name_r()]]
    create_indicator_gauge(data, first_data_name_r(), first_country_color,
                           star, "14px", -15)
  })
  
  output$second_domain_chart <- renderHighchart({
    data <- comp_domain_chart_data_r()[[second_data_name_r()]]
    star <- data > comp_domain_chart_data_r()[[first_data_name_r()]]
    create_indicator_gauge(data, second_data_name_r(), second_country_color,
                           star, "14px", -15)
  })
  
  output$domain_diff_html_value <- renderUI({
    difference <- comp_domain_chart_data_r()[[first_data_name_r()]] -
      comp_domain_chart_data_r()[[second_data_name_r()]]
    value <- paste0(ifelse(difference > 0, "\u2191 ", ""),
                    abs(round(difference, 2)),
                    ifelse(difference < 0, " \u2191", ""))
    div(HTML(sprintf("<text style='color:%s'> %s </text>",
                     ifelse(difference > 0,
                            first_country_color, 
                            second_country_color)
                     , value)))
  })
  
  output$comp_subdomains_chart <- renderHighchart({
    data <- comp_subdomains_chart_data_r()
    highchart() %>%
      hc_chart(marginLeft = "90", backgroundColor = "transparent") %>%
      hc_add_series(data[[first_data_name_r()]], type="bar",
                    name = first_data_name_r(),
                    color = first_country_color, fillOpacity = 0.4,
                    showInLegend = FALSE) %>%
      hc_add_series(data[[second_data_name_r()]], type="bar",
                    name = second_data_name_r(),
                    color = second_country_color, fillOpacity = 0.4,
                    showInLegend = FALSE) %>%
      hc_xAxis(categories = if(length(data$Indicator) == 1) {
        list(data$Indicator)
      } else {
        data$Indicator
      },
      labels = list(style = list(fontSize = "10px",
                                 textOverflow = "none"))) %>%
      hc_yAxis(min = 0, max = 100, tickAmount = 5, showLastLabel = TRUE) %>%
      hc_plotOptions(series = list(events = list(
        click = JS("function(event) {
          Shiny.onInputChange('comp_clicked_subdomain', [event.point.category])
        }")
      ))) %>%
      hc_credits(enabled = TRUE,
                 text = "*Click on bars to change subdomain",
                 href = "",
                 position = list(x = 0, align = "left"),
                 style = list(cursor = "default"))%>%
      hc_add_theme(my_hc_theme)
  })
  
  output$subdomain_html_name <- renderUI({
    domain <- comp_subdomain_r()
    div(HTML(sprintf("<text style='color:%s'> %s </text>",
                     domain_color_mapping[input$comp_domain],
                     domain)))
  })
  
  output$first_subdomain_chart <- renderHighchart({
    data <- comp_subdomain_chart_data_r()[[first_data_name_r()]]
    star <- data > comp_subdomain_chart_data_r()[[second_data_name_r()]]
    create_indicator_gauge(data, first_data_name_r(), first_country_color,
                           star, "12px", -12)
  })
  
  output$second_subdomain_chart <- renderHighchart({
    data <- comp_subdomain_chart_data_r()[[second_data_name_r()]]
    star <- data > comp_subdomain_chart_data_r()[[first_data_name_r()]]
    create_indicator_gauge(data, second_data_name_r(), second_country_color,
                           star, "12px", -12)
  })
  
  output$subdomain_diff_html_value <- renderUI({
    difference <- comp_subdomain_chart_data_r()[[first_data_name_r()]] -
      comp_subdomain_chart_data_r()[[second_data_name_r()]]
    value <- paste0(ifelse(difference > 0, "\u2191 ", ""),
                    abs(round(difference, 2)),
                    ifelse(difference < 0, " \u2191", ""))
    div(HTML(sprintf("<text style='color:%s'> %s </text>",
                     ifelse(difference > 0,
                            first_country_color, 
                            second_country_color)
                     , value)))
  })
  
  output$comp_indicators_table <- renderReactable({
    # Define data
    data <- comp_indicators_table_data_r()
    
    # Define column styles
    col_names <- data %>% colnames()
    indicator_col_def <- list(colDef(
      align = "left",
      style = list(position = "sticky", zIndex = 1, left = 0,
                   color = rgb(102, 102, 102, maxColorValue = 255),
                   background = "white"),
      headerStyle = list(position = "sticky", zIndex = 1, left = 0,
                         background = "white")
    )) %>%
      `names<-`("Indicator")
    first_data_col_def <- list(colDef(
      cell = function(value, index) {
        div(class = "comp_indicators_table_score",
            style = list(borderColor = first_country_color),
            ifelse(data$Difference[index] > 0,
                   paste0(value, " \u002A"),
                   value))
      },
      align = "center",
      width = 60,
      style = function(value) {
        list(alignSelf = "center", color = my_pal(value))
      }
    )) %>%
      `names<-`(first_data_name_r())
    difference_col_def <- list(colDef(
      cell = function(value) {
        div(paste0(ifelse(value > 0, "\u2191 ", ""),
                   abs(round(value, 2)),
                   ifelse(value < 0, " \u2191", "")))
      },
      align = "center",
      html = TRUE,
      width = 60,
      style = function(value) {
        list(alignSelf = "center", color = ifelse(value > 0,
                                                  first_country_color, 
                                                  second_country_color))
      }
    )) %>%
      `names<-`("Difference")
    second_data_col_def <- list(colDef(
      cell = function(value, index) {
        div(class = "comp_indicators_table_score",
            style = list(borderColor = second_country_color),
            ifelse(data$Difference[index] < 0,
                   paste0(value, " \u002A"),
                   value))
      },
      align = "center",
      width = 60,
      style = function(value) {
        list(alignSelf = "center", color = my_pal(value))
      }
    )) %>%
      `names<-`(second_data_name_r())
    col_defs <- c(indicator_col_def, first_data_col_def, difference_col_def,
                  second_data_col_def)
    
    # Create table
    reactable(data,
              columns = col_defs,
              style = list(
                fontSize = 12
              ))
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
      id = "download_data",
      condition = !(input$data_tabset_panel == "Data table" &&
                      nrow(data_table_r()) == 0)
    )
  })


  ##----------------- Outputs -----------------##
  
  output$download_data <- downloadHandler(
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
      
      if (input$download_type == "CSV")  {
        write.csv2(data, file, row.names = FALSE)
      } else {
        write.xlsx(as.data.frame(data), file, row.names = FALSE)
      }
    }
  )
  
  output$data_table <- renderReactable({
    col_defs <- list(
      Year = colDef(
        width = 60,
        style = list(position = "sticky", zIndex = 1, left = 0,
                     background = "white"),
        headerStyle = list(position = "sticky", zIndex = 1, left = 0,
                           background = "white")
      ),
      "Country code" = colDef(show = FALSE),
      Country = colDef(
        width = 120,
        style = list(position = "sticky", zIndex = 1, left = "60px",
                     background = "white"),
        headerStyle = list(position = "sticky", zIndex = 1, left = "60px",
                           background = "white")
      )
    )
    reactable(data_table_r(),
              showSortable = TRUE,
              defaultColDef = colDef(
                minWidth = 180
              ),
              columns = col_defs,
              style = list(
                fontSize = 12
              )
    )
  })
  
  output$metadata_table <- renderReactable({
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
