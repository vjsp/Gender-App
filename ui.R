##################### ui.R #####################

dashboardPage(
  dashboardHeader(title = "Gender Equality App"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar_menu",
      menuItem("Map Explorer", tabName = "map", icon = icon("globe")),
      conditionalPanel("input.sidebar_menu == 'map'",
        class = "sidebar_conditional_panel",
        sliderTextInput("map_year",
          label = h5("Select year"),
          choices = gei_years,
          selected = current_year,
          animate = animationOptions(interval = 3000, loop = FALSE)
        )
      ),
      menuItem("Trend Explorer", tabName = "trend", icon = icon("chart-line")),
      conditionalPanel("input.sidebar_menu == 'trend'",
        class = "sidebar_conditional_panel",
        sliderTextInput("trend_years",
          label = h5("Select years range"),
          choices = gei_years,
          selected = default_year_range
        ),
        pickerInput("trend_countries",
          label = h5("Select countries"), 
          choices = unique(levels(gei_data$Country)),
          selected = c("European Union 28", "France", "Greece", "Spain",
                       "Sweden","United Kingdom"),
          options = list(`actions-box` = TRUE,
                         `dropup-auto` = FALSE),
          multiple = TRUE
        )
      ),
      menuItem("Country Explorer", tabName = "country", icon = icon("flag")),
      menuItem("Country Comparator", tabName = "comparator",
               icon = icon("balance-scale")),
      menuItem("Data Explorer", tabName = "data", icon = icon("folder-open")),
      menuItem("About", tabName = "about", icon = icon("book")),
      div(
        id = "legend_container",
        box(
          width = 12, title = "Legend", solidHeader = TRUE,
          p(class = "GEI_option", "Gender Equality Index"),
          p(class = "Domain_option", "Domain"),
          p(class = "Subdomain_option", "Subdomain"),
          p(class = "Indicator_option", "Indicator")
        )
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(HTML(js))
    ),
    tabItems(
      # Map Explorer content
      tabItem(tabName = "map",
        div(id = "map_outer", class = "outer", height = "100%",
          leafletOutput("myMap"),
          absolutePanel(id = "map_indicator_panel",
            width = "90%",
            top = 10,
            left = "50%",
            draggable = TRUE,
            selectInput(inputId = "indicator",
              label = NULL,
              choices = setNames(gei_indicators$Id,
                                 gei_indicators$`Indicator (s)`),
              selected = "GEI",
              width = "80%"
            )
          ),
          absolutePanel(id = "map_graphs",
            width = "30%",
            top = 100,
            right = 0,
            draggable = TRUE,
            div(id = "eu_value_container",
              actionButton("eu_button", "", icon = icon("globe-europe")),
              htmlOutput("euHtmlValue")
            ),
            div(id = "ranking_container",
              column(class = "ranking", width = 6,
                div(class = "rank_title", "Top 3"),
                formattableOutput("top3Table")
              ),
              column(class = "ranking", width = 6,
                div(class = "rank_title", "Bottom 3"),
                formattableOutput("bottom3Table")
              )
            ),
            conditionalPanel(condition = "input.indicator == 'GEI'",
              class = "conditional_panel",
              highchartOutput("domainsChart", height = "230")
              # chartJSRadarOutput("radarPlot", height = "250")
            ),
            conditionalPanel(condition = "input.indicator.startsWith('D')",
              class = "conditional_panel",
              highchartOutput("subdomainsChart", height = "230")
            ),
            conditionalPanel(condition = "input.indicator.startsWith('S')",
              class = "conditional_panel",
              highchartOutput("indicatorsChart", height = "230")
            ),
            conditionalPanel(condition = "input.indicator.startsWith('I')",
              id = "metric_container", class = "conditional_panel",
              div(id = "metrics_charts", width = 12, height = "230",
                div(id = "country_value_container",
                    htmlOutput("countryHtmlValue")
                ),
                div(id = "metrics_charts_container",
                  highchartOutput("totalChart", height = "180"),
                  highchartOutput("womenChart", height = "180"),
                  highchartOutput("menChart", height = "180")
                )
              )
            )
          )
        ),
      ),
      # Trend Explorer content
      tabItem(tabName = "trend",
        div(id = "trend_outer", class = "outer", height = "100%",
          fluidRow(
            div(id = "trend_indicator_panel",
              width = "90%",
              top = 10,
              left = "50%",
              draggable = TRUE,
              selectInput(inputId = "trend_indicator",
                label = NULL,
                choices = setNames(gei_indicators$Id,
                                   gei_indicators$`Indicator (s)`),
                selected = "GEI",
                width = "80%"
              )
            )
          ),
          fluidRow(id = "trend_tabs_row",
            tabsetPanel(
              tabPanel("Trend graph",  highchartOutput("trendChart")),
              tabPanel("Scores table", reactableOutput("trendScoreTable")),
              tabPanel("Variations table", reactableOutput("trendVarTable"))
            )
          )
        )
      ),
      # Data Explorer content
      tabItem(tabName = "data")
    )
  ),
  skin = "purple"
)
