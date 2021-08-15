## ui.R ##

dashboardPage(
  dashboardHeader(title = "Gender Equality App"),
  dashboardSidebar(
    sidebarMenu(
      convert_menu_item(
        menuItem("Map Explorer", tabName = "map", icon = icon("globe"),
          sliderTextInput("plot_date",
            label = h5("Select year"),
            choices = sort(unique(gei_data$Year)),
            selected = current_year,
            grid = FALSE,
            animate = animationOptions(interval = 3000, loop = FALSE)
          )
        ),
        tab_name = "map"
      ),
      convert_menu_item(
        menuItem("Trend Explorer", tabName = "trend", icon = icon("chart-line")),
        tab_name = "trend"
      ),
      convert_menu_item(
        menuItem("Country Explorer", tabName = "country", icon = icon("flag")),
        tab_name = "country"
      ),
      convert_menu_item(
        menuItem("Country Comparator", tabName = "comparator",
          icon = icon("balance-scale")
        ),
        tab_name = "comparator"),
      convert_menu_item(
        menuItem("Data Explorer", tabName = "data", icon = icon("folder-open")),
        tab_name = "data"),
      convert_menu_item(
        menuItem("About", tabName = "about", icon = icon("book")),
        tab_name = "about"),
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
        div(class = "outer", height = "100%",
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
              highchartOutput("domainsChart", height = "225")
              # chartJSRadarOutput("radarPlot", height = "250")
            ),
            conditionalPanel(condition = "input.indicator.startsWith('D')",
              class = "conditional_panel",
              highchartOutput("subdomainsChart", height = "225")
            ),
            conditionalPanel(condition = "input.indicator.startsWith('S')",
              class = "conditional_panel",
              highchartOutput("indicatorsChart", height = "225")
            ),
            conditionalPanel(condition = "input.indicator.startsWith('I')",
              id = "metric_container", class = "conditional_panel",
              div(id = "metrics_charts", width = 12, height = 225,
                div(id = "country_value_container",
                    htmlOutput("countryHtmlValue")
                ),
                div(id = "metrics_charts_container",
                  highchartOutput("totalChart", height = "175"),
                  highchartOutput("womenChart", height = "175"),
                  highchartOutput("menChart", height = "175")
                )
              )
            )
          )
        ),
      )
    )
  ),
  skin = "purple"
)
