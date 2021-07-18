## ui.R ##
#source("./global.R")
#source("./datasets_treatment.r")

dashboardPage(
  dashboardHeader(title="Gender Equality App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map Explorer", tabName = "map", icon = icon("globe")),
      menuItem("Trend Explorer", tabName = "map", icon = icon("chart-line")),
      menuItem("Country Explorer", tabName = "country", icon = icon("flag")),
      menuItem("Country Comparator", tabName = "comparator",
               icon = icon("balance-scale")),
      menuItem("Data Explorer", tabName = "data", icon = icon("folder-open")),
      menuItem("About", tabName = "about", icon = icon("book"))
    ),
    sliderTextInput("plot_date",
      label = h5("Select year"),
      choices = sort(unique(gei_data$Year)),
      selected = current_year,
      grid = FALSE,
      animate = animationOptions(interval = 3000, loop = FALSE)
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      # Map Explorer content
      tabItem(tabName = "map",
        div(class="outer", height = "100%",
          leafletOutput("mymap"),
          absolutePanel(id = "map_indicator_panel",
            top = 10,
            draggable = TRUE,
            width = "90%",
            left = "50%",
            selectInput(inputId = "indicator",
              label = NULL,
              choices = setNames(gei_indicators$Id, gei_indicators$Indicator),
              selected = "GEI",
              width = "80%"
            )
          ),
          absolutePanel(id = "map_graphs",
            top = 100,
            draggable = TRUE,
            width = "30%",
            right = 0,
            div("EU-28"),
            conditionalPanel(condition = "input.indicator == 'GEI'",
                             chartJSRadarOutput("radarPlot", height = "250")
            )
          )
        ),
      )
    )
  )
)