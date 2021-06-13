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
      menuItem("Country Comparator", tabName = "comparator", icon = icon("balance-scale")),
      menuItem("Data Explorer", tabName = "data", icon = icon("folder-open")),
      menuItem("About", tabName = "about", icon = icon("book"))
    ),
    #selectInput("indicator", "Indicator:", gei_indicators_df["Indicator"]),
    sliderTextInput("plot_date",
      label = h5("Select year"),
      choices = sort(unique(my_data$Year)),
      selected = current_year,
      grid = FALSE,
      animate = animationOptions(interval = 3000,loop = FALSE)
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      # Map Explorer content
      tabItem(tabName = "map",
        # fluidRow(
        #   column(width = 12, align = 'center',
        #          selectInput("indicator", "Indicator:",
        #                      gei_indicators_df["Indicator"],
        #                      width = '100%'
        #          )
        #   )
        # ),

          #box(leafletOutput("mymap")),
          div(class="outer",
              leafletOutput("mymap"),
              absolutePanel(id = "map_indicator_panel",
                top = 10,
                draggable = TRUE,
                width = "90%",
                left = "50%",
                selectInput(inputId = "indicator",
                            label = NULL,
                            choices = gei_indicators_df["Indicator"],
                            width = "80%"
                )
              )
          ),
        fluidRow(
          box(chartJSRadarOutput("radarPlot2", height = "250")),
          box(plotOutput('radarPlot', height="250"))
        )
      )
    )
  )
)