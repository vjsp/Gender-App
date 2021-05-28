## ui.R ##
source("./global.R")
source("./datasets_treatment.r")

dashboardPage(
  dashboardHeader(title="Gender Equality App"),
  dashboardSidebar(
    menuItem("Map Explorer", tabName = "map", icon = icon("dashboard")),
    menuItem("Trend Explorer", tabName = "map", icon = icon("th")),
    menuItem("Country Explorer", tabName = "country", icon = icon("th")),
    menuItem("Comparator", tabName = "comparator", icon = icon("th")),
    menuItem("Data", tabName = "data", icon = icon("th")),
    menuItem("About", tabName = "about", icon = icon("th")),
    selectInput("indicator", "Indicator:", gei_indicators_df["Indicator"]),
    sliderTextInput("plot_date",
      label = h5("Select year"),
      choices = sort(unique(my_data$Year)),
      selected = current_year,
      grid = FALSE,
      animate = animationOptions(interval = 3000,loop = FALSE)
    )
  ),
  dashboardBody(
    tabItems(
      # Map Explorer content
      tabItem(tabName = "map",
        fluidRow(
          box(leafletOutput("mymap")),
          box(height="250"),
          box(plotOutput('radarPlot', height="250"))
        )
      )
    )
  )
)