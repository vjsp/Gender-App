## ui.R ##
source("./global.R")

dashboardPage(
  dashboardHeader(title="Gender Equality App"),
  dashboardSidebar(
    menuItem("Map Explorer", tabName = "map", icon = icon("dashboard")),
    menuItem("Country Explorer", tabName = "country", icon = icon("th")),
    menuItem("Comparator", tabName = "comparator", icon = icon("th")),
    menuItem("Data", tabName = "data", icon = icon("th")),
    menuItem("About", tabName = "about", icon = icon("th")),
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
          box(plotOutput('radarPlot', height="250"))
        )
      )
    )
  )
)