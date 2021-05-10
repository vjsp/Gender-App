# Install and load necessary libraries
if(!require(geojsonio)) install.packages("geojsonio",
                                         repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet",
                                       repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr",
                                        repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl",
                                     repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny",
                                     repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets",
                                            repos = "http://cran.us.r-project.org")

# Input data
world_countries <- geojson_read("data/world.geo.json", what = "sp")
countries <- read.csv("data/countries.csv")
gei <- read_excel("data/GEI.xlsx")

colnames(countries)[1] <- "Country"
my_data <- merge(gei, countries, by="Country")
current_year <- max(my_data$Year)

my_pal <- colorNumeric(palette="viridis",
                       domain=my_data$`Overall Gender Equality Index`,
                       na.color="transparent")
gei_year = subset(my_data, Year==current_year) 
cv_large_countries = gei_year %>% filter(alpha.3 %in% world_countries$adm0_a3)
plot_map <- world_countries[world_countries$adm0_a3 %in% cv_large_countries$alpha.3, ]

mytext <- paste(
  "Country: ", cv_large_countries$Country,"<br/>", 
  "Work: ", cv_large_countries$`Work (Domain score)`, "<br/>", 
  "Money: ", cv_large_countries$`Money (Domain score)`,
  sep="") %>%
  lapply(htmltools::HTML)

basemap <- leaflet(plot_map) %>%
  addProviderTiles(providers$CartoDB.Positron,
                   options = providerTileOptions(noWrap = TRUE)
                   ) %>%
  addLegend("bottomright",
            pal = my_pal,
            values = ~cv_large_countries$`Overall Gender Equality Index`,
            title = "<small>GEI</small>")

ui <- bootstrapPage(
  navbarPage("Gender App", id="nav",
             tabPanel("Gender Equality Index",
                      sidebarLayout(
                        sidebarPanel(
                          sliderTextInput("plot_date",
                                          label = h5("Select year"),
                                          choices = unique(my_data$Year),
                                          selected = current_year,
                                          grid = FALSE,
                                          animate = animationOptions(interval = 3000,
                                                                     loop = FALSE)
                          )
                        ),
                        mainPanel(leafletOutput("mymap"))
                      )
             )
  )
)

server <- function(input, output, session) {
  
  formatted_date = reactive({
    input$plot_date
  })
  
  reactive_db = reactive({
    my_data %>% filter(Year == formatted_date())
  })
  
  reactive_polygons = reactive({
    world_countries[world_countries$adm0_a3 %in% cv_large_countries$alpha.3, ]
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha.3 %in% world_countries$adm0_a3)
    worldcountry_subset = world_countries[world_countries$adm0_a3 %in% large_countries$alpha.3, ]
    large_countries = large_countries[match(worldcountry_subset$adm0_a3, large_countries$alpha.3),]
    large_countries
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
                  fillOpacity = 0.15,
                  fillColor = ~my_pal(reactive_db_large()$`Overall Gender Equality Index`),
                  label = paste(
                    "Country: ", reactive_db_large()$Country,"<br/>", 
                    "Work: ", reactive_db_large()$`Work (Domain score)`, "<br/>", 
                    "Money: ", reactive_db_large()$`Money (Domain score)`,
                    sep="") %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                  ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)