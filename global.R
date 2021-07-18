## global.R ##

## Libraries
# Define used repositories
default_repos = "http://cran.us.r-project.org"
# Install and load necessary libraries
if(!require(dplyr)) install.packages("dplyr", repos = default_repos)
if(!require(fmsb)) install.packages("fmsb",repos = default_repos)
if(!require(geojsonio)) install.packages("geojsonio", repos = default_repos)
if(!require(leaflet)) install.packages("leaflet", repos = defualt_repos)
if(!require(magrittr)) install.packages("magrittr", repos = default_repos)
if(!require(radarchart)) install.packages("radarchart", repos = default_repos)
if(!require(readxl)) install.packages("readxl", repos = default_repos)
if(!require(shiny)) install.packages("shiny", repos = default_repos)
if(!require(shinydashboard)) install.packages("shinydashboard", repos = default_repos)
if(!require(shinyjs)) install.packages("shinyjs", repos = default_repos)
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = default_repos)


## Input data
geo_data <- geojson_read("data/world.geo.json", what = "sp")
gei_data <- readRDS(file = "data/GEI_data.rds")
gei_metadata <- readRDS(file = "data/GEI_metadata.rds")
gei_indicators <- readRDS(file = "data/GEI_indicators.rds")


## Set variables and initial values
current_year <- max(gei_data$Year)
gei_data_year <- subset(gei_data, Year == current_year)
map_data <- geo_data[geo_data$iso_a2 %in% gei_data_year$`Country code`, ]
my_pal <- colorNumeric(palette = "viridis",
                       domain = gei_data$`Gender Equality Index`,
                       na.color = "transparent")

mytext <- paste(
  "Country: ", gei_data_year$Country,"<br/>", 
  "Work: ", gei_data_year$`WORK`, "<br/>", 
  "Money: ", gei_data_year$`MONEY`,
  sep="") %>%
  lapply(htmltools::HTML)

basemap <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron,
                   options = providerTileOptions(noWrap = TRUE)) %>%
  addLegend("bottomleft",
            pal = my_pal,
            values = gei_data_year$`Gender Equality Index`) %>%
  setView(30.44, 56.00, 3)