## global.R ##

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
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = default_repos)

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
            title = "<small>GEI</small>") %>%
  setView(8.44, 56.00, 3)