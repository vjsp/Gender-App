## global.R ##

## Libraries ----------------------

# Define used repositories
default_repos = "http://cran.us.r-project.org"
# Install and load necessary libraries
if(!require(dplyr)) install.packages("dplyr", repos = default_repos)
if(!require(formattable)) install.packages("formattable",repos = default_repos)
if(!require(geojsonio)) install.packages("geojsonio", repos = default_repos)
if(!require(highcharter)) install.packages("highcharter", repos = default_repos)
if(!require(leaflet)) install.packages("leaflet", repos = defualt_repos)
if(!require(magrittr)) install.packages("magrittr", repos = default_repos)
if(!require(radarchart)) install.packages("radarchart", repos = default_repos)
if(!require(readxl)) install.packages("readxl", repos = default_repos)
if(!require(shiny)) install.packages("shiny", repos = default_repos)
if(!require(shinydashboard)) install.packages("shinydashboard", repos = default_repos)
if(!require(shinyjs)) install.packages("shinyjs", repos = default_repos)
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = default_repos)
if(!require(tidyr)) install.packages("tidyr", repos = default_repos)


## Input data ----------------------

geo_data <- geojson_read("data/world.geo.json", what = "sp")
gei_data <- readRDS(file = "data/GEI_data.rds")
gei_metadata <- readRDS(file = "data/GEI_metadata.rds")
gei_full_indicators <- readRDS(file = "data/GEI_indicators.rds")


## Variables and initial values ----------------------

# Set the current (latest) year
current_year <- max(gei_data$Year)

# Exclude the gender detail from indicators dataframe
gei_indicators <- gei_full_indicators %>% filter(Type != "Metric")

# Round all data to 2 decimals
gei_data <- gei_data %>% mutate_if(is.numeric, round, digits=2)

# Define the color palette taking into account all the values
my_pal <- colorNumeric(palette = "viridis",
                       domain = c(min(gei_data[3:54]), max(gei_data[3:54])),
                       na.color = "transparent")

# Set highcharter theme to change the font
my_hc_theme <- hc_theme(
  chart = list(
    style = list(fontFamily = "Source Sans Pro")
  )
)


## Auxiliar functions ----------------------

# Function to fix the item selection issue when using input elements in sidebar
# @param menu_item - Menu item
# @param tab_name - Tab name
# @return A list of dataframes
convert_menu_item <- function(menu_item,tab_name) {
  menu_item$children[[1]]$attribs['data-toggle'] <- "tab"
  menu_item$children[[1]]$attribs['data-value'] <- tab_name
  menu_item
}

# Function to use the color palette with formattable
# @return A formatter using the color palette
format_color <- function (...) {
  formatter("span", style = function(x) {
    style(color = my_pal(x))
  })
}

# Function to round up a number taking into account the number of digits
# @param x - The number to be rounded up
# @param digits - The number of digits
# @return The rounded up number
ceiling_digits <- function(x, digits) {
  round(x + 5*10^(-digits -1), digits)
}

# Function to determine the max value in gauge charts
# If the value is higher than 1000, it rounds up to the nearest thousand
# @param indicator - The indicator whose max value is obtained
# @return A number with the max value to use
get_max_value <- function(indicator) {
  indicator_id <- gei_full_indicators %>%
    filter(`Indicator (s)` == indicator) %>%
    pull(Id)
  data <- gei_data %>%
    select(gei_full_indicators %>% 
             filter(`Parent Id` == indicator_id) %>% 
             select("Indicator (s)") %>%
             unlist(use.names=FALSE))
  max_value <- 100 # Default value
  
  if (grepl("(PPS)", indicator, fixed = TRUE)) {
    # For indicators related to PPS, the max overall value is taken, since a
    # default maximum value can not be assumed
    max_value <- data  %>% max()
  } else if (indicator == "Duration of working life (years)") {
    # For the special case of duration of working life, it is considered a
    # maximum of 50 years, except if a higer value exists
    max_value <- data %>% max(., 50)
  } else {
    # For the remaining indicators, a theoretical maximum value of 100 is
    # supposed (most of the are percentages), except if a higher value exists
    # (it could only happen with years)
    max_value <- data %>% max(., 100)
  }
  
  if (max_value > 1000) {
    print(max_value)
    print(log10(max_value))
    max_value %<>% ceiling_digits(., -floor(1 * log10(.)))
  } else {
    max_value
  }
}

# Function to create the gauge charts for gender metrics
# @param data - The value (number) to be represented
# @param max_value - The max value to be used in the chart
# @param title - The chart's title (subtitle)
# @param color - The color used to fill the chart
# @return A gauge chart
create_gauge <- function(data, max_value, title, color) {
  highchart() %>%
    hc_chart(type = 'solidgauge') %>%
    hc_add_series(data, showInLegend = FALSE) %>%
    hc_subtitle(text = title, y = 25, style = list(fontSize = "12px")) %>%
    hc_yAxis(min = 0, max = max_value, lineWidth = 0, minorTickWidth = 0,
             tickPositions = list(0, max_value),
             labels = list(y = 15, distance = 0), showLastLabel = TRUE) %>%
    hc_pane(startAngle = -120, endAngle = 120, size = "100%",
            background = list(outerRadius = '100%', innerRadius = '80%',
                              shape = 'arc')) %>%
    hc_plotOptions(solidgauge = list(innerRadius = '80%',
                                     dataLabels = list(y = -20,
                                                       borderWidth = 0,
                                                       useHTML = TRUE, 
                                                       style = list(
                                                         fontSize = '12px')
                                     ))) %>%
    hc_colors(color = color) %>%
    hc_tooltip(enabled = FALSE) %>%
    hc_add_theme(my_hc_theme)
}

## Scripts ----------------------

# Javascript function to delay transitions in conditional panels
js <- "
$(document).ready(function(){
  $('.conditional_panel').on('show', function(){
    $(this).css('opacity', 0).delay(500).animate({opacity: 1}, 
                                                  {duration: 500});
  });
});
"