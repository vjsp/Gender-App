################### global.R ###################
# @author Víctor Julio Sánchez Pollo           #
# @version 25/09/2021                          #
################################################

###=============== Libraries ===============###

# Used repositories
default_repos = "http://cran.us.r-project.org"
# Libraries installation and loading
if(!require(dplyr)) install.packages("dplyr", repos = default_repos)
if(!require(formattable)) install.packages("formattable",repos = default_repos)
if(!require(geojsonio)) install.packages("geojsonio", repos = default_repos)
if(!require(highcharter)) install.packages("highcharter", repos = default_repos)
if(!require(leaflet)) install.packages("leaflet", repos = defualt_repos)
if(!require(magrittr)) install.packages("magrittr", repos = default_repos)
if(!require(openxlsx)) install.packages("openxlsx", repos = default_repos)
if(!require(reactable)) install.packages("reactable", repos = default_repos)
if(!require(shiny)) install.packages("shiny", repos = default_repos)
if(!require(shinydashboard)) install.packages("shinydashboard", repos = default_repos)
if(!require(shinyjs)) install.packages("shinyjs", repos = default_repos)
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = default_repos)
if(!require(sparkline)) install.packages("sparkline", repos = default_repos)
if(!require(tidyr)) install.packages("tidyr", repos = default_repos)


###=============== Input data ===============###

geo_data <- geojson_read("data/world.geo.json", what = "sp")
gei_data <- readRDS(file = "data/GEI_data.rds")
gei_metadata <- readRDS(file = "data/GEI_metadata.rds")
gei_full_indicators <- readRDS(file = "data/GEI_indicators.rds")


###========== Modified data frames ==========###

# GEI indicators without the gender detail metrics
gei_indicators <- gei_full_indicators %>% filter(Type != "Metric")

# GEI data with all data rounded out to 2 decimals
gei_data <- gei_data %>% mutate_if(is.numeric, round, digits = 2)


###====== Variables and initial values ======###

# Vector with the available years
gei_years <- sort(unique(gei_data$Year))

# Current (latest) year
current_year <- max(gei_years)

# Default year range (it is defined to the full range)
default_year_range <- c(min(gei_years), max(gei_years))

# Countries preceded by "the"
countries_with_the <- c("European Union 28", "Czech Rep.", "Netherlands",
                        "United Kingdom")


###============ Info html texts ============###

gei_html_text <- "<img id = 'gei_info_image' 
  src = 'images/Gender_Equality_Index_domains.png'
  alt = 'Gender Equality Index'>
  The <span id='gei_info_name'>Gender Equality Index</span> is an important
  policy-making tool to measure the progress of gender equality in the European
  Union over time. It is developed by the <a href = 'https://eige.europa.eu'
  target = '_blank'>European Institute for Gender Equality (EIGE)</a>. Each
  year, it gives the EU and the Member States (including the United Kingdom) a
  score from 1 to 100. A score of 100 would mean that a country had reached full
  equality between women and men.
  <br/>The scores are based on the gaps between women and men and levels of
  achievement in six core domains: work, money, knowledge, time, power and 
  health, and their sub-domains; which are analyzed through a complete set of
  key indicators.
  <br/>The Index gives visibility to areas that need improvement and supports
  policy makers to design more effective gender equality measures.
  <br/>Since the first edition in 2013, the Gender Equality Index has tracked
  and reported progress by providing a comprehensive measure of gender equality,
  tailored to fit the EU’s policy goals. It reveals both progress and setbacks,
  and explores what can be done better to seize opportunities for change.
  <br/>"

work_html_text <- "The domain of <span class = 'domain_info_name' 
  style = 'color:%s'>work</span> measures the extent to which women and men can
  benefit from equal access to employment and good working conditions.
  <br/> Its score is determined through five indicators grouped into two
  subdomains:"

money_html_text <- "The domain of <span class = 'domain_info_name'
  style = 'color:%s'>money</span> measures gender inequalities in access to
  financial resources and women’s and men’s economic situation.
  <br/> Its score is determined through four indicators grouped into two
  subdomains:"

knowledge_html_text <- "The domain of <span class = 'domain_info_name'
  style = 'color:%s'>knowledge</span> measures gender inequalities in
  educational attainment, participation in education and training over the life
  course and gender segregation.
  <br/> Its score is determined through three indicators grouped into two
  subdomains:"

time_html_text <- "The domain of <span class = 'domain_info_name'
  style = 'color:%s'>time</span> measures gender inequalities in allocation of
  time spent doing care and domestic work and social activities.
  <br/> Its score is determined through four indicators grouped into two
  subdomains:"

power_html_text <- "The domain of <span class = 'domain_info_name'
  style = 'color:%s'>power</span> measures gender equality in decision-making
  positions across the political, economic and social spheres.
  <br/> Its score is determined through eight indicators grouped into three
  subdomains:"

health_html_text <- "The domain of <span class = 'domain_info_name'
  style = 'color:%s'>health</span> measures gender equality in three health
  related aspects: health status, health behaviour and access to health
  services. 
  <br/> Its score is determined through seven indicators grouped into three
  subdomains:"


###=========== Colors & Palettes ===========###

gei_color <- rgb(149, 75, 146, maxColorValue = 255)
first_country_color <- rgb(0, 74, 153, maxColorValue = 255);
second_country_color <- rgb(206, 153, 0, maxColorValue = 255);
default_green <- rgb(46, 182, 44, maxColorValue = 255)
default_red <- rgb(220, 28, 19, maxColorValue = 255)
work_color <- rgb(52, 150, 176, maxColorValue = 255)
money_color <- rgb(230, 160, 0, maxColorValue = 255)
knowledge_color <- rgb(91, 155, 105, maxColorValue = 255)
time_color <- rgb(225, 108, 66, maxColorValue = 255)
power_color <- rgb(232, 83, 79, maxColorValue = 255)
health_color <- rgb(190, 117, 148, maxColorValue = 255)

# Scores color palette
# It takes into account all possible values (0-100)
my_pal <- colorNumeric(palette = "viridis",
                       domain = c(0,100),
                       na.color = "transparent")

# Trend (growth) color palette
# It takes into account different ranges for positive and negative values
trend_pal <- function(value = NA) {
  # Define color values
  high_green <- rgb(46, 182, 44, maxColorValue = 255)
  low_green <- rgb(131, 212, 117, maxColorValue = 255)
  blue <- rgb(0, 0, 240, maxColorValue = 255)
  low_red <- rgb(240, 116, 11, maxColorValue = 255)
  high_red <- rgb(220, 28, 19, maxColorValue = 255)
  
  # Define auxiliary palettes
  green_palette <- colorRampPalette(colors = c(low_green, high_green),
                                    space = "rgb")(100)
  red_palette <- colorRampPalette(colors = c(high_red, low_red),
                                  space = "rgb")(100)

  # Set different colors for positive and negative value defining limits and
  # a special case for 0
  ifelse(value >= 20,
    high_green,
    ifelse((value > 0) & (value < 20),
      colorNumeric(palette = green_palette, domain = c(0.01,20))(value),
      ifelse(value == 0,
        blue,
        ifelse((value > -20) & (value < 0),
          colorNumeric(palette = red_palette, domain = c(-20,-0.01))(value),
          ifelse(value <= -20,
            high_red,
            "gray"
          )
        )
      )
    )
  )
}

# GEI-Domain color mapping (include both, GEI and domains)
gei_domain_color_mapping <- c("Gender Equality Index" = gei_color,
                              "WORK" = work_color,
                              "MONEY" = money_color,
                              "KNOWLEDGE" = knowledge_color,
                              "TIME" = time_color,
                              "POWER" = power_color,
                              "HEALTH" = health_color)


###=========== Highcharter themes ===========###

# Highcharter theme used to change the default font
my_hc_theme <- hc_theme(
  chart = list(
    style = list(fontFamily = "Source Sans Pro")
  )
)


###============ Reactable themes ============###
center_rt_theme <- reactableTheme(
  # Vertically center cells
  headerStyle = list(display = "flex", flexDirection = "column",
                     justifyContent = "center"),
  cellStyle = list(display = "flex", flexDirection = "column",
                   justifyContent = "center")
)


###========== Auxiliary functions ==========###

# Function to use the color palette with formattable
# @return A formatter using the color palette to style the text
format_color_formattable <- function (...) {
  formatter("span", style = function(x) {
    style(color = my_pal(x))
  })
}

# Function to round up a number taking into account the number of digits
# @param x - The number to be rounded up
# @param digits - The number of digits
# @return The rounded up number
ceiling_digits <- function(x, digits) {
  round(x + 5 * 10^(-digits -1), digits)
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
             unlist(use.names = FALSE))
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
create_gender_gauge <- function(data, max_value, title, color) {
  highchart() %>%
    hc_chart(type = "solidgauge") %>%
    hc_add_series(data, showInLegend = FALSE) %>%
    hc_subtitle(text = title, y = 25, style = list(fontSize = "12px")) %>%
    hc_yAxis(min = 0, max = max_value, lineWidth = 0, minorTickWidth = 0,
             tickPositions = list(0, max_value),
             labels = list(y = 15, distance = 0), showLastLabel = TRUE) %>%
    hc_pane(startAngle = -120, endAngle = 120, size = "100%",
            background = list(outerRadius = "100%", innerRadius = "80%",
                              shape = "arc")) %>%
    hc_plotOptions(solidgauge = list(
      innerRadius = "80%",
      dataLabels = list(y = -20, borderWidth = 0, useHTML = TRUE,
                        style = list(fontSize = "12px"))
    )) %>%
    hc_colors(color = color) %>%
    hc_tooltip(enabled = FALSE) %>%
    hc_add_theme(my_hc_theme)
}

# Function to create the gauge charts for indicators values
# @param data - The value (number) to be represented
# @param name - The serie's name
# @param color - The color used to fill the chart
# @param star - Boolean that indicates if a star must be shown next to the value
# @param value_size - A valid font-size for the value label
# @param y_label - The y position offset of the label in pixels
# @return A gauge chart
create_indicator_gauge <- function(data, name, color, star, value_size, y_label) {
  highchart() %>%
    hc_chart(type = "solidgauge", backgroundColor = "transparent") %>%
    hc_add_series(data, name = name, showInLegend = FALSE) %>%
    hc_yAxis(min = 0, max = 100, tickWidth = 0, minorTicks = FALSE,
             labels = list(enabled = FALSE)) %>%
    hc_pane(size = "100%",
            background = list(outerRadius = "100%", innerRadius = "75%",
                              shape = "arc")) %>%
    hc_plotOptions(solidgauge = list(
      innerRadius = "75%",
      dataLabels = list(y = y_label, borderWidth = 0, useHTML = TRUE,
                        format =  paste0("{point.y}",
                                         ifelse(star,
                                                " \u002A",
                                                "")),
                        style = list(fontSize = value_size,
                                     color = my_pal(data)))
    )) %>%
    hc_colors(color = color) %>%
    hc_tooltip(outside = TRUE) %>%
    hc_add_theme(my_hc_theme)
}

# Function to adapt the selector style taking into account the selected
# indicator
# @param input_object Select input object
# @param indicator_panel_id Indicator's panel HTML Id
set_selector_style <- function(input_object, indicator_panel_id) {
  if (input_object == 'GEI') {
    runjs(paste0('$("',indicator_panel_id, ' .selectize-input").
        addClass("gei_option")'))
    runjs(paste0('$("',indicator_panel_id, ' .selectize-input").
        removeClass("domain_option subdomain_option indicator_option")'))
  } else if (substr(input_object,1,1) == 'D'){
    runjs(paste0('$("',indicator_panel_id, ' .selectize-input").
        addClass("domain_option")'))
    runjs(paste0('$("',indicator_panel_id, ' .selectize-input").
        removeClass("gei_option subdomain_option indicator_option")'))
  } else if (substr(input_object,1,1) == 'S'){
    runjs(paste0('$("',indicator_panel_id, ' .selectize-input").
        addClass("subdomain_option")'))
    runjs(paste0('$("',indicator_panel_id, ' .selectize-input").
        removeClass("gei_option domain_option indicator_option")'))
  } else if (substr(input_object,1,1) == 'I'){
    runjs(paste0('$("',indicator_panel_id, ' .selectize-input").
        addClass("indicator_option")'))
    runjs(paste0('$("',indicator_panel_id, ' .selectize-input").
        removeClass("gei_option domain_option subdomain_option")'))
  } else {
    runjs(paste0('$("',indicator_panel_id, ' .selectize-input").
        removeClass(
          "gei_option domain_option subdomain_option indicator_option"
        )'))
  }
}

# Function to obtain a color with a defined level of opacity (alpha)
# @param color - The base color
# @param alpha - The alpha (opacity) value ([0,1]). Default: 1
# @return A rgba color
apply_alpha_to_color <- function(color, alpha = 1) {
  # "If" condition is used to avoid errors when "color" is empty
  if (length(color) != 0) {
    rgb_color <- col2rgb(color)
    rgb(rgb_color[1],rgb_color[2],rgb_color[3], alpha * 255, maxColorValue = 255)
}}

# Function to transform a cardinal into an ordinal number
# In case of passing a decimal number, this is rounded out
# @param number - The number to be transformed into an ordinal
# @return A string with the ordinal number in short format (i.e. 1st)
number_to_ordinal <- function(number) {
  rounded_number <- round(number)
  ifelse((rounded_number %% 10 == 1) & (rounded_number %% 100 != 11),
    paste0(rounded_number, "st"),
    ifelse((rounded_number %% 10 == 2) & (rounded_number %% 100 != 12),
      paste0(rounded_number, "nd"),
      ifelse((rounded_number %% 10 == 3) & (rounded_number %% 100 != 13),
        paste0(rounded_number, "rd"),
        paste0(rounded_number, "th")
      )
    )
  )
} 

###=============== JS scripts ===============###

# Javascript function to apply some styles and animations
js <- "
$(document).ready(function(){
  // Delay transitions in map_graphs conditional panels
  $('#map_graphs .conditional_panel').on('show', function(){
    $(this).css('opacity', 0).delay(500).animate({opacity: 1}, 
                                                  {duration: 500});
  });
  
  // Top to bottom slide effect on sidebars conditional panels
  $('.sidebar_conditional_panel').on('show', function(){
    $('.sidebar_conditional_panel').removeClass('top_to_bottom');
    $('.sidebar_conditional_panel').css('overflow', 'hidden');
    $(this).addClass('top_to_bottom');
    setTimeout(function() {
      $('.sidebar_conditional_panel').css('overflow', 'visible');
    }, 1000);
  });
  
  // Color styling in domains buttons
  $('.domains_buttons_container [value=\"WORK\"]').parent().
    addClass('work_button');
  $('.domains_buttons_container [value=\"MONEY\"]').parent().
    addClass('money_button');
  $('.domains_buttons_container [value=\"KNOWLEDGE\"]').parent().
    addClass('knowledge_button');
  $('.domains_buttons_container [value=\"TIME\"]').parent().
    addClass('time_button');
  $('.domains_buttons_container [value=\"POWER\"]').parent().
    addClass('power_button');
  $('.domains_buttons_container [value=\"HEALTH\"]').parent().
    addClass('health_button');
});
"