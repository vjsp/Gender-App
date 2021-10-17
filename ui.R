##################### ui.R #####################
# @author Víctor Julio Sánchez Pollo           #
# @version 17/10/2021                          #
################################################

dashboardPage(
  dashboardHeader(title = "Gender Equality App",
                  dropdownMenuOutput("info_menu"),
                  dropdownMenuOutput("credits_menu")),
  dashboardSidebar(
    sidebarMenu(id = "sidebar_menu",
      menuItem("Map Explorer", tabName = "map", icon = icon("globe")),
      conditionalPanel("input.sidebar_menu == 'map'",
        class = "sidebar_conditional_panel",
        sliderTextInput("map_year",
          label = h5("Select year"),
          choices = gei_years,
          selected = current_year,
          animate = animationOptions(interval = 3000, loop = FALSE)
        )
      ),
      menuItem("Trend Explorer", tabName = "trend", icon = icon("chart-line")),
      conditionalPanel("input.sidebar_menu == 'trend'",
        class = "sidebar_conditional_panel",
        uiOutput("trend_years_slider"),
        uiOutput("trend_countries_picker")
      ),
      menuItem("Country Explorer", tabName = "country", icon = icon("flag")),
      conditionalPanel("input.sidebar_menu == 'country'",
        class = "sidebar_conditional_panel",
        uiOutput("country_country_picker"),
        uiOutput("country_year_slider")
      ),
      menuItem("Comparator", tabName = "comparator",
               icon = icon("balance-scale")),
      menuItem("Data Explorer", tabName = "data", icon = icon("folder-open")),
      conditionalPanel("input.sidebar_menu == 'data' &&
                       input.data_tabset_panel == 'Data table'",
        class = "sidebar_conditional_panel",
        uiOutput("data_years_picker"),
        uiOutput("data_countries_picker")
      ),
      menuItem("About", tabName = "about", icon = icon("book")),
      div(
        id = "legend_container",
        box(
          width = 12, title = "Legend", solidHeader = TRUE,
          p(class = "gei_option", "Gender Equality Index"),
          p(class = "domain_option", "Domain"),
          p(class = "subdomain_option", "Subdomain"),
          p(class = "indicator_option", "Indicator")
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
        div(id = "map_outer", class = "outer", height = "100%",
          leafletOutput("my_map"),
          absolutePanel(id = "map_indicator_panel",
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
            top = 100,
            right = 0,
            width = "30%",
            draggable = TRUE,
            div(id = "eu_value_container",
              actionButton("eu_button", "", icon = icon("globe-europe")),
              htmlOutput("eu_html_value")
            ),
            div(id = "ranking_container",
              column(class = "ranking", width = 6,
                div(class = "rank_title", "Top 3"),
                formattableOutput("top_3_table")
              ),
              column(class = "ranking", width = 6,
                div(class = "rank_title", "Bottom 3"),
                formattableOutput("bottom_3_table")
              )
            ),
            conditionalPanel(condition = "input.indicator == 'GEI'",
              class = "conditional_panel",
              highchartOutput("domains_chart", height = "230")
            ),
            conditionalPanel(condition = "input.indicator.startsWith('D')",
              class = "conditional_panel",
              highchartOutput("subdomains_chart", height = "230")
            ),
            conditionalPanel(condition = "input.indicator.startsWith('S')",
              class = "conditional_panel",
              highchartOutput("indicators_chart", height = "230")
            ),
            conditionalPanel(condition = "input.indicator.startsWith('I')",
              id = "metric_container", class = "conditional_panel",
              div(id = "metrics_charts", width = 12, height = "230",
                div(id = "country_value_container",
                    htmlOutput("country_html_value")
                ),
                div(id = "metrics_charts_container",
                  highchartOutput("total_chart", height = "180"),
                  highchartOutput("women_chart", height = "180"),
                  highchartOutput("men_chart", height = "180")
                )
              )
            )
          )
        ),
      ),
      # Trend Explorer content
      tabItem(tabName = "trend",
        div(id = "trend_outer", class = "outer", height = "100%",
          fluidRow(
            div(id = "trend_indicator_panel",
              draggable = TRUE,
              selectInput(inputId = "trend_indicator",
                label = NULL,
                choices = setNames(gei_indicators$Id,
                                   gei_indicators$`Indicator (s)`),
                selected = "GEI",
                width = "80%"
              )
            )
          ),
          fluidRow(id = "trend_tabs_row", class = "tabs_row",
            tabsetPanel(id = "trend_tabset_panel",
              tabPanel("Trend graph",  highchartOutput("trend_chart")),
              tabPanel("Scores table", reactableOutput("trend_score_table")),
              tabPanel("Variations table", reactableOutput("trend_var_table"))
            )
          )
        )
      ),
      # Country Explorer content
      tabItem(tabName = "country",
        fluidRow(id = "country_explorer_box_container",
          box(
            width = 12,
            fluidRow(
              column(width = 4,
                htmlOutput("country_html_name"),
                highchartOutput("country_trend_chart", height = 200)
              ),
              column(width = 4,
                htmlOutput("country_domain_html_title"),
                highchartOutput("country_domain_chart", height = 120),
                textOutput("country_domain_ranking_text")
              ),
              column(width = 4,
                htmlOutput("country_domain_html_detail"),
              )
            ),
            fluidRow(
              reactableOutput("country_trend_table")
            )
          )
        )
      ),
      # Country Comparator content
      tabItem(tabName = "comparator",
        fluidRow(id = "gei_comparator_container",
          box(width = 12,
            column(id = "first_country_panel",
              class = "country_comparator_panel",
              width = 4,
              highchartOutput("first_gei_chart", height = 120),
              selectInput(inputId = "comp_first_country",
                label = NULL,
                choices = unique(levels(gei_data$Country)),
                selected = "Spain"
              ),
              selectInput(inputId = "comp_first_year",
                label = NULL,
                choices = gei_years,
                selected = current_year
              ),
              height = 250
            ),
            column(width = 4, minWidth = 220,
              highchartOutput("comp_domains_chart", height = 240)
            ),
            column(id = "second_country_panel",
              class = "country_comparator_panel",
              width = 4,
              highchartOutput("second_gei_chart", height = 120),
              selectInput(inputId = "comp_second_country",
                label = NULL,
                choices = unique(levels(gei_data$Country)),
                selected = "European Union 28"
              ),
              selectInput(inputId = "comp_second_year",
                label = NULL,
                choices = gei_years,
                selected = current_year
              )
            )
          )
        ),
        fluidRow(class = "domains_buttons_container",
          radioGroupButtons(
            inputId = "comp_domain",
            choices = gei_indicators %>%
              filter(Type == "Domain") %>%
              pull("Indicator (s)"),
            justified = TRUE
          )
        ),
        fluidRow(id = "domain_comparator_container",
          box(width = 12,
            column(id = "domain_charts_panel", width = 2,
              fluidRow(
                highchartOutput("first_domain_chart", height = 100),
              ),
              fluidRow(
                htmlOutput("domain_diff_html_value")
              ),
              fluidRow(
                highchartOutput("second_domain_chart", height = 100),
              )
            ),
            column(width = 4,
              highchartOutput("comp_subdomains_chart", height = 250)
            ),
            column(width = 6,
              fluidRow(
                htmlOutput("subdomain_html_name")
              ),
              fluidRow(
                div(id = "subdomain_charts_container",
                  highchartOutput("first_subdomain_chart", height = 80),
                  htmlOutput("subdomain_diff_html_value", height = 80),
                  highchartOutput("second_subdomain_chart", height = 80)
                )
              ),
              fluidRow(
                reactableOutput("comp_indicators_table")
              )
            )
          )
        )
      ),
      # Data Explorer content
      tabItem(tabName = "data",
        fluidRow(
          div(id = "download_panel",
              radioGroupButtons(
                inputId = "download_type",
                choices = c("CSV", "Excel"),
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
              ),
              downloadButton("download_data")
          )
        ),
        fluidRow(class = "tabs_row",
          tabsetPanel(id = "data_tabset_panel",
            tabPanel("Data table", reactableOutput("data_table")),
            tabPanel("Metadata table", reactableOutput("metadata_table"))
          )
        )
      ),
      # About content
      tabItem(tabName = "about",
        fluidRow(id = "gei_info_container",
          box(width = 12,
            column(width = 9,
              htmlOutput("gei_info_html_text")
            ),
            column(id = "value_boxes_section", width = 3,
                box(width = 12,
                    div(class = "box_value", 28),
                    div(class = "box_subtitle", "countries")
                ),
                box(width = 12,
                    div(class = "box_value", 6),
                    div(class = "box_subtitle", "domains")
                ),
                box(width = 12,
                    div(class = "box_value", 31),
                    div(class = "box_subtitle", "indicators")
                ),
                box(width = 12,
                    div(class = "box_value", 5),
                    div(class = "box_subtitle", "editions")
                )
            )
          )
        ),
        fluidRow(class = "domains_buttons_container",
          radioGroupButtons(
            inputId = "info_domain",
            choices = gei_indicators %>%
              filter(Type == "Domain") %>%
              pull("Indicator (s)"),
            justified = TRUE
          )
        ),
        fluidRow(id = "domain_info_container",
          box(width = 12,
            column(id = "domain_info_section", width = 4,
              htmlOutput("domain_info_html_text"),
              # selectInput(inputId = "info_subdomain",
              #             label = NULL,
              #             choices = NULL
              # ),
              radioGroupButtons(inputId = "info_subdomain",
                                label = NULL,
                                choices = "",
                                direction = "vertical"
              ),
              uiOutput("domain_select_style")
            ),
            column(width = 8,
              reactableOutput("info_indicators_metadata_table")
            )
          )
        )
      )
    )
  ),
  skin = "purple"
)
