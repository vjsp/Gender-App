############## Datasets treatment ##############
# @author Víctor Julio Sánchez Pollo           #
# @version 25/09/2021                          #
################################################

###=============== Libraries ===============###

# Used repositories
default_repos = "http://cran.us.r-project.org"
# Libraries installation and loading
if(!require(dplyr)) install.packages("dplyr", repos = default_repos)
if(!require(geojsonio)) install.packages("geojsonio", repos = default_repos)
if(!require(openxlsx)) install.packages("openxlsx", repos = default_repos)
if(!require(tidyr)) install.packages("tidyr", repos = default_repos)



###=============== Variables ===============###

# Path to GEI data original excel file
gei_file_path <- "./data/Gender_Equality_Index.xlsx"
# Path to world geodata json file
geo_file_path <- "./data/world.geo.json"
# Mapping beetween data and GEI years
year_mapping <- list("2010" = 2013,
                     "2012" = 2015,
                     "2015" = 2017,
                     "2017" = 2019,
                     "2018" = 2020)
# Countries code mapping
country_code_mapping <- list("UK" = "GB",
                             "EL" = "GR")
# Additional EU regions
eu_regions_df <- data.frame("Country code" = "EU28",
                            Country = "European Union 28",
                            check.names = FALSE)



###=============== Functions ===============###

# Function to read all sheets from excel file
# @param xlsx_file - File with .xlsx extension
# @return A list of data frames
read_all_sheets <- function(xlsx_file, ...) {
  sheet_names <- openxlsx::getSheetNames(xlsx_file)
  sheet_list <- as.list(rep(NA, length(sheet_names)))
  names(sheet_list) <- sheet_names
  for (sn in sheet_names) {
    sheet_list[[sn]] <- openxlsx::read.xlsx(xlsx_file, sheet = sn, ...)
  }
  sheet_list
}



###================== Main ==================###

## File reading
gei_file_data <- read_all_sheets(gei_file_path, sep.names = " ",
                                 fillMergedCells = TRUE)
world_geo_data <- geojson_read("data/world.geo.json", what = "sp")


## Countries dataframe building
countries_df <- data.frame("Country code" = world_geo_data$iso_a2,
                           Country = world_geo_data$name,
                           check.names = FALSE) %>%
  rbind(eu_regions_df)
  

## GEI metadata dataframe building
gei_metadata_df <- data.frame(gei_file_data["Metadata"]) %>%
  # Remove unnecessary columns and duplicates
  `colnames<-`(paste0("X", 1:ncol(.))) %>%
  select(c(0:5)) %>%
  unique() %>%
  group_by(X1, X2, X3, X4) %>%
  mutate(X5 = paste0(X5, collapse = " ")) %>%
  unique() %>%
  # Set column names and remove rows with unnecessary info (header and
  # additional variable)
  `colnames<-`(.[1,]) %>%
  ungroup() %>%
  slice(-c(1,nrow(.))) %>%
  rename(Indicator = "Indicator and reference population") %>%
  # Remove leading a trailing whitespaces
  mutate_if(is.character, funs(gsub("(^\\s+)|(\\s+$)", "", .))) %>%
  `colnames<-`(gsub("(^\\s+)|(\\s+$)", "", names(.)))
# Save df as RDS
saveRDS(gei_metadata_df, file = "data/GEI_metadata.rds")


## GEI data dataframe building
gei_data_df <- gei_file_data[-which(names(gei_file_data) == "Metadata")] %>%
  # Put all years data into a single df
  bind_rows(.id = "Year") %>%
  # Replace data years with GEI years and align country code with ISO
  mutate(Year = unlist(year_mapping[Year]),
         Country = ifelse(Country %in% names(country_code_mapping),
                          unlist(country_code_mapping[Country] %>% 
                                   replace(.=="NULL", NA)),
                          Country)) %>%
  rename("Country code" = Country) %>%
  # Add country names
  merge(countries_df, by = "Country code") %>%
  # Reorder columns to keep together same year data
  relocate("Year", .before = "Country code") %>%
  relocate("Country", .after = "Country code") %>%
  # Reorder rows by Year and Country (alphabetically but keeping EU28 as first
  # of them)
  mutate(Country = {
    factor(Country) %>% 
      relevel(Country, ref = "European Union 28")
  }) %>%
  arrange_at(c("Year","Country"))
# Save df as RDS
saveRDS(gei_data_df, file = "data/GEI_data.rds")
           

## GEI indicators dataframe building
# Create a df with all levels indicators from metadata
gei_indicators_df <- gei_metadata_df %>%
  ungroup() %>%
  select(Domain, Subdomain, Indicator)
indicators <- NULL
for (i in 1:nrow(gei_indicators_df)) {
  indicators <- c(indicators, gei_indicators_df[i, ])
}
gei_indicators_df <- data.frame(Type = names(indicators),
                                Indicator = unlist(indicators)) %>%
  unique() %>%
  add_row(Type = "Index", Indicator = "Gender Equality Index", .before = 1)

# Add Id and Parent Id columns
id_column <- NULL
parent_id_column <- NULL
id <- NULL
parent_id <- NULL
d_counter <- 0
s_counter <- 0
i_counter <- 0
for (i in 1:nrow(gei_indicators_df)) {
  switch(gei_indicators_df[i, "Type"],
         "Index" = {
           id <- "GEI"
           parent_id <- NA_character_
         },
         "Domain" = {
           d_counter <- d_counter + 1
           s_counter <- 0
           i_counter <- 0
           id <- paste0("D", d_counter)
           parent_id <- "GEI"
         },
         "Subdomain" = {
           s_counter <- s_counter + 1
           i_counter <- 0
           id <- paste0("S", d_counter, s_counter)
           parent_id <- paste0("D", d_counter)
         },
         "Indicator" = {
           i_counter <- i_counter + 1
           id <- paste0("I", d_counter, s_counter, i_counter)
           parent_id <- paste0("S", d_counter, s_counter)
         }
  )
  id_column <- c(id_column, id)
  parent_id_column <- c(parent_id_column, parent_id)
}
gei_indicators_df %<>% mutate(Id = id_column,
                            "Parent Id" = parent_id_column)

# Include gender metrics
for (i in 1:nrow(gei_indicators_df)) {
  if(gei_indicators_df[i, "Type"] == "Indicator") {
    gei_indicators_df <- gei_indicators_df %>%
      add_row(Type = "Metric",
              Indicator = paste0(gei_indicators_df[i, "Indicator"], " W"),
              Id = gsub("I", "M", paste0(gei_indicators_df[i, "Id"], "W")),
              "Parent Id" = gei_indicators_df[i, "Id"]) %>%
      add_row(Type = "Metric",
              Indicator = paste0(gei_indicators_df[i, "Indicator"], " M"),
              Id = gsub("I", "M", paste0(gei_indicators_df[i, "Id"], "M")),
              "Parent Id" = gei_indicators_df[i, "Id"]) %>%
      add_row(Type = "Metric",
              Indicator = paste0(gei_indicators_df[i, "Indicator"], " T"),
              Id = gsub("I", "M", paste0(gei_indicators_df[i, "Id"], "T")),
              "Parent Id" = gei_indicators_df[i, "Id"])
  }
}

# Include short description from data
gei_indicators_df %<>% 
  mutate("Indicator (s)" = names(gei_data_df[,4:(ncol(gei_data_df)-6)]))

# Save df as RDS
saveRDS(gei_indicators_df, file = "data/GEI_indicators.rds")
