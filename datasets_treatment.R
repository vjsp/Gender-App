## Datasets treatment ##

## Libraries
library(openxlsx)
library(dplyr)
library(tidyr)

## Variables
file_path <- "./data/Gender_Equality_Index.xlsx"
# Mapping beetween data and GEI years
year_mapping = list('2010' = 2013,
                    '2012' = 2015,
                    '2015' = 2017,
                    '2017' = 2019,
                    '2018' = 2020)

## Functions
# Function to read all sheets from excel file
# @param xlsxFile - File with .xlsx extension
# @return A list of dataframes
read_all_sheets = function(xlsxFile, ...) {
  sheet_names = openxlsx::getSheetNames(xlsxFile)
  sheet_list = as.list(rep(NA, length(sheet_names)))
  names(sheet_list) = sheet_names
  for (sn in sheet_names) {
    sheet_list[[sn]] = openxlsx::read.xlsx(xlsxFile, sheet=sn, ...)
  }
  return(sheet_list)
}

## Main
# Read excel file
gei_file_data <- read_all_sheets(file_path, sep.names = " ",
                                 fillMergedCells = TRUE)

# Build GEI metadata dataframe
orig_metadata_df <- data.frame(gei_file_data['Metadata'])
colnames(orig_metadata_df) <- paste0("X",1:ncol(orig_metadata_df))
gei_metadata_df <- orig_metadata_df[0:5] %>%
  unique() %>%
  group_by(X1, X2, X3, X4) %>%
  mutate(X5 = paste0(X5, collapse = "")) %>%
  unique()
names(gei_metadata_df) <- gei_metadata_df[1,]
gei_metadata_df <- gei_metadata_df[-1,]
gei_metadata_df <- rename(gei_metadata_df,
                          Indicator = "Indicator and reference population")

# Build GEI data dataframe
gei_data <- gei_file_data[-which(names(gei_file_data) == "Metadata")]
gei_data_df <- bind_rows(gei_data, .id = "Year")
gei_data_df$Year <- year_mapping[gei_data_df$Year]
  
# Build GEI indicators list
gei_indicators_df <- gei_metadata_df %>%
  ungroup() %>%
  select('Domain', 'Subdomain', 'Indicator')
indicators <- NULL
for (i in 1:nrow(gei_indicators_df)) {
  indicators <- c(indicators,gei_indicators_df[i,])
}
gei_indicators_df <- data.frame(Type = names(indicators),
                                Indicator = unlist(indicators)) %>%
  unique()
rownames(gei_indicators_df) <- NULL #Regenerate index

