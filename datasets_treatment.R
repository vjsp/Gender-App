# Datasets treatment

# Libraries
library(openxlsx)
library(dplyr)
library(tidyr)

# Read GEI original metadata from file
file_path <- "./data/Gender_Equality_Index.xlsx"
orig_metadata_df <- read.xlsx(xlsxFile = file_path, fillMergedCells = TRUE,
                  colNames = FALSE, startRow = 3)

# Build GEI metadata dataframe
gei_metadata_df <- orig_metadata_df[0:5] %>%
  unique() %>%
  group_by(X1, X2, X3, X4) %>%
  mutate(X5 = paste0(X5, collapse = "")) %>%
  unique()
names(gei_metadata_df) <- gei_metadata_df[1,]
gei_metadata_df <- gei_metadata_df[-1,]
gei_metadata_df <- rename(gei_metadata_df,
                              Indicator = "Indicator and reference population") 
  
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
rownames(gei_indicators_df) <- NULL
