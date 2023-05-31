## outbreak table

## only rerun if we want to recreate the final datasets 
REGENERATE_DATA <- FALSE

if(REGENERATE_DATA) source("R/marburg/script_marburg.R") 

source("R/outbreak_table.R")

df <- read.csv("data/marburg/final/outbreak_final.csv")
article_df <- read.csv("data/marburg/final/article_final.csv")

# merge with article ID to get the y-axis labels
df <- article_df %>% dplyr::select(article_id,first_author_first_name,year_publication) %>%
  dplyr::right_join(df, by = "article_id") %>%
  mutate(article_label = as.character(paste0(first_author_first_name," ",year_publication)),
         outbreak_start_month = chartr(old = "-", new = " ", x = outbreak_start_month),
         outbreak_end_month = chartr(old = "-", new = " ", x = outbreak_end_month),
         cases_mode_detection = if_else(cases_mode_detection == "Molecular (PCR etc)", 
                                        "Molecular", cases_mode_detection),
         outbreak_country = dplyr::if_else(outbreak_country == "Congo, Dem. Rep.", 
                                           "Democratic Republic of the Congo",
                                           outbreak_country),
         outbreak_location = dplyr::if_else(outbreak_location == "Marburg (23), Frankfurt am Main (6), Belgrade (2)",
                                            "Marburg, Frankfurt am Main, Belgrade",
                                            outbreak_location)) %>%
  dplyr::arrange(outbreak_country, outbreak_start_year) 

outbreak_table(df,"marburg")
