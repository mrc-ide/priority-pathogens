# Functions for cleaning data 
library(dplyr)
library(janitor)
library(rio)
library(tidyr)

# import_df <- function(pathogen){
#   import(paste0('data/', pathogen, "/", pathogen, "_article.xlsx"))
# }


clean_dfs <- function(df, column_name){
  #' input: data frame to clean, representative column that indicates no data in quotations
  #'  article: column_name = article_title
  #'  parameter: column_name = parameter_type
  #'  model: column_name = model_type
  #'  outbreak: column_name = outbreak_country
  #' process: clean names, drop rows with NAs, adds a grouping variable for classification of the parameter type
  #' output: df 
  df <- df %>%
    clean_names() %>%
    dplyr::filter(!is.na(column_name)) 
  
  if('parameter_type' %in% colnames(df)){
    df <- df %>%
      mutate(parameter_class = case_when(grepl('Human delay', parameter_type) ~ 'Human delay',
                                         grepl('Seroprevalence', parameter_type) ~ 'Seroprevalence',
                                         grepl('Mutations', parameter_type) ~ 'Mutations',
                                         grepl('Risk factors', parameter_type) ~ 'Risk factors',
                                         grepl('Reproduction number', parameter_type) ~ 'Reproduction number',
                                         grepl('Severity', parameter_type) ~ 'Severity',
                                         grepl('Mosquito', parameter_type) ~ "Mosquito",
                                         grepl('Relative', parameter_type) ~ 'Relative contribution',
                                         TRUE ~ 'Other'))
  }
  
  return(df)
}

get_details <- function(article_df){
  #' input: article data frame
  #' process: pulls out the covidence ID, article ID, and name of extractor and if duplicated
  #' output: df with 4 columns
  
  df <- article_df %>%
    select(c(article_id, covidence_id, name_data_entry)) %>%
    mutate(double_extracted = ifelse(duplicated(covidence_id) & !duplicated(article_id), 1, 0))
  
  return(df)
}

get_double_extracted <- function(df, detail){
  #' input: data frame
  #' process: gets double extracted articles and identifies if matching
  #' output: data frame 
  df <- df %>%
    left_join(details, by = 'article_id') %>%
    dplyr::filter(double_extracted == 1) )
    # mutate use is_matching function 
}

is_matching <- function(){
  
}


get_correct_extracted <- function(){
  #' input: data frame, details df
  #' process: 
  #' output: 
}