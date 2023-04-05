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
    janitor::clean_names() %>%
    dplyr::filter(!is.na(.data[[column_name]]))

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
    dplyr::select(c(article_id, covidence_id, name_data_entry)) %>%
    dplyr::mutate(double_extracted = ifelse(duplicated(covidence_id) & !duplicated(article_id), 1, 0)) 
  
  return(df)
}

add_details <- function(df, detail) {
  df <- df %>%
    left_join(details, by = 'article_id')
  return(df)
}

get_double_extracted <- function(df_with_detail){
  #' input: data frame
  #' process: gets double extracted articles and identifies if matching
  #' output: data frame 
  df <- df %>%
    dplyr::filter(double_extracted == 1) %>%
    dplyr::mutate(double_matching = is_matching())
}

is_matching <- function(df, column_name1, column_name2){
  #' input: dataframe of double extracted rows, columns that you want to match by
  #' suggested columns:
    #' parameter df: parameter type, population location
    #' article df, NA --> make a separate function for the quality assessment?
    #' outbreaks df: outbreak_date_year, outbreak_country
    #' model df: model type, compartmental model (unsure on this one though)
  #' process: matches the row entries by covidence id and other columns, determines if they differ
  #' output output 0 if they don't match and 1 if they do
  df <- df %>%
    dplyr::group_by(covidence_id, .data[[column_name1]], .data[[column_name2]]) %>%
    dplyr::summarise(rows = nrow())
    
  
}


get_correct_extracted <- function(){
  #' input: data frame, details df
  #' process: 
  #' output: 
}