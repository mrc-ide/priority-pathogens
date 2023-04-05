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

filter_extracted <- function(df, double = FALSE, matching = FALSE,
                             column_name1 = "NA", column_name2 = "NA",
                             id_name1 = "NA", id_name2 = "NA") {
  #' input: data frame
  #' process: gets double extracted articles and identifies if matching
  #' output: data frame
  if(double == FALSE){
    df <- df %>%
    dplyr::filter(double_extracted == 0)
  } else if(double == TRUE){
    df <- df %>%
      dplyr::filter(double_extracted == 1)
  }
  
  if(double == TRUE) {
    df <- df %>%
      dplyr::group_by(covidence_id) %>%
      dplyr::mutate(names_combined = list(unique(name_data_entry))) %>%
      dplyr::group_by(across(-c(names_combined, name_data_entry, .data[[id_name1]], .data[[id_name2]]))) %>%
      dplyr::mutate(num_rows = sum(n())) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(matching = ifelse(num_rows == 2, 1, 0))
    if(matching == TRUE) {
      df <- df %>% 
        dplyr::filter(matching == 1) %>%
        dplyr::distinct()
    } else if(matching == FALSE) {
      df <- df %>%
        dplyr::filter(matching == 0)
    }
  }
  return(df)
}

get_correct_extracted <- function(df_single, df_match, df_fixed){
  #' input: input the dataframe of single extracted data, matching data with duplicates removed and fixed non-concordants with duplicates removed
  #' process: rbind
  #' output: output complete dataset to be used in analysis steps
}