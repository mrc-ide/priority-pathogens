# Functions for cleaning data 
library(dplyr)
library(janitor)
library(rio)
library(tidyr)
library(stringr)

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
    dplyr::filter(!is.na(all_of(column_name)))

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
                                         TRUE ~ 'Other transmission parameters'))%>%
      mutate(population_country = stringr::str_replace(population_country, 'Congo, Rep.', 'Republic of the Congo'),
             population_country = stringr::str_replace(population_country, 'Congo, Dem. Rep.', 'Democratic Republic of the Congo'))
  }
  
  return(df)
}

get_details <- function(article_df, double_vec){
  #' input: article data frame
  #' process: pulls out the covidence ID, article ID, and name of extractor and if duplicated
  #' output: df with 4 columns
  df <- article_df %>%
    dplyr::select(c(article_id, covidence_id, name_data_entry)) %>%
    dplyr::mutate(double_extracted = ifelse(covidence_id %in% double_vec, 1, 0)) 
  
  return(df)
}

add_details <- function(df, detail) {
  df <- df %>%
    left_join(details, by = 'article_id')
  return(df)
}

filter_extracted <- function(df, double = FALSE, matching = FALSE,
                             id_name1 = "NA", id_name2 = "NA") {
  #' input: data frame
  #' process: gets the desired dataset -- double = F -> single extracted and then matching determines whether it'll give matches (w/0 duplicates) or disconcordant dfs
  #' output: desired data frame based on the parameters
  if(double == FALSE) {
    df <- df %>%
      dplyr::filter(double_extracted == 0)
  } else if(double == TRUE) {
    df <- df %>%
      dplyr::filter(double_extracted == 1) %>%
      dplyr::group_by(covidence_id) %>%
      # dplyr::mutate(names_combined = list(unique(name_data_entry))) %>%
      dplyr::group_by(across(-c(all_of(id_name1), all_of(id_name2)))) %>%
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

needs_fixing <- function(df_disconcordant, df_detail) {
  #' input: dataframe of disconcordant double-extracted values and the details df
  #' process: join the duplicated papers in details with df_dis and order to make the fixing process straightforward
  #' output: rds of the data that needs fixing ordered in a sensible way
  dis_detail <- df_detail %>%
    dplyr::filter(double_extracted == 1) %>% 
    dplyr::filter(covidence_id %in% unique(df_disconcordant$covidence_id)) %>%
    dplyr::arrange(covidence_id)
  dis_detail$fixed = NA
  
  df <- dplyr::full_join(df_disconcordant, dis_detail) %>%
    dplyr::relocate(c(fixed, covidence_id, name_data_entry)) %>%
    dplyr::arrange(covidence_id) 
  return(df)
}

get_correct_extracted <- function(df_single, df_match, df_fixed){
  #' input: input the dataframe of single extracted data, matching data with duplicates removed and fixed non-concordants with duplicates removed
  #' process: rbind
  #' output: output complete dataset to be used in analysis steps
  
  df_fixed <- df_fixed %>% 
    dplyr::select(names(df_single))
  
  df <- rbind(df_single, df_match, df_fixed)
  return(df)
}
