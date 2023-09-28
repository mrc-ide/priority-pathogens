# Functions for cleaning data 
library(dplyr)
library(janitor)
library(rio)
library(tidyr)
library(stringr)
library(ids)

# copied across some of this from data_cleaning.R script:
#' input: data frame to clean
#' process: clean names, removes old ids and extractor names, adds a grouping 
#' variable for classification of the parameter type
#' output: df 
clean_dfs <- function(df, pathogen){
  
  if('article_title' %in% colnames(df)){
    df <- df %>%
      select(-c("article_id", "covidence_id_text", "name_data_entry")) %>%
      rename(first_author_surname = first_aauthor_surname) %>%
      relocate(c(id, covidence_id, pathogen,
                 first_author_first_name, first_author_surname)) %>%
      arrange(covidence_id)
  }
  
    if('model_type' %in% colnames(df)){
      df <- df %>%
        select(-c("article_id", "access_model_id", "name_data_entry")) %>%
        relocate(c(id, model_data_id, covidence_id, pathogen)) %>%
        arrange(covidence_id)
      
      # Pathogen-specific model data cleaning
      if (pathogen == "EBOLA") {
        
        # edit ebola_variant names
        df <- df %>%
          mutate(
            ebola_variant = case_when(
              covidence_id == 15947 ~ "Bundibugyo virus (BDBV)",
              covidence_id == 5675 ~ 
                "Bundibugyo virus (BDBV);Sudan virus (SUDV);Taï Forest virus (TAFV);Zaire Ebola virus (EBOV)",
              TRUE ~ ebola_variant))
      }
    } 
  
  if('outbreak_id' %in% colnames(df)){
    df <- df %>%
      mutate(
        # Format start/stop months for outbreaks
        outbreak_start_month = substring(outbreak_start_month, 1, 3),
        outbreak_end_month = substring(outbreak_end_month, 1, 3),
        # Outbreak country names
        outbreak_country = str_replace(outbreak_country, 'Congo, Rep.',
                                       'Republic of the Congo'),
        outbreak_country = str_replace(outbreak_country, 'Congo, Dem. Rep.',
                                       'Democratic Republic of the Congo'),
        outbreak_country = str_replace(outbreak_country, 'Yuogslavia',
                                       'Yugoslavia'))
  }
  
if('parameter_type' %in% colnames(df)) {
  df <- df %>%
    mutate(
      # Group parameters
      parameter_class = case_when(
        grepl('Human delay', parameter_type) ~ 'Human delay',
        grepl('Seroprevalence', parameter_type) ~ 'Seroprevalence',
        grepl('Mutations', parameter_type) ~ 'Mutations',
        grepl('Risk factors', parameter_type) ~ 'Risk factors',
        grepl('Reproduction number', parameter_type) ~ 'Reproduction number',
        grepl('Severity', parameter_type) ~ 'Severity',
        grepl('Mosquito', parameter_type) ~ "Mosquito",
        grepl('Relative', parameter_type) ~ 'Relative contribution',
        grepl('Overdispersion', parameter_type) ~ 'Overdispersion',
        grepl('Risk factors', parameter_type) ~ 'Risk factors',
        grepl('Attack rate', parameter_type) ~ 'Attack rate',
        grepl('Doubling time', parameter_type) ~ 'Doubling time',
        grepl('Growth rate', parameter_type) ~ 'Growth rate',
        TRUE ~ 'Other transmission parameters'),
      # Population country
      population_country = str_replace(population_country, 'Congo, Rep.',
                                       'Republic of the Congo'),
      population_country = str_replace(population_country, 'Congo, Dem. Rep.',
                                       'Democratic Republic of the Congo'),
      population_country = str_replace(population_country, 'Yuogslavia',
                                       'Yugoslavia'),
      # combining range and uncertainty range
      parameter_uncertainty_type =
        ifelse(!is.na(parameter_upper_bound) &
                 is.na(parameter_uncertainty_upper_value),
               'Range', parameter_uncertainty_type),
      parameter_uncertainty_upper_value =
        ifelse(!is.na(parameter_upper_bound) &
                 is.na(parameter_uncertainty_upper_value),
               parameter_upper_bound, parameter_uncertainty_upper_value),
      parameter_uncertainty_lower_value =
        ifelse(!is.na(parameter_lower_bound) &
                 is.na(parameter_uncertainty_lower_value),
               parameter_lower_bound, parameter_uncertainty_lower_value),
      # parameter_type name consistency
      parameter_type =
        ifelse(parameter_type == "Growth rate ®", "Growth rate (r)",
               ifelse(parameter_type == "Reproduction number (Effective; Re)",
                      "Reproduction number (Effective, Re)",
                      ifelse(parameter_type == "Mutations ‚Äì substitution rate",
                             "Mutations – substitution rate", parameter_type)))
      ) %>%
    select(-c("article_id", "access_param_id", "name_data_entry")) %>%
    relocate(c(id, parameter_data_id, covidence_id, pathogen)) %>%
    arrange(covidence_id)
  
  # Pathogen-specific parameter data cleaning
  if (pathogen == "EBOLA") {
    
    # merge ebola_variant and ebola_variant_p
    df <- df %>%
      mutate(
        ebola_variant_p = case_when(
          ebola_variant_p == "Bundibugyo virus (BDBV)¬†" ~ ebola_variant,
          TRUE ~ ebola_variant_p)) %>%
      mutate(
        ebola_variant_fix = case_when(
          covidence_id %in% c(163, 662, 847, 6346) ~ "Zaire Ebola virus (EBOV)",
          covidence_id %in% c(2548, 904, 17835) ~ 
            "Bundibugyo virus (BDBV);Sudan virus (SUDV);Taï Forest virus (TAFV);Zaire Ebola virus (EBOV)",
          TRUE ~ ebola_variant_p)) %>%
      select(-c(ebola_variant_p, ebola_variant)) %>%
      rename(ebola_variant = ebola_variant_fix)
    
    # fix the population start and population end month
    df <- df %>%
      mutate(
        population_study_end_month = 
          case_when(population_study_end_month == "3" ~ "Mar",
                    TRUE ~ population_study_end_month)) %>%
      mutate(
        population_study_end_month = gsub("[^a-zA-Z]", "", population_study_end_month),
        population_study_end_month = substr(population_study_end_month, 1, 3)) %>%
      mutate(
        population_study_start_month = gsub("[^a-zA-Z]", "", population_study_start_month),
        population_study_start_month = substr(population_study_start_month, 1, 3))
    
    # Add ids for new parameters - hacky and needs to be fixed so that each new 
    # parameter has a unique parameter_data_id
    df <- df %>%
      mutate(id = case_when(
        covidence_id == 104 & is.na(id) ~ 
          unique(id[covidence_id == 104 & parameter_type == "Risk factors"]),
        TRUE ~ id)) %>%
      mutate(parameter_data_id = case_when(
        covidence_id == 104 & is.na(parameter_data_id) ~ 
          random_id(n = 1, use_openssl = FALSE),
        TRUE ~ parameter_data_id))
  }
  
  if (pathogen == "MARBURG") {
    df <- df %>%
      mutate(
        population_study_start_month = substring(population_study_start_month, 1, 3),
        population_study_end_month = substring(population_study_end_month, 1, 3))
    }
  
  }
  df
}