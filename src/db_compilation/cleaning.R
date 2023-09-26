# Functions for cleaning data 
library(dplyr)
library(janitor)
library(rio)
library(tidyr)
library(stringr)

# copied across from data_cleaning.R script:
#' input: data frame to clean, representative column that indicates no data
#' in quotations
#' article: column_name = article_title
#' parameter: column_name = parameter_type
#' model: column_name = model_type
#' outbreak: column_name = outbreak_country
#' process: clean names, drop rows with NAs, adds a grouping variable for
#' classification of the parameter type
#' output: df 
clean_dfs <- function(df, column_name){

  df <- df %>%
    clean_names() %>%
    filter(!is.na(all_of(column_name)))
  
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
      # Format the start/stop months
      population_study_start_month = substring(population_study_start_month, 1, 3),
      population_study_end_month = substring(population_study_end_month, 1, 3),
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
                      "Reproduction number (Effective, Re)", parameter_type))
      )
  }
  df
}