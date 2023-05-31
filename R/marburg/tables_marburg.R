# FUnction to produce table

REGENERATE_DATA <- FALSE

if(REGENERATE_DATA) source("R/script.R")

library(ggplot2)
library(dplyr)
library(flextable)
library(scales)
library(stringr)
library(gt)

par_final <- read.csv("data/marburg/final/parameter_final.csv")
article_df <- read.csv("data/marburg/final/article_final.csv")

# merge with article ID article labels
df <- merge(par_final, article_df %>% dplyr::select(article_id, first_author_first_name,year_publication),
            all.x=TRUE, by="article_id") %>%
  mutate(article_label = as.character(paste0(first_author_first_name," ",year_publication))) %>%
  dplyr::arrange(article_label, -year_publication) %>%
  dplyr::filter(article_id %in% c(17,15) == FALSE) %>%
  # mutate variables
  mutate(population_study_start_month = substring(population_study_start_month, 1, 3),
         population_study_end_month = substring(population_study_end_month, 1, 3),
         # fixing missing month/year 
         population_study_start_month = ifelse((article_id == 57), 'Oct', population_study_start_month),
         population_study_end_month = ifelse((article_id == 57), 'Sep', population_study_end_month),
         population_study_start_year = ifelse((article_id == 57), '1998', population_study_start_year),
         population_study_end_year = ifelse((article_id == 57), '2000', population_study_end_year),
         # making survey year nice for table
         `Survey year` = ifelse(population_study_start_year == population_study_end_year & !is.na(population_study_start_month) & !is.na(population_study_end_month),
                                paste0(population_study_start_month, "-", population_study_end_month, " ", population_study_start_year),
                                ifelse(population_study_start_year != population_study_end_year, 
                                       paste0(population_study_start_year,"-", population_study_end_year), population_study_start_year)),
         # fixing mislabeling of range
         parameter_uncertainty_type = ifelse((article_id == 57 & parameter_data_id == 89) | 
                                               (article_id == 7 & parameter_data_id == 78), 'Range', parameter_uncertainty_type),
         # fixing missing country 
         population_country = ifelse(article_id == 58 | article_id == 57, 'Democratic Republic of the Congo', 
                                     ifelse(article_id == 6, 'South Africa', population_country)),
         # combining range and uncertainty range
         parameter_uncertainty_type = ifelse(!is.na(parameter_upper_bound) & is.na(parameter_uncertainty_upper_value), 'Range', parameter_uncertainty_type),
         parameter_uncertainty_upper_value = ifelse(!is.na(parameter_upper_bound) & is.na(parameter_uncertainty_upper_value), parameter_upper_bound, parameter_uncertainty_upper_value),
         parameter_uncertainty_lower_value = ifelse(!is.na(parameter_lower_bound) & is.na(parameter_uncertainty_lower_value), parameter_lower_bound, parameter_uncertainty_lower_value),
         Uncertainty = ifelse(parameter_uncertainty_type == 'CI95%', paste0(parameter_uncertainty_lower_value, ", ", parameter_uncertainty_upper_value),
                              ifelse(parameter_uncertainty_type %in% c('Range', 'Highest Posterior Density Interval 95%'), paste0(parameter_uncertainty_lower_value, ' - ', parameter_uncertainty_upper_value),
                                     NA))) 

sero_table(df,pathogen="marburg")
delay_table(df,pathogen="marburg")
