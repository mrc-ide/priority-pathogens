# FUnction to produce table

REGENERATE_DATA <- FALSE

if(REGENERATE_DATA) source("R/script.R")

library(ggplot2)
library(dplyr)
library(flextable)
library(scales)

par_final <- read.csv("data/marburg/final/parameter_final.csv")
article_df <- read.csv("data/marburg/final/article_final.csv")

# merge with article ID article labels
df <- merge(par_final, article_df %>% dplyr::select(article_id, first_author_first_name,year_publication),
            all.x=TRUE, by="article_id") %>%
  mutate(article_label = as.character(paste0(first_author_first_name," ",year_publication))) %>%
  dplyr::arrange(article_label, -year_publication) %>%
  dplyr::filter(article_id %in% c(17,15) == FALSE) 

border_style = officer::fp_border(color="black", width=1)

# Seroprevalence table
sero_table <- function(){
  sero_tbl <- df %>%
  # select variables
  mutate(parameter_value = percent(round(parameter_value, 2), scale = 1),
         population_study_start_month = substring(population_study_start_month, 1, 3),
         population_study_end_month = substring(population_study_end_month, 1, 3),
         `Survey year` = ifelse(population_study_start_year == population_study_end_year & !is.na(population_study_start_month) & !is.na(population_study_end_month) & population_study_start_month!= population_study_end_month,
                                paste0(population_study_start_month, "-", population_study_end_month, " ", population_study_start_year),
                                ifelse(population_study_start_year != population_study_end_year, 
                                       paste0(population_study_start_year,"-", population_study_end_year), population_study_start_year)),
         parameter_type = str_replace(parameter_type, 'Seroprevalence - ', ''),
         parameter_type = str_replace(parameter_type, 'Human delay - ', '')) %>%
  filter(parameter_class == 'Seroprevalence') %>%
  select(c(Article = article_label, 
           Country = population_country, 
           `Survey year`, 
           `Parameter type*` = parameter_type, 
           `Seroprevalence (%)` = parameter_value, 
           `Number Seropositive` = cfr_ifr_numerator, 
           `Sample size` = cfr_ifr_denominator, 
           `Population Group` = population_group,
           parameter_class,
           `Timing of survey` = method_moment_value )) %>%
  arrange(`Parameter type*`) %>%
  group_by(`Parameter type*`) %>%
  mutate(index_of_change = row_number(),
         index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
  flextable(col_keys = c("Article", "Country", "Parameter type*", "Survey year", "Seroprevalence (%)", 'Number Seropositive', 
                         'Sample size', 'Population Group', 'Timing of survey')) %>%
  fontsize(i = 1, size = 12, part = "header") %>%  # adjust font size of header
  border_remove() %>%
  autofit() %>%
  theme_booktabs() %>%
  vline(j = c(3), border = border_style) %>%
  hline(i = ~ index_of_change == 1) %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  add_footer_lines("*HAI/HI: Hemagglutination Inhibition Assay; IFA: Indirect Fluorescent Antibody assay; IgG: Immunoglobulin G; IgM: Immunoglobulin M; Unspecified assay.")
sero_tbl
save_as_image(sero_tbl, path = "sero_tbl.png")
}

# Human Delay table
delay_table <- function(){
  delay_tbl <- df %>%
  # select variables
  mutate(population_study_start_month = substring(population_study_start_month, 1, 3),
         population_study_end_month = substring(population_study_end_month, 1, 3),
         `Survey year` = ifelse(population_study_start_year == population_study_end_year & !is.na(population_study_start_month) & !is.na(population_study_end_month),
                                paste0(population_study_start_month, "-", population_study_end_month, " ", population_study_start_year),
                                ifelse(population_study_start_year != population_study_end_year, 
                                       paste0(population_study_start_year,"-", population_study_end_year), population_study_start_year)),
         `Parameter type` = str_to_title(str_replace(parameter_type, 'Seroprevalence - ', '')),
         `Parameter type` = str_to_title(str_replace(parameter_type, 'Human delay - ', '')),
         Uncertainty = ifelse(parameter_uncertainty_type == '95% CI', paste0(parameter_uncertainty_lower_value, ", ", parameter_uncertainty_upper_value),
                              ifelse(parameter_uncertainty_type %in% c('Range', 'Highest Posterior Density Interval 95%'), paste0(parameter_uncertainty_lower_value, '-', parameter_uncertainty_upper_value),
                                     NA))) %>%
  
  filter(parameter_class == 'Human delay') %>%
  
  select(c(Article = article_label, 
           Country = population_country, 
           `Survey year`, 
           `Parameter type`, 
           `Delays (days)` = parameter_value, 
           Uncertainty,
           `Population Group` = population_group,
           `Timing of survey` = method_moment_value )) %>%
  arrange(`Parameter type`) %>%
  group_by(`Parameter type`) %>%
  mutate(index_of_change = row_number(),
         index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
  flextable(col_keys = c("Article", "Country", "Parameter type", "Survey year", "Delays (days)", 'Population Group', 'Timing of survey')) %>%
  fontsize(i = 1, size = 12, part = "header") %>%  # adjust font size of header
  border_remove() %>%
  autofit() %>%
  theme_booktabs() %>%
  vline(j = c(3), border = border_style) %>%
  hline(i = ~ index_of_change == 1) %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  add_footer_lines("")
delay_tbl

save_as_image(delay_tbl, path = "delay_tbl.png")
}