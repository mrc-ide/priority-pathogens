# FUnction to produce table

REGENERATE_DATA <- FALSE

if(REGENERATE_DATA) source("R/script.R")

library(ggplot2)
library(dplyr)
library(flextable)
library(scales)
library(stringr)

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



# Seroprevalence table
sero_table <- function(){
  border_style = officer::fp_border(color="black", width=1)
  set_flextable_defaults(background.color = "white")
  
  sero_tbl <- df %>%
    mutate(parameter_value = percent(round(parameter_value, 2), scale = 1),
           parameter_type = str_replace(parameter_type, 'Seroprevalence - ', '')) %>%
    filter(parameter_class == 'Seroprevalence') %>%
    select(c(Article = article_label, 
             Country = population_country, 
             `Survey year`, 
             `Parameter type*` = parameter_type, 
             `Seroprevalence (%)` = parameter_value, 
             Uncertainty,
             `Uncertainty type` = parameter_uncertainty_type,
             `Number Seropositive` = cfr_ifr_numerator, 
             `Sample size` = cfr_ifr_denominator, 
             `Population Group` = population_group,
             parameter_class,
             `Timing of survey` = method_moment_value,
             `Disaggregated data\navailable` = method_disaggregated_by))
  
  multicountry <- sero_tbl[which(sero_tbl$Country == "Cameroon, Central African Republic, Chad, Republic of the Congo, Equatorial Guinea, Gabon"),]
  
  sero_tbl <- sero_tbl %>%
    filter(!Country == "Cameroon, Central African Republic, Chad, Republic of the Congo, Equatorial Guinea, Gabon") %>%
    arrange(Country, `Survey year`, `Parameter type*`) %>%
    rbind(multicountry) %>%
    group_by(Country) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
    flextable(col_keys = c("Article", "Country", "Parameter type*", "Survey year", "Seroprevalence (%)", 'Uncertainty', 'Uncertainty type',
                           'Number Seropositive', 'Sample size', 'Population Group', 'Timing of survey', 'Disaggregated data\navailable')) %>%
    fontsize(i = 1, size = 12, part = "header") %>%  # adjust font size of header
    border_remove() %>%
    autofit() %>%
    theme_booktabs() %>%
    vline(j = c(4), border = border_style) %>%
    hline(i = ~ index_of_change == 1) %>%
    bold(i = 1, bold = TRUE, part = "header") %>%
    add_footer_lines("*HAI/HI: Hemagglutination Inhibition Assay; IFA: Indirect Fluorescent Antibody assay; IgG: Immunoglobulin G; IgM: Immunoglobulin M; Unspecified assay.")
  sero_tbl
  save_as_image(sero_tbl, path = "sero_tbl.png")
}
sero_table()

# Human Delay table
delay_table <- function(){
  border_style = officer::fp_border(color="black", width=1)
  set_flextable_defaults(background.color = "white")
  
  delay_tbl <- df %>%
    filter(parameter_class == 'Human delay') %>%
    mutate(parameter_type = str_to_title(str_replace(parameter_type, 'Human delay - ', ''))) %>%
    select(c(Article = article_label, 
             Country = population_country, 
             `Survey year`, 
             `Parameter type` = parameter_type, 
             `Delays (days)` = parameter_value, 
             Statistic = parameter_value_type,
             Uncertainty,
             `Uncertainty type` = parameter_uncertainty_type,
             `Population Group` = population_group,
             `Timing of survey` = method_moment_value,
             Outcome = riskfactor_outcome,
             `Disaggregated data\navailable` = method_disaggregated_by,
             # `Risk factor outcome` = riskfactor_outcome
             )) %>%
    arrange(`Parameter type`, Country, `Survey year`) %>%
    group_by(Country) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
    flextable(col_keys = c("Article", "Country", "Parameter type", "Survey year", "Delays (days)", 'Statistic',
                           'Uncertainty', 'Uncertainty type', 'Population Group', 'Timing of survey', 'Outcome','Disaggregated data\navailable')) %>%
    fontsize(i = 1, size = 12, part = "header") %>%  # adjust font size of header
    border_remove() %>%
    autofit() %>%
    theme_booktabs() %>%
    vline(j = c(4), border = border_style) %>%
    hline(i = ~ index_of_change == 1) %>%
    bold(i = 1, bold = TRUE, part = "header") %>%
    add_footer_lines("")
  delay_tbl
  
  save_as_image(delay_tbl, path = "delay_tbl.png")
}
delay_table()
