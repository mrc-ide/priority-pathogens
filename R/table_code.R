# Functions to produce table

library(ggplot2)
library(dplyr)
library(flextable)
library(scales)
library(stringr)
library(gt)
library(tm)


# Seroprevalence table
sero_table <- function(df,pathogen){
  border_style = officer::fp_border(color="black", width=1)
  set_flextable_defaults(background.color = "white")
  
  sero_tbl <- df %>%
    mutate(parameter_value = round(parameter_value, 2),
           parameter_type = str_replace(parameter_type, 'Seroprevalence - ', '')) %>%
    filter(parameter_class == 'Seroprevalence') %>%
    select(c(Article = article_label, 
             Country = population_country, 
             `Survey year`, 
             `Parameter type*` = parameter_type, 
             `Seroprevalence (%)` = parameter_value, 
             `Uncertainty (%)` = Uncertainty,
             `Uncertainty type` = parameter_uncertainty_type,
             `Number Seropositive` = cfr_ifr_numerator, 
             `Sample size` = cfr_ifr_denominator, 
             `Population Group` = population_group,
             parameter_class,
             `Timing of survey` = method_moment_value,
             `Disaggregated data\navailable` = method_disaggregated_by))
  
  multicountry <- sero_tbl[which(sero_tbl$Country == "Cameroon, Central African Republic, Chad, Republic of the Congo, Equatorial Guinea, Gabon"),]
  
  sero_flextable_tbl <- sero_tbl %>%
    filter(!Country == "Cameroon, Central African Republic, Chad, Republic of the Congo, Equatorial Guinea, Gabon") %>%
    arrange(Country, `Survey year`, `Parameter type*`) %>%
    rbind(multicountry) %>%
    group_by(Country) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
    flextable(col_keys = c("Article", "Country", "Parameter type*", "Survey year", "Seroprevalence (%)", 'Uncertainty (%)', 'Uncertainty type',
                           'Number Seropositive', 'Sample size', 'Population Group', 'Timing of survey', 'Disaggregated data\navailable')) %>%
    fontsize(i = 1, size = 12, part = "header") %>%  # adjust font size of header
    border_remove() %>%
    autofit() %>%
    theme_booktabs() %>%
    vline(j = c(4), border = border_style) %>%
    hline(i = ~ index_of_change == 1) %>%
    bold(i = 1, bold = TRUE, part = "header") %>%
    add_footer_lines("*HAI/HI: Hemagglutination Inhibition Assay; IFA: Indirect Fluorescent Antibody assay; IgG: Immunoglobulin G; IgM: Immunoglobulin M; Unspecified assay.")
  sero_flextable_tbl
  save_as_image(sero_flextable_tbl, path = paste0("data/", pathogen,"/output/sero_tbl.png"))
  
  sero_latex <- sero_tbl %>%
    filter(!Country == "Cameroon, Central African Republic, Chad, Republic of the Congo, Equatorial Guinea, Gabon") %>%
    arrange(Country, `Survey year`, `Parameter type*`) %>%
    rbind(multicountry) %>%
    group_by(Country) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>% 
    mutate(across(everything(), ~ replace(.x, is.na(.x), ""))) %>% dplyr::select(-c(parameter_class,index_of_change)) %>%
    gt() %>% 
    tab_source_note(
      source_note = "*HAI/HI: Hemagglutination Inhibition Assay; IFA: Indirect Fluorescent Antibody assay; IgG: Immunoglobulin G; IgM: Immunoglobulin M; Unspecified assay.") %>%
    as_latex() %>% as.character()
  
  writeLines(sero_latex, paste0("data/", pathogen,"/output/sero_latex_tbl.txt"))
}


# Human Delay table
delay_table <- function(df, pathogen){
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
             # `Disaggregated data\navailable` = method_disaggregated_by,
             # `Risk factor outcome` = riskfactor_outcome
             )) %>%
    arrange(`Parameter type`, Country, `Survey year`) %>%
    group_by(`Parameter type`) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
    flextable(col_keys = c("Article", "Country", "Parameter type", "Survey year", "Delays (days)", 'Statistic',
                           'Uncertainty', 'Uncertainty type', 'Population Group', 'Timing of survey', 'Outcome')) %>%#,'Disaggregated data\navailable'
    fontsize(i = 1, size = 12, part = "header") %>%  # adjust font size of header
    border_remove() %>%
    autofit() %>%
    theme_booktabs() %>%
    vline(j = c(4), border = border_style) %>%
    hline(i = ~ index_of_change == 1) %>%
    bold(i = 1, bold = TRUE, part = "header") %>%
    add_footer_lines("")
  delay_tbl
  
  delay_latex <- df %>%
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
    group_by(`Parameter type`) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
    mutate(across(everything(), ~ replace(.x, is.na(.x), ""))) %>% dplyr::select(-c(index_of_change)) %>%
    gt() %>% 
    as_latex() %>% as.character()
  
  save_as_image(delay_tbl, path = paste0("data/", pathogen,"/output/delay_tbl.png"))
  writeLines(delay_latex, paste0("data/", pathogen,"/output/delay_latex_tbl.txt"))
}

# risk factor table
risk_table <- function(df,pathogen){
  border_style = officer::fp_border(color="black", width=1)
  set_flextable_defaults(background.color = "white")
  
  risk_tbl <- df %>%
    dplyr::filter(parameter_class == 'Risk factors') %>%
    dplyr::mutate(riskfactor_occupation = str_replace_all(riskfactor_occupation, "burrial", "burial"),
                  riskfactor_name = str_replace_all(riskfactor_name, ";", ", ")) %>%
    dplyr::select(c(Article = article_label, 
             Country = population_country, 
             `Survey year`, 
             'Outcome' = riskfactor_outcome,
             'Risk factor' = riskfactor_name,
             'Occupation' = riskfactor_occupation,
             'Significant' = riskfactor_significant,
             'Adjusted' = riskfactor_adjusted,
             `Sample size` = population_sample_size, 
             'Population sample type' = population_sample_type,
             `Population group` = population_group,
             `Timing of survey` = method_moment_value)) %>%
    dplyr::arrange(Outcome)
  
  risk_flextable_tbl <- risk_tbl %>%
    arrange(Outcome, Country, `Survey year`) %>%
    group_by(Country) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
    flextable(col_keys = c("Article", "Country", "Survey year", "Outcome", 'Risk factor', 'Occupation',
                           'Significant', 'Adjusted', 'Sample size', 'Population sample type',
                           'Population group', 'Timing of survey')) %>%
    fontsize(i = 1, size = 12, part = "header") %>%  # adjust font size of header
    border_remove() %>%
    autofit() %>%
    theme_booktabs() %>%
    vline(j = c(4), border = border_style) %>%
    hline(i = ~ index_of_change == 1) %>%
    bold(i = 1, bold = TRUE, part = "header")
  save_as_image(risk_flextable_tbl, path = paste0("data/", pathogen,"/output/risk_tbl.png"))
  
 risk_latex <- risk_tbl %>%
   arrange(Outcome, Country, `Survey year`) %>%
   group_by(Country) %>%
   mutate(index_of_change = row_number(),
          index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>% 
    mutate(across(everything(), ~ replace(.x, is.na(.x), ""))) %>% dplyr::select(-c(index_of_change)) %>%
    gt() %>% 
    as_latex() %>% as.character()
  
  writeLines(risk_latex, paste0("data/", pathogen,"/output/risk_latex_tbl.txt"))
}

date_start <- function(start_day, start_month, start_year) {
  start_day <- as.character(start_day)
  start_year <- as.character(start_year)
  
  if(!is.na(start_day)) {
    start_date <- paste0(start_day, " ", str_split_fixed(start_month, " ", 2)[1], " ", start_year)
  } else if(is.na(start_day) & !is.na(start_month)) {
    start_date <- paste0(str_split_fixed(start_month, " ", 2)[1], " ", start_year)
  } else if(is.na(start_day) & is.na(start_month)) {
    start_date <- start_year
  }
  start_date
}

# date_start(start_day = df$outbreak_start_day[i],
#            start_month = df$outbreak_start_month[i],
#            start_year = df$outbreak_start_year[i])

date_end <- function(end_day, end_month, end_year) {
  end_day <- as.character(end_day)
  end_year <- as.character(end_year)
  if(!is.na(end_day)) {
    end_date <- paste0(end_day, " ", str_split_fixed(end_month, " ", 2)[1], " ", end_year)
  } else if(is.na(end_day) && !is.na(end_month)) {
    end_date <- paste0(str_split_fixed(end_month, " ", 2)[1], " ", end_year)
  } else if(is.na(end_day) && is.na(end_month)) {
    end_date <- end_year
  }
  end_date
}

outbreak_table <- function(df,pathogen){
  border_style = officer::fp_border(color="black", width=1)
  set_flextable_defaults(background.color = "white")
  
  outbreak_tbl <- df %>%
    dplyr::filter(is.na(outbreak_start_year) == FALSE) %>%
    select(c(article_label, outbreak_start_day, outbreak_start_month, outbreak_start_year,
             outbreak_end_day, outbreak_end_month, outbreak_date_year, 
             outbreak_country, outbreak_location, 
             cases_confirmed, cases_suspected, cases_asymptomatic, cases_mode_detection,
             cases_severe_hospitalised, deaths)) %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(outbreak_start_month = str_replace_all(tm::removeNumbers(outbreak_start_month), "-",""),
                  outbreak_end_month = str_replace_all(tm::removeNumbers(outbreak_end_month), "-","")) %>%
    dplyr::mutate(outbreak_start = date_start(start_day = outbreak_start_day,
                                              start_month = outbreak_start_month,
                                              start_year = outbreak_start_year), 
                  outbreak_end = date_end(end_day = outbreak_end_day,
                                          end_month = outbreak_end_month,
                                          end_year = outbreak_date_year),
                  cases_mode_detection = str_replace_all(cases_mode_detection, "Molecular (PCR etc)", "Molecular")) %>%
    dplyr::distinct() %>%
    dplyr::arrange(outbreak_country, outbreak_date_year) %>%
    group_by(outbreak_country, outbreak_start_year) %>%    
    dplyr::select(Country = outbreak_country,
                  Location = outbreak_location,
                  Article = article_label,
                  Start = outbreak_start, 
                  End = outbreak_end, 
                  Deaths = deaths,
                  "Confirmed" = cases_confirmed,
                  "Suspected" = cases_suspected,
                  "Asymptomatic" = cases_asymptomatic,
                  "Severe/hospitalised" = cases_severe_hospitalised,
                  "Confirmation Method" = cases_mode_detection) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
    flextable(col_keys = c("Country",
                           "Location", "Article", 
                           "Start", "End", 
                           "Deaths", "Confirmed",
                           "Suspected", "Asymptomatic", 
                           "Severe/hospitalised", "Confirmation Method")) %>%
    fontsize(i = 1, size = 12, part = "header") %>%  # adjust font size of header
    border_remove() %>%
    autofit() %>%
    theme_booktabs() %>%
    vline(j = c(3, 5, 7, 10), border = border_style) %>%
    hline(i = ~ index_of_change == 1) %>%
    bold(i = 1, bold = TRUE, part = "header") 
  save_as_image(outbreak_tbl, path = paste0("data/", pathogen,"/output/outbreak_tbl.png"))
  
  outbreak_latex_tbl <- df %>%
    dplyr::filter(is.na(outbreak_start_year) == FALSE) %>%
    select(c(article_label, outbreak_start_day, outbreak_start_month, outbreak_start_year,
             outbreak_end_day, outbreak_end_month, outbreak_date_year, 
             outbreak_country, outbreak_location, 
             cases_confirmed, cases_suspected, cases_asymptomatic, cases_mode_detection,
             cases_severe_hospitalised, deaths)) %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(outbreak_start_month = str_replace_all(tm::removeNumbers(outbreak_start_month), "-",""),
                  outbreak_end_month = str_replace_all(tm::removeNumbers(outbreak_end_month), "-","")) %>%
    dplyr::mutate(outbreak_start = date_start(start_day = outbreak_start_day,
                                              start_month = outbreak_start_month,
                                              start_year = outbreak_start_year), 
                  outbreak_end = date_end(end_day = outbreak_end_day,
                                          end_month = outbreak_end_month,
                                          end_year = outbreak_date_year)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(outbreak_country, outbreak_date_year) %>%
    group_by(outbreak_country, outbreak_start_year) %>%    
    dplyr::select(Country = outbreak_country,
                  Location = outbreak_location,
                  Article = article_label,
                  Start = outbreak_start, 
                  End = outbreak_end, 
                  Deaths = deaths,
                  "Confirmed" = cases_confirmed,
                  "Suspected" = cases_suspected,
                  "Asymptomatic" = cases_asymptomatic,
                  "Severe/hospitalised" = cases_severe_hospitalised,
                  "Confirmation Method" = cases_mode_detection) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
    mutate(across(everything(), ~ replace(.x, is.na(.x), ""))) %>% dplyr::select(-c(index_of_change)) %>%
    gt() %>% 
    as_latex() %>% as.character()
  
  writeLines(outbreak_latex_tbl, paste0("data/", pathogen,"/output/outbreak_latex_tbl.txt"))
}
