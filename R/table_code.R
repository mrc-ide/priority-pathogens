# Functions to produce table

library(ggplot2)
library(dplyr)
library(flextable)
library(scales)
library(stringr)
library(gt)


# Seroprevalence table
sero_table <- function(df,pathogen){
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
  
  sero_flextable_tbl <- sero_tbl %>%
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
    group_by(Country) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
    mutate(across(everything(), ~ replace(.x, is.na(.x), ""))) %>% dplyr::select(-c(index_of_change)) %>%
    gt() %>% 
    as_latex() %>% as.character()
  
  save_as_image(delay_tbl, path = paste0("data/", pathogen,"/output/delay_tbl.png"))
  writeLines(delay_latex, paste0("data/", pathogen,"/output/delay_latex_tbl.txt"))
}

