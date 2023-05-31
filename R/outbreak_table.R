## outbreak table
library(tidyverse)
library(flextable)
library(gt)

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
    select(c(article_label, outbreak_start_day, outbreak_start_month, outbreak_start_year,
             outbreak_end_day, outbreak_end_month, outbreak_date_year, 
             outbreak_country, outbreak_location, 
             cases_confirmed, cases_suspected, cases_asymptomatic, cases_mode_detection,
             cases_severe_hospitalised, deaths)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(outbreak_start = date_start(start_day = outbreak_start_day,
                                              start_month = outbreak_start_month,
                                              start_year = outbreak_start_year), 
                  outbreak_end = date_end(end_day = outbreak_end_day,
                                            end_month = outbreak_end_month,
                                            end_year = outbreak_date_year))%>%
    dplyr::arrange(outbreak_country, outbreak_start) %>%
    dplyr::select(Country = outbreak_country,
                  Location = outbreak_location,
                  Article = article_label,
                  Start = outbreak_start, 
                  End = outbreak_end, 
                  "Confirmed Cases" = cases_confirmed,
                  "Confirmation Method" = cases_mode_detection, 
                  "Suspected Cases" = cases_suspected,
                  "Asymptomatic Cases" = cases_asymptomatic,
                  "Severe/hospitalised Cases" = cases_severe_hospitalised,
                  Deaths = deaths) %>%
    group_by(Country) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
    flextable(col_keys = c("Country",
                           "Location", "Article", 
                           "Start", "End", 
                           "Confirmed Cases", "Confirmation Method",
                           "Suspected Cases", "Asymptomatic Cases", 
                           "Severe/hospitalised Cases", "Deaths")) %>%
    fontsize(i = 1, size = 12, part = "header") %>%  # adjust font size of header
    border_remove() %>%
    autofit() %>%
    theme_booktabs() %>%
    vline(j = c(3, 5, 7, 10), border = border_style) %>%
    hline(i = ~ index_of_change == 1) %>%
    bold(i = 1, bold = TRUE, part = "header") 
  save_as_image(outbreak_tbl, path = paste0("data/", pathogen,"/output/outbreak_tbl.png"))
  
  outbreak_latex_tbl <- df %>%
    select(c(article_label, outbreak_start_day, outbreak_start_month, outbreak_start_year,
             outbreak_end_day, outbreak_end_month, outbreak_date_year, 
             outbreak_country, outbreak_location, 
             cases_confirmed, cases_suspected, cases_asymptomatic, cases_mode_detection,
             cases_severe_hospitalised, deaths)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(outbreak_start = date_start(start_day = outbreak_start_day,
                                              start_month = outbreak_start_month,
                                              start_year = outbreak_start_year), 
                  outbreak_end = date_end(end_day = outbreak_end_day,
                                          end_month = outbreak_end_month,
                                          end_year = outbreak_date_year))%>%
    dplyr::arrange(outbreak_country, outbreak_start) %>%
    dplyr::select(Country = outbreak_country,
                  Location = outbreak_location,
                  Article = article_label,
                  Start = outbreak_start, 
                  End = outbreak_end, 
                  "Confirmed Cases" = cases_confirmed,
                  "Confirmation Method" = cases_mode_detection, 
                  "Suspected Cases" = cases_suspected,
                  "Asymptomatic Cases" = cases_asymptomatic,
                  "Severe/hospitalised Cases" = cases_severe_hospitalised,
                  Deaths = deaths) %>%
    group_by(Country) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(index_of_change == max(index_of_change),1,0)) %>%
    mutate(across(everything(), ~ replace(.x, is.na(.x), ""))) %>% dplyr::select(-c(index_of_change)) %>%
    gt() %>% 
    as_latex() %>% as.character()
  
  writeLines(outbreak_latex_tbl, paste0("data/", pathogen,"/output/outbreak_latex_tbl.txt"))
}

