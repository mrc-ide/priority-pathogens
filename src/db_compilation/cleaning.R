# Functions for cleaning data and adding qa scores
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
        #select(-c("article_id", "access_model_id", "name_data_entry")) %>%
        select(-c("article_id", "name_data_entry")) %>%
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
      df <- df %>% select(-c("access_model_id"))
  }

  if('outbreak_id' %in% colnames(df)){
    df <- df %>%
      select(-c("article_id", "outbreak_id", "name_data_entry")) %>%
      relocate(c(id, outbreak_data_id, covidence_id, pathogen)) %>%
      arrange(covidence_id) %>%
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
    
    df <- df %>% select(-c("access_outbreak_id"))
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
      # # combining range and uncertainty range
      # parameter_uncertainty_type =
      #   ifelse(!is.na(parameter_upper_bound) &
      #            is.na(parameter_uncertainty_upper_value),
      #          'Range', parameter_uncertainty_type),
      # parameter_uncertainty_upper_value =
      #   ifelse(!is.na(parameter_upper_bound) &
      #            is.na(parameter_uncertainty_upper_value),
      #          parameter_upper_bound, parameter_uncertainty_upper_value),
      # parameter_uncertainty_lower_value =
      #   ifelse(!is.na(parameter_lower_bound) &
      #            is.na(parameter_uncertainty_lower_value),
      #          parameter_lower_bound, parameter_uncertainty_lower_value),
      # parameter_type name consistency
      parameter_type =
        ifelse(parameter_type == "Growth rate ®", "Growth rate (r)",
               ifelse(parameter_type == "Reproduction number (Effective; Re)",
                      "Reproduction number (Effective, Re)",
                      ifelse(parameter_type == "Mutations ‚Äì substitution rate",
                             "Mutations – substitution rate", parameter_type)))
      ) %>%
    #select(-c("article_id", "access_param_id", "name_data_entry")) %>%
    select(-c("article_id", "name_data_entry")) %>%
    relocate(c(id, parameter_data_id, covidence_id, pathogen)) %>%
    arrange(covidence_id)

            ## Pathogen-specific parameter data cleaning
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
          
              # clean the other human delays and merge the common ones into parameter_type
              df <- df %>%
                mutate(other_delay_start = case_when(
                  other_delay_start == "Other: Health center visit" ~ "Seeking Care",
                  other_delay_start %in% c("Infection", "Contact with Primary Case") ~ "Exposure/Infection",
                  other_delay_start == "Funeral Start" ~ "Other",
                  other_delay_start == "Positive Test" ~ "Positive test",
                  other_delay_start == "Sampling date" ~ "Sample collection",
                  other_delay_start %in% c("Other: Enter Timepoint in Text Box",
                                           "Other: Not specified 'duration of illness of those who died'",
                                           "Other: alert of illness onset") ~ "Symptom Onset/Fever",
                  other_delay_start == "Testing" ~ "Test",
                  TRUE ~ other_delay_start
                )) %>%
                mutate(other_delay_end = case_when(
                  other_delay_end == "Death in the community" ~ "Death in community",
                  other_delay_end %in% c("Negative RT-PCR", "Negative Test") ~ "Negative test",
                  other_delay_end %in% c(
                    "Clearance of Ebola virus RNA from seminal fluid in 50% of male survivors",
                    "Clearance of Ebola virus RNA from seminal fluid in 90% of male survivors",
                    "Release of sequencing data to response teams",
                    "Funeral End",
                    "Other: Enter Timepoint in Text Box",
                    "Other: first undetectable viremia") ~ "Other",
                  other_delay_end %in% c(
                    "Removal from community", "Household quarantine", "Isolation") ~ "Quarantine",
                  other_delay_end == "Symptom Resolution" ~ "Recovery/non-Infectiousness",
                  other_delay_end %in% c("Detection", "Notification", "Reporting of symptoms",
                                         "Other: Case report completion",
                                         "Report (for confirmed cases with known outcomes)",
                                         "Other: official report") ~ "Reporting",
                  other_delay_end %in% c("Testing", "EVD Testing") ~ "Test",
                  other_delay_end %in% c("Test result", "Result", "Test Results") ~ "Test result",
                  TRUE ~ other_delay_end
                )) %>%
                mutate(other_delay =
                         paste(other_delay_start, "to", other_delay_end, sep = " ")) %>%
                mutate(parameter_type =
                         case_when(
                           other_delay %in% c(
                             "Symptom Onset/Fever to Admission to Care/Hospitalisation",
                             "Symptom Onset/Fever to Death",
                             "Admission to Care/Hospitalisation to Death",
                             "Admission to Care/Hospitalisation to Discharge from Care/Hospital",
                             "Symptom Onset/Fever to Reporting",
                             "Symptom Onset/Fever to Recovery/non-Infectiousness",
                             "Admission to Care/Hospitalisation to Recovery/non-Infectiousness",
                             "Symptom Onset/Fever to Discharge from Care/Hospital",
                             "Death to Burial",
                             "Symptom Onset/Fever to Quarantine",
                             "Symptom Onset/Fever to Test",
                             "Symptom Onset/Fever to Seeking Care"
                             ) ~ paste("Human delay -", other_delay),
                           TRUE ~ parameter_type)
                           )
          
              # Add article id and parameter ids for new parameters
              df <- df %>%
                mutate(id = case_when(
                  covidence_id == 104 & is.na(id) ~
                    unique(id[covidence_id == 104 & parameter_type == "Risk factors"]),
                  TRUE ~ id)) %>%
                mutate(id = case_when(
                  covidence_id == 2548 & is.na(id) ~
                    unique(id[covidence_id == 2548 & !is.na(id)]),
                  TRUE ~ id)) %>%
                mutate(id = case_when(
                  covidence_id == 1044 & is.na(id) ~
                    unique(id[covidence_id == 1044 & !is.na(id)]),
                  TRUE ~ id))
          
                idx <- which(df$covidence_id %in% c(104, 1044, 2548) & is.na(df$parameter_data_id))
                df$parameter_data_id[idx] <-
                    random_id(n = length(idx), use_openssl = FALSE)
          
            }

            if (pathogen == "MARBURG") {
              df <- df %>%
                mutate(
                  population_study_start_month = substring(population_study_start_month, 1, 3),
                  population_study_end_month = substring(population_study_end_month, 1, 3))
            }
  
            if (pathogen == "LASSA") {
            
            #removed parameters
            rmrows <- paste(c(61,152), c(3,1), sep='_')
            df <- df %>%
                  mutate(temp_col = paste(covidence_id, access_param_id, sep='_')) %>%
                  filter(!temp_col %in% rmrows) %>%
                  select(-temp_col)
            
            }

  
  
  
  
  
  
  
  
  
  
  
  df <- df %>% select(-c("access_param_id"))
  }
  
  df
  
}

# Add qa scores to article data
# input: articles data and parameter data
# output: articles data with two new variables: model_only and article_qa_score
add_qa_scores <- function(articles_df, params_df){

  # add a model_only variable to article data (as denominator for qa will be different)
  articles_df <- articles_df %>%
    mutate(model_only =
             as.numeric(!id %in% params_df$id)) %>%
    # if an article is model_only, make 5-7 NA for consistency between scores
    mutate(qa_d5 = ifelse(model_only == 1, NA, qa_d5),
           qa_d6 = ifelse(model_only == 1, NA, qa_d6),
           qa_d7 = ifelse(model_only == 1, NA, qa_d7)) %>%
    # add qa score to article data
    mutate(total_qa =
             rowSums(!is.na(
               select(., qa_m1, qa_m2, qa_a3, qa_a4, qa_d5, qa_d6, qa_d7))),
           yes_score = rowSums(
             select(., qa_m1, qa_m2, qa_a3, qa_a4, qa_d5, qa_d6, qa_d7) == "Yes",
             na.rm = TRUE),
           article_qa_score = ifelse(total_qa > 0, yes_score / total_qa * 100, NA)) %>%
    select(-c(total_qa, yes_score))

  articles_df

}