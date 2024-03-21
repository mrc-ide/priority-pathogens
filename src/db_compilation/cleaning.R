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
  
  if (pathogen == "LASSA") {
    #journal consistency
    df <- df %>% 
          mutate(journal = str_to_title(journal)) %>%
          mutate(journal = sub("^The\\s+", "", journal)) %>%
          mutate(journal = ifelse(journal == 'Bmj','British Medical Journal',
                           ifelse(journal == 'Transactions Of The Royal Society Tropical Medicine And Hygiene','Transactions Of The Royal Society Of Tropical Medicine And Hygiene',        
                           journal))) %>%
    #journal typos
          mutate(journal = case_when(
                 covidence_id == 870 ~ 'Transactions Of The Royal Society Of Tropical Medicine And Hygiene',
                 covidence_id == 2684 ~ 'International Journal Of Infectious Diseases',
                 covidence_id == 669 ~ 'American Journal Of Tropical Medicine And Hygiene',
                 covidence_id == 4237 ~ 'Chaos Solitons & Fractals',
                 covidence_id == 2655 ~ 'Lancet Infectious Diseases',
                 TRUE ~ journal)) %>%
    #year typos
          mutate(year_publication = case_when(
                 covidence_id == 1012 ~ 2008,
                 covidence_id == 1368 ~ 2001,
                 covidence_id == 1444 ~ 2017,
                 covidence_id == 2008 ~ 1983,
                 TRUE ~ year_publication)) %>%
    #volume typos
          mutate(volume = case_when(
                 covidence_id == 1012 ~ 42,
                 covidence_id == 4251 ~ 2020,
                 TRUE ~ volume)) %>%
    #issue typos
          mutate(issue = case_when(
                 covidence_id %in% c(1012, 168) ~ 1,
                 covidence_id %in% c(79) ~ 2,
                 covidence_id %in% c(49, 941) ~ 3,
                 covidence_id %in% c(225,870) ~ 4,
                 covidence_id %in% c(2133) ~ 6,
                 covidence_id %in% c(1447) ~ 11,
                 covidence_id %in% c(2801) ~ 26,
                 covidence_id %in% c(845, 3147, 3153, 3215) ~ NA,
                 TRUE ~ issue)) %>%
    #page typos
          mutate(page_first = case_when(
                 covidence_id %in% c(757) ~ 972,
                 covidence_id %in% c(1413, 2567, 2610, 2661, 3215, 3258, 3635) ~ NA,
                 TRUE ~ page_first)) %>%
          mutate(page_last = case_when(
                 covidence_id == 263 ~ 28,
                 covidence_id == 757 ~ 977,
                 covidence_id == 1235 ~ 1205,
                 covidence_id == 4745 ~ 784,
                 TRUE ~ page_last)) %>%
    #title typos
          mutate(article_title = case_when(
                 covidence_id == 444 ~ 'Genetic characterization of Lassa virus strains isolated from 2012 to 2016 in southeastern Nigeria',
                 covidence_id == 545 ~ 'Lassa Virus Activity in Guinea - Distribution of Human Antiviral Antibody-Defined Using Enzyme-Linked-Immunosorbent-Assay with Recombinant Antigen',
                 covidence_id == 645 ~ 'Clinical Observations in 42 Patients with Lassa Fever',
                 covidence_id == 874 ~ 'Lassa Fever in Eastern Province of Sierra Leone, 1970-1972 .2. Clinical Observations and Virological Studies on Selected Hospital Cases',
                 covidence_id == 1012 ~ 'Strain-specific antibody response to Lassa virus in the local population of west Africa',
                 covidence_id == 2648 ~ 'Rodent-borne infections in rural Ghanaian farming communities',
                 covidence_id == 4371 ~ 'Assessing the dynamics of Lassa fever with impact of environmental sanitation: optimal control and cost-effectiveness analysis',
                 covidence_id == 4727 ~ 'Lassa Fever among Children in Eastern Province, Sierra Leone: A 7-year Retrospective Analysis (2012-2018)',
                 TRUE ~ article_title)) %>%
    #title consistency
          mutate(article_title = gsub(";", ",", article_title)) %>%
          mutate(article_title = gsub("\n", " ", article_title)) %>%
          mutate(article_title = gsub("\\s+", " ", article_title)) %>%
          mutate(article_title = str_to_title(article_title)) %>%
    #missing dois
          mutate(doi = sub(".*?10\\.", "10.", doi)) %>%
          mutate(doi = case_when(
                 covidence_id == 2656 ~ '10.1016/j.ijid.2019.03.030',
                 covidence_id == 2662 ~ '10.1016/S2214-109X(20)30518-0',
                 covidence_id == 1426 ~ '10.1016/j.cell.2015.07.020',
                 covidence_id == 441  ~ '10.1016/j.sste.2014.04.005',
                 covidence_id == 444  ~ '10.1371/journal.pntd.0006971',
                 covidence_id == 461  ~ '10.1016/S1473-3099(18)30121-X',
                 TRUE ~ doi)) %>%
    #paper copies
          mutate(paper_copy_only = case_when(
                 covidence_id %in% c(845,917) ~ FALSE,
                 TRUE ~ paper_copy_only)) %>%
    #name typos
          mutate(first_author_surname = case_when(
                 covidence_id == 2648 ~ 'Shirley C.',
                 covidence_id == 1447 ~ 'N.A.',
                 TRUE ~ first_author_surname)) %>%
          mutate(first_author_first_name = sub(".*\\.(.*)", "\\1", first_author_first_name)) %>%
          mutate(first_author_first_name = sub("^\\s+", "", first_author_first_name)) %>%                 
          mutate(first_author_first_name = case_when(
                 covidence_id == 2648 ~ 'Nimo-Paintsil',
                 covidence_id == 2585 ~ 'Dalhat',
                 covidence_id == 1033 ~ 'Ehichioya',
                 covidence_id == 661 ~ 'Kerneis',
                 TRUE ~ first_author_first_name))          
    #revised qa after parameter removed: now outbreak only
    df[df$covidence_id==152, c("qa_m1", "qa_m2", "qa_a3","qa_a4", "qa_d6", "qa_d7")] <- NA
  }  
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
  } else if (pathogen == "LASSA") {
    #reclassified models
    df <- df %>%
          mutate(model_type = case_when(
                 covidence_id == 2620 ~ 'Other',
                 TRUE ~ model_type),
                 stoch_deter = case_when(
                 covidence_id == 285  ~ 'Stochastic',
                 TRUE ~ stoch_deter),
    #missed transmission pathways
                 transmission_route = case_when(
                 covidence_id %in% c(558,560) ~ 'Human to human (direct contact);Vector/Animal to human',
                 TRUE ~ transmission_route),
    #assumptions should apply to human mixing only
                 assumptions = case_when(
                 covidence_id %in% c(4133,4113,4237,4243,4316,4340,2578,2707,4207,
                                     4487,4193,4162,2801,4517,4597,4827,4213) ~ 'Homogeneous mixing',
                 covidence_id %in% c(558) ~ 'Heterogenity in transmission rates - over time',
                 covidence_id %in% c(2620,4343) ~ 'Heterogenity in transmission rates - between groups',
                 covidence_id %in% c(2617) ~ 'Heterogenity in transmission rates - between groups;Heterogenity in transmission rates - over time',
                 covidence_id %in% c(4118) ~ 'Heterogenity in transmission rates - over time;Latent period is same as incubation period',
                 TRUE ~ assumptions),
    #compartments should apply to humans only
                 compartmental_type = case_when(
                 covidence_id %in% c(2617, 2620) ~ 'Not compartmental',
                 covidence_id %in% c(4517, 4827, 4597, 2578, 2707, 4136, 4144, 4207, 4371) ~ 'Other compartmental',
                 covidence_id %in% c(4162, 4487, 3659, 4243, 4316, 4131) ~ 'SEIR',
                 covidence_id %in% c(4193, 4213, 4237, 4251) ~ 'SIR',
                 TRUE ~ compartmental_type),
    #unspecified interventions
                 interventions_type = case_when(
                 covidence_id %in% c(558,560) | is.na(interventions_type) ~ 'Unspecified',
                 TRUE ~ interventions_type))
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
    
  if (pathogen == "LASSA") {
    #clean locations
    df <- df %>%
          mutate(outbreak_location = gsub(",", ";", outbreak_location)) %>%
          mutate(outbreak_location = gsub("and", ";", outbreak_location)) %>%
          mutate(outbreak_location = sub("^\\s+", "", outbreak_location)) %>%      
          mutate(outbreak_location = str_to_title(outbreak_location)) %>%      
          mutate(outbreak_location = case_when(
                 covidence_id == 560 & access_outbreak_id == 3 ~ 'Kenema Government Hospital',
                 TRUE ~ outbreak_location)) %>%
    #unspecified case detection mode 
          mutate(cases_mode_detection = case_when(
                 is.na(cases_mode_detection) ~ 'Unspecified',
                 TRUE ~ cases_mode_detection))
  } 
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
    population_country = str_replace(population_country, 'Congo; Rep.',
                                     'Republic of the Congo'),
    population_country = str_replace(population_country, 'Congo; Dem. Rep.',
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
  } else if (pathogen == "MARBURG") {
    df <- df %>%
      mutate(
        population_study_start_month = substring(population_study_start_month, 1, 3),
        population_study_end_month = substring(population_study_end_month, 1, 3))
  } else if (pathogen == "LASSA") {
    #removed parameters
    rmrows <- paste(c(61,152,854), c(3,1,5), sep='_')
    df <- df %>%
          mutate(temp_col = paste(covidence_id, access_param_id, sep='_')) %>%
          filter(!temp_col %in% rmrows) %>%
          select(-temp_col) %>%
    #correct parameter types
          mutate(parameter_type = case_when(
                 covidence_id == 854 & access_param_id %in% c(5, 6) ~ 'Human delay - time in care (length of stay)',
                 TRUE ~ parameter_type),
                 other_delay_start = case_when(
                 covidence_id == 854 & access_param_id %in% c(5, 6) ~ NA_character_,
                 TRUE ~ other_delay_start),
                 other_delay_end = case_when(
                 covidence_id == 854 & access_param_id %in% c(5, 6) ~ NA_character_,
                 TRUE ~ other_delay_end),
    #numeric parameter values        
                 parameter_value = as.numeric(parameter_value),
    #parameter value type consistency
                 parameter_value_type = case_when(
                 is.na(parameter_value) ~ NA_character_,
                 !is.na(parameter_value) & is.na(parameter_value_type) ~ 'Unspecified',
                 TRUE ~ parameter_value_type),
    #unspecified cfr/ifr methods
                 cfr_ifr_method = case_when(
                 parameter_class == 'Severity' & is.na(cfr_ifr_method) ~ 'Unspecified',
                 TRUE ~ cfr_ifr_method),
    #specify other risk factor outcomes
                 riskfactor_outcome = case_when(
                 covidence_id == 2627 & access_param_id == 22 ~ 'Reproduction Number',
                 covidence_id == 3153 & access_param_id == 33 ~ 'Onset-Admission Delay',
                 covidence_id == 920 & access_param_id == 4 ~ 'Viremia',
                 covidence_id == 1328 & access_param_id %in% c(16, 17, 18, 19) ~ 'Occurrence',
                 covidence_id == 441 & access_param_id %in% c(6,7) ~ 'Occurrence',
                 covidence_id == 2661 & access_param_id %in% c(15) ~ 'Occurrence',
                 covidence_id == 2661 & access_param_id == 16 ~ 'Incidence',
                 TRUE ~ riskfactor_outcome),
    #unnecessary risk factor occupations
                 riskfactor_occupation = case_when(
                 !is.na(riskfactor_occupation) & riskfactor_occupation == "Unspecified" ~ NA_character_,
                 TRUE ~ riskfactor_occupation),
    #location consistency
                 population_location = str_to_title(sub("^\\s+", "", population_location)),
    #unspecified timing
                 method_moment_value = case_when(
                 is.na(method_moment_value) ~ 'Unspecified',
                 TRUE ~ method_moment_value),
    #correct contexts
                 population_sample_type = case_when(
                 covidence_id == 669 ~ 'Household based', 
                 covidence_id == 652 ~ 'Community based',
                 covidence_id == 4684 ~ 'Community based', 
                 TRUE ~ population_sample_type),
                 population_group = case_when(
                 covidence_id == 669 ~ 'Mixed groups',
                 covidence_id == 652 ~ 'Other',
                 TRUE ~ population_group),
    #unspecified sex
                 population_sex = case_when(
                 is.na(population_sex) ~ 'Unspecified',
                 TRUE ~ population_sex))
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