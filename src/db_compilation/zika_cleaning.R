# Functions for cleaning data and adding QA scores
library(dplyr)
library(janitor)
library(rio)
library(tidyr)
library(stringr)
library(ids)

##########################
# Cleaning functions for each df #
##########################

#' Input: article/model/parameter/outbreak data frame to clean
#' Process: clean names, removes old ids and extractor names, adds a grouping
#' variable for classification of the parameter type, fix entry errors
#' output: clean article/model/parameter/outbreak df
#' 
####################
# Article cleaning #
####################
zika_clean_articles <- function(df, pathogen){
  
  df <- df %>%
    clean_names() %>%
    mutate(across(.cols = where(is.character), .fns = ~dplyr::na_if(.x, "NA"))) %>%
    select(-c("article_id")) %>%#, "name_data_entry"
    rename(first_author_surname = first_aauthor_surname) %>%
    relocate(c(
      id, covidence_id, pathogen,
      first_author_first_name, first_author_surname
    )) %>%
    arrange(covidence_id) %>%
    # Add article label
    mutate(
      article_label = as.character(
        paste0(first_author_surname, " ", year_publication)
      )
    )
  
  ######################################
  # Pathogen-specific article cleaning #
  ######################################
  
  
  # here we need to remove duplicates by article and covidence IDs
  df <- df %>%
    mutate(# Fix issues with dois
      doi = str_remove_all(doi, 'doi:'),
      doi = str_remove_all(doi, 'http://dx.doi.org/'),
      doi = str_remove_all(doi, 'https://doi.org/'),
      doi = str_remove(doi, "^/"),
      doi = case_when(
        covidence_id %in% 435 ~ "10.1007/s00705-015-2695-5",
        covidence_id %in% 845 ~ "10.1016/s1473-3099(18)30718-7",
        covidence_id %in% 1154 ~ "10.1136/bmjgh-2017-000309",
        covidence_id %in% 1993 ~ "10.1080/00034983.1983.11811687",
        covidence_id %in% 3042 ~ "10.1371/journal.pntd.0004726",
        TRUE ~ doi
      )) #%>% 
  # # Update article label
  #   # for different articles by the same author in the same year
  #   article_label = make.unique(article_label, sep = " ."),
  #   article_label = gsub("\\.1", "(b)", article_label),
  #   article_label = gsub("\\.2", "(c)", article_label),
  #   article_label = gsub("\\.3", "(d)", article_label)
  # )
  
  if (pathogen == "EBOLA") {
    df <- df %>%
      # 12389: model only paper (growth model), 2921: Not extracted from,
      # 6471: sneaky duplicate, 5898: mutation frequencies (not mutation rates
      # as no time component) paper only had mutations so removing whole paper
      filter(!covidence_id %in% c(2921, 12389, 5898, 6471)) %>%
      mutate(
        # edit qa score for Maganga 2014 (NOTE: qa scores only edited after
        # discussion with co-authors)
        qa_m1 = case_when(covidence_id %in% 3138 ~ "No", TRUE ~ qa_m1),
        qa_a3 = case_when(covidence_id %in% 3138 ~ "No", TRUE ~ qa_a3),
        # add missing qa score for 17054
        qa_m1 = case_when(covidence_id %in% 17054 ~ "Yes", TRUE ~ qa_m1),
        qa_m2 = case_when(covidence_id %in% 17054 ~ "Yes", TRUE ~ qa_m2),
        qa_a3 = case_when(covidence_id %in% 17054 ~ "Yes", TRUE ~ qa_a3),
        qa_a4 = case_when(covidence_id %in% 17054 ~ "Yes", TRUE ~ qa_a4),
        qa_d5 = case_when(covidence_id %in% 17054 ~ "Yes", TRUE ~ qa_d5),
        qa_d6 = case_when(covidence_id %in% 17054 ~ "Yes", TRUE ~ qa_d6),
        qa_d7 = case_when(covidence_id %in% 17054 ~ "Yes", TRUE ~ qa_d7),
        # clean up author names (remove initials and fully capitalised names)
        first_author_surname = str_replace(first_author_surname, "\\b[A-Z]\\.", ""),
        first_author_surname = case_when(
          first_author_surname %in% "OUEMBA TASSE ́" ~ "Ouemba Tasse",
          first_author_surname %in% "R Glynn" ~ "Glynn",
          first_author_surname %in% "JUGA" ~ "Juga",
          first_author_surname %in% "KI" ~ "Ki",
          first_author_surname %in% "Area\r\nArea" ~ "Area",
          first_author_surname %in% "DAUTEL" ~ "Dautel",
          covidence_id %in% 12045 & first_author_surname %in% "Gilda" ~ "Grard",
          covidence_id %in% 4132 ~ "Hunt",
          first_author_surname %in% "Report of an International Commission" ~
            "International Commission",
          TRUE ~ first_author_surname
        ),
        # Fix issues with dois
        doi = case_when(
          covidence_id %in% 3058 ~ "10.3201/eid2201.151410",
          covidence_id %in% 1407 ~ "10.1038/nature14612",
          covidence_id %in% 6472 ~ "10.1136/bmj.2.6086.539",
          TRUE ~ doi
        )
      ) %>%
      # Update article label
      mutate(
        article_label = as.character(
          paste0(first_author_surname, " ", year_publication)
        ),
        # for different articles by the same author in the same year
        article_label = make.unique(article_label, sep = " ."),
        article_label = gsub("\\.1", "(b)", article_label),
        article_label = gsub("\\.2", "(c)", article_label),
        article_label = gsub("\\.3", "(d)", article_label)
      )
  }
  
  return(df)
}

#################
# Model cleaning #
##################
zika_clean_models <- function(df, pathogen){    
  
  df <- df %>%
    clean_names() %>%
    mutate(across(.cols = where(is.character), .fns = ~dplyr::na_if(.x, "NA"))) %>%
    select(-c("article_id", "name_data_entry")) %>%
    relocate(c(id, model_data_id, covidence_id, pathogen)) %>%
    arrange(covidence_id)
  
  df <- df %>%
    
    # Update compartmental type 
    mutate(compartmental_type = case_when( 
      compartmental_type == 'NA' | is.na(compartmental_type) ~ 'Not compartmental',
      # need to check compartmental type for  1405, 2012, 3738, 5553, 5540 (other + SEIR-SEI?)
      TRUE ~ compartmental_type
    ))
  # one model with missing name, ids, etc -- look into this? model_data_id = ccc0683c9eebf812a071f334fe2c8611
  
  
  
  df <- df %>% select(-c("access_model_id"))
  return(df)
}

#####################
# Outbreak cleaning #
#####################

zika_clean_outbreaks <- function(df, pathogen){
  df <- df %>%
    clean_names() %>%
    mutate(across(.cols = where(is.character), .fns = ~dplyr::na_if(.x, "NA"))) %>%
    select(-c("article_id", "outbreak_id", "name_data_entry")) %>%
    relocate(c(id, outbreak_data_id, covidence_id, pathogen)) %>%
    arrange(covidence_id) %>%
    mutate(
      # Format start/stop months for outbreaks
      outbreak_start_month = str_sub(outbreak_start_month, 1, 3),
      outbreak_end_month = str_sub(outbreak_end_month, 1, 3),#lubridate::month(as.Date(as.numeric(outbreak_end_month) - 1, origin = "1899-12-30")),
      # Make date variable 
      outbreak_start_date = case_when((!is.na(outbreak_start_year) & !is.na(outbreak_start_month) & !is.na(outbreak_start_day)) ~
                                        lubridate::ymd(paste(outbreak_start_year, outbreak_start_month, outbreak_start_day, sep ='-')),
                                      (!is.na(outbreak_start_year) & !is.na(outbreak_start_month) & is.na(outbreak_start_day)) ~ 
                                        lubridate::ymd(paste(outbreak_start_year, outbreak_start_month, 15, sep ='-')),
                                      (!is.na(outbreak_start_year) & is.na(outbreak_start_month) & is.na(outbreak_start_day)) ~ 
                                        lubridate::ymd(paste(outbreak_start_year, 01, 15, sep ='-')),
                                      TRUE ~ NA),
      outbreak_end_date = case_when((!is.na(outbreak_date_year) & !is.na(outbreak_end_month) & !is.na(outbreak_end_day)) ~
                                      lubridate::ymd(paste(outbreak_date_year, outbreak_end_month, outbreak_end_day, sep ='-')),
                                    (!is.na(outbreak_date_year) & !is.na(outbreak_end_month) & is.na(outbreak_end_day)) ~ 
                                      lubridate::ymd(paste(outbreak_date_year, outbreak_end_month, 15, sep ='-')),
                                    (!is.na(outbreak_date_year) & is.na(outbreak_end_month) & is.na(outbreak_end_day)) ~ 
                                      lubridate::ymd(paste(outbreak_date_year, 01, 15, sep ='-')),
                                    TRUE ~ NA)) 
  
  df <- df %>%
    # Clean outbreak location
    mutate(# Outbreak location 
      outbreak_location = case_when(
        outbreak_location == "Entire Country" ~ NA,
        outbreak_location == 'Australes' | outbreak_location == "Polynesia: Austral Islands (AUS)" 
        ~ "Austral Islands",
        outbreak_location == 'Moorea' | outbreak_location == "Polynesia: Mo'orea Island (MOO)" 
        ~ "Mo'orea",
        outbreak_location == 'Iles Sous-Le-Vent' | outbreak_location == 'Iles sous-le-vent' | 
          outbreak_location == "Polynesia: Sous-le-vent Islands (SLV)" | outbreak_location=='Sous-Le-Vent Islands'
        ~ 'Sous-Le-Vent Islands',
        outbreak_location == "Marquises" | outbreak_location == "Polynesia: Marquesas Islands (MRQ)"
        ~ "Marquesas Islands",
        outbreak_location == "West Indies: Guadelopue (GLP)" ~ 'Guadeloupe',
        outbreak_location == "West Indies: Martinique (MTQ)" ~ "Martinique",
        outbreak_location == "West Indies: Saint-Martin (MAF)" ~ "Saint Martin",
        outbreak_location == "Polynesia: Tuamotus (TUA)" ~ "Tuamotus",
        outbreak_location == "Polynesia: Tahiti (TAH)" ~ "Tahiti",
        outbreak_location == "Cucuta" ~ "Cúcuta",
        TRUE ~ outbreak_location
      ),
      outbreak_country = case_when(
        outbreak_location == "Martinique" ~ "Martinique",
        outbreak_location == "Saint Martin" ~ "Saint Martin",
        TRUE ~ outbreak_country
      )
    ) %>%
    # mutate(outbreak_location = gsub(",", ";", outbreak_location)) %>%
    # mutate(outbreak_location = gsub("and", ";", outbreak_location)) %>%
    # mutate(outbreak_location = sub("^\\s+", "", outbreak_location)) %>%
    mutate(outbreak_location = str_to_title(outbreak_location)) %>%
    # unspecified case detection mode
    mutate(cases_mode_detection = case_when(
      is.na(cases_mode_detection) ~ "Unspecified",
      TRUE ~ cases_mode_detection
    ))
  
  df <- df %>%
    mutate(      # Outbreak country names
      outbreak_country = str_replace(
        outbreak_country, "Congo, Rep.",
        "Republic of the Congo"
      ),
      outbreak_country = str_replace(
        outbreak_country, "Congo, Dem. Rep.",
        "Democratic Republic of the Congo"
      ),
      outbreak_country = str_replace(
        outbreak_country, "Yuogslavia",
        "Yugoslavia"
      ),
      outbreak_country = str_replace(
        outbreak_country, 'Micronesia, Fed. Sts.',
        'Federated States of Micronesia'
      ),
      outbreak_country = case_when(
        covidence_id == 947 & outbreak_location %in% c("Austral Islands", "Marquesas Islands",
                                                       "Mo'orea", "Sous-Le-Vent Islands",
                                                       "Tahiti", "Tuamotus") ~ "French Polynesia",
        covidence_id == 947 & outbreak_location == "Martinique" ~ "France (Martinique)",
        covidence_id == 947 & outbreak_location == "Guadelopue" ~ "France (Guadeloupe)", 
        covidence_id == 947 & outbreak_location == "Saint-Martin" ~ "France (Saint-Martin)",
        TRUE ~ outbreak_country
      )) 
  
  
  df <- df %>% select(-c("access_outbreak_id"))
  return(df)
}

######################
# Parameter cleaning #
######################
zika_clean_params <- function(df, pathogen){
  
  df <- df %>%
    clean_names() %>%
    mutate(
      across(.cols = where(is.character), .fns = ~dplyr::na_if(.x, "NA")),
      # Change variable types
      cfr_ifr_numerator = as.integer(cfr_ifr_numerator),
      cfr_ifr_denominator = as.integer(cfr_ifr_denominator),
      population_study_start_day = as.numeric(population_study_start_day),
      method_disaggregated_by = str_replace_all(method_disaggregated_by, ";", ", "),
      population_location = str_replace_all(population_location, ";", ","),
      across(.cols = c(parameter_value, parameter_lower_bound, parameter_upper_bound, 
                       parameter_uncertainty_lower_value, parameter_uncertainty_upper_value,
                       parameter_2_value, parameter_2_lower_bound, parameter_2_upper_bound, 
                       parameter_2_uncertainty_lower_value, parameter_2_uncertainty_upper_value,
                       parameter_2_uncertainty_single_value,
                       exponent, exponent_2, distribution_2_par1_value, distribution_2_par2_value, 
                       distribution_par1_value, distribution_par2_value), .fns = as.numeric),
      across(.cols = c(population_study_start_year, population_study_end_year,
                       population_study_start_day, population_study_end_day),
             .fns = as.integer),
      
      # Update parameter type values 
      parameter_type = case_when(parameter_type == 'Seroprevalence - PRNT' ~ 'Seroprevalence - Neutralisation/PRNT',
                                 TRUE ~ parameter_type))
  
  df <- df %>%
    # Group parameters
    mutate(parameter_class = case_when(
      grepl("Human delay", parameter_type) ~ "Human delay",
      grepl("Seroprevalence", parameter_type) ~ "Seroprevalence",
      grepl("Mutations", parameter_type) ~ "Mutations",
      grepl("Risk factors", parameter_type) ~ "Risk factors",
      grepl("Reproduction number", parameter_type) ~ "Reproduction number",
      grepl("Severity", parameter_type) ~ "Severity",
      grepl("Mosquito", parameter_type) ~ "Mosquito",
      grepl("Relative", parameter_type) ~ "Relative contribution",
      grepl("Overdispersion", parameter_type) ~ "Overdispersion",
      grepl("Risk factors", parameter_type) ~ "Risk factors",
      grepl("Attack rate", parameter_type) ~ "Attack rate",
      grepl("Doubling time", parameter_type) ~ "Doubling time",
      grepl("Growth rate", parameter_type) ~ "Growth rate",
      TRUE ~ "Other transmission parameters"
    ))
  
  
  
  # More cleaning 
  df <- df %>%
    mutate(
      
      population_group = case_when(
        population_group == 'Persons under investigatioPersons under investigationPersons under investigation' ~ 'Persons under investigation',
        TRUE ~ population_group
      ),
      # Population country
      population_country = str_replace_all(population_country, ";", ","),
      population_country = str_replace(
        population_country, "Congo, Rep.",
        "Republic of the Congo"
      ),
      population_country = str_replace(
        population_country, "Congo, Dem. Rep.",
        "Democratic Republic of the Congo"
      ),
      population_country = str_replace(
        population_country, "Democratic Republic of the Congo",
        "DRC"
      ),
      population_country = str_replace(
        population_country, "Yuogslavia",
        "Yugoslavia"
      ),
      population_country = str_replace(
        population_country, "Gambia, The",
        "The Gambia"
      ),
      population_country = str_replace(
        population_country, "Micronesia,  Fed. Sts.",
        'Federated States of Micronesia'
      ),
      population_country = str_replace(
        population_country, "Venezuela, RB",
        "Venezuela"
      ),
      population_country = str_replace(
        population_country, "Taiwan, China",
        "Taiwan"
      ),
      population_country = str_replace(
        population_country, "Lao PDR",
        "Laos"
      ),
      population_country = str_replace(
        population_country, "Lao PDRIran;  Islamic Rep.",
        "Iran"
      ),
      population_country = str_replace_all(population_country, ",", ", "),
      population_country =
        case_when(
          population_country %in% c('Puerto Rico, United States') ~ 'Puerto Rico',
          population_country %in%
            "El Salvador;Guatemala;Honduras;Mexico;Nicaragua" ~
            "Multi-country: Central America (n = 5)",
          population_country %in%
            "Brazil;French Polynesia" ~
            "Multi-country: Brazil and French Polynesia",
          population_country %in%
            "Brazil;Ecuador;Micronesia; Fed. Sts." ~
            "Multi-country: Brazil, Ecuador, Federated States of Micronesia",
          population_country %in%
            "Brazil;Colombia;French Polynesia" ~
            "Multi-country: Brazil, Colombia, French Polynesia",
          population_country %in%
            "Brazil;Colombia;El Salvador;Haiti;Honduras;Mexico;Puerto Rico;Venezuela; RB" ~
            "Multi-country: Central and South America (n = 8)",
          population_country %in% c(
            "Brazil;Colombia;El Salvador"
          ) ~ "Multi-country: Brazil, Colombia, El Salvador",
          population_country %in% c(
            'Brazil;Colombia;Dominican Republic;El Salvador;Guatemala;Haiti;Honduras;Jamaica;Puerto Rico;United States'
          ) ~ 'Multi-country: Americas (n = 10)',
          population_country %in% c(
            "Brazil;Colombia"
          ) ~ "Multi-country: Brazil and Colombia",
          population_country %in% c(
            'Brazil;Cambodia;China;Colombia;Dominican Republic;French Polynesia;Guatemala;Haiti;Mexico;Micronesia; Fed. Sts.;Philippines;Puerto Rico;Suriname;Thailand'
          ) ~ "Multi-country: South and Central America (n = 8) and Asia (n = 6)",
          population_country %in% c(
            "Brazil;Cambodia;Central African Republic;China;French Polynesia;Guatemala;Haiti;Malaysia;Micronesia; Fed. Sts.;Nigeria;Philippines;Puerto Rico;Senegal;Suriname;Thailand;Uganda;Venezuela; RB"
          ) ~ "Multi-country: South and Central America (n = 7), Asia (n = 7), Africa (n = 4)",
          population_country %in% c(
            "Brazil;Cambodia;Central African Republic;China;Colombia;Guatemala;Haiti;Italy;Malaysia;Mexico;Micronesia; Fed. Sts.;Nigeria;Panama;Philippines;Puerto Rico;Senegal;South Africa;Suriname;Thailand;Uganda;United States"
          ) ~ "Multi-country: Americas (n = 8), Asia (n = 6), Africa (n = 5), Europe (n = 1)",
          population_country %in% c(
            "Argentina;Barbados;Bolivia;Colombia;Costa Rica;CuraÃ§ao;Dominican Republic;Ecuador;El Salvador;Guatemala;Haiti;Honduras;Jamaica;Mexico;Nicaragua;Panama;Sint Maarten (Dutch part);Suriname;Trinidad and Tobago;Venezuela; RB;Virgin Islands (U.S.)"
          ) ~ "Multi-country: Americas (n = 21)",
          population_country %in% c(
            "Argentina, Austria, Barbados, Bolivia, Cabo Verde, Costa Rica, Denmark, Dominican Republic, Ecuador, El Salvador, Fiji, Finland, Guatemala, Haiti, Honduras, Jamaica, Mexico, Netherlands, Nicaragua, Panama, Paraguay, Peru, Portugal, Puerto Rico, Samoa, Spain, Suriname, Switzerland, Taiwan, China, Tonga, United Kingdom, Vanuatu, Venezuela, RB, Virgin Islands (U.S.)"
          ) ~ "Multi-country: Americas (n = 20), Europe (n = 8), Asia (n = 6), Africa (n = 1)",
          population_country %in% c(
            "Antigua and Barbuda;Argentina;Aruba;Bahamas; The;Barbados;Belize;Bolivia;Brazil;Colombia;Costa Rica;Cuba;CuraÃ§ao;Dominican Republic;Ecuador;El Salvador;Grenada;Guatemala;Guyana;Haiti;Honduras;Jamaica;Mexico;Nicaragua;Panama;Paraguay;Peru;Puerto Rico;St. Vincent and the Grenadines;Suriname;Trinidad and Tobago;Venezuela; RB;Virgin Islands (U.S.)"
          ) ~ "Multi-country: Americas (n = 32)",
          is.na(population_country) ~ "Unspecified",
          TRUE ~ population_country
        )
    ) 
  
  # Clean delays 
  df <- zika_clean_delays(df)
  
  # Clean genomic data 
  df <- zika_clean_genomics(df)
  
  # Clean seroprevalence
  df <- zika_clean_serop(df)
  
  # Exponent
  # exponent = 
  #   case_when(
  #     parameter_class %in% "Mutations" &
  #       covidence_id %in% 6340 ~ -3,
  #     parameter_class %in% "Attack rate" &
  #       covidence_id %in% c(241, 2240, 2241, 6472, 7199) ~ 0,
  #     TRUE ~ exponent
  #   ),
  
  # Parameter_unit cleaning
  df <- df %>%
    mutate(parameter_unit =
             case_when(
               # delays
               parameter_class %in% "Human delay" &
                 parameter_unit %in% "Per day" ~ "Days",
               parameter_class %in% "Human delay" &
                 parameter_unit %in% "Per week" ~ "Weeks",
               TRUE ~ parameter_unit
             )
    ) %>%
    mutate(
      # Inverse parameters
      inverse_param = case_when(
        covidence_id == 5888 & parameter_class %in% "Human delay" ~ TRUE, 
        covidence_id == 6765 & parameter_class %in% "Human delay" ~ TRUE, 
        TRUE ~ as.logical(inverse_param)
      ),
      parameter_type = ifelse(
        inverse_param %in% TRUE,
        paste(parameter_type, " (inverse parameter)"), parameter_type
      ),
      
      # create combined variable for survey date
      survey_start_date =
        case_when(
          if_all(c(
            population_study_start_day,
            population_study_start_month,
            population_study_start_year
          ), is.na) ~ "Unspecified",
          is.na(population_study_start_day) & !is.na(population_study_start_month) ~
            paste(population_study_start_month, population_study_start_year),
          is.na(population_study_start_day) & is.na(population_study_start_month) ~
            paste(population_study_start_year),
          TRUE ~ paste(
            population_study_start_day,
            population_study_start_month,
            population_study_start_year
          )
        ),
      survey_end_date =
        case_when(
          if_all(c(
            population_study_end_day,
            population_study_end_month,
            population_study_end_year
          ), is.na) ~ "Unspecified",
          is.na(population_study_end_day) & is.na(population_study_end_month) ~
            paste(population_study_end_year),
          is.na(population_study_end_day) & !is.na(population_study_end_month) ~
            paste(population_study_end_month, population_study_end_year),
          TRUE ~ paste(
            population_study_end_day,
            population_study_end_month,
            population_study_end_year
          )
        ),
      survey_date =
        case_when(
          survey_start_date == "Unspecified" & survey_end_date == "Unspecified" ~
            "Unspecified",
          survey_start_date != "Unspecified" & survey_end_date == "Unspecified" ~
            paste(survey_start_date, "-", "Unspecified"),
          survey_start_date == "Unspecified" & survey_end_date != "Unspecified" ~
            paste("Unspecified", "-", survey_end_date),
          survey_start_date == survey_end_date ~ paste(survey_start_date),
          TRUE ~ paste(survey_start_date, "-", survey_end_date)
        ))
  
  # Add article id and parameter ids for new parameters
  #     id =
  #       case_when(
  #         covidence_id %in% 104 & is.na(id) ~
  #           unique(id[covidence_id %in% 104 & parameter_type %in% "Risk factors"]),
  #         covidence_id %in% 2548 & is.na(id) ~
  #           unique(id[covidence_id %in% 2548 & !is.na(id)]),
  #         covidence_id %in% 1044 & is.na(id) ~
  #           unique(id[covidence_id %in% 1044 & !is.na(id)]),
  #         TRUE ~ id
  #       )
  #   )
  # 
  # idx <- which(df$covidence_id %in% c(104, 1044, 2548) & is.na(df$parameter_data_id))
  # df$parameter_data_id[idx] <-
  #   random_id(n = length(idx), use_openssl = FALSE)
  
  
  
  return(df)
}


zika_clean_delays <- function(params_df){
  
  df <- params_df %>%
    # clean the other human delays and merge the common ones into parameter_type
    mutate(
      other_delay_start = case_when(
        other_delay_start %in% "Other: Health center visit" ~ "Health center visit",
        other_delay_start %in% c("Infection", "Contact with Primary Case") ~ "Exposure/Infection",
        other_delay_start %in% "Funeral Start" ~ "Funeral start",
        other_delay_start %in% "Positive Test" ~ "Positive test",
        other_delay_start %in% "Sampling date" ~ "Sample collection",
        other_delay_start %in% c("Who notification", "Notification") ~ "Reporting",
        other_delay_start %in% c(
          "Other: Not specified 'duration of illness of those who died'",
          "Other: alert of illness onset"
        ) ~ "Symptom Onset/Fever",
        other_delay_start %in% "Testing" ~ "Test",
        other_delay_start %in% "Other: type timepoint in this text box" ~ NA, 
        TRUE ~ other_delay_start
      ),
      other_delay_end = case_when(
        other_delay_end %in% "Death in the community" ~ "Death in community",
        other_delay_end %in% c("Negative RT-PCR", "Negative Test") ~ "Negative test",
        other_delay_end %in% "Funeral End" ~ "Funeral end",
        other_delay_end %in% "Other: first undetectable viremia" ~ "First undetectable viremia",
        other_delay_end %in% "Other: Enter Timepoint in Text Box" ~ "Other",
        other_delay_end %in% c(
          "Removal from community", "Household quarantine", "Isolation"
        ) ~ "Quarantine",
        other_delay_end %in% "Symptom Resolution" ~ "Recovery/non-Infectiousness",
        other_delay_end %in% c(
          "Detection", "Notification", "Reporting of symptoms",
          "Other: Case report completion", "Who notification",
          "Report (for confirmed cases with known outcomes)",
          "Other: official report",
          "First reporting in country"
        ) ~ "Reporting",
        other_delay_end %in% c("Testing", "EVD Testing") ~ "Test",
        other_delay_end %in% c("Test result", "Result", "Test Results", "Diagnosis/test result") ~ "Test result",
        other_delay_end %in% "Onset of neurological symptoms" ~ "Onset of neurologic symptoms",
        other_delay_end %in% "Seroconversion IgM" ~ "Seroconversion (IgM)",
        other_delay_end %in% "Seroreversion IgM" ~ "Seroreversion (IgM)",
        other_delay_end %in% "Susceptible" ~ "Susceptibility",
        other_delay_end %in% "Seeking Care" ~ "Admission to care",
        other_delay_end %in% "Negative PCR test - urine" ~ "Negative PCR test in urine",
        other_delay_end %in% "Negative PCR test - saliva" ~ "Negative PCR test in saliva",
        other_delay_end %in% "Negative PCR test - serum/plasma" ~ "Negative PCR test in serum/plasma",
        other_delay_end %in% "Other: type timepoint in this text box" ~ NA, 
        TRUE ~ other_delay_end
      ),
      other_delay =
        case_when(
          !is.na(other_delay_start) & other_delay_start != "Other: type timepoint in this text box" ~
            paste(other_delay_start, "to", other_delay_end, sep = " ")
        ),
      # Update parameter type to have 'other' delay names 
      parameter_type =
        case_when(
          !is.na(other_delay) ~ paste("Human delay -", other_delay),
          TRUE ~ parameter_type
        ),
      parameter_type =
        str_replace_all(
          parameter_type, c(
            "care/hospitalisation" = "care",
            "care/hospital" = "care",
            "onset/fever" = "onset",
            "critical care/Icu" = "critical care"
          )
        ),
      parameter_type =
        case_when(
          parameter_type %in% "Exposure/infection to infectiousness" ~
            "Latent period",
          parameter_type %in% "Exposure/infection to symptom onset" ~
            "Incubation period",
          parameter_type %in% "Time in care (length of stay)" ~
            "Admission to care to death/discharge",
          TRUE ~ parameter_type
        ),
      parameter_type = str_replace(parameter_type, "\\bwho\\b", "WHO"),
      parameter_type = str_replace(parameter_type, "\\bWho\\b", "WHO"),
      parameter_type = str_replace(parameter_type, "\\brna\\b", "RNA"),
      parameter_type = str_replace(parameter_type, "\\bzikv\\b", "ZIKV"),
      parameter_type = str_replace(parameter_type, "\\bigm\\b", "IgM"),
      parameter_type = str_replace(parameter_type, "\\bigg\\b", "IgG"),
      parameter_type = str_replace(parameter_type, "\\bNaat\\b", "NAAT"),
      parameter_type = str_replace(parameter_type, "\\bpcr\\b", "PCR"),
      parameter_type = str_replace(parameter_type, "\\bIcu\\b", "ICU"),
      parameter_type = str_replace(parameter_type, "igg antibody detection", "antibody detection (IgM/IgG)"),
      parameter_type = str_replace(parameter_type, "igm antibody detection", "antibody detection (IgM/IgG)"),
      parameter_type = str_replace(parameter_type, ">", " to ")) 
  
  # Make shortened version of delay name
  df <- df %>%
    mutate(delay_short =
             case_when(
               parameter_class %in% "Human delay" ~
                 gsub("^Human delay - ", "", parameter_type),
               TRUE ~ NA
             ),
           delay_short = str_to_sentence(delay_short),
           delay_start =
             case_when(
               startsWith(delay_short, "Admission to care") ~ "Admission to care",
               startsWith(delay_short, "Admission to critical care") ~ "Admission to critical care",
               startsWith(delay_short, "Symptom onset") ~ "Symptom onset",
               startsWith(delay_short, 'Onset') ~ "Symptom onset",
               startsWith(delay_short, "Death to burial") ~ "Death to burial",
               startsWith(delay_short, "Exposure/infection") ~ "Exposure/infection",
               startsWith(delay_short, "Recovery/non-infectiousness") ~ "Recovery",
               delay_short %in%
                 c(
                   "Incubation period", "Latent period", "Infectious period",
                   "Generation time", "Serial interval"
                 ) ~ "Infection process",
               TRUE ~ "Other"
             ))
  
  return(df)
}

zika_clean_genomics <- function(params_df){
  
  df <- params_df %>%
    mutate(
      # Edit name of genome sites 
      genome_site = case_when(
        genome_site %in% c('wgs','WGS') ~ 'Whole genome sequence',
        TRUE ~ genome_site
      ),
      # Fix incorrect parameter unit 
      parameter_unit = case_when(
        covidence_id == 7717 & parameter_type == "Mutations - evolutionary rate" ~ "Substitutions/site/year",
        TRUE ~ parameter_unit
      )
    ) %>%
    # Remove empty row 
    filter(!(covidence_id == 4438 & parameter_type == "Mutations – mutation rate" & is.na(parameter_value)))
}

zika_clean_serop <- function(params_df){
  
  ID_PRNT_onELISA <- c(11011, 162, 6270, 10252, 6197, 1352, 10625, 5895, 3451, 5714, 4381, 
                       6501, 5846, 6591, 6972, 17988, 5715, 10094, 1674, 7491, 1121, 7048, 
                       12555, 5667, 9768, 23326, 6762, 4447, 6432) #not all of these have uncorrect den
  
  ID_unspecified_country <- c(10999, 5951, 6182, 5971, 6197, 6681)  #identified with filtered_params <- params_all %>% filter(grepl("^Seroprevalence.+", parameter_type))  %>% filter(is.na(Population_country))
 
    df <- params_df %>%
    
      mutate(
        # Add a new col that specifies if PRNT on positive ELISA only
        prnt_on_elisa = ifelse(covidence_id %in% ID_PRNT_onELISA & parameter_type == "Seroprevalence - Neutralisation/PRNT", "V", "F"),
        
        #Change denominator when PRNT on ELISA pos only but den currently reports total tested samples
        cfr_ifr_denominator = ifelse(covidence_id == 162 & parameter_type == "Seroprevalence - Neutralisation/PRNT",  31, cfr_ifr_denominator),
        cfr_ifr_numerator = ifelse(covidence_id == 162 & parameter_type == "Seroprevalence - Neutralisation/PRNT",  1, cfr_ifr_numerator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 6197 & parameter_type == "Seroprevalence - Neutralisation/PRNT",  229, cfr_ifr_denominator),
        
        cfr_ifr_denominator = ifelse(covidence_id ==5895 & parameter_type == "Seroprevalence - Neutralisation/PRNT",  83, cfr_ifr_denominator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 3451 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 13, 16, cfr_ifr_denominator),
        cfr_ifr_numerator = ifelse(covidence_id == 3451 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 13, 12, cfr_ifr_numerator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 3451 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 18, 15, cfr_ifr_denominator),
        cfr_ifr_numerator = ifelse(covidence_id == 3451 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 18, 5, cfr_ifr_numerator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 4381 & parameter_type == "Seroprevalence - Neutralisation/PRNT",  36, cfr_ifr_denominator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 6501 & parameter_type == "Seroprevalence - Neutralisation/PRNT", 144, cfr_ifr_denominator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 5846 & parameter_type == "Seroprevalence - Neutralisation/PRNT", 82, cfr_ifr_denominator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 6591 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 2,  8, cfr_ifr_denominator),
        cfr_ifr_denominator = ifelse(covidence_id == 6591 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 7,  11, cfr_ifr_denominator),
        cfr_ifr_denominator = ifelse(covidence_id == 6591 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 47,  85, cfr_ifr_denominator),
        cfr_ifr_denominator = ifelse(covidence_id == 6591 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 14,  23, cfr_ifr_denominator),
        cfr_ifr_denominator = ifelse(covidence_id == 6591 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 15,  25, cfr_ifr_denominator),
        cfr_ifr_denominator = ifelse(covidence_id == 6591 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 6,  12, cfr_ifr_denominator),
        cfr_ifr_denominator = ifelse(covidence_id == 6591 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 29,  105, cfr_ifr_denominator),
        cfr_ifr_denominator = ifelse(covidence_id == 6591 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 22,  53, cfr_ifr_denominator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 17988 & parameter_type == "Seroprevalence - Neutralisation/PRNT",  5, cfr_ifr_denominator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 10094 & parameter_type == "Seroprevalence - Neutralisation/PRNT",  6, cfr_ifr_denominator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 1674 & parameter_type == "Seroprevalence - Neutralisation/PRNT",  65, cfr_ifr_denominator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 7048 & parameter_type == "Seroprevalence - Neutralisation/PRNT",  21, cfr_ifr_denominator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 12555 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 48,  55, cfr_ifr_denominator),
        cfr_ifr_denominator = ifelse(covidence_id == 12555 & parameter_type == "Seroprevalence - Neutralisation/PRNT" & cfr_ifr_numerator == 87, 93 , cfr_ifr_denominator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 23326 & parameter_type == "Seroprevalence - Neutralisation/PRNT",  47, cfr_ifr_denominator),
        
        cfr_ifr_denominator = ifelse(covidence_id == 4447 & parameter_type == "Seroprevalence - Neutralisation/PRNT",  4, cfr_ifr_denominator),
        
        # Change serop-Unspecified to specific type
        parameter_type = ifelse(covidence_id == 5846 & parameter_type == "Seroprevalence - NS1 BOB ELISA" , "Seroprevalence - NS1 BOB ELISA" , parameter_type),
        parameter_type = ifelse(covidence_id ==  & parameter_type == "Seroprevalence - ZIKV EDII"  , "Seroprevalence - Biotinylated-EDIII antigen capture ELISA" , parameter_type),
        parameter_type = ifelse(covidence_id %in% c(5890, 2578) & parameter_type == "Seroprevalence - Unspecified" , "Seroprevalence - IgG and IgM", parameter_type),
        parameter_type = ifelse(covidence_id == 3589 & parameter_type == "Seroprevalence - Unspecified" , "Seroprevalence - IgM", parameter_type),
        parameter_type = ifelse(covidence_id == 10697 & parameter_type == "Seroprevalence - Unspecified" , "Seroprevalence - MIA", parameter_type), 
        parameter_type = ifelse(covidence_id %in% c(483, 7252, 23230) & parameter_type == "Seroprevalence - Unspecified" , "Seroprevalence - NS1 BOB ELISA", parameter_type),
        parameter_type = ifelse(covidence_id == 6072 & parameter_type == "Seroprevalence - Unspecified" , "Seroprevalence - Neutralisation/PRNT", parameter_type),
        
        #Specify country in locations defined as "Unspecified"
        population_country = ifelse(covidence_id == 5971 & population_location ==  "22 municipalities of French Guiana" , "France" , population_country),
        population_country = ifelse(covidence_id == 5951 & population_location ==  "Recife"  , "Brazil" , population_country),
        population_country = ifelse(covidence_id == 10999 & population_location ==  "Adamawa state", "Nigeria" , population_country),
        population_country = ifelse(covidence_id == 10999 & population_location ==  "Borno state", "Nigeria" , population_country),
        population_country = ifelse(covidence_id == 10999 & population_location ==  "Bauchi state" , "Nigeria" , population_country),
        population_country = ifelse(covidence_id == 6182 & population_location ==  "Sao Paulo" , "Brazil" , population_country),
        population_country = ifelse(covidence_id == 6197 & population_location ==  "Ouagadougou; Bobo-Dioulasso", "Burkina Faso" , population_country),
        population_country = ifelse(covidence_id == 6681, "Cabo Verde" , population_country)

      )
    
    return(df)
}
    
# Commented out because it is in cleaning.R script now
# #################################
# # Add QA scores to article data #
# #################################
# 
# # input: articles data and parameter data
# # output: articles data with two new variables: model_only and article_qa_score
# 
# add_qa_scores <- function(articles_df, params_df) {
#   # add a model_only variable to article data (as denominator for qa will be different)
#   articles_df <- articles_df %>%
#     mutate(
#       model_only =
#         as.numeric(!id %in% params_df$id)
#     ) %>%
#     # if an article is model_only, make 5-7 NA for consistency between scores --  removed for Zika
#     # mutate(
#     #   qa_d5 = ifelse(model_only %in% 1, NA, qa_d5),
#     #   qa_d6 = ifelse(model_only %in% 1, NA, qa_d6),
#     #   qa_d7 = ifelse(model_only %in% 1, NA, qa_d7)
#     # ) %>%
#     # add qa score to article data
#     mutate(
#       total_qa =
#         rowSums(!is.na(
#           select(., qa_m1, qa_m2, qa_a3, qa_a4, qa_d5, qa_d6, qa_d7)
#         )),
#       yes_score = rowSums(
#         select(., qa_m1, qa_m2, qa_a3, qa_a4, qa_d5, qa_d6, qa_d7) == "Yes",
#         na.rm = TRUE
#       ),
#       article_qa_score = ifelse(total_qa > 0, yes_score / total_qa * 100, NA)
#     ) %>%
#     select(-c(total_qa, yes_score)) %>%
#     # Remove observation without qa score (second extractor)
#     group_by(covidence_id) %>%
#     filter(!(is.na(article_qa_score) & sum(!is.na(article_qa_score)) > 0)) %>%
#     ungroup
# 
#   articles_df
# }
