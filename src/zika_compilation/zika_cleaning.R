# Functions for cleaning data and adding QA scores
library(dplyr)
library(janitor)
library(rio)
library(tidyr)
library(stringr)
library(ids)


# Covidence ID and filtering out papers that should be removed fixing function
fix_cov_ids <- function(df, pathogen){

  df <- df %>%
    mutate(# Fix incorrect covidence id
      covidence_id = case_when(
        covidence_id == 6238 ~ 6246,
        TRUE ~ covidence_id)
      ) %>%
    filter(!covidence_id %in% c(7033, 3491,  1663, 66, 4338, 4340)) %>%# this is a Research Letter, commeents and not pars
    filter(covidence_id != 5749) %>% # this is the same as 873
    filter(covidence_id != 10652) %>% # this is the same as 2302
    filter(covidence_id != 3077) %>%# this isa Brief Report (wrong study type)
    filter(covidence_id != 1475) %>%# research letter
    filter(covidence_id != 10009) # this is using the exact same data as 6993 and cites it


  return(df)
}

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

  # Fix any missing covidence IDs
  df <- fix_cov_ids(df, pathogen = "ZIKA")

  df <- df  %>%
    # Fix incorrect year
    mutate(year_publication= ifelse(covidence_id == 10125, 2020, year_publication))

  df <- df %>%
    mutate(across(.cols = where(is.character), .fns = ~dplyr::na_if(.x, "NA"))) %>%
    select(-c("article_id")) %>%#, "name_data_entry"
    rename(first_author_surname = first_aauthor_surname) %>%
    relocate(c(
      id, covidence_id, pathogen,
      first_author_first_name, first_author_surname
    )) %>%
    arrange(covidence_id) %>%
    # Fix article with group name in first name column
    mutate(first_author_surname = case_when(
      covidence_id == 4427 ~ "Singapore Zika Study Group",
      covidence_id == 8019 ~ "Mishra",
      TRUE ~ first_author_surname)) %>%
    # Surname and first names were mixed up
    mutate(first_author_first_name = ifelse(covidence_id == 927, 'J', first_author_first_name),
           first_author_surname = ifelse(covidence_id == 927, 'Rocklov', first_author_surname)) %>%
    # Make Names title case
    mutate(first_author_surname = str_to_title(first_author_surname),
           first_author_first_name = str_to_title(first_author_first_name)) %>%
    # Fix article with group name in first name column and other name issues
    mutate(
      first_author_surname = case_when(
        covidence_id == 4427 ~ "Singapore Zika Study Group",
        covidence_id == 674 ~ "Sebrango-Rodriguez",
        covidence_id == 1508 ~ "O'Reilly",
        covidence_id == 1941 ~ "Lourenco",
        TRUE ~ first_author_surname),
      first_author_first_name = case_when(
        covidence_id == 674 ~ "C.R.",
        covidence_id == 24965 ~ "F.B.",
        covidence_id == 22284 ~ "C.S.",
        covidence_id == 4308 ~ "Allard",
        covidence_id == 564 ~ "Y.A",
        covidence_id == 890 ~ "D.P.",
        covidence_id == 1993 ~ "J.G.",
        covidence_id == 3100 ~ "W.E.",
        covidence_id == 4365 ~ "F.B.",
        covidence_id == 4464 ~ "I.I.",
        covidence_id == 5530 ~ "O.C.",
        covidence_id == 5535 ~ "M.S.C.",
        covidence_id == 5537 ~ "M.A.",
        covidence_id == 5543 ~ "A.K.T.",
        covidence_id == 5552 ~ "L.P.",
        covidence_id == 5572 ~ "S.M.",
        covidence_id == 5621 ~ "F.X.",
        covidence_id == 5665 ~ "E.O.",
        covidence_id == 5694 ~ "Y.C.",
        covidence_id == 6585 ~ "T.M.",
        covidence_id == 6618 ~ "P.C.",
        covidence_id == 6658 ~ "L.A.",
        covidence_id == 6670 ~ "S.M.",
        covidence_id == 6744 ~ "C.C.",
        covidence_id == 6762 ~ "S.N.",
        covidence_id == 7050 ~ "J.K.",
        covidence_id == 7090 ~ "M.H.",
        covidence_id == 7416 ~ "A.G.",
        covidence_id == 7448 ~ "L.E.",
        covidence_id == 7464 ~ "A.R.",
        covidence_id == 7491 ~ "M.M.N.",
        covidence_id == 7503 ~ "N.S.",
        covidence_id == 7892 ~ "Anneke L",
        covidence_id == 8302 ~ "T.A.",
        covidence_id == 8608 ~ "J.M.",
        covidence_id == 9768 ~ "S.B.",
        covidence_id == 9777 ~ "A.S.",
        covidence_id == 9881 ~ "C.H.A.",
        covidence_id == 10009 ~ "S.F.",
        covidence_id == 10010 ~ "H.A.",
        covidence_id == 10094 ~ "M.J.",
        covidence_id == 10125 ~ "A.A.",
        covidence_id == 10429 ~ "E.J.M.",
        covidence_id == 10437 ~ "I.R.",
        covidence_id == 10495 ~ "A.C.",
        covidence_id == 10509 ~ "V.V.",
        covidence_id == 10561 ~ "G.C.",
        covidence_id == 10697 ~ "G.A.",
        covidence_id == 10743 ~ "S.E.",
        covidence_id == 11133 ~ "B.B.",
        covidence_id == 11297 ~ "R.A.A.",
        covidence_id == 11328 ~ "P.M.S.",
        covidence_id == 11434 ~ "C.M.",
        covidence_id == 11438 ~ "A.R.S.",
        covidence_id == 11493 ~ "G.O.",
        covidence_id == 11565 ~ "R.E.",
        covidence_id == 11575 ~ "J.D.",
        covidence_id == 11720 ~ "F.A.",
        covidence_id == 11853 ~ "I.J.A.A.",
        covidence_id == 11900 ~ "R.S.",
        covidence_id == 11966 ~ "J.P.",
        covidence_id == 12064 ~ "A.O.",
        covidence_id == 12074 ~ "C.L.",
        covidence_id == 12202 ~ "H.J.",
        covidence_id == 12234 ~ "R.A.",
        covidence_id == 12630 ~ "J.T.",
        covidence_id == 12798 ~ "B.S.",
        covidence_id == 22284 ~ "C.S.",
        covidence_id == 24965 ~ "F.B.",
        covidence_id == 25075 ~ "K.M.",
        TRUE ~ first_author_first_name
      )) %>%
    # Add article label
    mutate(
      article_label = as.character(
        paste0(first_author_surname, " ", year_publication)
      )
    )

  ######################################
  # Pathogen-specific article cleaning #
  ######################################

  #
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

  return(df)
}

#################
# Model cleaning #
##################
zika_clean_models <- function(df, pathogen){

  # Fix any missing covidence IDs
  df <- fix_cov_ids(df, pathogen = "ZIKA")

  df <- df %>%
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

  # Fix stochastic/deterministic determination
  df <- df %>%
    mutate(stoch_deter = case_when(
      covidence_id == 3403 & model_type == 'Branching process' ~ "Stochastic",
      is.na(stoch_deter) ~ "Unspecified",
      TRUE ~ stoch_deter
    ))


  df <- df %>% select(-c("access_model_id"))

  #cleaning based on extractors notes
  df <- zika_clean_mod_notes(df)
  return(df)
}

#####################
# Outbreak cleaning #
#####################

zika_clean_outbreaks <- function(df, pathogen){

  # Fix any missing covidence IDs
  df <- fix_cov_ids(df, pathogen = "ZIKA")

  df <- df %>%
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
        outbreak_country == 'Puerto Rico' ~ 'Puerto Rico',
        outbreak_location == "Entire Country" ~ NA,
        outbreak_location == 'Rajastan' ~ 'Rajasthan',
        outbreak_location == 'Australes' | outbreak_location == "Polynesia: Austral Islands (AUS)"
        ~ "Austral Islands, French Polynesia",
        outbreak_location == 'Moorea' | outbreak_location == "Polynesia: Mo'orea Island (MOO)"
        ~ "Mo'orea, French Polynesia",
        outbreak_location == 'Iles Sous-Le-Vent' | outbreak_location == 'Iles sous-le-vent' |
          outbreak_location == "Polynesia: Sous-le-vent Islands (SLV)" | outbreak_location=='Sous-Le-Vent Islands'
        ~ 'Sous-le-vent Islands, French Polynesia',
        outbreak_location == "Marquises" | outbreak_location == "Polynesia: Marquesas Islands (MRQ)"
        ~ "Marquesas Islands, French Polynesia",
        outbreak_location == 'Shastri Nagar And Surrounding Area Jaipur;\r\nRajasthan' ~ "Jaipur, Rajasthan", # very specific neighborhood which is not relevant; switch to writing it this way
        outbreak_location == "West Indies: Guadelopue (GLP)" ~ 'Guadeloupe',
        outbreak_location == "West Indies: Martinique (MTQ)" ~ "Martinique",
        outbreak_location == "West Indies: Saint-Martin (MAF)" ~ "Saint Martin",
        outbreak_location == "Polynesia: Tuamotus (TUA)" ~ "Tuamotus, French Polynesia",
        outbreak_location == 'Tuamotu-Gambier' ~ "Tuamotu-Gambier, French Polynesia",
        outbreak_location == "Polynesia: Tahiti (TAH)" | outbreak_location == "Tahiti" ~ "Tahiti, French Polynesias",
        outbreak_location == "Cucuta" ~ "Cúcuta",
        outbreak_location == 'St. Croix; St. John; St. Thomas;' ~ 'St. Croix, St. John, St. Thomas',
        outbreak_location == 'West Indies: Guadeloupe and Martinique' ~ "Guadeloupe, Martinique",
        TRUE ~ outbreak_location
      ),
      # outbreak_location = case_when(
      #   outbreak_location %in% c("Austral Islands", "Marquesas Islands",
      #                            "Mo'orea", "Sous-le-vent Islands","Sous-Le-Vent Islands",
      #                            "Tahiti", "Tuamotus") ~ paste(outbreak_location, ", French Polynesia"),
        # TRUE~ outbreak_location
      # )
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
        outbreak_location == 'Puerto Rico'~ "United States",
        outbreak_location %in% c("Austral Islands, French Polynesia", "Marquesas Islands, French Polynesia",
                                                       "Mo'orea, French Polynesia, French Polynesia", "Sous-le-vent Islands, French Polynesia","Sous-Le-Vent Islands, French Polynesia",
                                                       "Tahiti, French Polynesia", "Tuamotus, French Polynesia") ~ 'France',
        outbreak_location == 'Entire Country' ~ NA,
        outbreak_location == "Martinique" ~ "France",
        outbreak_location == "Guadeloupe" ~ "France",
        outbreak_location == "Saint Martin" ~ "France and the Netherlands",
        outbreak_location == 'French Guiana' ~ 'France',
        outbreak_location == 'Martinique, Guadeloupe, French Guiana' ~ 'France',
        outbreak_location == "Guadeloupe; Martinique" ~ "France",
        grepl("Puerto Rico", outbreak_location) ~ 'United States',
        # If Yap, should be Micronesia not French Polynesia
        outbreak_location == 'Yap Island' ~ "Federated States of Micronesia",
        # Add France as country for French Polynsia and French Guinea and New Caledonia
        grepl("French Polynesia", outbreak_location) | outbreak_location == 'Sous-le-vent Islands' ~ "France",
        outbreak_location == 'New Caledonia' ~ "France",
        grepl('Croix', outbreak_location) | grepl('St. John', outbreak_location) | grepl('St. Thomas', outbreak_location) ~ "United States",
        TRUE ~ outbreak_country
      ))

  df <- df %>%
    # Fix type of case
    mutate(cases_unspecified = ifelse(covidence_id == 6163, NA, cases_unspecified),
           cases_suspected = ifelse(covidence_id == 6163, 40741, cases_suspected),
           cases_mode_detection = ifelse(covidence_id == 6163, 'Confirmed + Suspected', cases_mode_detection)) # these are confirmed+suspected cases which I think we can categorize as suspected

  df <- df %>% select(-c("access_outbreak_id"))
  return(df)
}

######################
# Parameter cleaning #
######################
zika_clean_params <- function(df, pathogen){

  # Fix any missing covidence IDs
  df <- fix_cov_ids(df, pathogen = "ZIKA")

  # # Make sure newer pathogens with correctly spelled variable name is consistent with old ones
  if ('parameter_uncertainty_single_type' %in% colnames(df)) {
    colnames(df)[colnames(df) == 'parameter_uncertainty_single_type'] <- 'parameter_uncertainty_singe_type'
    flag <- TRUE
  } else flag = FALSE

  # Reorder variables
  df <- df %>%
    select(covidence_id, parameter_type, parameter_unit, exponent, parameter_value, parameter_value_type,
           parameter_uncertainty_lower_value, parameter_uncertainty_upper_value,
           parameter_uncertainty_type, parameter_uncertainty_single_value, parameter_uncertainty_singe_type,
           parameter_lower_bound, parameter_upper_bound, genome_site, everything()) %>%
    relocate(pathogen, access_param_id, article_id, id, parameter_data_id, .after = last_col())

  # Update the variable types
  df <- df %>%
    mutate(
      across(.cols = where(is.character), .fns = ~dplyr::na_if(.x, "NA")),
      # Change variable types
      cfr_ifr_numerator = as.integer(cfr_ifr_numerator),
      cfr_ifr_denominator = as.integer(cfr_ifr_denominator),
      population_study_start_day = as.numeric(population_study_start_day),
      across(.cols = c(parameter_value, parameter_lower_bound, parameter_upper_bound,
                       parameter_uncertainty_single_value,
                       parameter_uncertainty_lower_value, parameter_uncertainty_upper_value,
                       parameter_2_value, parameter_2_lower_bound, parameter_2_upper_bound,
                       parameter_2_uncertainty_lower_value, parameter_2_uncertainty_upper_value,
                       parameter_2_uncertainty_single_value,
                       exponent, exponent_2, distribution_2_par1_value, distribution_2_par2_value,
                       distribution_par1_value, distribution_par2_value,
                       parameter_2_sample_paired_lower, parameter_2_sample_paired_upper,
                       population_sample_size),
             .fns = as.numeric),
      across(.cols = c(population_study_start_year, population_study_end_year,
                       population_study_start_day, population_study_end_day),
             .fns = as.integer),
      genomic_sequence_available = as.logical(genomic_sequence_available),
      cfr_ifr_method = ifelse(cfr_ifr_method == 'Naïve','Naive',cfr_ifr_method),
      # Replace ; with ,
      method_disaggregated_by = str_replace_all(method_disaggregated_by, ";", ", "),
      population_location = str_replace_all(population_location, ";", ","),

      # Update parameter type values
      parameter_type = case_when(parameter_type == 'Seroprevalence - PRNT' ~ 'Seroprevalence - Neutralisation/PRNT',
                                 parameter_type == "Growth rate ®" ~"Growth rate (r)",
                                 parameter_type == "Reproduction number (Effective; Re)" ~ "Reproduction number (Effective, Re)",
                                 parameter_type %in%
                                   c("Mutations ‚Äì substitution rate", "Mutations \x96 substitution rate", "Mutations â€“ substitution rate", "Mutations – substitution rate") ~ "Mutations - substitution rate",
                                 parameter_type %in% c("Mutations – mutation rate", "Mutations â€“ mutation rate") ~ "Mutations - mutation rate",
                                 parameter_type == 'Miscarriage rate' ~ "Pregnancy loss probability",
                                 parameter_type == 'Zika congenital syndrome (microcephaly) risk' ~ "Zika congenital syndrome (microcephaly) probability",
                                 parameter_type == "Relative contribution - zoonotic to human" ~ "Relative contribution - vector-borne",
                                 parameter_type == "Relative contribution - human to human" ~ "Relative contribution - sexual",
                                 parameter_type == 'Reproduction number (Basic R0) - Human' ~ "Reproduction number (Basic R0) - Sexual",
                                 parameter_type == 'Reproduction number (Basic R0) - Mosquito' ~ "Reproduction number (Basic R0) - Vector-borne",
                                 parameter_type == 'Reproduction number (Effective; Re) - Human' ~ 'Reproduction number (Effective; Re) - Sexual',
                                 parameter_type == 'Reproduction number (Effective; Re) - Mosquito' ~ 'Reproduction number (Effective; Re) - Vector-borne',
                                 TRUE ~ parameter_type)) %>%
    # Change parameter value type central-unspecified to just Central
    mutate(parameter_value_type = ifelse(parameter_value_type == 'Central - unspecified', "Central", parameter_value_type)) %>%
    # Fix incorrect parameter types
    mutate(parameter_type = case_when(
      parameter_type == "Seroprevalence - Unspecified" & covidence_id == 6072 ~ "Risk factors",
      parameter_type == "Zika congenital syndrome (microcephaly) probability" & covidence_id == 12202 ~ "Risk factors",
      covidence_id == 1954 & parameter_type == 'Mutations - mutation rate' ~ "Mutations - substitution rate",
      TRUE ~ parameter_type
    )) %>%
    # Remove overdispersion
    filter(parameter_type != 'Overdispersion')

  # Make parameter class
  df <- df %>%
    # Group parameters
    mutate(parameter_class = case_when(
      grepl("Human delay", parameter_type) ~ "Human delay",
      grepl("Seroprevalence", parameter_type) ~ "Seroprevalence",
      grepl("Mutations", parameter_type) ~ "Mutations",
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
    )) %>%
    relocate(c(id, parameter_data_id, covidence_id, pathogen)) %>%
    arrange(covidence_id)

  # Round, combine uncertainty and bounds
  df <- df %>%
    mutate(
      across(
        c(
          parameter_value, parameter_lower_bound, parameter_upper_bound,
          parameter_uncertainty_single_value, parameter_uncertainty_lower_value, parameter_uncertainty_upper_value,
          parameter_2_value, parameter_2_lower_bound, parameter_2_upper_bound,
          parameter_2_uncertainty_single_value, parameter_2_uncertainty_lower_value, parameter_2_uncertainty_upper_value,
        ),
        ~ ifelse(!parameter_class %in% c("Mutations", "Attack rate", "Overdispersion"), round(., digits = 4),
                 ifelse(parameter_class %in% c("Attack rate", "Overdispersion"), round(., digits = 3), .)
        )
      ),
      # Combine central upper and lower bounds
      parameter_bounds =
        ifelse(!is.na(parameter_lower_bound) & !is.na(parameter_upper_bound),
               paste(parameter_lower_bound, "-", parameter_upper_bound),
               NA
        ),
      # Uncertainty type
      parameter_uncertainty_type = case_when(
        parameter_uncertainty_type %in%
          "CI95%" ~ "95% CI",
        parameter_uncertainty_type %in%
          "CRI95%" ~ "95% CrI",
        parameter_uncertainty_type %in%
          "CI90%" ~ "90% CI",
        parameter_uncertainty_type %in%
          "CRI90%" ~ "90% CrI",
        parameter_uncertainty_type %in%
          "Highest Posterior Density Interval 95%" ~ "HPDI 95%",
        parameter_uncertainty_type %in%
          "Inter Quartile Range (IQR)" ~ "IQR",
        TRUE ~ parameter_uncertainty_type
      ),
      # Single uncertainty type
      parameter_uncertainty_singe_type = case_when(
        parameter_uncertainty_singe_type %in%
          "Standard deviation (Sd)" ~ "Standard Deviation",
        parameter_uncertainty_singe_type %in%
          "Standard Error (SE)" ~ "Standard Error",
        TRUE ~ parameter_uncertainty_singe_type
      ),

      # Combine uncertainty types and values
      comb_uncertainty_type =
        case_when(
          !is.na(parameter_uncertainty_lower_value) ~
            paste(parameter_uncertainty_type),
          !is.na(parameter_uncertainty_single_value) ~
            paste(parameter_uncertainty_singe_type),
          TRUE ~ NA
        ), comb_uncertainty =
        case_when(
          !is.na(parameter_uncertainty_lower_value) & !is.na(parameter_uncertainty_upper_value) ~
            paste(parameter_uncertainty_lower_value, "-", parameter_uncertainty_upper_value),
          !is.na(parameter_uncertainty_single_value) ~
            paste(parameter_uncertainty_single_value),
          TRUE ~ NA
        ),

      # shorten a longer method_r name
      method_r =
        case_when(method_r %in% "Renewal equations / Branching process" ~ "Branching process",
                  covidence_id == 3152 & parameter_type == 'Reproduction number (Basic R0)' ~ "Growth rate",
                  TRUE ~ method_r
        ))

  # Context cleaning
  df <- df %>%
    mutate(
      # Population group
      population_group = case_when(
        population_group == 'Persons under investigatioPersons under investigationPersons under investigation' ~ 'Persons under investigation',
        grepl('Other', population_group) ~ "Other",
        grepl('Fertile', population_group) ~ "Other",
        population_group %in% c("Pregnant women - mothers of cases","Pregnant women - mothers of controls","Pregnant women - Zika cases") ~ "Pregnant women",
        population_group %in% c('Outdoor workers','Sex workers', 'Within 100m of index case households') ~ "Other",
        TRUE ~ population_group
      )) %>%
    mutate(
      # Population location
      population_location = str_replace(
        population_location, "Ribeirao",
        "Ribeirão"
      ),
      population_location = str_replace(
        population_location, "Sao Paulo",
        "São Paulo"
      ),
      population_location = str_replace(
        population_location, "Sao Paolo",
        "São Paulo"
      ),
      population_location = str_replace(
        population_location, ", Laneri model",
        ""
      ),
      population_location = str_replace(
        population_location, "Laneri model",
        ""
      ),
      population_location = str_replace(
        population_location, ", SEIR-SEI model",
        ""
      ),
      population_location = str_replace(
        population_location, "SEIR-SEI model",
        ""
      ),
      population_location = str_replace(
        population_location, "Moorea",
        "Mo'orea"
      ),
      population_location  = str_replace(
        population_location, "Marquises",
        "Marquesas Islands"
      ),
      population_location = str_replace(
        population_location, "Leon",
        "León"
      ),
      population_location = str_replace(
        population_location, "Juniai",
        "Jundiai"
      ),
      population_location = str_replace(
        population_location, "Hospital\r\n",
        ""
      ),
      population_location = str_replace(
        population_location, "\r\n",
        " "
      ),
      population_location = case_when(
        population_location == 'Polynesia and West Indies' ~ "French Polynesia and French West Indies",
        population_country == 'New Caledonia' ~ 'New Caledonia',
        population_location == 'Saint Lucia' ~ "Saint Lucia",
        # population_location == 'French Polynesia' & population_country == 'French Polynesia' ~ NA,
        covidence_id == 6211 ~ "Singapore",
        population_location %in%
          c("Salvador; Bahia", "Salvador city in Bahia","Salvador") ~ "Salvador, Bahia",
        population_location %in% "Salvador (Hospital Geral\r\nRoberto Santos)" ~ "Salvador, Bahia (Hospital Geral Roberto Santos)",
        population_location %in% "Saint-Martin" ~ "Saint Martin",
        population_location %in% c("Rio de Janeiro (city)", "Rio de Janeiro", "Rio de janeiro", "Rio  de Janeiro") ~
          "Rio de Janeiro",
        population_location == 'Ponce' ~ 'Ponce, Puerto Rico',
        population_location %in% c("Ponce, San Juan, Guayama") ~ "Puerto Rico: Ponce, San Juan, Guayama",
        population_location == 'San Juan metropolitan area' ~ "San Juan, Puerto Rico",
        population_country == 'Puerto Rico' ~ "Puerto Rico",
        population_location %in% "3 states in Nigeria\r\nNigeria; Abia State in Southern Nigeria; and Kaduna\r\nState in Northern Nigeria" ~
          "Nasarawa state, Abia state, Kaduna state",
        population_location %in% c("Sao Paolo State", "São Paulo State") ~ "São Paulo state",
        population_location %in% "San Andres" ~ "San Andrés",
        population_location %in% "risaralda" ~ "Risaralda",
        population_location %in% "Pernambuco" ~ "Pernambuco State",
        population_location %in% "Paraiba" ~ "Paraíba",
        population_location %in% c("Northeastern Brazil", "North-East Brazil") ~ "Northeast Brazil",
        population_location %in% "Maranhao" ~ "Maranhao state",
        population_location %in% c("Ile Sous", "Sous-le-vent","Iles Sous-le-vent") ~ "Sous-le-vent Islands, French Polynesia",
        (population_country == 'French Polynesia') ~ ifelse(!is.na(population_location) & population_location !='French Polynesia', paste0(population_location, ", French Polynesia"), 'French Polynesia'), # add French polynesia to location
        population_location %in% "Hospital\r\nHospital da Restauração; Recife; Pernambuco;" ~
          "Hospital da Restauração, Recife, Pernambuco",
        population_location %in% "Hospital Universitari Vall d’Hebron\r\nBarcelona; Catalonia" ~
          "Hospital Universitari Vall d’Hebron, Barcelona, Catalonia",
        population_location %in% "Guadeloupe,Martinique, French Guiana" ~ "Guadeloupe, Martinique, French Guiana",
        covidence_id == 5907 & population_location == "Asia & Americas" ~ "Americas and Oceania",
        population_location %in% "Entire country" ~ NA,
        population_location == "Yap"~ "Yap Island",
        population_location == 'West Indies: Guadeloupe and Martinique' ~ "Guadeloupe; Martinique",
        population_location == "French Guyana" ~ "French Guiana",
        # Below is from manually looking at genomic analyses - these are multi-country analyses and don't have specific locations
        covidence_id %in% c(3152, 6361, 5908) & parameter_class == "Mutations" ~ NA,
        TRUE ~ population_location
      ),
      population_location = str_replace_all(population_location, ";", ","))

  df <- df %>%
    mutate(
      # Population country
      population_country = str_replace(
        population_country, "Congo; Rep.",
        "Republic of the Congo"
      ),
      population_country = str_replace(
        population_country, "Congo; Dem. Rep.",
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
        population_country, "Gambia; The",
        "The Gambia"
      ),
      population_country = str_replace(
        population_country, "Micronesia; Fed. Sts.",
        'Federated States of Micronesia'
      ),
      population_country = str_replace(
        population_country, "Venezuela; RB",
        "Venezuela"
      ),
      population_country = str_replace(
        population_country, "Taiwan; China",
        "Taiwan"
      ),
      population_country = str_replace(
        population_country, "Lao PDR",
        "Laos"
      ),
      population_country = str_replace(
        population_country, "Iran; Islamic Rep.",
        "Iran"
      ),
      population_country =
        case_when(
          population_location == "French Polynesia and French West Indies" ~ "France",
          population_location == 'Saint Lucia' ~ NA,
          grepl("Puerto Rico", population_location) ~ 'United States',
          # If Yap, should be Micronesia not French Polynesia
          population_location == 'Yap Island' ~ "Federated States of Micronesia",
          # Add France as country for French Polynsia and French Guinea and New Caledonia
          grepl("French Polynesia", population_location) | population_location == 'Sous-le-vent Islands' ~ "France",
          population_location == 'French Guiana' ~ "France",
          population_location == 'New Caledonia' ~ "France",
          #Specify country in locations defined as "Unspecified"
          covidence_id == 10741 & population_location == 'Ponce' ~ 'United States',
          covidence_id == 4438 & population_location == 'Florida' ~ "United States",
          covidence_id == 5971 & population_location == "22 municipalities of French Guiana" ~ "France" ,
          covidence_id == 5951 & population_location ==  "Recife" ~ "Brazil",
          covidence_id == 10999 & population_location ==  "Adamawa state" ~ "Nigeria",
          covidence_id == 10999 & population_location ==  "Borno state" ~ "Nigeria" ,
          covidence_id == 10999 & population_location ==  "Bauchi state" ~ "Nigeria",
          covidence_id == 6182 & population_location ==  "São Paulo" ~ "Brazil",
          covidence_id == 6197 & population_location ==  "Ouagadougou; Bobo-Dioulasso" ~ "Burkina Faso",
          covidence_id == 6197 & population_location ==  "Ouagadougou, Bobo-Dioulasso" ~ "Burkina Faso",
          covidence_id == 6681 ~ "Cabo Verde",
          covidence_id == 873 & population_location == "Ponce, San Juan, Guayama" ~ 'United States',
          population_location == 'Martinique, Guadeloupe, French Guiana' ~ 'France',
          population_location == 'Guadeloupe, Martinique, French Guiana' ~ 'France',
          # Specify countries in locations for genomic data (then will change to be multi-country...)
          covidence_id == 3152 & parameter_class == 'Mutations' ~ "Brazil, Polynesia, Caribbean, Central America, South America",
          covidence_id == 3490 & parameter_class == "Mutations" ~ "Southeastern Asia (n = 20, 1966-2016), Pacific (n = 23, 2007-2016) and the Americas (n = 22, 2014-2015)",
          covidence_id == 5908 & parameter_class == "Mutations" ~ "Central African Republic, Nigeria, Senegal, Uganda, Malaysia, Cambodia, Thailand, Singapore, French Polynesia, Mexico, Honduras, Guatemala, Panama, Venezuela, Colombia, Ecuador, Brazil, Suriname, American Samoa, Tonga, the Philippines, Micronesia, Puerto Rico, Haiti, Dominican Republic, Martinique, Cuba",
          covidence_id == 6361 & parameter_class == 'Mutations' ~ "USA, Malaysia, Philippines, Singapore, Thailand, Indonesia, China, Brazil, French Polynesia, Yap Island, Venezuela, Samoa, Suriname, Guatemala, Uganda, Senegal, Nigeria",
          covidence_id == 7717 & population_study_start_year == 1966 ~ "Mexico, United States, Nicaragua, Honduras, Haiti, Puerto Rico, Panama, Guatemala, Suriname, Brazil, Venezuela, Colombia, Ecuador, Singapore, Australia, Thailand, China, Japan, French Polynesia, South Korea",
          covidence_id == 7717 & population_study_start_year == 2013 ~ "Uganda, Malaysia, Honduras, Puerto Rico, China, Japan, South Korea, Colombia, Brazil, Nicaragua, Dominican Republic, Philippines, Thailand, Australia, Ecuador, French Polynesia, Austria, Cambodia, Haiti, United States, Panama, Senegal, Singapore, Mexico",
          covidence_id == 10526 & parameter_class == 'Mutations' ~ "Cambodia, Singapore, Myanmar, China, Vietnam, French Polynesia, United States, Puerto Rico, Nicaragua, Haiti, Honduras, Dominican Republic, Mexico, Brazil, French Guiana, Ecuador, Suriname, Venezuela, Colombia, India, Indonesia, Philippines, Micronesia, Malaysia",
          covidence_id == 4328 & parameter_class == 'Mutations' ~ "Multi-country: Americas",
          covidence_id == 5907 & parameter_class == 'Mutations' ~ "Multi-country: Americas, Oceania",
          covidence_id == 6511 & parameter_class == 'Mutations' ~ "Multi-country: Pacific, Americas",
          covidence_id == 6571 & parameter_class == 'Mutations' ~ "Malaysia, Micronesia, Cambodia, Philippines, Thailand, Canada, French Polynesia, Haiti, Honduras, Brazil, Suriname, Colombia, Guatemala, Mexico, Puerto Rico, Panama, Martinique, French Guiana, Nicaragua, Italy, China, Australia, Venezuela, USA, Dominican Republic, Ecuador, United Kingdom, Japan, Singapore",
          covidence_id == 4438 & population_sample_size == 139 ~ "Brazil, USA, other Americas",
          covidence_id == 3154 & parameter_class == 'Mutations' ~ "Russia, Colombia, Brazil, China, Thailand, Indonesia, Mexico, Philippines, Haiti, Martinique, Puerto Rico, Guatemala, Suriname, Cook Islands, New Caledonia, Easter Island, French Polynesia, Indonesia, Malaysia, Cambodia, Micronesia, Senegal, Central African Republic, Nigeria, Uganda, Norway",
          TRUE ~ population_country),
      population_country_original = population_country)

  df <- df %>%
    mutate(population_country = case_when(
          # Clean multi country names
          population_country %in%
            "Belize;Bolivia;Colombia;Costa Rica;Dominican Republic;Ecuador;El Salvador;Guatemala;Honduras;Mexico;Panama;Peru;Puerto Rico" ~
            "Multi-country: South and Central America (n = 13)",
          population_country %in%
            "Belize;Colombia;Costa Rica;Dominican Republic;El Salvador;Guatemala;Honduras;Mexico;Panama;Peru" ~
            "Multi-country: South and Central America (n = 10)",
          population_country %in%
            "Bolivia;Brazil;Ecuador;Nicaragua;Puerto Rico" ~
            "Multi-country: South and Central America (n = 5)",
          population_country %in%
            "El Salvador;Guatemala;Honduras;Mexico;Nicaragua" ~
            "Multi-country: Central America (n = 5)",
          population_country %in%
            "Brazil;Cambodia;Central African Republic;China;Colombia;Guatemala;Haiti;Italy;Malaysia;Mexico;Federated States of Micronesia;Nigeria;Panama;Philippines;Puerto Rico;Senegal;South Africa;Suriname;Thailand;Uganda;United States" ~
            "Multi-country: Americas (n = 9), Asia (n = 5), Oceania (n = 1), Africa (n = 5), Europe (n = 1)",
          population_country %in%
            "Brazil;China;Colombia;Cuba;Dominican Republic;French Polynesia;Haiti;Honduras;Jamaica;Malaysia;Mexico;Nicaragua;Nigeria;Panama;Puerto Rico;Senegal;Thailand;Uganda;United States;Venezuela" ~
            "Multi-country: Americas (n = 13), Asia (n = 3), Africa (n = 2), Oceania (n = 1)",
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
            "Brazil;Colombia;El Salvador;Haiti;Honduras;Mexico;Puerto Rico;Venezuela" ~
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
            'Brazil;Cambodia;China;Colombia;Dominican Republic;French Polynesia;Guatemala;Haiti;Mexico;Federated States of Micronesia;Philippines;Puerto Rico;Suriname;Thailand'
          ) ~ "Multi-country: South and Central America (n = 8), Asia (n = 5), Oceania (n = 1)",
          population_country %in% c(
            "Brazil;Cambodia;Central African Republic;China;French Polynesia;Guatemala;Haiti;Malaysia;Federated States of Micronesia;Nigeria;Philippines;Puerto Rico;Senegal;Suriname;Thailand;Uganda;Venezuela"
          ) ~ "Multi-country: South and Central America (n = 7), Asia (n = 6), Oceania (n = 1), Africa (n = 4)",
          population_country %in% c(
            "Brazil;Cambodia;Central African Republic;China;Colombia;Guatemala;Haiti;Italy;Malaysia;Mexico;Federated States of Micronesia;Nigeria;Panama;Philippines;Puerto Rico;Senegal;South Africa;Suriname;Thailand;Uganda;United States"
          ) ~ "Multi-country: Americas (n = 8), Asia (n = 6), Africa (n = 5), Europe (n = 1)",
          population_country %in% c(
            "Argentina;Barbados;Bolivia;Colombia;Costa Rica;CuraÃ§ao;Dominican Republic;Ecuador;El Salvador;Guatemala;Haiti;Honduras;Jamaica;Mexico;Nicaragua;Panama;Sint Maarten (Dutch part);Suriname;Trinidad and Tobago;Venezuela;Virgin Islands (U.S.)"
          ) ~ "Multi-country: Americas (n = 21)",
          population_country %in% c(
            "Argentina, Austria, Barbados, Bolivia, Cabo Verde, Costa Rica, Denmark, Dominican Republic, Ecuador, El Salvador, Fiji, Finland, Guatemala, Haiti, Honduras, Jamaica, Mexico, Netherlands, Nicaragua, Panama, Paraguay, Peru, Portugal, Puerto Rico, Samoa, Spain, Suriname, Switzerland, Taiwan, China, Tonga, United Kingdom, Vanuatu, Venezuela, RB, Virgin Islands (U.S.)"
          ) ~ "Multi-country: Americas (n = 19), Europe (n = 8), Asia (n = 1), Oceania (n = 4)",
          population_country %in% c(
            "Antigua and Barbuda;Argentina;Aruba;Bahamas; The;Barbados;Belize;Bolivia;Brazil;Colombia;Costa Rica;Cuba;CuraÃ§ao;Dominican Republic;Ecuador;El Salvador;Grenada;Guatemala;Guyana;Haiti;Honduras;Jamaica;Mexico;Nicaragua;Panama;Paraguay;Peru;Puerto Rico;St. Vincent and the Grenadines;Suriname;Trinidad and Tobago;Venezuela;Virgin Islands (U.S.)"
          ) ~ "Multi-country: Americas (n = 32)",
          population_country %in% c(
            "France;Netherlands"
          ) ~ "Multi-country: France and Netherlands",
          population_country %in% c(
            "Puerto Rico;United States"
          ) ~ "Multi-country: Puerto Rico and the United States",
          population_country %in% "Brazil,Ecuador,Federated States of Micronesia" ~
            "Multi-country: Brazil, Ecuador, Federated States of Micronesia",
          # Genomic ones manually cleaned
          population_country == "Brazil, Polynesia, Caribbean, Central America, South America" ~ "Multi-country: Americas, Asia, Oceania",
          population_country == "Southeastern Asia (n = 20, 1966-2016), Pacific (n = 23, 2007-2016) and the Americas (n = 22, 2014-2015)" ~ "Multi-country: Americas, Asia, Oceania",
          population_country %in% c(
            "Central African Republic, Nigeria, Senegal, Uganda, Malaysia, Cambodia, Thailand, Singapore, French Polynesia, Mexico, Honduras, Guatemala, Panama, Venezuela, Colombia, Ecuador, Brazil, Suriname, American Samoa, Tonga, the Philippines, Micronesia, Puerto Rico, Haiti, Dominican Republic, Martinique, Cuba"
          ) ~ "Multi-country: Africa (n = 4), Americas (n = 14), Asia (n = 5), Oceania (n = 3)",
          population_country %in% c(
            "USA, Malaysia, Philippines, Singapore, Thailand, Indonesia, China, Brazil, French Polynesia, Yap Island, Venezuela, Samoa, Suriname, Guatemala, Uganda, Senegal, Nigeria"
          ) ~ "Multi-country: Africa (n = 3), Americas (n = 5), Asia (n = 6), Oceania (n = 3)",
          population_country %in% c(
            "Mexico, United States, Nicaragua, Honduras, Haiti, Puerto Rico, Panama, Guatemala, Suriname, Brazil, Venezuela, Colombia, Ecuador, Singapore, Australia, Thailand, China, Japan, French Polynesia, South Korea"
          ) ~ "Multi-country: Americas (n = 13), Asia (n = 5), Oceania (n = 2)",
          population_country %in% c(
            "Uganda, Malaysia, Honduras, Puerto Rico, China, Japan, South Korea, Colombia, Brazil, Nicaragua, Dominican Republic, Philippines, Thailand, Australia, Ecuador, French Polynesia, Austria, Cambodia, Haiti, United States, Panama, Senegal, Singapore, Mexico"
          ) ~ 'Multi-country: Africa (n = 2), Americas (n = 11), Asia (n = 8), Oceania (n = 2), Europe (n = 1)',
          population_country %in% c(
            "Cambodia, Singapore, Myanmar, China, Vietnam, French Polynesia, United States, Puerto Rico, Nicaragua, Haiti, Honduras, Dominican Republic, Mexico, Brazil, French Guiana, Ecuador, Suriname, Venezuela, Colombia, India, Indonesia, Philippines, Micronesia, Malaysia"
          ) ~ "Multi-country: Americas (n = 13), Asia: (n = 9), Oceania (n = 2)",
          population_country %in% c(
            "Malaysia, Micronesia, Cambodia, Philippines, Thailand, Canada, French Polynesia, Haiti, Honduras, Brazil, Suriname, Colombia, Guatemala, Mexico, Puerto Rico, Panama, Martinique, French Guiana, Nicaragua, Italy, China, Australia, Venezuela, USA, Dominican Republic, Ecuador, United Kingdom, Japan, Singapore"
          ) ~ "Multi-country: Americas (n = 17), Asia (n = 7), Oceania (n = 3), Europe (n = 2)",
          population_country %in% c("Brazil, USA, other Americas") ~ "Multi-country: Americas",
          population_country %in% c(
            "Russia, Colombia, Brazil, China, Thailand, Indonesia, Mexico, Philippines, Haiti, Martinique, Puerto Rico, Guatemala, Suriname, Cook Islands, New Caledonia, Easter Island, French Polynesia, Indonesia, Malaysia, Cambodia, Micronesia, Senegal, Central African Republic, Nigeria, Uganda, Norway"
          ) ~ "Multi-country: Africa (n = 4), Americas (n = 9), Asia (n = 7), Oceania (n = 4), Europe (n = 2)",
          population_location %in% c(
            'French Guiana', '22 municipalities of French Guiana', "Centre Hospitalier de l’Ouest Guyanais (CHOG, Saint-Laurent-du-Maroni, in French Guiana)",
            "western French Guiana"
          ) ~ 'France',# (French Guiana)',
          population_location == 'Martinique, Guadeloupe, French Guiana' ~ 'France',# (Martinique, Guadeloupe, French Guiana)',
          population_location == "Guadeloupe; Martinique" ~ "France",# (Guadeloupe; Martinique)",
          population_location == 'Guadeloupe, Martinique, Saint-Martin' ~ "France",# (Guadeloupe, Martinique, Saint-Martin)",
          is.na(population_country) ~ "Unspecified",
          TRUE ~ population_country
        ),
      population_country = str_replace_all(population_country, ";", ","),
      population_country_original = str_replace_all(population_country_original, ";", ", ")
    )

  # Clean disaggregation variables
  df <- df %>%
    # Fix variables mixed up from the db1 fixing file
    mutate(method_disaggregated = ifelse(method_disaggregated=='Method' & covidence_id == 729, TRUE, as.logical(method_disaggregated)),
           method_disaggregated_by = ifelse(method_disaggregated_by == 'TRUE' & covidence_id == 729, "Method", method_disaggregated_by))

  # Clean delays
  df <- zika_clean_delays(df)

  # Clean genomic data
  df <- zika_clean_genomics(df)

  # Clean seroprevalence
  df <- zika_clean_serop(df)

  #cleaining based on extractors notes
  df <- zika_clean_pars_notes(df)

  # Cleaning reproduction number
  df <- zika_clean_r(df)

  # Clean attack rate
  df <- zika_clean_ar(df)

  # Clean microcephaly risk
  df <- zika_clean_zcs_microcephaly(df)

  # Parameter_unit cleaning
  df <- df %>%
    mutate(parameter_unit =
             case_when(
               # delays
               covidence_id == 430 & parameter_type == "Mosquito delay - extrinsic incubation period (inverse parameter)" ~ "Per day",
               covidence_id == 430 & parameter_type == "Human delay - latent period (inverse parameter)" ~ "Per day",
               parameter_class %in% c("Human delay","Mosquito") &
                 inverse_param == TRUE & parameter_unit == "Days" ~ "Per day",
               parameter_class %in% c("Human delay","Mosquito") &
                 inverse_param == TRUE & parameter_unit == "Days" ~ "Per week",
               covidence_id == 5535 & parameter_type == "Mosquito delay - extrinsic incubation period" ~ "Days",
               # Reproduction numbers
               grepl("Reproduction", parameter_type) & is.na(parameter_unit) ~ "No units",
               # others
               covidence_id == 7047 & parameter_type == "Zika congenital syndrome (microcephaly) probability" ~ "Per 100,000 population",
               covidence_id == 10933 & parameter_type == "Seroprevalence - IgG" ~ "Percentage (%)",
               covidence_id == 1679 & parameter_unit == "per 100;000 population" ~ "Per 100,000 population",
               covidence_id == 4032 & parameter_unit == "Per 10000 population" ~ "Per 10,000 population",
               covidence_id == 4268 & parameter_unit == "Per 100000" ~ "Per 100,000 population",
               parameter_unit == 'Per 1000 births' ~ "Per 1,000 births",
               covidence_id == 6521 & parameter_type == "Zika congenital syndrome (microcephaly) probability" ~ NA, # this has been changed to be num/denomi
               TRUE ~ parameter_unit
             )
    ) %>%
    mutate(
      # Inverse parameters
      inverse_param = case_when(
        covidence_id == 5888 & parameter_class %in% "Human delay" ~ TRUE,
        covidence_id == 6765 & parameter_class %in% "Human delay" ~ TRUE,
        covidence_id == 6670 ~ FALSE,
        TRUE ~ as.logical(inverse_param)
      ),
      parameter_type = ifelse(
        inverse_param %in% TRUE,
        paste(parameter_type, "(inverse parameter)"), parameter_type
      ),

      # Exponents - if missing then it should be 0 since that is the default
      exponent = case_when(
        is.na(exponent) ~ 0,
        # fix incorrect exponent
        covidence_id == 10180 & parameter_type == 'Mutations - evolutionary rate' ~ -3,
        TRUE ~ exponent
      ),
      exponent_2 = case_when(
        is.na(exponent_2) ~ 0,
        TRUE ~ exponent_2
      ),
      # Fix issues with dates for 6270
      population_study_start_year = ifelse(covidence_id ==6270, 2016, population_study_start_year),
      population_study_start_month = ifelse(covidence_id == 6270, 'Jan', population_study_start_month),
      population_study_end_year = ifelse(covidence_id == 6270, 2019, population_study_end_year),
      population_study_end_month = ifelse(covidence_id == 6270, 'Apr', population_study_end_month),
      # Fix missing dates for 430
      population_study_start_year = ifelse(covidence_id == 430, 2015, population_study_start_year),
      population_study_end_year = ifelse(covidence_id == 430, 2016, population_study_end_year),
      # Format start/stop months for parameters
      population_study_start_month = str_sub(population_study_start_month, 1, 3),
      population_study_end_month = str_sub(population_study_end_month, 1, 3),

      population_group = ifelse(is.na(population_group) & covidence_id == 4427, "Persons under investigation", population_group),



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

  #Cleaning of attack rate unit (fixing units)
  df <- df %>%
    filter(!(covidence_id ==  6272 & parameter_type == "Attack rate")) %>%   #no values extracted --> TO REMOVE!
    mutate(parameter_value_type = ifelse(covidence_id %in% c(890, 1154, 1308, 1821, 3221, 3337, 5671, 6670, 7047, 7336, 10749, 11866) & is.na(parameter_value_type) & parameter_type == "Attack rate",
                                         "Unspecified", parameter_value_type),
           parameter_unit = ifelse(covidence_id %in% c(1941, 6321, 6670) & parameter_unit == "No units" & parameter_type == "Attack rate",
                                   "Unspecified", parameter_unit),
           parameter_unit = ifelse(covidence_id %in% c(890, 890, 1154, 1308, 1821, 3337,
                                                       3221,  5671, 6670,  7047,
                                                       7336, 10749, 11866) & is.na(parameter_unit) & parameter_type == "Attack rate",
                                   "Unspecified", parameter_unit))

  #cleaning of relative contributions (fixing units) and growth rate
  df <- df %>%
    mutate(
      parameter_value = ifelse(covidence_id == 422 & parameter_type %in% c("Relative contribution - sexual",
                                                                           "Relative contribution - vector-borne"), parameter_value * 100, parameter_value),
      parameter_uncertainty_upper_value = ifelse(covidence_id == 422 & parameter_type %in% c("Relative contribution - sexual",
                                                                                             "Relative contribution - vector-borne"), parameter_uncertainty_upper_value * 100 , parameter_uncertainty_upper_value),
      parameter_uncertainty_lower_value = ifelse(covidence_id == 422 & parameter_type %in% c("Relative contribution - sexual",
                                                                                             "Relative contribution - vector-borne"), parameter_uncertainty_lower_value * 100 , parameter_uncertainty_lower_value),
      parameter_unit = ifelse(covidence_id == 422  & parameter_type %in% c("Relative contribution - sexual",
                                                                        "Relative contribution - vector-borne"), "Percentage (%)", parameter_unit),
      parameter_unit = ifelse(covidence_id == 422  & parameter_type == "Growth rate (r)", "Per day", parameter_unit)
    )

  #Cleaning of severity (fixing units)
  df <- df %>%
    filter(!(covidence_id == 7437 & parameter_type %in% c("Severity - case fatality rate (CFR)",
                                                       "Severity - proportion of symptomatic cases"))) %>% #cov id 7437 does not  have cfr or symptomatic cases
    mutate(
      population_group = ifelse(covidence_id == 6798 & parameter_type %in% c("Severity - case fatality rate (CFR)",
                                                                            "Severity - proportion of symptomatic cases"),
                               "Persons under investigation", population_group),
      parameter_unit = ifelse(covidence_id == 12128 & parameter_type %in% c("Severity - case fatality rate (CFR)",
                                                                            "Severity - proportion of symptomatic cases"), "Percentage (%)", parameter_unit),
      parameter_value_type = ifelse(covidence_id == 12128 & parameter_type %in% c("Severity - case fatality rate (CFR)",
                                                                                  "Severity - proportion of symptomatic cases"), "Unspecified", parameter_value_type),
      parameter_value_type = ifelse(covidence_id == 3528 & parameter_type %in% c("Severity - case fatality rate (CFR)",
                                                                                 "Severity - proportion of symptomatic cases"), "Unspecified", parameter_value_type))
  # Cleaning of wrongly extracted attack rates

  df <- df %>%
    mutate(
      exponent =  ifelse(covidence_id %in% c(3337, 1941) & parameter_type == "Attack rate", 0, exponent),
      parameter_unit =  ifelse(covidence_id %in% c(3337, 5949) & parameter_type == "Attack rate", "Per 10,000 population", parameter_unit),
      parameter_unit =  ifelse(covidence_id %in% c(3221, 5671, 11866) & parameter_type == "Attack rate", "Percentage (%)", parameter_unit)
    )

   #Removing wrongly extracted symptomatic proportion
  df <- df[!(df$covidence_id == 10318 & df$parameter_type == "Severity - proportion of symptomatic cases" ), ]

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

zika_clean_ar <- function(params_df){ #to convert from epidemic size (written as attack rate)
  params_df <- params_df %>%
    mutate(
      parameter_value = ifelse(covidence_id == 6542 & parameter_type == "Attack rate", cfr_ifr_numerator / cfr_ifr_denominator * 100,
       parameter_value),
      parameter_lower_bound = ifelse(covidence_id == 6542 & parameter_type == "Attack rate", parameter_lower_bound / cfr_ifr_denominator * 100,
                                     parameter_lower_bound),
      parameter_upper_bound = ifelse(covidence_id == 6542 & parameter_type == "Attack rate", parameter_upper_bound / cfr_ifr_denominator * 100,
                                     parameter_upper_bound)) #%>%
    # fix incorrectly entered variables
    # mutate(parameter_uncertainty_lower_value = ifelse(covidence_id == 6542 & !is.na(parameter_lower_bound), parameter_lower_bound, parameter_uncertainty_lower_value),
    #        parameter_uncertainty_upper_value = ifelse(covidence_id == 6542 & !is.na(parameter_upper_bound), parameter_upper_bound, parameter_uncertainty_upper_value),
    #        parameter_lower_bound = ifelse(covidence_id == 6542 & !is.na(parameter_uncertainty_lower_value), NA, parameter_lower_bound),
    #        parameter_upper_bound = ifelse(covidence_id == 6542 & !is.na(parameter_uncertainty_upper_value), NA, parameter_upper_bound)
    #        )
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
            "Care/Hospitalisation" = "care",
            "Care/Hospital" = "care",
            "Onset/Fever" = "Onset",
            "Symptom Onset" = "Onset",
            "Symptom Onset/Fever" = "Onset",
            "Critical Care/Icu" = "critical care"
          )
        ),
      parameter_type =
        case_when(
          parameter_type %in% "Exposure/infection to infectiousness" ~
            "Human delay - latent period",
          parameter_type %in% "Exposure/infection to symptom onset" ~
            "Human delay - incubation period",
          parameter_type %in% "Human delay - time in care (length of stay)" ~
            'Human delay - Admission to care to Discharge from care',
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
      parameter_type = str_replace(parameter_type, ">", " to ")) %>%
    # Combine a few delays
    mutate(parameter_type = case_when(
      parameter_type == "Human delay - admission to care to discharge/recovery" ~ "Human delay - Admission to care to Discharge from care",
      parameter_type == 'Human delay - Symptom Onset to Admission to care' ~ "Human delay - Onset to Admission to care",
      parameter_type == "Human delay - symptom onset to admission to care" ~ "Human delay - Onset to Admission to care",
      parameter_type == 'Human delay - Onset to GeoSentinel clinic presentation' ~ "Human delay - Onset to Admission to care",
      TRUE ~ parameter_type
    ))

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

  # Fix mosquito delays
  df <- df %>%
    mutate(parameter_type = ifelse(covidence_id == 5535 & parameter_type == "Mosquito delay - extrinsic incubation period",
                                   "Mosquito delay - extrinsic incubation period (EIP10)", parameter_type),
           parameter_value_type = case_when(
             covidence_id %in% c(12153, 6106) & parameter_type == "Mosquito delay - extrinsic incubation period" ~ 'Central',
             TRUE ~ parameter_value_type
             )
           )

  # Add in sample sizes
  df <- df %>%
    mutate(population_sample_size = case_when(
      covidence_id == 588 & parameter_type == "Human delay - incubation period" & population_country == 'El Salvador' ~ 11825,
      covidence_id == 588 & parameter_type == "Human delay - incubation period" & population_country == 'Suriname' ~ 3042,
      TRUE ~ population_sample_size
    ))

  # Remove delays that aren't estimated (parameters were fixed in model and originally were extracted but should not have been)
  df <- df %>%
    filter(!(covidence_id == 6765 & grepl('incubation',parameter_type) & population_location %in% c("Sous-le-vent Islands, French Polynesia", "Marquesas Islands, French Polynesia", "Yap Island")),
           !(covidence_id == 6765 & grepl('infectious period', parameter_type) & population_location %in% c('Tahiti, French Polynesia', 'Tuamotu-Gambier, French Polynesia','Australes, French Polynesia','Yap Island',"Mo'orea, French Polynesia")))

  # Add in distribution type and location / country
  df <- df %>%
    mutate(distribution_type = ifelse(covidence_id == 946 & parameter_type == 'Human delay - generation time',
                                      'Gamma', distribution_type),
           population_location = ifelse(covidence_id == 946 & parameter_type == 'Human delay - generation time',
                                        "Guadeloupe, Martinique, Saint-Martin", population_location),
           population_country = ifelse(covidence_id == 946 & parameter_type == 'Human delay - generation time',
                                       "France", population_country))

  # Fix dates
  df <- df %>%
    mutate(population_study_end_month = ifelse(covidence_id == 4427 & grepl('Serial Interval', parameter_type), 'Aug', population_study_end_month),
           population_study_end_year = ifelse(covidence_id == 4427 & grepl('Serial Interval', parameter_type), 2016, population_study_end_year),
           population_study_start_month = ifelse(covidence_id == 4427 & grepl('Serial Interval', parameter_type), 'Nov', population_study_start_month),
           population_study_start_year = ifelse(covidence_id == 4427 & grepl('Serial Interval', parameter_type), 2016, population_study_start_year))

  return(df)
}

zika_clean_r <- function(params_df){
  params_df <- params_df %>%
    mutate(parameter_value = case_when(
      covidence_id == 7647 & parameter_type == "Reproduction number (Basic R0)" ~ 1.12,
      TRUE ~ parameter_value
    ))

  # Re-label incorrectly reported reproduction numbers
  params_df <- params_df %>%
    mutate(parameter_type = case_when(
      covidence_id == 6211 & parameter_value == 0.24 ~ "Reproduction number (Effective, Re)",
      covidence_id == 4427 & parameter_value == 1.22 ~ "Reproduction number (Effective, Re)",
      TRUE ~ parameter_type
    ))

  return(params_df)
}

zika_clean_zcs_microcephaly <- function(df){
  df <- df %>%
    # Remove incorrect entry
    filter(!(covidence_id == 12074 & cfr_ifr_numerator == 6)) %>%
    # Change 6521 to report without units and in num/denominator columns instead
    mutate(parameter_value = case_when(
      covidence_id == 6521 & parameter_type == "Zika congenital syndrome (microcephaly) probability" ~ NA,
      covidence_id == 6270 & parameter_type == 'Zika congenital syndrome (microcephaly) probability' ~ NA, # was incorrrect
      covidence_id == 12074 & parameter_type == 'Zika congenital syndrome (microcephaly) probability' ~ 5.26, # was less precise (5 only)
      TRUE ~ parameter_value
    ),
    cfr_ifr_denominator = case_when(
      covidence_id == 6521 & parameter_type == "Zika congenital syndrome (microcephaly) probability" ~ 11,
      covidence_id == 4014  & parameter_type == 'Zika congenital syndrome (microcephaly) probability' ~ 117, # mixed up num and denom
      covidence_id == 4014  & parameter_type == 'Pregnancy loss probability' ~ 134, # mixed up num and denom
      covidence_id == 18620  & parameter_type == 'Zika congenital syndrome (microcephaly) probability' ~ 77, # mixed up num and denom
      covidence_id == 10741  & parameter_type == 'Zika congenital syndrome (microcephaly) probability' ~ 3, # missing
      covidence_id == 11966  & parameter_type == 'Zika congenital syndrome (microcephaly) probability' ~ 154, # missing
      TRUE ~ cfr_ifr_denominator
    ),
    cfr_ifr_numerator = case_when(
      covidence_id == 6521 & parameter_type == "Zika congenital syndrome (microcephaly) probability" ~ 2,
      covidence_id == 4014 & parameter_type == 'Zika congenital syndrome (microcephaly) probability' ~ 4, # mixed up num and denom
      covidence_id == 4014 & parameter_type == 'Pregnancy loss probability' ~ 9, # mixed up num and denom
      covidence_id == 18620 & parameter_type == 'Zika congenital syndrome (microcephaly) probability' ~ 4, # mixed up num and denom
      covidence_id == 6301 & parameter_type == "Zika congenital syndrome (microcephaly) probability" ~ 18, # missing
      covidence_id == 6028 & parameter_type == "Pregnancy loss probability" ~ 11, # missing
      covidence_id == 11966  & parameter_type == 'Zika congenital syndrome (microcephaly) probability' ~ 7, # missing
      covidence_id == 6270 & parameter_type == 'Zika congenital syndrome (microcephaly) probability' ~ 2, # was incorrrect
      covidence_id == 12074 & parameter_type == 'Zika congenital syndrome (microcephaly) probability' ~ 5, # was incorrect (24)
      TRUE ~ cfr_ifr_numerator
    )) %>%
    filter(!(covidence_id == 6993 & parameter_value %in% c(81.0, 16.7, 0.02))) %>% # not a ZCS probability
    filter(!(covidence_id == 10681 & parameter_value %in% c(10, 6))) %>% #these were IgG prevalences not ZCS risks (Table 1)
    mutate(population_sample_type = ifelse(covidence_id == 6993, 'Hospital based', population_sample_type),
           population_group = ifelse(covidence_id == 6993, 'Pregnant women',population_group))

  # After cleaning that finished on 12 June, 2025, we decided to pull in the edited spreadsheet instead of cleaning it here (because of the many changes)
  covids_toedit <- c(5715, 5895, 6175, 10818, # these are cov ids with entries that we removed that didn't have another entry (for either CZS or misc),
                                              # so they will not be added back in when we bind below (they do have other entries for other parameters)
                     705,1236,1326,2704,2713,3071,4014,4324,4383,5753,5902,6028,6072,6120,6241,6270,6301,6427,
                     6501,6521,6547,6658,6670,6686,6810,6823,6952,6971,6993,7050,7165,7452,7503,10125,10318,
                     10509,10681,10741,10760,10965,11048,11297,11565,11900,11966,12074,18620,18800,19021,31944,31965)

  # remove the entries that we are editing
  df_filtered <- df %>%
    filter(!(covidence_id %in% covids_toedit & (parameter_type == "Zika congenital syndrome (microcephaly) probability" |
                                                  parameter_type == 'Pregnancy loss probability'))) %>%
    mutate(metaanalysis_inclusion = NA,
           women_or_infants = NA,
           pregnancy_outcome_type = NA)

  # add the new ones back in
  df <- df_filtered %>%
    rbind(czs_misc_cleaned)

}

zika_clean_genomics <- function(df){

  # Add new column for lineage if specified
  df <- df %>%
    mutate(genomic_lineage = case_when(
      covidence_id == 3152 & parameter_class == 'Mutations' ~ "Asian; AmZIKV lineage",
      covidence_id %in% c(3154, 5907, 6571) & parameter_class == 'Mutations' ~ "Asian lineage",
      covidence_id == 6717 & parameter_class == 'Mutations' ~ "Manaus clade",
      covidence_id == 10526 & parameter_class == 'Mutations' ~ "Asian lineage (Thai strains)",
      TRUE ~ NA
     ))

  df <- df %>%
    mutate(
      # Edit name of genome sites
      genome_site = case_when(
        genome_site %in% c('wgs','WGS') ~ 'Whole genome sequence',
        covidence_id %in% c(506, 3152, 3163, 3490, 4328, 4438, 5907, 5908, 6361, 6511, 6717, 10526) &
          parameter_class == 'Mutations' ~ "Whole genome sequence",
        TRUE ~ genome_site
      ),
      # Fix incorrect parameter unit
      parameter_unit = case_when(
        covidence_id == 7717 & parameter_type == "Mutations - evolutionary rate" ~ "Substitutions/site/year",
        TRUE ~ parameter_unit
      ),
      # Fix incorrect parameter_type name
      parameter_type = case_when(
        covidence_id == 4328 & grepl("Mutation", parameter_type) ~ "Mutations - evolutionary rate",
        TRUE ~ parameter_type
      ),
      # Fix incorrect parameter value types
      parameter_value_type = case_when(
        covidence_id %in% c(5907, 10180) & parameter_type == "Mutations - evolutionary rate" ~ 'Central',
        TRUE ~ parameter_value_type
      ),
      method_moment_value = case_when(
        covidence_id == 1954 & parameter_class == 'Mutations' ~ 'Mid-outbreak',
        is.na(method_moment_value) ~ "Unspecified",
        TRUE ~ method_moment_value
      ),
      population_sample_size = case_when(
        covidence_id == 5907 & parameter_class == 'Mutations' ~ 360,
        covidence_id == 6717 & parameter_class == 'Mutations' ~ 59,
        covidence_id == 10180 & parameter_class == 'Mutations' ~ 238,
        covidence_id == 6511 & parameter_class == 'Mutations' ~ 269,
        TRUE ~ population_sample_size
      ),
      genomic_sequence_available = case_when(
        covidence_id %in% c(506, 1954, 4438, 5908, 6361, 6571, 7717, 12777) & parameter_class == 'Mutations' ~ TRUE,
        TRUE ~ genomic_sequence_available
      ),
      cfr_ifr_denominator = ifelse(covidence_id == 1954 & parameter_class == 'Mutations', 10272, cfr_ifr_denominator) # adding in the number of bases so we can calc sub rate/site/year
    ) %>%
    # Remove empty row
    filter(!(covidence_id == 4438 & parameter_type == "Mutations - mutation rate" & is.na(parameter_value))) %>%
    # Remove incorrectly extracted row
    filter(!(covidence_id == 506 & parameter_type == 'Mutations - mutation rate' & parameter_lower_bound == 0.2)) %>%
    # Remove paper extracted twice (both Tristan and Sangeeta extracted the same parameter it seems)
    filter(!(covidence_id == 1663 & parameter_type == "Mutations â€“ substitution rate" & access_param_id == 5))

  # Correct specific genomic entries that had incorrect unit and values in incorrect variables
  df <- df %>%
    mutate(parameter_unit = ifelse(covidence_id == 1954 & parameter_unit == 'Substitutions/site/year',
                                   "Mutations/year", parameter_unit),
           parameter_lower_bound = ifelse(covidence_id == 1954 & parameter_unit == 'Mutations/year',
                                          12, parameter_lower_bound),
           parameter_upper_bound = ifelse(covidence_id == 1954 & parameter_unit == 'Mutations/year',
                                          25, parameter_upper_bound),
           parameter_uncertainty_lower_value = ifelse(covidence_id == 1954 & parameter_unit == 'Mutations/year',
                                                      NA, parameter_uncertainty_lower_value),
           parameter_uncertainty_upper_value = ifelse(covidence_id == 1954 & parameter_unit == 'Mutations/year',
                                                      NA, parameter_uncertainty_upper_value),
           # parameter_uncertainty_type = ifelse(covidence_id == 1954 & parameter_unit == 'Mutations/year',
           #                                     "Range", parameter_uncertainty_type),
           parameter_2_lower_bound = ifelse(covidence_id == 1954 & parameter_unit == 'Mutations/year',
                                            NA, parameter_2_lower_bound),
           parameter_2_upper_bound = ifelse(covidence_id == 1954 & parameter_unit == 'Mutations/year',
                                            NA, parameter_2_upper_bound),
           exponent = ifelse(covidence_id == 1954, 0, exponent)
    ) %>%
    # cov id 6717 had incorrect values for lower and upper uncertainty given the exponent -3
    mutate(parameter_uncertainty_lower_value = case_when(
      covidence_id == 6717 & parameter_type == "Mutations - evolutionary rate" ~ 0.77,
      TRUE ~ parameter_uncertainty_lower_value
    ),
    parameter_uncertainty_upper_value = case_when(
      covidence_id == 6717 & parameter_type == "Mutations - evolutionary rate" ~ 1.43,
      TRUE ~ parameter_uncertainty_upper_value
    )) %>%
    # Make value entered with exponent (in paper, lower value is written second - have changed this for uncertainty to be lower, then higher)
    mutate(parameter_value = ifelse(covidence_id == 4328 & parameter_type == "Mutations - evolutionary rate", 1.55, parameter_value),
           parameter_uncertainty_lower_value = ifelse(covidence_id == 4328 & parameter_type == "Mutations - evolutionary rate", 1.06, parameter_uncertainty_lower_value),
           parameter_uncertainty_upper_value = ifelse(covidence_id == 4328 & parameter_type == "Mutations - evolutionary rate", 2.05, parameter_uncertainty_upper_value),
           exponent = ifelse(covidence_id == 4328 & parameter_type == "Mutations - evolutionary rate", -3, exponent))
    # Correct incorrect way of entering exponents (because lower and upper intervals have diff exponents) NB 1663 has been excluded as wrong study type
    # mutate(parameter_value = ifelse(covidence_id == 1663 & parameter_value == 1.15, 0.00115, parameter_value),
    #        exponent = ifelse(covidence_id == 1663 & exponent == -3, 0, exponent))

  # Correct entries with missing variables
  df <- df %>%
    mutate(parameter_statistical_approach = ifelse(covidence_id == 7717 & parameter_class == 'Mutations',
                                                   "Estimated model parameter", parameter_statistical_approach),
           parameter_value_type = case_when(
             covidence_id == 7717 & is.na(parameter_value_type) ~ "Mean",
             covidence_id == 4427 & parameter_type == "Human delay - serial interval" ~ 'Mean',
             covidence_id == 2353 & parameter_type == "Human delay - infectious period" ~ "Median",
             TRUE ~ parameter_value_type),
           parameter_uncertainty_type = ifelse(covidence_id == 7717 & parameter_uncertainty_type == 'CI95%',
                                               "CRI95%", parameter_uncertainty_type),
           data_available = case_when(
             covidence_id == 1663 & is.na(data_available) ~ 'Yes - as an attachment',
             TRUE ~ data_available
           )
    )

  # Correct sampling dates
  df <- df %>%
    mutate(
      population_study_end_day = case_when(
        covidence_id == 3152 & parameter_class == 'Mutations' ~ 1,
        covidence_id == 4438 & parameter_class == 'Mutations' & population_location == "Florida" ~ 11,
        TRUE ~ population_study_end_day),
      population_study_end_month = case_when(
        covidence_id %in% c(3152, 6717) & parameter_class == 'Mutations' ~ "Mar",
        covidence_id == 4438 & parameter_class == 'Mutations' & population_location == "Florida" ~ "Oct",
        covidence_id == 4438 & parameter_class == 'Mutations' & population_location != "Florida" ~ "Nov",
        covidence_id == 3490 & parameter_class == 'Mutations' ~ "Aug",
        TRUE ~ population_study_end_month
      ),
      population_study_end_year = case_when(
        covidence_id %in% c(3490, 6717) & parameter_class == 'Mutations' ~ 2017,
        covidence_id == 4438 & parameter_class == 'Mutations' ~ 2016,
        covidence_id == 946 ~ 2017,
        TRUE ~ population_study_end_year
      ),
      population_study_start_year = case_when(
        covidence_id %in% c(3152, 3490) & parameter_class == "Mutations" ~ 1966,
        covidence_id == 4438 & parameter_class == 'Mutations' & population_location == "Florida" ~ 2016,
        covidence_id == 4438 & parameter_class == 'Mutations' & population_location != "Florida" ~ 2015,
        covidence_id == 6717 & parameter_class == 'Mutations' ~ 2015,
        covidence_id == 12777 & parameter_class == 'Mutations' ~ 1947,
        covidence_id == 6511 & parameter_class == 'Mutations' ~ 2013,
        covidence_id == 946 ~ 2015,
        TRUE ~ population_study_start_year
      ),
      population_study_start_month = case_when(
        covidence_id == 4438 & parameter_class == 'Mutations' & population_location == "Florida" ~ 'Jun',
        covidence_id == 6717 & parameter_class == 'Mutations' ~ 'Dec',
        TRUE ~ population_study_start_month
      ),
      population_study_start_day = case_when(
        covidence_id == 6717 & parameter_class == 'Mutations' ~ 15,
        TRUE ~ population_study_start_day
      )



    )


}


zika_clean_serop <- function(params_df){

  ID_PRNT_onELISA <- c(11011, 162, 6270, 10252, 6197, 1352, 10625, 5895, 3451, 5714, 4381,
                       6501, 5846, 6591, 6972, 17988, 5715, 10094, 1674, 7491, 1121, 7048,
                       12555, 5667, 9768, 23326, 6762, 4447, 6432, 6681,
                       10429, 11434, 11328) #not all of these have uncorrect den

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

        cfr_ifr_denominator = ifelse(covidence_id == 6681 & parameter_type == "Seroprevalence - Neutralisation/PRNT",  311, cfr_ifr_denominator),

        # Change serop-Unspecified to specific type
        parameter_type = ifelse(covidence_id == 5846 & parameter_type == "Seroprevalence - NS1 BOB ELISA" , "Seroprevalence - NS1 BOB ELISA" , parameter_type),
        parameter_type = ifelse(covidence_id == 6841 & parameter_type == "Seroprevalence - ZIKV EDII"  , "Seroprevalence - Biotinylated-EDIII antigen capture ELISA" , parameter_type),
        parameter_type = ifelse(covidence_id %in% c(5890, 2578) & parameter_type == "Seroprevalence - Unspecified" , "Seroprevalence - IgG and IgM", parameter_type),
        parameter_type = ifelse(covidence_id == 3589 & parameter_type == "Seroprevalence - Unspecified" , "Seroprevalence - IgM", parameter_type),
        parameter_type = ifelse(covidence_id == 10697 & parameter_type == "Seroprevalence - Unspecified" , "Seroprevalence - MIA", parameter_type),
        parameter_type = ifelse(covidence_id %in% c(483, 7252, 23230) & parameter_type == "Seroprevalence - Unspecified" , "Seroprevalence - NS1 BOB ELISA", parameter_type),
        parameter_type = ifelse(covidence_id == 6072 & parameter_type == "Seroprevalence - Unspecified" , "Seroprevalence - Neutralisation/PRNT", parameter_type),

        parameter_type = ifelse(covidence_id %in% c(6680, 8155) & parameter_type == "Seroprevalence - Unspecified" , "Seroprevalence - MIA" , parameter_type),
        # parameter_type = ifelse(covidence_id == 11853 & parameter_type == "Seroprevalence - Unspecified"  , "Seroprevalence - Multiple assays combined" , parameter_type),

        parameter_type = ifelse(covidence_id == 174 & parameter_type == "Seroprevalence - Unspecified" ,  "Seroprevalence - Western blot", parameter_type),
        parameter_type = ifelse(covidence_id == 10429 & parameter_type == "Seroprevalence - Unspecified" ,  "Seroprevalence - NS1 BOB ELISA", parameter_type),


        #change a seroprevalence label in risk factor
        parameter_type = ifelse(covidence_id == 6072 & parameter_type == "Seroprevalence - Unspecified" , "Risk factors", parameter_type),
        parameter_type = ifelse(covidence_id == 1941 & parameter_type == "Seroprevalence - Unspecified" , "Seroprevalence - IgM", parameter_type)

      ) %>%

      #remove entry IgG from 6681 as it's all teste grouped together
      # remove 6072 because papaer doesn't report IgG overall positivity
      filter(!(covidence_id == 6072 & parameter_type == "Seroprevalence - IgM")) %>%
      #remove 11853 because multiple tests combined
      filter(!(covidence_id == 11853 & parameter_type == "Seroprevalence - Unspecified")) %>%
      # remove 10652 because it is same paper as 2302
      filter(!(covidence_id == 10652 & grepl("^Seroprevalence.+", parameter_type)))

    return(df)
}


zika_clean_mod_notes <- function(model_df){

  df <- model_df %>%
    mutate(compartmental_type = ifelse(covidence_id %in% c(430, 2632, 3017, 25488, 5536,
                                                           25512, 5910, 6380) & compartmental_type == "Other compartmental",
                                       "Other SEIR-SEI", compartmental_type),

           #Other models
           compartmental_type = ifelse(covidence_id %in% c(8019, 27250,27081, 5744, 5700,
                                                           6159, 5773, 5800, 8031, 6141,
                                                           6180, 5581, 5821, 5607) & compartmental_type == "Other compartmental",
                                       "SIR-SI", compartmental_type),
           compartmental_type = ifelse(covidence_id %in% c(5874, 5776) & compartmental_type == "Other compartmental",
                                       "SIR-SEI", compartmental_type),
           compartmental_type = ifelse(covidence_id %in% c(5747, 6247) & compartmental_type == "Other compartmental",
                                       "SIRS-SI", compartmental_type),
           compartmental_type = ifelse(covidence_id == 7966 & compartmental_type == "Other compartmental",
                                       "SEI-SI", compartmental_type),
           compartmental_type = ifelse(covidence_id == 25509 & compartmental_type == "Other compartmental",
                                       "SAIR-SEI", compartmental_type),
           compartmental_type = ifelse(covidence_id == 5841 & compartmental_type == "Other compartmental",
                                       "SEIR-SI", compartmental_type),
           compartmental_type = ifelse(covidence_id %in% c(19025,25075, 25162 ) & compartmental_type == "Other compartmental",
                                       "SI-SI", compartmental_type),
           compartmental_type = ifelse(covidence_id == 6775 & compartmental_type == "Other compartmental",
                                       "SLIR-SLI", compartmental_type)

           #to check what kind of model extracted for ID 6772,  6015, 5725



    )
  return(df)
}

zika_clean_pars_notes <-  function(params_df){
  # unique(params_all$population_sample_type) "Travel based"
  #unique(params_all$population_group) # Blood donors
  df <- params_df %>%

    mutate(
      # Add a new col that specifies if PRNT on positive ELISA only
      population_sample_type = ifelse(covidence_id %in% c(1663, 2362, 28411, 7437, 10749, 6686), "Travel based", population_sample_type),
      population_group = ifelse(covidence_id %in% c(3030, 3614, 18107, 5714, 4074, 4447, 6182, 6197), "Blood donors", population_group))
# check 10252 because also sex workers

return(df)
}

