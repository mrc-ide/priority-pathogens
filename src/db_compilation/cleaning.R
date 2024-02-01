# Functions for cleaning data and adding QA scores
library(dplyr)
library(janitor)
library(rio)
library(tidyr)
library(stringr)
library(ids)

##########################
# Main cleaning function #
##########################

#' Input: article/model/parameter/outbreak data frame to clean
#' Process: clean names, removes old ids and extractor names, adds a grouping
#' variable for classification of the parameter type, fix entry errors
#' output: clean article/model/parameter/outbreak df
clean_dfs <- function(df, pathogen) {
  ####################
  # Article cleaning #
  ####################

  if ("article_title" %in% colnames(df)) {
    df <- df %>%
      select(-c("article_id", "name_data_entry")) %>%
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

    if (pathogen == "EBOLA") {
      # edit qa score for Maganga 2014 (NOTE: qa scores only edited after
      # discussion with co-authors)
      df <- df %>%
        mutate(
          qa_m1 = case_when(covidence_id %in% 3138 ~ "No", TRUE ~ qa_m1),
          qa_a3 = case_when(covidence_id %in% 3138 ~ "No", TRUE ~ qa_a3),
          # clean up author names (remove initials and fully capitalised names)
          first_author_surname = str_replace(first_author_surname, "\\b[A-Z]\\.", ""),
          first_author_surname = case_when(
            first_author_surname %in% "OUEMBA TASSE ́" ~ "Ouemba Tasse",
            first_author_surname %in% "R Glynn" ~ "Glynn",
            first_author_surname %in% "JUGA" ~ "Juga",
            first_author_surname %in% "KI" ~ "Ki",
            TRUE ~ first_author_surname
          ),
          # Fix issue with a doi
          doi = case_when(
            covidence_id %in% 3058 ~ "10.3201/eid2201.151410",
            TRUE ~ doi
          )
        ) %>%
        # Update article label
        mutate(
          article_label = as.character(
            paste0(first_author_surname, " ", year_publication)
          )
        )
    }

    if (pathogen == "LASSA") {
      # journal consistency
      df <- df %>%
        mutate(journal = str_to_title(journal)) %>%
        mutate(journal = sub("^The\\s+", "", journal)) %>%
        mutate(journal = ifelse(journal %in% "Bmj", "British Medical Journal",
          ifelse(journal %in% "Transactions Of The Royal Society Tropical Medicine And Hygiene", "Transactions Of The Royal Society Of Tropical Medicine And Hygiene",
            journal
          )
        )) %>%
        # journal typos
        mutate(journal = case_when(
          covidence_id %in% 870 ~ "Transactions Of The Royal Society Of Tropical Medicine And Hygiene",
          covidence_id %in% 2684 ~ "International Journal Of Infectious Diseases",
          covidence_id %in% 669 ~ "American Journal Of Tropical Medicine And Hygiene",
          covidence_id %in% 4237 ~ "Chaos Solitons & Fractals",
          covidence_id %in% 2655 ~ "Lancet Infectious Diseases",
          TRUE ~ journal
        )) %>%
        # year typos
        mutate(year_publication = case_when(
          covidence_id %in% 1012 ~ 2008,
          covidence_id %in% 1368 ~ 2001,
          covidence_id %in% 1444 ~ 2017,
          covidence_id %in% 2008 ~ 1983,
          TRUE ~ year_publication
        )) %>%
        # volume typos
        mutate(volume = case_when(
          covidence_id %in% 1012 ~ 42,
          covidence_id %in% 4251 ~ 2020,
          TRUE ~ volume
        )) %>%
        # issue typos
        mutate(issue = case_when(
          covidence_id %in% c(1012, 168) ~ 1,
          covidence_id %in% c(79) ~ 2,
          covidence_id %in% c(49, 941) ~ 3,
          covidence_id %in% c(225, 870) ~ 4,
          covidence_id %in% c(2133) ~ 6,
          covidence_id %in% c(1447) ~ 11,
          covidence_id %in% c(2801) ~ 26,
          covidence_id %in% c(845, 3147, 3153, 3215) ~ NA,
          TRUE ~ issue
        )) %>%
        # page typos
        mutate(page_first = case_when(
          covidence_id %in% c(757) ~ 972,
          covidence_id %in% c(1413, 2567, 2610, 2661, 3215, 3258, 3635) ~ NA,
          TRUE ~ page_first
        )) %>%
        mutate(page_last = case_when(
          covidence_id %in% 263 ~ 28,
          covidence_id %in% 757 ~ 977,
          covidence_id %in% 1235 ~ 1205,
          covidence_id %in% 4745 ~ 784,
          TRUE ~ page_last
        )) %>%
        # title typos
        mutate(article_title = case_when(
          covidence_id %in% 444 ~ "Genetic characterization of Lassa virus strains isolated from 2012 to 2016 in southeastern Nigeria",
          covidence_id %in% 545 ~ "Lassa Virus Activity in Guinea - Distribution of Human Antiviral Antibody-Defined Using Enzyme-Linked-Immunosorbent-Assay with Recombinant Antigen",
          covidence_id %in% 645 ~ "Clinical Observations in 42 Patients with Lassa Fever",
          covidence_id %in% 874 ~ "Lassa Fever in Eastern Province of Sierra Leone, 1970-1972 .2. Clinical Observations and Virological Studies on Selected Hospital Cases",
          covidence_id %in% 1012 ~ "Strain-specific antibody response to Lassa virus in the local population of west Africa",
          covidence_id %in% 2648 ~ "Rodent-borne infections in rural Ghanaian farming communities",
          covidence_id %in% 4371 ~ "Assessing the dynamics of Lassa fever with impact of environmental sanitation: optimal control and cost-effectiveness analysis",
          covidence_id %in% 4727 ~ "Lassa Fever among Children in Eastern Province, Sierra Leone: A 7-year Retrospective Analysis (2012-2018)",
          TRUE ~ article_title
        )) %>%
        # title consistency
        mutate(article_title = gsub(";", ",", article_title)) %>%
        mutate(article_title = gsub("\n", " ", article_title)) %>%
        mutate(article_title = gsub("\\s+", " ", article_title)) %>%
        mutate(article_title = str_to_title(article_title)) %>%
        # missing dois
        mutate(doi = sub(".*?10\\.", "10.", doi)) %>%
        mutate(doi = case_when(
          covidence_id %in% 2656 ~ "10.1016/j.ijid.2019.03.030",
          covidence_id %in% 2662 ~ "10.1016/S2214-109X(20)30518-0",
          covidence_id %in% 1426 ~ "10.1016/j.cell.2015.07.020",
          covidence_id %in% 441 ~ "10.1016/j.sste.2014.04.005",
          covidence_id %in% 444 ~ "10.1371/journal.pntd.0006971",
          covidence_id %in% 461 ~ "10.1016/S1473-3099(18)30121-X",
          TRUE ~ doi
        )) %>%
        # paper copies
        mutate(paper_copy_only = case_when(
          covidence_id %in% c(845, 917) ~ FALSE,
          TRUE ~ paper_copy_only
        )) %>%
        # name typos
        mutate(first_author_surname = case_when(
          covidence_id %in% 2648 ~ "Shirley C.",
          covidence_id %in% 1447 ~ "N.A.",
          TRUE ~ first_author_surname
        )) %>%
        mutate(first_author_first_name = sub(".*\\.(.*)", "\\1", first_author_first_name)) %>%
        mutate(first_author_first_name = sub("^\\s+", "", first_author_first_name)) %>%
        mutate(first_author_first_name = case_when(
          covidence_id %in% 2648 ~ "Nimo-Paintsil",
          covidence_id %in% 2585 ~ "Dalhat",
          covidence_id %in% 1033 ~ "Ehichioya",
          covidence_id %in% 661 ~ "Kerneis",
          TRUE ~ first_author_first_name
        ))
      # revised qa after parameter removed: now outbreak only
      df[df$covidence_id %in% 152, c("qa_m1", "qa_m2", "qa_a3", "qa_a4", "qa_d6", "qa_d7")] <- NA
    }
  }

  ##################
  # Model cleaning #
  ##################

  if ("model_type" %in% colnames(df)) {
    df <- df %>%
      select(-c("article_id", "name_data_entry")) %>%
      relocate(c(id, model_data_id, covidence_id, pathogen)) %>%
      arrange(covidence_id)

    #########################################
    # Pathogen-specific model data cleaning #
    #########################################

    if (pathogen == "EBOLA") {
      # edit ebola_variant names
      df <- df %>%
        mutate(
          ebola_variant = case_when(
            covidence_id %in% 15947 ~ "Bundibugyo virus (BDBV)",
            covidence_id %in% 5675 ~
              "Bundibugyo virus (BDBV);Sudan virus (SUDV);Taï Forest virus (TAFV);Zaire Ebola virus (EBOV)",
            TRUE ~ ebola_variant
          )
        )
    }

    if (pathogen == "LASSA") {
      # reclassified models
      df <- df %>%
        mutate(
          model_type = case_when(
            covidence_id %in% 2620 ~ "Other",
            TRUE ~ model_type
          ),
          stoch_deter = case_when(
            covidence_id %in% 285 ~ "Stochastic",
            TRUE ~ stoch_deter
          ),
          # missed transmission pathways
          transmission_route = case_when(
            covidence_id %in% c(558, 560) ~ "Human to human (direct contact);Vector/Animal to human",
            TRUE ~ transmission_route
          ),
          # assumptions should apply to human mixing only
          assumptions = case_when(
            covidence_id %in% c(
              4133, 4113, 4237, 4243, 4316, 4340, 2578, 2707, 4207,
              4487, 4193, 4162, 2801, 4517, 4597, 4827, 4213
            ) ~ "Homogeneous mixing",
            covidence_id %in% c(558) ~ "Heterogenity in transmission rates - over time",
            covidence_id %in% c(2620, 4343) ~ "Heterogenity in transmission rates - between groups",
            covidence_id %in% c(2617) ~ "Heterogenity in transmission rates - between groups;Heterogenity in transmission rates - over time",
            covidence_id %in% c(4118) ~ "Heterogenity in transmission rates - over time;Latent period is same as incubation period",
            TRUE ~ assumptions
          ),
          # compartments should apply to humans only
          compartmental_type = case_when(
            covidence_id %in% c(2617, 2620) ~ "Not compartmental",
            covidence_id %in% c(4517, 4827, 4597, 2578, 2707, 4136, 4144, 4207, 4371) ~ "Other compartmental",
            covidence_id %in% c(4162, 4487, 3659, 4243, 4316, 4131) ~ "SEIR",
            covidence_id %in% c(4193, 4213, 4237, 4251) ~ "SIR",
            TRUE ~ compartmental_type
          ),
          # unspecified interventions
          interventions_type = case_when(
            covidence_id %in% c(558, 560) | is.na(interventions_type) ~ "Unspecified",
            TRUE ~ interventions_type
          )
        )
    }
    df <- df %>% select(-c("access_model_id"))
  }

  #####################
  # Outbreak cleaning #
  #####################

  if ("outbreak_id" %in% colnames(df)) {
    df <- df %>%
      select(-c("article_id", "outbreak_id", "name_data_entry")) %>%
      relocate(c(id, outbreak_data_id, covidence_id, pathogen)) %>%
      arrange(covidence_id) %>%
      mutate(
        # Format start/stop months for outbreaks
        outbreak_start_month = substring(outbreak_start_month, 1, 3),
        outbreak_end_month = substring(outbreak_end_month, 1, 3),
        # Outbreak country names
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
        )
      )

    if (pathogen == "LASSA") {
      # clean locations
      df <- df %>%
        mutate(outbreak_location = gsub(",", ";", outbreak_location)) %>%
        mutate(outbreak_location = gsub("and", ";", outbreak_location)) %>%
        mutate(outbreak_location = sub("^\\s+", "", outbreak_location)) %>%
        mutate(outbreak_location = str_to_title(outbreak_location)) %>%
        mutate(outbreak_location = case_when(
          covidence_id %in% 560 & access_outbreak_id %in% 3 ~ "Kenema Government Hospital",
          TRUE ~ outbreak_location
        )) %>%
        # unspecified case detection mode
        mutate(cases_mode_detection = case_when(
          is.na(cases_mode_detection) ~ "Unspecified",
          TRUE ~ cases_mode_detection
        ))
    }
    df <- df %>% select(-c("access_outbreak_id"))
  }

  ######################
  # Parameter cleaning #
  ######################

  if ("parameter_type" %in% colnames(df)) {
    df <- df %>%
      mutate(
        # Change variable types
        cfr_ifr_numerator = as.integer(cfr_ifr_numerator),
        population_study_start_day = as.numeric(population_study_start_day),
        method_disaggregated_by = str_replace_all(method_disaggregated_by, ";", ", "),

        # Group parameters
        parameter_class = case_when(
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
        population_country = str_replace_all(population_country, ",", ", ")
      )

    #############################################
    # Pathogen-specific parameter data cleaning #
    #############################################

    if (pathogen == "MARBURG") {
      df <- df %>%
        mutate(
          population_study_start_month = substring(population_study_start_month, 1, 3),
          population_study_end_month = substring(population_study_end_month, 1, 3)
        )
    }

    if (pathogen == "EBOLA") {
      df <- df %>%
        # Remove parameters from theoretical model papers/synthetic data papers
        # (e.g. 2857)/correspondence/wrong entries
        filter(!(covidence_id %in% c(1510, 2857, 4301, 5765, 5870, 17096))) %>%
        # Remove duplicate entry from single extracted paper not identified as distinct
        filter(!(covidence_id %in% 3532 & access_param_id %in% 37)) %>%
        # Remove risk factor for covidence ID 17054
        filter(!(covidence_id %in% 17054 & parameter_type %in% "Risk factors")) %>%
        # Remove one risk factor from covidence ID 4764
        filter(!(riskfactor_name %in% "Other" &
          covidence_id %in% 4764 & access_param_id %in% 74)) %>%
        # Correct missing context for cov ID 18236
        group_by(covidence_id) %>%
        mutate(
          across(
            starts_with("population"),
            ~ ifelse(covidence_id %in% 18236 & is.na(.),
              first(.[covidence_id %in% 18236]), .
            )
          )
        ) %>%
        ungroup() %>%
        # Population country
        mutate(
          population_country =
            case_when(
              population_country %in%
                "France, Germany, Italy, Mali, Netherlands, Nigeria, Norway, Senegal, Spain, Switzerland, United Kingdom, United States" ~
                "Multi-country: Africa, Europe, USA (n = 12)",
              population_country %in%
                "The Gambia, Guinea, Liberia, Nigeria, Senegal, Sierra Leone, United Kingdom, United States" ~
                "Multi-country: Africa, Europe, USA (n = 8)",
              population_country %in%
                "Guinea, Italy, Liberia, Mali, Nigeria, Senegal, Sierra Leone, Spain, United Kingdom, United States" ~
                "Multi-country: Africa, Europe, USA (n = 10)",
              population_country %in%
                "DRC, Republic of the Congo, Côte d'Ivoire, Gabon, South Africa, South Sudan, Uganda" ~
                "Multi-country: Africa (n = 7)",
              population_country %in%
                "Cameroon, Central African Republic, Chad, Republic of the Congo, Equatorial Guinea, Gabon" ~
                "Multi-country: Africa (n = 6)",
              population_country %in%
                "Cameroon, DRC, Republic of the Congo, Ghana, Uganda" ~
                "Multi-country: Africa (n = 5)",
              # 11688 = Guinea, Liberia, Nigeria, Sierra Leone, Spain, USA
              covidence_id %in% 11688 ~
                "Multi-country: Africa, Europe, USA (n = 6)",
              # 509 = USA, Germany, Switzerland, UK, Spain, Norway, Italy, France, Netherlands
              covidence_id %in% 509 ~
                "Multi-country: Europe & USA (n = 9)",
              # 23720 = Congo, Guinea, Liberia, Nigeria, Sierra Leone, Uganda
              # (https://github.com/DrakeLab/taube-transmission-trees)
              covidence_id %in% 23720 ~
                "Multi-country: Africa (n = 6)",
              # Was unspecified and checked the paper
              covidence_id %in% 4568 ~ "Guinea, Liberia, Sierra Leone",
              covidence_id %in% 18535 ~ "DRC",
              covidence_id %in% 18536 ~ "Guinea",
              is.na(population_country) ~ "Unspecified",
              TRUE ~ population_country
            )
        ) %>%
        # merge ebola_variant and ebola_variant_p
        mutate(
          ebola_variant_p = case_when(
            ebola_variant_p %in% "Bundibugyo virus (BDBV)¬†" ~ ebola_variant,
            TRUE ~ ebola_variant_p
          ),
          ebola_variant_fix = case_when(
            covidence_id %in% c(163, 662, 847, 6346) ~ "Zaire Ebola virus (EBOV)",
            covidence_id %in% c(2548, 904, 17835) ~
              "Bundibugyo virus (BDBV);Sudan virus (SUDV);Taï Forest virus (TAFV);Zaire Ebola virus (EBOV)",
            TRUE ~ ebola_variant_p
          )
        ) %>%
        select(-c(ebola_variant_p, ebola_variant)) %>%
        rename(ebola_variant = ebola_variant_fix) %>%
        # clean the other human delays and merge the common ones into parameter_type
        mutate(
          other_delay_start = case_when(
            other_delay_start %in% "Other: Health center visit" ~ "Health center visit",
            other_delay_start %in% c("Infection", "Contact with Primary Case") ~ "Exposure/Infection",
            other_delay_start %in% "Funeral Start" ~ "Funeral start",
            other_delay_start %in% "Positive Test" ~ "Positive test",
            other_delay_start %in% "Sampling date" ~ "Sample collection",
            other_delay_start %in% c(
              "Other: Enter Timepoint in Text Box",
              "Other: Not specified 'duration of illness of those who died'",
              "Other: alert of illness onset"
            ) ~ "Symptom Onset/Fever",
            other_delay_start %in% "Testing" ~ "Test",
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
              "Other: Case report completion",
              "Report (for confirmed cases with known outcomes)",
              "Other: official report"
            ) ~ "Reporting",
            other_delay_end %in% c("Testing", "EVD Testing") ~ "Test",
            other_delay_end %in% c("Test result", "Result", "Test Results") ~ "Test result",
            TRUE ~ other_delay_end
          ),
          other_delay =
            case_when(
              !is.na(other_delay_start) ~
                paste(other_delay_start, "to", other_delay_end, sep = " ")
            ),
          parameter_type =
            case_when(
              !is.na(other_delay) ~ paste("Human delay -", other_delay),
              TRUE ~ parameter_type
            ),
          delay_short =
            case_when(
              parameter_class %in% "Human delay" ~
                gsub("^Human delay - ", "", parameter_type),
              TRUE ~ NA
            ),
          delay_short = str_to_sentence(delay_short),
          delay_short =
            str_replace_all(
              delay_short, c(
                "care/hospitalisation" = "care",
                "care/hospital" = "care",
                "onset/fever" = "onset"
              )
            ),
          delay_short =
            case_when(
              delay_short %in% "Exposure/infection to infectiousness" ~
                "Latent period",
              delay_short %in% "Exposure/infection to symptom onset" ~
                "Incubation period",
              delay_short %in% "Time in care (length of stay)" ~
                "Admission to care to death/discharge",
              TRUE ~ delay_short
            ),
          delay_start =
            case_when(
              startsWith(delay_short, "Admission to care") ~ "Admission to care",
              startsWith(delay_short, "Symptom onset") ~ "Symptom onset",
              startsWith(delay_short, "Death to burial") ~ "Death to burial",
              startsWith(delay_short, "Exposure/infection") ~ "Exposure/infection",
              delay_short %in%
                c(
                  "Incubation period", "Latent period", "Infectious period",
                  "Generation time", "Serial interval"
                ) ~ "Infection process",
              TRUE ~ "Other"
            ),
          parameter_unit =
            case_when(
              parameter_class %in% "Human delay" &
                parameter_unit %in% "Per day" ~ "Days",
              parameter_class %in% "Human delay" &
                is.na(parameter_unit) &
                covidence_id %in% c(3776, 16951, 18371) ~ "Days",
              parameter_class %in% "Human delay" &
                parameter_unit %in% "Per week" ~ "Weeks",
              parameter_type %in% "Overdispersion" &
                is.na(parameter_unit) &
                covidence_id %in% c(2065, 4787, 5940, 23720) ~ "No units",
              TRUE ~ parameter_unit
            )
        ) %>%
        # clean the other risk factors and create new variable
        mutate(
          riskfactor_outcome =
            case_when(
              covidence_id %in% 16579 & riskfactor_outcome %in% "Other" ~ "Infection",
              TRUE ~ riskfactor_outcome
            ),
          # Variable for "other" risk factor outcomes
          other_rf_outcome =
            case_when(
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 562 ~ "Admitted patient testing PCR positive",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% c(16279, 19084) ~ "RNA persistence",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 1561 ~ "Outbreak", # environmental factors as risk factor for ebola outbreak
              riskfactor_outcome %in% "Other" &
                covidence_id %in% c(1749, 4253, 4257) ~ "Household transmission",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 17861 ~ "Hospitalisation",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 2160 ~ "Importation", # with travel restrictions implemented
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 3487 &
                access_param_id %in% c(29, 30) ~ "Viremia at admission",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 3487 &
                access_param_id %in% c(31, 32) ~ "Highest viremia during hospitalisation",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% c(5005, 18536) ~ "Onward transmission", # 5005: SES for onward transmission
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 5567 &
                riskfactor_name %in% c("Age;Occupation;Other", "Age") ~ "Primary case within household",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 5567 &
                riskfactor_name %in% c("Age;Other", "Occupation;Sex") ~ "Non-primary case within household",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 15448 ~ "Reporting death of household member/relative",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 16699 ~ "Exposure", # Rf SES
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 17157 &
                access_param_id %in% 300 ~ "Onset to admission delay",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 17139 ~ "Length of stay",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 17157 &
                access_param_id %in% 301 ~ "Length of stay",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 17275 &
                access_param_id %in% 295 ~ "Onset to admission delay",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 17275 &
                access_param_id %in% 296 ~ "Length of stay",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 17981 ~ "Spillover",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 18103 ~ "Testing positive out of all individuals with possible infection",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 18354 ~ "Loss to follow-up of contacts",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 18605 &
                access_param_id %in% 297 ~ "Onset to admission delay",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 18605 &
                access_param_id %in% 298 ~ "Length of stay",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 4764 &
                access_param_id %in% c(71, 72) ~ "Importation", # phylogeographic GLM
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 4764 &
                access_param_id %in% c(73) ~ "Cumulative case counts", # Bayesian GLM
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 15436 ~ "Intravenous fluid exposure",
              riskfactor_outcome %in% "Other" &
                covidence_id %in% 1375 ~ "Developing symptomatic disease given reported contact",
              TRUE ~ NA
            )
        ) %>%
        mutate(
          # Fix entry errors
          # Add missed Country
          population_country = ifelse(
            covidence_id %in% c(1170, 18371), "Sierra Leone", population_country
          ),

          # Correct entry cov ID 16279
          parameter_type = ifelse(
            covidence_id %in% 16279 &
              parameter_type %in%
                "Human delay - Symptom Onset/Fever to Death" &
              parameter_class %in% "Risk factors",
            "Risk factors", parameter_type
          ),

          # Correct entry cov ID 4900
          parameter_lower_bound = ifelse(
            covidence_id %in% 4900 &
              parameter_type %in%
                "Human delay - Symptom Onset/Fever to Admission to Care/Hospitalisation",
            3.4, parameter_lower_bound
          ),
          parameter_upper_bound = ifelse(
            covidence_id %in% 4900 &
              parameter_type %in%
                "Human delay - Symptom Onset/Fever to Admission to Care/Hospitalisation",
            6.0, parameter_upper_bound
          ),
          parameter_value_type = ifelse(
            covidence_id %in% 4900 &
              parameter_type %in%
                "Human delay - Symptom Onset/Fever to Admission to Care/Hospitalisation",
            "Mean", parameter_value_type
          ),

          # Correct entry cov ID 1889
          parameter_value = ifelse(
            covidence_id %in% 1889 &
              parameter_type %in% "Severity - case fatality rate (CFR)",
            95.5, parameter_value
          ),

          # Correct entry cov ID 16951
          parameter_unit = ifelse(
            covidence_id %in% 16951 &
              parameter_type %in%
                "Human delay - Symptom Onset/Fever to Reporting",
            "Days", parameter_unit
          ),
          parameter_value_type = ifelse(
            covidence_id %in% 16951 &
              parameter_type %in%
                "Human delay - Symptom Onset/Fever to Reporting",
            "Median", parameter_value_type
          ),

          # Correct entry cov ID 57
          parameter_unit = ifelse(
            covidence_id %in% 57 &
              parameter_type %in%
                "Human delay - Symptom Onset/Fever to Death",
            "Days", parameter_unit
          ),

          # Correct entry cov ID 3470
          parameter_unit = ifelse(
            covidence_id %in% 3470 &
              parameter_type %in%
                "Human delay - latent period",
            "Days", parameter_unit
          ),

          # Correct entry cov ID 4787
          parameter_from_figure = ifelse(
            covidence_id %in% 4787 &
              parameter_class %in% "Reproduction number",
            TRUE, parameter_from_figure
          ),

          # Correct entry cov ID 18236
          population_country = ifelse(
            covidence_id %in% 18236 &
              parameter_type %in%
                "Human delay - Symptom Onset/Fever to Admission to Care/Hospitalisation",
            "Guinea", population_country
          ),

          # Correct entry cov ID 18372
          population_country = ifelse(
            covidence_id %in% 18372 &
              parameter_class %in% "Human delay",
            "DRC", population_country
          ),

          # Correct entry cov ID 8691
          parameter_lower_bound = ifelse(
            covidence_id %in% 8691 &
              delay_short %in% "Symptom onset to death",
            7, parameter_lower_bound
          ),

          # Correct entry cov ID 16951
          parameter_uncertainty_type = case_when(
            covidence_id %in% 16951 &
              delay_short %in% "Health center visit to symptom onset" ~ "Range",
            TRUE ~ parameter_uncertainty_type
          ),
          parameter_uncertainty_lower_value = case_when(
            covidence_id %in% 16951 &
              delay_short %in% "Health center visit to symptom onset" ~
              parameter_lower_bound,
            TRUE ~ parameter_uncertainty_lower_value
          ),
          parameter_uncertainty_upper_value = case_when(
            covidence_id %in% 16951 &
              delay_short %in% "Health center visit to symptom onset" ~
              parameter_upper_bound,
            TRUE ~ parameter_uncertainty_upper_value
          ),
          parameter_lower_bound = case_when(
            covidence_id %in% 16951 &
              delay_short %in% "Health center visit to symptom onset" ~ NA,
            TRUE ~ parameter_lower_bound
          ),
          parameter_upper_bound = case_when(
            covidence_id %in% 16951 &
              delay_short %in% "Health center visit to symptom onset" ~ NA,
            TRUE ~ parameter_upper_bound
          ),

          # Correct entry cov ID 17715
          parameter_value = case_when(
            covidence_id %in% 17715 & parameter_class %in% "Human delay" ~ 31.25,
            TRUE ~ parameter_value
          ),
          parameter_value_type = case_when(
            covidence_id %in% 17715 & parameter_class %in% "Human delay" ~ "Mean",
            TRUE ~ parameter_value_type
          ),
          parameter_uncertainty_type = case_when(
            covidence_id %in% 17715 & parameter_class %in% "Human delay" ~ "Range",
            TRUE ~ parameter_uncertainty_type
          ),
          parameter_uncertainty_lower_value = case_when(
            covidence_id %in% 17715 & parameter_class %in% "Human delay" ~ 11,
            TRUE ~ parameter_uncertainty_lower_value
          ),
          parameter_uncertainty_upper_value = case_when(
            covidence_id %in% 17715 & parameter_class %in% "Human delay" ~ 71,
            TRUE ~ parameter_uncertainty_upper_value
          ),
          inverse_param = case_when(
            covidence_id %in% 17715 & parameter_class %in% "Human delay" ~ FALSE,
            TRUE ~ inverse_param
          ),

          # Correct cov if 5889
          parameter_uncertainty_type = case_when(
            covidence_id %in% 5889 & parameter_class %in% "Human delay" ~ "HDPI 95%",
            TRUE ~ parameter_uncertainty_type
          ),

          # Correct cov id 5935
          inverse_param = case_when(
            covidence_id %in% 5935 & parameter_class %in% "Human delay" ~ TRUE,
            TRUE ~ inverse_param
          ),

          # Correct missing parameter units for severity
          parameter_unit = case_when(
            covidence_id %in% c(4967, 5404, 4900, 2150) &
              parameter_class %in% "Severity" ~ "Percentage (%)",
            TRUE ~ parameter_unit
          ),

          # Correct parameter_value_type for NA/unspecified entries
          parameter_value_type =
            case_when(
              parameter_class %in% "Reproduction number" &
                is.na(parameter_value_type) &
                covidence_id %in% c(
                  65, 507, 701, 3814, 3777, 2882, 9378, 2065, 1053, 18372,
                  5033, 4991, 16599, 17200, 17730, 18944, 23719, 11620, 11565
                ) ~ "Unspecified",
              parameter_class %in% "Reproduction number" &
                is.na(parameter_value_type) & covidence_id %in% c(
                30, 754, 885, 1053, 8709, 19236, 4966
              ) ~ "Mean",
              parameter_class %in% "Reproduction number" &
                covidence_id %in% 944 ~ "Mean",
              parameter_class %in% "Reproduction number" &
                covidence_id %in% 6003 ~ "Median",
              parameter_class %in% "Reproduction number" &
                is.na(parameter_value_type) &
                covidence_id %in% 17881 ~ "Median",
              parameter_class %in% "Human delay" &
                is.na(parameter_value_type) & covidence_id %in% c(
                30, 57, 885, 5005, 9546, 16599, 18372, 19236
              ) ~ "Mean",
              parameter_class %in% "Human delay" &
                is.na(parameter_value_type) &
                covidence_id %in% c(1071, 3776, 8691) ~ "Median",
              parameter_class %in% "Human delay" &
                is.na(parameter_value_type) &
                covidence_id %in% c(6472, 7199) ~ "Unspecified",
              parameter_type %in% "Overdispersion" &
                is.na(parameter_value_type) &
                covidence_id %in% c(2065, 5940) ~ "Unspecified",
              parameter_type %in% "Overdispersion" &
                is.na(parameter_value_type) &
                covidence_id %in% c(4787) ~ "Mean",
              TRUE ~ parameter_value_type
            ),

          # Add missing method_r for overdispersion parameters
          method_r =
            case_when(
              parameter_type %in% "Overdispersion" & is.na(method_r) &
                covidence_id %in% c(1015, 3058, 5940, 24286) ~ "Branching process",
              parameter_type %in% "Overdispersion" & is.na(method_r) &
                covidence_id %in% 18536 ~ "Empirical (contact tracing)",
              parameter_type %in% "Overdispersion" & is.na(method_r) &
                covidence_id %in% c(3470, 3471) ~ "Other",
              TRUE ~ method_r
            ),

          # fix the survey dates
          population_study_start_day =
            case_when(
              covidence_id %in% 404 ~ 1,
              covidence_id %in% 4364 & parameter_class %in% "Severity" ~ 30,
              covidence_id %in% 1012 ~ 1,
              covidence_id %in% 1170 ~ 27,
              covidence_id %in% 18372 & parameter_class %in% "Human delay" ~ 5,
              covidence_id %in% 17956 & parameter_class %in% "Human delay" ~ 3,
              covidence_id %in% 16951 & parameter_class %in% "Human delay" ~ 30,
              covidence_id %in% 23669 & parameter_class %in% "Human delay" ~ 14,
              TRUE ~ population_study_start_day
            ),
          population_study_end_day =
            case_when(
              covidence_id %in% 404 ~ 12,
              covidence_id %in% 1170 ~ 31,
              covidence_id %in% 4364 & parameter_class %in% "Severity" ~ 28,
              covidence_id %in% 18372 & parameter_class %in% "Human delay" ~ 2,
              covidence_id %in% 17956 & parameter_class %in% "Human delay" ~ 27,
              covidence_id %in% 11688 ~ 12,
              TRUE ~ population_study_end_day
            ),
          population_study_start_month =
            case_when(
              covidence_id %in% 404 ~ "Mar",
              covidence_id %in% 1170 ~ "May",
              covidence_id %in% 1686 ~ "Dec",
              covidence_id %in% 1888 ~ "Oct",
              covidence_id %in% 4364 & parameter_class %in% "Severity" ~ "Dec",
              covidence_id %in% 18372 & parameter_class %in% "Human delay" ~ "Aug",
              covidence_id %in% 1012 ~ "Jul",
              covidence_id %in% 17956 & parameter_class %in% "Human delay" ~ "Jul",
              covidence_id %in% 11688 ~ "Jan",
              covidence_id %in% 16951 & parameter_class %in% "Human delay" ~ "Apr",
              covidence_id %in% 23669 & parameter_class %in% "Human delay" ~ "Mar",
              TRUE ~ population_study_start_month
            ),
          population_study_end_month =
            case_when(
              population_study_end_month %in% "3" ~ "Mar",
              covidence_id %in% 404 ~ "Jul",
              covidence_id %in% 1170 ~ "Aug",
              covidence_id %in% 1686 ~ "Jan",
              covidence_id %in% 1888 ~ "Feb",
              covidence_id %in% 4364 & parameter_class %in% "Severity" ~ "Sep",
              covidence_id %in% 18372 & parameter_class %in% "Human delay" ~ "Feb",
              covidence_id %in% 17956 & parameter_class %in% "Human delay" ~ "Jun",
              covidence_id %in% 23669 & parameter_class %in% "Human delay" ~ "Apr",
              covidence_id %in% 11688 ~ "Oct",
              TRUE ~ population_study_end_month
            ),
          population_study_end_month = gsub("[^a-zA-Z]", "", population_study_end_month),
          population_study_end_month = substr(population_study_end_month, 1, 3),
          population_study_start_month = gsub("[^a-zA-Z]", "", population_study_start_month),
          population_study_start_month = substr(population_study_start_month, 1, 3),
          population_study_start_year =
            case_when(
              covidence_id %in% 404 ~ 1995,
              covidence_id %in% 1170 ~ 2014,
              covidence_id %in% 1686 ~ 2014,
              covidence_id %in% 1012 ~ 2014,
              covidence_id %in% 1653 ~ 2014,
              covidence_id %in% 1888 ~ 2000,
              covidence_id %in% 4364 & parameter_class %in% "Severity" ~ 2013,
              covidence_id %in% 18372 & parameter_class %in% "Human delay" ~ 2018,
              covidence_id %in% 18535 ~ 1995,
              covidence_id %in% 17956 & parameter_class %in% "Human delay" ~ 2014,
              covidence_id %in% 11688 ~ 2014,
              covidence_id %in% 16951 & parameter_class %in% "Human delay" ~ 2018,
              covidence_id %in% 23669 & parameter_class %in% "Human delay" ~ 2014,
              covidence_id %in% 23720 ~ 2000,
              covidence_id %in% 23986 ~ 1995,
              TRUE ~ population_study_start_year
            ),
          population_study_end_year =
            case_when(
              covidence_id %in% 404 ~ 1995,
              covidence_id %in% 904 & parameter_value %in% 65.9 ~ 2014,
              covidence_id %in% 1170 ~ 2014,
              covidence_id %in% 1653 ~ 2014,
              covidence_id %in% 1686 ~ 2015,
              covidence_id %in% 1888 ~ 2001,
              covidence_id %in% 4364 & parameter_class %in% "Severity" ~ 2015,
              covidence_id %in% 11688 ~ 2014,
              covidence_id %in% 18372 & parameter_class %in% "Human delay" ~ 2020,
              covidence_id %in% 18535 ~ 1995,
              covidence_id %in% 17956 & parameter_class %in% "Human delay" ~ 2015,
              covidence_id %in% 23669 & parameter_class %in% "Human delay" ~ 2016,
              covidence_id %in% 23507 ~ 2016,
              covidence_id %in% 23720 ~ 2015,
              covidence_id %in% 23986 ~ 1995,
              TRUE ~ population_study_end_year
            ),

          # Inverse parameters
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
            ),

          # Add article id and parameter ids for new parameters
          id =
            case_when(
              covidence_id %in% 104 & is.na(id) ~
                unique(id[covidence_id %in% 104 & parameter_type %in% "Risk factors"]),
              covidence_id %in% 2548 & is.na(id) ~
                unique(id[covidence_id %in% 2548 & !is.na(id)]),
              covidence_id %in% 1044 & is.na(id) ~
                unique(id[covidence_id %in% 1044 & !is.na(id)]),
              TRUE ~ id
            )
        )

      idx <- which(df$covidence_id %in% c(104, 1044, 2548) & is.na(df$parameter_data_id))
      df$parameter_data_id[idx] <-
        random_id(n = length(idx), use_openssl = FALSE)
    }

    if (pathogen == "LASSA") {
      # removed parameters
      rmrows <- paste(c(61, 152), c(3, 1), sep = "_")
      df <- df %>%
        mutate(temp_col = paste(covidence_id, access_param_id, sep = "_")) %>%
        filter(!temp_col %in% rmrows) %>%
        select(-temp_col) %>%
        # correct parameter types
        mutate(
          parameter_type = case_when(
            covidence_id %in% 854 & access_param_id %in% c(5, 6) ~ "Human delay - time in care (length of stay)",
            TRUE ~ parameter_type
          ),
          other_delay_start = case_when(
            covidence_id %in% 854 & access_param_id %in% c(5, 6) ~ NA_character_,
            TRUE ~ other_delay_start
          ),
          other_delay_end = case_when(
            covidence_id %in% 854 & access_param_id %in% c(5, 6) ~ NA_character_,
            TRUE ~ other_delay_end
          ),
          # numeric parameter values
          parameter_value = as.numeric(parameter_value),
          # parameter value type consistency
          parameter_value_type = case_when(
            is.na(parameter_value) ~ NA_character_,
            !is.na(parameter_value) & is.na(parameter_value_type) ~ "Unspecified",
            TRUE ~ parameter_value_type
          ),
          # unspecified cfr/ifr methods
          cfr_ifr_method = case_when(
            parameter_class %in% "Severity" & is.na(cfr_ifr_method) ~ "Unspecified",
            TRUE ~ cfr_ifr_method
          ),
          # specify other risk factor outcomes
          riskfactor_outcome = case_when(
            covidence_id %in% 2627 & access_param_id %in% 22 ~ "Reproduction Number",
            covidence_id %in% 3153 & access_param_id %in% 33 ~ "Onset-Admission Delay",
            covidence_id %in% 920 & access_param_id %in% 4 ~ "Viremia",
            covidence_id %in% 1328 & access_param_id %in% c(16, 17, 18, 19) ~ "Occurrence",
            covidence_id %in% 441 & access_param_id %in% c(6, 7) ~ "Occurrence",
            covidence_id %in% 2661 & access_param_id %in% c(15) ~ "Occurrence",
            covidence_id %in% 2661 & access_param_id %in% 16 ~ "Incidence",
            TRUE ~ riskfactor_outcome
          ),
          # unnecessary risk factor occupations
          riskfactor_occupation = case_when(
            !is.na(riskfactor_occupation) & riskfactor_occupation %in% "Unspecified" ~ NA_character_,
            TRUE ~ riskfactor_occupation
          ),
          # location consistency
          population_location = str_to_title(sub("^\\s+", "", population_location)),
          # unspecified timing
          method_moment_value = case_when(
            is.na(method_moment_value) ~ "Unspecified",
            TRUE ~ method_moment_value
          ),
          # correct contexts
          population_sample_type = case_when(
            covidence_id %in% 669 ~ "Household based",
            covidence_id %in% 652 ~ "Community based",
            covidence_id %in% 4684 ~ "Community based",
            TRUE ~ population_sample_type
          ),
          population_group = case_when(
            covidence_id %in% 669 ~ "Mixed groups",
            covidence_id %in% 652 ~ "Other",
            TRUE ~ population_group
          ),
          # unspecified sex
          population_sex = case_when(
            is.na(population_sex) ~ "Unspecified",
            TRUE ~ population_sex
          )
        )
    }

    ## GENERIC CLEANING AGAIN:

    df <- df %>%
      mutate(

        # Rounding of parameter values and uncertainty
        across(
          c(
            parameter_value, parameter_lower_bound, parameter_upper_bound,
            parameter_uncertainty_single_value,
            parameter_uncertainty_lower_value,
            parameter_uncertainty_upper_value
          ),
          ~ round(., digits = 2)
        ),

        # Parameter value
        # Combine central upper and lower bounds
        parameter_bounds =
          ifelse(!is.na(parameter_lower_bound) & !is.na(parameter_upper_bound),
            paste(parameter_lower_bound, "-", parameter_upper_bound),
            NA
          ),

        # Uncertainty
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
          ),
        comb_uncertainty =
          case_when(
            !is.na(parameter_uncertainty_lower_value) & !is.na(parameter_uncertainty_upper_value) ~
              paste(parameter_uncertainty_lower_value, "-", parameter_uncertainty_upper_value),
            !is.na(parameter_uncertainty_single_value) ~
              paste(parameter_uncertainty_single_value),
            TRUE ~ NA
          ),

        # shorten a longer method_r name
        method_r =
          ifelse(method_r %in% "Renewal equations / Branching process",
            "Branching process", method_r
          ),

        # parameter_type name consistency
        parameter_type =
          case_when(
            parameter_type %in% "Growth rate ®" ~ "Growth rate (r)",
            parameter_type %in% "Reproduction number (Effective; Re)" ~
              "Reproduction number (Effective, Re)",
            parameter_type %in% "Mutations ‚Äì substitution rate" ~
              "Mutations – substitution rate",
            TRUE ~ parameter_type
          )
      ) %>%
      select(-c("article_id", "access_param_id", "name_data_entry")) %>%
      relocate(c(id, parameter_data_id, covidence_id, pathogen)) %>%
      arrange(covidence_id)
  }

  df
}


#################################
# Add QA scores to article data #
#################################

# input: articles data and parameter data
# output: articles data with two new variables: model_only and article_qa_score

add_qa_scores <- function(articles_df, params_df) {
  # add a model_only variable to article data (as denominator for qa will be different)
  articles_df <- articles_df %>%
    mutate(
      model_only =
        as.numeric(!id %in% params_df$id)
    ) %>%
    # if an article is model_only, make 5-7 NA for consistency between scores
    mutate(
      qa_d5 = ifelse(model_only %in% 1, NA, qa_d5),
      qa_d6 = ifelse(model_only %in% 1, NA, qa_d6),
      qa_d7 = ifelse(model_only %in% 1, NA, qa_d7)
    ) %>%
    # add qa score to article data
    mutate(
      total_qa =
        rowSums(!is.na(
          select(., qa_m1, qa_m2, qa_a3, qa_a4, qa_d5, qa_d6, qa_d7)
        )),
      yes_score = rowSums(
        select(., qa_m1, qa_m2, qa_a3, qa_a4, qa_d5, qa_d6, qa_d7) == "Yes",
        na.rm = TRUE
      ),
      article_qa_score = ifelse(total_qa > 0, yes_score / total_qa * 100, NA)
    ) %>%
    select(-c(total_qa, yes_score))

  articles_df
}
