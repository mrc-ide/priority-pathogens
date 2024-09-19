lassa_cleaning <- function(df) {
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
    )) %>% # volume typos
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
    mutate(first_author_first_name = case_when(
      covidence_id %in% 2648 ~ "Shirley C.",
      covidence_id %in% 1447 ~ "N.A.",
      TRUE ~ first_author_first_name
    )) %>%
    mutate(first_author_surname = case_when(
      covidence_id %in% 2648 ~ "Nimo-Paintsil",
      covidence_id %in% 2585 ~ "Dalhat",
      covidence_id %in% 1033 ~ "Ehichioya",
      covidence_id %in% 661 ~ "Kerneis",
      TRUE ~ first_author_surname
    )) %>%
    mutate(first_author_surname = sub(".*\\.(.*)", "\\1", first_author_surname)) %>%
    mutate(first_author_surname = sub("^\\s+", "", first_author_surname)) 
     # revised qa after parameter removed: now outbreak only
  df[df$covidence_id %in% 152, c("qa_m1", "qa_m2", "qa_a3", "qa_a4", "qa_d6", "qa_d7")] <- NA
  df
}

lassa_models_cleaning <- function(df) {
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
  df
}

lassa_outbreaks_cleaning <- function(df) {
  # clean locations
  df <- df %>%
    mutate(outbreak_location = gsub(",", ";", outbreak_location)) %>%
    mutate(outbreak_location = gsub("and", ";", outbreak_location)) %>%
    mutate(outbreak_location = sub("^\\s+", "", outbreak_location)) %>%
        mutate(outbreak_location = case_when(
      covidence_id %in% 560 & access_outbreak_id %in% 3 ~ "Kenema Government Hospital",
      TRUE ~ outbreak_location
    )) %>%
    mutate(outbreak_location = str_to_title(outbreak_location)) %>%
    # unspecified case detection mode
    mutate(cases_mode_detection = case_when(
      is.na(cases_mode_detection) ~ "Unspecified",
      TRUE ~ cases_mode_detection
    ))
  df
}

lassa_params_cleaning <- function(df) {
  # removed parameters
  rmrows <- paste(c(61, 152, 920), c(3, 1, 4), sep = "_")
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
  df
}

more_lassa_cleaning_generic <- function(df) {
  # removed parameters
  rmrows <- paste(c(61, 152, 854), c(3, 1, 5), sep = "_")
  df <- df %>%
    mutate(temp_col = paste(covidence_id, access_param_id, sep = "_")) %>%
    filter(!temp_col %in% rmrows) %>%
    select(-temp_col) %>%
    # correct parameter types
    mutate(
      parameter_type = case_when(
        covidence_id == 854 & access_param_id %in% c(5, 6) ~ "Human delay - time in care (length of stay)",
        TRUE ~ parameter_type
      ),
      other_delay_start = case_when(
        covidence_id == 854 & access_param_id %in% c(5, 6) ~ NA_character_,
        TRUE ~ other_delay_start
      ),
      other_delay_end = case_when(
        covidence_id == 854 & access_param_id %in% c(5, 6) ~ NA_character_,
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
        parameter_class == "Severity" & is.na(cfr_ifr_method) ~ "Unspecified",
        TRUE ~ cfr_ifr_method
      ),
      # specify other risk factor outcomes
      riskfactor_outcome = case_when(
        covidence_id == 2627 & access_param_id == 22 ~ "Reproduction Number",
        covidence_id == 3153 & access_param_id == 33 ~ "Onset-Admission Delay",
        covidence_id == 920 & access_param_id == 4 ~ "Viremia",
        covidence_id == 1328 & access_param_id %in% c(16, 17, 18, 19) ~ "Occurrence",
        covidence_id == 441 & access_param_id %in% c(6, 7) ~ "Occurrence",
        covidence_id == 2661 & access_param_id %in% c(15) ~ "Occurrence",
        covidence_id == 2661 & access_param_id == 16 ~ "Incidence",
        TRUE ~ riskfactor_outcome
      ),
      # unnecessary risk factor occupations
      riskfactor_occupation = case_when(
        !is.na(riskfactor_occupation) & riskfactor_occupation == "Unspecified" ~ NA_character_,
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
        covidence_id == 669 ~ "Household based",
        covidence_id == 652 ~ "Community based",
        covidence_id == 4684 ~ "Community based",
        TRUE ~ population_sample_type
      ),
      population_group = case_when(
        covidence_id == 669 ~ "Mixed groups",
        covidence_id == 652 ~ "Other",
        TRUE ~ population_group
      ),
      # unspecified sex
      population_sex = case_when(
        is.na(population_sex) ~ "Unspecified", TRUE ~ population_sex
      )
    ) # Rounding of parameter values and uncertainty



  df
}
