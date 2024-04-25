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
    
    if (pathogen == "SARS") 
    {
      #journal consistency
      df <- df %>% 
        mutate(journal = str_to_title(journal)) %>%
        mutate(journal = sub("^The\\s+", "", journal)) %>%
        mutate(journal = ifelse(journal == 'Bmj','British Medical Journal',
                                ifelse(journal == 'Transactions Of The Royal Society Tropical Medicine And Hygiene','Transactions Of The Royal Society Of Tropical Medicine And Hygiene',        
                                       journal))) %>%
        #journal typos
        # mutate(journal = case_when(
        #   covidence_id == 870 ~ 'Transactions Of The Royal Society Of Tropical Medicine And Hygiene',
        #   TRUE ~ journal)) %>%
        #year typos
        mutate(year_publication = case_when(
          covidence_id == 613 ~ 2004,
          covidence_id == 3394 ~ 2018,
          covidence_id == 4233 ~ 2009,
          covidence_id == 4261 ~ 2006,
          covidence_id == 5861 ~ 2003,
          covidence_id == 5880 ~ 2008,
          covidence_id == 6144 ~ 2006,
          TRUE ~ year_publication)) %>%
        #volume typos
        mutate(volume = case_when(
          covidence_id == 2043 ~ 5,
          covidence_id == 6239 ~ 68,
          covidence_id == 7256 ~ 15,
          TRUE ~ volume)) %>%
        #issue typos
        mutate(issue = case_when(
          covidence_id %in% c(558, 4217,	4676,	7256,	5650,	5484) ~ '1',
          covidence_id %in% c(3421,	7260,	8759,	10922,	4261) ~ '2',
          covidence_id %in% c(1036,	1564,	3449,	3468,	4169,	5635,	11040) ~ '3', 
          covidence_id %in% c(1046,	5880,	6145) ~ '4',
          covidence_id %in% c(1102,	2043,	3343,	4675) ~ '5',
          covidence_id %in% c(2006,	11042) ~ '6',
          covidence_id %in% c(523,	4614, 5585) ~ '9',
          covidence_id %in% c(5589,	5595) ~ '10',
          covidence_id %in% c(4184) ~ '12', 
          covidence_id %in% c(5879) ~ '14', 
          covidence_id %in% c(5069) ~ '1554', 
          covidence_id %in% c(3375) ~ '5627', 
          covidence_id %in% c(5393) ~ '7433', 								
          TRUE ~ issue)) %>%
        #page typos
        mutate(page_first = case_when(
          covidence_id %in% c(793) ~ 32,
          covidence_id %in% c(1641) ~ 195,
          covidence_id %in% c(4614) ~ 925,
          # covidence_id %in% c(1413, 2567, 2610, 2661, 3215, 3258, 3635) ~ NA,
          TRUE ~ page_first)) %>%
        mutate(page_last = case_when(
          covidence_id == 1641 ~ 194,
          covidence_id == 4614 ~ 926,
          TRUE ~ page_last)) %>%
        #title typos
        mutate(article_title = case_when(
          covidence_id == 392 ~ 'Epidemiologic clues to SARS origin in China',
          covidence_id == 513 ~ 'Prevalence of subclinical infection by the SARS coronavirus among general practitioners in Hong Kong',
          covidence_id == 613 ~ 'Characterization of severe acute respiratory syndrome coronavirus genomes in Taiwan: molecular epidemiology and genome evolution',
          covidence_id == 718 ~ 'Clinical description of a completed outbreak of SARS in Vietnam, February-May 2003',
          covidence_id == 727 ~ 'Risk of severe acute respiratory syndrome-associated coronavirus transmission aboard commercial aircraft',
          covidence_id == 745 ~ 'Brief Report: Incubation Period Duration and Severity of Clinical Disease Following Severe Acute Respiratory Syndrome Coronavirus Infection',
          covidence_id == 1046 ~ 'The SARS outbreak in a general hospital in Tianjin, China -- the case of super-spreader',
          covidence_id == 1564 ~ 'Theoretically estimated risk of severe acute respiratory syndrome transmission through blood transfusion during an epidemic in Shenzhen, Guangdong, China in 2003',
          covidence_id == 1641 ~ 'SARS surveillance during emergency public health response, United States, March-July 2003',
          covidence_id == 1672 ~ 'The effect of global travel on the spread of SARS',
          covidence_id == 2043 ~ 'Risk factors for SARS transmission from patients requiring intubation: a multicentre investigation in Toronto, Canada',
          covidence_id == 2613 ~ 'Rapid awareness and transmission of severe acute respiratory syndrome in Hanoi French Hospital, Vietnam',
          covidence_id == 4285 ~ 'SARS in Singapore--predictors of disease severity',
          covidence_id == 4455 ~ 'Severe acute respiratory syndrome (SARS) in Singapore: clinical features of index patient and initial contacts',
          covidence_id == 4561 ~ 'Neutralizing antibody response and SARS severity',
          covidence_id == 4612 ~ 'Secondary household transmission of SARS, Singapore',
          covidence_id == 5387 ~ 'Transmission characteristics of MERS and SARS in the healthcare setting: a comparative study',
          covidence_id == 5589 ~ 'A simple approximate mathematical model to predict the number of severe acute respiratory syndrome cases and deaths',
          covidence_id == 5635 ~ 'Effectiveness of noninvasive positive pressure ventilation in the treatment of acute respiratory failure in severe acute respiratory syndrome',
          covidence_id == 5743 ~ 'The outbreak of SARS at Tan Tock Seng Hospital--relating epidemiology to control',
          covidence_id == 5878 ~ 'Alternative methods of estimating an incubation distribution: examples from severe acute respiratory syndrome',
          covidence_id == 7260 ~ 'Reconstruction of the infection curve for SARS epidemic in Beijing, China using a back-projection method',
          covidence_id == 12002 ~ 'Heterogeneous and Stochastic Agent-Based Models for Analyzing Infectious Diseases Super Spreaders',
          TRUE ~ article_title)) %>%
        #title consistency
        mutate(article_title = gsub(";", ",", article_title)) %>%
        mutate(article_title = gsub("\n", " ", article_title)) %>%
        mutate(article_title = gsub("\\s+", " ", article_title)) %>%
        mutate(article_title = str_to_title(article_title)) %>%
        #missing dois
        mutate(doi = sub(".*?10\\.", "10.", doi)) %>%
        mutate(doi = case_when(
          covidence_id == 1090 ~ '10.1016/s019606440300828x',
          covidence_id == 4261 ~ '10.1017/s0950268805004826',
          covidence_id == 4268 ~ '10.3201/eid1009.040155',
          covidence_id == 4387 ~ '10.1093/aje/kwh056',
          covidence_id == 5006 ~ '10.1007/s11325-004-0097-0',
          covidence_id == 5635 ~ '10.1378/chest.126.3.845',
          covidence_id == 5652 ~ '10.1001/jama.293.12.1450-c',
          covidence_id == 5682 ~ '10.1128/jcm.44.2.359-365.2006',
          covidence_id == 5879 ~ '10.1001/archinte.166.14.1505',
          covidence_id == 6239 ~ '10.1016/s1726-4901(09)70124-8',
          covidence_id == 7260 ~ '10.1080/03610910701792562',
          covidence_id == 8001 ~ '10.1360/03ww0126',
          covidence_id == 10922 ~ '10.1016/j.jmaa.2006.11.026',
          covidence_id == 10924 ~ '10.1137/040615547',
          covidence_id == 11000 ~ '10.3201/eid1309.070081',
          TRUE ~ doi)) %>%
        #paper copies
        # mutate(paper_copy_only = case_when(
        #   covidence_id %in% c(845,917) ~ FALSE,
        #   TRUE ~ paper_copy_only)) %>%
        #name typos
        # mutate(first_author_surname = case_when(
        #   covidence_id == 2648 ~ 'Shirley C.',
        #   covidence_id == 1447 ~ 'N.A.',
        #   TRUE ~ first_author_surname)) %>%
        mutate(first_author_first_name = sub(".*\\.(.*)", "\\1", first_author_first_name)) %>%
        mutate(first_author_first_name = sub("^\\s+", "", first_author_first_name)) #%>%                 
      # mutate(first_author_first_name = case_when(
      #   covidence_id == 2648 ~ 'Nimo-Paintsil',
      #   covidence_id == 2585 ~ 'Dalhat',
      #   covidence_id == 1033 ~ 'Ehichioya',
      #   covidence_id == 661 ~ 'Kerneis',
      #   TRUE ~ first_author_first_name))     
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
        # growth model (model only paper)
        filter(!covidence_id %in% 12389) %>%
        mutate(
          model_type = case_when(
            covidence_id %in% 5005 & model_type %in% "Unspecified" ~
              "Agent / Individual based", TRUE ~ model_type
          ),
          ebola_variant = case_when(
            covidence_id %in% 15947 ~ "Bundibugyo virus (BDBV)",
            covidence_id %in% 5675 ~
              "Bundibugyo virus (BDBV);Sudan virus (SUDV);Taï Forest virus (TAFV);Zaire Ebola virus (EBOV)",
            TRUE ~ ebola_variant
          )
        ) %>%
        # group decision to remove compartmental type because it's inconsistent
        select(-compartmental_type)
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
    
    if (pathogen == 'SARS') {
      df <- df %>% filter(!is.na(id))
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
  }

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
        population_location = str_replace_all(population_location, ";", ","),

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
  }

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
        # (e.g. 2857, 2858)/correspondence/wrong entries, 6471: sneaky duplicate,
        # 5898: mutation frequencies (not mutation rates as no time component)
        # this paper only had mutations so removing whole paper
        filter(!(covidence_id %in% c(1510, 2857, 2858, 4301, 5765, 5870, 5898, 6471, 17096))) %>%
        # Remove duplicate entry from single extracted paper not identified as distinct
        filter(!(covidence_id %in% 3532 & access_param_id %in% 37)) %>%
        # Remove risk factor for covidence ID 17054 and remove NA risk factor -
        # ID 2890 checked paper and potentially based on simulated data
        filter(!(covidence_id %in% c(2890, 17054) & parameter_type %in% "Risk factors")) %>%
        # Remove one risk factor from covidence ID 4764
        filter(!(riskfactor_name %in% "Other" &
          covidence_id %in% 4764 & access_param_id %in% 74)) %>%
        # 3681 - Remove 3 attack rates that are actually death rates
        # 6263 - not reported as rate or percentage, excluded due to no maths rule
        filter(!(covidence_id %in% c(3681, 6263) & parameter_class %in% "Attack rate")) %>%
        # Remove delay parameter where table and text don't match
        filter(!(covidence_id %in% 15544 & parameter_class %in% "Human delay")) %>%
        # Remove reproduction number entry without values
        filter(!(covidence_id %in% 4966 & parameter_class %in% "Reproduction number")) %>%
        # Remove severity estimates - 23507 = incorrect entry, 5654 = dupe of
        # separate Cherif 2018 but less info, 2124 = dupe of separate Sadek 1999 but less info,
        # 2947 = CFRs sourced from other studies
        filter(!(covidence_id %in% c(23507, 5654, 2124, 2947) & parameter_class %in% "Severity")) %>%
        # Remove mutation rate (null estimate)
        filter(!(covidence_id %in% 19237 & exponent %in% -5)) %>%
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
          
          # When "multi-country" change "location" to the full country list
          population_location =
            case_when(
              is.na(population_location) &
              population_country %in% c(
                "France, Germany, Italy, Mali, Netherlands, Nigeria, Norway, Senegal, Spain, Switzerland, United Kingdom, United States",
                "The Gambia, Guinea, Liberia, Nigeria, Senegal, Sierra Leone, United Kingdom, United States",
                "Guinea, Italy, Liberia, Mali, Nigeria, Senegal, Sierra Leone, Spain, United Kingdom, United States",
                "DRC, Republic of the Congo, Côte d'Ivoire, Gabon, South Africa, South Sudan, Uganda",
                "Cameroon, Central African Republic, Chad, Republic of the Congo, Equatorial Guinea, Gabon",
                "Cameroon, DRC, Republic of the Congo, Ghana, Uganda",
                "DRC, Republic of the Congo, Gabon, Guinea, Sierra Leone"
              ) ~ population_country,
              is.na(population_location) & covidence_id %in% 11688 ~
                "Guinea, Liberia, Nigeria, Sierra Leone, Spain, United States",
              is.na(population_location) & covidence_id %in% 509 ~
                "France, Germany, Italy, Netherlands, Norway, Switzerland, Spain, United Kingdom, United States",
              is.na(population_location) & covidence_id %in% 23720 ~
                "Congo, Guinea, Liberia, Nigeria, Sierra Leone, Uganda",
              is.na(population_location) & covidence_id %in% 654 ~
                "DRC, Italy, Ivory Coast, South Sudan, Philippines, US",
              is.na(population_location) & covidence_id %in% 2548 ~
                "DRC, Gabon, Italy, Ivory Coast, Philippines, Republic of the Congo, Sudan, Uganda, United States",
              TRUE ~ population_location
            ),
    
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
              population_country %in% c(
                "DRC, Republic of the Congo, Gabon, Guinea, Sierra Leone",
                "Cameroon, DRC, Republic of the Congo, Ghana, Uganda"
              ) ~ "Multi-country: Africa (n = 5)",
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
              # 654 = DRC, South Sudan, US, Philippines, Italy, Ivory Coast
              covidence_id %in% 654 ~
                "Multi-country: Africa, Asia, Europe, USA (n = 6)",
              # 2548 = DRC, Gabon, Italy, Ivory Coast, Philippines, Republic of the Congo, Sudan, Uganda, USA
              covidence_id %in% 2548 ~
                "Multi-country: Africa, Asia, Europe, USA (n = 9)",
              # Was unspecified and checked the paper
              covidence_id %in% 4568 ~ "Guinea, Liberia, Sierra Leone",
              covidence_id %in% c(18372, 18535) ~ "DRC",
              covidence_id %in% c(17835, 18236, 18536) ~ "Guinea",
              covidence_id %in% c(1170, 18371) ~ "Sierra Leone",
              covidence_id %in% 16201 ~ "Guinea",
              is.na(population_country) & covidence_id %in% 5005 ~ "Liberia",
              covidence_id %in% 5939 & is.na(population_country) ~ "Guinea, Liberia, Sierra Leone",
              # Correction
              parameter_class %in% "Attack rate" & covidence_id %in% 6472 ~ "DRC",
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
            other_delay_start %in% c("Who notification", "Notification") ~ "Reporting",
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
              "Other: Case report completion", "Who notification",
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
          delay_short = str_replace(delay_short, "\\bwho\\b", "WHO"),
          delay_short = str_replace(delay_short, "\\bWho\\b", "WHO"),
          delay_short = str_replace(delay_short, "\\brna\\b", "RNA"),
          delay_short = str_replace(delay_short, "igg antibody detection", "antibody detection (IgM/IgG)"),
          delay_short = str_replace(delay_short, "igm antibody detection", "antibody detection (IgM/IgG)"),
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
          # Parameter_value cleaning
          parameter_value = 
            case_when(
              parameter_class %in% "Human delay" &
                covidence_id %in% 17715 ~ 31.25,
              parameter_type %in% "Severity - case fatality rate (CFR)" &
                covidence_id %in% 1889 ~ 95.5,
              parameter_class %in% "Mutations" &
                covidence_id %in% 4763 ~ 1.07,
              parameter_class %in% "Mutations" &
                covidence_id %in% 6340 ~ 1.075,
              parameter_class %in% "Attack rate" &
                covidence_id %in% 2240 ~ 2.5,
              parameter_class %in% "Attack rate" &
                covidence_id %in% 241 ~ 0.043,
              parameter_class %in% "Seroprevalence" &
                covidence_id %in% c(3822, 4168, 16643) ~
                cfr_ifr_numerator/cfr_ifr_denominator * 100,
              parameter_class %in% "Severity" &
                covidence_id %in% 45 ~ 54.7,
              TRUE ~ parameter_value
            ),
          # Exponent
          exponent = 
            case_when(
              parameter_class %in% "Mutations" &
                covidence_id %in% 6340 ~ -3,
              parameter_class %in% "Attack rate" &
                covidence_id %in% c(241, 2240, 2241, 6472, 7199) ~ 0,
              TRUE ~ exponent
            ),
          # Parameter_type
          parameter_type =
            case_when(
              parameter_type %in% "Human delay - Symptom Onset/Fever to Death" &
                parameter_class %in% "Risk factors" &
                covidence_id %in% 16279 ~ "Risk factors",
              TRUE ~ parameter_type
              ),
          # Parameter_unit cleaning
          parameter_unit =
            case_when(
              # delays
              parameter_class %in% "Human delay" &
                parameter_unit %in% "Per day" ~ "Days",
              parameter_class %in% "Human delay" &
                (is.na(parameter_unit) | parameter_unit %in% "Unspecified") &
                covidence_id %in% c(57, 3470, 3776, 16951, 18371) ~ "Days",
              parameter_class %in% "Human delay" &
                parameter_unit %in% "Per week" ~ "Weeks",
              parameter_type %in% "Overdispersion" &
                is.na(parameter_unit) &
                covidence_id %in% c(2065, 4787, 5940, 23720) ~ "No units",
              # seroprevalence
              parameter_class %in% "Seroprevalence" &
                covidence_id %in% c(3822, 4168, 16643) ~ "Percentage (%)",
              # growth rate
              parameter_class %in% "Growth rate" &
                is.na(parameter_unit) &
                covidence_id %in% 2506 ~ "Unspecified",
              parameter_class %in% "Growth rate" &
                is.na(parameter_unit) &
                covidence_id %in% 15958 ~ "Per week",
              # severity
              parameter_class %in% "Severity" &
                covidence_id %in% c(4967, 5404, 4900, 2150) ~ "Percentage (%)",
              # mutations
              parameter_class %in% "Mutations" &
                covidence_id %in% 5898 &
                parameter_value %in% 2.2 ~ "Indels/base sequenced",
              parameter_class %in% "Mutations" &
                covidence_id %in% 5898 &
                parameter_value %in% 4.1 ~ "SNPs/nucleotide sequenced",
              parameter_class %in% "Mutations" &
                covidence_id %in% c(4763, 5197) ~ "Substitutions/site/year",
              # attack rate
              parameter_class %in% "Attack rate" &
                covidence_id %in% 4991 ~ "No units",
              parameter_class %in% "Attack rate" &
                  covidence_id %in% c(241, 2240, 2241, 4253) ~ "Percentage (%)",
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
          # Lower bounds
          parameter_lower_bound =
            case_when(
              parameter_type %in% "Human delay - Symptom Onset/Fever to Admission to Care/Hospitalisation" &
                covidence_id %in% 4900 ~ 3.4,
              delay_short %in% "Symptom onset to death" &
                covidence_id %in% 8691 ~ 7,
              delay_short %in% "Health center visit to symptom onset" &
                covidence_id %in% 16951 ~ NA,
              parameter_class %in% "Mutations" &
                covidence_id %in% 6340 ~ NA,
              parameter_class %in% "Attack rate" &
                covidence_id %in% 6472 ~ 0.1,
              parameter_class %in% "Attack rate" &
                covidence_id %in% 11467 ~ 7,
              parameter_class %in% "Attack rate" &
                covidence_id %in% 2241 ~ 0.01,
              parameter_class %in% "Attack rate" &
                population_sample_type %in% "Community based" &
                covidence_id %in% 7199 ~ 0.34,
              parameter_class %in% "Attack rate" &
                covidence_id %in% 241 ~ 0.003,
              parameter_class %in% "Severity" &
                covidence_id %in% 45 ~ 8.6,
              TRUE ~ parameter_lower_bound
            ),
          
          # Upper bounds
          parameter_upper_bound =
            case_when(
              parameter_type %in%
                "Human delay - Symptom Onset/Fever to Admission to Care/Hospitalisation" &
                covidence_id %in% 4900 ~ 6.0,
              delay_short %in% "Health center visit to symptom onset" &
                covidence_id %in% 16951 ~ NA,
              parameter_class %in% "Mutations" &
                covidence_id %in% 6340 ~ NA,
              parameter_class %in% "Attack rate" &
                covidence_id %in% 6472 ~ 0.8,
              parameter_class %in% "Attack rate" &
                covidence_id %in% 11467 ~ 19,
              parameter_class %in% "Attack rate" &
                covidence_id %in% 2241 ~ 0.238,
              parameter_class %in% "Attack rate" &
                population_sample_size %in% 280 &
                covidence_id %in% 7199 ~ 1.42,
              parameter_class %in% "Attack rate" &
                covidence_id %in% 241 ~ 0.185,
              parameter_class %in% "Severity" &
                covidence_id %in% 45 ~ 93.3,
              TRUE ~ parameter_upper_bound
            ),
          
          # Disaggregated
          method_disaggregated =
            case_when(
              parameter_class %in% "Severity" &
                covidence_id %in% 45 ~ TRUE,
              TRUE ~ method_disaggregated
            ),
          
          # Disaggregated by
          method_disaggregated_by =
            case_when(
              parameter_class %in% "Attack rate" &
                covidence_id %in% 11467 ~ "Age",
              parameter_class %in% "Attack rate" &
                covidence_id %in% 6472 ~ "Unspecified",
              parameter_class %in% "Attack rate" &
                population_sample_type %in% "Household based" &
                covidence_id %in% 7199 ~ "Level of exposure",
              # "Other" disaggregations:
              # 4253 - adjustments to try and remove bias
              # 4991 - 9 combinations of possible incubation period and
              # infectious period distributions
              parameter_class %in% "Attack rate" &
                !is.na(parameter_lower_bound) &
                covidence_id %in% c(4253, 4991) ~ "Other",
              # "Other" for 45: Viral load
              parameter_class %in% "Severity" &
                covidence_id %in% 45 ~ "Age, Other, Sex, Symptoms",
              TRUE ~ method_disaggregated_by
            ),
          
          # Uncertainty lower bounds
          parameter_uncertainty_lower_value = case_when(
            covidence_id %in% 16951 &
              delay_short %in% "Health center visit to symptom onset" ~
              parameter_lower_bound,
            parameter_class %in% "Human delay" &
              covidence_id %in% 17715 ~ 11,
            parameter_class %in% "Mutations" &
              covidence_id %in% 6340 ~ 0.932, # putting it on same exponent scale
            TRUE ~ parameter_uncertainty_lower_value
          ),
          
          # Uncertainty upper bounds
          parameter_uncertainty_upper_value =
            case_when(
              delay_short %in% "Health center visit to symptom onset" &
                covidence_id %in% 16951 ~ parameter_upper_bound,
              parameter_class %in% "Human delay" &
                covidence_id %in% 17715 ~ 71,
              parameter_class %in% "Mutations" &
                covidence_id %in% 6340 ~ 1.22, # putting it on same exponent scale
              TRUE ~ parameter_uncertainty_upper_value
            ),

          # Uncertainty type
          parameter_uncertainty_type =
            case_when(
              delay_short %in% "Health center visit to symptom onset" &
                covidence_id %in% 16951 ~ "Range",
              parameter_class %in% "Human delay" &
                covidence_id %in% 17715 ~ "Range",
              parameter_class %in% "Human delay" &
                covidence_id %in% 5889 ~ "HDPI 95%",
              TRUE ~ parameter_uncertainty_type
            ),

          # Correct parameter_value_type for NA/unspecified entries
          parameter_value_type =
            case_when(
              # reproduction number
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
              # Human delays
              parameter_class %in% "Human delay" &
                is.na(parameter_value_type) & covidence_id %in% c(
                30, 57, 885, 4900, 5005, 9546, 16599, 17715, 18372, 19236
              ) ~ "Mean",
              parameter_class %in% "Human delay" &
                is.na(parameter_value_type) &
                covidence_id %in% c(1071, 3776, 8691) ~ "Median",
              parameter_type %in%
                "Human delay - Symptom Onset/Fever to Reporting" &
                covidence_id %in% 16951 ~ "Median",
              parameter_class %in% "Human delay" &
                is.na(parameter_value_type) &
                covidence_id %in% c(6472, 7199) ~ "Unspecified",
              # Overdispersion
              parameter_type %in% "Overdispersion" &
                is.na(parameter_value_type) &
                covidence_id %in% c(2065, 5940) ~ "Unspecified",
              parameter_type %in% "Overdispersion" &
                is.na(parameter_value_type) &
                covidence_id %in% c(4787) ~ "Mean",
              # Growth rate
              parameter_class %in% "Growth rate" &
                is.na(parameter_value_type) &
                covidence_id %in% c(442, 507) ~ "Unspecified",
              # Doubing time
              parameter_class %in% "Doubling time" &
                is.na(parameter_value_type) &
                covidence_id %in% 6346 ~ "Unspecified",
              # Mutations
              parameter_class %in% "Mutations" &
                is.na(parameter_value_type) &
                covidence_id %in% c(1407, 4763, 5197) ~ "Mean",
              # Attack rate
              parameter_class %in% "Attack rate" &
                is.na(parameter_value_type) &
                covidence_id %in% c(241, 1030, 1053, 2240, 2241, 3052, 4829,
                                    4991, 5404, 7199, 11467) ~ "Other", # "Rates"
              parameter_class %in% "Attack rate" &
                covidence_id %in% 6472 ~ "Unspecified",
              TRUE ~ parameter_value_type
            ),

          # Method_r for overdispersion parameters
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

          # Survey dates
          population_study_start_day =
            case_when(
              covidence_id %in% 404 ~ 1,
              covidence_id %in% 4364 & parameter_class %in% "Severity" ~ 30,
              covidence_id %in% 1012 ~ 1,
              covidence_id %in% 1170 ~ 27,
              covidence_id %in% 16201 & parameter_class %in% "Seroprevalence" ~ 23,
              covidence_id %in% 18372 & parameter_class %in% "Human delay" ~ 5,
              covidence_id %in% 17956 & parameter_class %in% "Human delay" ~ 3,
              covidence_id %in% 16951 & parameter_class %in% "Human delay" ~ 30,
              covidence_id %in% 23669 & parameter_class %in% "Human delay" ~ 14,
              covidence_id %in% 5005 & parameter_class %in% "Risk factors" ~ 28,
              TRUE ~ population_study_start_day
            ),
          population_study_end_day =
            case_when(
              covidence_id %in% 404 ~ 12,
              covidence_id %in% 1170 ~ 31,
              covidence_id %in% 16201 & parameter_class %in% "Seroprevalence" ~ 11,
              covidence_id %in% 4364 & parameter_class %in% "Severity" ~ 28,
              covidence_id %in% 18372 & parameter_class %in% "Human delay" ~ 2,
              covidence_id %in% 17956 & parameter_class %in% "Human delay" ~ 27,
              covidence_id %in% 5005 & parameter_class %in% "Risk factors" ~ 1,
              covidence_id %in% 11688 ~ 12,
              TRUE ~ population_study_end_day
            ),
          population_study_start_month =
            case_when(
              covidence_id %in% 404 ~ "Mar",
              covidence_id %in% 1170 ~ "May",
              covidence_id %in% 1407 ~ "Mar",
              covidence_id %in% 1686 ~ "Dec",
              covidence_id %in% 1888 ~ "Oct",
              covidence_id %in% 1912 ~ "Aug",
              covidence_id %in% 1911 ~ "Aug",
              covidence_id %in% 4364 & parameter_class %in% "Severity" ~ "Dec",
              covidence_id %in% 18372 & parameter_class %in% "Human delay" ~ "Aug",
              covidence_id %in% 1012 ~ "Jul",
              covidence_id %in% 17956 & parameter_class %in% "Human delay" ~ "Jul",
              covidence_id %in% 11688 ~ "Jan",
              covidence_id %in% 16951 & parameter_class %in% "Human delay" ~ "Apr",
              covidence_id %in% 23669 & parameter_class %in% "Human delay" ~ "Mar",
              covidence_id %in% c(2497, 16201) & parameter_class %in% "Seroprevalence" ~ "Mar",
              covidence_id %in% 5005 & parameter_class %in% "Risk factors" ~ "Feb",
              TRUE ~ population_study_start_month
            ),
          population_study_end_month =
            case_when(
              population_study_end_month %in% "3" ~ "Mar",
              covidence_id %in% 404 ~ "Jul",
              covidence_id %in% 1170 ~ "Aug",
              covidence_id %in% 1407 ~ "Dec",
              covidence_id %in% 1686 ~ "Jan",
              covidence_id %in% 1888 ~ "Feb",
              covidence_id %in% 1911 ~ "Aug",
              covidence_id %in% 1912 ~ "Sep",
              covidence_id %in% 4364 & parameter_class %in% "Severity" ~ "Sep",
              covidence_id %in% 18372 & parameter_class %in% "Human delay" ~ "Feb",
              covidence_id %in% 17956 & parameter_class %in% "Human delay" ~ "Jun",
              covidence_id %in% 23669 & parameter_class %in% "Human delay" ~ "Apr",
              covidence_id %in% 11688 ~ "Oct",
              covidence_id %in% 16201 & parameter_class %in% "Seroprevalence" ~ "Jul",
              covidence_id %in% 2497 & parameter_class %in% "Seroprevalence" ~ "Dec",
              covidence_id %in% 5005 & parameter_class %in% "Risk factors" ~ "Dec",
              TRUE ~ population_study_end_month
            ),
          population_study_end_month = gsub("[^a-zA-Z]", "", population_study_end_month),
          population_study_end_month = substr(population_study_end_month, 1, 3),
          population_study_start_month = gsub("[^a-zA-Z]", "", population_study_start_month),
          population_study_start_month = substr(population_study_start_month, 1, 3),
          population_study_start_year =
            case_when(
              covidence_id %in% 654 ~ 1976,
              covidence_id %in% c(404, 18535, 23986) ~ 1995,
              covidence_id %in% c(1888, 23720) ~ 2000,
              covidence_id %in% 1911 ~ 2002,
              covidence_id %in% 1912 ~ 2007,
              covidence_id %in% 4364 & parameter_class %in% "Severity" ~ 2013,
              covidence_id %in% c(1170, 1686, 1407, 1012, 1653, 4209, 11688) ~ 2014,
              covidence_id %in% c(17956, 23669) & parameter_class %in% "Human delay" ~ 2014,
              covidence_id %in% c(2497, 16201) & parameter_class %in% "Seroprevalence" ~ 2015,
              covidence_id %in% c(16951, 18372) & parameter_class %in% "Human delay" ~ 2018,
              covidence_id %in% 5005 & parameter_class %in% "Risk factors" ~ 2014,
              covidence_id %in% 6471 ~ 1976,
              covidence_id %in% 24646 ~ 1995,
              TRUE ~ population_study_start_year
            ),
          population_study_end_year =
            case_when(
              covidence_id %in% c(404, 18535, 23986) ~ 1995,
              covidence_id %in% 1888 ~ 2001,
              covidence_id %in% 1911 ~ 2002,
              covidence_id %in% 1912 ~ 2007,
              covidence_id %in% c(1170, 1407, 1653, 4209, 11688) ~ 2014,
              covidence_id %in% 904 & parameter_value %in% 65.9 ~ 2014,
              covidence_id %in% c(1686, 23720) ~ 2015,
              covidence_id %in% 17956 & parameter_class %in% "Human delay" ~ 2015,
              covidence_id %in% 4364 & parameter_class %in% "Severity" ~ 2015,
              covidence_id %in% 23507 ~ 2016,
              covidence_id %in% 2497 & parameter_class %in% "Seroprevalence" ~ 2015,
              covidence_id %in% 16201 & parameter_class %in% "Seroprevalence" ~ 2016,
              covidence_id %in% 23669 & parameter_class %in% "Human delay" ~ 2016,
              covidence_id %in% 18372 & parameter_class %in% "Human delay" ~ 2020,
              covidence_id %in% 5005 & parameter_class %in% "Risk factors" ~ 2014,
              covidence_id %in% 6471 ~ 1976,
              covidence_id %in% 24646 ~ 1995,
              TRUE ~ population_study_end_year
            ),
          
          # Sample size
          population_sample_size =
            case_when(
              parameter_class %in% "Attack rate" &
              covidence_id %in% 3052 ~ 17,
              parameter_class %in% "Attack rate" &
                covidence_id %in% 2240 ~ 1000,
              parameter_class %in% "Attack rate" &
                covidence_id %in% 1053 ~ 301,
              parameter_class %in% "Attack rate" &
                population_sample_type %in% "Household based" &
                covidence_id %in% 7199 ~ 17,
              parameter_class %in% "Seroprevalence" &
                covidence_id %in% 2497 ~ 694,
              TRUE ~ population_sample_size
            ),
          
          # Population group
          population_group =
            case_when(
              parameter_class %in% "Attack rate" &
                covidence_id %in% c(1053, 2240, 2241) ~ "Persons under investigation",
              TRUE ~ population_group
            ),
          
          # Sample type
          population_sample_type =
            case_when(
              parameter_class %in% "Attack rate" &
                covidence_id %in% 2240 ~ "Contact based",
              parameter_class %in% "Attack rate" &
                covidence_id %in% 2241 ~ "Community based",
              parameter_class %in% "Attack rate" &
                parameter_value %in% 12 &
                covidence_id %in% 7199 ~ "Household based",
              parameter_class %in% "Seroprevalence" &
                covidence_id %in% c(2497, 16201) ~ "Hospital based",
              parameter_class %in% "Seroprevalence" &
                covidence_id %in% 2354 ~ "Population based",
              parameter_class %in% "Seroprevalence" &
                covidence_id %in% 16757 ~ "Other",
              TRUE ~ population_sample_type
            ),
          
          # Create attack_rate_type variable 
          # NOTE: covidence ids 17111 and 19193 are from figure only
          attack_rate_type =
            case_when(
              # Primary attack rate (population based)
              parameter_class %in% "Attack rate" &
                covidence_id %in% c(241, 1030, 1044, 2241, 2328, 3052, 5404) ~ "Primary",
              parameter_class %in% "Attack rate" &
                population_sample_type %in% "Community based" &
                covidence_id %in% 7199 ~ "Primary",
                # Secondary attack rate (contact based)
              parameter_class %in% "Attack rate" &
                covidence_id %in% c(1053, 1749, 2240, 4253, 4829, 4991, 6472,
                                    11467, 4796, 5601, 6470, 6471) ~ "Secondary",
              parameter_class %in% "Attack rate" &
                population_sample_type %in% "Household based" &
              covidence_id %in% 7199 ~ "Secondary",
              TRUE ~ NA
            ),
          
          # cfr
          cfr_ifr_method = case_when(
            parameter_class %in% "Severity" & covidence_id %in% 45 ~ "Naive",
            TRUE ~ cfr_ifr_method
          ),
          cfr_ifr_numerator = case_when(
            parameter_class %in% "Severity" & covidence_id %in% 45 ~ 76,
            TRUE ~ cfr_ifr_numerator
          ),
          cfr_ifr_denominator = case_when(
            parameter_class %in% "Severity" & covidence_id %in% 45 ~ 139,
            TRUE ~ cfr_ifr_denominator
          ),

          # Clean genome_site for mutation rates
          genome_site = str_replace_all(genome_site, ";", ","),
            #(?i) makes it case-insensitive
          genome_site = str_replace(genome_site, "(?i)whole genome", "Whole genome"),
          genome_site = str_replace(genome_site, "Complete genome", "Whole genome"),
            # coding complete = all ORFs are complete (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4068259/)
          genome_site = str_replace(genome_site, "Coding-complete genomes", "Partial genome"),
          genome_site = str_replace(genome_site, "partial genome", "Partial genome"),
          genome_site = str_replace(genome_site, "7 concatenated protein-coding sequences", "Partial genome"),
          genome_site = str_replace(genome_site, "partial sequences", "Partial sequences"),
          genome_site = str_replace(genome_site, "\\(GP\\)", ""),
          genome_site = str_replace(genome_site, "(?i)glycoprotein", "Glycoprotein"),
          genome_site = str_replace(genome_site, "GP", "Glycoprotein"),
            # The viral RNA genome is encapsidated by the nucleoprotein (NP)
          genome_site = str_replace(genome_site, "\\bNP\\b", "Nucleoprotein"),
            # The L protein of Ebola virus (EBOV) is the catalytic subunit of the RNA‑dependent RNA polymerase complex
          genome_site = str_replace(genome_site, "polymerase", "Polymerase"),
            # Now that unit cleaning of GFP entries are complete can remove indel and SNP from site names
          genome_site = str_replace(genome_site, "GFP-indel", "GFP (recombinant EBOV)"),
          genome_site = str_replace(genome_site, "GFP-SNP", "GFP (recombinant EBOV)"),
          
          # Parameter from figure
          parameter_from_figure =
            case_when(
              parameter_class %in% "Reproduction number" & covidence_id %in% 4787 ~ TRUE,
              TRUE ~ parameter_from_figure
          ),
          
          # Inverse parameters
          inverse_param =
            case_when(
              parameter_class %in% "Human delay" & covidence_id %in% 17715 ~ FALSE,
              parameter_class %in% "Human delay" & covidence_id %in% 5935 ~ TRUE,
              TRUE ~ inverse_param
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
