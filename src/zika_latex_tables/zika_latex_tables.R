#task to create lassa latex tables

library(dplyr) 
library(stringr)
library(tibble)
library(purrr)
library(readr)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
# orderly_dependency("zika_compilation", "latest(parameter:pathogen == this:pathogen)",
#                    c("articles.rds", "outbreaks.rds", "models.rds", "parameters.rds"))
orderly_dependency("zika_prep_data", "latest(parameter:pathogen == this:pathogen &&
                   parameter:plotting == FALSE)",
                   c("articles_curated.rds", "outbreaks_curated.rds", "models_curated.rds", "parameters_curated.rds"))
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
orderly_artefact(description = "zika-specific tables", files = c("latex_outbreaks.csv",
                                           "latex_models.csv",
                                           "latex_transmission.csv","latex_delays.csv",
                                           "latex_severity.csv","latex_seroprevalence.csv",
                                           "latex_riskfactors.csv",
                                           "latex_miscarriage_zcs.csv",
                                           'latex_relative_contribution.csv',
                                           'latex_genomic.csv'))

###################
## DATA CURATION ##
###################

articles   <- readRDS("articles_curated.rds")
outbreaks  <- readRDS("outbreaks_curated.rds")
models     <- readRDS("models_curated.rds")
parameters <- readRDS("parameters_curated.rds")  %>%
  ########################### to remove once this is sorted
  filter(!is.na(parameter_data_id))

# dfs <- curation(articles,outbreaks,models,parameters, plotting = FALSE)
# 
# articles   <- dfs$articles
# outbreaks  <- dfs$outbreaks
# models     <- dfs$models
# parameters <- dfs$parameters

###############
## OUTBREAKS ## ----
###############

outbreaks <- outbreaks %>%
  mutate(outbreak_location = str_replace_all(outbreak_location,' \\(Up\\)',''),
         outbreak_location = str_replace_all(outbreak_location,',', ";")) %>%
  mutate(
    # Handle start date text
    sdate = case_when(
      !is.na(outbreak_start_day) & !is.na(outbreak_start_month) & !is.na(outbreak_start_year) ~ 
        paste0(sprintf("%02d", outbreak_start_day), " ", outbreak_start_month, " ", outbreak_start_year),
      is.na(outbreak_start_day) & !is.na(outbreak_start_month) & !is.na(outbreak_start_year) ~ 
        paste0(outbreak_start_month, " ", outbreak_start_year),
      is.na(outbreak_start_day) & is.na(outbreak_start_month) & !is.na(outbreak_start_year) ~ 
        as.character(outbreak_start_year),
      TRUE ~ NA_character_
    ),
    
    # Handle end date text
    edate = case_when(
      !is.na(outbreak_end_day) & !is.na(outbreak_end_month) & !is.na(outbreak_date_year) ~ 
        paste0(sprintf("%02d", outbreak_end_day), " ", outbreak_end_month, " ", outbreak_date_year),
      is.na(outbreak_end_day) & !is.na(outbreak_end_month) & !is.na(outbreak_date_year) ~ 
        paste0(outbreak_end_month, " ", outbreak_date_year),
      is.na(outbreak_end_day) & is.na(outbreak_end_month) & !is.na(outbreak_date_year) ~ 
        as.character(outbreak_date_year),
      TRUE ~ NA_character_
    ),
    
    # Build final dates string
    dates = case_when(
      !is.na(sdate) & !is.na(edate) ~ paste0(sdate, " - ", edate),
      !is.na(sdate) & is.na(edate) ~ sdate,
      is.na(sdate) & !is.na(edate) ~ edate,
      !is.na(sdate) & is.na(edate) ~ "Unspecified",
      TRUE ~ NA_character_
    )
  ) 

outbreaks <- outbreaks %>% 
  mutate(nocases = ifelse(is.na(cases_confirmed) & is.na(cases_suspected) & is.na(cases_asymptomatic) *
                            is.na(cases_severe) & is.na(cases_unspecified) & is.na(female_cases) & is.na(male_cases), 1, 0),
         outbreak_location = ifelse(nocases == 1, paste0(outbreak_location, "*"), outbreak_location)) %>%
  mutate_all(~ ifelse(is.na(.), "", .))

outs <- outbreaks %>%
  select(outbreak_country, outbreak_location, dates, 
         cases_suspected, cases_confirmed, cases_mode_detection,	
         female_cases, male_cases,
         refs,sdate,edate)
outs$cases_mode_detection <- gsub(" \\(PCR etc\\)", "", outs$cases_mode_detection)
outs <- outs %>% arrange(tolower(outbreak_country),desc(sdate),desc(edate))
outs$sdate <- NULL
outs$edate <- NULL
outs <- insert_blank_rows(outs,"outbreak_country")
write.table(outs, file = "latex_outbreaks.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#models ----

mods <- models %>%
  select(model_type,stoch_deter,transmission_route,assumptions,
         compartmental_type,theoretical_model,interventions_type,refs,covidence_id)
mods$model_type <- gsub("Agent / Individual based", 'Individual based', mods$model_type)
mods$stoch_deter <- gsub("Deterministic;Stochastic", "Stochastic",mods$stoch_deter)
mods$model_type  <- paste(mods$model_type, mods$stoch_deter, sep = " - ")
mods$model_type <- gsub("NA - Unspecified", "Unspecified", mods$model_type)
mods$stoch_deter <- NULL
mods$model_type  <- gsub("Branching process - Stochastic", "Branching Process", mods$model_type)
mods$transmission_route <- gsub("Vector/Animal to human", "Mosquito-Human", mods$transmission_route)
mods$transmission_route <- gsub("Human to human \\(direct contact\\)", "Human-Human", mods$transmission_route)
mods$transmission_route <- gsub("Airborne or close contact", "Airborne", mods$transmission_route)
mods$transmission_route <- gsub("Human-Human \\(Sexual\\)","Sexual", mods$transmission_route)
mods$transmission_route <- gsub("Unspecified;Mosquito-Human","Mosquito-Human;Unspecified",mods$transmission_route)
mods$transmission_route <- ifelse(is.na(mods$transmission_route), 'Unspecified', mods$transmission_route)
mods$assumptions        <- gsub("Latent period is same as incubation period", "Latent and incubation periods are same", mods$assumptions)
mods$assumptions        <- gsub("Heterogenity in transmission rates - over time", "Heterogenity in transmission over time", mods$assumptions)
mods$assumptions        <- gsub("Heterogenity in transmission rates - between human groups", "Heterogenity in transmission between human groups", mods$assumptions)
mods$assumptions        <- gsub("Heterogenity in transmission rates - between human and vector", "Heterogenity in transmission between human and vector", mods$assumptions)
mods$assumptions        <- gsub("Cross-immunity between Zika and dengue", "Cross-immunity", mods$assumptions)
mods$assumptions        <- gsub("Age dependent susceptibility", "Age", mods$assumptions)
mods$assumptions        <- gsub("^;|;$", "", mods$assumptions)
mods$compartmental_type <- gsub("Not compartmental", "", mods$compartmental_type)
mods$compartmental_type <- gsub("Other compartmental", "Other", mods$compartmental_type)
mods$theoretical_model  <- gsub("FALSE", "Fitted", mods$theoretical_model)
mods$theoretical_model  <- gsub("TRUE", "Theoretical", mods$theoretical_model)
mods$interventions_type <- gsub("Vector/Animal control", "Mosquito Control", mods$interventions_type)
mods$interventions_type <- gsub("changes", "Changes", mods$interventions_type)
mods$interventions_type <- gsub("tracing", "Tracing", mods$interventions_type)
mods$interventions_type <- gsub("Unspecified", "", mods$interventions_type)
mods <- mods %>% select(-c("covidence_id"))
mods$transmission_route <- factor(mods$transmission_route,
                                  levels = c("Mosquito-Human","Human-Human","Sexual","Sexual;Mosquito-Human",
                                             "Human-Human;Sexual;Mosquito-Human", "Human-Human;Mosquito-Human",
                                             "Mosquito-Human;Unspecified", "Unspecified", 'Missing'))
mods$compartmental_type <- factor(mods$compartmental_type,
                                  levels = c("","SEIR-SEI","SIER-SI","SEI-SI","SEIR;SIR","SI-SI","SIR-SEI","SIR-SI","SIRS-SI", 'SLIR-SLI',
                                             "SIR","SEIR","Other SEIR-SEI","SAIR-SEI","Not compartmental","Not compartmental;SEIR-SEI",
                                             "Other","Other compartmental;SIR","Other compartmental;SEIR:SEI"))
mods <- mods[order(mods$model_type,mods$compartmental_type,mods$transmission_route,mods$assumptions),]
mods$transmission_route <- as.character(mods$transmission_route)
mods$compartmental_type <- as.character(mods$compartmental_type)
mods <- insert_blank_rows(mods,"model_type")
write.table(mods, file = "latex_models.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)


# Prep par data for tables ----
# make variable with location and country to use or not
parameters <- parameters %>%
  mutate(label_group = case_when(
    is.na(population_location) | population_location == '' ~ population_country,
    (population_location !="" & !is.na(population_location)) & population_country != 'Unspecified' ~ paste0(population_location,' (',population_country,')'),
    (!is.na(population_location) & population_country == 'Unspecified') | (population_country == 'Brazil') ~ population_location,
    TRUE ~ NA))

parameters <- mutate_at(parameters, 
                        vars(parameter_value, parameter_lower_bound, parameter_upper_bound, 
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value, 
                             parameter_uncertainty_single_value, distribution_par1_value, distribution_par2_value,
                             # distribution_par1_uncertainty, distribution_par2_uncertainty, 
                             
                             parameter_value, parameter_2_lower_bound, parameter_2_upper_bound,
                             parameter_2_uncertainty_lower_value, parameter_2_uncertainty_upper_value,
                             parameter_2_uncertainty_single_value, distribution_2_par1_value, distribution_2_par2_value
                             # distribution_2_par1_uncertainty, distribution_2_par2_uncertainty
                             ),
                        ~sub("\\.?0+$", "", sprintf("%.10f", round(., 10))))
##
parameters <- parameters %>%
  mutate(parameter_value=
           ifelse(parameter_value!="NA" & parameter_value != ' NA', 
                  parameter_value,
                  ifelse((parameter_lower_bound!="NA" & parameter_value != ' NA') & 
                           parameter_upper_bound!="NA",
                         paste(parameter_lower_bound, parameter_upper_bound, sep = " - "), "")))
#
parameters$parameter_unit <- gsub("Substitutions/site/year", "s/s/y", parameters$parameter_unit)
parameters$parameter_unit <- gsub("No units", "", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Per", "per", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Percentage \\(%\\)", "\\\\%", parameters$parameter_unit)
parameters$parameter_unit <- gsub("percentage \\(%\\)", "\\\\%", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Days", "days", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Weeks", "weeks", parameters$parameter_unit)
parameters$parameter_unit <- gsub("per 100;000 population", "per 100k", parameters$parameter_unit)
parameters$parameter_unit <- gsub("per 100,000 population", "per 100k", parameters$parameter_unit)
parameters$parameter_unit <- gsub("per 10,000 population", "per 10k", parameters$parameter_unit)
parameters$parameter_unit <- gsub("per 1,000 births", "per 1k births", parameters$parameter_unit)


parameters <- parameters %>% mutate(parameter_unit = case_when(
  exponent == 0 ~ parameter_unit,
  exponent == -2 & parameter_unit == "" & parameter_class != "Reproduction number" ~ "\\%",
  exponent == -3 & parameter_unit == "" & parameter_class != "Reproduction number" ~ "per 1000",
  exponent == -4 & parameter_unit == "" & parameter_class != "Reproduction number" ~ "per 10k",
  exponent == -5 & parameter_unit == "" & parameter_class != "Reproduction number" ~ "per 100k",
  parameter_unit == 'Unspecified' & parameter_type %in% c("Miscarriage rate", "Zika congenital syndrome (microcephaly) risk") ~ '',
  TRUE ~ paste(parameter_unit, sprintf("$10^{%d}$",exponent), sep=" ")))
# new bit
parameters <- parameters %>% 
  mutate(parameter_value = case_when(
    ( parameter_unit == "" | is.na(parameter_unit)) ~ parameter_value,#parameter_class %in% c("Severity","Seroprevalence") |
    TRUE ~ paste(parameter_value, parameter_unit, sep = " ")))
parameters <- parameters %>%
  mutate(unc_type=
           ifelse(!is.na(distribution_par2_type), 
                  paste(distribution_type, distribution_par2_type, sep = " "), 
                  ifelse(!is.na(parameter_uncertainty_type),
                         paste(parameter_uncertainty_type),    
                         ifelse(!is.na(parameter_uncertainty_singe_type),
                                paste(parameter_uncertainty_singe_type),
                                ifelse(!is.na(parameter_2_value_type),
                                       paste(parameter_2_value_type),"")))))
parameters$unc_type <- gsub(" \\(paired\\)", '', parameters$unc_type)
parameters$unc_type <- gsub(" \\(paired or unpaired\\)", '', parameters$unc_type)
parameters$unc_type <- gsub("Inter Quartile Range \\(IQR\\)", "IQR", parameters$unc_type)
parameters$unc_type <- gsub("Standard Error", "SE", parameters$unc_type)
parameters$unc_type <- gsub("Gamma Standard deviation", "Gamma SD", parameters$unc_type)
parameters$unc_type <- gsub("Standard Deviation", "SD", parameters$unc_type)
parameters$unc_type <- gsub("Highest Posterior Density Interval 95%", "HPDI95%", parameters$unc_type)
parameters$unc_type <- gsub("CRI95%", "CrI95%", parameters$unc_type)
parameters$unc_type <- gsub("%", "\\\\%", parameters$unc_type)
#
parameters <- parameters %>%
  mutate(uncertainty=
           ifelse(parameter_uncertainty_single_value!="NA",
                  paste(parameter_uncertainty_single_value),
                  ifelse(distribution_par2_value!="NA",
                         paste(distribution_par2_value),
                         ifelse(parameter_uncertainty_lower_value!="NA" & parameter_uncertainty_upper_value!="NA",
                                paste(parameter_uncertainty_lower_value, parameter_uncertainty_upper_value, sep = "-"), 
                                ""))))
# new bit
parameters <- parameters %>% mutate(unc_type = case_when(
  unc_type %in% c("Unspecified","") ~ "",
  TRUE ~  paste(unc_type, ": ", uncertainty, sep = "")))
#
parameters$cfr_ifr_denominator[is.na(parameters$cfr_ifr_denominator)] <-
                                    parameters$population_sample_size[is.na(parameters$cfr_ifr_denominator)]
#parameters$population_country <- gsub(",", "", parameters$population_country)
#new as well
parameters <- parameters %>%
  mutate(population_study_start_month = substr(population_study_start_month, 1, 3),
         population_study_end_month   = substr(population_study_end_month, 1, 3)) %>%
  mutate(
    # Handle start date text
    start_text = case_when(
      !is.na(population_study_start_day) & !is.na(population_study_start_month) & !is.na(population_study_start_year) ~ 
        paste0(sprintf("%02d", population_study_start_day), " ", population_study_start_month, " ", population_study_start_year),
      is.na(population_study_start_day) & !is.na(population_study_start_month) & !is.na(population_study_start_year) ~ 
        paste0(population_study_start_month, " ", population_study_start_year),
      is.na(population_study_start_day) & is.na(population_study_start_month) & !is.na(population_study_start_year) ~ 
        as.character(population_study_start_year),
      TRUE ~ NA_character_
    ),
    
    # Handle end date text
    end_text = case_when(
      !is.na(population_study_end_day) & !is.na(population_study_end_month) & !is.na(population_study_end_year) ~ 
        paste0(sprintf("%02d", population_study_end_day), " ", population_study_end_month, " ", population_study_end_year),
      is.na(population_study_end_day) & !is.na(population_study_end_month) & !is.na(population_study_end_year) ~ 
        paste0(population_study_end_month, " ", population_study_end_year),
      is.na(population_study_end_day) & is.na(population_study_end_month) & !is.na(population_study_end_year) ~ 
        as.character(population_study_end_year),
      TRUE ~ NA_character_
    ),
    
    # Build final dates string
    dates = case_when(
      !is.na(start_text) & !is.na(end_text) ~ paste0(start_text, " - ", end_text),
      !is.na(start_text) & is.na(end_text) ~ start_text,
      is.na(start_text) & !is.na(end_text) ~ end_text,
      survey_start_date == 'Unspecified' & survey_end_date == 'Unspecified' ~ "Unspecified",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-start_text, -end_text)
#
parameters$population_sample_type <- gsub("\\s.*", "", parameters$population_sample_type)
parameters$population_group <- str_to_title(parameters$population_group)
parameters$method_disaggregated_by <- gsub("Disease generation","Disease Generation",parameters$method_disaggregated_by)
parameters$method_disaggregated_by <- gsub("Level of exposure","Level of Exposure",parameters$method_disaggregated_by)
parameters <- parameters %>% mutate_all(~ ifelse(is.na(.), "", .))

parameters <- parameters %>% mutate(method_disaggregated_by = gsub(", ", ";", method_disaggregated_by),
                                    population_country = gsub(", ", ";", population_country))


#parameters - transmission ----
trns_params <- parameters %>%
  filter(grepl("Attack|Relative contribution|Growth rate|Reproduction", parameter_type, ignore.case = TRUE)) %>%
  select(parameter_type, parameter_value, unc_type, #uncertainty,
         method_disaggregated_by, 
         method_r,
         population_sample_size,
         population_country, dates,
         population_sample_type, population_group, refs, central) %>%
  mutate(method_r = str_replace_all(method_r, ';',', '))
trns_params$parameter_type  <- gsub("Relative contribution - human to human", "Human-Human Transmission Contribution", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("Relative contribution - zoonotic to human", "Human-Vector Transmission Contribution", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("Attack rate (inverse parameter)", "Attack rate", trns_params$parameter_type)
trns_params$parameter_type <- gsub("Reproduction number \\(Basic R0\\)", "Reproduction Number", trns_params$parameter_type)
trns_params$parameter_type <- gsub("Reproduction number \\(Effective; Re\\)", "Effective Reproduction Number", trns_params$parameter_type)
# trns_params$parameter_type  <- gsub("\\(.*?\\)", "", trns_params$parameter_type)
trns_params$parameter_type  <- str_to_title(trns_params$parameter_type)
# trns_params$parameter_type <- factor(trns_params$parameter_type, 
#                                      levels = c("Reproduction Number ","Growth Rate ","Attack Rate",
#                                                 "Human-Human Transmission Contribution","Evolutionary Rate","Substitution Rate"))
trns_params$method_r <- str_to_title(trns_params$method_r)
trns_params <- trns_params[order(trns_params$parameter_type,as.numeric(trns_params$central)),]
trns_params <- trns_params %>% select(-central)
trns_params <- insert_blank_rows(trns_params,"parameter_type")
write.table(trns_params, file = "latex_transmission.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# parameters - genomic 
genomic_params <- parameters %>%
  filter(grepl('Mutations', parameter_type, ignore.case = TRUE)) %>%
  select(parameter_type, parameter_value, unc_type, #uncertainty,
         genome_site, 
         population_sample_size,
         dates,
         population_sample_type, population_group, refs, central)
genomic_params$parameter_type  <- gsub("Mutations – ", "", genomic_params$parameter_type)
genomic_params$parameter_type  <- gsub("Mutations - ", "", genomic_params$parameter_type)
genomic_params$parameter_type  <- gsub("Mutations â€“ ", "", genomic_params$parameter_type)
genomic_params$parameter_type  <- str_to_title(genomic_params$parameter_type)
genomic_params <- genomic_params[order(genomic_params$parameter_type,genomic_params$genome_site,as.numeric(genomic_params$central)),]
genomic_params <- genomic_params %>% select(-central)
genomic_params <- insert_blank_rows(genomic_params,"parameter_type")
write.table(genomic_params, file = "latex_genomic.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)


#parameters - human delays ----
var_select <- c("parameter_value", "parameter_lower_bound", "parameter_upper_bound", 
                "parameter_uncertainty_lower_value", "parameter_uncertainty_upper_value",
                "parameter_2_value", "parameter_2_lower_bound", "parameter_2_upper_bound", 
                "parameter_2_uncertainty_lower_value", "parameter_2_uncertainty_upper_value")
hdel_params <- parameters %>%
  filter(grepl("delay", parameter_type, ignore.case = TRUE)) %>%
  select(parameter_type, other_delay_start, other_delay_end,
         parameter_value, parameter_value_type, unc_type, #uncertainty,
         method_disaggregated_by, 
         population_sample_size,
         population_country, dates,
         population_sample_type, population_group, refs, central) 
hdel_params$parameter_type <- sub("^.* - ", "", hdel_params$parameter_type)
hdel_params$parameter_type <- sub(">", " - ", hdel_params$parameter_type)
hdel_params$parameter_type <- str_to_title(hdel_params$parameter_type)
hdel_params$parameter_type <- gsub("  \\(.*?\\)", "", hdel_params$parameter_type)
hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bIcu\\b", "ICU")
hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bNaat\\b", "NAAT")
hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bRna\\b", "RNA")
hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bIgm\\b", "IgM")
hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bIgg\\b", "IgG")
hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bPcr\\b", "PCR")
hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bZikv\\b", "ZIKV")
hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bEip10\\b", "EIP")
hdel_params <- hdel_params %>%
  mutate(other_delay_start = ifelse(other_delay_start=="Other: type timepoint in this text box", NA, other_delay_start),
         other_delay_end = ifelse(other_delay_end=="Other: type timepoint in this text box", NA, other_delay_end))
hdel_params <- hdel_params %>% select(-c("other_delay_start","other_delay_end"))
hdel_params$parameter_type <- sub("Symptom Onset", "Onset", hdel_params$parameter_type)
hdel_params$parameter_type <- sub("Admission To Care", "Admission", hdel_params$parameter_type)
hdel_params$parameter_type <- sub("Admission To care", "Admission", hdel_params$parameter_type)
hdel_params$parameter_type <- sub("Discharge/Recovery", "Recovery", hdel_params$parameter_type)
# hdel_params$parameter_type <- factor(hdel_params$parameter_type, 
#                                      levels = c("Incubation Period",
#                                                 "Onset - Testing","Testing - Test Result","Onset - Admission",
#                                                 "Onset - Start of Treatment","Duration of Antiviral Treatment","Duration of Antibacterial Therapy","Duration of Oxygen Therapy",
#                                                 "Admission - Symptom Resolution","Symptomatic Period",
#                                                 "Admission - Recovery","Admission - Death","Time In Care ",
#                                                 "Onset - Recovery","Onset - Death"))
hdel_params <- hdel_params[order(hdel_params$parameter_type,as.numeric(hdel_params$central)),]
hdel_params <- hdel_params %>% select(-central)
hdel_params <- insert_blank_rows(hdel_params,"parameter_type")
write.table(hdel_params, file = "latex_delays.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters - CFRs ----
cfrs_params <- parameters %>%
  filter(grepl("Severity", parameter_type, ignore.case = TRUE)) %>%
  mutate(parameter_type = ifelse(parameter_type == 'Severity - case fatality rate (CFR)', 'Case fatality ratio',
                                 ifelse(parameter_type == 'Severity - proportion of symptomatic cases', 
                                        "Proportion of symptomatic cases", parameter_type))) %>%
  select(parameter_type, parameter_value, unc_type, #uncertainty,
         method_disaggregated_by, 
         cfr_ifr_method, cfr_ifr_numerator,cfr_ifr_denominator,
         population_country, dates,
         population_sample_type, population_group, refs, central)
cfrs_params$population_country <- gsub(";", "\\, ", cfrs_params$population_country)
cfrs_params <- cfrs_params %>% arrange(tolower(population_country),as.numeric(central))
cfrs_params <- cfrs_params %>% select(-central)
cfrs_params <- insert_blank_rows(cfrs_params,"parameter_type")
write.table(cfrs_params, file = "latex_severity.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters - seroprevalence ----
sero_params1 <- parameters %>%
  filter(grepl("Seroprevalence", parameter_type, ignore.case = TRUE)) %>%
  select(parameter_type, parameter_value, unc_type, #uncertainty,
         method_disaggregated_by, 
         cfr_ifr_numerator,cfr_ifr_denominator,population_sample_size,
         prnt_on_elisa, population_country, dates,
         population_sample_type, population_group, refs, central, 
         covidence_id)
sero_params <- sero_params1 %>%
  mutate(parameter_type = case_when(
    parameter_type == 'Seroprevalence - Neutralisation/PRNT' ~ 'PRNT',
    parameter_type %in% c("Seroprevalence - Biotinylated-EDIII antigen capture ELISA") ~ 'Biotinylated-\nEDIII antigen\ncapture ELISA',
    TRUE ~ parameter_type))

sero_params$prnt_on_elisa <- ifelse(sero_params$prnt_on_elisa == 'F', '', 
                                    ifelse(sero_params$prnt_on_elisa == 'V', 'TRUE', sero_params$prnt_on_elisa))
sero_params$parameter_type <- sub("^.* - ", "", sero_params$parameter_type)
sero_params$population_country <- gsub(";", "\\, ", sero_params$population_country)
sero_params$population_country[sero_params$population_country==""] <- "Unspecified" 
sero_params <- sero_params %>% arrange(tolower(population_country), parameter_type, as.numeric(central))
sero_params <- sero_params %>% select(-central, -covidence_id)
sero_params <- insert_blank_rows(sero_params,"population_country")
write.table(sero_params, file = "latex_seroprevalence.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters - risk factors ----
risk_params <- parameters %>%
  filter(grepl("Risk factors", parameter_type, ignore.case = TRUE)) %>%
  select(riskfactor_outcome, 
         riskfactor_name,	riskfactor_significant, 
         riskfactor_adjusted, population_sample_size,
         population_country, dates,
         population_sample_type, population_group, refs)
risk_params$riskfactor_outcome <- factor(risk_params$riskfactor_outcome, 
                                         levels = c("Infection",#"Reproduction Number","Attack Rate","Occurrence",
                                                    "Serology",#"Death","Viremia","Onset-Admission Delay","Incidence",
                                                    "Guillain Barre syndrome", "Hospitalisation", "Low birthweight",
                                                    "Microcephaly", "Miscarriage/stillbirth", "Other", "Other neurological symptoms in general population",
                                                    "Premature birth", "Severe disease (in general population)", "Symptomatic infection",
                                                    "Zika congenital syndrome/other birth defects", "ZIKV detection"))
risk_params$riskfactor_significant <- str_to_title(risk_params$riskfactor_significant)
risk_params$riskfactor_significant <- factor(risk_params$riskfactor_significant, 
                                             levels = c("Significant","Not Significant","Unspecified"))

risk_params$riskfactor_adjusted <- str_to_title(risk_params$riskfactor_adjusted)
# risk_params <- risk_params %>% mutate(riskfactor_name = gsub("Contact with animal","Contact with Animal",riskfactor_name),
#                                       riskfactor_name = gsub("Household contact","Household Contact",riskfactor_name),
#                                       riskfactor_name = gsub("Close contact","Close Contact",riskfactor_name),
#                                       riskfactor_name = gsub("Non-household contact","Non-household Contact",riskfactor_name))

risk_params <- risk_params[order(risk_params$riskfactor_outcome,risk_params$riskfactor_significant,
                                 risk_params$riskfactor_name,risk_params$riskfactor_adjusted),]
risk_params$riskfactor_significant <- as.character(risk_params$riskfactor_significant)
risk_params <- insert_blank_rows(risk_params,"riskfactor_outcome")
write.table(risk_params, file = "latex_riskfactors.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)


# parameters -- miscarriage rate/microcephaly risk ----
infants <- parameters %>% 
  filter(parameter_type %in% c("Miscarriage rate", "Zika congenital syndrome (microcephaly) risk")) %>%
  select(parameter_type, parameter_value, unc_type, #uncertainty,
         cfr_ifr_method, cfr_ifr_numerator,cfr_ifr_denominator,
         population_country, dates,
         population_sample_type, refs, central) %>%
  mutate(cfr_ifr_method = ifelse(cfr_ifr_method == '','Unspecified', cfr_ifr_method))
infants <- infants %>% arrange(parameter_type, tolower(population_country),as.numeric(central))
infants <- infants %>% select(-central)
infants <- insert_blank_rows(infants,"parameter_type")
write.table(infants, file = "latex_miscarriage_zcs.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# parameters -- relative contributions ----
relative <- parameters %>%
  filter(grepl("Relative contribution", parameter_type)) %>%
  select(parameter_type, parameter_value, unc_type, #uncertainty,
         population_country, dates,
         population_sample_type, population_group, refs, central)
relative <- relative %>% arrange(tolower(population_country),as.numeric(central))
relative <- relative %>% select(-central)
relative <- insert_blank_rows(relative,"parameter_type")
write.table(relative, file = "latex_relative_contribution.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)