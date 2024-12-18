#task to create lassa latex tables

library(dplyr) 
library(stringr)
library(tibble)
library(purrr)
library(readr)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("zika_compilation", "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "outbreaks.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("zika_functions.R" = "zika_functions.R")
source("zika_functions.R")
orderly_artefact(description = "zika-specific tables", files = c("latex_outbreaks.csv",
                                           "latex_models.csv",
                                           "latex_transmission.csv","latex_delays.csv",
                                           "latex_severity.csv","latex_seroprevalence.csv",
                                           "latex_riskfactors.csv"))

###################
## DATA CURATION ##
###################

articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("parameters.csv") %>%
  ########################### to remove once this is sorted
  filter(!is.na(parameter_data_id))

dfs <- curation(articles,outbreaks,models,parameters, plotting = FALSE)

articles   <- dfs$articles
outbreaks  <- dfs$outbreaks
models     <- dfs$models
parameters <- dfs$parameters

###############
## OUTBREAKS ## ----
###############

outbreaks <- outbreaks %>%
  mutate(dates = paste(outbreak_start_day,outbreak_start_month,outbreak_start_year,"-",
                       outbreak_end_day,outbreak_end_month,outbreak_date_year),
         dates = gsub("NA","",dates),
         dates = trimws(gsub("\\s{2,}"," ",dates)),
         dates = gsub("\\b(\\d{4})\\s*-\\s*\\1\\b","\\1",dates,perl = TRUE)) %>%
  mutate(outbreak_start_day   = coalesce(outbreak_start_day,1),
         outbreak_start_month = ifelse(is.na(outbreak_start_month),"Jan",outbreak_start_month),
         outbreak_start_year  = coalesce(outbreak_start_year,0),
         sdate = as.Date(paste(outbreak_start_day,outbreak_start_month,
                               outbreak_start_year, sep = "-"), format = "%d-%b-%Y")) %>%
  mutate(outbreak_end_day   = coalesce(outbreak_end_day,28),
         outbreak_end_month = ifelse(is.na(outbreak_end_month),"Dec",outbreak_end_month),
         outbreak_date_year = coalesce(outbreak_date_year,2024),
         edate = as.Date(paste(outbreak_end_day,outbreak_end_month,
                               outbreak_date_year, sep = "-"), format = "%d-%b-%Y"))

outbreaks <- outbreaks %>% mutate_all(~ ifelse(is.na(.), "", .))
outbreaks <- outbreaks %>% mutate(outbreak_location = gsub("Fct;","FCT;",outbreak_location),
                                  outbreak_location = gsub("Kenema ;","Kenema;",outbreak_location),
                                  outbreak_location = gsub("OuéMé","Ouémé",outbreak_location))

outs <- outbreaks %>%
  select(outbreak_country, outbreak_location, dates, 
         cases_suspected, cases_confirmed, cases_mode_detection, cases_severe, deaths,			
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
mods$stoch_deter <- gsub("Deterministic;Stochastic", "Stochastic",mods$stoch_deter)
mods$model_type  <- paste(mods$model_type, mods$stoch_deter, sep = " - ")
mods$stoch_deter <- NULL
mods$model_type  <- gsub("Branching process - Stochastic", "Branching Process", mods$model_type)
mods$transmission_route <- gsub("Vector/Animal to human", "Mosquito-Human", mods$transmission_route)
mods$transmission_route <- gsub("Human to human \\(direct contact\\)", "Human-Human", mods$transmission_route)
mods$transmission_route <- gsub("Airborne or close contact", "Airborne", mods$transmission_route)
mods$transmission_route <- gsub("Sexual", "Human-Human (Sexual)", mods$transmission_route)
mods$transmission_route <- gsub("Unspecified;Mosquito-Human","Mosquito-Human;Unspecified",mods$transmission_route)
mods$transmission_route <- ifelse(is.na(mods$transmission_route), 'Missing', mods$transmission_route)
mods$assumptions        <- gsub("Homogeneous mixing", "", mods$assumptions)
mods$assumptions        <- gsub("Latent period is same as incubation period", "", mods$assumptions)
mods$assumptions        <- gsub("Heterogenity in transmission rates - over time", "Time", mods$assumptions)
mods$assumptions        <- gsub("Heterogenity in transmission rates - between groups", "Groups", mods$assumptions)
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
# mods                    <- mods %>% mutate(assumptions = case_when(
#   covidence_id %in% c(285,2617,2620,5511) ~ gsub("Groups","Spatial",assumptions),
#   covidence_id %in% c(4120,4371) ~ gsub("Groups","Community Hygiene",assumptions),
#   covidence_id %in% c(4136,4251) ~ gsub("Groups","Socio-Economic Status",assumptions),
#   covidence_id %in% c(4343) ~ gsub("Groups","Protective Behaviour",assumptions),
#   covidence_id %in% c(3735) ~ gsub("Groups","Quarantine Status",assumptions),
#   covidence_id %in% c(5513) ~ gsub("Groups","Sex",assumptions),
#   TRUE ~ assumptions))
mods <- mods %>% select(-c("covidence_id"))
mods$transmission_route <- factor(mods$transmission_route,
                                  levels = c("Mosquito-Human","Human-Human","Human-Human (Sexual)","Human-Human (Sexual);Mosquito-Human",
                                             "Human-Human;Human-Human (Sexual);Mosquito-Human", "Human-Human;Mosquito-Human",
                                             "Rodent-Human;Human-Human;Airborne","Mosquito-Human;Unspecified", "Unspecified", 'Missing'))
mods$compartmental_type <- factor(mods$compartmental_type,
                                  levels = c("","SIR","SEIR","Other","Other;SIR"))
mods <- mods[order(mods$model_type,mods$transmission_route,mods$assumptions,
                   mods$compartmental_type),]
mods$transmission_route <- as.character(mods$transmission_route)
mods$compartmental_type <- as.character(mods$compartmental_type)
mods <- insert_blank_rows(mods,"model_type")
write.table(mods, file = "latex_models.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters ----
parameters <- mutate_at(parameters, 
                        vars(parameter_value, parameter_lower_bound, parameter_upper_bound, 
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value, 
                             parameter_uncertainty_single_value, distribution_par1_value, distribution_par2_value,
                             distribution_par1_uncertainty, distribution_par2_uncertainty, 
                             
                             parameter_value, parameter_2_lower_bound, parameter_2_upper_bound,
                             parameter_2_uncertainty_lower_value, parameter_2_uncertainty_upper_value,
                             parameter_2_uncertainty_single_value, distribution_2_par1_value, distribution_2_par2_value,
                             distribution_2_par1_uncertainty, distribution_2_par2_uncertainty),
                        ~sub("\\.?0+$", "", sprintf("%.10f", round(., 10))))
##
parameters <- parameters %>%
  mutate(parameter_value=
           ifelse(parameter_value!="NA", 
                  parameter_value,
                  ifelse(parameter_lower_bound!="NA" & parameter_upper_bound!="NA",
                         paste(parameter_lower_bound, parameter_upper_bound, sep = " - "), "")))
#
parameters$parameter_unit <- gsub("Substitutions/site/year", "s/s/y", parameters$parameter_unit)
parameters$parameter_unit <- gsub("No units", "", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Per day", "per day", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Percentage \\(%\\)", "\\\\%", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Days", "days", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Weeks", "weeks", parameters$parameter_unit)
parameters$parameter_unit <- gsub("per 100;000 population", "per 100k")

parameters <- parameters %>% mutate(parameter_unit = case_when(
  exponent == 0 ~ parameter_unit,
  exponent == -2 & parameter_unit == "" & parameter_class != "Reproduction number" ~ "\\%",
  exponent == -3 & parameter_unit == "" & parameter_class != "Reproduction number" ~ "per 1000",
  exponent == -4 & parameter_unit == "" & parameter_class != "Reproduction number" ~ "per 10k",
  exponent == -5 & parameter_unit == "" & parameter_class != "Reproduction number" ~ "per 100k",
  TRUE ~ paste(parameter_unit, sprintf("$10^{%d}$",exponent), sep=" ")))
# new bit
parameters <- parameters %>% mutate(parameter_value = case_when(
  (parameter_class %in% c("Severity","Seroprevalence") | parameter_unit == "") ~ parameter_value,
  TRUE ~ paste(parameter_value, parameter_unit, sep = " ")))
parameters <- parameters %>%
              mutate(parameter_unit = ifelse(parameter_unit == "Percentage (%)", "%", parameter_unit),
                     values = paste0(values, parameter_unit, sep = " "))
parameters <- parameters %>%
  mutate(unc_type=
           ifelse(!is.na(distribution_par2_type), 
                  paste(distribution_type, distribution_par2_type, sep = " "), 
                  ifelse(!is.na(parameter_uncertainty_type),
                         paste(parameter_uncertainty_type),    
                         ifelse(!is.na(parameter_uncertainty_singe_type),
                                paste(parameter_uncertainty_singe_type),""))))
parameters$unc_type <- gsub("Inter Quartile Range \\(IQR\\)", "IQR", parameters$unc_type)
parameters$unc_type <- gsub("Standard Error", "SE", parameters$unc_type)
parameters$unc_type <- gsub("Gamma Standard deviation", "Gamma SD", parameters$unc_type)
parameters$unc_type <- gsub("Standard Deviation", "SD", parameters$unc_type)
parameters$unc_type <- gsub("Highest Posterior Density Interval 95%", "CrI95%", parameters$unc_type)
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
##                     
#parameters$cfr_ifr_denominator[is.na(parameters$cfr_ifr_denominator)] <- 
#                                     parameters$population_sample_size[is.na(parameters$cfr_ifr_denominator)]        
#parameters$population_country <- gsub(",", "", parameters$population_country)
#new as well
parameters <- parameters %>%
  mutate(population_study_start_month = substr(population_study_start_month, 1, 3),
         population_study_end_month   = substr(population_study_end_month, 1, 3)) %>%
  mutate(dates = paste(population_study_start_day,population_study_start_month,population_study_start_year,"-",
                       population_study_end_day,population_study_end_month,population_study_end_year),
         dates = gsub("NA","",dates),
         dates = trimws(gsub("\\s{2,}"," ",dates)),
         dates = gsub("\\b(\\d{4})\\s*-\\s*\\1\\b","\\1",dates,perl = TRUE),
         dates = ifelse(dates == "-","",dates)) %>%
  mutate(population_study_start_day   = coalesce(population_study_start_day,1),
         population_study_start_month = ifelse(is.na(population_study_start_month),"Jan",population_study_start_month),
         population_study_start_year  = coalesce(population_study_start_year,0),
         sdate = as.Date(paste(population_study_start_day,population_study_start_month,
                               population_study_start_year, sep = "-"), format = "%d-%b-%Y")) %>%
  mutate(population_study_end_day   = coalesce(population_study_end_day,28),
         population_study_end_month = ifelse(is.na(population_study_end_month),"Dec",population_study_end_month),
         population_study_end_year = coalesce(population_study_end_year,2024),
         edate = as.Date(paste(population_study_end_day,population_study_end_month,
                               population_study_end_year, sep = "-"), format = "%d-%b-%Y"))
#
parameters$population_sample_type <- gsub("\\s.*", "", parameters$population_sample_type)
parameters$population_group <- str_to_title(parameters$population_group)
parameters$method_disaggregated_by <- gsub("Disease generation","Disease Generation",parameters$method_disaggregated_by)
parameters$method_disaggregated_by <- gsub("Level of exposure","Level of Exposure",parameters$method_disaggregated_by)
parameters <- parameters %>% mutate_all(~ ifelse(is.na(.), "", .))

parameters <- parameters %>% mutate(method_disaggregated_by = gsub(", ", ";", method_disaggregated_by),
                                    population_country = gsub(", ", ";", population_country))

#parameters - transmission
trns_params <- parameters %>%
  filter(grepl("Mutations|Attack|Relative contribution|Growth rate|Reproduction", parameter_type, ignore.case = TRUE)) %>%
  select(parameter_type, parameter_value, unc_type,
         method_disaggregated_by, 
         genome_site, method_r,
         population_sample_size,
         population_country, dates,
         population_sample_type, population_group, refs, central)
trns_params$parameter_type  <- gsub("Relative contribution - human to human", "Human-Human Transmission Contribution", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("Relative contribution - zoonotic to human", "Human-Vector Transmission Contribution", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("Mutations – ", "", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("Mutations - ", "", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("Mutations â€“ ", "", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("Attack rate (inverse parameter)", "Attack rate", trns_params$parameter_type)
# trns_params$parameter_type  <- gsub("\\(.*?\\)", "", trns_params$parameter_type)
trns_params$parameter_type  <- str_to_title(trns_params$parameter_type)
# trns_params$parameter_type <- factor(trns_params$parameter_type, 
#                                      levels = c("Reproduction Number ","Growth Rate ","Attack Rate",
#                                                 "Human-Human Transmission Contribution","Evolutionary Rate","Substitution Rate"))
trns_params$method_r <- str_to_title(trns_params$method_r)
trns_params <- trns_params[order(trns_params$parameter_type,trns_params$genome_site,as.numeric(trns_params$central)),]
trns_params <- trns_params %>% select(-central)
trns_params <- insert_blank_rows(trns_params,"parameter_type")
write.table(trns_params, file = "latex_transmission.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters - human delays
hdel_params <- parameters %>%
  filter(grepl("Human delay", parameter_type, ignore.case = TRUE)) %>%
  select(parameter_type, other_delay_start, other_delay_end,
         parameter_value, parameter_value_type, unc_type,
         method_disaggregated_by, 
         population_sample_size,
         population_country, dates,
         population_sample_type, population_group, refs, central)
hdel_params$parameter_type <- sub("^.* - ", "", hdel_params$parameter_type)
hdel_params$parameter_type <- sub(">", " - ", hdel_params$parameter_type)
hdel_params$parameter_type <- str_to_title(hdel_params$parameter_type)
hdel_params$parameter_type <- gsub("  \\(.*?\\)", "", hdel_params$parameter_type)
hdel_params <- hdel_params %>% mutate(parameter_type = case_when(
  (other_delay_start =="Symptom Onset/Fever" & other_delay_end == "Symptom Resolution") ~ "Symptomatic Period",   
  (other_delay_start =="Symptom Onset/Fever" & other_delay_end == "Specimen Collection") ~ "Onset - Testing",   
  (other_delay_start =="Specimen Collection" & other_delay_end == "Test Result") ~ "Testing - Test Result",   
  (other_delay_start =="Admission to Care/Hospitalisation" & other_delay_end == "Symptom Resolution") ~ "Admission - Symptom Resolution",   
  (other_delay_start =="Symptom Onset/Fever" & other_delay_end == "Other: Sample collection/Diagnosis") ~ "Onset - Testing",   
  (other_delay_start =="Symptom Onset/Fever" & other_delay_end == "Other: Start of ribavirin treatment") ~ "Onset - Start of Treatment",   
  (other_delay_start =="Other: Start of ribavirin treatment" & other_delay_end == "Other: End of ribavirin treatment") ~ "Duration of Antiviral Treatment",   
  (other_delay_start =="Other: Start of oxygen therapy" & other_delay_end == "Other: End of oxygen therapy") ~ "Duration of Oxygen Therapy",   
  (other_delay_start =="Other: Start of antibacterial therapy" & other_delay_end == "Other: End of antibacterial therapy") ~ "Duration of Antibacterial Therapy",   
  TRUE ~ parameter_type))
hdel_params <- hdel_params %>%
  mutate(other_delay_start = ifelse(other_delay_start=="Other: type timepoint in this text box", NA, other_delay_start),
         other_delay_end = ifelse(other_delay_end=="Other: type timepoint in this text box", NA, other_delay_end))
hdel_params <- hdel_params %>% select(-c("other_delay_start","other_delay_end"))
hdel_params$parameter_type <- sub("Symptom Onset", "Onset", hdel_params$parameter_type)
hdel_params$parameter_type <- sub("Admission To Care", "Admission", hdel_params$parameter_type)
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

#parameters - CFRs
cfrs_params <- parameters %>%
  filter(grepl("Severity - case fatality rate", parameter_type, ignore.case = TRUE)) %>%
  select(parameter_value, unc_type,
         method_disaggregated_by, 
         cfr_ifr_method, cfr_ifr_numerator,cfr_ifr_denominator,
         population_country, dates,
         population_sample_type, population_group, refs, central)
cfrs_params$population_country <- gsub(";", "\\, ", cfrs_params$population_country)
cfrs_params <- cfrs_params %>% arrange(tolower(population_country),as.numeric(central))
cfrs_params <- cfrs_params %>% select(-central)
cfrs_params <- insert_blank_rows(cfrs_params,"population_country")
write.table(cfrs_params, file = "latex_severity.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters - seroprevalence
sero_params <- parameters %>%
  filter(grepl("Seroprevalence", parameter_type, ignore.case = TRUE)) %>%
  select(parameter_value, unc_type,
         method_disaggregated_by, 
         parameter_type,cfr_ifr_numerator,cfr_ifr_denominator,
         population_country, dates,
         population_sample_type, population_group, refs, central)
sero_params$parameter_type <- sub("^.* - ", "", sero_params$parameter_type)
sero_params$population_country <- gsub(";", "\\, ", sero_params$population_country)
sero_params$population_country[sero_params$population_country==""] <- "Unspecified" 
sero_params <- sero_params %>% arrange(tolower(population_country), parameter_type, as.numeric(central))
sero_params <- sero_params %>% select(-central)
sero_params <- insert_blank_rows(sero_params,"population_country")
write.table(sero_params, file = "latex_seroprevalence.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters - risk factors
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
