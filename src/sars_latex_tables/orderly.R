#task to create sars latex tables

library(dplyr) 
library(stringr)
library(tibble)
library(purrr)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("db_compilation", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
orderly_artefact("sars-specific tables",c("latex_models.csv",
                                          "latex_transmission.csv","latex_delays.csv",
                                          "latex_severity.csv","latex_seroprevalence.csv",
                                          "latex_riskfactors.csv"))

###################
## DATA CURATION ##
###################

articles   <- read_csv("articles.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("parameters.csv")

dfs <- curation(articles,tibble(),models,parameters, adjust_for_exponents = FALSE )

articles   <- dfs$articles
models     <- dfs$models
parameters <- dfs$parameters

#models
mods <- models %>%
  filter(!is.na(model_type) & !is.na(covidence_id) | (!(is.na(model_type)&is.na(refs))) | !is.na(refs)) %>%
  select(model_type,stoch_deter,transmission_route,assumptions,
         compartmental_type,theoretical_model,interventions_type,refs,covidence_id)
mods$model_type  <- paste(mods$model_type, mods$stoch_deter, sep = " - ")
mods$stoch_deter <- NULL
mods$model_type  <- gsub("Branching process - Stochastic", "Branching Process", mods$model_type)
mods$transmission_route <- gsub("Human to human \\(direct contact\\)", "Human-Human", mods$transmission_route)
mods$transmission_route <- gsub("Airborne or close contact;Human to human \\(direct contact\\)","Airborne or close contact; Human-Human",mods$transmission_route)
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
mods$interventions_type <- gsub("changes", "Changes", mods$interventions_type)
mods$interventions_type <- gsub("tracing", "Tracing", mods$interventions_type)
mods$interventions_type <- gsub("Unspecified", "", mods$interventions_type)
# mods                    <- mods %>% mutate(assumptions = case_when(
#   covidence_id %in% c(285,2617,2620) ~ gsub("Groups","Spatial",assumptions),
#   covidence_id %in% c(4120,4371) ~ gsub("Groups","Community Hygiene",assumptions),
#   covidence_id %in% c(4136,4251) ~ gsub("Groups","Socio-Economic Status",assumptions),
#   covidence_id %in% c(4343) ~ gsub("Groups","Protective Behaviour",assumptions),
#   covidence_id %in% c(3735) ~ gsub("Groups","Quarantine Status",assumptions),
#   TRUE ~ assumptions))
mods <- mods %>% select(-c("covidence_id"))
mods$transmission_route <- factor(mods$transmission_route,
                                  levels = c("Human-Human","Airborne or close contact; Human-Human"))
mods$compartmental_type <- factor(mods$compartmental_type,
                                  levels = c("","SIR","SEIR","Other","Other;SIR","SIR;SIS","SIS"))
mods <- mods[order(mods$model_type,mods$transmission_route,mods$assumptions,
                   mods$compartmental_type),]
mods$transmission_route <- as.character(mods$transmission_route)
mods$compartmental_type <- as.character(mods$compartmental_type)

mods$transmission_route[is.na(mods$transmission_route)] <- ""
mods$compartmental_type[is.na(mods$compartmental_type)] <- ""
mods$assumptions[is.na(mods$assumptions)]               <- ""
mods$interventions_type[is.na(mods$interventions_type)] <- ""
mods$refs[is.na(mods$refs)]                             <- ""

mods <- insert_blank_rows(mods,"model_type")
write.table(mods, file = "latex_models.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters
parameters <- mutate_at(parameters, 
                        vars(parameter_value, parameter_lower_bound, parameter_upper_bound, 
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value, 
                             parameter_uncertainty_single_value, distribution_par1_value, distribution_par2_value),
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
parameters$parameter_unit <- gsub("Mutations/genome/generation \\(U\\)", "m/g/g", parameters$parameter_unit)
parameters$parameter_unit <- gsub("No units", "", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Per day", "per day", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Per hour", "per hour", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Percentage \\(%\\)", "\\\\%", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Days", "days", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Weeks", "weeks", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Max. nr. of cases superspreading \\(related to case\\)", "mnc", parameters$parameter_unit)

parameters <- parameters %>% mutate(parameter_unit = case_when(
  exponent == 0 ~ parameter_unit,
  exponent == -2 & parameter_unit == "" ~ "\\\\%",
  exponent == -3 & parameter_unit == "" ~ "per 1000",
  exponent == -4 & parameter_unit == "" ~ "per 10k",
  TRUE ~ paste(parameter_unit, sprintf("$10^{%d}$",exponent), sep=" ")))
# new bit
parameters <- parameters %>% mutate(parameter_value = case_when(
  (parameter_class %in% c("Severity","Seroprevalence") | parameter_unit == "") ~ parameter_value,
  TRUE ~ paste(parameter_value, parameter_unit, sep = " ")))
# parameters <- parameters %>%
#               mutate(parameter_unit = ifelse(parameter_unit == "Percentage (%)", "%", parameter_unit),
#                      values = paste0(values, parameter_unit, sep = " "))
parameters <- parameters %>%
  mutate(unc_type=
           ifelse(!is.na(distribution_par2_type), 
                  paste(distribution_type, distribution_par2_type, sep = " "), 
                  ifelse(!is.na(parameter_uncertainty_type),
                         paste(parameter_uncertainty_type),    
                         ifelse(!is.na(parameter_uncertainty_singe_type),
                                paste(parameter_uncertainty_singe_type),""))))
parameters$unc_type <- gsub("Inter Quartile Range \\(IQR\\)", "IQR", parameters$unc_type)
parameters$unc_type <- gsub("IQR \\[Estimator\\]", "IQR_e", parameters$unc_type)
parameters$unc_type <- gsub("IQR \\[Sample\\]", "IQR_s", parameters$unc_type)
parameters$unc_type <- gsub("Range \\[Estimator\\]", "Range_e", parameters$unc_type)
parameters$unc_type <- gsub("Range \\[Sample\\]", "Range_s", parameters$unc_type)
parameters$unc_type <- gsub("Standard Error \\(SE\\)", "SE", parameters$unc_type)
parameters$unc_type <- gsub("Gamma Standard deviation", "Gamma SD", parameters$unc_type)
parameters$unc_type <- gsub("Normal Standard deviation", "Normal SD", parameters$unc_type)
parameters$unc_type <- gsub("Normal-Log Variance", "Normal-Log var", parameters$unc_type)
parameters$unc_type <- gsub("Standard deviation \\(Sd\\)", "SD", parameters$unc_type)
parameters$unc_type <- gsub("SD \\[Estimator\\]", "SD_e", parameters$unc_type)
parameters$unc_type <- gsub("SD \\[Sample\\]", "SD_s", parameters$unc_type)
parameters$unc_type <- gsub("Highest Posterior Density Interval 95%", "HPDI95%", parameters$unc_type)
parameters$unc_type <- gsub("CRI95%", "CrI95%", parameters$unc_type)
parameters$unc_type <- gsub("%", "\\\\%", parameters$unc_type)
parameters$unc_type <- gsub("_", "\\\\_", parameters$unc_type)
#
parameters <- parameters %>%
  mutate(uncertainty=
           ifelse(distribution_par2_value!="NA",
                  paste(distribution_par2_value),
                  ifelse(parameter_uncertainty_lower_value!="NA" & parameter_uncertainty_upper_value!="NA",
                         paste(parameter_uncertainty_lower_value, parameter_uncertainty_upper_value, sep = "-"),
                         ifelse(parameter_uncertainty_single_value!="NA",
                                paste(parameter_uncertainty_single_value),
                                ""))))
# new bit
parameters <- parameters %>% mutate(unc_type = case_when(
  unc_type %in% c("Unspecified","") ~ "",
  TRUE ~  paste(unc_type, ": ", uncertainty, sep = "")))
##                     
#parameters$cfr_ifr_denominator[is.na(parameters$cfr_ifr_denominator)] <- 
#                                     parameters$population_sample_size[is.na(parameters$cfr_ifr_denominator)]        
parameters$population_country <- gsub(",", "", parameters$population_country)
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

#parameters - transmission
trns_params <- parameters %>%
  filter(grepl("Mutations|Attack|Growth rate|Reproduction|beta - per capita contact rate per unit of time|Overdispersion", parameter_type, ignore.case = TRUE)) %>%
  select(parameter_type, parameter_value, unc_type,
         method_disaggregated_by, 
         genome_site, method_r,
         population_sample_size,
         population_country, dates,
         population_sample_type, population_group, refs, central)
trns_params$parameter_type  <- gsub("beta - per capita contact rate per unit of time", "beta", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("Mutations - ", "", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("Mutations – ", "", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("Attack rate", "Attack Rate", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("Growth rate \\(r\\)", "Growth Rate", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("\\(Basic R0\\)", "R0", trns_params$parameter_type)
trns_params$parameter_type  <- gsub("\\(Effective, Re\\)", "Re", trns_params$parameter_type)
trns_params$parameter_type  <- str_trim(str_to_title(trns_params$parameter_type))
trns_params$parameter_type <- factor(trns_params$parameter_type, 
                                     levels = c("Reproduction Number R0", "Reproduction Number Re",
                                                "Growth Rate", "Overdispersion","Beta",
                                                "Attack Rate", "Secondary Attack Rate",
                                                "Mutation Rate","Substitution Rate"))
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
hdel_params$parameter_type <- gsub("\\(.*?\\)", "", hdel_params$parameter_type)
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
hdel_params <- hdel_params %>% select(-c("other_delay_start","other_delay_end"))
hdel_params$parameter_type <- sub("Symptom Onset", "Onset", hdel_params$parameter_type)
hdel_params$parameter_type <- sub("Admission To Care", "Admission", hdel_params$parameter_type)
hdel_params$parameter_type <- sub("Discharge/Recovery", "Recovery", hdel_params$parameter_type)
hdel_params$parameter_type <- factor(hdel_params$parameter_type, 
                                     levels = c("Incubation Period", "Latent Period", "Infectious Period",
                                                "Onset - Admission", "Symptomatic Period",
                                                "Other Human Delay ", "Generation Time","Serial Interval",
                                                "Admission - Recovery","Admission - Death","Time In Care ",
                                                "Onset - Recovery","Onset - Death"))
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
cfrs_params <- cfrs_params %>% arrange(population_country,as.numeric(central))
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
sero_params <- sero_params %>% arrange(population_country, parameter_type, as.numeric(central))
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
                                         levels = c("Infection", "Severe disease", "Probable case", "Superspreading",           
                                                    "Other", "Death","Serology", "Attack Rate", "Incidence",                
                                                    "Mortality", "Recovery", "Fever & being febrile",
                                                    "Incubation Period", "Admission-Death Delay", 
                                                    "Admission-Discharge Delay", "Incidence Rate"))
risk_params$riskfactor_significant <- str_to_title(risk_params$riskfactor_significant)
risk_params$riskfactor_significant <- factor(risk_params$riskfactor_significant, 
                                             levels = c("Significant","Not Significant","Unspecified"))

risk_params$riskfactor_adjusted <- str_to_title(risk_params$riskfactor_adjusted)
risk_params <- risk_params %>% mutate(riskfactor_name = gsub("Contact with animal","Contact with Animal",riskfactor_name),
                                      riskfactor_name = gsub("Household contact","Household Contact",riskfactor_name),
                                      riskfactor_name = gsub("Close contact","Close Contact",riskfactor_name),
                                      riskfactor_name = gsub("Non-household contact","Non-household Contact",riskfactor_name))

risk_params <- risk_params[order(risk_params$riskfactor_outcome,risk_params$riskfactor_significant,
                                 risk_params$riskfactor_name,risk_params$riskfactor_adjusted),]
risk_params$riskfactor_significant <- as.character(risk_params$riskfactor_significant)
risk_params$riskfactor_significant[is.na(risk_params$riskfactor_significant)] <- "NA"
risk_params <- insert_blank_rows(risk_params,"riskfactor_outcome")
write.table(risk_params, file = "latex_riskfactors.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

