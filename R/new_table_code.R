library(dplyr) 
library(stringr)
library(tibble)
library(purrr)

articles   <- read.csv("../archive/db_compilation/20240131-173644-a90bdeaf/articles.csv",  check.names = FALSE, na.strings = c("", "NA"))
outbreaks  <- read.csv("../archive/db_compilation/20240131-173644-a90bdeaf/outbreaks.csv",  check.names = FALSE, na.strings = c("", "NA"))
models     <- read.csv("../archive/db_compilation/20240131-173644-a90bdeaf/models.csv",  check.names = FALSE, na.strings = c("", "NA"))
parameters <- read.csv("../archive/db_compilation/20240131-173644-a90bdeaf/parameters.csv",  check.names = FALSE, na.strings = c("", "NA"))

insert_blank_rows <- function(dataframe, column) {
  tlabels   <- unique(dataframe[as.character(substitute(column))])#unique values of column
  
  dataframe <- as_tibble(dataframe) %>%
    group_split({{ column }}) %>%
    map_dfr(~ add_row(.x, .after = Inf))#split by values in column
  dataframe <- rbind(NA,dataframe)#add NAs for top row header
  dataframe <- dataframe[-nrow(dataframe),]#remove NAs at bottom
  dataframe[as.character(substitute(column))] <- NULL#remove column
  
  inds                 <- which(!complete.cases(dataframe))
  dataframe[inds,1]    <- tlabels
  dataframe[[1]][inds] <- paste0("\\bfseries{", dataframe[[1]][inds], "}")
  dataframe            <- dataframe %>% mutate_all(~ ifelse(is.na(.), "", .))
}

#outbreaks

outbreaks$covidence_id <- with(articles, 
                          paste(first_author_first_name[match(outbreaks$covidence_id, covidence_id)],
                          " (",year_publication[match(outbreaks$covidence_id, covidence_id)],")", sep = ""))

outbreaks <- outbreaks %>% 
             mutate(sdate=ifelse(!is.na(outbreak_start_month),
                    paste(outbreak_start_month,outbreak_start_year),outbreak_start_year))
outbreaks <- outbreaks %>% 
             mutate(edate=ifelse(!is.na(outbreak_end_month),
                    paste(outbreak_end_month,outbreak_date_year),outbreak_date_year))
outbreaks <- outbreaks %>% 
             mutate(dates=ifelse(!is.na(edate),
                    paste(sdate,edate,sep="-"),paste0(sdate,"-")))
outbreaks <- outbreaks %>% mutate_all(~ ifelse(is.na(.), "", .))

outs <- outbreaks %>%
        select(outbreak_country, outbreak_location, dates, 
               cases_suspected, cases_confirmed, cases_severe, deaths, cases_mode_detection,			
               covidence_id)
outs$cases_mode_detection <- gsub(" \\(PCR etc\\)", "", outs$cases_mode_detection)
outs <- outs %>%
        mutate(outbreak_start_year=outbreaks$outbreak_start_year)
outs <- outs %>% arrange(outbreak_country,-outbreak_start_year)
outs$outbreak_start_year <- NULL
outs <- insert_blank_rows(outs,outbreak_country)
write.table(outs, file = "lassa plots/outs.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#models

models$covidence_id <- with(articles, 
                       paste(first_author_first_name[match(models$covidence_id, covidence_id)],
                       " (",year_publication[match(models$covidence_id, covidence_id)],")", sep = ""))

mods <- models %>%
        select(model_type,stoch_deter,transmission_route,assumptions,
               compartmental_type,theoretical_model,interventions_type,covidence_id)
mods$model_type  <- paste(mods$model_type, mods$stoch_deter, sep = " - ")
mods$stoch_deter <- NULL
mods$model_type  <- gsub("Branching process - Stochastic", "Branching Process", mods$model_type)
mods$transmission_route <- gsub("Vector/Animal to human", "Vector-Human", mods$transmission_route)
mods$transmission_route <- gsub("Human to human \\(direct contact\\)", "Human-Human", mods$transmission_route)
mods$transmission_route <- gsub("Airborne or close contact", "Airborne", mods$transmission_route)
mods$assumptions        <- gsub("Homogeneous mixing", "", mods$assumptions)
mods$assumptions        <- gsub("Latent period is same as incubation period", "", mods$assumptions)
mods$assumptions        <- gsub("Heterogenity in transmission rates - over time", "Time", mods$assumptions)
mods$assumptions        <- gsub("Heterogenity in transmission rates - between groups", "Groups", mods$assumptions)
mods$assumptions        <- gsub("Age dependent susceptibility", "Age", mods$assumptions)
mods$assumptions        <- gsub("^;|;$", "", mods$assumptions)
mods$compartmental_type <- gsub("Not compartmental", "", mods$compartmental_type)
mods$compartmental_type <- gsub("Other compartmental", "Other", mods$compartmental_type)
mods$theoretical_model  <- gsub("FALSE", "Yes", mods$theoretical_model)
mods$theoretical_model  <- gsub("TRUE", "", mods$theoretical_model)
mods$interventions_type <- gsub("Vector/Animal", "Vector", mods$interventions_type)
mods$interventions_type <- gsub("Unspecified", "", mods$interventions_type)
mods <- mods %>% arrange(model_type,transmission_route,assumptions)
mods <- insert_blank_rows(mods,model_type)
write.table(mods, file = "lassa plots/mods.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters

parameters$covidence_id <- with(articles, 
                           paste(first_author_first_name[match(parameters$covidence_id, covidence_id)],
                           " (",year_publication[match(parameters$covidence_id, covidence_id)],")", sep = ""))
##
parameters <- parameters %>% 
              mutate(values = ifelse(grepl("Mutations", parameter_type), sprintf("%.5f", parameter_value * (10^exponent)), 
                                     sprintf("%.1f", parameter_value * (10^exponent))),
                     parameter_lower_bound = ifelse(grepl("Mutations", parameter_type), sprintf("%.5f", parameter_lower_bound * (10^exponent)), 
                                     sprintf("%.1f", parameter_lower_bound * (10^exponent))),
                     parameter_upper_bound = ifelse(grepl("Mutations", parameter_type), sprintf("%.5f", parameter_upper_bound * (10^exponent)), 
                                     sprintf("%.1f", parameter_upper_bound * (10^exponent))))
#
parameters <- parameters %>% 
              mutate(parameter_uncertainty_single_value = ifelse(grepl("Mutations", parameter_type), sprintf("%.5f", parameter_uncertainty_single_value * (10^exponent)), 
                                     sprintf("%.1f", parameter_uncertainty_single_value * (10^exponent))),
                     parameter_uncertainty_lower_value = ifelse(grepl("Mutations", parameter_type), sprintf("%.5f", parameter_uncertainty_lower_value * (10^exponent)), 
                                     sprintf("%.1f", parameter_uncertainty_lower_value * (10^exponent))),
                     parameter_uncertainty_upper_value = ifelse(grepl("Mutations", parameter_type), sprintf("%.5f", parameter_uncertainty_upper_value * (10^exponent)), 
                                     sprintf("%.1f", parameter_uncertainty_upper_value * (10^exponent))),
                     distribution_par1_value = ifelse(grepl("Mutations", parameter_type), sprintf("%.5f", distribution_par1_value), 
                                     sprintf("%.1f", distribution_par1_value)),
                     distribution_par2_value = ifelse(grepl("Mutations", parameter_type), sprintf("%.5f", distribution_par2_value), 
                                     sprintf("%.1f", distribution_par2_value)))
##
parameters <- parameters %>%
              mutate(values=
                     ifelse(values!="NA", 
                     values,
                     ifelse(parameter_lower_bound!="NA" & parameter_upper_bound!="NA",
                     paste(parameter_lower_bound, parameter_upper_bound, sep = "-"), "")))
# parameters <- parameters %>%
#               mutate(parameter_unit = ifelse(parameter_unit == "Percentage (%)", "%", parameter_unit),
#                      values = paste0(values, parameter_unit, sep = " "))
parameters <- parameters %>%
              mutate(uncertainty=
                     ifelse(parameter_uncertainty_single_value!="NA",
                     paste(parameter_uncertainty_single_value),                    
                     ifelse(parameter_uncertainty_lower_value!="NA" & parameter_uncertainty_upper_value!="NA",
                     paste(parameter_uncertainty_lower_value, parameter_uncertainty_upper_value, sep = "-"), 
                     ifelse(distribution_par2_value!="NA",
                     paste(distribution_par2_value),""))))
parameters <- parameters %>%
              mutate(unc_type=
                     ifelse(!is.na(distribution_par2_type), 
                     paste(distribution_type, distribution_par2_type, sep = " "), 
                     ifelse(!is.na(parameter_uncertainty_type),
                     paste(parameter_uncertainty_type),    
                     ifelse(!is.na(parameter_uncertainty_singe_type),
                     paste(parameter_uncertainty_singe_type),""))))
parameters$unc_type <- gsub("Inter Quartile Range \\(IQR\\)", "IQR", parameters$unc_type)
parameters$unc_type <- gsub("Standard Error \\(SE\\)", "St.E", parameters$unc_type)
parameters$unc_type <- gsub("Gamma Standard deviation", "Gamma St.D", parameters$unc_type)
parameters$unc_type <- gsub("Standard deviation \\(Sd\\)", "St.D", parameters$unc_type)
parameters$unc_type <- gsub("Highest Posterior Density Interval 95%", "CrI95%", parameters$unc_type)
##                     
#parameters$cfr_ifr_denominator[is.na(parameters$cfr_ifr_denominator)] <- 
#                                     parameters$population_sample_size[is.na(parameters$cfr_ifr_denominator)]        
parameters$population_country <- gsub(",", "", parameters$population_country)
parameters <- parameters %>% 
              mutate(sdate=ifelse(!is.na(population_study_start_month),
                     paste(population_study_start_month,population_study_start_year),population_study_start_year))
parameters <- parameters %>% 
              mutate(edate=ifelse(!is.na(population_study_end_month),
                     paste(population_study_end_month,population_study_end_year),population_study_end_year))
parameters <- parameters %>% 
              mutate(dates=ifelse(!is.na(sdate),
                     paste(sdate,edate,sep="-"),NA))
parameters$population_sample_type <- gsub("\\s.*", "", parameters$population_sample_type)
parameters <- parameters %>% mutate_all(~ ifelse(is.na(.), "", .))

#parameters - seroprevalence
sero_params <- parameters %>%
               filter(grepl("Seroprevalence", parameter_type, ignore.case = TRUE)) %>%
               select(parameter_type, values,
               uncertainty, unc_type,
               method_disaggregated_by, 
               cfr_ifr_numerator,cfr_ifr_denominator,
               population_country, dates,
               population_sample_type, population_group, covidence_id)
sero_params$parameter_type <- sub("^.* - ", "", sero_params$parameter_type)
sero_params$unc_type       <- gsub("%", "\\\\%", sero_params$unc_type)
sero_params <- sero_params %>% arrange(parameter_type, population_country, 
                               as.numeric(gsub("([0-9]+)-.*", "\\1", gsub("[^-0-9.]", "", values))))
sero_params <- insert_blank_rows(sero_params,parameter_type)
write.table(sero_params, file = "lassa plots/params_sero.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters - transmission
trns_params <- parameters %>%
               filter(grepl("Mutations|Attack|Relative contribution|Growth rate|Reproduction", parameter_type, ignore.case = TRUE)) %>%
               select(parameter_type, values, parameter_from_figure, parameter_unit, 
               uncertainty, unc_type,
               method_disaggregated_by, 
               genome_site, method_r,
               population_sample_size,
               population_country, dates,
               population_sample_type, population_group, covidence_id)
trns_params <- trns_params[!trns_params$parameter_from_figure,]
trns_params$parameter_from_figure <- NULL
trns_params$values <- paste(trns_params$values, trns_params$parameter_unit, sep = " ")
trns_params$parameter_unit <- NULL
trns_params$parameter_type <- gsub("Relative contribution - human to human", "Human Transmission Contribution", trns_params$parameter_type)
trns_params$parameter_type <- gsub("Mutations - ", "", trns_params$parameter_type)
trns_params$parameter_type <- gsub("Mutations – ", "", trns_params$parameter_type)
trns_params$parameter_type <- gsub("\\(.*?\\)", "", trns_params$parameter_type)
trns_params$parameter_type <- str_to_title(trns_params$parameter_type)
trns_params$values         <- gsub("Substitutions/site/year", "s/s/y", trns_params$values)
trns_params$values         <- gsub("No units", "", trns_params$values)
trns_params$values         <- gsub(" Percentage \\(%\\)", "\\\\%", trns_params$values)
trns_params$unc_type       <- gsub("%", "\\\\%", trns_params$unc_type)
trns_params <- trns_params %>% arrange(parameter_type,  
                                       as.numeric(gsub("([0-9]+)-.*", "\\1", gsub("[^-0-9.]", "", values))))
trns_params <- insert_blank_rows(trns_params,parameter_type)
write.table(trns_params, file = "lassa plots/params_trns.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters - human delays
hdel_params <- parameters %>%
               filter(grepl("Human delay", parameter_type, ignore.case = TRUE)) %>%
               select(parameter_type, values, parameter_from_figure, parameter_unit, parameter_value_type,
               uncertainty, unc_type,
               method_disaggregated_by, 
               population_sample_size,
               population_country, dates,
               population_sample_type, population_group, covidence_id)
hdel_params <- hdel_params[!hdel_params$parameter_from_figure,]
hdel_params$parameter_from_figure <- NULL
hdel_params$values <- paste(hdel_params$values, hdel_params$parameter_unit, sep = " ")
hdel_params$parameter_unit <- NULL
hdel_params$parameter_type <- sub("^.* - ", "", hdel_params$parameter_type)
hdel_params$parameter_type <- sub(">", " - ", hdel_params$parameter_type)
hdel_params$parameter_type <- str_to_title(hdel_params$parameter_type)
hdel_params$parameter_type <- gsub("\\(.*?\\)", "", hdel_params$parameter_type)
hdel_params$unc_type <- gsub("%", "\\\\%", hdel_params$unc_type)
hdel_params <- hdel_params %>% arrange(parameter_type,  
                                       as.numeric(gsub("([0-9]+)-.*", "\\1", gsub("[^-0-9.]", "", values))))
hdel_params <- insert_blank_rows(hdel_params,parameter_type)
write.table(hdel_params, file = "lassa plots/params_hdel.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters - CFRs
cfrs_params <- parameters %>%
               filter(grepl("Severity - case fatality rate", parameter_type, ignore.case = TRUE)) %>%
               select(values,
               uncertainty, unc_type,
               method_disaggregated_by, 
               cfr_ifr_method, cfr_ifr_numerator,cfr_ifr_denominator,
               population_country, dates,
               population_sample_type, population_group, covidence_id)
cfrs_params$population_country <- gsub(";", " ", cfrs_params$population_country)
cfrs_params$unc_type <- gsub("%", "\\\\%", cfrs_params$unc_type)
cfrs_params <- cfrs_params %>% arrange(population_country, population_group, 
                               as.numeric(gsub("([0-9]+)-.*", "\\1", gsub("[^-0-9.]", "", values))))         
cfrs_params <- insert_blank_rows(cfrs_params,population_country)
write.table(cfrs_params, file = "lassa plots/params_cfrs.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#parameters - risk factors
risk_params <- parameters %>%
               filter(grepl("Risk factors", parameter_type, ignore.case = TRUE)) %>%
               select(riskfactor_outcome, riskfactor_significant,
               riskfactor_name,	riskfactor_occupation, 
               riskfactor_adjusted, population_sample_size,
               population_country, dates,
               population_sample_type, population_group, covidence_id)
risk_params$riskfactor_outcome <- paste(risk_params$riskfactor_outcome, risk_params$riskfactor_significant, sep = " - ")
risk_params$riskfactor_significant <- NULL
risk_params <- risk_params %>% arrange(riskfactor_outcome, riskfactor_adjusted, riskfactor_name) 
risk_params <- insert_blank_rows(risk_params,riskfactor_outcome)
write.table(risk_params, file = "lassa plots/params_risk.csv", sep = ",", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)