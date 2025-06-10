library(dplyr)
library(orderly2)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

# *============================= Helper functions =============================*
create_cleaning_table <- function(data_table, save_name, article_table){
  article_table <- article_table |>
    select(c("id", "article_id", "covidence_id", "name_data_entry"))
  data_table <- inner_join(article_table, data_table)
  data_table <- data_table |>
    select(-id)
  write_csv(data_table, save_name)
}

# *============================================================================*
# *------------------------------ Orderly config ------------------------------*
orderly_strict_mode()

orderly_parameters(pathogen = NULL)

orderly_dependency(
  "db_cleaning",
  "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "models.csv", "params.csv", "outbreaks.csv"))

orderly_shared_resource("nipah_functions.R"="nipah_functions.R")

source("nipah_functions.R")

orderly_artefact(
  description="Merged single and double extracted data as csv and rds files",
  c("latex_models.csv", "latex_delays.csv", "latex_outbreaks.csv",
    "latex_riskfactors.csv", "latex_seroprevalence.csv", "latex_severity.csv",
    "latex_transmission.csv", "cleaning_models.csv", "cleaning_delays.csv",
    "cleaning_outbreaks.csv", "cleaning_riskfactors.csv",
    "cleaning_seroprevalence.csv", "cleaning_severity.csv",
    "cleaning_transmission.csv")
  )
# *------------------------------- Read in data -------------------------------*
articles <- read_csv("articles.csv")
models <- read_csv("models.csv")
outbreaks <- read_csv("outbreaks.csv")
parameters <- read_csv("params.csv")

# Expected for curation
parameters$parameter_from_figure <- ifelse(
  (is.na(parameters$parameter_from_figure) |
     parameters$parameter_from_figure=="No"),
  FALSE, TRUE)

# TODO: Check data_curation
dfs <- data_curation(articles, outbreaks,models,parameters, plotting = FALSE)

# *============================================================================*
# *================================== Models ==================================*
models <- dfs$models

mods <- models |>
filter(!is.na(refs) |
         !(is.na(model_type) & is.na(covidence_id))
       ) |>
select(model_type,
       stoch_deter,
       transmission_route,
       assumptions,
       compartmental_type,
       model_compartmental_other,
       theoretical_model,
       interventions_type,
       model_spatial,
       model_spillover,
       model_fitting_method,
       model_uncertainty,
       model_data,
       code_available,
       model_language,
       model_readme,
       refs,
       id)

# Replace all commas with semicolons
mods <- mods |>
  mutate(across(where(is.character), ~ gsub(",", ";", .)))

mods$model_type  <- paste(mods$model_type, mods$stoch_deter, sep = " - ")

# Substitutions
model_type_replacements <- c("Branching process - Stochastic"="Branching Process")
transmission_route_replacements <- c(
  "Human to human \\(direct contact\\)"="Human-Human",
  "Airborne or close contact;Human to human \\(direct contact\\)"="Airborne or close contact; Human-Human"
  )

assumptions_replacements <- c(
  "Homogeneous mixing"="",
  "Latent period is same as incubation period"="",
  "^;|;$"="",
  "Heterogenity in transmission rates - over time"="Time",
  "Heterogenity in transmission rates - between groups"="Groups",
  "Age dependent susceptibility"="Age")

compartmental_type_replacements <- c("Not compartmental"="",
                                "Other compartmental"="Other")

interventions_type_replacements <- c("changes"="Changes",
                                "tracing"="Tracing",
                                "Unspecified"="")

theoretical_model_replacements <- c(
  "Yes"="Theoretical",
  "No"="Fitted"
)

mod_value_replacements <- list(
  "model_type"=model_type_replacements,
  "transmission_route"=model_type_replacements,
  "assumptions"=assumptions_replacements,
  "compartmental_type"=compartmental_type_replacements,
  "interventions_type"=interventions_type_replacements,
  "theoretical_model"=theoretical_model_replacements
)

mods[names(mod_value_replacements)] <- lapply(
  names(mod_value_replacements),
  function(name) str_replace_all(mods[[name]], mod_value_replacements[[name]]))

# na replacements
# Named replacement
# Single col but vectorised so that additional columns can be added if necessary
na_replacement <- list(
  theoretical_model = "Theoretical" # Confirm
)

mods <- replace_na(mods, na_replacement)

# Create fitted variable (Redcap coding is the opposite)
# Add superscripts
# Map model_data to Yes/No
# Combine Code text
mods <- mods |>
  mutate(fitted = ifelse(theoretical_model=="Theoretical",
                                    "No", "Yes"),
         fitted=ifelse(
           model_fitting_method=="" | is.na(model_fitting_method),
           fitted, paste0(fitted, "$^*$")),
         model_data=ifelse(model_data %in% c("","Not available"), "No", "Yes"),
         code=ifelse(code_available=="No" | is.na(code_available), "No", "Yes"),
         code=ifelse(model_readme=="No" | is.na(model_readme), code,
                     paste0(code, "$^+$")),
         code=ifelse(is.na(model_language), code, paste0(code, " (",
                                                         model_language, ")"))
         )

# Replace all remaining NAs with ""
mods <- mods |>
  mutate_all(~ ifelse(is.na(.), "", .))

# Ordering
mods <- mods[order(mods$model_type,mods$transmission_route,
                   mods$compartmental_type,
                   mods$assumptions),]

mods <- mods |>
  # recoded theoretical model to match other pathogens
  relocate(fitted, .after = theoretical_model) |>
  relocate(refs, .after = last_col())

# Cleaning table
create_cleaning_table(mods, "cleaning_models.csv", articles)

mods <- mods |>
  select(-c(id,
            model_language,
            model_readme,
            code_available,
            model_fitting_method,
            theoretical_model,
            stoch_deter)
         )

# Latex table
mods <- insert_blank_rows(mods,"model_type")

write.table(mods, file = "latex_models.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# *================================ Outbreaks =================================*
outbreaks <- dfs$outbreaks

# Dates
outbreaks <- outbreaks |>
  mutate(outbreak_start_day   = coalesce(as.numeric(outbreak_start_day),1),
         outbreak_start_month = ifelse(is.na(outbreak_start_month),01,outbreak_start_month),
         outbreak_start_year  = coalesce(outbreak_start_year,0),
         sdate = as.Date(paste(outbreak_start_day,outbreak_start_month,
                               outbreak_start_year, sep = "-"), format = "%d-%m-%Y")) |>
  mutate(outbreak_end_day   = coalesce(as.numeric(outbreak_end_day),28),
         outbreak_end_month = ifelse(is.na(outbreak_end_month), 12, outbreak_end_month),
         outbreak_end_year = coalesce(outbreak_end_year,2024),
         edate = as.Date(paste(outbreak_end_day,outbreak_end_month,
                               outbreak_end_year, sep = "-"), format = "%d-%m-%Y")) |>
  mutate(dates = paste0(
    paste(outbreak_start_day, outbreak_start_month, outbreak_start_year, sep="/"),
    " - ",
    paste(outbreak_end_day,outbreak_end_month,outbreak_end_year, sep="/")
    ),
         dates = gsub("NA","",dates),
         dates = trimws(gsub("\\s{2,}"," ",dates)),
         dates = gsub("\\b(\\d{4})\\s*-\\s*\\1\\b","\\1", dates,perl = TRUE))

# Columns not included:
# outbreak_duration_months,
# ongoing,
# outbreak_location_type,
# pre_outbreak,
# cases_severe, # no severe cases
# population_size,
# male_cases,
# female_cases,
# prop_female_cases,
# outbreak_notes,
outs <- outbreaks |>
  select(outbreak_country,
         outbreak_location,
         dates,
         outbreak_source,
         cases_mode_detection,
         cases_confirmed,
         cases_suspected,
         outbreak_probable,
         cases_unspecified,
         cases_asymptomatic, # Star
         asymptomatic_transmission, # Plus
         deaths,
         type_cases_sex_disagg,
         prop_male_cases,
         refs,
         sdate,
         edate,
         id)

# Replace all commas with semicolons
outs <- outs |>
  mutate(across(where(is.character), ~ gsub(",", ";", .)))

outs <- outs |>
  mutate(# Add superscripts
         superscript=ifelse((!is.na(asymptomatic_transmission) &
                               asymptomatic_transmission=="Yes"),"*",""),
         superscript=ifelse((!is.na(cases_asymptomatic) &
                               outs$cases_asymptomatic > 0),
                            paste0(superscript,"+"), superscript),
               dates=ifelse(superscript!="",
                            paste0(dates, "$^", superscript, "$"),
                            dates),
         # Convert proportions and round
         prop_male_cases=ifelse(prop_male_cases>1, prop_male_cases/100, prop_male_cases),
         prop_male_cases=round(prop_male_cases, 2),
         # Since we use + as the superscript, replace + in strings to &
         # will only apply to Confirmed + Suspected
         cases_mode_detection=str_replace_all(cases_mode_detection, "\\+", "\\\\&")
         )

# Replace all NAs with ""
outs <- outs |>
  mutate_all(~ ifelse(is.na(.), "", .))

# Order rows
outs <- outs |> arrange(
  tolower(outbreak_country),
  desc(sdate),desc(edate))

outs$edate <- NULL
outs$sdate <- NULL

# Cleaning table
create_cleaning_table(outs, "cleaning_outbreaks.csv", articles)

outs <- outs |>
  select(-c(id, cases_asymptomatic, asymptomatic_transmission, superscript))

# Latex table
outs <- insert_blank_rows(outs,"outbreak_country")
write.table(outs, file = "latex_outbreaks.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# *================================ Parameters ================================*
# *----------------------------- General cleaning -----------------------------*
# Parameters not mapped:
# parameter_paired
# parameter_hd_from
# parameter_hd_to
# parameter_context_location_type
parameters <- dfs$parameters

# From Zika: round to 10 decimal places and remove any trailing zeroes
parameters <- mutate_at(
  parameters,
  vars(parameter_value, parameter_lower_bound, parameter_upper_bound,
       parameter_uncertainty_lower_value, parameter_uncertainty_upper_value,
       parameter_uncertainty_single_value, distribution_par1_value,
       distribution_par2_value, parameter_2_value, parameter_2_lower_bound,
       parameter_2_upper_bound, parameter_2_uncertainty_lower_value,
       parameter_2_uncertainty_upper_value,
       parameter_2_uncertainty_single_value, distribution_2_par1_value,
       distribution_2_par2_value),
  ~sub("\\.?0+$", "", sprintf("%.10f", round(., 10))))

# TODO: Unspecified to ""?
p_unit_replacements <- c(
  "substitutions/site/year" = "s/s/y",
  "no units" = "",
  "percentage \\(%\\)" = "\\\\%",
  "max. nr. of cases superspreading \\(related to case\\)"="mnc")

unc_replacements <- c(
  " \\(paired\\)" = "",
  " \\(paired or unpaired\\)" = "",
  "Inter Quartile Range \\(IQR\\)" = "IQR",
  "Standard Error" = "SE",
  "Standard Deviation" = "SD",
  "Gamma Standard deviation" = "Gamma SD",
  "Highest Posterior Density Interval 95%" = "HPDI95%",
  "CRI95%" = "CrI95%",
  "%" = "\\\\%"
)

combine_cols <- function(df, columns, collapse=" "){
  output_vec <- apply(df[,columns], 1,
                      function(col) paste(na.omit(col), collapse = collapse))

  output_vec[output_vec == "NA"] <- NA
  output_vec[output_vec == ""] <- NA

  return(output_vec)
}

# Treat both parameter value + uncertainty and variability the same
param_identifier <- c("", "_2")
new_col_identifier <- c("", "_var")

for (i in 1:length(param_identifier)) {
  # identifiers
  id <- param_identifier[i]
  nc_id <- new_col_identifier[i]

  # col names
  value_col <- paste0("parameter", id, "_value")
  value_type_col <- paste0("parameter", id, "_value_type")
  value_bounds_col <- paste0("parameter",id,"_bounds")

  unit_col  <- paste0("parameter", id, "_unit")
  exp_col    <- paste0("exponent", id)

  # Na cols
  parameters[[exp_col]][is.na(parameters[[exp_col]])] <- 0

  # Clean parameter unit
  parameters[[unit_col]][is.na(parameters[[unit_col]])] <- "unspecified"
  parameters[[unit_col]] <- tolower(parameters[[unit_col]])

  # Unit replacements
  parameters[[unit_col]] <- str_replace_all(parameters[[unit_col]],
                                            p_unit_replacements)

  parameters[[value_col]] <- coalesce(
    na_if(parameters[[value_col]], "NA"), # coalesce only works with NA, so convert string
    parameters[[value_bounds_col]],
    "")

  # Adding units to values
  parameters[[value_col]] <-  ifelse(
    parameters[[unit_col]] == "" | parameters[[unit_col]] == "unspecified",
    parameters[[value_col]],
    paste( parameters[[value_col]], parameters[[unit_col]], sep = " "))

  # Unit replacement based on explonents, assuming no units provided and not R
  unit_r_condition <- (parameters[[unit_col]] == "" &
                         parameters[["parameter_class"]] != "Reproduction number")

  # # 5.6 exponent? Changed d to f -> for float exponents
  parameters[[unit_col]] <- case_when(
    parameters[[exp_col]] == 0 ~ parameters[[unit_col]],
    parameters[[exp_col]] == -2 & unit_r_condition ~ "\\%",
    parameters[[exp_col]] == -3 & unit_r_condition ~ "per 1000",
    parameters[[exp_col]] == -4 & unit_r_condition ~ "per 10k",
    parameters[[exp_col]] == -5 & unit_r_condition ~ "per 100k",
    TRUE ~ paste0(
      parameters[[unit_col]], " ",
      "10^{", sub("\\.?0+$", "", sprintf("%.5f", parameters[[exp_col]])), "}"
    )
  )

  # Loop across dist par 1 and 2 for params 1 and 2
  # Add a superscript if distribution uncertainty is available
  for (j in 1:2){
    dist_par_type  <- paste0("distribution", id, "_par",i,"_type")
    dist_par_unc  <- paste0("distribution", id, "_par",i,"_uncertainty")
    parameters[[dist_par_type]] <- ifelse(
      !is.na(parameters[[dist_par_unc]]),
      paste0(parameters[[dist_par_type]], "$^+$"),
      parameters[[dist_par_type]]
    )
  }

  # colnames
  dist_1 <- paste0("distribution", id, "_1")
  dist_type_1 <- paste0("distribution", id, "_par1_type")
  dist_value_1 <- paste0("distribution", id, "_par1_value")

  dist_2 <- paste0("distribution", id, "_2")
  dist_type_2 <- paste0("distribution", id, "_par2_type")
  dist_value_2 <- paste0("distribution", id, "_par2_value")

  dist_type <- paste0("distribution", id, "_type")

  combined_dist <- paste0("distribution", nc_id)

  parameters[[dist_1]] <- combine_cols(parameters,
                                      c(dist_type_1, dist_value_1),
                                      ": ")

  parameters[[dist_2]] <- combine_cols(parameters,
                                       c(dist_type_2, dist_value_2),
                                       ": ")

  parameters[[combined_dist]] <- combine_cols(parameters,
                                          c(dist_1, dist_2),
                                          ", ")

  parameters[[combined_dist]] <- combine_cols(parameters,
                                              c(dist_type, combined_dist),
                                              "- ")

  # colnames
  param_unc_single_value <- paste0("parameter",
                                   id,
                                   "_uncertainty_single_value")
  param_unc_single_type <- paste0("parameter",
                                   id,
                                   "_uncertainty_single_type")
  param_unc_type <- paste0("parameter", id,
                           "_uncertainty_type")

  unc_bounds <- paste0("uncertainty", id, "_bounds")

  unc <- paste0("uncertainty", nc_id)
  unc_type <- paste0("unc", nc_id, "_type")

  # Is distribution_par2_value mutually exclusive for uncertainty bounds??
  parameters[[unc]] <- coalesce(
      na_if(parameters[[param_unc_single_value]],"NA"),
      parameters[[unc_bounds]],
      ""
    )

  # Check if combined var can be used? When value is missing combined var is NA
  parameters[[unc_type]] <- coalesce(parameters[[param_unc_type]],
                                     parameters[[param_unc_single_type]],
                                     NA)

  parameters[[unc_type]] <- ifelse(
    parameters[[unc_type]] %in% c("Unspecified","", NA),
    "",
    paste(parameters[[unc_type]], ": ", parameters[[unc]], sep = "")
    )

  # Since dist is favoured show dist but indicate that there is also uncertainty
  parameters[[combined_dist]] <- ifelse(
    !is.na(parameters[[combined_dist]]) & !is.na(parameters[[unc_type]]),
    paste0(parameters[[combined_dist]], "$^*$"),
    parameters[[unc_type]])

  # Favour distribution
  parameters[[unc_type]] <- coalesce(parameters[[combined_dist]],
                                     parameters[[unc_type]],
                                     NA)

  parameters[[unc_type]] <- str_replace_all(parameters[[unc_type]],
                                            unc_replacements)
}

# Dates
parameters <- parameters |>
  mutate(population_study_start_day   = coalesce(as.numeric(gsub("x+", NA, population_study_start_day)),1), # Repalce x's with NA, numeric, coalesce
         population_study_start_month = coalesce(as.numeric(gsub("x+", NA, population_study_start_month)),1),
         population_study_start_year  = coalesce(as.numeric(gsub("x+", NA, population_study_start_year)), 1000),
         sdate = as.Date(paste(population_study_start_day,population_study_start_month,
                               population_study_start_year, sep = "-"), format = "%d-%m-%Y"),
         population_study_end_day   = coalesce(as.numeric(gsub("x+", NA, population_study_end_day)), 28), # more precise?
         population_study_end_month = coalesce(as.numeric(gsub("x+", NA, population_study_end_month)),1),
         population_study_end_year = coalesce(as.numeric(gsub("x+", NA, population_study_end_year)),2025),
         edate = as.Date(paste(population_study_end_day,population_study_end_month,
                               population_study_end_year, sep = "-"), format = "%d-%m-%Y")) |>
  mutate(dates = paste0(
    paste(population_study_start_day, population_study_start_month, population_study_start_year, sep="/"),
    " - ",
    paste(population_study_end_day,population_study_end_month,population_study_end_year, sep="/")),
    dates = gsub("NA","",dates),
    dates = trimws(gsub("\\s{2,}"," ",dates)),
    dates = gsub("\\b(\\d{4})\\s*-\\s*\\1\\b","\\1",dates,perl = TRUE),
    dates = ifelse(dates == "-","",dates))

# Replacements
population_sample_type_replacements <- c(
  "Trade / business"="Trade/business",
  "\\s.*"=""  # keep the first word only
)

method_disaggregated_by_replacements <- c(
  "Level of exposure"="Level of Exposure",
  "Disease generation"="Disease Generation"
)

param_value_replacements <- list(
  "population_sample_type"=population_sample_type_replacements,
  "method_disaggregated_by"=method_disaggregated_by_replacements
)

parameters[names(param_value_replacements)] <- lapply(
  names(param_value_replacements),
  function(name) str_replace_all(parameters[[name]],
                                 param_value_replacements[[name]]))

parameters$population_group <- str_to_title(parameters$population_group)

# CFR and Serology
# Copy population sample if CFR is na
parameters$cfr_ifr_denominator[is.na(parameters$cfr_ifr_denominator)] <-
  parameters$population_sample_size[is.na(parameters$cfr_ifr_denominator)]

# Replace all NAs with ""
parameters <- parameters |>
  mutate_all(~ ifelse(is.na(.), "", .))

# Replace all commas with semicolons
parameters <- parameters |>
  mutate(across(where(is.character), ~ gsub(",", ";", .)))

# *------------------------------- Transmission -------------------------------*
# Check what should still be added
# TODO: variability
trns_params <- parameters |>
  filter(
  grepl(paste0("Attack|Relative contribution|Growth rate|Reproduction|",
               "Mutations|Overdispersion|proportion of symptomatic cases"),
        parameter_type, ignore.case = TRUE)) |>
  select(parameter_type, parameter_value, unc_type,
         parameter_2_value, unc_var_type,
         method_disaggregated_by,
         genome_site, method_r,
         population_sample_size,
         population_country, dates,
         population_sample_type,
         population_group,
         refs,
         central,
         id)

trns_params_pt_replacements <- c(
  "Reproduction number \\(Basic R0\\)" = "Reproduction Number",
  "Relative contribution - human to human" = "Human-Human Transmission Contribution",
  "Relative contribution - zoonotic to human" = "Human-Vector Transmission Contribution",
  "Attack rate (inverse parameter)" = "Attack rate",
  "Reproduction number \\(Effective; Re\\)" = "Effective Reproduction Number",
  "Mutations - " = "",
  "Severity - proportion of symptomatic cases" = "Proportion of Symptomatic Cases"
)

trns_params$parameter_type <- str_replace_all(trns_params$parameter_type,
                                              trns_params_pt_replacements)

trns_params$parameter_type  <- str_to_title(trns_params$parameter_type)
trns_params$method_r <- str_to_title(trns_params$method_r)

trns_params <- trns_params[order(trns_params$parameter_type,
                                 trns_params$genome_site,
                                 as.numeric(trns_params$central)),]


trns_params <- trns_params |>
  select(-c(central, unc_var_type))

# Cleaning table
create_cleaning_table(trns_params, "cleaning_transmission.csv", articles)

trns_params <- trns_params |>
  select(-id)

# Latex table
trns_params <- insert_blank_rows(trns_params,"parameter_type")
write.table(trns_params, file = "latex_transmission.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)
# *------------------------------- Human delays -------------------------------*
hdel_params <- parameters |>
  filter(grepl("delay", parameter_type, ignore.case = TRUE)) |>
  select(parameter_type, other_delay_start, other_delay_end,
         parameter_hd_from, parameter_hd_to,
         parameter_value, parameter_value_type, unc_type,
         parameter_2_value, unc_var_type,
         method_disaggregated_by,
         population_sample_size,
         population_country, dates,
         population_sample_type, population_group, refs, central, id)

hdel_params$parameter_type <- sub("^.* - ", "", hdel_params$parameter_type)
hdel_params$parameter_type <- sub(">", " - ", hdel_params$parameter_type)
hdel_params$parameter_type <- str_to_title(hdel_params$parameter_type)
hdel_params$parameter_type <- gsub("  \\(.*?\\)", "", hdel_params$parameter_type)

# hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bIcu\\b", "ICU")
# hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bNaat\\b", "NAAT")
# hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bRna\\b", "RNA")
# hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bIgm\\b", "IgM")
# hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bIgg\\b", "IgG")
# hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bPcr\\b", "PCR")
# hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bZikv\\b", "ZIKV")
# hdel_params$parameter_type = str_replace(hdel_params$parameter_type, "\\bEip\\b", "EIP")

# TODO: Use during cleaning
# Comment out for now
# hdel_params <- hdel_params |> mutate(parameter_type = case_when(
#   (other_delay_start =="Symptom Onset/Fever" & other_delay_end == "Symptom Resolution") ~ "Symptomatic Period",
#   (other_delay_start =="Symptom Onset/Fever" & other_delay_end == "Specimen Collection") ~ "Onset - Testing",
#   (other_delay_start =="Specimen Collection" & other_delay_end == "Test Result") ~ "Testing - Test Result",
#   (other_delay_start =="Admission to Care/Hospitalisation" & other_delay_end == "Symptom Resolution") ~ "Admission - Symptom Resolution",
#   (other_delay_start =="Symptom Onset/Fever" & other_delay_end == "Other: Sample collection/Diagnosis") ~ "Onset - Testing",
#   (other_delay_start =="Symptom Onset/Fever" & other_delay_end == "Other: Start of ribavirin treatment") ~ "Onset - Start of Treatment",
#   (other_delay_start =="Other: Start of ribavirin treatment" & other_delay_end == "Other: End of ribavirin treatment") ~ "Duration of Antiviral Treatment",
#   (other_delay_start =="Other: Start of oxygen therapy" & other_delay_end == "Other: End of oxygen therapy") ~ "Duration of Oxygen Therapy",
#   (other_delay_start =="Other: Start of antibacterial therapy" & other_delay_end == "Other: End of antibacterial therapy") ~ "Duration of Antibacterial Therapy",
#   TRUE ~ parameter_type))

hdel_params <- hdel_params |> mutate(
  parameter_hd_from = ifelse(parameter_hd_from %in% c("Other", ""),
                             other_delay_start, parameter_hd_from),
  parameter_hd_to = ifelse(parameter_hd_to %in% c("Other", ""),
                           other_delay_end, parameter_hd_to),
  other = paste(parameter_hd_from, ":", parameter_hd_to),
  parameter_type = coalesce(na_if(other, " : "),
                            parameter_type)
  )

hdel_params <- hdel_params |>
  select(-c(other_delay_start,
            other_delay_end,
            parameter_hd_from,
            parameter_hd_to,
            other)
         )

parameter_type_replacement <- c("Symptom Onset"="Onset",
                                "Admission To Care"="Admission",
                                "Discharge/Recovery"="Recovery")
hdel_params$parameter_type <- str_replace_all(hdel_params$parameter_type,
                                              parameter_type_replacement)

hdel_params <- hdel_params[order(hdel_params$parameter_type,
                                 hdel_params$population_country,
                                 as.numeric(hdel_params$central)),]

hdel_params <- hdel_params |>
  select(-c(central))

# Cleaning table
create_cleaning_table(hdel_params, "cleaning_delays.csv", articles)

hdel_params <- hdel_params |>
  select(-id)

# Latex table
hdel_params <- insert_blank_rows(hdel_params,"parameter_type")
write.table(hdel_params, file = "latex_delays.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# *----------------------------------- CFR ------------------------------------*
cfrs_params <- parameters |>
  filter(grepl("(CFR)", parameter_type, ignore.case = TRUE)) |>
  mutate(parameter_type = ifelse(parameter_type == 'Severity - case fatality rate (CFR)',
                                 'Case fatality ratio',
                                 parameter_type)) |>
  select(parameter_value, unc_type,
         parameter_2_value, unc_var_type,
         method_disaggregated_by,
         cfr_ifr_method, cfr_ifr_numerator,cfr_ifr_denominator,
         population_country, dates,
         population_sample_type, population_group, refs, central, id)

cfrs_params$population_country <- gsub(";", "\\, ", cfrs_params$population_country)
cfrs_params$population_country[cfrs_params$population_country==""] <- "Unspecified"

cfrs_params <- cfrs_params |> arrange(tolower(population_country),
                                       as.numeric(central))

cfrs_params |>
  filter(parameter_2_value != "" | unc_var_type != "") |>
  NROW()

cfrs_params <- cfrs_params |> select(-c(central,
                                         parameter_2_value,
                                         unc_var_type))

# Cleaning table
create_cleaning_table(cfrs_params, "cleaning_severity.csv", articles)

cfrs_params <- cfrs_params |>
  select(-id)

# Latex table
cfrs_params <- insert_blank_rows(cfrs_params, "population_country")

write.table(cfrs_params, file = "latex_severity.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# *--------------------------------- Serology ---------------------------------*
sero_params <- parameters |>
  filter(grepl("Seroprevalence", parameter_type, ignore.case = TRUE)) |>
  select(parameter_value, unc_type,
         parameter_2_value, unc_var_type,
         method_disaggregated_by,
         parameter_type,cfr_ifr_numerator,cfr_ifr_denominator,
         population_country, dates,
         population_sample_type, population_group, refs, central,
         covidence_id,
         id)

sero_params$parameter_type <- sub("^.* - ", "", sero_params$parameter_type)
sero_params$population_country <- gsub(";", "\\, ", sero_params$population_country)
sero_params$population_country[sero_params$population_country==""] <- "Unspecified"
sero_params <- sero_params |>
  arrange(tolower(population_country), parameter_type, as.numeric(central))

# No variability for sero
sero_params |>
  filter(parameter_2_value != "" | unc_var_type != "") |>
  NROW()

sero_params <- sero_params |>
  select(-c(central, covidence_id, parameter_2_value, unc_var_type))

# Cleaning table
create_cleaning_table(sero_params, "cleaning_seroprevalence.csv", articles)

sero_params <- sero_params |>
  select(-id)

# Latex table
sero_params <- insert_blank_rows(sero_params,"population_country")
write.table(sero_params, file = "latex_seroprevalence.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# *------------------------------- Risk factors -------------------------------*
risk_params <- parameters |>
  filter(grepl("Risk factors", parameter_type, ignore.case = TRUE)) |>
  select(riskfactor_outcome,
         riskfactor_name,	riskfactor_significant,
         riskfactor_adjusted, population_sample_size,
         population_country, dates,
         population_sample_type, population_group, refs,
         id)

# TODO: Update
risk_params$riskfactor_outcome <- factor(risk_params$riskfactor_outcome,
                                         levels = c("Infection",
                                                    "Serology",
                                                    "Death (in general population)",
                                                    "Spillover risk",
                                                    "Other neurological symptoms in general population",
                                                    "Other",
                                                    ""))

risk_params$riskfactor_significant <- str_to_title(risk_params$riskfactor_significant)

risk_params$riskfactor_significant <- factor(risk_params$riskfactor_significant,
                                             levels = c("Significant","Not Significant","Unspecified"))

risk_params$riskfactor_adjusted <- str_to_title(risk_params$riskfactor_adjusted)

risk_params <- risk_params[order(risk_params$riskfactor_outcome,
                                 risk_params$riskfactor_significant,
                                 risk_params$riskfactor_name,
                                 risk_params$riskfactor_adjusted),]

risk_params$riskfactor_significant <- as.character(risk_params$riskfactor_significant)

# Cleaning table
create_cleaning_table(risk_params, "cleaning_riskfactors.csv", articles)

risk_params <- risk_params |>
  select(-id)

# Latex table
risk_params <- insert_blank_rows(risk_params,"riskfactor_outcome")
write.table(risk_params, file = "latex_riskfactors.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)
