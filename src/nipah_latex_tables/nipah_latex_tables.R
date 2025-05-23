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
  "nipah_cleaning",
  "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "models.csv", "params.csv", "outbreaks.csv"))

orderly_resource("nipah_functions.R")

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

# *---------------------------------- Models ----------------------------------*
models     <- dfs$models

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
mods$stoch_deter <- NULL

# Substitutions
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
mods$theoretical_model  <- gsub("Yes", "Theoretical", mods$theoretical_model)
mods$theoretical_model  <- gsub("No", "Fitted", mods$theoretical_model)
mods$interventions_type <- gsub("changes", "Changes", mods$interventions_type)
mods$interventions_type <- gsub("tracing", "Tracing", mods$interventions_type)
mods$interventions_type <- gsub("Unspecified", "", mods$interventions_type)

# na replacements
# Confirm<<<<
mods$theoretical_model[is.na(mods$theoretical_model)] <- "Theoretical"
mods$transmission_route[is.na(mods$transmission_route)] <- ""
mods$compartmental_type[is.na(mods$compartmental_type)] <- ""
mods$assumptions[is.na(mods$assumptions)]               <- ""
mods$interventions_type[is.na(mods$interventions_type)] <- ""
mods$refs[is.na(mods$refs)]                             <- ""

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
            theoretical_model)
         )

# Latex table
mods <- insert_blank_rows(mods,"model_type")

write.table(mods, file = "latex_models.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)
# *-------------------------------- Outbreaks ---------------------------------*
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
# *-------------------------------- Parameters --------------------------------*
# Parameters not mapped:
# parameter_paired
# parameter_hd_from
# parameter_hd_to
# parameter_context_location_type
parameters <- dfs$parameters

# From Zika: round to 10 decimal places and remove any trailing zeroes
parameters <- mutate_at(parameters,
                        vars(parameter_value, parameter_lower_bound, parameter_upper_bound,
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value,
                             parameter_uncertainty_single_value, distribution_par1_value, distribution_par2_value,
                             parameter_value, parameter_2_lower_bound, parameter_2_upper_bound,
                             parameter_2_uncertainty_lower_value, parameter_2_uncertainty_upper_value,
                             parameter_2_uncertainty_single_value, distribution_2_par1_value, distribution_2_par2_value
                        ),
                        ~sub("\\.?0+$", "", sprintf("%.10f", round(., 10))))

# Merge parameter value and lower and upper bound and replace na with 0
# NA replacement
na_replacement <- c("exponent" = 0)
parameters$exponent[is.na(parameters$exponent)] <- 0

parameters <- parameters |>
  mutate(parameter_value = coalesce(
    na_if(parameter_value, "NA"), # coalesce only works with NA, so convert string
    parameter_bounds,
    "")
  )

# Clean parameter unit
# NA replacement
na_replacement <- c(na_replacement,
                    c("parameter_unit" = "unspecified"))

parameters$parameter_unit[is.na(parameters$parameter_unit)] <- "unspecified"
parameters$parameter_unit <- tolower(parameters$parameter_unit)

# TODO: Unspecified to ""?
p_unit_replacements <- c(
  "substitutions/site/year" = "s/s/y",
  "no units" = "",
  "percentage \\(%\\)" = "\\\\%",
  "max. nr. of cases superspreading \\(related to case\\)"="mnc")

parameters$parameter_unit <- str_replace_all(parameters$parameter_unit,
                                             p_unit_replacements)

# 5.6 exponent? Changed d to f -> for float exponents
parameters <- parameters |> mutate(parameter_unit = case_when(
  exponent == 0 ~ parameter_unit,
  exponent == -2 & parameter_unit == "" & parameter_class != "Reproduction number" ~ "\\%",
  exponent == -3 & parameter_unit == "" & parameter_class != "Reproduction number" ~ "per 1000",
  exponent == -4 & parameter_unit == "" & parameter_class != "Reproduction number" ~ "per 10k",
  exponent == -5 & parameter_unit == "" & parameter_class != "Reproduction number" ~ "per 100k",
  TRUE ~ paste0(parameter_unit, " ", "10^{", sub("\\.?0+$", "", sprintf("%.5f",exponent)), "}"))
  )

# Add parameter unit to parameter if specified
parameters <- parameters |>
  mutate(parameter_value = case_when(
    (parameter_unit == "" | parameter_unit=="unspecified") ~ parameter_value,
    TRUE ~ paste(parameter_value, parameter_unit, sep = " ")))

# Uncertainty
combine_cols <- function(df, columns, collapse=" "){
  output_vec <- apply(df[,columns], 1,
                      function(col) paste(na.omit(col), collapse = collapse))

  output_vec[output_vec == "NA"] <- NA
  output_vec[output_vec == ""] <- NA

  return(output_vec)
}

# Add a superscript if distribution uncertainty is available
parameters <- parameters |>
    mutate(distribution_par1_type = ifelse(
      !is.na(distribution_par1_uncertainty),
      paste0(distribution_par1_type,"$^+$"),
      distribution_par1_type),
      distribution_par2_type = ifelse(
        !is.na(distribution_par2_uncertainty),
        paste0(distribution_par2_type,"$^+$"),
        distribution_par2_type)
      )

parameters$distribution_1 <- combine_cols(
  parameters,
  c("distribution_par1_type", "distribution_par1_value"),
  ": ")

parameters$distribution_2 <- combine_cols(
  parameters,
  c("distribution_par2_type", "distribution_par2_value"),
  ": ")

parameters$distribution <- combine_cols(
  parameters,
  c("distribution_1", "distribution_2"),
  ", ")

parameters$distribution <- combine_cols(
  parameters,
  c("distribution_type", "distribution"),
  "- ")


# More efficient and readable than the above
# Is distribution_par2_value mutually exclusive for uncertainty bounds??
parameters <- parameters  |>
  mutate(uncertainty = coalesce(
    na_if(parameter_uncertainty_single_value,"NA"),
    uncertainty_bounds,
    ""
  ))

# Check if combined var can be used? When value is missing combined var is NA
parameters <- parameters |>
  mutate(unc_type = coalesce(parameter_uncertainty_type,
                             parameter_uncertainty_single_type,
                             NA)
  )

parameters <- parameters |> mutate(unc_type = case_when(
  unc_type %in% c("Unspecified","", NA) ~ "",
  TRUE ~  paste(unc_type, ": ", uncertainty, sep = "")))

# Since dist is favoured show dist but indicate that there is also uncertainty
parameters <- parameters |>
  mutate(
    distribution=ifelse(!is.na(distribution) & !is.na(unc_type),
                    paste0(distribution, "$^*$"),
                    unc_type)
  )

# Favour distribution
parameters <- parameters |>
  mutate(unc_type = coalesce(distribution,
                             unc_type,
                             NA)
  )

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

parameters$unc_type <- str_replace_all(parameters$unc_type,
                                       unc_replacements)

# *------------------------------- Variability --------------------------------*
# Which columns relate to parameter 2 (variability)
colnames(parameters)[grepl("parameter_2|par2", colnames(parameters))]

na_replacement <- c("exponent_2" = 0)
parameters$exponent_2[is.na(parameters$exponent_2)] <- 0

parameters <- parameters |>
  mutate(parameter_2_value = coalesce(
    parameter_2_bounds,
    as.character(parameter_2_value),
    NA)
  )

# Clean parameter unit
# NA replacement
na_replacement <- c(na_replacement,
                    c("parameter_2_unit" = "unspecified"))

parameters$parameter_2_unit[is.na(parameters$parameter_2_unit)] <- "unspecified"
parameters$parameter_2_unit <- tolower(parameters$parameter_2_unit)

parameters$parameter_2_unit <- str_replace_all(parameters$parameter_2_unit,
                                               p_unit_replacements)

# Should be the same
parameters <- parameters |> mutate(parameter_2_unit = ifelse(
  is.na(parameter_2_unit) & !is.na(na_if(parameter_2_value, "NA")), parameter_unit, parameter_2_unit)
  )

# 5.6 exponent? Changed d -> for float exponents
parameters <- parameters |> mutate(parameter_2_unit = case_when(
  exponent_2 == 0 ~ parameter_2_unit,
  exponent_2 == -2 & parameter_2_unit == "" & parameter_class != "Reproduction number" ~ "\\%",
  exponent_2 == -3 & parameter_2_unit == "" & parameter_class != "Reproduction number" ~ "per 1000",
  exponent_2 == -4 & parameter_2_unit == "" & parameter_class != "Reproduction number" ~ "per 10k",
  exponent_2 == -5 & parameter_2_unit == "" & parameter_class != "Reproduction number" ~ "per 100k",
  TRUE ~ paste0(parameter_2_unit, " ", "10^{", sub("\\.?0+$", "", sprintf("%.5f",exponent_2)), "}"))
)

# Add parameter unit to parameter if specific
parameters <- parameters |>
  mutate(parameter_2_value = case_when(
    (parameter_2_unit == "" | parameter_2_unit=="unspecified") ~ parameter_2_value,
    is.na(parameter_2_value) ~ NA,
    TRUE ~ paste(parameter_2_value, parameter_2_unit, sep = " ")))

parameters$parameter_2_value_type <- str_replace_all(parameters$parameter_2_value_type,
                                                     unc_replacements)

parameters <- parameters |>
  mutate(parameter_2_value = case_when(
    is.na(parameter_2_value_type) ~ parameter_2_value,
    is.na(parameter_2_value) ~ parameter_2_value_type,
    TRUE ~ paste0(parameter_2_value_type, ": ", parameter_2_value))
    )

# Uncertainty
parameters <- parameters |>
  mutate(distribution_2_par1_type = ifelse(
    !is.na(distribution_2_par1_uncertainty),
    paste0(distribution_2_par1_type,"$^+$"),
    distribution_2_par1_type),
    distribution_2_par2_type = ifelse(
      !is.na(distribution_2_par2_uncertainty),
      paste0(distribution_2_par2_type,"$^+$"),
      distribution_2_par2_type)
  )

parameters$distribution_var_1 <- combine_cols(
  parameters,
  c("distribution_2_par1_type", "distribution_2_par1_value"),
  ": ")

parameters$distribution_var_2 <- combine_cols(
  parameters,
  c("distribution_2_par2_type", "distribution_2_par2_value"),
  ": ")

parameters$distribution_var <- combine_cols(
  parameters,
  c("distribution_var_1", "distribution_var_2"),
  ", ")

parameters$distribution_var <- combine_cols(
  parameters,
  c("distribution_2_type", "distribution_var"),
  "| ")

# More efficient and readable than the above
# Is distribution_par2_value mutually exclusive for uncertainty bounds??
parameters <- parameters  |>
  mutate(uncertainty_var = coalesce(
    na_if(parameter_2_uncertainty_single_value,"NA"),
    uncertainty_2_bounds,
    ""
  ))

# Check if combined var can be used? When value is missing combined var is NA
parameters <- parameters |>
  mutate(unc_var_type = coalesce(parameter_2_uncertainty_type,
                                 parameter_2_uncertainty_single_type,
                                 NA)
  )

# new bit
parameters <- parameters |> mutate(unc_var_type = case_when(
  unc_var_type %in% c("Unspecified","", NA) ~ "",
  TRUE ~  paste(unc_var_type, ": ", uncertainty_var, sep = "")))

parameters <- parameters |>
  mutate(
    distribution_var=ifelse(!is.na(distribution_var) & !is.na(unc_var_type),
                        paste0(distribution_var, "$^*$"),
                        unc_var_type)
  ) # Since dist is favoured show dist but indicate that there is also uncertainty

# Favour dist
parameters <- parameters |>
  mutate(unc_var_type = coalesce(distribution_var,
                               unc_var_type,
                               NA)
  )

parameters$unc_var_type <- str_replace_all(parameters$unc_var_type,
                                           unc_replacements)

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

#
parameters$population_sample_type <- gsub("Trade / business","Trade/business",parameters$population_sample_type)
parameters$population_sample_type <- gsub("\\s.*", "", parameters$population_sample_type) # keep the first word only

parameters$population_group <- str_to_title(parameters$population_group)

parameters$method_disaggregated_by <- gsub("Disease generation","Disease Generation",parameters$method_disaggregated_by)
parameters$method_disaggregated_by <- gsub("Level of exposure","Level of Exposure",parameters$method_disaggregated_by)

# CFR
# Copy population sample if CFR is na
parameters$cfr_ifr_denominator[is.na(parameters$cfr_ifr_denominator)] <-
  parameters$population_sample_size[is.na(parameters$cfr_ifr_denominator)]

# Replace all na with ""
parameters <- parameters |>
  mutate_all(~ ifelse(is.na(.), "", .))

#
parameters <- parameters |>
  mutate(across(where(is.character), ~ gsub(",", ";", .)))

# *------------------------------- Transmission -------------------------------*
# Check what should still be added
# TODO: variability
trns_params <- parameters |>
  filter(grepl("Attack|Relative contribution|Growth rate|Reproduction|Mutations|Overdispersion", parameter_type, ignore.case = TRUE)) |>
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
  "Mutations - " = ""
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

hdel_params$parameter_type <- sub("Symptom Onset", "Onset", hdel_params$parameter_type)
hdel_params$parameter_type <- sub("Admission To Care", "Admission", hdel_params$parameter_type)
hdel_params$parameter_type <- sub("Discharge/Recovery", "Recovery", hdel_params$parameter_type)

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
  filter(grepl("Severity", parameter_type, ignore.case = TRUE)) |>
  mutate(parameter_type = ifelse(parameter_type == 'Severity - case fatality rate (CFR)', 'Case fatality ratio',
                                 ifelse(parameter_type == 'Severity - proportion of symptomatic cases',
                                        "Proportion of symptomatic cases", parameter_type))) |>
  select(parameter_type, parameter_value, unc_type,
         parameter_2_value, unc_var_type,
         method_disaggregated_by,
         cfr_ifr_method, cfr_ifr_numerator,cfr_ifr_denominator,
         population_country, dates,
         population_sample_type, population_group, refs, central, id)

cfrs_params$population_country <- gsub(";", "\\, ", cfrs_params$population_country)

cfrs_params <- cfrs_params |> arrange(parameter_type,
                                       tolower(population_country),
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
cfrs_params <- insert_blank_rows(cfrs_params,"parameter_type")

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
sero_params <- sero_params |> arrange(tolower(population_country), parameter_type, as.numeric(central))

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

risk_params <- risk_params[order(risk_params$riskfactor_outcome,risk_params$riskfactor_significant,
                                 risk_params$riskfactor_name,risk_params$riskfactor_adjusted),]
risk_params$riskfactor_significant <- as.character(risk_params$riskfactor_significant)

# Cleaning table
create_cleaning_table(risk_params, "cleaning_riskfactors.csv", articles)

risk_params <- risk_params |>
  select(-id)

# Latex table
risk_params <- insert_blank_rows(risk_params,"riskfactor_outcome")
write.table(risk_params, file = "latex_riskfactors.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)
