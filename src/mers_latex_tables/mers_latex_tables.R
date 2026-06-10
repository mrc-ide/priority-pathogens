library(dplyr)
library(orderly)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

# *============================= Helper functions =============================*
create_cleaning_table <- function(data_table, save_name, article_table){
  article_table <- article_table |>
    select(c("id", "article_id", "covidence_id", "name_data_entry",
             "article_title", "doi"))
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
  c("articles.csv", "models.csv", "params.csv"))

orderly_shared_resource("mers_functions.R"="mers_functions.R")

source("mers_functions.R")

orderly_artefact(
  description="Merged single and double extracted data as csv and rds files",
  c("latex_models.csv", "latex_delays.csv", "latex_ifr.csv",
    "latex_riskfactors.csv", "latex_seroprevalence.csv", "latex_severity.csv",
    "latex_transmission.csv", "cleaning_models.csv", "cleaning_delays.csv",
    "cleaning_riskfactors.csv", "cleaning_ifr.csv",
    "cleaning_seroprevalence.csv", "cleaning_severity.csv",
    "cleaning_transmission.csv")
)
# *------------------------------- Read in data -------------------------------*
articles <- read_csv("articles.csv")
models <- read_csv("models.csv")
outbreaks <- tibble()
parameters <- read_csv("params.csv")

# TODO: Check data_curation
dfs <- data_curation(articles, outbreaks,models,parameters, plotting = FALSE)

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
         model_notes,
         refs,
         id,
         access_model_id)

# Replace all commas with semicolons
mods <- mods |>
  mutate(across(where(is.character), ~ gsub(",", ";", .)))

mods$model_type  <- paste(mods$model_type, mods$stoch_deter, sep = " - ")

# Substitutions
model_type_replacements <- c("branching process - stochastic"="branching process",
                             "branching process - na" = "branching process",
                             "unspecified - na" = "unspecified",
                             " model"="")

transmission_route_replacements <- c("human to human \\(direct contact\\)"="Human-human",
                                     "vector/animal"="animal",
                                     "animal to human"="animal-human",
                                     "unspecified"="",
                                     "^;|;$"="")

assumptions_replacements <- c(
  "homogeneous mixing"="",
  "other"="",
  "unspecified"="",
  # Possible leading or trailing semi colons based on the above
  "^;|;$"="",
  "heterogenity in transmission rates - over time"="time",
  "heterogenity in transmission rates - between human groups"="groups",
  "heterogenity in transmission rates - between human and vector"="animal",
  "age dependent susceptibility"="age")

compartmental_type_replacements <- c("not compartmental"="",
                                     "other compartmental; please specify"="other")

interventions_type_replacements <- c("unspecified"="",
                                     "vector/animal"="animal")

theoretical_model_replacements <- c(
  "yes"="theoretical",
  "no"="fitted"
)

mod_value_replacements <- list(
  "model_type"=model_type_replacements,
  "transmission_route"=transmission_route_replacements,
  "assumptions"=assumptions_replacements,
  "compartmental_type"=compartmental_type_replacements,
  "interventions_type"=interventions_type_replacements,
  "theoretical_model"=theoretical_model_replacements
)

# Replacements from above
mods[names(mod_value_replacements)] <- lapply(
  names(mod_value_replacements),
  function(name) str_replace_all(tolower(mods[[name]]),
                                 mod_value_replacements[[name]])
)

# Sentence case
mods[names(mod_value_replacements)] <- lapply(
  names(mod_value_replacements),
  function(name) str_to_sentence(mods[[name]]))

# Assuming only NA and compartmental models if not Other or ""
# Names were likely in correct format before tolower above
# when doing replacements...
mods["compartmental_type"] <- sapply(
  mods["compartmental_type"], function(val) ifelse(!val %in% c("Other", ""),
                                                   toupper(val), val))

# na replacements
# Named replacement
# Single col but vectorised so that additiomods["compartmental_type"]nal columns can be added if necessary
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
            access_model_id,
            model_language,
            model_readme,
            code_available,
            model_fitting_method,
            theoretical_model,
            stoch_deter,
            model_notes,
            model_compartmental_other)
  )

# Latex table
mods <- insert_blank_rows(mods,"model_type")
mods <- mods |>
  mutate(transmission_route = str_replace(
    transmission_route,
    "\\\\bfseries",
    "\\\\rowcolor{imperial_cool_grey!50}\\\\bfseries")
  )
write.table(mods, file = "latex_models.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)


# *================================ Parameters ================================*
# *----------------------------- General cleaning -----------------------------*
# Parameters not mapped:
# parameter_paired
# parameter_context_location_type
parameters <- dfs$parameters
# Recode some of the "other" human delays:
# First, recode:
recode_delays <- c(
  "Admission" = "Admission to care",
  "Admission to hospital" = "Admission to care",
  "Symptom Onset/Fever" = "Symptom onset",
  "Onset" = "Symptom onset",
  "Admission to ICU" = "Admission to Critical Care/ICU",
  "symptom onset" = "Symptom onset",
  "Symptom Onset" = "Symptom onset",
  "Days from symptom onset to intubation, median (Q1, Q3)" = "Symptom onset",
  "Days from the onset of symptoms to the emergency room, median (Q1, Q3)" = "Symptom onset",
  "time to the emergency room" = "Admission to Critical Care/ICU",
  "Diagnosis" = "Diagnosis/test result",
  "Disease onset" = "Symptom onset",
  "First positive RT-PCR test" = "Diagnosis/test result",
  "Hospital admission" = "Admission to care",
  "Hospitalisation" = "Admission to care",
  "Hospitalization" = "Admission to care",
  "case notification" = "Diagnosis/test result",
  "Lab confirmation" = "Diagnosis/test result",
  "Lab confirmation (WHO definition)" = "Diagnosis/test result",
  "Laboratory confirmation" = "Diagnosis/test result",
  "ICU Admission" = "Admission to Critical Care/ICU",
  "ICU admission" = "Admission to Critical Care/ICU",
  "MERS confirmation/test" = "Diagnosis/test result",
  "Onset of illness" = "Symptom onset",
  "Onset of symptoms" = "Symptom onset",
  "Onset of Symptoms" = "Symptom onset",
  "emergency room" = "Admission to Critical Care/ICU",
  "Emergency room" = "Admission to Critical Care/ICU",
  "intubation" = "Intubation",
  "confirmation" = "Diagnosis/test result",
  "Isolation unit" = "Isolation",
  "Lab confirmation" = "Diagnosis/test result",
  "Negative PCR Test (Sputum)" = "Negative test",
  "Negative PCR" = "Negative test",
  "negative swab" = "Negative test",
  "peak viral load" = "Peak viral load",
  "Virus detection" = "Diagnosis/test result",
  "Symptom Onset while in Hospital" = "Symptom onset",
  "Symtom onset" = "Symptom onset",
  "the first PCR-positive result" = "Diagnosis/test result",
  "Time intubated" = "Intubation",
  "unspecified" = "Unspecified",
  "NA" = "Unspecified",
  "initiation of mechanical ventilation" = "Start of mechanical ventilation",
  "Mechanical ventilator start" = "Start of mechanical ventilation",
  "Ventilation support" = "Start of mechanical ventilation",
  "Oxygen supplementation" = "Start of mechanical ventilation",
  "Onset of ventilation" = "Start of mechanical ventilation",
  "Time to RNA clearance" = "viral RNA clearance",
  "End mechanical ventilation" = "End of mechanical ventilation",
  "Case observation" = "Diagnosis/test result",
  "symtpom onset" = "Symptom onset",
  "time to pneumonia" = "pneumonia development",
  "mechanical ventilator end" = "End of mechanical ventilation",
  "Other human delay (go to section)" = "Unspecified",
  "Days from symptom onset to ICU admission, median (Q1, Q3)" = "Symptom onset",
  "Begin mechanical ventilation" = "Start of mechanical ventilation",
  "taking a sample which showed a negative result with RT-PCR for MERS-CoV" = "Negative test",
  "Positive conversion of immunofluorescent antibody (IFA) titre (â‰¥1:640) for MERS-CoV" = "Positive IFA",
  "Mechanical ventilator end" = "End of mechanical ventilation",
  "End of ICU stay" = "Discharge from Critical Care/ICU",
  "ICU Discharge/Death" = "Discharge from Critical Care/ICU",
  "Time from entering ICU" = "Admission to Critical Care/ICU",
  "Time to leaving ICU either by discharge or death" = "Time leaving ICU by discharge or death",
  "Illness onset" = "Symptom onset",
  "hospital admission" = "Admission to care",
  "Mechanical Ventilation" = "Start of mechanical ventilation",
  "Outcome (recovery or death)" = "Death or discharge",
  "Notification to WHO" = "Case notification to WHO"
)

parameters <- parameters %>%
  mutate(
    other_delay_start = recode(other_delay_start, !!!recode_delays),
    other_delay_end   = recode(other_delay_end, !!!recode_delays)
  )

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
  "max. nr. of cases superspreading \\(related to case\\)"="mnc"
  #"unspecified" = ""
  )

unc_replacements <- c(
  " \\(paired\\)" = "",
  " \\(paired or unpaired\\)" = "",
  "Inter Quartile Range \\(IQR\\)" = "IQR",
  "Standard Error" = "SE",
  "Standard Deviation" = "SD",
  "Standard deviation \\(Sd\\)" = "SD",
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

  # Clean parameter unit
  # Ignores central - but presumably fine since presumably any case where
  # central is used the unit is consistent (e.g. CFR)
  non_blank_unit_filter <- (is.na(parameters[[unit_col]]) &
                              parameters[[value_col]]!="NA")
  parameters[[unit_col]][non_blank_unit_filter] <- "Unspecified"
  parameters[[unit_col]] <- tolower(parameters[[unit_col]])

  # Unit replacements
  parameters[[unit_col]] <- str_replace_all(parameters[[unit_col]],
                                            p_unit_replacements)

  # Unit replacement based on exponents, assuming no units provided and not R
  unit_r_condition <- (parameters[[unit_col]] == "" &
                         parameters[["parameter_class"]] != "Reproduction number")

  # Exponents
  # 5.6 exponent? Fixed (changed d to f -> for float exponents)
  parameters[[unit_col]] <- case_when(
    is.na(parameters[[exp_col]]) | parameters[[exp_col]] == 0 ~ parameters[[unit_col]],
    parameters[[exp_col]] == -2 & unit_r_condition ~ "\\%",
    parameters[[exp_col]] == -3 & unit_r_condition ~ "per 1000",
    parameters[[exp_col]] == -4 & unit_r_condition ~ "per 10k",
    parameters[[exp_col]] == -5 & unit_r_condition ~ "per 100k",
    TRUE ~ paste0(
      parameters[[unit_col]], " ",
      "10^{", sub("\\.?0+$", "", sprintf("%.5f", parameters[[exp_col]])), "}"
    )
  )

  parameters[[value_col]] <- coalesce(
    na_if(parameters[[value_col]], "NA"), # coalesce only works with NA, so convert string
    parameters[[value_bounds_col]],
    "")

  # Adding units to values
  parameters[[unit_col]] <- na_if(parameters[[unit_col]], "NA")
  parameters[[unit_col]] <- replace_na(parameters[[unit_col]], "")

  parameters[[value_col]] <- ifelse(
    parameters[[unit_col]] == "" | parameters[[unit_col]] == "Unspecified",
    parameters[[value_col]],
    paste(parameters[[value_col]], parameters[[unit_col]], sep = " ")
  )

  if (id=="_2"){
    parameters[["parameter_2_value_type"]] <- str_replace_all(
      parameters[["parameter_2_value_type"]], unc_replacements)

    # Variability needs a type
    parameters[[value_col]] <-  ifelse(
      parameters[["parameter_2_value_type"]] %in% c("Unspecified","", NA),
      "",
      paste0(parameters[["parameter_2_value_type"]], ": ",
             parameters[[value_col]])
    )
  }

  # Loop across dist par 1 and 2 for params 1 and 2
  # Add a superscript if distribution uncertainty is available
  for (j in 1:2){
    dist_par_type  <- paste0("distribution", id, "_par",j,"_type")
    dist_par_unc  <- paste0("distribution", id, "_par",j,"_uncertainty")
    # quick fix for SD
    parameters[[dist_par_type]] <- str_replace(parameters[[dist_par_type]],
                                               "Standard deviation", "SD")

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
                                              " - ")

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

  parameters[[unc_type]] <- str_replace_all(parameters[[unc_type]],
                                            unc_replacements)

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
}

# Dates
parameters <- parameters |>
  mutate(population_study_start_day = as.numeric(gsub("x+", "", population_study_start_day)),
         population_study_start_month = as.numeric(gsub("x+", "", population_study_start_month)),
         population_study_start_year = as.numeric(gsub("x+", "", population_study_start_year)),
         population_study_end_day = as.numeric(gsub("x+", "", population_study_end_day)),
         population_study_end_month = as.numeric(gsub("x+", "", population_study_end_month)),
         population_study_end_year = as.numeric(gsub("x+", "", population_study_end_year)),
         start_month_abbr = ifelse(!is.na(population_study_start_month),
                                   month.abb[population_study_start_month],
                                   NA),
         start_date = paste(population_study_start_day,
                            start_month_abbr,
                            population_study_start_year),
         start_date=na_if(start_date, "NA NA NA"),
         end_month_abbr = ifelse(!is.na(population_study_end_month),
                                 month.abb[population_study_end_month],
                                 NA),
         end_date = paste(population_study_end_day,
                          end_month_abbr,
                          population_study_end_year),
         end_date=na_if(end_date, "NA NA NA"),
         dates = case_when(
           start_date==end_date~start_date,
           !is.na(start_date) & is.na(end_date)~paste0(start_date, " - Unspecified"),
           is.na(start_date) & !is.na(end_date)~paste0("Unspecified - ", end_date),
           !is.na(start_date) & !is.na(end_date) ~ paste0(start_date, " - ", end_date),
           TRUE ~ "Unspecified"),
         dates = gsub("NA","",dates),
         dates = trimws(gsub("\\s{2,}"," ",dates)),
         dates = gsub("\\b(\\d{4})\\s*-\\s*\\1\\b","\\1",dates,perl = TRUE))


# Replacements
population_sample_type_replacements <- c(
  "Trade / business"="Trade/business",
  "\\s.*"=""# keep the first word only
)

# Decide if sentence case, done separately because of Level 'of' Exposure case
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

parameters <- parameters |>
  mutate(across(c(population_group, population_sample_type),
                ~ replace_na(na_if(.x, ""), "Unspecified"))
  )

parameters$population_group <- str_to_title(parameters$population_group)

# CFR and Serology
# Copy population sample if CFR is na
# CHECK MOVE
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
# No variability
trns_params <- parameters |>
  filter(
    grepl(paste0("Attack|Relative contribution|Growth rate|Reproduction|",
                 "Mutations|Overdispersion|proportion of symptomatic cases|proportion of asymptomatic cases|symptomatic proportion|asymptomatic proportion"),
          parameter_type, ignore.case = TRUE)) |>
  select(parameter_type, parameter_value, unc_type,
         method_disaggregated_by,
         genome_site, method_r,
         population_sample_size,
         population_country, dates,
         population_sample_type,
         population_group,
         refs,
         central,
         id,
         access_param_id)

trns_params_pt_replacements <- c(
  "Reproduction number \\(Basic R0\\)" = "Reproduction number R0",
  "Severity - proportion of symptomatic cases" = "Proportion of symptomatic cases",
  "Severity - symptomatic proportion of infections" = "Proportion of symptomatic cases",
  "Severity - proportion of asymptomatic cases" = "Proportion of asymptomatic cases",
  "Severity - asymptomatic proportion of infections" = "Proportion of asymptomatic cases",
  "Mutations - evolutionary rate" = "Evolutionary rate",
  "Mutations - substitution rate" = "Substitution rate",
  # Only primary attack rate
  "Attack rate" = "Primary attack rate")

trns_params$parameter_type <- str_replace_all(trns_params$parameter_type,
                                              trns_params_pt_replacements)

trns_params$method_r <- str_to_sentence(trns_params$method_r)

trns_params |> count(parameter_type)
trns_params$parameter_type <- factor(
  trns_params$parameter_type,
  levels = c("Reproduction number R0",
             "Reproduction number R0 - Human",
             "Reproduction number (Effective; Re)",
             "Reproduction number (Effective; Re) - Human",
             "Overdispersion",
             "Growth rate (r)",
             "Primary attack rate",
             "Secondary attack rate",
             "Proportion of symptomatic cases",
             "Proportion of asymptomatic cases",
             "Relative contribution - human to human",
             "Relative contribution - zoonotic to human",
             "Evolutionary rate",
             "Substitution rate"))

trns_params <- trns_params[order(trns_params$parameter_type,
                                 as.numeric(trns_params$central)),]

trns_params <- trns_params |>
  select(-c(central))

# Don't need "unspecified" for reproduction numbers.
trns_params <- trns_params %>%
  mutate(
    parameter_value = if_else(
      str_detect(parameter_type, "Reproduction number"),
      str_remove(parameter_value, " unspecified"),
      parameter_value
    )
  )

# Cleaning table
create_cleaning_table(trns_params, "cleaning_transmission.csv", articles)

trns_params <- trns_params |>
  select(-c(id, access_param_id))

# Latex table
trns_params <- insert_blank_rows(trns_params,"parameter_type")


trns_params <- trns_params |>
  mutate(parameter_value = str_replace(
    parameter_value,
    "\\\\bfseries",
    "\\\\rowcolor{imperial_cool_grey!50}\\\\bfseries")
  )

## Fix issue where a solo % messes up the LaTeX:
trns_params$genome_site[trns_params$genome_site == "genome coverage of over 30%"] <- "genome coverage of over 30\\%"

write.table(trns_params, file = "latex_transmission.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)
# *------------------------------- Human delays -------------------------------*
hdel_params <- parameters |>
  filter(grepl("delay", parameter_type, ignore.case = TRUE)) |>
  select(parameter_type, other_delay_start, other_delay_end,
         parameter_value, parameter_value_type, unc_type,
         parameter_2_value,
         method_disaggregated_by,
         population_sample_size,
         population_country, dates,
         population_sample_type, population_group, refs, central, id,
         access_param_id)

hdel_params$parameter_type <- sub("^.* - ", "", hdel_params$parameter_type)
hdel_params$parameter_type <- sub(">", " - ", hdel_params$parameter_type)
hdel_params$parameter_type <- str_to_title(hdel_params$parameter_type)
hdel_params$parameter_type <- gsub("  \\(.*?\\)", "", hdel_params$parameter_type)

# These were specific to Nipah and don't necessarily to relate to MERS
hdel_params <- hdel_params |> mutate(parameter_type = case_when(
  (other_delay_start =="symptom onset" & other_delay_end == "confirmation") ~ "Onset - confirmation", #(or Symptom onset>diagnosis/test result)
  (other_delay_start =="symptom onset" & other_delay_end == "notification") ~ "Onset - notification",
  (other_delay_start =="Infectiousness" & other_delay_end == "Illness onset") ~ "Infectiousness - illness onset",
  (other_delay_start =="Illness onset" & other_delay_end == "Diagnosis/test result") ~ "Onset - Diagnosis/test result",
  (other_delay_start =="Onset" & other_delay_end == "Diagnosis/test result") ~ "Onset - Diagnosis/test result",
  (other_delay_start =="Admission to hospital" & other_delay_end == "Admission to Critical Care/ICU") ~ "Admission To Care - Admission to Critical Care/ICU",
  (other_delay_start =="Admission to ICU" & other_delay_end == "Discharge from Critical Care/ICU") ~ "Admission to ICU - Discharge from Critical Care/ICU",
  (other_delay_start =="Ventilation start" & other_delay_end == "Ventilation end") ~ "Time in mechanical ventilation",
  (other_delay_start =="Admission to ICU" & other_delay_end == "Death") ~ "Admission to ICU - Death",
  #Nipah ones...
  (other_delay_start =="Start of severe illness" & other_delay_end == "End of severe illness (recovery or death)") ~ "Duration of severe illness",
  (other_delay_start =="Begin ventilation" & other_delay_end == "End ventilation") ~ "Duration of ventilation",
  (other_delay_start =="Onset of ventilation" & other_delay_end == "End of ventilation") ~ "Duration of ventilation",
  (other_delay_start =="First neurological episode" & other_delay_end == "Second neurological episode") ~ "Duration between initial neurological episodes",
  (other_delay_start =="Exposure/Infection" & other_delay_end == "Neurological episode") ~ "Exposure - neurological episode",
  TRUE ~ parameter_type))


#This for now will redefine the "other" human delays
##TODO: Might want to consider something cleaner here, like manually re-writing as above
 hdel_params <- hdel_params |> mutate(
   other = paste(other_delay_start, ":", other_delay_end),
   parameter_type = coalesce(na_if(other, " : "),
                             parameter_type)
   )

 hdel_params <- hdel_params |>
   select(-c(other_delay_start,
             other_delay_end,
             other)
          )

#hdel_params <- hdel_params |>
#  select(-c(other_delay_start,
#            other_delay_end))

# "Discharge/recovery"="Recovery" <- Not correct for current Discharge/recovery delay?
parameter_type_replacement <- c("Symptom Onset"="Onset",
                                "Admission To Care"="Admission")
hdel_params$parameter_type <- str_replace_all(hdel_params$parameter_type,
                                              parameter_type_replacement)

hdel_params$parameter_type <- str_to_sentence(hdel_params$parameter_type)

hdel_params$parameter_value_type[hdel_params$parameter_value_type==""] <- "Unspecified"


hdel_params |>
  distinct(parameter_type) |> print(n=63)

hdel_params$parameter_type <- factor(
  hdel_params$parameter_type,
  levels = c("Incubation period",
             "Infectious period",
             "Onset - admission",
             "Onset - death",
             #"Onset - recovery/death",
             "Onset - discharge/recovery",
             "Admission - death",
             "Admission - discharge/recovery",
             "Time in care (length of stay)",
             "Serial interval",
             "Generation time",
             "Admission to care : symptom onset",
             "Admission to care : diagnosis/test result",
             "Admission to care : laboratory confirmation (who definition)",
             "Admission to care : admission to critical care/icu",
             "Admission to care : corticosteroid initiation",
             "Admission to care : isolation",
             "Admission to care : defervescence",
             "Admission to care : antiviral therapy",
             "Admission to care : death or discharge",
             "Admission to critical care/icu : discharge from critical care/icu",
             "Admission to critical care/icu : corticosteroid initiation",
             "Admission to critical care/icu : death",
             "Admission to critical care/icu : time leaving icu by discharge or death",
             "Admission to critical care/icu : viral clearance",
             "Ventilation start : ventilation end",
             "Start of mechanical ventilation : end of mechanical ventilation",
             "Start of mechanical ventilation : corticosteroid initiation",
             "Infectiousness : symptom onset",
             "Isolation : end of isolation",
             "Isolation : discharge from care/hospital",
             "Symptom onset : diagnosis/test result",
             "Symptom onset : test taken",
             "Symptom onset : negative test",
             "Symptom onset : positive ifa",
             "Symptom onset : peak viral load",
             "Symptom onset : notification",
             "Symptom onset : case reporting",
             "Symptom onset : case notification to who",
             "Symptom onset : isolation",
             "Symptom onset : intubation",
             "Symptom onset : pneumonia development",
             "Symptom onset : seeking care",
             "Symptom onset : admission to care/hospitalisation",
             "Symptom onset : admission to critical care/icu",
             "Symptom onset : corticosteroid initiation",
             "Symptom onset : initiation of antiviral treatment",
             "Symptom onset : start of mechanical ventilation",
             "Symptom onset : invasive mechanical ventilation",
             "Symptom onset : death or discharge",
             "Symptom onset : recovery/non-infectiousness",
             "Diagnosis/test result : reporting by official entities",
             "Diagnosis/test result : viral rna clearance",
             "Diagnosis/test result : have detectable mers-cov antibodies",
             "Diagnosis/test result : discharge from care/hospital",
             "Diagnosis/test result : death",
             "Viral rna detected : viral rna clearance",
             "Recovery : test taken",
             "Intubation : time extubated",
             "Infection : recovery/non-infectiousness",
             "Unspecified : symptom resolution"))

hdel_params <- hdel_params[order(hdel_params$parameter_type,
                                 as.numeric(hdel_params$central),
                                 hdel_params$population_country),]

hdel_params <- hdel_params |>
  select(-c(central))

# Cleaning table
create_cleaning_table(hdel_params, "cleaning_delays.csv", articles)

hdel_params <- hdel_params |>
  select(-c(id, access_param_id))

# Latex table
hdel_params <- insert_blank_rows(hdel_params, "parameter_type")

hdel_params <- hdel_params |>
  mutate(parameter_value = str_replace(
    parameter_value,
    "\\\\bfseries",
    "\\\\rowcolor{imperial_cool_grey!50}\\\\bfseries")
  )

write.table(hdel_params, file = "latex_delays.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# *----------------------------------- CFR ------------------------------------*
cfrs_params <- parameters |>
  filter(grepl("(CFR)", parameter_type, ignore.case = TRUE)) |>
  mutate(parameter_type = ifelse(
    parameter_type == 'Severity - case fatality rate (CFR)',
    'Case fatality ratio',
    parameter_type)) |>
  select(parameter_value, unc_type,
         parameter_2_value, unc_var_type,
         method_disaggregated_by,
         cfr_ifr_method, cfr_ifr_numerator,cfr_ifr_denominator,
         population_country, dates,
         population_sample_type, population_group,
         parameter_notes, refs, central, id, access_param_id)

cfrs_params[, c("parameter_value", "unc_type")] <- sapply(
  c("parameter_value", "unc_type"),
  function(col) gsub(" \\\\%", "", cfrs_params[[col]]))

cfrs_params$population_country <- gsub(";", "\\, ",
                                       cfrs_params$population_country)

cfrs_params$cfr_ifr_method[cfrs_params$cfr_ifr_method==""] <- "Unspecified"
cfrs_params$population_country[cfrs_params$population_country==""] <- "Unspecified"

cfrs_params <- cfrs_params |>
  mutate(# Add central to parameter_value with * and format all to 1 decimal
    parameter_value=str_replace_all(parameter_value, " unspecified", ""),
    # param_sort = coalesce(ifelse(parameter_value=="", NA, parameter_value),
    #                       central),

    #Convert to 1dp
    parameter_value = case_when(
      parameter_value == "" & central != "" ~ paste0(
        sprintf("%.1f", as.numeric(central)), "$^*$"
      ),
      parameter_value == "" ~ "",
      TRUE ~ str_replace_all(
        parameter_value,
        "(\\d+(?:\\.\\d+)?)",
        function(x) sprintf("%.1f", as.numeric(x))
      )
    ),




    # CI to 1 decimal
    unc_type=str_replace_all(unc_type,
                             "(\\d+(?:\\.\\d+)?)",
                             function(x) sprintf("%.1f", as.numeric(x)))
  )



cfrs_params <- cfrs_params |>
  arrange(tolower(population_country),
          desc(as.numeric(central)))

cfrs_params |>
  filter(parameter_2_value != "" | unc_var_type != "") |>
  NROW()

cfrs_params <- cfrs_params |> select(-c(parameter_2_value,
                                        unc_var_type))

# Cleaning table
create_cleaning_table(cfrs_params, "cleaning_severity.csv", articles)

cfrs_params <- cfrs_params |>
  select(-c(parameter_notes, id, access_param_id, central))

# Latex table
cfrs_params <- insert_blank_rows(cfrs_params, "population_country")
cfrs_params <- cfrs_params |>
  mutate(parameter_value = str_replace(
    parameter_value,
    "\\\\bfseries",
    "\\\\rowcolor{imperial_cool_grey!50}\\\\bfseries")
  )

## Remove unneccessary * from the distribution column
cfrs_params$unc_type <- gsub("\\$\\^\\*\\$", "", cfrs_params$unc_type)

write.table(cfrs_params, file = "latex_severity.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# *----------------------------------- IFR ------------------------------------*
ifrs_params <- parameters |>
  filter(grepl("(IFR)", parameter_type, ignore.case = TRUE)) |>
  mutate(parameter_type = ifelse(
    parameter_type == 'Severity - infection fatality rate (IFR)',
    'Infection fatality ratio',
    parameter_type)) |>
  select(parameter_value, unc_type,
         parameter_2_value, unc_var_type,
         method_disaggregated_by,
         cfr_ifr_method, cfr_ifr_numerator,cfr_ifr_denominator,
         population_country, dates,
         population_sample_type, population_group,
         parameter_notes, refs, central, id, access_param_id)

ifrs_params[, c("parameter_value", "unc_type")] <- sapply(
  c("parameter_value", "unc_type"),
  function(col) gsub(" \\\\%", "", ifrs_params[[col]]))

ifrs_params$population_country <- gsub(";", "\\, ",
                                       ifrs_params$population_country)

ifrs_params$cfr_ifr_method[ifrs_params$cfr_ifr_method==""] <- "Unspecified"
ifrs_params$population_country[ifrs_params$population_country==""] <- "Unspecified"

ifrs_params <- ifrs_params |>
  mutate(# Add central to parameter_value with * and format all to 1 decimal
    parameter_value=str_replace_all(parameter_value, " unspecified", ""),
    # param_sort = coalesce(ifelse(parameter_value=="", NA, parameter_value),
    #                       central),

    #Convert to 1dp
    parameter_value = case_when(
      parameter_value == "" & central != "" ~ paste0(
        sprintf("%.1f", as.numeric(central)), "$^*$"
      ),
      parameter_value == "" ~ "",
      TRUE ~ str_replace_all(
        parameter_value,
        "(\\d+(?:\\.\\d+)?)",
        function(x) sprintf("%.1f", as.numeric(x))
      )
    ),




    # CI to 1 decimal
    unc_type=str_replace_all(unc_type,
                             "(\\d+(?:\\.\\d+)?)",
                             function(x) sprintf("%.1f", as.numeric(x)))
  )



ifrs_params <- ifrs_params |>
  arrange(tolower(population_country),
          desc(as.numeric(central)))

ifrs_params |>
  filter(parameter_2_value != "" | unc_var_type != "") |>
  NROW()

ifrs_params <- ifrs_params |> select(-c(parameter_2_value,
                                        unc_var_type))

# Cleaning table
create_cleaning_table(ifrs_params, "cleaning_ifr.csv", articles)

ifrs_params <- ifrs_params |>
  select(-c(parameter_notes, id, access_param_id, central))

# Latex table
ifrs_params <- insert_blank_rows(ifrs_params, "population_country")
ifrs_params <- ifrs_params |>
  mutate(parameter_value = str_replace(
    parameter_value,
    "\\\\bfseries",
    "\\\\rowcolor{imperial_cool_grey!50}\\\\bfseries")
  )

write.table(ifrs_params, file = "latex_ifr.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# *--------------------------------- Serology ---------------------------------*
sero_params <- parameters |>
  filter(grepl("Seroprevalence", parameter_type, ignore.case = TRUE)) |>
  select(parameter_value, unc_type,
         parameter_2_value, unc_var_type,
         method_disaggregated_by,
         parameter_type,cfr_ifr_numerator,cfr_ifr_denominator,
         population_country, dates,
         population_sample_type, population_group,
         parameter_notes, refs, central, id, access_param_id)

sero_params  <- sero_params |>
  mutate(in_CSF = case_when(str_detect(parameter_notes,'CSF')~TRUE,
                            TRUE~FALSE))

sero_params[, c("parameter_value", "unc_type")] <- sapply(
  c("parameter_value", "unc_type"),
  function(col) gsub(" \\\\%", "", sero_params[[col]]))

# Get rid of Seroprevalence -
sero_params$parameter_type <- sub("^.* - ", "", sero_params$parameter_type)

sero_params <- sero_params |>
  mutate(# Add central to parameter_value with * and format all to 1 decimal
    parameter_value=str_replace_all(parameter_value, " unspecified", ""),
    parameter_value=case_when(
      parameter_value=="" & central!=""~paste0(
        sprintf("%.1f", as.numeric(central)), "$^*$"),
      parameter_value=="" ~ "",
      # Format this way since there are ranges
      TRUE~ str_replace_all(parameter_value,
                            "(\\d+(?:\\.\\d+)?)",
                            function(x) sprintf("%.1f", as.numeric(x)))),
    parameter_value=ifelse(in_CSF==TRUE,
                           paste0(parameter_value, "$^+$"),
                           parameter_value),
    # CI to 1 decimal
    unc_type=str_replace_all(unc_type,
                             "(\\d+(?:\\.\\d+)?)",
                             function(x) sprintf("%.1f", as.numeric(x)))
  )

sero_params <- sero_params |>
  arrange(tolower(population_country),
          parameter_type,
          desc(as.numeric(central)))

# No variability for sero
sero_params |>
  filter(parameter_2_value != "" | unc_var_type != "") |>
  NROW()

sero_params <- sero_params |>
  select(-c(parameter_2_value, unc_var_type))

# Cleaning table
create_cleaning_table(sero_params, "cleaning_seroprevalence.csv", articles)

sero_params <- sero_params |>
  select(-c(parameter_notes, id, access_param_id, central))

# Latex table
sero_params <- insert_blank_rows(sero_params,"population_country")

sero_params <- sero_params |>
  mutate(parameter_value = str_replace(
    parameter_value,
    "\\\\bfseries",
    "\\\\rowcolor{imperial_cool_grey!50}\\\\bfseries")
  )
write.table(sero_params, file = "latex_seroprevalence.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# *------------------------------- Risk factors -------------------------------*
risk_params <- parameters |>
  filter(grepl("Risk factors", parameter_type, ignore.case = TRUE)) |>
  select(riskfactor_outcome,
         riskfactor_name,	riskfactor_significant,
         riskfactor_adjusted, population_sample_size,
         population_country, dates,
         population_sample_type, population_group,
         parameter_notes, refs, id, access_param_id)

risk_params |> count(riskfactor_outcome)

risk_params$riskfactor_outcome <- factor(risk_params$riskfactor_outcome,
                                         levels = c("Infection",
                                                    "Severe disease",
                                                    "Severe disease (in general population)",
                                                    "Serology",
                                                    "Death",
                                                    "Death (in general population)",
                                                    "Recovery",
                                                    "Other"),
                                         labels=c("Infection",
                                                  "Severe disease",
                                                  "Severe disease",
                                                  "Serology",
                                                  "Death",
                                                  "Death",
                                                  "Recovery",
                                                  "Other"))

risk_params$riskfactor_significant[risk_params$riskfactor_significant==""] <- "Unspecified"

risk_params$riskfactor_significant <- str_to_sentence(risk_params$riskfactor_significant)

risk_params$riskfactor_significant <- factor(risk_params$riskfactor_significant,
                                             levels = c("Significant","Not significant","Unspecified"))

risk_params$riskfactor_adjusted <- str_to_sentence(risk_params$riskfactor_adjusted)
risk_params$riskfactor_adjusted <- replace_na(
  na_if(risk_params$riskfactor_adjusted, ""), "Unspecified")
risk_params$riskfactor_adjusted <- factor(
  risk_params$riskfactor_adjusted,
  levels = c("Adjusted","Not adjusted","Unspecified"))

risk_params <- risk_params[
  order(
    risk_params$riskfactor_outcome,
    risk_params$riskfactor_significant,
    risk_params$riskfactor_adjusted,
    # Other last
    risk_params$riskfactor_name == "Other",
    risk_params$riskfactor_name,
    na.last = TRUE
  ),
]

# Do this before insert_blank_rows else factors are cast to numbers
risk_params$riskfactor_significant <- as.character(risk_params$riskfactor_significant)
risk_params$riskfactor_adjusted <- as.character(risk_params$riskfactor_adjusted)

# Cleaning table
create_cleaning_table(risk_params, "cleaning_riskfactors.csv", articles)

risk_params <- risk_params |>
  select(-c(parameter_notes, id, access_param_id))

# Latex table
risk_params <- insert_blank_rows(risk_params,"riskfactor_outcome")
risk_params <- risk_params |>
  mutate(riskfactor_name = str_replace(
    riskfactor_name,
    "\\\\bfseries",
    "\\\\rowcolor{imperial_cool_grey!50}\\\\bfseries")
  )

write.table(risk_params, file = "latex_riskfactors.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)
