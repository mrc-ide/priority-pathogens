## Risk Factors

library(dplyr)
library(orderly2)
library(readr)
library(ggplot2)
library(ggforce)
library(flextable)
library(officer)
library(purrr)
library(scales)
library(epitrix)
library(stringr)

orderly_strict_mode()

# Outputs
orderly_artefact(
  "Tables for risk factors",
  c(
    "Risk_tables/risk_factors_for_death.png",
    "Risk_tables/risk_factors_for_infection.png",
    "Risk_tables/risk_factors_for_serology.png",
    "Risk_tables/risk_factors_for_recovery.png",
    "Risk_tables/risk_factors_for_severe_disease.png",
    "Risk_tables/risk_factors_for_symptoms.png",
    "Risk_tables/other_risk_factor_outcomes.png"
  )
)

orderly_parameters(pathogen = "EBOLA")
# orderly_parameters(pathogen = NULL)

# Get data from db_compilation
orderly_dependency(
  "db_compilation",
  "latest(parameter:pathogen == this:pathogen)",
  c(
    "articles.csv",
    "parameters.csv"
  )
)

# Script with functions for plots/tables
orderly_shared_resource("ebola_functions.R" = "ebola_functions.R")
orderly_shared_resource("ebola_visualisation.R" = "ebola_visualisation.R")

# Load data
articles <- read_csv("articles.csv")
params <- read_csv("parameters.csv")
source("ebola_functions.R")
source("ebola_visualisation.R")

parameter <- "Risk factors"

df <- left_join(
  params,
  articles[, c(
    "covidence_id", "first_author_surname", "year_publication",
    "article_label", "doi", "notes"
  )],
  by = "covidence_id"
) %>%
  arrange(article_label, -year_publication)

species_levels <- factor(df$ebola_species, levels = c(sort(
  setdiff(unique(df$ebola_species), "Unspecified"),
  decreasing = TRUE
), "Unspecified"))

rf_dat <- df %>%
  mutate(
    population_country = as.factor(population_country)
  ) %>%
  filter(parameter_class %in% parameter) %>%
  filter(!parameter_from_figure %in% TRUE)

unique(rf_dat$riskfactor_outcome)
table(rf_dat$riskfactor_outcome, useNA = "ifany")

# Create directory for results
dir.create("Risk_tables")

####################################
# Risk factors for death (n = 136) #
####################################

death_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Death") %>%
  mutate(
    riskfactor_adjusted =
      case_when(
        is.na(riskfactor_adjusted) ~ "Unspecified",
        TRUE ~ riskfactor_adjusted
      )
  )

table(death_dat$riskfactor_name, useNA = "ifany")

# Total count of risk factors across significant/insignificant/unspecified
death_total_rf <- table(unlist(str_split(death_dat$riskfactor_name, ";")))

# Separate based on significance and adjustment
death_sig_adj <- data.frame(table(unlist(str_split(death_dat$riskfactor_name[death_dat$riskfactor_significant %in% "Significant" & death_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))
death_insig_adj <- data.frame(table(unlist(str_split(death_dat$riskfactor_name[death_dat$riskfactor_significant %in% "Not significant" & death_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))
death_unspec_adj <- data.frame(table(unlist(str_split(death_dat$riskfactor_name[death_dat$riskfactor_significant %in% "Unspecified" & death_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))
death_sig_unadj <- data.frame(table(unlist(str_split(death_dat$riskfactor_name[death_dat$riskfactor_significant %in% "Significant" & death_dat$riskfactor_adjusted %in% "Not adjusted"], ";"))))
death_insig_unadj <- data.frame(table(unlist(str_split(death_dat$riskfactor_name[death_dat$riskfactor_significant %in% "Not significant" & death_dat$riskfactor_adjusted %in% "Not adjusted"], ";"))))
death_unspec_unadj <- data.frame(table(unlist(str_split(death_dat$riskfactor_name[death_dat$riskfactor_significant %in% "Unspecified" & death_dat$riskfactor_adjusted %in% "Not adjusted"], ";"))))
death_sig_unspec <- data.frame(table(unlist(str_split(death_dat$riskfactor_name[death_dat$riskfactor_significant %in% "Significant" & death_dat$riskfactor_adjusted %in% "Unspecified"], ";"))))
death_insig_unspec <- data.frame(table(unlist(str_split(death_dat$riskfactor_name[death_dat$riskfactor_significant %in% "Not significant" & death_dat$riskfactor_adjusted %in% "Unspecified"], ";"))))
death_unspec_unspec <- data.frame(table(unlist(str_split(death_dat$riskfactor_name[death_dat$riskfactor_significant %in% "Unspecified" & death_dat$riskfactor_adjusted %in% "Unspecified"], ";"))))

deaths_comb <- bind_rows(
  "Significant_Adjusted" = death_sig_adj,
  "Insignificant_Adjusted" = death_insig_adj,
  "Unspecified_Adjusted" = death_unspec_adj,
  "Significant_Unadjusted" = death_sig_unadj,
  "Insignificant_Unadjusted" = death_insig_unadj,
  "Unspecified_Unadjusted" = death_unspec_unadj,
  "Significant_Unspecified" = death_sig_unspec,
  "Insignificant_Unspecified" = death_insig_unspec,
  "Unspecified_Unspecified" = death_unspec_unspec,
  .id = "Type"
)

deaths_comb <- separate(deaths_comb, col = "Type", sep = "_", into = c("Significance", "Adjustment"))

# Removing "Other" risk factors here (n = 110)
death_rf_ft <- deaths_comb %>%
  mutate(
    `Risk factor for death` = Var1,
    `Number of papers` = Freq,
    `Risk factor for death` = as.factor(`Risk factor for death`)
  ) %>%
  select(
    `Risk factor for death`, Significance, Adjustment, `Number of papers`,
    -Freq, -Var1
  ) %>%
  filter(!`Risk factor for death` %in% "Other") %>%
  arrange(`Risk factor for death`) %>%
  flextable() %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "right")

save_as_image(death_rf_ft, path = "Risk_tables/risk_factors_for_death.png")

#######################################
# Risk factors for infection (n = 61) #
#######################################

infection_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Infection")

table(infection_dat$riskfactor_name, useNA = "ifany")
# TO DO: Check the NA

# Total count of risk factors across significant/insignificant/unspecified
infection_total_rf <- table(unlist(str_split(infection_dat$riskfactor_name, ";")))

# Separate based on significance and adjustment
infection_sig_adj <- data.frame(table(unlist(str_split(infection_dat$riskfactor_name[infection_dat$riskfactor_significant %in% "Significant" & infection_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))
infection_insig_adj <- data.frame(table(unlist(str_split(infection_dat$riskfactor_name[infection_dat$riskfactor_significant %in% "Not significant" & infection_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))
infection_unspec_adj <- data.frame(table(unlist(str_split(infection_dat$riskfactor_name[infection_dat$riskfactor_significant %in% "Unspecified" & infection_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))
infection_sig_unadj <- data.frame(table(unlist(str_split(infection_dat$riskfactor_name[infection_dat$riskfactor_significant %in% "Significant" & infection_dat$riskfactor_adjusted %in% "Not adjusted"], ";"))))
infection_insig_unadj <- data.frame(table(unlist(str_split(infection_dat$riskfactor_name[infection_dat$riskfactor_significant %in% "Not significant" & infection_dat$riskfactor_adjusted %in% "Not adjusted"], ";"))))
infection_unspec_unadj <- data.frame(table(unlist(str_split(infection_dat$riskfactor_name[infection_dat$riskfactor_significant %in% "Unspecified" & infection_dat$riskfactor_adjusted %in% "Not adjusted"], ";"))))
infection_sig_unspec <- data.frame(table(unlist(str_split(infection_dat$riskfactor_name[infection_dat$riskfactor_significant %in% "Significant" & infection_dat$riskfactor_adjusted %in% "Unspecified"], ";"))))
infection_insig_unspec <- data.frame(table(unlist(str_split(infection_dat$riskfactor_name[infection_dat$riskfactor_significant %in% "Not significant" & infection_dat$riskfactor_adjusted %in% "Unspecified"], ";"))))
infection_unspec_unspec <- data.frame(table(unlist(str_split(infection_dat$riskfactor_name[infection_dat$riskfactor_significant %in% "Unspecified" & infection_dat$riskfactor_adjusted %in% "Unspecified"], ";"))))

infections_comb <- bind_rows(
  "Significant_Adjusted" = infection_sig_adj,
  "Insignificant_Adjusted" = infection_insig_adj,
  "Unspecified_Adjusted" = infection_unspec_adj,
  "Significant_Unadjusted" = infection_sig_unadj,
  "Insignificant_Unadjusted" = infection_insig_unadj,
  "Unspecified_Unadjusted" = infection_unspec_unadj,
  "Significant_Unspecified" = infection_sig_unspec,
  "Insignificant_Unspecified" = infection_insig_unspec,
  "Unspecified_Unspecified" = infection_unspec_unspec,
  .id = "Type"
)

infections_comb <- separate(infections_comb, col = "Type", sep = "_", into = c("Significance", "Adjustment"))

# Removing "Other" risk factors here (n = 36)
infection_rf_ft <- infections_comb %>%
  mutate(
    `Risk factor for infection` = Var1,
    `Number of papers` = Freq,
    `Risk factor for infection` = as.factor(`Risk factor for infection`)
  ) %>%
  select(
    `Risk factor for infection`, Significance, Adjustment, `Number of papers`,
    -Freq, -Var1
  ) %>%
  filter(!`Risk factor for infection` %in% "Other") %>%
  arrange(`Risk factor for infection`) %>%
  flextable() %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "right")

save_as_image(infection_rf_ft, path = "Risk_tables/risk_factors_for_infection.png")

######################################
# Risk factors for Serology (n = 44) #
######################################

serology_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Serology")

table(serology_dat$riskfactor_name, useNA = "ifany")

# Total count of risk factors across significant/insignificant/unspecified
serology_total_rf <- table(unlist(str_split(serology_dat$riskfactor_name, ";")))

# Separate based on significance and adjustment
serology_sig_adj <- data.frame(table(unlist(str_split(serology_dat$riskfactor_name[serology_dat$riskfactor_significant %in% "Significant" & serology_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))
serology_insig_adj <- data.frame(table(unlist(str_split(serology_dat$riskfactor_name[serology_dat$riskfactor_significant %in% "Not significant" & serology_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))
serology_sig_unadj <- data.frame(table(unlist(str_split(serology_dat$riskfactor_name[serology_dat$riskfactor_significant %in% "Significant" & serology_dat$riskfactor_adjusted %in% "Not adjusted"], ";"))))
serology_insig_unadj <- data.frame(table(unlist(str_split(serology_dat$riskfactor_name[serology_dat$riskfactor_significant %in% "Not significant" & serology_dat$riskfactor_adjusted %in% "Not adjusted"], ";"))))
serology_sig_unspec <- data.frame(table(unlist(str_split(serology_dat$riskfactor_name[serology_dat$riskfactor_significant %in% "Significant" & serology_dat$riskfactor_adjusted %in% "Unspecified"], ";"))))
serology_insig_unspec <- data.frame(table(unlist(str_split(serology_dat$riskfactor_name[serology_dat$riskfactor_significant %in% "Not significant" & serology_dat$riskfactor_adjusted %in% "Unspecified"], ";"))))

serology_comb <- bind_rows(
  "Significant_Adjusted" = serology_sig_adj,
  "Insignificant_Adjusted" = serology_insig_adj,
  "Significant_Unadjusted" = serology_sig_unadj,
  "Insignificant_Unadjusted" = serology_insig_unadj,
  "Significant_Unspecified" = serology_sig_unspec,
  "Insignificant_Unspecified" = serology_insig_unspec,
  .id = "Type"
)

serology_comb <- separate(serology_comb, col = "Type", sep = "_", into = c("Significance", "Adjustment"))

# Could remove "Other" risk factors here (n = 27)
serology_rf_ft <- serology_comb %>%
  mutate(
    `Risk factor for serology` = Var1,
    `Number of papers` = Freq,
    `Risk factor for serology` = as.factor(`Risk factor for serology`)
  ) %>%
  select(
    `Risk factor for serology`, Significance, Adjustment, `Number of papers`,
    -Freq, -Var1
  ) %>%
  # filter(!`Risk factor for serology` %in% "Other") %>%
  arrange(`Risk factor for serology`) %>%
  flextable() %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "right")

save_as_image(serology_rf_ft, path = "Risk_tables/risk_factors_for_serology.png")

############################################
# Factors associated with Recovery (n = 7) #
############################################

recovery_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Recovery")

table(recovery_dat$riskfactor_name, useNA = "ifany")

# Total count of risk factors across significant/insignificant/unspecified
recovery_total_rf <- table(unlist(str_split(recovery_dat$riskfactor_name, ";")))

# Separate based on significance and adjustment
recovery_sig_adj <- data.frame(table(unlist(str_split(recovery_dat$riskfactor_name[recovery_dat$riskfactor_significant %in% "Significant" & recovery_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))
recovery_insig_adj <- data.frame(table(unlist(str_split(recovery_dat$riskfactor_name[recovery_dat$riskfactor_significant %in% "Not significant" & recovery_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))
recovery_sig_unadj <- data.frame(table(unlist(str_split(recovery_dat$riskfactor_name[recovery_dat$riskfactor_significant %in% "Significant" & recovery_dat$riskfactor_adjusted %in% "Not adjusted"], ";"))))
recovery_insig_unadj <- data.frame(table(unlist(str_split(recovery_dat$riskfactor_name[recovery_dat$riskfactor_significant %in% "Not significant" & recovery_dat$riskfactor_adjusted %in% "Not adjusted"], ";"))))
recovery_sig_unspec <- data.frame(table(unlist(str_split(recovery_dat$riskfactor_name[recovery_dat$riskfactor_significant %in% "Significant" & recovery_dat$riskfactor_adjusted %in% "Unspecified"], ";"))))
recovery_insig_unspec <- data.frame(table(unlist(str_split(recovery_dat$riskfactor_name[recovery_dat$riskfactor_significant %in% "Not significant" & recovery_dat$riskfactor_adjusted %in% "Unspecified"], ";"))))

recovery_comb <- bind_rows(
  "Significant_Adjusted" = recovery_sig_adj,
  "Insignificant_Adjusted" = recovery_insig_adj,
  "Significant_Unadjusted" = recovery_sig_unadj,
  "Insignificant_Unadjusted" = recovery_insig_unadj,
  "Significant_Unspecified" = recovery_sig_unspec,
  "Insignificant_Unspecified" = recovery_insig_unspec,
  .id = "Type"
)

recovery_comb <- separate(recovery_comb, col = "Type", sep = "_", into = c("Significance", "Adjustment"))

# Removed "Other" risk factors here (n = 5)
recovery_rf_ft <- recovery_comb %>%
  mutate(
    `Protective factor for recovery` = Var1,
    `Number of papers` = Freq,
    `Protective factor for recovery` = as.factor(`Protective factor for recovery`)
  ) %>%
  select(
    `Protective factor for recovery`, Significance, Adjustment, `Number of papers`,
    -Freq, -Var1
  ) %>%
  filter(!`Protective factor for recovery` %in% "Other") %>%
  arrange(`Protective factor for recovery`) %>%
  flextable() %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "right")

save_as_image(recovery_rf_ft, path = "Risk_tables/risk_factors_for_recovery.png")

###########################################
# Risk factors for Severe disease (n = 3) #
###########################################

severe_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Severe disease") %>%
  mutate(
    riskfactor_adjusted =
      case_when(
        is.na(riskfactor_adjusted) ~ "Unspecified",
        TRUE ~ riskfactor_adjusted
      )
  )

table(severe_dat$riskfactor_name, useNA = "ifany")

# Separate based on significance and adjustment
severe_sig_adj <- data.frame(table(unlist(str_split(severe_dat$riskfactor_name[severe_dat$riskfactor_significant %in% "Significant" & severe_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))
severe_insig_adj <- data.frame(table(unlist(str_split(severe_dat$riskfactor_name[severe_dat$riskfactor_significant %in% "Not significant" & severe_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))

severe_comb <- bind_rows(
  "Significant_Adjusted" = severe_sig_adj,
  "Insignificant_Adjusted" = severe_insig_adj,
  .id = "Type"
)

severe_comb <- separate(severe_comb, col = "Type", sep = "_", into = c("Significance", "Adjustment"))

# "Other" risk factors here (n = 2)
severe_rf_ft <- severe_comb %>%
  mutate(
    `Risk factor for severe disease` = Var1,
    `Number of papers` = Freq,
    `Risk factor for severe` = as.factor(`Risk factor for severe disease`)
  ) %>%
  select(
    `Risk factor for severe disease`, Significance, Adjustment, `Number of papers`,
    -Freq, -Var1
  ) %>%
  #filter(!`Risk factor for severe disease` %in% "Other") %>%
  arrange(`Risk factor for severe disease`) %>%
  flextable() %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "right")

save_as_image(severe_rf_ft, path = "Risk_tables/risk_factors_for_severe_disease.png")

#####################################
# Risk factors for Symptoms (n = 2) #
#####################################

symptom_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Symptoms") %>%
  mutate(
    riskfactor_adjusted =
      case_when(
        is.na(riskfactor_adjusted) ~ "Unspecified",
        TRUE ~ riskfactor_adjusted
      )
  )

table(symptom_dat$riskfactor_name, useNA = "ifany")

# Separate based on significance and adjustment
symptom_unspec_adj <- data.frame(table(unlist(str_split(symptom_dat$riskfactor_name[symptom_dat$riskfactor_significant %in% "Unspecified" & symptom_dat$riskfactor_adjusted %in% "Adjusted"], ";"))))
symptom_unspec_unadj <- data.frame(table(unlist(str_split(symptom_dat$riskfactor_name[symptom_dat$riskfactor_significant %in% "Unspecified" & symptom_dat$riskfactor_adjusted %in% "Not adjusted"], ";"))))

symptom_comb <- bind_rows(
  "Unspecified_Adjusted" = symptom_unspec_adj,
  "Unspecified_Unadjusted" = symptom_unspec_unadj,
  .id = "Type"
)

symptom_comb <- separate(symptom_comb, col = "Type", sep = "_", into = c("Significance", "Adjustment"))

# Removing "Other" risk factors here (n = 110)
symptom_rf_ft <- symptom_comb %>%
  mutate(
    `Risk factor for symptoms` = Var1,
    `Number of papers` = Freq,
    `Risk factor for symptom` = as.factor(`Risk factor for symptoms`)
  ) %>%
  select(
    `Risk factor for symptoms`, Significance, Adjustment, `Number of papers`,
    -Freq, -Var1
  ) %>%
  filter(!`Risk factor for symptoms` %in% "Other") %>%
  arrange(`Risk factor for symptoms`) %>%
  flextable() %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "right")

save_as_image(symptom_rf_ft, path = "Risk_tables/risk_factors_for_symptoms.png")

#######################
# 58 "Other" outcomes #
#######################

# In cleaning.R new other_rf_outcome variable created
other_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Other")

test_other <- other_dat %>% select(
  covidence_id, other_rf_outcome, riskfactor_name,
  riskfactor_significant, riskfactor_adjusted, doi, notes
)

outcome_summary <- other_dat %>%
  group_by(other_rf_outcome) %>%
  summarise(
    count_occurrences = n(),
    count_unique_ids = n_distinct(covidence_id)
  )


other_rf_outcomes_ft <- outcome_summary %>%
  mutate(
    `Other outcomes considered for risk factors` = other_rf_outcome,
    `Number of parameters` = count_occurrences,
    `Number of papers` = count_unique_ids
  ) %>%
  select(
    `Other outcomes considered for risk factors`, `Number of parameters`,
    `Number of papers`
  ) %>%
  arrange(desc(`Number of papers`), desc(`Number of parameters`)) %>%
  flextable() %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "right")

save_as_image(other_rf_outcomes_ft, path = "Risk_tables/other_risk_factor_outcomes.png")
