## Risk Factors

library(dplyr)
library(tidyr)
library(orderly2)
library(readr)
library(ggplot2)
library(ggforce)
library(flextable)
library(ftExtra)
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
    "Risk_tables/protective_factors_for_recovery.png",
    "Risk_tables/risk_factors_for_severe_disease.png",
    "Risk_tables/risk_factors_for_symptoms.png",
    "Risk_tables/other_risk_factor_outcomes.png",
    "Risk_tables/risk_factors_for_onward_transmission.png"
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

rf_dat <- df %>%
  mutate(
    population_country = as.factor(population_country),
    riskfactor_adjusted =
        case_when(
          is.na(riskfactor_adjusted) ~ "Unspecified",
          TRUE ~ riskfactor_adjusted
        ),
    riskfactor_significant =
      case_when(
        riskfactor_significant %in% "Not Significant" ~ "Not significant",
        is.na(riskfactor_significant) |
          riskfactor_significant %in% "Unspecified" ~ "Unspecified significance",
        TRUE ~ riskfactor_significant
      ),
    riskfactor_significant = factor(
      riskfactor_significant,
      levels = c("Significant", "Not significant", "Unspecified significance")
    ),
    riskfactor_adjusted = factor(
      riskfactor_adjusted,
      levels = c("Adjusted", "Not adjusted", "Unspecified")
    ),
    riskfactor_name = gsub("Cormobidity", "Comorbidity", riskfactor_name)
  ) %>%
  filter(parameter_class %in% parameter) %>%
  filter(!parameter_from_figure %in% TRUE)

unique(rf_dat$riskfactor_outcome)
table(rf_dat$riskfactor_outcome, useNA = "ifany")
table(rf_dat$riskfactor_adjusted, useNA = "ifany")
table(rf_dat$riskfactor_significant, useNA = "ifany")

# Create directory for results
dir.create("Risk_tables")

####################################
# Risk factors for death (n = 136) #
####################################

death_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Death") %>%
  mutate(`Risk Factor for Death` = riskfactor_name)

table(death_dat$riskfactor_name, useNA = "ifany")

death_table <- death_dat %>%
  separate_rows(`Risk Factor for Death`, sep = ";") %>%
  group_by(riskfactor_significant, riskfactor_adjusted, `Risk Factor for Death`) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c(riskfactor_significant, riskfactor_adjusted),
              values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(across(-1))) %>%
  arrange(ifelse(`Risk Factor for Death` %in% "Other", Inf, desc(Total))) %>%
  flextable() %>%
  split_header() %>%
  span_header() %>%
  fontsize(i = 1:2, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1:2, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  vline(j = c(1, 4, 7, 10)) %>%
  border_inner_h(part = "header") %>%
  border_outer() %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  line_spacing(i = 1:2, space = 1.5, part = "header")

save_as_image(death_table, path = "Risk_tables/risk_factors_for_death.png")

#######################################
# Risk factors for infection (n = 59) #
#######################################

# Checked and removed NA risk factor in cleaning.R (ID 2890)

infection_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Infection") %>%
  mutate(`Risk Factor for Infection` = riskfactor_name)

table(infection_dat$riskfactor_name, useNA = "ifany")

inf_table <- infection_dat %>%
  separate_rows(`Risk Factor for Infection`, sep = ";") %>%
  group_by(riskfactor_significant, riskfactor_adjusted, `Risk Factor for Infection`) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c(riskfactor_significant, riskfactor_adjusted),
              values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(across(-1))) %>%
  arrange(ifelse(`Risk Factor for Infection` %in% "Other", Inf, desc(Total))) %>%
  flextable() %>%
  split_header() %>%
  span_header() %>%
  fontsize(i = 1:2, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1:2, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  vline(j = c(1, 4, 7, 10)) %>%
  border_inner_h(part = "header") %>%
  border_outer() %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  line_spacing(i = 1:2, space = 1.5, part = "header")

save_as_image(inf_table, path = "Risk_tables/risk_factors_for_infection.png")

######################################
# Risk factors for Serology (n = 44) #
######################################

serology_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Serology") %>%
  mutate(`Risk Factor for Serology` = riskfactor_name)

table(serology_dat$riskfactor_name, useNA = "ifany")

sero_table <- serology_dat %>%
  separate_rows(`Risk Factor for Serology`, sep = ";") %>%
  group_by(riskfactor_significant, riskfactor_adjusted, `Risk Factor for Serology`) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c(riskfactor_significant, riskfactor_adjusted),
              values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(across(-1))) %>%
  arrange(ifelse(`Risk Factor for Serology` %in% "Other", Inf, desc(Total))) %>%
  flextable() %>%
  split_header() %>%
  span_header() %>%
  fontsize(i = 1:2, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1:2, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  vline(j = c(1, 4, 7)) %>%
  border_inner_h(part = "header") %>%
  border_outer() %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  line_spacing(i = 1:2, space = 1.5, part = "header")

save_as_image(sero_table, path = "Risk_tables/risk_factors_for_serology.png")

############################################
# Factors associated with Recovery (n = 7) #
############################################

recovery_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Recovery") %>%
  mutate(`Protective Factor for Recovery` = riskfactor_name)

table(recovery_dat$riskfactor_name, useNA = "ifany")

recovery_table <- recovery_dat %>%
  separate_rows(`Protective Factor for Recovery`, sep = ";") %>%
  group_by(riskfactor_significant, riskfactor_adjusted, `Protective Factor for Recovery`) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c(riskfactor_significant, riskfactor_adjusted),
              values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(across(-1))) %>%
  arrange(ifelse(`Protective Factor for Recovery` %in% "Other", Inf, desc(Total))) %>%
  flextable() %>%
  split_header() %>%
  span_header() %>%
  fontsize(i = 1:2, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1:2, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  vline(j = c(1, 4, 7)) %>%
  border_inner_h(part = "header") %>%
  border_outer() %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  line_spacing(i = 1:2, space = 1.5, part = "header")

save_as_image(recovery_table, path = "Risk_tables/protective_factors_for_recovery.png")

###########################################
# Risk factors for Severe disease (n = 3) #
###########################################

severe_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Severe disease") %>%
  mutate(`Risk Factor for Severe Disease` = riskfactor_name)

table(severe_dat$riskfactor_name, useNA = "ifany")

severe_table <- severe_dat %>%
  separate_rows(`Risk Factor for Severe Disease`, sep = ";") %>%
  group_by(riskfactor_significant, riskfactor_adjusted, `Risk Factor for Severe Disease`) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c(riskfactor_significant, riskfactor_adjusted),
              values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(across(-1))) %>%
  arrange(ifelse(`Risk Factor for Severe Disease` %in% "Other", Inf, desc(Total))) %>%
  flextable() %>%
  split_header() %>%
  span_header() %>%
  fontsize(i = 1:2, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1:2, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  vline(j = c(1, 2, 3)) %>%
  border_inner_h(part = "header") %>%
  border_outer() %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  line_spacing(i = 1:2, space = 1.5, part = "header")

save_as_image(severe_table, path = "Risk_tables/risk_factors_for_severe_disease.png")

#####################################
# Risk factors for Symptoms (n = 2) #
#####################################

symptom_dat <- rf_dat %>%
  filter(riskfactor_outcome %in% "Symptoms") %>%
  mutate(`Risk Factor for Symptoms` = riskfactor_name)

table(symptom_dat$riskfactor_name, useNA = "ifany")

symptom_table <- symptom_dat %>%
  separate_rows(`Risk Factor for Symptoms`, sep = ";") %>%
  group_by(riskfactor_significant, riskfactor_adjusted, `Risk Factor for Symptoms`) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c(riskfactor_significant, riskfactor_adjusted),
              values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(across(-1))) %>%
  arrange(ifelse(`Risk Factor for Symptoms` %in% "Other", Inf, desc(Total))) %>%
  flextable() %>%
  split_header() %>%
  span_header() %>%
  fontsize(i = 1:2, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1:2, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  vline(j = c(1, 2, 3)) %>%
  border_inner_h(part = "header") %>%
  border_outer() %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  line_spacing(i = 1:2, space = 1.5, part = "header")

save_as_image(symptom_table, path = "Risk_tables/risk_factors_for_symptoms.png")

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

## Risk factors associated with onward transmission:

onward_transmission_dat <- other_dat %>%
  filter(other_rf_outcome %in% "Onward transmission") %>%
  mutate(
    riskfactor_name =
      case_when(
        riskfactor_name %in% "Other" &
          riskfactor_significant %in% "Not significant" ~ "Location (rural/urban)",
        # "There was no significant association between urban or rural
        # location and number of subsequent cases."
        riskfactor_name %in% "Age;Funeral;Hospitalisation;Other" ~
          "Age;Funeral;Hospitalisation;Sex;Survival;First generation of transmission chain",
        # We found significant associations with the number of secondary cases
        # generated of the following characteristics: sex, outcome (survival/burial/ETU),
        # age category, and being the first generation of a chain
        # Age: "We found that children and young adults had lower onward
        # transmission, whereas infections in older adults were more likely
        # to result in large numbers of secondary cases.
        # Burial and hospitalisation: Attending an ETU was associated with a
        # large decrease in the number of transmission events, and unsafe burial
        # was associated with an almost 2-fold increase in number of transmissions.
        # First gen: We found that the first generation of each chain was associated
        # with a higher number of secondary cases than those identified later in the chain.
        riskfactor_name %in% "Other" &
          riskfactor_significant %in% "Significant" ~ "Socioeconomic status",
        # "Overcrowding and lack of education on how the disease is transmitted
        # could explain the observed differences in number of secondary cases."
        TRUE ~ riskfactor_name),
        `Risk Factor for Onward Transmission` = riskfactor_name
    )

onw_transm_table <- onward_transmission_dat %>%
  separate_rows(`Risk Factor for Onward Transmission`, sep = ";") %>%
  group_by(riskfactor_significant, riskfactor_adjusted, `Risk Factor for Onward Transmission`) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c(riskfactor_significant, riskfactor_adjusted),
              values_from = count, values_fill = 0) %>%
  flextable() %>%
  split_header() %>%
  span_header() %>%
  fontsize(i = 1:2, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1:2, bold = TRUE, part = "header") %>%
  add_footer_lines("") %>%
  vline(j = c(1, 2, 3)) %>%
  border_inner_h(part = "header") %>%
  border_outer() %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  line_spacing(i = 1:2, space = 1.5, part = "header")

save_as_image(onw_transm_table, path = "Risk_tables/risk_factors_for_onward_transmission.png")
