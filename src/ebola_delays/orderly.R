## Human Delays

library(dplyr)
library(orderly2)
library(readr)
library(ggplot2)
library(ggforce)
library(flextable)
library(officer)
library(purrr)
library(scales)

orderly_strict_mode()

#orderly_parameters(pathogen = "EBOLA")
orderly_parameters(pathogen = NULL)

orderly_artefact(
  "Plots and tables for severity parameters",
  c(
    "Delay_plots/symp_plot_filtered.png",
    "Delay_plots/adm_plot_filtered.png",
    "Delay_plots/infp_plot_filtered.png",
    "Delay_plots/dtb_plot_filtered.png",
    "Delay_tables/qa_filtered/symptom_onset_table.png",
    "Delay_tables/qa_filtered/admission_table.png",
    "Delay_tables/qa_filtered/inf_process_table.png",
    "Delay_tables/qa_filtered/death_to_burial_table.png",
    "Delay_tables/unfiltered/symptom_onset_table.png",
    "Delay_tables/unfiltered/admission_table.png",
    "Delay_tables/unfiltered/inf_process_table.png",
    "Delay_tables/unfiltered/death_to_burial_table.png",
    "Delay_tables/qa_filtered/symp_ranges_table.png",
    "Delay_tables/qa_filtered/adm_ranges_table.png",
    "Delay_tables/qa_filtered/infp_ranges_table.png"
  )
)

# Get data from db_compilation
orderly_dependency(
  "db_compilation",
  "latest(parameter:pathogen == this:pathogen)",
  c(
    "articles.csv",
    "parameters.csv"
  )
)

orderly_shared_resource("ebola_functions.R" = "ebola_functions.R")
orderly_shared_resource("ebola_visualisation.R" = "ebola_visualisation.R")

# Load data
articles <- read_csv("articles.csv")
params <- read_csv("parameters.csv")
source("ebola_functions.R")
source("ebola_visualisation.R")

parameter <- "Human delay"

# Changed ID to covidence_id because some entries weren't getting labels
df <- left_join(
  params,
  articles[, c("covidence_id", "first_author_surname", "year_publication", "article_label")],
  by = "covidence_id"
) %>%
  arrange(article_label, -year_publication)

delay_dat <- df %>%
  mutate(
    population_country = as.factor(population_country)
  ) %>%
  filter(parameter_class == parameter) %>%
  filter(parameter_from_figure == "FALSE") %>%
  filter(!(delay_short %in% "Other human delay (go to section)")) %>%
  # Remove outlier - Martinez 2022, latent period mean 31.25 (range 11-71)
  filter(!(covidence_id == 17715)) %>%
  mutate(
    article_label_unique = make.unique(article_label),
    article_label =
      case_when(article_label == "WHO/International Study Team 1978" ~
        "WHO/Int. Study Team 1978", TRUE ~ article_label),
    outbreak = order_ebola_outbreaks(outbreak),
    delay_short =
      factor(delay_short,
        levels = c(
          "Incubation period", "Latent period",
          "Infectious period", "Generation time",
          "Serial interval", "Symptom onset to test",
          "Symptom onset to reporting",
          "Symptom onset to seeking care",
          "Symptom onset to admission to care",
          "Symptom onset to quarantine",
          "Symptom onset to recovery/non-infectiousness",
          "Symptom onset to discharge from care",
          "Symptom onset to death",
          "Admission to care to recovery/non-infectiousness",
          "Admission to care to discharge from care",
          "Admission to care to death",
          "Admission to care to death/discharge",
          "Death to burial"
        )
      ),
    population_study_start_month =
      factor(population_study_start_month,
        levels = c(
          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
          "Sep", "Oct", "Nov", "Dec"
        )
      ),
    # Adjustments to put parameters on the same timescale (weeks --> days)
    parameter_value =
      case_when(
        covidence_id %in% c(17956, 17097) ~
          round(parameter_value * 7, digits = 2),
        inverse_param == TRUE & !is.na(parameter_value) ~
          round(1 / parameter_value, digits = 2),
        TRUE ~ parameter_value
      ),
    parameter_lower_bound =
      case_when(
        covidence_id == 16599 &
          delay_short %in% c("Latent period", "Infectious period") ~
          round(parameter_lower_bound * 7, digits = 2),
        inverse_param == TRUE & !is.na(parameter_lower_bound) ~
          round(1 / parameter_lower_bound, digits = 2),
        TRUE ~ parameter_lower_bound
      ),
    parameter_upper_bound =
      case_when(
        covidence_id == 16599 &
          delay_short %in% c("Latent period", "Infectious period") ~
          round(parameter_upper_bound * 7, digits = 2),
        inverse_param == TRUE & !is.na(parameter_upper_bound) ~
          round(1 / parameter_upper_bound, digits = 2),
        TRUE ~ parameter_upper_bound
      ),
    parameter_bounds =
      case_when(
        covidence_id == 16599 | (inverse_param == TRUE & !is.na(parameter_lower_bound)) ~
          paste(parameter_lower_bound, "-", parameter_upper_bound),
        TRUE ~ parameter_bounds
      ),
    parameter_unit =
      case_when(
        covidence_id %in% c(16599, 17956, 17097) ~ "Days",
        TRUE ~ parameter_unit
      ),
    parameter_uncertainty_single_value =
      case_when(
        covidence_id == 17956 &
          parameter_uncertainty_singe_type == "Standard Deviation" ~
          round(parameter_uncertainty_single_value * sqrt(7), digits = 2),
        TRUE ~ parameter_uncertainty_single_value
      ),
  ) %>%
  group_by(delay_short)

# Order data for plots
ordered_dat <- delay_dat %>%
  group_by(outbreak) %>%
  mutate(
    range_midpoint =
      ifelse(is.na(parameter_value) & !is.na(parameter_upper_bound),
        parameter_upper_bound - parameter_lower_bound, NA
      ),
    temp_order_by = ifelse(!is.na(parameter_value),
      parameter_value,
      range_midpoint
    )
  ) %>%
  arrange(temp_order_by) %>%
  mutate(
    article_label_unique = factor(
      article_label_unique,
      levels = unique(article_label_unique)
    )
  )

# Create directory for results
dir.create("Delay_plots")
dir.create("Delay_tables")
dir.create("Delay_tables/qa_filtered")
dir.create("Delay_tables/unfiltered")


# PLOTS

# Symptom onset to X with qa_filter of >=50
symp_dat <- ordered_dat %>%
  filter(delay_start == "Symptom onset")

symp_plot_qa <- create_plot(
  symp_dat,
  param = parameter,
  qa_filter = TRUE,
  facet_by = "delay_short",
  symbol_col_by = "outbreak"
)

# Admission to X with qa_filter of >=50
adm_dat <- ordered_dat %>%
  filter(delay_start == "Admission to care")

adm_plot_qa <- create_plot(
  adm_dat,
  param = parameter,
  qa_filter = TRUE,
  facet_by = "delay_short",
  symbol_col_by = "outbreak"
)

# Infection process with qa_filter of >=50
infp_dat <- ordered_dat %>%
  filter(delay_start == "Infection process")

infp_plot_qa <- create_plot(
  infp_dat,
  param = parameter,
  qa_filter = TRUE,
  facet_by = "delay_short",
  symbol_col_by = "outbreak"
)

# Death to burial with qa_filter of >=50
dtb_dat <- ordered_dat %>%
  filter(delay_start == "Death to burial")

dtb_plot_qa <- create_plot(
  dtb_dat,
  param = parameter,
  qa_filter = TRUE,
  facet_by = "delay_short",
  symbol_col_by = "outbreak"
)


ggsave("Delay_plots/symp_plot_filtered.png", symp_plot_qa,
  width = 9, height = 16, units = "in", bg = "white"
)

ggsave("Delay_plots/adm_plot_filtered.png", adm_plot_qa,
  width = 9, height = 9, units = "in", bg = "white"
)

ggsave("Delay_plots/infp_plot_filtered.png", infp_plot_qa,
  width = 9, height = 9, units = "in", bg = "white"
)

ggsave("Delay_plots/dtb_plot_filtered.png", dtb_plot_qa,
  width = 9, height = 2, units = "in", bg = "white"
)

# OVERVIEW TABLES

# Symptom onset to X table with qa_filter of >=50
# I have checked the two Rosello 2015 0's (10.7554/eLife.09015)
so_table_qa <- create_table(
  delay_dat,
  param = parameter,
  delay_type = "Symptom onset",
  group = "delay_short",
  qa_filter = TRUE
)

# Admission to care to X table with qa_filter of >=50
adm_table_qa <- create_table(
  delay_dat,
  param = parameter,
  delay_type = "Admission to care",
  group = "delay_short",
  qa_filter = TRUE
)

# Infection process table with qa_filter of >=50
ip_table_qa <- create_table(
  delay_dat,
  param = parameter,
  delay_type = "Infection process",
  group = "delay_short",
  qa_filter = TRUE
)

# Death to burial table with qa_filter of >=50
dtb_table_qa <- create_table(
  delay_dat,
  param = parameter,
  delay_type = "Death to burial",
  group = "delay_short",
  qa_filter = TRUE
)

# Save
save_as_image(so_table_qa,
  path = "Delay_tables/qa_filtered/symptom_onset_table.png"
)
save_as_image(adm_table_qa,
  path = "Delay_tables/qa_filtered/admission_table.png"
)
save_as_image(ip_table_qa,
  path = "Delay_tables/qa_filtered/inf_process_table.png"
)
save_as_image(dtb_table_qa,
  path = "Delay_tables/qa_filtered/death_to_burial_table.png"
)

# Symptom onset to X table with NO qa_filter
so_table_all <- create_table(
  delay_dat,
  param = parameter,
  delay_type = "Symptom onset",
  group = "delay_short",
  qa_filter = FALSE
)

# Admission to X table with NO qa_filter
adm_table_all <- create_table(
  delay_dat,
  param = parameter,
  delay_type = "Admission to care",
  group = "delay_short",
  qa_filter = FALSE
)

# Infection process table with NO qa_filter
ip_table_all <- create_table(
  delay_dat,
  param = parameter,
  delay_type = "Infection process",
  group = "delay_short",
  qa_filter = FALSE
)

# Death to burial table with NO qa_filter
dtb_table_all <- create_table(
  delay_dat,
  param = parameter,
  delay_type = "Death to burial",
  group = "delay_short",
  qa_filter = FALSE
)

# Save
save_as_image(so_table_all,
  path = "Delay_tables/unfiltered/symptom_onset_table.png"
)
save_as_image(adm_table_all,
  path = "Delay_tables/unfiltered/admission_table.png"
)
save_as_image(ip_table_all,
  path = "Delay_tables/unfiltered/inf_process_table.png"
)
save_as_image(dtb_table_all,
  path = "Delay_tables/unfiltered/death_to_burial_table.png"
)


# RANGE TABLES

symp_ranges_outbreak <- create_range_table(
  df = symp_dat,
  main_group = "delay_short", main_group_label = "Delay",
  sub_group = "outbreak", sub_group_label = "Outbreak",
  qa_filter = TRUE, rounding = "1d"
)

adm_ranges_outbreak <- create_range_table(
  df = adm_dat,
  main_group = "delay_short", main_group_label = "Delay",
  sub_group = "outbreak", sub_group_label = "Outbreak",
  qa_filter = TRUE, rounding = "1d"
)

infp_ranges_outbreak <- create_range_table(
  df = infp_dat,
  main_group = "delay_short", main_group_label = "Delay",
  sub_group = "outbreak", sub_group_label = "Outbreak",
  qa_filter = TRUE, rounding = "1d"
)

# Save
save_as_image(symp_ranges_outbreak,
  path = "Delay_tables/qa_filtered/symp_ranges_table.png"
)
save_as_image(adm_ranges_outbreak,
  path = "Delay_tables/qa_filtered/adm_ranges_table.png"
)
save_as_image(infp_ranges_outbreak,
  path = "Delay_tables/qa_filtered/infp_ranges_table.png"
)
