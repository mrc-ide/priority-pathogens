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
library(epitrix)
library(stringr)
library(grid)
library(gridExtra)
library(metafor)
library(meta)
library(estmeansd)

orderly_strict_mode()

orderly_parameters(pathogen = "EBOLA")
# orderly_parameters(pathogen = NULL)

orderly_artefact(
  "Plots, tables and meta_analysis for delay parameters",
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
    "Delay_tables/qa_filtered/infp_ranges_table.png",
    "Delay_tables/qa_filtered/select_ranges_table.png",
    "Meta_plots/Variance/incubation_period.png",
    "Meta_plots/Variance/serial_interval.png",
    "Meta_plots/Variance/onset_to_death.png",
    "Meta_plots/Variance/meta_delays_variance.png",
    "Meta_plots/Uncertainty/infectious_period.png",
    "Meta_plots/Uncertainty/serial_interval.png",
    "Meta_plots/Uncertainty/meta_delays_uncertainty.png"
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
  articles[, c("covidence_id", "first_author_surname", "year_publication", "article_label", "doi")],
  by = "covidence_id"
) %>%
  arrange(article_label, -year_publication)

delay_dat <- df %>%
  mutate(
    population_country = as.factor(population_country)
  ) %>%
  filter(parameter_class %in% parameter) %>%
  filter(!parameter_from_figure %in% TRUE) %>%
  filter(!delay_short %in% "Other human delay (go to section)") %>%
  # Remove Martinez 2022 - outlier, latent period mean 31.25 (range 11-71)
  filter(!(covidence_id %in% 17715)) %>%
  # Remove Baller 2022 - table and text don't match
  filter(!(covidence_id %in% 15544)) %>%
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
    # Account for distributions
    parameter_value_type =
      case_when(
        is.na(parameter_value) & distribution_type %in% "Gamma" &
          distribution_par1_type %in% "Shape" & distribution_par2_type %in% "Scale" ~
          "Mean", TRUE ~ parameter_value_type
      ),
    parameter_value =
      case_when(
        is.na(parameter_value) & distribution_type %in% "Gamma" &
          distribution_par1_type %in% "Shape" & distribution_par2_type %in% "Scale" ~
          gamma_shapescale2mucv(
            shape = distribution_par1_value,
            scale = distribution_par2_value
          )$mu,
        TRUE ~ parameter_value
      ),
    parameter_uncertainty_single_value =
      case_when(
        distribution_type %in% "Gamma" & is.na(parameter_uncertainty_singe_type) &
          distribution_par1_type %in% "Shape" & distribution_par2_type %in% "Scale" ~
          gamma_shapescale2mucv(
            shape = distribution_par1_value,
            scale = distribution_par2_value
          )$mu *
            gamma_shapescale2mucv(
              shape = distribution_par1_value,
              scale = distribution_par2_value
            )$cv,
        distribution_type == "Gamma" & is.na(parameter_uncertainty_singe_type) &
          distribution_par2_type == "Mean sd" ~ distribution_par2_value,
        TRUE ~ parameter_uncertainty_single_value
      ),
    parameter_uncertainty_singe_type =
      case_when(
        distribution_type %in% "Gamma" & is.na(parameter_uncertainty_singe_type) &
          distribution_par1_type %in% "Shape" & distribution_par2_type %in% "Scale" ~
          "Standard Deviation",
        distribution_type %in% "Gamma" & is.na(parameter_uncertainty_singe_type) &
          distribution_par2_type %in% "Mean sd" ~ "Standard Deviation",
        TRUE ~ parameter_uncertainty_singe_type
      ),
    # Adjustments to put parameters on the same timescale (e.g. weeks --> days)
    # and to convert values and uncertainty for inverse parameters
    parameter_value =
      case_when(
        covidence_id %in% c(17956, 17097) ~
          round(parameter_value * 7, digits = 2),
        inverse_param %in% TRUE & !is.na(parameter_value) ~
          round(1 / parameter_value, digits = 2),
        parameter_unit %in% "Hours" ~ round(parameter_value / 24, digits = 2),
        TRUE ~ parameter_value
      ),
    lower_bound_temp =
      case_when(
        inverse_param %in% TRUE & !is.na(parameter_lower_bound) ~
          parameter_lower_bound,
        TRUE ~ NA
      ),
    parameter_lower_bound =
      case_when(
        covidence_id %in% 16599 &
          delay_short %in% c("Latent period", "Infectious period") ~
          round(parameter_lower_bound * 7, digits = 2),
        inverse_param %in% TRUE & !is.na(parameter_lower_bound) ~
          round(1 / parameter_upper_bound, digits = 2),
        TRUE ~ parameter_lower_bound
      ),
    parameter_upper_bound =
      case_when(
        covidence_id %in% 16599 &
          delay_short %in% c("Latent period", "Infectious period") ~
          round(parameter_upper_bound * 7, digits = 2),
        inverse_param %in% TRUE & !is.na(parameter_upper_bound) ~
          round(1 / lower_bound_temp, digits = 2),
        TRUE ~ parameter_upper_bound
      ),
    parameter_bounds =
      case_when(
        covidence_id %in% 16599 | !is.na(parameter_lower_bound) ~
          paste(parameter_lower_bound, "-", parameter_upper_bound),
        TRUE ~ parameter_bounds
      ),
    parameter_uncertainty_single_value =
      case_when(
        covidence_id %in% 17956 &
          parameter_uncertainty_singe_type %in% "Standard Deviation" ~
          round(parameter_uncertainty_single_value * sqrt(7), digits = 2),
        TRUE ~ parameter_uncertainty_single_value
      ),
    upper_unc_temp =
      case_when(
        inverse_param %in% TRUE & !is.na(parameter_uncertainty_upper_value) ~
          parameter_uncertainty_upper_value,
        TRUE ~ NA
      ),
    parameter_uncertainty_upper_value =
      case_when(
        parameter_unit %in% "Hours" ~
          round(parameter_uncertainty_upper_value / 24, digits = 2),
        inverse_param %in% TRUE & !is.na(parameter_uncertainty_upper_value) ~
          round(1 / parameter_uncertainty_lower_value, digits = 2),
        TRUE ~ parameter_uncertainty_upper_value
      ),
    parameter_uncertainty_lower_value =
      case_when(
        parameter_unit %in% "Hours" ~
          round(parameter_uncertainty_lower_value / 24, digits = 2),
        inverse_param %in% TRUE & !is.na(parameter_uncertainty_lower_value) ~
          round(1 / upper_unc_temp, digits = 2),
        TRUE ~ parameter_uncertainty_lower_value
      ),
    parameter_unit =
      case_when(
        covidence_id %in% c(16599, 17956, 17097) ~ "Days",
        parameter_unit %in% "Hours" ~ "Days",
        TRUE ~ parameter_unit
      )
  ) %>%
  select(-c(lower_bound_temp, upper_unc_temp)) %>%
  # remove sneaky duplicate entries
  distinct_at(vars(-article_label_unique, -parameter_data_id), .keep_all = TRUE) %>%
  # modify standard deviation and standard error to visualise uncertainty in plots
  mutate(
    parameter_uncertainty_type =
      case_when(
        is.na(parameter_uncertainty_type) &
          parameter_uncertainty_singe_type %in% "Standard Deviation" ~
          "Standard Deviation",
        is.na(parameter_uncertainty_type) &
          parameter_uncertainty_singe_type %in% "Standard Error" ~
          "Standard Error",
        TRUE ~ parameter_uncertainty_type
      ),
    parameter_uncertainty_lower_value =
      case_when(
        parameter_uncertainty_type %in% c("Standard Deviation", "Standard Error") ~
          parameter_value - parameter_uncertainty_single_value,
        TRUE ~ parameter_uncertainty_lower_value
      ),
    parameter_uncertainty_upper_value =
      case_when(
        parameter_uncertainty_type %in% c("Standard Deviation", "Standard Error") ~
          parameter_value + parameter_uncertainty_single_value,
        TRUE ~ parameter_uncertainty_upper_value
      ),
    # Handle different types of standard deviation
    parameter_uncertainty_singe_type =
      case_when(
        parameter_uncertainty_singe_type %in% "Standard Deviation" &
          covidence_id %in% c(15947, 2941, 17956) & inverse_param %in% TRUE ~
          "SD of the inverse mean",
        parameter_uncertainty_singe_type %in% "Standard Deviation" &
          covidence_id %in% c(15947, 2941, 17956) ~ "SD of the mean",
        parameter_uncertainty_singe_type %in% "Standard Deviation" &
          !covidence_id %in% c(15947, 2941, 17956) ~ "SD of the sample",
        TRUE ~ parameter_uncertainty_singe_type
      )
  ) %>%
  group_by(delay_short)

# Order data for plots
ordered_dat <- delay_dat %>%
  group_by(outbreak) %>%
  mutate(
    range_midpoint =
      ifelse(is.na(parameter_value) & !is.na(parameter_upper_bound),
        (parameter_upper_bound - parameter_lower_bound) / 2 + parameter_lower_bound, NA
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
dir.create("Meta_plots")
dir.create("Meta_plots/Variance")
dir.create("Meta_plots/Uncertainty")

#########
# PLOTS #
#########

# Symptom onset to X with qa_filter of >=50
symp_dat <- ordered_dat %>%
  filter(delay_start %in% "Symptom onset")

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
  filter(delay_start %in% "Infection process")

infp_plot_qa <- create_plot(
  infp_dat,
  param = parameter,
  qa_filter = TRUE,
  facet_by = "delay_short",
  symbol_col_by = "outbreak"
)

# Death to burial with qa_filter of >=50
dtb_dat <- ordered_dat %>%
  filter(delay_start %in% "Death to burial")

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

###################
# OVERVIEW TABLES #
###################

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

################
# RANGE TABLES #
################

symp_ranges_outbreak <- create_range_table(
  df = symp_dat,
  main_group = "delay_short", main_group_label = "Delay",
  sub_group = "ebola_species", sub_group_label = "Species",
  qa_filter = TRUE, rounding = "1d"
)

adm_ranges_outbreak <- create_range_table(
  df = adm_dat,
  main_group = "delay_short", main_group_label = "Delay",
  sub_group = "ebola_species", sub_group_label = "Species",
  qa_filter = TRUE, rounding = "1d"
)

infp_ranges_outbreak <- create_range_table(
  df = infp_dat,
  main_group = "delay_short", main_group_label = "Delay",
  sub_group = "ebola_species", sub_group_label = "Species",
  qa_filter = TRUE, rounding = "1d"
)

select_dat <- ordered_dat %>%
  filter(delay_start %in% "Infection process" |
    delay_short %in% c(
      "Symptom onset to death",
      "Symptom onset to recovery/non-infectiousness"
    ))

selection_ranges_outbreak <- create_range_table(
  df = select_dat,
  main_group = "delay_short", main_group_label = "Delay",
  sub_group = "ebola_species", sub_group_label = "Species",
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
save_as_image(selection_ranges_outbreak,
  path = "Delay_tables/qa_filtered/select_ranges_table.png"
)


## META ANALYSIS

# Note SDs were checked and assigned either sd_sample or sd_mean and considered separately
# Entries we can use for the meta-analysis:
# mean and SD (everything to be converted to this format)
# mean and SE (can convert SE to SD using sample sizes before supplying to metamean)
# mean and CI/CrI (can convert CI/CrI to SD using sample sizes before supplying to metamean - see link below)
# https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
# median and IQR (input as is into metamean - converted to mean/SD using Cai method)
# median and range (input as is into metamean - converted to mean/SD using Cai method)

meta_dat <- delay_dat %>%
  filter(
    !is.na(parameter_value),
    !is.na(population_sample_size),
    !is.na(parameter_uncertainty_singe_type) | !is.na(parameter_uncertainty_type),
    parameter_value_type != "Unspecified",
    delay_short %in% c(
      "Incubation period", "Latent period",
      "Infectious period", "Generation time",
      "Serial interval", "Symptom onset to death",
      "Symptom onset to recovery/non-infectiousness"
    )
  ) %>%
  mutate(
    xbar = case_when(parameter_value_type %in% "Mean" ~
      parameter_value, TRUE ~ NA),
    median = case_when(parameter_value_type %in% "Median" ~
      parameter_value, TRUE ~ NA),
    sd = case_when(parameter_value_type %in% "Mean" &
      parameter_uncertainty_singe_type %in% "SD of the sample" ~
      parameter_uncertainty_single_value, TRUE ~ NA),
    serr = case_when(parameter_value_type %in% "Mean" &
      parameter_uncertainty_singe_type %in% "Standard Error" ~
      parameter_uncertainty_single_value, TRUE ~ NA),
    q1 = case_when(parameter_value_type %in% "Median" &
      parameter_uncertainty_type %in% "IQR" ~
      parameter_uncertainty_lower_value, TRUE ~ NA),
    q3 = case_when(parameter_value_type %in% "Median" &
      parameter_uncertainty_type %in% "IQR" ~
      parameter_uncertainty_upper_value, TRUE ~ NA),
    min = case_when(parameter_value_type %in% "Median" &
      parameter_uncertainty_type %in% "Range" ~
      parameter_uncertainty_lower_value, TRUE ~ NA),
    max = case_when(parameter_value_type %in% "Median" &
      parameter_uncertainty_type %in% "Range" ~
      parameter_uncertainty_upper_value, TRUE ~ NA),
    ci_min = case_when(parameter_value_type %in% "Mean" &
      parameter_uncertainty_type %in% "95% CI" ~
      parameter_uncertainty_lower_value, TRUE ~ NA),
    ci_max = case_when(parameter_value_type %in% "Mean" &
      parameter_uncertainty_type %in% "95% CI" ~
      parameter_uncertainty_upper_value, TRUE ~ NA),
    cri_min = case_when(parameter_value_type %in% "Mean" &
      parameter_uncertainty_type %in% "95% CrI" ~
      parameter_uncertainty_lower_value, TRUE ~ NA),
    cri_max = case_when(parameter_value_type %in% "Mean" &
      parameter_uncertainty_type %in% "95% CrI" ~
      parameter_uncertainty_upper_value, TRUE ~ NA)
  ) %>%
  # convert SEs and CIs/CrIs for means into SD using sample size (see cochrane chapter above)
  mutate(
    sd = case_when(
      is.na(sd) & !is.na(serr) ~ serr * sqrt(population_sample_size),
      # confidence intervals
      is.na(sd) & !is.na(ci_min) & population_sample_size >= 60 ~
        sqrt(population_sample_size) * (ci_max - ci_min) / 3.92,
      is.na(sd) & !is.na(ci_min) & population_sample_size < 60 ~
        sqrt(population_sample_size) * (ci_max - ci_min) /
          (2 * qt((1 + 0.95) / 2, df = population_sample_size - 1)),
      # credible intervals
      is.na(sd) & !is.na(cri_min) & population_sample_size >= 60 ~
        sqrt(population_sample_size) * (cri_max - cri_min) / 3.92,
      is.na(sd) & !is.na(cri_min) & population_sample_size < 60 ~
        sqrt(population_sample_size) * (cri_max - cri_min) /
          (2 * qt((1 + 0.95) / 2, df = population_sample_size - 1)),
      TRUE ~ sd
    )
  ) %>%
  # remove converted uncertainty variables
  select(-c(serr, ci_min, ci_max, cri_min, cri_max)) %>%
  # remove rows where all uncertainty variables are empty
  filter(!is.na(sd) | !is.na(q1) | !is.na(q3) | !is.na(min) | !is.na(max))

# meta for variance: sd of sample, standard error, IQR, range
meta_var <- meta_dat %>%
  filter(parameter_uncertainty_singe_type %in% c(
    "SD of the sample",
    "Standard Error"
  ) |
    parameter_uncertainty_type %in% c("IQR", "Range"))

# meta for uncertainty: sd of mean, confidence intervals, credible intervals
meta_unc <- meta_dat %>%
  filter(parameter_uncertainty_singe_type %in% c("SD of the mean") |
    parameter_uncertainty_type %in% c("95% CI", "95% CrI"))


##########################
# VARIANCE META-ANALYSIS #
##########################

# Variance data (incubation period, serial interval, symptom onset to death)
sotd_var <- meta_var %>% filter(delay_short %in% "Symptom onset to death") # 17
incub_var <- meta_var %>% filter(delay_short %in% "Incubation period") # 8
serial_var <- meta_var %>% filter(delay_short %in% "Serial interval") # 8

# Not included as too few data points:
# sotr_var <- meta_var %>% filter(delay_short %in% "Symptom onset to recovery/non-infectiousness") # 2
# inf_var <- meta_var %>% filter(delay_short %in% "Infectious period") # 1

# Symptom onset to death meta-analysis
set.seed(6)
sotd_var_ma <- metamean(
  data = sotd_var,
  n = population_sample_size,
  mean = xbar,
  sd = sd,
  studlab = article_label,
  median = median,
  q1 = q1,
  q3 = q3,
  min = min,
  max = max,
  # Method for Unknown Non-Normal Distributions (MLN) approach (Cai et al. (2021)):
  method.mean = "Cai",
  method.sd = "Cai",
  # no transformation of data before meta-analysis:
  sm = "MRAW",
  method.tau = "ML"
)

png(file = "Meta_plots/Variance/onset_to_death.png", width = 9500, height = 5500, res = 1000)
forest.meta(sotd_var_ma,
  digits = 2, digits.sd = 2, digits.weight = 2, layout = "RevMan5",
  weight.study = "same", col.square.lines = "black", col.square = "dodgerblue3",
  col.study = "black", col.inside = "black", col.diamond.lines = "black",
  col.diamond.common = "dodgerblue3", col.diamond.random = "dodgerblue3",
  at = seq(5, 14, by = 3),
  xlim = c(5, 14),
  xlab = "Symptom onset to death (days)", fontsize = 10
)
dev.off()

# Incubation period meta-analysis
# remove entry where upper quantile is equal to median as it results in an error
incub_var <- incub_var %>% filter(article_label_unique != "Francesconi 2003.1")
set.seed(6)
incub_var_ma <- metamean(
  data = incub_var,
  n = population_sample_size,
  mean = xbar,
  sd = sd,
  studlab = article_label,
  median = median,
  q1 = q1,
  q3 = q3,
  min = min,
  max = max,
  method.mean = "Cai",
  method.sd = "Cai",
  sm = "MRAW",
  method.tau = "ML"
)

png(file = "Meta_plots/Variance/incubation_period.png", width = 9500, height = 4000, res = 1000)
forest.meta(incub_var_ma,
  digits = 2, digits.sd = 2, digits.weight = 2, layout = "RevMan5",
  weight.study = "same", col.square.lines = "black", col.square = "dodgerblue3",
  col.study = "black", col.inside = "black", col.diamond.lines = "black",
  col.diamond.common = "dodgerblue3", col.diamond.random = "dodgerblue3",
  at = seq(5, 15, by = 3),
  xlim = c(5, 15),
  xlab = "Incubation period (days)", fontsize = 10
)
dev.off()


# Serial interval meta-analysis
set.seed(6)
serial_var_ma <- metamean(
  data = serial_var,
  n = population_sample_size,
  mean = xbar,
  sd = sd,
  studlab = article_label,
  median = median,
  q1 = q1,
  q3 = q3,
  min = min,
  max = max,
  method.mean = "Cai",
  method.sd = "Cai",
  sm = "MRAW",
  method.tau = "ML"
)

png(file = "Meta_plots/Variance/serial_interval.png", width = 9500, height = 4000, res = 1000)
forest.meta(serial_var_ma,
  digits = 2, digits.sd = 2, digits.weight = 2, layout = "RevMan5",
  weight.study = "same", col.square.lines = "black", col.square = "dodgerblue3",
  col.study = "black", col.inside = "black", col.diamond.lines = "black",
  col.diamond.common = "dodgerblue3", col.diamond.random = "dodgerblue3",
  at = seq(9, 22, by = 3),
  xlim = c(9, 22),
  xlab = "Serial interval (days)", fontsize = 10
)
dev.off()


#############################
# UNCERTAINTY META-ANALYSIS #
#############################

# Uncertainty data (infectious period and serial interval)
inf_unc <- meta_unc %>% filter(delay_short %in% "Infectious period") # 6
serial_unc <- meta_unc %>% filter(delay_short %in% "Serial interval") # 4

# Not included as too few data points:
# incub_unc <- meta_unc %>% filter(delay_short %in% "Incubation period") # 2
# latent_unc <- meta_unc %>% filter(delay_short == "Latent period") # 1
# sotd_unc <- meta_unc %>% filter(delay_short %in% "Symptom onset to death") # 0
# sotr_unc <- meta_unc %>% filter(delay_short %in% "Symptom onset to recovery/non-infectiousness") # 0

# Infectious period meta-analysis
set.seed(6)
inf_unc_ma <- metamean(
  data = inf_unc,
  n = population_sample_size,
  mean = xbar,
  sd = sd,
  studlab = article_label,
  median = median,
  q1 = q1,
  q3 = q3,
  min = min,
  max = max,
  method.mean = "Cai",
  method.sd = "Cai",
  sm = "MRAW",
  method.tau = "ML"
)

png(file = "Meta_plots/Uncertainty/infectious_period.png", width = 9500, height = 4000, res = 1000)
forest.meta(inf_unc_ma,
  digits = 2, digits.sd = 2, digits.weight = 2,
  layout = "RevMan5",
  weight.study = "same",
  col.square.lines = "black", col.square = "maroon",
  col.study = "black", col.inside = "black", col.diamond.lines = "black",
  col.diamond.common = "maroon", col.diamond.random = "maroon",
  at = seq(2, 18, by = 2),
  xlim = c(2, 18),
  xlab = "Mean infectious period (days)", fontsize = 10
)
dev.off()

# Serial interval meta-analysis for uncertainty
set.seed(6)
serial_unc_ma <- metamean(
  data = serial_unc,
  n = population_sample_size,
  mean = xbar,
  sd = sd,
  studlab = article_label,
  median = median,
  q1 = q1,
  q3 = q3,
  min = min,
  max = max,
  method.mean = "Cai",
  method.sd = "Cai",
  sm = "MRAW",
  method.tau = "ML"
)

png(file = "Meta_plots/Uncertainty/serial_interval.png", width = 9500, height = 3500, res = 1000)
forest.meta(serial_unc_ma,
  digits = 2, digits.sd = 2, digits.weight = 2, layout = "RevMan5",
  weight.study = "same", col.square.lines = "black", col.square = "maroon",
  col.study = "black", col.inside = "black", col.diamond.lines = "black",
  col.diamond.common = "maroon", col.diamond.random = "maroon",
  at = seq(8, 22, by = 3),
  xlim = c(8, 22),
  xlab = "Mean serial interval (days)", fontsize = 10
)
dev.off()



# combine variance plots
p1_var <- png::readPNG("Meta_plots/Variance/incubation_period.png", native = TRUE)
p2_var <- png::readPNG("Meta_plots/Variance/serial_interval.png", native = TRUE)
p3_var <- png::readPNG("Meta_plots/Variance/onset_to_death.png", native = TRUE)

# combine uncertainty plots
p1_unc <- png::readPNG("Meta_plots/Uncertainty/infectious_period.png", native = TRUE)
p2_unc <- png::readPNG("Meta_plots/Uncertainty/serial_interval.png", native = TRUE)

# Create plots for each image
plot1_var <- rasterGrob(p1_var, width = 0.9)
plot2_var <- rasterGrob(p2_var, width = 0.9)
plot3_var <- rasterGrob(p3_var, width = 0.9)

heights_var <- c(
  heightDetails(plot1_var),
  heightDetails(plot2_var),
  heightDetails(plot3_var)
)

plot1_unc <- rasterGrob(p1_unc, width = 0.9)
plot2_unc <- rasterGrob(p2_unc, width = 0.9)

heights_unc <- c(
  heightDetails(plot1_unc),
  heightDetails(plot2_unc)
)

# Normalise the heights to make them proportional
p_heights_var <- heights_var / sum(heights_var)
p_heights_unc <- heights_unc / sum(heights_unc)

# Arrange and display the plots in a grid
md_var <- grid.arrange(plot1_var, plot2_var, plot3_var, ncol = 1, heights = p_heights_var)
md_unc <- grid.arrange(plot1_unc, plot2_unc, ncol = 1, heights = p_heights_unc)

ggsave("Meta_plots/Variance/meta_delays_variance.png", plot = md_var, width = 7, height = 9)
ggsave("Meta_plots/Uncertainty/meta_delays_uncertainty.png", plot = md_unc, width = 7, height = 4.5)
