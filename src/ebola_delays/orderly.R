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
    "Delay_tables/unfiltered/exposure_table.png",
    "Delay_tables/unfiltered/other_table.png",
    "Delay_tables/qa_filtered/symp_ranges_table.png",
    "Delay_tables/qa_filtered/adm_ranges_table.png",
    "Delay_tables/qa_filtered/infp_ranges_table.png",
    "Delay_tables/qa_filtered/select_ranges_table.png",
    "Delay_tables/qa_filtered/exposure_table.png",
    "Delay_tables/qa_filtered/other_table.png",
    "Meta_plots/incubation_period.png",
    "Meta_plots/serial_interval.png",
    "Meta_plots/onset_to_death.png",
    "Meta_plots/meta_delays_variance_unfiltered.png",
    "Meta_plots/meta_delays_variance_QAfiltered.png"
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
  articles[, c(
    "covidence_id", "first_author_surname", "year_publication",
    "article_label", "doi"
  )],
  by = "covidence_id"
) %>%
  arrange(article_label, -year_publication)

delay_dat <- df %>%
  mutate(
    population_country = as.factor(population_country)
  ) %>%
  filter(parameter_class %in% parameter) %>%
  filter(!parameter_from_figure %in% TRUE) %>%
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
    delay_short = str_replace(delay_short, "\\bigg\\b", "IgG"),
    delay_short = str_replace(delay_short, "\\bigm\\b", "IgM"),
    delay_short = str_replace(delay_short, "\\bwho\\b", "WHO"),
    delay_short = str_replace(delay_short, "\\bWho\\b", "WHO"),
    delay_short = str_replace(delay_short, "\\brna\\b", "RNA"),
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
        covidence_id %in% c(16599, 17956, 17097, 23669) ~ "Days",
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
        (parameter_upper_bound - parameter_lower_bound) /
          2 + parameter_lower_bound, NA
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

#########
# PLOTS #
#########

# Symptom onset to X with qa_filter of >=50
symp_dat <- ordered_dat %>%
  filter(delay_start %in% "Symptom onset") %>%
  filter(!delay_short %in% c("Symptom onset to other")) %>%
  mutate(
    delay_short =
      case_when(
        delay_short %in% c(
          "Symptom onset to death in hospital",
          "Symptom onset to death in community"
        ) ~
          "Symptom onset to death", TRUE ~ delay_short
      ),
    delay_short =
      factor(delay_short,
        levels = c(
          "Symptom onset to test",
          "Symptom onset to test result",
          "Symptom onset to negative test",
          "Symptom onset to IgM antibody detection",
          "Symptom onset to IgG antibody detection",
          "Symptom onset to reporting",
          "Symptom onset to WHO notification",
          "Symptom onset to seeking care",
          "Symptom onset to quarantine",
          "Symptom onset to diagnosis",
          "Symptom onset to admission to care",
          "Symptom onset to recovery/non-infectiousness",
          "Symptom onset to first undetectable viremia",
          "Symptom onset to discharge from care",
          "Symptom onset to death"
        )
      )
  )

symp_plot_qa <- create_plot(
  symp_dat,
  param = parameter,
  qa_filter = TRUE,
  facet_by = "delay_short",
  symbol_col_by = "outbreak"
)

# Admission to X with qa_filter of >=50
adm_dat <- ordered_dat %>%
  filter(delay_start == "Admission to care") %>%
  mutate(
    delay_short =
      factor(delay_short,
        levels = c(
          "Admission to care to recovery/non-infectiousness",
          "Admission to care to discharge from care",
          "Admission to care to death",
          "Admission to care to death/discharge",
          "Admission to care to death/recovery",
          "Admission to care to negative test"
        )
      )
  )

adm_plot_qa <- create_plot(
  adm_dat,
  param = parameter,
  qa_filter = TRUE,
  facet_by = "delay_short",
  symbol_col_by = "outbreak"
)

# Infection process with qa_filter of >=50
infp_dat <- ordered_dat %>%
  filter(delay_start %in% "Infection process") %>%
  mutate(
    delay_short =
      factor(delay_short,
        levels = c(
          "Incubation period", "Latent period",
          "Infectious period", "Generation time",
          "Serial interval"
        )
      )
  )

infp_plot_qa <- create_plot(
  infp_dat,
  param = parameter,
  qa_filter = TRUE,
  facet_by = "delay_short",
  symbol_col_by = "outbreak"
)

# Death to burial with qa_filter >=50
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

# Exposure/infection table with qa_filter of >=50
eti_table_qa <- create_table(
  delay_dat,
  param = parameter,
  delay_type = "Exposure/infection",
  group = "delay_short",
  qa_filter = TRUE
)

# All delays other than symptom, admission, and infection process with qa_filter of >=50
other_dat <- ordered_dat %>%
  filter(!delay_start %in% c(
    "Symptom onset",
    "Admission to care",
    "Infection process"
  )) %>%
  group_by(delay_start)

other_table_qa <- create_table(
  other_dat,
  param = parameter,
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
save_as_image(eti_table_qa,
  path = "Delay_tables/qa_filtered/exposure_table.png"
)
save_as_image(other_table_qa,
  path = "Delay_tables/qa_filtered/other_table.png"
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

# Exposure/infection table with NO qa_filter
eti_table_all <- create_table(
  delay_dat,
  param = parameter,
  delay_type = "Exposure/infection",
  group = "delay_short",
  qa_filter = FALSE
)

# All delays other than symptom, admission, and infection process with NO qa_filter
other_table_all <- create_table(
  other_dat,
  param = parameter,
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
save_as_image(eti_table_all,
  path = "Delay_tables/unfiltered/exposure_table.png"
)
save_as_image(other_table_all,
  path = "Delay_tables/unfiltered/other_table.png"
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


##########################
# VARIANCE META-ANALYSIS #
##########################

# Note SDs were checked and assigned either sd_sample or sd_mean
# Entries we can use for the meta-analysis:
# mean and SD of sample (everything to be converted to this format)
# mean and SE (can convert SE to SD using sample sizes before supplying to metamean)
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
      parameter_uncertainty_upper_value, TRUE ~ NA)
  ) %>%
  # convert SEs for means into SD using sample size
  mutate(
    sd = case_when(
      is.na(sd) & !is.na(serr) ~ serr * sqrt(population_sample_size),
      TRUE ~ sd
    )
  ) %>%
  # remove converted uncertainty variables
  select(-c(serr)) %>%
  # remove rows where all uncertainty variables are empty
  filter(!is.na(sd) | !is.na(q1) | !is.na(q3) | !is.na(min) | !is.na(max))

# meta for variance: sd of sample, standard error, IQR, range
meta_var <- meta_dat %>%
  filter(parameter_uncertainty_singe_type %in% c(
    "SD of the sample",
    "Standard Error"
  ) |
    parameter_uncertainty_type %in% c("IQR", "Range"))

# Variance data (incubation period, serial interval, symptom onset to death)
sotd_var <- meta_var %>% filter(delay_short %in% "Symptom onset to death") # 18 without filter
incub_var <- meta_var %>% filter(delay_short %in% "Incubation period") # 8 (all qa scores >50)
serial_var <- meta_var %>% filter(delay_short %in% "Serial interval") # 8 without filter

sotd_var_qa <- meta_var %>% filter(delay_short %in% "Symptom onset to death") %>%
  filter(article_qa_score >= 50) # 13 with qa filter
serial_var_qa <- meta_var %>% filter(delay_short %in% "Serial interval") %>%
  filter(article_qa_score >= 50) # 5 with qa filter

# Not included as too few data points:
# sotr_var <- meta_var %>% filter(delay_short %in% "Symptom onset to recovery/non-infectiousness") # 2
# inf_var <- meta_var %>% filter(delay_short %in% "Infectious period") # 1

# Symptom onset to death meta-analysis WITHOUT FILTER
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

png(file = "Meta_plots/onset_to_death.png", width = 9500, height = 6000, res = 1000)
forest.meta(sotd_var_ma,
  digits = 2, digits.sd = 2, digits.weight = 2, layout = "RevMan5",
  weight.study = "same", col.square.lines = "black", col.square = "dodgerblue3",
  col.study = "black", col.inside = "black", col.diamond.lines = "black",
  col.diamond.common = "dodgerblue3", col.diamond.random = "dodgerblue3",
  at = seq(0, 15, by = 3),
  xlim = c(0, 15),
  xlab = "Symptom onset to death (days)", fontsize = 10
)
dev.off()

# Symptom onset to death meta-analysis WITH QA FILTER
set.seed(6)
sotd_var_ma_qa <- metamean(
  data = sotd_var_qa,
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

png(file = "Meta_plots/onset_to_death_qafilter.png", width = 9500, height = 6000, res = 1000)
forest.meta(sotd_var_ma_qa,
            digits = 2, digits.sd = 2, digits.weight = 2, layout = "RevMan5",
            weight.study = "same", col.square.lines = "black", col.square = "dodgerblue3",
            col.study = "black", col.inside = "black", col.diamond.lines = "black",
            col.diamond.common = "dodgerblue3", col.diamond.random = "dodgerblue3",
            at = seq(0, 15, by = 3),
            xlim = c(0, 15),
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

png(file = "Meta_plots/incubation_period.png", width = 9500, height = 3500, res = 1000)
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


# Serial interval meta-analysis NO FILTER
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

png(file = "Meta_plots/serial_interval.png", width = 9500, height = 3800, res = 1000)
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

# Serial interval meta-analysis WITH QA FILTER
set.seed(6)
serial_var_ma_qa <- metamean(
  data = serial_var_qa,
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

png(file = "Meta_plots/serial_interval_qafilter.png", width = 9500, height = 3800, res = 1000)
forest.meta(serial_var_ma_qa,
            digits = 2, digits.sd = 2, digits.weight = 2, layout = "RevMan5",
            weight.study = "same", col.square.lines = "black", col.square = "dodgerblue3",
            col.study = "black", col.inside = "black", col.diamond.lines = "black",
            col.diamond.common = "dodgerblue3", col.diamond.random = "dodgerblue3",
            at = seq(9, 22, by = 3),
            xlim = c(9, 22),
            xlab = "Serial interval (days)", fontsize = 10
)
dev.off()

# combine variance plots
p1_var <- png::readPNG("Meta_plots/incubation_period.png", native = TRUE)
p2_var <- png::readPNG("Meta_plots/serial_interval.png", native = TRUE)
p3_var <- png::readPNG("Meta_plots/onset_to_death.png", native = TRUE)

p1_qa <- png::readPNG("Meta_plots/incubation_period.png", native = TRUE)  # no difference to above
p2_qa <- png::readPNG("Meta_plots/serial_interval_qafilter.png", native = TRUE)
p3_qa <- png::readPNG("Meta_plots/onset_to_death_qafilter.png", native = TRUE)

# Create plots for each image
plot1_var <- rasterGrob(p1_var, width = 0.9)
plot2_var <- rasterGrob(p2_var, width = 0.9)
plot3_var <- rasterGrob(p3_var, width = 0.9)

plot1_qa <- rasterGrob(p1_qa, width = 0.9)
plot2_qa <- rasterGrob(p2_qa, width = 0.9)
plot3_qa <- rasterGrob(p3_qa, width = 0.9)

heights_var <- c(
  heightDetails(plot1_var),
  heightDetails(plot2_var),
  heightDetails(plot3_var)
)

heights_qa <- c(
  heightDetails(plot1_qa),
  heightDetails(plot2_qa),
  heightDetails(plot3_qa)
)

# Normalise the heights to make them proportional
p_heights_var <- heights_var / sum(heights_var)
p_heights_qa <- heights_qa / sum(heights_qa)

# Arrange and display the plots in a grid
md_var <- grid.arrange(plot1_var, plot2_var, plot3_var, ncol = 1, heights = p_heights_var)
md_qa <- grid.arrange(plot1_qa, plot2_qa, plot3_qa, ncol = 1, heights = p_heights_qa)

ggsave("Meta_plots/meta_delays_variance_unfiltered.png", plot = md_var, width = 7, height = 10)
ggsave("Meta_plots/meta_delays_variance_QAfiltered.png", plot = md_qa, width = 7, height = 10)
