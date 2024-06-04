## Attack rate

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
  "Table for attack rate",
  c(
    "Attack_rate_results/primary_table_filtered.png",
    "Attack_rate_results/primary_table_unfiltered.png",
    "Attack_rate_results/secondary_table_filtered.png",
    "Attack_rate_results/secondary_table_unfiltered.png"
  )
)

orderly_parameters(pathogen = "EBOLA")

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

parameter <- "Attack rate"

ar_dat <- left_join(
  params,
  articles[, c(
    "covidence_id", "first_author_surname", "year_publication",
    "article_label", "doi", "notes"
  )],
  by = "covidence_id"
) %>%
  arrange(article_label, -year_publication) %>%
  mutate(
    population_country = as.factor(population_country),
    article_label_unique = make.unique(article_label),
    article_label =
      case_when(article_label == "WHO/International Study Team 1978" ~
                  "WHO/Int. Study Team 1978", TRUE ~ article_label),
    outbreak = order_ebola_outbreaks(outbreak),
    ebola_species = factor(ebola_species, levels = c(
      sort(setdiff(unique(ebola_species), "Unspecified"), decreasing = FALSE),
      "Unspecified"
    )),
    population_study_start_month =
      factor(population_study_start_month,
        levels = c(
          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
          "Sep", "Oct", "Nov", "Dec"
        )
      ),
    parameter_value_type =
      case_when(parameter_value_type %in% c("Unspecified", "Other")
      ~ "Other/Unspecified", TRUE ~ parameter_value_type),
    # Deal with "approximate" sample size identified in one paper (not in
    # cleaning.R to avoid messing with variable class)
    population_sample_size = as.character(population_sample_size),
    population_sample_size =
      case_when(
        covidence_id %in% 2240 ~ "~1000",
        TRUE ~ population_sample_size
      ),
    # Add to legend that we're making an assumption to translate the 3 Fang
    # estimates with "No units" into percentages:
    parameter_value =
       case_when(parameter_unit %in% "No units" ~ parameter_value * 100,
                 TRUE ~ parameter_value),
    parameter_upper_bound =
      case_when(parameter_unit %in% "No units" ~ parameter_upper_bound * 100,
                TRUE ~ parameter_upper_bound),
    parameter_lower_bound =
      case_when(parameter_unit %in% "No units" ~ parameter_lower_bound * 100,
                TRUE ~ parameter_lower_bound),
    parameter_uncertainty_upper_value =
      case_when(parameter_unit %in% "No units" ~ parameter_uncertainty_upper_value * 100,
                TRUE ~ parameter_uncertainty_upper_value),
    parameter_uncertainty_lower_value =
      case_when(parameter_unit %in% "No units" ~ parameter_uncertainty_lower_value * 100,
                TRUE ~ parameter_uncertainty_lower_value),
    comb_uncertainty =
      case_when(parameter_unit %in% "No units" ~
                  paste(parameter_uncertainty_lower_value, "-", parameter_uncertainty_upper_value),
                TRUE ~ comb_uncertainty),
    parameter_bounds =
      case_when(parameter_unit %in% "No units" & !is.na(parameter_lower_bound) ~
                  paste(parameter_lower_bound, "-", parameter_upper_bound),
                TRUE ~ parameter_bounds),
    parameter_unit =
      case_when(parameter_unit %in% "No units" ~ "Percentage (%)",
                TRUE ~ parameter_unit)
  ) %>%
  filter(parameter_class %in% parameter) %>%
  filter(!parameter_from_figure %in% TRUE)


# Check for NA and unspecified and add fixes to cleaning.R:
ar_dat$parameter_value_type # check 14 NA (x)
ar_dat$population_country # all fine
ar_dat$survey_date # all fine
ar_dat$outbreak # all fine
ar_dat$parameter_unit # check 8 NA - 3x "no units" checked (x)
ar_dat$population_sample_type # check 2 NA - Emond 1977 (id 6472) very vague, keep "Unspecified"
ar_dat$population_group # Emond 1977 (id 6472) very vague, keep "Unspecified"
ar_dat$population_sample_size
ar_dat$exponent
ar_dat$attack_rate_type

# Create directory for results
dir.create("Attack_rate_results")

# Order data
ordered_dat <- ar_dat %>%
  group_by(population_country) %>%
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
    ),
    parameter_value = as.character(parameter_value) # otherwise percentages .000
  )

# Create table
primary_dat <- ordered_dat %>% filter(attack_rate_type %in% "Primary")
secondary_dat <- ordered_dat %>% filter(attack_rate_type %in% "Secondary")

print(sprintf("n model studies primary: %d", length(unique(primary_dat$article_label))))
print(sprintf("m models primary: %d", length(primary_dat$article_label)))

print(sprintf("n model studies secondary: %d", length(unique(secondary_dat$article_label))))
print(sprintf("m models secondary: %d", length(secondary_dat$article_label)))

primary_tab_qa <- create_table(
  primary_dat,
  param = parameter,
  qa_filter = TRUE
)

primary_tab <- create_table(
  primary_dat,
  param = parameter,
  qa_filter = FALSE
)

secondary_tab_qa <- create_table(
  secondary_dat,
  param = parameter,
  qa_filter = TRUE
)

secondary_tab <- create_table(
  secondary_dat,
  param = parameter,
  qa_filter = FALSE
)

# Save
save_as_image(primary_tab_qa, path = "Attack_rate_results/primary_table_filtered.png")
save_as_image(primary_tab, path = "Attack_rate_results/primary_table_unfiltered.png")
save_as_image(secondary_tab_qa, path = "Attack_rate_results/secondary_table_filtered.png")
save_as_image(secondary_tab, path = "Attack_rate_results/secondary_table_unfiltered.png")
