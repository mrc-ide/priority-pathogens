## Severity

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

# orderly_parameters(pathogen = "EBOLA")
orderly_parameters(pathogen = NULL)

orderly_artefact(
  "Plots and tables for severity parameters",
  c(
    "Severity_plots/plot_outbreak_filtered.png",
    "Severity_plots/plot_country_filtered.png",
    "Severity_plots/plot_outbreak_unfiltered.png",
    "Severity_plots/plot_country_unfiltered.png",
    "Severity_tables/qa_filtered/tab_filtered.png",
    "Severity_tables/unfiltered/tab_unfiltered.png"
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

parameter <- "Severity"



df <- left_join(
  params,
  articles[, c("id", "first_author_surname", "year_publication", "article_label")],
  by = "id"
) %>%
  arrange(article_label, -year_publication)

sev_dat <- df %>%
  mutate(
    population_country = as.factor(population_country)
  ) %>%
  filter(parameter_class == parameter) %>%
  filter(parameter_from_figure == "FALSE") %>%
  filter(!covidence_id == 23507) %>% # incorrect entry
  filter(!covidence_id == 5654) %>% # dupe of separate Cherif 2018, but less info
  filter(!covidence_id == 2124) %>% # dupe of separate Sadek 1999, but less info
  group_by(parameter_type) %>%
  mutate(
    article_label_unique = make.unique(article_label),
    article_label =
      case_when(article_label == "WHO/International Study Team 1978" ~
        "WHO/Int. Study Team 1978", TRUE ~ article_label),
    outbreak = order_ebola_outbreaks(outbreak),
    cfr_ifr_method = case_when(
      is.na(cfr_ifr_method) ~ "Unspecified", TRUE ~ cfr_ifr_method
    ),
    population_study_start_month =
      factor(population_study_start_month,
        levels = c(
          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
          "Sep", "Oct", "Nov", "Dec"
        )
      ),
    # Convert ratios to percentages to make CFRs comparable
    parameter_value =
      case_when(
        covidence_id %in% c(5252, 18371, 5868) ~
          parameter_value * 100,
        TRUE ~ parameter_value
      ),
    parameter_uncertainty_lower_value =
      case_when(
        covidence_id %in% c(5252, 5868) ~
          parameter_uncertainty_lower_value * 100,
        TRUE ~ parameter_uncertainty_lower_value
      ),
    parameter_uncertainty_upper_value =
      case_when(
        covidence_id %in% c(5252, 5868) ~
          parameter_uncertainty_upper_value * 100,
        TRUE ~ parameter_uncertainty_upper_value
      ),
    comb_uncertainty =
      case_when(
        covidence_id == 5252 & comb_uncertainty == "0.8 - 0.94" ~
          "80 - 94",
        covidence_id == 5868 & comb_uncertainty == "0.14 - 0.71" ~
          "14 - 71",
        TRUE ~ comb_uncertainty
      ),
    parameter_lower_bound =
      case_when(
        covidence_id %in% c(18372, 5871) ~
          parameter_lower_bound * 100,
        TRUE ~ parameter_lower_bound
      ),
    parameter_upper_bound =
      case_when(
        covidence_id %in% c(18372, 5871) ~
          parameter_upper_bound * 100,
        TRUE ~ parameter_upper_bound
      ),
    parameter_bounds =
      case_when(
        covidence_id == 18372 & parameter_bounds == "0.5 - 0.8" ~
          "50 - 80",
        covidence_id == 5871 & parameter_bounds == "0.48 - 0.74" ~
          "48 - 74",
        TRUE ~ parameter_bounds
      ),
    parameter_unit =
      case_when(
        covidence_id %in% c(5252, 18371, 18372, 5868, 5871) ~
          "Percentage (%)",
        TRUE ~ parameter_unit
      )
  )

# Order data for plots
ordered_dat <- sev_dat %>%
  group_by(population_country) %>%
  mutate(
    range_midpoint =
      ifelse(is.na(parameter_value) & !is.na(parameter_upper_bound),
        parameter_upper_bound - parameter_lower_bound, NA
      ),
    temp_order_by = ifelse(!is.na(parameter_value),
      parameter_value,
      range_midpoint
    ),
    # temporary before checks:
    parameter_value_type =
      case_when(
        is.na(parameter_value_type) ~ "Unspecified",
        TRUE ~ parameter_value_type
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
dir.create("Severity_plots")
dir.create("Severity_tables")
dir.create("Severity_tables/qa_filtered")
dir.create("Severity_tables/unfiltered")


# PLOTS/TABLES

# Plot with qa_filter of >=50
plot_outbreak_qa <- create_plot(
  ordered_dat,
  param = parameter,
  qa_filter = TRUE,
  symbol_shape_by = "cfr_ifr_method",
  facet_by = "outbreak",
  symbol_col_by = "population_country"
)

plot_country_qa <- create_plot(
  ordered_dat,
  param = parameter,
  qa_filter = TRUE,
  symbol_shape_by = "cfr_ifr_method",
  facet_by = "population_country",
  symbol_col_by = "outbreak"
)

# Plot with NO qa_filter
plot_outbreak_all <- create_plot(
  ordered_dat,
  param = parameter,
  qa_filter = FALSE,
  symbol_shape_by = "cfr_ifr_method",
  facet_by = "outbreak",
  symbol_col_by = "population_country"
)

plot_country_all <- create_plot(
  ordered_dat,
  param = parameter,
  qa_filter = FALSE,
  symbol_shape_by = "cfr_ifr_method",
  facet_by = "population_country",
  symbol_col_by = "outbreak"
)

# Save
ggsave("Severity_plots/plot_outbreak_filtered.png", plot_outbreak_qa,
  width = 9, height = 16, units = "in", bg = "white"
)

ggsave("Severity_plots/plot_country_filtered.png", plot_country_qa,
  width = 9, height = 16, units = "in", bg = "white"
)

ggsave("Severity_plots/plot_outbreak_unfiltered.png", plot_outbreak_all,
  width = 9, height = 21, units = "in", bg = "white"
)

ggsave("Severity_plots/plot_country_unfiltered.png", plot_country_all,
  width = 9, height = 21, units = "in", bg = "white"
)


# CFR table with qa_filter of >=50
severity_table_qa <- create_table(
  sev_dat,
  param = parameter,
  qa_filter = TRUE,
  rounding = "integer"
)

# CFR table with NO qa_filter
severity_table_all <- create_table(
  sev_dat,
  param = parameter,
  qa_filter = FALSE,
  rounding = "integer"
)

# Save
save_as_image(severity_table_qa,
  path = "Severity_tables/qa_filtered/tab_filtered.png"
)

save_as_image(severity_table_all,
  path = "Severity_tables/unfiltered/tab_unfiltered.png"
)


# Create summary tables giving the range of central values by specified groups

range_outbreak_country <- create_range_table(
  df = ordered_dat,
  main_group = "outbreak", main_group_label = "Outbreak",
  sub_group = "population_country", sub_group_label = "Country",
  qa_filter = TRUE, rounding = "integer"
)

range_country_outbreak <- create_range_table(
  df = ordered_dat,
  main_group = "population_country", main_group_label = "Country",
  sub_group = "outbreak", sub_group_label = "Outbreak",
  qa_filter = TRUE, rounding = "integer"
)

range_dat <- ordered_dat %>%
  filter(ebola_species != "Unspecified")

range_species_outbreak <- create_range_table(
  df = range_dat,
  main_group = "ebola_species", main_group_label = "Species",
  sub_group = "outbreak", sub_group_label = "Outbreak",
  qa_filter = TRUE, rounding = "integer"
)

# Save
save_as_image(range_outbreak_country,
  path = "Severity_tables/qa_filtered/range_outbreak_country.png"
)
save_as_image(range_country_outbreak,
  path = "Severity_tables/qa_filtered/range_country_outbreak.png"
)
save_as_image(range_species_outbreak,
  path = "Severity_tables/qa_filtered/range_species_outbreak.png"
)
