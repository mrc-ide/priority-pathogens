# Task to summarise reproduction number estimates

library(dplyr)
library(orderly2)
library(readr)
library(ggplot2)
library(ggforce)
library(flextable)
library(officer)
library(purrr)
library(janitor)
library(stringr)

orderly_strict_mode()

# Outputs
orderly_artefact(
  "Plots and tables for reproduction number parameters",
  c(
    "R_plots/basic_r_plot_filtered.png",
    "R_plots/eff_r_plot_filtered.png",
    
    "R_tables/qa_filtered/basic_r_tab_filtered.png",
    "R_tables/qa_filtered/eff_r_tab_filtered.png",
    "R_tables/unfiltered/basic_r_tab_all.png",
    "R_tables/unfiltered/eff_r_tab_all.png",
    
    "R_tables/qa_filtered/range_outbreak.png",
    "R_tables/qa_filtered/range_country.png",
    "R_tables/qa_filtered/range_species.png"
  )
)

#orderly_parameters(pathogen = "EBOLA")
orderly_parameters(pathogen = NULL)

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
orderly_resource("functions.R")

# Load data
articles <- read_csv("articles.csv")
params <- read_csv("parameters.csv")
source("functions.R")

parameter <- "Reproduction number"

df <- left_join(
  params,
  articles[, c("id", "first_author_surname", "year_publication", "article_label")],
  by = "id"
) %>%
  arrange(article_label, -year_publication)

species_levels <- factor(df$ebola_species, levels = c(sort(
  setdiff(unique(df$ebola_species), "Unspecified"), decreasing = TRUE), "Unspecified"))

df_plot <- df %>%
  filter(parameter_class == parameter) %>%
  filter(parameter_from_figure == "FALSE") %>%
  filter(!covidence_id == 4966) %>% # entry without values
  group_by(parameter_type) %>%
  mutate(
    parameter_type_short =
      ifelse(parameter_type == "Reproduction number (Basic R0)", "Basic (R0)",
        ifelse(parameter_type == "Reproduction number (Effective, Re)",
          "Effective (Re)", NA
        )
      ),
    article_label_unique = make.unique(article_label),
    outbreak = factor(outbreak, levels = c(
      "DRC, 1976",
      "South Sudan, 1976",
      "DRC & South Sudan, 1976",
      "DRC, 1995",
      "Uganda, 2000-2001",
      "Uganda, 2007",
      "DRC, 2008-2009",
      "DRC, 2012",
      "West Africa 2013-2016",
      "DRC, 2014",
      "DRC, 2018-2020",
      "Guinea, 2021",
      "Uganda, 2022",
      "Unspecified"
    )),
    ebola_species = factor(ebola_species, levels = c(
      sort(setdiff(unique(ebola_species), "Unspecified"), decreasing = FALSE),
      "Unspecified")),
    population_study_start_month =
      factor(population_study_start_month,
        levels = c(
          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
          "Sep", "Oct", "Nov", "Dec"
        )
      )
  )

ordered_dat <- df_plot %>%
  group_by(population_country) %>%
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
dir.create("R_plots")
dir.create("R_tables")
dir.create("R_tables/qa_filtered")
dir.create("R_tables/unfiltered")

# PLOTS

# Plots with qa_filter of >=50
basic_r_plot_qa <- create_plot(
  ordered_dat,
  param = parameter,
  r_type = "Basic (R0)",
  qa_filter = TRUE
)

eff_r_plot_qa <- create_plot(
  ordered_dat,
  param = parameter,
  r_type = "Effective (Re)",
  qa_filter = TRUE
)

# Save
ggsave("R_plots/basic_r_plot_filtered.png", basic_r_plot_qa,
  width = 8, height = 11, units = "in", bg = "white"
)

ggsave("R_plots/eff_r_plot_filtered.png", eff_r_plot_qa,
  width = 8, height = 5, units = "in", bg = "white"
)

# SUMMARY TABLES

# Tables with qa_filter of >=50
basic_r_tab_qa <- create_table(
  ordered_dat,
  param = parameter,
  r_type = "Basic (R0)",
  qa_filter = TRUE
  )

eff_r_tab_qa <- create_table(
  ordered_dat,
  param = parameter,
  r_type = "Effective (Re)",
  qa_filter = TRUE
  )

# Tables with no qa_filter
basic_r_tab <- create_table(
  ordered_dat,
  param = parameter,
  r_type = "Basic (R0)",
  qa_filter = FALSE)

eff_r_tab <- create_table(
  ordered_dat,
  param = parameter,
  r_type = "Effective (Re)",
  qa_filter = FALSE)

# Save
save_as_image(basic_r_tab_qa, path = "R_tables/qa_filtered/basic_r_tab_filtered.png")
save_as_image(eff_r_tab_qa, path = "R_tables/qa_filtered/eff_r_tab_filtered.png")
save_as_image(basic_r_tab, path = "R_tables/unfiltered/basic_r_tab_all.png")
save_as_image(eff_r_tab, path = "R_tables/unfiltered/eff_r_tab_all.png")


# For the basic reproduction number, summary table giving the range of central
# values for each mean, median, and other/unspecified parameter types:

range_dat <- ordered_dat %>%
  filter(
    parameter_type_short == "Basic (R0)",
    !parameter_value_type == "Standard Deviation"
  ) %>%
  mutate(
    parameter_value_type =
      case_when(parameter_value_type %in% c("Unspecified", "Other")
      ~ "Other_Unspecified", TRUE ~ parameter_value_type)
  )

range_outbreak <- create_range_table(
  df = range_dat,
  group_tab = "Outbreak",
  value_type = c("Mean", "Median", "Other_Unspecified"),
  qa_filter = TRUE)

range_country <- create_range_table(
  df = range_dat,
  group_tab = "Country",
  value_type = c("Mean", "Median", "Other_Unspecified"),
  qa_filter = TRUE)

range_species <- create_range_table(
  df = range_dat,
  group_tab = "Species",
  value_type = c("Mean", "Median", "Other_Unspecified"),
  qa_filter = TRUE)

# Save
save_as_image(range_outbreak, path = "R_tables/qa_filtered/range_outbreak.png")
save_as_image(range_country, path = "R_tables/qa_filtered/range_country.png")
save_as_image(range_species, path = "R_tables/qa_filtered/range_species.png")
