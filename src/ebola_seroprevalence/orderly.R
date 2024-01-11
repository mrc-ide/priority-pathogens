## Seroprevalence

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

orderly_parameters(pathogen = "EBOLA")

orderly_artefact(
  "Plots and tables for severity parameters",
  c(
    "Seroprevalence_plots/plot_filtered.png",
    "Seroprevalence_tables/tab_filtered.png",
    "Seroprevalence_tables/tab_unfiltered.png"
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

parameter <- "Seroprevalence"



df <- left_join(
  params,
  articles[, c("id", "first_author_surname", "year_publication", "article_label")],
  by = "id"
) %>%
  arrange(article_label, -year_publication)

sero_dat <- df %>%
  mutate(
    population_country = as.factor(population_country)
  ) %>%
  filter(parameter_class == parameter) %>%
  filter(parameter_from_figure == "FALSE") %>%
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
      )
  ) %>%
  # modify standard deviation and standard error to visualise uncertainty in plots
mutate(
  parameter_uncertainty_type =
    case_when(is.na(parameter_uncertainty_type) &
                parameter_uncertainty_singe_type == "Standard Deviation" ~
                "Standard Deviation",
              is.na(parameter_uncertainty_type) &
                parameter_uncertainty_singe_type == "Standard Error" ~
                "Standard Error",
              TRUE ~ parameter_uncertainty_type),
  parameter_uncertainty_lower_value =
    case_when(parameter_uncertainty_type %in% c("Standard Deviation", "Standard Error") ~
                parameter_value - parameter_uncertainty_single_value,
              TRUE ~ parameter_uncertainty_lower_value),
  parameter_uncertainty_upper_value =
    case_when(parameter_uncertainty_type %in% c("Standard Deviation", "Standard Error") ~
                parameter_value + parameter_uncertainty_single_value,
              TRUE ~ parameter_uncertainty_upper_value)
)

# Order data for plots
ordered_dat <- sero_dat %>%
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
dir.create("Seroprevalence_plots")
dir.create("Seroprevalence_tables")


## PLOTS

# Plot with qa_filter of >=50
plot_qa <- create_plot(
  ordered_dat,
  param = parameter,
  qa_filter = TRUE,
  facet_by = "parameter_type",
  symbol_col_by = "population_country"
)

# Save
ggsave("Seroprevalence_plots/plot_filtered.png", plot_qa,
       width = 9, height = 8, units = "in", bg = "white"
)


## TABLES
# Note: These tables don't include outbreak. Timing of seroprevalence surveys
# do not necessarily correspond to a specific outbreak, and this would complicate
# assigning outbreaks based on survey date.

sero_table_qa <- create_table(
  sero_dat,
  param = parameter,
  group = "parameter_type",
  qa_filter = TRUE
)

sero_table_all <- create_table(
  sero_dat,
  param = parameter,
  group = "parameter_type",
  qa_filter = FALSE
)

# Save
save_as_image(sero_table_qa,
              path = "Seroprevalence_tables/tab_filtered.png"
)

save_as_image(sero_table_all,
              path = "Seroprevalence_tables/tab_unfiltered.png"
)
