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
    "Seroprevalence_tables/tab_unfiltered.png",
    "Seroprevalence_tables/seroprevalence_history.png",
    "Seroprevalence_tables/seroprevalence_no_history.png"
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
  articles[, c("id", "first_author_surname", "year_publication", "article_label",
               "notes", "doi")],
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
      ),
    # split by history of officially reported EVD cases
    outbreak_history =
      case_when(
        population_country %in% c("Gabon", "Sudan", "South Sudan", "DRC",
                                  "Republic of the Congo", "Guinea",
                                  "Sierra Leone", "Liberia", "Uganda",
                                  "Guinea, Liberia, Sierra Leone") ~ "Yes",
        population_country %in% c("Mali", "Cameroon", "United Kingdom",
                                  "Kenya", "Central African Republic",
                                  "Madagascar", "Tanzania", "Djibouti") ~ "No",
        population_country %in% c("Multi-country: Africa (n = 6)",
                                  "Multi-country: Africa (n = 5)") ~ "Both",
        TRUE ~ NA)
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

# Checked:
# Johnson id 11425 - unspecified survey date, timing, central type
# Johnson id 11421 - unspecified survey date, timing, central type
# Diallo id 16201 - found survey dates, sample type, country
# Nkuba-Ndaye id 18091 - no specific survey dates but outbreak DRC 2018-2020
# Mulangu id 1912 - found survey dates, survey in villages with no history of EVD outbreaks
# Mulangu id 1911 - found survey dates
# Mathiot id 2354 - unspecified survey date, added sample type, looked at zaire and sudan species separately
# Halfmann id 16757 - sample type "Other" as it was survivors and close contacts (either relatives of HCWs)
# Rodhain id 2635 - not duplicate, one Zaire antigens, one Sudan antigens
# WHO/International Study Team id 6471 - extractor noted "Other" is hospital staff contact

# Order data for plots
ordered_dat <- sero_dat %>%
  group_by(population_country) %>%
  mutate(
    range_midpoint =
      ifelse(is.na(parameter_value) & !is.na(parameter_upper_bound),
             (parameter_upper_bound - parameter_lower_bound) /
               2 + parameter_lower_bound, NA
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

sero_dat <- sero_dat %>%
  mutate(parameter_value_type =
           case_when(parameter_value_type %in% "Unspecified" ~ NA,
                     TRUE ~ parameter_value_type),
         # Add asterisk for Rodhain to explain that they're not duplicate entries
         article_label =
           case_when(covidence_id %in% 2635 ~ paste0(article_label, "*"),
                     TRUE ~ article_label)
         )

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

sero_dat_history <- sero_dat %>%
  filter(outbreak_history %in% "Yes")

sero_table_history <- create_table(
  sero_dat_history,
  param = parameter,
  group = "parameter_type",
  qa_filter = FALSE
)

sero_dat_no_history <- sero_dat %>%
  filter(outbreak_history %in% c("No", "Both"))

sero_table_no_history <- create_table(
  sero_dat_no_history,
  param = parameter,
  group = "parameter_type",
  qa_filter = FALSE
)

sero_range_history <- create_range_table(
  sero_dat_history,
  main_group = "population_country",
  main_group_label = "Country",
  sub_group = "population_sample_type", 
  sub_group_label = "Sample Type", 
  qa_filter = FALSE,
  rounding = "none")

sero_range_no_history <- create_range_table(
  sero_dat_no_history,
  main_group = "population_country",
  main_group_label = "Country",
  sub_group = "population_sample_type",
  sub_group_label = "Sample Type",
  qa_filter = FALSE,
  rounding = "none")

# Save
save_as_image(sero_table_qa,
              path = "Seroprevalence_tables/tab_filtered.png"
)

save_as_image(sero_table_all,
              path = "Seroprevalence_tables/tab_unfiltered.png"
)

save_as_image(sero_table_history,
              path = "Seroprevalence_tables/seroprevalence_history.png"
)

save_as_image(sero_table_no_history,
              path = "Seroprevalence_tables/seroprevalence_no_history.png"
)

save_as_image(sero_range_history,
              path = "Seroprevalence_tables/range_history.png"
)

save_as_image(sero_range_no_history,
              path = "Seroprevalence_tables/range_no_history.png"
)

