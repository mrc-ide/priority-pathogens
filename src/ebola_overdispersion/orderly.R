## Overdispersion

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
  "Plot and table for overdispersion",
  c(
    "Overdispersion_results/plot.png",
    "Overdispersion_results/table.png"
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

parameter <- "Overdispersion"

over_dat <- left_join(
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
    # Distribution types were all checked by AC
    distribution_type =
      case_when(!is.na(distribution_type) ~ "Negative-Binomial",
                TRUE ~ distribution_type),
    # Create new "distribution_models" variable to filter out anything other than
    # distributions which model offspring distribution
    distribution_models =
      case_when(covidence_id %in% 2065 & parameter_lower_bound %in% 0.72 ~ "Daily case counts",
                covidence_id %in% 2065 & parameter_lower_bound %in% 0.72 ~ "Daily deaths counts",
                TRUE ~ "Offspring distribution"),
    parameter_value_type =
      case_when(parameter_value_type %in% c("Unspecified", "Other")
      ~ "Other/Unspecified", TRUE ~ parameter_value_type)
  ) %>%
  filter(parameter_class %in% parameter) %>%
  filter(!parameter_from_figure %in% TRUE)


# Check for NA and unspecified:
over_dat$method_r
over_dat$parameter_value_type
over_dat$population_country
over_dat$survey_date
over_dat$outbreak
over_dat$distribution_type

# Pull numbers for text
n_param <- over_dat %>% filter(distribution_models %in% "Offspring distribution")
n_param %>% nrow() # 15 parameters
length(unique(n_param$covidence_id)) # 12 articles
n_param %>% filter(outbreak %in% "West Africa 2013-2016") %>% nrow() # 13 WA
table(n_param$ebola_species)

# Create directory for results (all QA scores > 50)
dir.create("Overdispersion_results")

# Order data
ordered_dat <- over_dat %>%
  group_by(population_country) %>%
  filter(distribution_models %in% "Offspring distribution") %>%
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

overd_plot <- create_plot(
  ordered_dat,
  param = parameter,
  qa_filter = FALSE,
  facet_by = "outbreak",
  symbol_shape_by = "parameter_value_type",
  symbol_col_by = "population_country"
)

# Save
ggsave("Overdispersion_results/plot.png", overd_plot,
  width = 9, height = 5, units = "in", bg = "white"
)

# Create table
overd_tab <- create_table(
  ordered_dat,
  param = parameter,
  qa_filter = FALSE
)

# Save
save_as_image(overd_tab, path = "Overdispersion_results/table.png")
