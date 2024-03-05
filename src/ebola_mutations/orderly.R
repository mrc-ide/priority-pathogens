## Mutations - mutation rate, evolutionary rate, substitution rate

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
  "Plot and table for mutations",
  c(
    "Mutation_results/qa_filtered/plot.png",
    "Mutation_results/qa_unfiltered/plot.png",
    "Mutation_results/qa_filtered/table.png",
    "Mutation_results/qa_unfiltered/table.png"
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

parameter <- "Mutations"

mut_dat <- left_join(
  params,
  articles[, c(
    "covidence_id", "first_author_surname", "year_publication",
    "article_label", "doi", "notes"
  )],
  by = "covidence_id"
) %>%
  arrange(article_label, -year_publication) %>%
  mutate(
    parameter_type = str_replace(parameter_type, "Mutations - ", ""),
    parameter_type = str_replace(parameter_type, "Mutations – ", ""),
    parameter_type = str_to_sentence(parameter_type),
    population_country = as.factor(population_country),
    # unique for each entry of article so they plot on separate lines
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
    parameter_value_type =
      case_when(parameter_value_type %in% c("Unspecified", "Other")
                ~ "Other/Unspecified", TRUE ~ parameter_value_type)
  ) %>%
  filter(parameter_class %in% parameter) %>%
  filter(!parameter_from_figure %in% TRUE) %>%
  # Adjust values so that they all have the same exponent (10^-4)
  mutate(
    parameter_value_unadj = parameter_value,
    parameter_lower_unadj = parameter_lower_bound,
    parameter_upper_unadj = parameter_upper_bound,
    parameter_value = 
      case_when(!is.na(parameter_value) ~ (parameter_value * 10^exponent)/10^-4,
                TRUE ~ NA
                ),
    parameter_upper_bound =
      case_when(!is.na(parameter_upper_bound) ~ (parameter_upper_bound * 10^exponent)/10^-4,
                TRUE ~ NA
      ),
    parameter_lower_bound =
      case_when(!is.na(parameter_lower_bound) ~ (parameter_lower_bound * 10^exponent)/10^-4,
                TRUE ~ NA
      ),
    parameter_uncertainty_single_value =
      case_when(!is.na(parameter_uncertainty_single_value) ~
                  (parameter_uncertainty_single_value * 10^exponent)/10^-4,
                TRUE ~ NA
      ),
    parameter_uncertainty_upper_value =
      case_when(!is.na(parameter_uncertainty_upper_value) ~
                  (parameter_uncertainty_upper_value * 10^exponent)/10^-4,
                TRUE ~ NA
      ),
    parameter_uncertainty_lower_value =
      case_when(!is.na(parameter_uncertainty_lower_value) ~
                  (parameter_uncertainty_lower_value * 10^exponent)/10^-4,
                TRUE ~ NA
      ),
    parameter_bounds =
      ifelse(!is.na(parameter_lower_bound) & !is.na(parameter_upper_bound),
             paste(parameter_lower_bound, "-", parameter_upper_bound),
             NA
      ),
    comb_uncertainty =
      case_when(
        !is.na(parameter_uncertainty_lower_value) & !is.na(parameter_uncertainty_upper_value) ~
          paste(parameter_uncertainty_lower_value, "-", parameter_uncertainty_upper_value),
        !is.na(parameter_uncertainty_single_value) ~
          paste(parameter_uncertainty_single_value),
        TRUE ~ NA
      ),
    parameter_unit =
      case_when(!is.na(parameter_unit) ~ paste(parameter_unit, "(10^-4)"),
                TRUE ~ NA
      ),
  )

# Check NA and unspecified and add fixes to cleaning.R in db_compilation:
mut_dat$parameter_value_type # Check 3 NA (x)
mut_dat$population_country # Check 6 unspecified
mut_dat$survey_date # Check 7 unspecified - 17835 (Membrebe) truly unspecified, 19237 x2 (Vrancken) unclear (x)
mut_dat$outbreak # Check 12 unspecified - 19237 x2 (Vrancken) unclear (x)
mut_dat$parameter_unit # Checked 4 Unspecified and NA (x)
mut_dat$genome_site # Combined variations of whole genome, glycoprotein, etc (x)
# 1st March: Alfson 5898 removed (mutation frequencies) and 1 from Vrancken (null estimate)
# 5th March: Added missed substitution rate for 17790 (Mbala-Kingebeni)

mut_dat %>% nrow() # 24 parameters
length(unique(mut_dat$covidence_id))

# Create directory for results
dir.create("Mutation_results")
dir.create("Mutation_results/qa_filtered")
dir.create("Mutation_results/qa_unfiltered")

# Order data
ordered_dat <- mut_dat %>%
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
    )
  )

# PLOTS

mut_plot_qa <- create_plot(
  ordered_dat,
  param = parameter,
  qa_filter = TRUE,
  facet_by = "parameter_type",
  symbol_shape_by = "parameter_value_type",
  symbol_col_by = "population_country",
  axis_label = Substitutions/site/year~(10^-4)
)

mut_plot <- create_plot(
  ordered_dat,
  param = parameter,
  qa_filter = FALSE,
  facet_by = "parameter_type",
  symbol_shape_by = "parameter_value_type",
  symbol_col_by = "population_country",
  axis_label = Substitutions/site/year~(10^-4)
)

# Save all for substitutions/site/year
ggsave("Mutation_results/qa_filtered/plot.png", mut_plot_qa,
       width = 10, height = 5, units = "in", bg = "white"
)

ggsave("Mutation_results/qa_unfiltered/plot.png", mut_plot,
       width = 10, height = 5, units = "in", bg = "white"
)

# TABLES

tab_dat <- ordered_dat %>%
  mutate(parameter_value = as.character(round(parameter_value, digits = 2)),
         ebola_species =
           case_when(ebola_species %in% "Bundibugyo, Sudan, Taï Forest & Zaire" ~
                       "All species", TRUE ~ ebola_species), # list all human pathogenic species in legend
         population_sample_type =
           case_when(population_sample_type %in% "Unspecified" ~ "", TRUE ~ population_sample_type)
         )

# QA unfiltered
mut_tab <- create_table(
  tab_dat,
  param = parameter,
  group = "parameter_type",
  #rounding = "2d",
  qa_filter = FALSE
)

# add superscript to subheadings
mut_tab <- mk_par(
  mut_tab, i = c(1, 14, 17), j = 1, part = "body", value = as_paragraph(
    paste0(parameter_type), " (Substitutions/site/year (10", as_sup("-4"), "))")
  )


# QA filtered
mut_tab_qa <- create_table(
  ordered_dat,
  param = parameter,
  group = "parameter_type",
  rounding = "2d",
  qa_filter = TRUE
)

# Save
save_as_image(mut_tab, path = "Mutation_results/qa_unfiltered/table.png")
save_as_image(mut_tab_qa, path = "Mutation_results/qa_filtered/table.png")

