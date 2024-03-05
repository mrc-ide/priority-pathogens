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
library(scales)
library(epitrix)
library(doconv)

orderly_strict_mode()

# Outputs
orderly_artefact(
  "Plots and tables for reproduction number parameters",
  c(
    "R_plots/basic_r_outbreak_filtered.png",
    "R_plots/basic_r_country_filtered.png",
    "R_plots/eff_r_outbreak_filtered.png",
    "R_plots/eff_r_country_filtered.png",
    "R_tables/qa_filtered/basic_r_tab_filtered.png",
    "R_tables/qa_filtered/eff_r_tab_filtered.png",
    "R_tables/unfiltered/basic_r_tab_all.png",
    "R_tables/unfiltered/eff_r_tab_all.png",
    "R_tables/qa_filtered/range_outbreak.png",
    "R_tables/qa_filtered/range_country.png",
    "R_tables/qa_filtered/range_species.png",
    "R_tables/qa_filtered/eff_range_outbreak.png",
    "R_tables/qa_filtered/eff_range_country.png",
    "R_tables/qa_filtered/eff_range_species.png",
    "R_tables/unfiltered/paginate_basic_r_tab_all.docx",
    "R_tables/unfiltered/paginate_basic_r_tab_all.pdf",
    "R_tables/unfiltered/paginate_effective_r_tab_all.docx",
    "R_tables/unfiltered/paginate_effective_r_tab_all.pdf"
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

parameter <- "Reproduction number"

df <- left_join(
  params,
  articles[, c("covidence_id", "first_author_surname", "year_publication",
               "article_label", "doi", "notes")],
  by = "covidence_id"
) %>%
  arrange(article_label, -year_publication)

species_levels <- factor(df$ebola_species, levels = c(sort(
  setdiff(unique(df$ebola_species), "Unspecified"),
  decreasing = TRUE
), "Unspecified"))

df_plot <- df %>%
  mutate(
    population_country = as.factor(population_country)
  ) %>%
  filter(parameter_class %in% parameter) %>%
  filter(!parameter_from_figure %in% TRUE) %>%
  group_by(parameter_type) %>%
  mutate(
    parameter_type_short =
      ifelse(parameter_type %in% "Reproduction number (Basic R0)", "Basic (R0)",
        ifelse(parameter_type %in% "Reproduction number (Effective, Re)",
          "Effective (Re)", NA
        )
      ),
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
      )
  ) %>%
  mutate(
    # Checked data and no need to account for distributions
    # modify standard deviation and standard error to visualise uncertainty in plots
  parameter_uncertainty_type =
    case_when(is.na(parameter_uncertainty_type) &
                parameter_uncertainty_singe_type %in% "Standard Deviation" ~
                "Standard Deviation",
              is.na(parameter_uncertainty_type) &
                parameter_uncertainty_singe_type %in% "Standard Error" ~
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

ordered_dat <- df_plot %>%
  group_by(population_country) %>%
  mutate(
    range_midpoint =
      ifelse(is.na(parameter_value) & !is.na(parameter_upper_bound),
             (parameter_upper_bound - parameter_lower_bound)/2 + parameter_lower_bound, NA
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

plot_dat <- ordered_dat %>%
  filter(!parameter_value_type %in% "Standard Deviation") %>%
  mutate(
    parameter_value_type =
      case_when(parameter_value_type %in% c("Unspecified", "Other", NA) ~ "Other/Unspecified",
                TRUE ~ parameter_value_type)
   )

# Pull numbers on QA filtered basic R for text
n_param <- plot_dat %>% filter(parameter_type_short %in% "Basic (R0)" & article_qa_score >= 50)
n_param %>% nrow() # 71 parameters
length(unique(n_param$covidence_id)) # 52 articles
n_param %>% filter(outbreak %in% "West Africa 2013-2016") %>% nrow() # 58 WA

# Pull numbers on QA filtered effective R for text
n_param_re <- plot_dat %>% filter(parameter_type_short %in% "Effective (Re)" & article_qa_score >= 50)
n_param_re %>% nrow() # 32 parameters
length(unique(n_param_re$covidence_id)) # 23 articles
n_param_re %>% filter(outbreak %in% "West Africa 2013-2016") %>% nrow() # 23 WA
# Not QA filtered
n_param_re <- plot_dat %>% filter(parameter_type_short %in% "Effective (Re)")
n_param_re %>% nrow() # 55 parameters
length(unique(n_param_re$covidence_id)) # 40 articles
table(n_param_re$method_r)

# Plots with qa_filter of >=50
basic_r_outbreak_qa <- create_plot(
  plot_dat,
  param = parameter,
  r_type = "Basic (R0)",
  qa_filter = TRUE,
  facet_by = "outbreak",
  symbol_shape_by = "parameter_value_type",
  symbol_col_by = "population_country"
)

basic_r_country_qa <- create_plot(
  plot_dat,
  param = parameter,
  r_type = "Basic (R0)",
  qa_filter = TRUE,
  facet_by = "population_country",
  symbol_shape_by = "parameter_value_type",
  symbol_col_by = "outbreak"
)

eff_r_outbreak_qa <- create_plot(
  plot_dat,
  param = parameter,
  r_type = "Effective (Re)",
  qa_filter = TRUE,
  facet_by = "outbreak",
  symbol_shape_by = "parameter_value_type",
  symbol_col_by = "population_country"
)

eff_r_country_qa <- create_plot(
  plot_dat,
  param = parameter,
  r_type = "Effective (Re)",
  qa_filter = TRUE,
  facet_by = "population_country",
  symbol_shape_by = "parameter_value_type",
  symbol_col_by = "outbreak"
)

# Save
ggsave("R_plots/basic_r_outbreak_filtered.png", basic_r_outbreak_qa,
  width = 9, height = 12, units = "in", bg = "white"
)

ggsave("R_plots/basic_r_country_filtered.png", basic_r_country_qa,
  width = 9, height = 12.5, units = "in", bg = "white"
)

ggsave("R_plots/eff_r_outbreak_filtered.png", eff_r_outbreak_qa,
  width = 9, height = 6, units = "in", bg = "white"
)

ggsave("R_plots/eff_r_country_filtered.png", eff_r_country_qa,
  width = 9, height = 6, units = "in", bg = "white"
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
  qa_filter = FALSE
)

eff_r_tab <- create_table(
  ordered_dat,
  param = parameter,
  r_type = "Effective (Re)",
  qa_filter = FALSE
)

# Save
save_as_image(basic_r_tab_qa, path = "R_tables/qa_filtered/basic_r_tab_filtered.png")
save_as_image(eff_r_tab_qa, path = "R_tables/qa_filtered/eff_r_tab_filtered.png")
save_as_image(basic_r_tab, path = "R_tables/unfiltered/basic_r_tab_all.png")
save_as_image(eff_r_tab, path = "R_tables/unfiltered/eff_r_tab_all.png")

# Save paginated version of basic R unfiltered table:
p_tab_basic <- paginate(basic_r_tab)
p_tab_eff <- paginate(eff_r_tab)

# Can only save paginated version to docx or rtf
# Make sure to remove white space by adjusting width and height
save_as_docx(p_tab_basic, path = "R_tables/unfiltered/paginate_basic_r_tab_all.docx",
             pr_section = prop_section(
               page_size = page_size(orient = "landscape", width = 19, height = 15),
               type = "continuous",
               page_margins = page_mar(bottom = 0, top = 0, right = 0, left = 0, gutter = 0)
               ))

save_as_docx(p_tab_eff, path = "R_tables/unfiltered/paginate_effective_r_tab_all.docx",
             pr_section = prop_section(
               page_size = page_size(orient = "landscape", width = 19, height = 12),
               type = "continuous",
               page_margins = page_mar(bottom = 0, top = 0, right = 0, left = 0, gutter = 0)
             ))

# Then convert to pdf
docx2pdf("R_tables/unfiltered/paginate_basic_r_tab_all.docx",
         output = "R_tables/unfiltered/paginate_basic_r_tab_all.pdf")
docx2pdf("R_tables/unfiltered/paginate_effective_r_tab_all.docx",
         output = "R_tables/unfiltered/paginate_effective_r_tab_all.pdf")


# For the basic reproduction number, summary table giving the range of central
# values for each mean, median, and other/unspecified parameter types:

basic_range_dat <- ordered_dat %>%
  filter(
    parameter_type_short %in% "Basic (R0)",
    !parameter_value_type %in% "Standard Deviation"
  ) %>%
  mutate(
    parameter_value_type =
      case_when(parameter_value_type %in% c("Unspecified", "Other")
      ~ "Other/Unspecified", TRUE ~ parameter_value_type)
  )

range_outbreak <- create_range_table(
  df = basic_range_dat,
  main_group = "outbreak", main_group_label = "Outbreak",
  sub_group = "parameter_value_type", sub_group_label = "Estimate Type",
  qa_filter = TRUE
)

range_country <- create_range_table(
  df = basic_range_dat,
  main_group = "population_country", main_group_label = "Country",
  sub_group = "parameter_value_type", sub_group_label = "Estimate Type",
  qa_filter = TRUE
)

range_species <- create_range_table(
  df = basic_range_dat,
  main_group = "ebola_species", main_group_label = "Species",
  sub_group = "parameter_value_type", sub_group_label = "Estimate Type",
  qa_filter = TRUE
)

# Save
save_as_image(range_outbreak, path = "R_tables/qa_filtered/range_outbreak.png")
save_as_image(range_country, path = "R_tables/qa_filtered/range_country.png")
save_as_image(range_species, path = "R_tables/qa_filtered/range_species.png")


# Same for effective R
eff_range_dat <- ordered_dat %>%
  filter(
    parameter_type_short %in% "Effective (Re)",
    !parameter_value_type %in% "Standard Deviation"
  ) %>%
  mutate(
    parameter_value_type =
      case_when(parameter_value_type %in% c("Unspecified", "Other")
                ~ "Other/Unspecified", TRUE ~ parameter_value_type)
  )

eff_range_outbreak <- create_range_table(
  df = eff_range_dat,
  main_group = "outbreak", main_group_label = "Outbreak",
  sub_group = "parameter_value_type", sub_group_label = "Estimate Type",
  qa_filter = TRUE
)

eff_range_country <- create_range_table(
  df = eff_range_dat,
  main_group = "population_country", main_group_label = "Country",
  sub_group = "parameter_value_type", sub_group_label = "Estimate Type",
  qa_filter = TRUE
)

eff_range_species <- create_range_table(
  df = eff_range_dat,
  main_group = "ebola_species", main_group_label = "Species",
  sub_group = "parameter_value_type", sub_group_label = "Estimate Type",
  qa_filter = TRUE
)

# Save
save_as_image(eff_range_outbreak, path = "R_tables/qa_filtered/eff_range_outbreak.png")
save_as_image(eff_range_country, path = "R_tables/qa_filtered/eff_range_country.png")
save_as_image(eff_range_species, path = "R_tables/qa_filtered/eff_range_species.png")

