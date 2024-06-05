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
library(cowplot)
library(stringr)
library(doconv)

orderly_strict_mode()

orderly_parameters(pathogen = "EBOLA")

orderly_artefact(
  "Plots and tables for severity parameters",
  c(
    "Severity_plots/plot_outbreak_filtered.png",
    "Severity_plots/plot_split_outbreak_filtered.pdf",
    "Severity_plots/plot_country_filtered.png",
    "Severity_plots/plot_outbreak_unfiltered.png",
    "Severity_plots/plot_country_unfiltered.png",
    "Severity_tables/qa_filtered/tab_filtered.png",
    "Severity_tables/qa_filtered/range_outbreak.png",
    "Severity_tables/qa_filtered/range_outbreak_country.png",
    "Severity_tables/qa_filtered/range_outbreak_country.docx",
    "Severity_tables/qa_filtered/range_country_outbreak.png",
    "Severity_tables/qa_filtered/range_species_outbreak.png",
    "Severity_tables/unfiltered/tab_unfiltered.png",
    "Severity_tables/unfiltered/paginate_severity_all.docx",
    "Severity_tables/unfiltered/paginate_severity_all.pdf"
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
  articles[, c("covidence_id", "first_author_surname", "year_publication",
               "article_label", "doi", "notes")],
  by = "covidence_id"
) %>%
  arrange(article_label, -year_publication)

sev_dat <- df %>%
  mutate(
    population_country = as.factor(population_country)
  ) %>%
  filter(parameter_class %in% parameter) %>%
  filter(!parameter_from_figure %in% TRUE) %>%
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
ordered_dat <- sev_dat %>%
  group_by(population_country) %>%
  mutate(
    range_midpoint =
      ifelse(is.na(parameter_value) & !is.na(parameter_upper_bound),
             (parameter_upper_bound - parameter_lower_bound)/2 + parameter_lower_bound, NA
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

# Pull numbers
ordered_dat %>% nrow() # 166
length(unique(ordered_dat$covidence_id)) # 130

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

## Split outbreak plot with WA on left and all other outbreaks on the right

# Add species to facet label
comb_facet_dat <- ordered_dat %>%
  mutate(
    outbreak_species = paste0(outbreak, " (", ebola_species, ")")
  )

# Match order of outbreak_species to outbreak
comb_facet_dat$outbreak_species <- factor(
  comb_facet_dat$outbreak_species,
  levels = unique(
    comb_facet_dat$outbreak_species[order(comb_facet_dat$outbreak)]
    )
  )

wa_dat <- comb_facet_dat %>% filter(outbreak == "West Africa 2013-2016")
other_dat <- comb_facet_dat %>% filter(outbreak != "West Africa 2013-2016")

wa_outbreak_qa <- create_plot(
  wa_dat,
  param = parameter,
  qa_filter = TRUE,
  symbol_shape_by = "cfr_ifr_method",
  facet_by = "outbreak_species",
  symbol_col_by = "population_country"
)

other_outbreak_qa <- create_plot(
  other_dat,
  param = parameter,
  qa_filter = TRUE,
  symbol_shape_by = "cfr_ifr_method",
  facet_by = "outbreak_species",
  symbol_col_by = "population_country"
)

# Get the original legend from outbreak qa plot
outbreak_legend <- get_legend(plot_outbreak_qa + theme(legend.position = "right"))

panels <- plot_grid(other_outbreak_qa + theme(legend.position = "none"),
                    wa_outbreak_qa + theme(legend.position = "none"),
                    nrow = 1, align = "hv")

plot_split_outbreak <- plot_grid(panels, outbreak_legend,
                                 ncol = 2, align = "hv", rel_widths = c(1, 0.3))

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
  width = 9, height = 18, units = "in", bg = "white"
)

ggsave("Severity_plots/plot_split_outbreak_filtered.pdf", plot_split_outbreak,
       width = 16, height = 11, units = "in", bg = "white"
)

ggsave("Severity_plots/plot_country_filtered.png", plot_country_qa,
  width = 9, height = 17, units = "in", bg = "white"
)

ggsave("Severity_plots/plot_outbreak_unfiltered.png", plot_outbreak_all,
  width = 9, height = 23, units = "in", bg = "white"
)

ggsave("Severity_plots/plot_country_unfiltered.png", plot_country_all,
  width = 9, height = 22, units = "in", bg = "white"
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

# Paginate the severity table
p_severity <- paginate(severity_table_all)

# Make sure to remove white space by adjusting width and height
save_as_docx(p_severity, path = "Severity_tables/unfiltered/paginate_severity_all.docx",
             pr_section = prop_section(
               page_size = page_size(orient = "landscape", width = 23, height = 17.5),
               type = "continuous",
               page_margins = page_mar(bottom = 0, top = 0, right = 0, left = 0, gutter = 0)
             ))

# Then convert to pdf
docx2pdf("Severity_tables/unfiltered/paginate_severity_all.docx",
         output = "Severity_tables/unfiltered/paginate_severity_all.pdf")


# Create summary tables giving the range of central values by specified groups

range_outbreak_country <- create_range_table(
  df = ordered_dat,
  main_group = "outbreak", main_group_label = "Outbreak",
  sub_group = "population_country", sub_group_label = "Country",
  qa_filter = TRUE, rounding = "integer"
)

range_outbreak <- create_range_table(
  df = ordered_dat,
  main_group = "outbreak", main_group_label = "Outbreak",
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

save_as_docx(range_outbreak_country, path = "Severity_tables/qa_filtered/range_outbreak_country.docx")

save_as_image(range_outbreak,
              path = "Severity_tables/qa_filtered/range_outbreak.png"
)
save_as_image(range_country_outbreak,
  path = "Severity_tables/qa_filtered/range_country_outbreak.png"
)
save_as_image(range_species_outbreak,
  path = "Severity_tables/qa_filtered/range_species_outbreak.png"
)
