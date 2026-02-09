# *============================== Nipah Serology ==============================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(grid)
library(orderly2)
library(patchwork)
library(readr)
library(stringr)
library(tidyr)

# *--------------------------------- Orderly ----------------------------------*
orderly_strict_mode()

orderly_parameters(pathogen = NULL)

orderly_dependency(
  "db_cleaning",
  "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "models.csv", "params.csv", "outbreaks.csv"))

orderly_shared_resource("nipah_functions.R"="nipah_functions.R")

source("nipah_functions.R")

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles,outbreaks,models,parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

# *----------------------------- Data preparation -----------------------------*
parameters <- parameters |>
  mutate(article_label = make.unique(refs)) |>
  mutate(article_label = factor(article_label,levels=rev(unique(article_label)))) |>
  mutate(in_CSF = case_when(str_detect(parameter_notes,'CSF')~TRUE,
                            TRUE~FALSE))

sero_studies <- parameters |>
  filter(parameter_class == 'Seroprevalence')

sero_no_unit_row_count <- sero_studies |>
  filter(parameter_unit=="Unspecified" | is.na(parameter_unit)) |>
  NROW()

cat("Number of extracted serology rows with no unit: ", sero_no_unit_row_count)

# Assumption: parameter unit is always a percentage
sero_studies <- sero_studies |>
  mutate(parameter_value = coalesce(parameter_value,central),
         parameter_unit="Percentage (%)",
         population_group = factor(
           population_group,
           levels = c(sort(setdiff(unique(population_group),
                                   c("Other", "Unspecified"))),
                      "Other", "Unspecified")))

# CSF studies
sero_studies |>
  filter(in_CSF) |>
  select(article_id, covidence_id, parameter_type, parameter_value,
         qa_score, refs) |>
  print()
# *---------------------------------- Plots -----------------------------------*
p1 <- forest_plot(sero_studies, 'Serology (%)', 'parameter_type', c(-4,104),
                  qa_alpha = 0.3, text_size = 28, sort=TRUE, point_size=6) +
  ggforce::facet_col(facets = vars(population_country),
                     scales = "free_y",
                     space = "free") +
  guides(fill = guide_none(),
         linetype = guide_none(),
         color = guide_legend(title = "Assay", order =2),
         shape = guide_legend(title = "Parameter type", order =1)) +
  theme(legend.position = c(0.84, 0.94))
ggsave(paste0("sero_apx_col_assay_pop_country.pdf"),
       plot = p1, width = 19, height = 35)

p2 <- forest_plot(sero_studies, 'Serology (%)', 'parameter_type', c(-4,104),
                  qa_alpha = 0.3, text_size = 28, sort=TRUE, point_size=6) +
  ggforce::facet_col(facets = vars(population_group),
                     scales = "free_y",
                     space = "free") +
  guides(fill = guide_none(),
         linetype = guide_none(),
         color = guide_legend(title = "Assay", order =2),
         shape = guide_legend(title = "Parameter type", order =1)) +
  theme(legend.position = c(0.84, 0.4))
ggsave(paste0("sero_apx_col_assay_pop_group.pdf"),
       plot = p2, width = 19, height = 35)

p3 <- forest_plot(sero_studies, 'Serology (%)', 'population_group', c(-4,104),
                  qa_alpha = 0.3, text_size = 28, sort=TRUE, point_size=6) +
  ggforce::facet_col(facets = vars(population_country),
                     scales = "free_y",
                     space = "free") +
  guides(fill = guide_none(),
         linetype = guide_none(),
         color = guide_legend(title = "Population group", order =2),
         shape = guide_legend(title = "Parameter type", order =1)) +
  theme(legend.position = c(0.86, 0.93))
ggsave(paste0("sero_apx_col_country_pop_country.pdf"),
       plot = p3, width = 19, height = 35)

# SI figures
# Split figures to allow for larger panels and figures to split over two pages
# Figure 1 by assay type
sero_apx_1 <-  (p1+ theme(legend.position = c(0.76, 0.94)) |
                  p2  + guides(color=guide_none(), shape=guide_none())) +
  plot_annotation(tag_levels = "A")

ggsave(paste0("sero_apx_col_assay_patchwork.pdf"),
       plot = sero_apx_1, width = 32, height = 38)

