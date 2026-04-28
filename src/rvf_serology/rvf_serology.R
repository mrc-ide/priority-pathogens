# *============================== RVF Serology ==============================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(grid)
library(orderly)
library(patchwork)
library(readr)
library(stringr)
library(tidyr)

# *--------------------------------- Orderly ----------------------------------*
orderly_strict_mode()

pars <- orderly_parameters(pathogen = NULL)

orderly_dependency(
  "db_cleaning",
  "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "models.csv", "params.csv"))

orderly_shared_resource("rvf_functions.R"="rvf_functions.R")

source("rvf_functions.R")

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- tibble()
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles,outbreaks,models,parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles, ignore_errors = TRUE)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)   #Note that the curation() processes removes any parameters that are parameter_from_figure = TRUE

# *----------------------------- Data preparation -----------------------------*
parameters <- parameters |>
  mutate(article_label = make.unique(refs)) |>
  mutate(article_label = factor(article_label,levels=rev(unique(article_label))))

sero_studies <- parameters |>
  filter(parameter_class == 'Seroprevalence')

sero_no_unit_row_count <- sero_studies |>
  filter(parameter_unit=="No units" | is.na(parameter_unit)) |>
  NROW()

cat("Number of extracted serology rows with no unit: ", sero_no_unit_row_count)
#7 no units, and 17 NAs
sero_studies_not_percentage <- sero_studies |>
  filter(parameter_unit=="No units" | is.na(parameter_unit))

#401_003 is strange, trying to capture a range of PRNT50 values, I (TR) say cut it for now but can discuss
sero_studies <- sero_studies |>
  filter(access_param_id != "401_003")

# All the others are cases of having numerator and denominator, but not specific central value (though a couple of people have put 0)

# Assumption: parameter unit is always a percentage
sero_studies <- sero_studies |>
  mutate(parameter_value = coalesce(parameter_value,central),
         parameter_unit="Percentage (%)",
         population_group = factor(
           population_group,
           levels = c(sort(setdiff(unique(population_group),
                                   c("Other", "Unspecified"))),
                      "Other", "Unspecified")))

# Remove the low-QA studies now
sero_studies <- filter(sero_studies, qa_score >= 0.5)

# *---------------------------------- Plots -----------------------------------*
p1 <- forest_plot(sero_studies, 'Serology (%)', 'parameter_type', c(-4,104),
                  qa_alpha = 0.3, text_size = 28, sort=TRUE, point_size=6) +
  #This facets by country, but atm, that's so many countries that the plot is a mess
  # ggforce::facet_col(facets = vars(population_country),
  #                    scales = "free_y",
  #                    space = "free") +
  guides(fill = guide_none(),
         linetype = guide_none(),
         color = guide_legend(title = "Assay", order =2),
         shape = guide_legend(title = "Parameter type", order =1)) +
  theme(legend.position = c(0.84, 0.74))
ggsave(paste0("sero_apx_col_assay_general.pdf"),
       plot = p1, width = 19, height = 23)
ggsave(paste0("sero_apx_col_assay_general.png"),
       plot = p1, width = 19, height = 23)


#Rename country tags:
sero_studies <- sero_studies |>
  mutate(population_country=ifelse(population_country=="China; Nigeria",
                                   "Other", population_country)) |>
  mutate(population_country=ifelse(population_country=="Egypt",
                                   "Other (MENAP)", population_country)) |>
  mutate(population_country=ifelse(population_country=="Germany; Netherlands; Qatar",
                                   "Other", population_country)) |>
  mutate(population_country=ifelse(population_country=="Jordan",
                                   "Other (MENAP)", population_country)) |>
  mutate(population_country=ifelse(population_country=="Kenya",
                                   "Other", population_country)) |>
  mutate(population_country=ifelse(population_country=="Malaysia",
                                   "Other", population_country)) |>
  mutate(population_country=ifelse(population_country=="Morocco",
                                   "Other (MENAP)", population_country)) |>
  mutate(population_country=ifelse(population_country=="Nigeria",
                                   "Other", population_country)) |>
  mutate(population_country=ifelse(population_country=="Pakistan",
                                   "Other (MENAP)", population_country)) |>
  mutate(population_country=ifelse(population_country=="United States of America",
                                   "Other", population_country))
#Set my preferred factor levels:
sero_studies$population_country <- factor(
  sero_studies$population_country,
  levels = c("Saudi Arabia", "United Arab Emirates", "Qatar",
             "Republic of Korea", "Other (MENAP)", "Other")  # <- your desired order
)

p1 <- forest_plot(sero_studies, 'Serology (%)', 'parameter_type', c(-4,104),
                  qa_alpha = 0.3, text_size = 28, sort=TRUE, point_size=6) +
  #This facets by country, but atm, that's so many countries that the plot is a mess
  ggforce::facet_col(facets = vars(population_country),
                     scales = "free_y",
                     space = "free") +
  guides(fill = guide_none(),
         linetype = guide_none(),
         color = guide_legend(title = "Assay", order =2),
         shape = guide_legend(title = "Parameter type", order =1)) +
  theme(legend.position = c(0.82, 0.91))
ggsave(paste0("sero_apx_col_assay_pop_country.pdf"),
       plot = p1, width = 19, height = 27)
ggsave(paste0("sero_apx_col_assay_pop_country.png"),
       plot = p1, width = 19, height = 27)

p2 <- forest_plot(sero_studies, 'Serology (%)', 'parameter_type', c(-4,104),
                  qa_alpha = 0.3, text_size = 28, sort=TRUE, point_size=6) +
  ggforce::facet_col(facets = vars(population_group),
                     scales = "free_y",
                     space = "free") +
  guides(fill = guide_none(),
         linetype = guide_none(),
         color = guide_legend(title = "Assay", order =2),
         shape = guide_legend(title = "Parameter type", order =1)) +
  theme(legend.position = c(0.84, 0.45))
ggsave(paste0("sero_apx_col_assay_pop_group.png"),
       plot = p2, width = 19, height = 27)
ggsave(paste0("sero_apx_col_assay_pop_group.pdf"),
       plot = p2, width = 19, height = 27)

p3 <- forest_plot(sero_studies, 'Serology (%)', 'population_group', c(-4,104),
                  qa_alpha = 0.3, text_size = 28, sort=TRUE, point_size=6) +
  ggforce::facet_col(facets = vars(population_country),
                     scales = "free_y",
                     space = "free") +
  guides(fill = guide_none(),
         linetype = guide_none(),
         color = guide_legend(title = "Population group", order =2),
         shape = guide_legend(title = "Parameter type", order =1)) +
  theme(legend.position = c(0.82, 0.91))
ggsave(paste0("sero_apx_col_country_pop_country.pdf"),
       plot = p3, width = 19, height = 27)
ggsave(paste0("sero_apx_col_country_pop_country.png"),
       plot = p3, width = 19, height = 27)

# SI figures
# Split figures to allow for larger panels and figures to split over two pages
# Figure 1 by assay type
sero_apx_1 <-  (p1+ theme(legend.position = c(0.76, 0.94)) |
                  p2  + guides(color=guide_none(), shape=guide_none())) +
  plot_annotation(tag_levels = "A")

ggsave(paste0("sero_apx_col_assay_patchwork.png"),
       plot = sero_apx_1, width = 32, height = 30)
ggsave(paste0("sero_apx_col_assay_patchwork.pdf"),
       plot = sero_apx_1, width = 32, height = 30)