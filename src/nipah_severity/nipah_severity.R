# *=================== Nipah severity meta-analysis & plots ===================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(grid)
library(metafor)
library(meta)
library(orderly2)
library(patchwork)
library(readr)
library(stringr)

# *--------------------------------- Orderly ----------------------------------*
orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "outbreaks.csv", "models.csv", "params.csv"))

orderly_shared_resource("nipah_functions.R" = "nipah_functions.R")
source("nipah_functions.R")

orderly_artefact("Nipah severity figures",
                 c("figure_severity.png", "figure_severity.pdf"))

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles,outbreaks,models,parameters, plotting = FALSE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

# *----------------------------- Data preparation -----------------------------*
d1 <- parameters |>
  filter(parameter_type == 'Severity - case fatality rate (CFR)')

# Unspecified parameter type for CFR with only numerator and denom
d1 <- d1 |>
  filter(parameter_unit != "Unspecified")

# Create subgroups here
# Lassa examples for study midyear, population_group, cfr_denom_cat
d1 <- d1 |>
  mutate(population_group = case_when(
    population_group == "Persons under investigation" ~ "Persons Under Investigation",
    population_group == "Persons with symptoms" ~ "Persons with symptoms",
    population_group == "Healthcare workers" ~ "Healthcare workers",
    population_group == "Abattoir workers" ~ "Animal workers",
    population_group == "Animal workers" ~ "Animal workers",
    TRUE ~ "Mixed Groups")) |>
  mutate(population_study_start_year = as.numeric(population_study_start_year),
         population_study_end_year = as.numeric(population_study_end_year),
         study_midyear = ifelse(!is.na(population_study_start_year) & !is.na(population_study_end_year),
                                round((population_study_start_year + population_study_end_year) / 2),
                                population_study_start_year)) |>
  mutate(study_midyear_cat = case_when(
    study_midyear %in% 1990:1999 ~ "1990-1999",
    study_midyear %in% 2000:2009 ~ "2000-2009",
    study_midyear %in% 2010:2019 ~ "2010-2019",
    study_midyear %in% 2020:2029 ~ "2020-Present",
    TRUE ~ "Unspecified")) |>
  mutate(cfr_denom_cat = case_when(
    cfr_ifr_denominator %in% 1:29      ~ "Reported Cases < 30",
    cfr_ifr_denominator %in% 30:99     ~ "Reported Cases = 30-99",
    cfr_ifr_denominator %in% 100:399   ~ "Reported Cases = 100-399",
    TRUE ~ "Unspecified")) |>
  # Deduplicating CFRs
  mutate(duplicate_cfr = case_when(
    covidence_id %in% c() ~ "Known",
    covidence_id %in% c() ~ "Assumed",
    TRUE ~ "False")) #only identified for estimates passed to meta-analysis (i.e. denominator not NA)

# *------------------------------ Meta-analysis -------------------------------*
da <- d1 |> filter(duplicate_cfr == "False")

m1 <- metaprop_wrap(dataframe = da, subgroup = "population_country",
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = "red",
                    width = 4000, height = 4000, resolution = 500)
m2 <- metaprop_wrap(dataframe = d1, subgroup = "study_midyear_cat",
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = "red",
                    width = 4000, height = 4000, resolution = 500)
m3 <- metaprop_wrap(dataframe = da |> arrange(cfr_ifr_denominator), subgroup = "cfr_denom_cat",
                    plot_pooled = TRUE, sort_by_subg = FALSE, plot_study = FALSE, digits = 3, colour = "red",
                    width = 4000, height = 4000, resolution = 500)
m4 <- metaprop_wrap(dataframe = da, subgroup = "population_group",
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = "red",
                    width = 4000, height = 4000, resolution = 500)

p1 <- m1$plot
p2 <- m2$plot
p3 <- m3$plot
p4 <- m4$plot

patchwork <- (p1 + p2 + p3 + p4) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_severity.png", plot = patchwork, width = 18, height = 12)
ggsave("figure_severity.pdf", plot = patchwork, width = 18, height = 12)

# Additional plots for SI
#figure_S6-S10: meta-analysis with all estimates plotted
#figure_S11: meta-analysis with only known duplicates excluded
db <- d1 |> filter(duplicate_cfr %in% c("False","Assumed"))
#figure_S12: meta-analysis without de-duplication
dc <- d1

