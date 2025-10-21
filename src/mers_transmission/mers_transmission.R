# *========================= MERS transmission plots ==========================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(orderly2)
library(patchwork)
library(readr)
library(stringr)

# *--------------------------------- Orderly ----------------------------------*
orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "params.csv"))

orderly_shared_resource("mers_functions.R" = "nipah_functions.R")
source("mers_functions.R")

orderly_artefact("MERS transmission figures",
                 c("figure_trans.png", "figure_trans.pdf"))

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks <- tibble()
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles

articles <- articles |>
  filter(covidence_id!=13554)

articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

# *----------------------------- Data preparation -----------------------------*
d1 <- parameters |> filter(parameter_type == 'Mutations - evolutionary rate')
d2 <- parameters |> filter(parameter_type == 'Mutations - substitution rate')
d3 <- parameters |> filter(parameter_class == 'Overdispersion')
d5 <- parameters |> filter(parameter_type == 'Severity - proportion of symptomatic cases')
d4 <- parameters |> filter(parameter_class == 'Attack rate')
d6 <- parameters |> filter(parameter_class == 'Reproduction number')

# Add prop symptomatic
#arrange data and format for plotting
variables_to_mutate <- c("parameter_value",
                         "parameter_lower_bound",
                         "parameter_upper_bound",
                         "parameter_uncertainty_lower_value",
                         "parameter_uncertainty_upper_value")

d1 <- d1 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4
d2 <- d2 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4
d4 <- d4 |> mutate(across(all_of(variables_to_mutate),
                   ~ ifelse(parameter_unit == "No units", . * 100, .))
                   ) |>
  mutate(parameter_unit = ifelse(parameter_unit == "No units",
                                 "Percentage (%)", parameter_unit))

d1 <- d1 |> arrange(genome_site,central)
d2 <- d2 |> arrange(genome_site, central)
d3 <- d3 |> arrange(population_group, central)

d4 <- d4 |>  arrange(population_group, central)
d5 <- d5 |> arrange(population_group, central)
d6 <- d6 |> arrange(population_group, central)

# *---------------------------------- Plots -----------------------------------*
# Set to 28 for other pathogens
text_size <- 14
# Generate plots
p1 <- forest_plot(d1,expression(Evolutionary~Rate~(s/s/y ~10^{-4})),
                  "genome_site", c(-0.01,15), text_size=text_size) +
  guides(color = guide_legend(title = "Gene", order = 1))

# Error is being caused by different units
unique(d2$parameter_unit[!is.na(d2$parameter_unit)])
d2 <- d2 |>
  mutate(parameter_unit=ifelse(parameter_unit=="Substitutions/site/year",
                               parameter_unit, NA))
p2 <- forest_plot(d2,
                  expression(Substitution~Rate~(s/s/y ~10^{-4})),
                  "genome_site",c(-5,150),
                  text_size=text_size) +
  guides(color = guide_legend(title = "Segment", order = 1))

unique(d3$parameter_unit[!is.na(d3$parameter_unit)])
d3 <- d3 |>
  mutate(parameter_unit=ifelse(
    parameter_unit=="Max. nr. of cases superspreading (related to case)",
    parameter_unit, NA))
p3 <- forest_plot(d3,'Overdispersion',"population_group", c(-1,85),
                  text_size=text_size)

unique(d4$parameter_unit[!is.na(d4$parameter_unit)])
d4 <- d4 |>
  mutate(parameter_unit=ifelse(parameter_unit=="Percentage (%)",
                               parameter_unit, NA))
p4 <- forest_plot(d4, 'Attack Rate (%)', "population_group", c(-1,100),
                  text_size=text_size)

p5 <- forest_plot(d5,'Proportion of Symptomatic Cases (%)', "population_group",
                  c(0, 110),
                  text_size=text_size)

unique(d6$parameter_unit[!is.na(d6$parameter_unit)])
d6$parameter_unit <- "No units"
p6 <- forest_plot(d6,'Reproduction Number',"population_group", c(-0.1,6),
                  text_size=text_size)

# Save plots
patchwork <- (p6 + p5 + p4 + p3 + p1 + p2) +
  plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_trans.png", plot = patchwork, width = 20, height = 18)
ggsave("figure_trans.pdf", plot = patchwork, width = 20, height = 18)
# *============================================================================*
