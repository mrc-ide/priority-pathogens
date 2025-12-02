# *========================= Nipah transmission plots =========================*
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
                   c("articles.csv", "outbreaks.csv", "models.csv", "params.csv"))

orderly_shared_resource("nipah_functions.R" = "nipah_functions.R")
source("nipah_functions.R")

orderly_artefact("Nipah transmission figures",
                 c("figure_trans.png", "figure_trans.pdf"))

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

# *----------------------------- Data preparation -----------------------------*
d1 <- parameters |> filter(parameter_type == "Mutations - evolutionary rate")
d2 <- parameters |> filter(parameter_type == "Mutations - substitution rate")
d3 <- parameters |> filter(parameter_class == "Overdispersion")
d4 <- parameters |> filter(parameter_class == "Attack rate")
d5 <- parameters |> filter(parameter_type == "Severity - proportion of symptomatic cases")
d6 <- parameters |> filter(parameter_class == "Reproduction number")

# Add prop symptomatic
# arrange data and format for plotting
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

d1 <- d1 |> arrange(genome_site,-central)

# TODO: check Upper and lower bound - zero out for now
d2 <- d2 |>
  mutate(across(
    c(parameter_upper_bound, parameter_lower_bound),
    ~ ifelse(covidence_id == 2760, NA, .))
    )

d2 <- d2 |> arrange(genome_site,-central)

# Different from SARS and Lassa
d3 <- d3 |>
  mutate(parameter_value_type = ifelse(parameter_value_type=="Other",
                                       "Unspecified",
                                       parameter_value_type)) |>
  arrange(-central)

d4 <- d4 |> mutate(arate=c("Primary","Primary")) |>
             arrange(arate,-central)

# Update central
d5 <- d5 |>
  mutate(parameter_value = ifelse(is.na(parameter_value),
                                  cfr_ifr_numerator/cfr_ifr_denominator,
                                  parameter_value)
                   )
d5 <- d5 |>
  arrange(-central)

d6 <- d6 |>
  arrange(parameter_type, -central)

d6 <- d6 |>
  mutate(parameter_type = factor(parameter_type,
                                 levels = unique(parameter_type),
                                 labels = c("Basic (R0)")))

# *---------------------------------- Plots -----------------------------------*
# Plot properties
text_size <- 12

# Get custom colours so that genome has different colours
lanonc_colours <- ggsci::pal_lancet("lanonc")(9)

all_pop_groups <- bind_rows(d3, d4, d5, d6) |>
  distinct(population_group) |>
  # arrange alphabetically but put other last
  arrange(population_group == "Other", population_group) |>
  pull()

custom_colour_pop_groups <- lanonc_colours[seq_along(all_pop_groups)]
names(custom_colour_pop_groups) <- all_pop_groups

all_genomes <- bind_rows(d1, d2) |>
  distinct(genome_site) |>
  pull()

custom_colour_genome_groups <- lanonc_colours[length(all_pop_groups) +
                                                seq_along(all_genomes)]
names(custom_colour_genome_groups) <- all_genomes

# Approach to getting unified axes is very hacky... :(
# Generate plots
# Repeat twice - with and without QA filter
# -1 to include all articles
qa_thresh_vec <- c(-1, 0.5)
labels <- c("SI_allqa_", "")

for (i in seq_along(qa_thresh_vec)){
  label <- labels[i]
  qa_threshold <- qa_thresh_vec[i]

  p1 <- forest_plot(d1 |> filter(qa_score>qa_threshold),
                    expression(Evolutionary~Rate~(s/s/y ~10^{-4})),
                    "genome_site",
                    c(-0.01,15), custom_colours = custom_colour_genome_groups,
                    segment_show.legend=c(color=TRUE, shape=FALSE),
                    text_size=text_size) +
    scale_color_manual(values=custom_colour_genome_groups,
                       limits=names(custom_colour_genome_groups)) +
    scale_fill_manual(values=custom_colour_genome_groups,
                      limits=names(custom_colour_genome_groups)) +
    guides(fill = guide_none(),
           color = guide_legend(title = "Genome type", order = 1,
                                override.aes = list(fill = custom_colour_genome_groups)),
           shape=guide_none())

  # Should this be segment?
  p2 <- forest_plot(d2 |> filter(qa_score>qa_threshold),
                    expression(Substitution~Rate~(s/s/y ~10^{-4})),
                    "genome_site",
                    c(0,16), custom_colours = custom_colour_genome_groups,
                    text_size=text_size) +
    guides(shape = guide_legend(title = "Parameter type", order=1),
           fill = guide_none(),
           color = guide_none())

  p3 <- forest_plot(d3 |> filter(qa_score>qa_threshold),
                    "Overdispersion (max nr. of cases related to a case)",
                    "population_group", c(0,35),
                    custom_colours = custom_colour_pop_groups,
                    text_size=text_size) +
    guides(color = guide_none(),
           shape = guide_none())

  p4 <- forest_plot(d4 |> filter(qa_score>qa_threshold),
                    "Primary Attack Rate (%)",
                    "population_group",
                    c(-0.01,3), custom_colours = custom_colour_pop_groups,
                    text_size=text_size) +
    guides(color = guide_none(),
           shape = guide_none())

  p5 <- forest_plot(d5 |> filter(qa_score>qa_threshold),
                    "Proportion of Symptomatic Cases (%)",
                    "population_group",
                    c(-5, 110), custom_colours = custom_colour_pop_groups,
                    text_size=text_size) +
    guides(color = guide_none(),
           shape = guide_none())

  p6 <- forest_plot(d6 |> filter(qa_score>qa_threshold),
                    "Basic Reproduction Number",
                    "population_group",
                    c(0, 1.5), custom_colours = custom_colour_pop_groups,
                    segment_show.legend=c(color=TRUE, shape=FALSE),
                    text_size=text_size) +
    scale_color_manual(values=custom_colour_pop_groups,
                       limits=names(custom_colour_pop_groups)) +
    scale_fill_manual(values=custom_colour_pop_groups,
                      limits=names(custom_colour_pop_groups)) +
    guides(fill = guide_none(),
           color = guide_legend(title = "Population type", order = 1,
                                override.aes = list(fill   = custom_colour_pop_groups)),
           shape=guide_none())

  # Save plots
  patchwork <- (p6 + p3 + p4 + p5 + p1 + p2) +
    plot_layout(ncol = 2, widths = c(1,1), guides = "collect")

  patchwork <- patchwork + plot_annotation(tag_levels = "A")
  ggsave(paste0("figure_",label,"trans.png"),
         plot = patchwork, width = 14, height = 10)
  ggsave(paste0("figure_",label,"trans.pdf"),
         plot = patchwork, width = 14, height = 10)
}
# *============================================================================*
