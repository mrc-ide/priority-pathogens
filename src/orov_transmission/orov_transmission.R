# *========================= orov transmission plots =========================*
library(dplyr)
library(ggplot2)
library(ggsci)
#library(orderly)
library(patchwork)
library(readr)
library(stringr)

# *--------------------------------- Orderly ----------------------------------*
pathogen <- orderly_parameters(pathogen = "OROV")

orderly_dependency(
  "db_compilation_orov",
  "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "outbreaks.csv", "parameters.csv")
)

orderly_shared_resource("orov_functions.R" = "orov_functions.R")
source("orov_functions.R")

orderly_artefact(
  description = "orov transmission figures",
  c("figure_trans.png", "figure_trans.pdf")
)

# *------------------------------ Data curation -------------------------------*
articles <- read_csv("articles.csv")
outbreaks <- read_csv("outbreaks.csv")
parameters <- read_csv("parameters.csv")

dfs <- curation(articles, outbreaks, tibble(), parameters, plotting = TRUE)

# articles <- dfs$articles
# articles <- epireview::assign_qa_score(articles = articles)$articles
# qa_scores <- articles |> dplyr::select(covidence_id, qa_score)

parameters <- dfs$parameters
parameters <- parameters |> mutate(qa_score = article_qa_score / 100)
parameters$parameter_class <- parameters$parameter_type_broad
# *----------------------------- Data preparation -----------------------------*
# Sort population sample type to match legend where Other, Unspecified, or NA
# are included. A neater solution would be to do this during cleaning or for the
# relevant subset of population groups included in the params considered in
# this script.
parameters <- parameters |>
  mutate(
    population_group = factor(
      population_group,
      levels = c(
        sort(setdiff(unique(population_group), c("Other", "Unspecified"))),
        "Other",
        "Unspecified"
      )
    )
  )

d1 <- parameters |> filter(parameter_type == "Mutations - evolutionary rate")
d2 <- parameters |> filter(parameter_type == "Mutations - substitution rate")
d3 <- parameters |> filter(parameter_type == "Mutations - mutation rate")
d4 <- parameters |> filter(parameter_type == "Attack rate")

d5 <- parameters |>
  filter(parameter_type == "Reproduction number (Effective, Re)")

# Add prop symptomatic
# arrange data and format for plotting
variables_to_mutate <- c(
  "parameter_value",
  "parameter_lower_bound",
  "parameter_upper_bound",
  "parameter_uncertainty_lower_value",
  "parameter_uncertainty_upper_value"
)

d1 <- d1 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4
d2 <- d2 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4
d3 <- d3 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4

d4 <- d4 |>
  mutate(across(
    all_of(variables_to_mutate),
    ~ ifelse(parameter_unit == "No units", . * 100, .)
  )) |>
  mutate(
    parameter_unit = ifelse(
      parameter_unit == "No units",
      "Percentage (%)",
      parameter_unit
    )
  )

d1 <- d1 |> arrange(genome_site, -central)
d1[1, "genome_site"] <- "UPDATE"

d2 <- d2 |> arrange(genome_site, -central)
d3 <- d3 |> arrange(genome_site, -central)

# TODO:check if primary or secondary arates
d4 <- d4 |>
  mutate(arate = c("?", "?", "?")) |>
  arrange(arate, -central)

d5 <- d5 |>
  arrange(parameter_type, -central)

d5 <- d5 |>
  mutate(
    parameter_type = factor(
      parameter_type,
      levels = unique(parameter_type),
      labels = c("Effective (Re)")
    )
  )

# For the legend to show all shapes
d3 <- d3 |>
  mutate(
    parameter_value_type = factor(
      parameter_value_type,
      levels = c("Mean", "Median", "Unspecified")
    )
  )

# *---------------------------------- Plots -----------------------------------*
# Plot properties
text_size <- 20
point_size <- 4.5

custom_colour_pop_groups <- get_colour_pop_groups(
  parameters,
  bind_rows(d3, d4, d5)
)
custom_colour_genome_groups <- get_colour_genome_groups(
  parameters,
  bind_rows(d1, d2, d3)
)

# Approach to getting unified axes is very hacky... :(
# Generate plots
# Repeat twice - with and without QA filter
# -1 to include all articles
qa_threshold <- -1
qa_alpha <- 0.3

p1 <- forest_plot(
  d1 |>
    filter(qa_score > qa_threshold),
  expression(
    Evolutionary ~ Rate ~ (s / s / y ~ 10^{
      -4
    })
  ),
  "genome_site",
  c(0, 20),
  custom_colours = custom_colour_genome_groups,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size
) +
  scale_color_manual(
    values = custom_colour_genome_groups,
    limits = names(custom_colour_genome_groups)
  ) +
  scale_fill_manual(
    values = custom_colour_genome_groups,
    limits = names(custom_colour_genome_groups)
  ) +
  guides(color = guide_none(), linetype = guide_none(), shape = guide_none())


p2 <- forest_plot(
  d2 |> filter(qa_score > qa_threshold),
  expression(
    Substitution ~ Rate ~ (s / s / y ~ 10^{
      -4
    })
  ),
  "genome_site",
  c(0, 16),
  custom_colours = custom_colour_genome_groups,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size
) +
  scale_color_manual(
    values = custom_colour_genome_groups,
    limits = names(custom_colour_genome_groups)
  ) +
  scale_fill_manual(
    values = custom_colour_genome_groups,
    limits = names(custom_colour_genome_groups)
  ) +
  guides(color = guide_none(), linetype = guide_none(), shape = guide_none())

p3 <- forest_plot(
  d3 |> filter(qa_score > qa_threshold),
  expression(
    Mutation ~ Rate ~ (s / s / y ~ 10^{
      -4
    })
  ),
  "genome_site",
  c(0, 16),
  custom_colours = custom_colour_genome_groups,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  point_show.legend = c(color = FALSE, shape = TRUE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size
) +
  scale_color_manual(
    values = custom_colour_genome_groups,
    limits = names(custom_colour_genome_groups)
  ) +
  scale_fill_manual(
    values = custom_colour_genome_groups,
    limits = names(custom_colour_genome_groups)
  ) +
  scale_shape_manual(
    values = c(Mean = 21, Median = 22, Unspecified = 24),
    limits = c("Mean", "Median", "Unspecified"),
    drop = FALSE
  ) +
  guides(
    fill = guide_none(),
    linetype = guide_none(),
    color = guide_legend(
      title = "Genome type",
      order = 2,
      override.aes = list(
        fill = custom_colour_genome_groups
      )
    ),
    shape = guide_legend(
      title = "Parameter type",
      order = 1,
    )
  )

p4 <- forest_plot(
  d4 |> filter(qa_score > qa_threshold),
  "Attack Rate (%)",
  "population_group",
  c(-0.01, 70),
  custom_colours = custom_colour_pop_groups,
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size
) +
  guides(color = guide_none(), linetype = guide_none(), shape = guide_none())

p5 <- forest_plot(
  d5 |> filter(qa_score > qa_threshold),
  "Basic Reproduction Number",
  "population_group",
  c(0, 1.5),
  custom_colours = custom_colour_pop_groups,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size
) +
  scale_color_manual(
    values = custom_colour_pop_groups,
    limits = names(custom_colour_pop_groups)
  ) +
  scale_fill_manual(
    values = custom_colour_pop_groups,
    limits = names(custom_colour_pop_groups)
  ) +
  guides(
    fill = guide_none(),
    linetype = guide_none(),
    color = guide_legend(
      title = "Population type",
      order = 2,
      override.aes = list(fill = custom_colour_pop_groups)
    ),
    shape = guide_none()
  )

p5 <- p5 +
  theme(
    legend.position = c(0.835, 0.25),
    legend.spacing = unit(2, "mm"),
    legend.key.height = unit(0.5, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 13)
  )
p3 <- p3 +
  theme(
    legend.position = c(0.275, 0.675),
    legend.spacing = unit(2, "mm"),
    legend.key.height = unit(0.5, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 13)
  )
# Save plots
patchwork <- (p5 + p4) / (p3 + p2 + p1)

patchwork <- patchwork +
  plot_annotation(tag_levels = "A") +
  plot_layout(byrow = FALSE) +
  theme(
    plot.tag.position = "topleft",
    plot.tag = element_text(size = 22)
  )

ggsave(
  paste0("figure_trans.png"),
  plot = patchwork,
  width = 21,
  height = 12
)
ggsave(
  paste0("figure_trans.pdf"),
  plot = patchwork,
  width = 21,
  height = 12
)
# *============================================================================*
