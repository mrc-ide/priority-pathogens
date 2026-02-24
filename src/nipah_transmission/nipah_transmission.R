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
# Sort population sample type to match legend where Other, Unspecified, or NA
# are included. A neater solution would be to do this during cleaning or for the
# relevant subset of population groups included in the params considered in
# this script.
parameters <- parameters  |>
  mutate(population_group = factor(
    population_group,
    levels=c("General population",
               sort(setdiff(unique(population_group),
                            c("General population", "Other", "Unspecified"))
               ),
               "Other", "Unspecified"
    )),
    parameter_value = coalesce(parameter_value, central))  #NOTE

d1 <- parameters |> filter(parameter_type == "Mutations - evolutionary rate")
d2 <- parameters |> filter(parameter_type == "Mutations - substitution rate")
d3 <- parameters |> filter(parameter_class == "Overdispersion")
d4 <- parameters |> filter(parameter_class == "Attack rate")
d5 <- parameters |> filter(parameter_class == "Reproduction number")

# Add prop symptomatic
# arrange data and format for plotting
variables_to_mutate <- c("parameter_value",
                         "parameter_lower_bound",
                         "parameter_upper_bound",
                         "parameter_uncertainty_lower_value",
                         "parameter_uncertainty_upper_value")

d1 <- d1 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4
d2 <- d2 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4
d4 <- d4 |>
  mutate(across(all_of(variables_to_mutate),
                ~ ifelse(parameter_unit == "No units", . * 100, .))) |>
  mutate(parameter_unit = ifelse(parameter_unit == "No units",
                                 "Percentage (%)", parameter_unit))

# Presti reports this metric as an evolutionary rate
# Rahman refers to the metric as both a substitution and evolutionary rate in
# the same text
d1 <- d1 |>
  mutate(refs=str_replace(refs, "Lo Presti \\(2016\\)",
                          "'Lo Presti (2016\\)'^'*'"))

d1 <- d1 |> arrange(genome_site,-central)

d2 <- d2 |> arrange(genome_site,-central)

# Different from SARS and Lassa
d3 <- d3 |>
  mutate(parameter_value_type = ifelse(parameter_value_type=="Other",
                                       "Unspecified",
                                       parameter_value_type)) |>
  arrange(-central)

d4 <- d4 |> mutate(arate=c("Primary","Primary")) |>
    arrange(arate,-central)

d5 <- d5 |>
  arrange(parameter_type, -central)

d5 <- d5 |>
  mutate(parameter_type = factor(parameter_type,
                                 levels = unique(parameter_type),
                                 labels = c("Basic (R0)")))

# *---------------------------------- Plots -----------------------------------*
# Plot properties
text_size <- 12

# Get custom colours so that genome has different colours
bmj_colours <- ggsci::pal_bmj("default")(9)
temp <- bmj_colours[4]
bmj_colours[4] <- bmj_colours[6]
bmj_colours[6] <- temp

temp <- bmj_colours[1]
bmj_colours[1] <- bmj_colours[2]
bmj_colours[2] <- temp

all_pop_groups <- parameters |>
  filter(!is.na(population_group)) |>
  distinct(population_group) |>
  arrange(desc(population_group == "General population"),
          population_group == "Unspecified", population_group == "Other",
          population_group) |>
  pull()

all_pop_groups <- levels(all_pop_groups)

custom_colour_pop_groups <- bmj_colours[seq_along(all_pop_groups)]
names(custom_colour_pop_groups) <- all_pop_groups

unique_groups_in_plot <- unique(bind_rows(d3, d4, d5, d5)$population_group)

custom_colour_pop_groups <- (custom_colour_pop_groups[all_pop_groups %in% (unique_groups_in_plot)])

bind_rows(d3, d4, d5, d5) |> distinct(population_group)

nejm_colours <-  c("#7876B1FF", "#EE4C97FF", "#EFE58B","#6F99ADFF")

all_genomes <- bind_rows(d1, d2) |>
  distinct(genome_site) |>
  pull()

custom_colour_genome_groups <- nejm_colours[length(all_genomes) +
                                              seq_along(all_genomes)]
names(custom_colour_genome_groups) <- all_genomes

# Approach to getting unified axes is very hacky... :(
# Generate plots
# Repeat twice - with and without QA filter
# -1 to include all articles
qa_thresh_vec <- c(-1, 0.5)
qa_alpha_vec <- c(0.3, 1)
labels <- c("SI_allqa_", "")

for (i in seq_along(qa_thresh_vec)){
  label <- labels[i]
  qa_threshold <- qa_thresh_vec[i]
  qa_alpha <- qa_alpha_vec[i]

  p1 <- forest_plot(d1 |>
                      rbind(d2) |>
                      filter(qa_score>qa_threshold),
                    expression(Substitution~Rate~(s/s/y ~10^{-4})),
                    "genome_site",
                    c(-0.01,145), custom_colours = custom_colour_genome_groups,
                    segment_show.legend=c(color=TRUE, shape=FALSE),
                    text_size=text_size, qa_alpha=qa_alpha,
                    sort=TRUE) +
    coord_cartesian(xlim = c(0, 16)) +
    annotate("segment", x = 15.5, xend = 15.9, y = 2, yend = 2,
      arrow = arrow(type = "open", length = unit(0.2, "cm"))) +
    scale_y_discrete(labels = function(x) parse(text = x)) +
    scale_color_manual(values=custom_colour_genome_groups,
                       limits=names(custom_colour_genome_groups)) +
    scale_fill_manual(values=custom_colour_genome_groups,
                      limits=names(custom_colour_genome_groups)) +
    guides(fill = guide_none(),
           linetype = guide_none(),
           color = guide_legend(title = "Genome type", order =1,
                                override.aes = list(
                                  fill = custom_colour_genome_groups)),
           shape=guide_none())

  # p2 <- forest_plot(d2 |> filter(qa_score>qa_threshold),
  #                   expression(Substitution~Rate~(s/s/y ~10^{-4})),
  #                   "genome_site",
  #                   c(0,16), custom_colours = custom_colour_genome_groups,
  #                   text_size=text_size, qa_alpha=qa_alpha, sort=TRUE) +
  #   guides(shape = guide_legend(title = "Parameter type", order=1),
  #          fill = guide_none(),
  #          color = guide_none())

  p3 <- forest_plot(d3 |> filter(qa_score>qa_threshold),
                    "Overdispersion (max nr. of secondary cases)",
                    "population_group", c(0,35),
                    custom_colours = custom_colour_pop_groups,
                    text_size=text_size, qa_alpha=qa_alpha,
                    sort=TRUE) +
    guides(color = guide_none(),
           linetype = guide_none(),
           shape = guide_none())

  p4 <- forest_plot(d4 |> filter(qa_score>qa_threshold),
                    "Attack Rate (%)",
                    "population_group",
                    c(-0.01,3), custom_colours = custom_colour_pop_groups,
                    text_size=text_size, qa_alpha=qa_alpha, sort=TRUE) +
    guides(color = guide_none(),
           linetype = guide_none(),
           shape = guide_none())

  p5 <- forest_plot(d5 |> filter(qa_score>qa_threshold),
                    "Basic Reproduction Number",
                    "population_group",
                    c(0, 1.5), custom_colours = custom_colour_pop_groups,
                    segment_show.legend=c(color=TRUE, shape=FALSE),
                    text_size=text_size, qa_alpha=qa_alpha, sort=TRUE) +
    scale_color_manual(values=custom_colour_pop_groups,
                       limits=names(custom_colour_pop_groups)) +
    scale_fill_manual(values=custom_colour_pop_groups,
                      limits=names(custom_colour_pop_groups)) +
    guides(fill = guide_none(),
           linetype = guide_none(),
           color = guide_legend(
             title = "Population type", order = 2,
             override.aes = list(fill = custom_colour_pop_groups)),
           shape=guide_legend(title = "Parameter type", order=1))

  p5 <- p5 + theme(legend.position = c(0.835, 0.3),
                   legend.spacing=unit(2, "mm"),
                   legend.key.height = unit(0.5, "cm"),
                   legend.margin=margin(0, 0, 0, 0))
  p1 <- p1 + theme(legend.position = c(0.875, 0.85),
                   legend.spacing=unit(2, "mm"),
                   legend.key.height = unit(0.5, "cm"),
                   legend.margin=margin(0, 0, 0, 0))
  # Save plots
  patchwork <- (p5 + p3 + p4 + p1) +
    plot_layout(ncol = 2, widths = c(1,1))

  patchwork <- patchwork +
    plot_annotation(tag_levels = "A") +
    plot_layout(byrow = FALSE)

  ggsave(paste0("figure_",label,"trans.png"),
         plot = patchwork, width = 17, height = 10)
  ggsave(paste0("figure_",label,"trans.pdf"),
         plot = patchwork, width = 17, height = 10)
}
# *============================================================================*
