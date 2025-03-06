library(tidyverse)
library(stringr)
library(metafor)
library(meta)
library(estmeansd)
library(mixdist)
library(ggplot2)
library(ggsci)
library(sf)
library(ragg)
library(ggspatial)
library(ggforce)
library(png)
library(grid)
library(patchwork)
library(gridExtra)
library(orderly2)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)

# orderly_dependency("zika_compilation", "latest(parameter:pathogen == this:pathogen)",
#                    c("articles.rds", "outbreaks.rds", "models.rds", "parameters.rds"))
orderly_dependency("zika_prep_data", "latest(parameter:pathogen == this:pathogen &&
                   parameter:pathogen == TRUE)",
                   c("articles_curated.rds", "outbreaks_curated.rds", "models_curated.rds", "parameters_curated.rds"))
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")

###################
## DATA CURATION ##
###################
TEXT_SIZE <- 28

articles   <- readRDS("articles_curated.rds")
models     <- readRDS("models_curated.rds")
parameters <- readRDS("parameters_curated.rds")
# 
# dfs <- data_curation(articles,tibble(),models,parameters, plotting = TRUE )
# 
# articles   <- dfs$articles
# qa_scores  <- articles %>% dplyr::select(covidence_id,qa_score)
# 
# models     <- dfs$models
# parameters <- dfs$parameters %>% left_join(qa_scores) %>%
#   mutate(article_label = make.unique(refs)) %>%
#   mutate(article_label = factor(article_label,levels=rev(unique(article_label))))

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

# Make plots of reproduction numbers
# epireview::forest_plot_r0(parameters, shape_by = 'parameter_value_type', col_by = 'r_pathway')

r0_pc <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Basic R0)'), 
            label = "Basic reproduction number",
            color_column = 'population_country',
            lims = c(0,17),
            custom_colours = c25)

r0_sampletype <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Basic R0)'), 
                        label = "Basic reproduction number",
                        color_column = 'population_sample_type',
                        lims = c(0,17),
                        custom_colours = c25)

r0_human <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Basic R0) - Human'), 
            label = "Basic reproduction number - human",
            color_column = 'population_country',
            lims = c(0,17),
            custom_colours = c25)

r0_mosquito <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Basic R0) - Mosquito'), 
            label = "Basic reproduction number - mosquito",
            color_column = 'population_country',
            lims = c(0,17),
            custom_colours = c25)


re <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Effective, Re)'), 
            label = "Effective reproduction number",
            color_column = 'population_country',
            lims = c(0,4),
            custom_colours = c25)

re_human <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Effective; Re) - Human'),
            label = "Effective reproduction number - human",
            color_column = 'population_country',
            lims = c(0,17),
            custom_colours = c25)

re_mosquito <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Effective; Re) - Mosquito'),
            label = "Effective reproduction number - mosquito",
            color_column = 'population_country',
            lims = c(0,17),
            custom_colours = c25)

ggsave("r0_pc.pdf", r0_pc, height = 15, width = 12)
ggsave("r0_sampletype.pdf", r0_sampletype, height = 15, width = 12)
ggsave("r0_human.pdf", r0_human, height = 15, width = 12)
ggsave("r0_mosquito.pdf", r0_mosquito, height = 15, width = 12)
ggsave("re.pdf", r0, height = 15, width = 12)
ggsave("re_human.pdf", r0_human, height = 15, width = 12)
ggsave("re_mosquito.pdf", r0_mosquito, height = 15, width = 12)

ggsave("r0_pc.png", r0_pc, height = 15, width = 12)
ggsave("r0_sampletype.png", r0_sampletype, height = 15, width = 12)
ggsave("r0_human.png", r0_human, height = 15, width = 12)
ggsave("r0_mosquito.png", r0_mosquito, height = 15, width = 12)
ggsave("re.png", r0, height = 15, width = 12)
ggsave("re_human.png", r0_human, height = 15, width = 12)
ggsave("re_mosquito.png", r0_mosquito, height = 15, width = 12)
