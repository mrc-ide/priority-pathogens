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
                   parameter:plotting == TRUE)",
                   c("articles_curated.rds", "outbreaks_curated.rds", "models_curated.rds", "parameters_curated.rds"))
# orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
# source("lassa_functions.R")
orderly_shared_resource("zika_functions.R" = "zika_functions.R")
source("zika_functions.R")

###################
## DATA CURATION ##
###################
TEXT_SIZE <- 28

articles   <- readRDS("articles_curated.rds")
models     <- readRDS("models_curated.rds")
parameters <- readRDS("parameters_curated.rds")%>%
  mutate(population_sample_type = ifelse(is.na(population_sample_type), 'Unspecified',population_sample_type)) %>%
  # for plotting remove location that creates confusion 
  mutate(population_location = ifelse(population_country_original == "Brazil;Colombia" & covidence_id == 268, NA, population_location),
         population_location = ifelse(population_location == "90 major cities across LAC", "90 major cities", population_location),
         # shorten name of Micronesia 
         population_country = ifelse(population_country == 'Federated States of Micronesia', "Micronesia", population_country))
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
  "green1","darkturquoise",  "yellow4", "yellow3",
  "darkorange4", "brown"
)

# Make plots of reproduction numbers
# epireview::forest_plot_r0(parameters, shape_by = 'parameter_value_type', col_by = 'r_pathway')
repronums <- parameters %>%
  filter(qa_score >= 0.5 & parameter_class == 'Reproduction number') 
repronumsnoqa <- parameters %>%
  filter(parameter_class == 'Reproduction number') 
table(repronums$parameter_type)
table(repronumsnoqa$parameter_type)
length(unique(repronums[grepl('Basic',repronums$parameter_type),]$covidence_id))
length(unique(repronums[grepl('Effective',repronums$parameter_type),]$covidence_id))

# TO find info about max and min of each param type
repsum <-repronums %>%
  group_by(parameter_type) %>%
  filter(parameter_value == max(parameter_value, na.rm = TRUE) | parameter_value == min(parameter_value, na.rm = TRUE)) %>%
  select(parameter_type:case_definition, method_r, method_moment_value, method_disaggregated, population_age_max:population_sex, survey_start_date, survey_end_date, article_label)

table(repronums$method_r)

basicr0 <- repronums %>% filter(parameter_type == 'Reproduction number (Basic R0)') 

r0_pc_noqa <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Basic R0)'), 
                     label = "Basic reproduction number",
                     color_column = 'population_country',
                     lims = c(0,17),
                     custom_colours = c25,
                     text_size = TEXT_SIZE)
r0_pc <- forest_plot(parameters %>% filter(qa_score >= 0.5 & parameter_type == 'Reproduction number (Basic R0)'), 
                     label = "Basic reproduction number",
                     color_column = 'population_country',
                     lims = c(0,17),
                     custom_colours = c25,
                     text_size = TEXT_SIZE)


r0_sampletype_noqa <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Basic R0)'), 
                             label = "Basic reproduction number",
                             ycol = 'label_group',
                             facet_by_country = TRUE,
                             color_column = 'population_sample_type',
                             shape_column = 'population_group',
                             lims = c(0,9),
                             custom_colours = c25,
                             text_size = TEXT_SIZE) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.8, .63))
 
r0_sampletype <- forest_plot(parameters %>% filter(qa_score >= 0.5 & parameter_type == 'Reproduction number (Basic R0)'), 
                             label = "Basic reproduction number",
                             ycol = 'label_group',
                             facet_by_country = TRUE,
                             color_column = 'population_sample_type',
                             shape_column = 'population_group',
                             lims = c(0,9),
                             custom_colours = c25,
                             text_size = TEXT_SIZE)+ 
  theme(legend.position = 'inside', legend.position.inside =  c(.8, .63))


# r0_human_noqa <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Basic R0) - Human'), 
#                         label = "Basic reproduction number - human",
#                         color_column = 'population_country',
#                         lims = c(0,17),
#                         custom_colours = c25)
# r0_human <- forest_plot(parameters %>% filter(qa_score >= 0.5 & parameter_type == 'Reproduction number (Basic R0) - Human'), 
#                         label = "Basic reproduction number - human",
#                         color_column = 'population_country',
#                         lims = c(0,17),
#                         custom_colours = c25)


r0_human_noqa <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Basic R0) - Human'), 
                         label = "Basic reproduction number - human",
                         ycol = 'label_group',
                         # facet_by_country = TRUE,
                         color_column = 'population_group',
                         shape_column = 'population_sample_type',
                         lims = c(0,17),
                         custom_colours = c25,
                         text_size = TEXT_SIZE)
r0_human <- forest_plot(parameters %>% filter(qa_score >= 0.5 & parameter_type == 'Reproduction number (Basic R0) - Human'), 
                        label = "Basic reproduction number - human",
                        ycol = 'label_group',
                        # facet_by_country = TRUE,
                        color_column = 'population_group',
                        shape_column = 'population_sample_type',
                        lims = c(0,17),
                        custom_colours = c25,
                        text_size = TEXT_SIZE)

# r0_mosquito_noqa <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Basic R0) - Mosquito'), 
#                            label = "Basic reproduction number - mosquito",
#                            color_column = 'population_country',
#                            lims = c(0,17),
#                            custom_colours = c25)
# r0_mosquito <- forest_plot(parameters %>% filter(qa_score >= 0.5 & parameter_type == 'Reproduction number (Basic R0) - Mosquito'), 
#                            label = "Basic reproduction number - mosquito",
#                            color_column = 'population_country',
#                            lims = c(0,17),
#                            custom_colours = c25)


r0_mosquito_noqa <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Basic R0) - Mosquito'), 
                           label = "Basic reproduction number - mosquito",
                           ycol = 'label_group',
                           # facet_by_country = TRUE,
                           color_column = 'population_group',
                           lims = c(0,17),
                           custom_colours = c25,
                           text_size = TEXT_SIZE)
r0_mosquito <- forest_plot(parameters %>% filter(qa_score >= 0.5 & parameter_type == 'Reproduction number (Basic R0) - Mosquito'), 
                            label = "Basic reproduction number - mosquito",
                            ycol = 'label_group',
                           # facet_by_country = TRUE,
                           color_column = 'population_group',
                            lims = c(0,17),
                            custom_colours = c25,
                           text_size = TEXT_SIZE)

re_noqa <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Effective, Re)'), 
                  label = "Effective reproduction number",
                  ycol = 'label_group',
                  # facet_by_country = TRUE,
                  color_column = 'population_sample_type',
                  shape_column = 'population_group',
                  lims = c(0,4),
                  custom_colours = c25,
                  text_size = TEXT_SIZE)
re <- forest_plot(parameters %>% filter(qa_score >= 0.5 & parameter_type == 'Reproduction number (Effective, Re)'), 
                  label = "Effective reproduction number",
                  ycol = 'label_group',
                  # facet_by_country = TRUE,
                  color_column = 'population_sample_type',
                  shape_column = 'population_group',
                  lims = c(0,4),
                  custom_colours = c25,
                  text_size = TEXT_SIZE)

re_human_noqa <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Effective; Re) - Human'),
                        label = "Effective reproduction number - human",
                        color_column = 'population_country',
                        lims = c(0,17),
                        custom_colours = c25,
                        text_size = TEXT_SIZE)
re_human <- forest_plot(parameters %>% filter(qa_score >= 0.5 & parameter_type == 'Reproduction number (Effective; Re) - Human'),
                        label = "Effective reproduction number - human",
                        color_column = 'population_country',
                        lims = c(0,17),
                        custom_colours = c25,
                        text_size = TEXT_SIZE)

re_mosquito <- forest_plot(parameters %>% filter(parameter_type == 'Reproduction number (Effective; Re) - Mosquito'),
                           label = "Effective reproduction number - mosquito",
                           color_column = 'population_country',
                           lims = c(0,17),
                           custom_colours = c25,
                           text_size = TEXT_SIZE)
# re_mosquito <- forest_plot(parameters %>% filter(qa_score >= 0.5 & parameter_type == 'Reproduction number (Effective; Re) - Mosquito'),
#                            label = "Effective reproduction number - mosquito",
#                            color_column = 'population_country',
#                            lims = c(0,17),
#                            custom_colours = c25)

HEIGHT = 25
WIDTH = 20
ggsave("r0_pc.pdf", r0_pc, height = HEIGHT, width = WIDTH, bg = 'white')
ggsave("r0_sampletype.pdf", r0_sampletype, height = 28, width = WIDTH, bg = 'white')
ggsave("r0_human.pdf", r0_human, height = 10, width = WIDTH, bg = 'white')
ggsave("r0_mosquito.pdf", r0_mosquito, height = 8, width = WIDTH, bg = 'white')
ggsave("re.pdf", re, height = 14, width = WIDTH, bg = 'white')
ggsave("re_human.pdf", re_human, height = 12, width = 12, bg = 'white')
ggsave("re_mosquito.pdf", re_mosquito, height = 10, width = 12, bg = 'white')

ggsave("r0_pc.png", r0_pc, height = HEIGHT, width = WIDTH, bg = 'white')
ggsave("r0_sampletype.png", r0_sampletype, height = 28, width = WIDTH, bg = 'white')
ggsave("r0_human.png", r0_human, height = 10, width = WIDTH, bg = 'white')
ggsave("r0_mosquito.png", r0_mosquito, height = 8, width = WIDTH, bg = 'white')
ggsave("re.png", re, height = 14, width = WIDTH, bg = 'white')
ggsave("re_human.png", re_human, height = 12, width = 12, bg = 'white')
ggsave("re_mosquito.png", re_mosquito, height = 10, width = 12, bg = 'white')

ggsave("r0_pc_noqa.pdf", r0_pc, height = HEIGHT, width = WIDTH, bg = 'white')
ggsave("r0_sampletype_noqa.pdf", r0_sampletype, height = 28, width = WIDTH, bg = 'white')
ggsave("r0_human_noqa.pdf", r0_human, height = 10, width = WIDTH, bg = 'white')
ggsave("r0_mosquito_noqa.pdf", r0_mosquito, height = 8, width = WIDTH, bg = 'white')
ggsave("re_noqa.pdf", re, height = 14, width = WIDTH, bg = 'white')
ggsave("re_human_noqa.pdf", re_human, height = 10, width = 12, bg = 'white')
# ggsave("re_mosquito_noqa.pdf", re_mosquito, height = 10, width = 12, bg = 'white')


# Combine the plots together 
layout <- 
"AAAAABBBB
AAAAABBBB
AAAAABBBB
AAAAACCCC
AAAAACCCC
AAAAACCCC"

r0_pl <- r0_sampletype + r0_human + r0_mosquito +
    plot_layout(design = layout) + plot_annotation(tag_levels = 'A')
r0__plnoqa <- r0_sampletype_noqa + r0_human_noqa + r0_mosquito_noqa +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')



re_pl <- re + re_human + re_mosquito + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')
re_plnoqa <- re_noqa + re_human_noqa +# re_mosquito_noqa + 
  plot_layout(design = "AAA
                        BBB") + plot_annotation(tag_levels = 'A')

ggsave("r0_pl.pdf", r0_pl, height = 38, width = 35, bg = 'white')
ggsave("r0__plnoqa.pdf", r0__plnoqa, height = 38, width = 35, bg = 'white')
ggsave("re_pl.pdf", re_pl, height = 12, width = 20, bg = 'white')
ggsave("re_plnoqa.pdf", re_plnoqa, height = 12, width = 20, bg = 'white')
ggsave("r0_pl.png", r0_pl, height = 38, width = 35, bg = 'white', dpi = 400)
ggsave("r0__plnoqa.png", r0__plnoqa, height = 38, width = 35, bg = 'white', dpi = 400)
ggsave("re_pl.png", re_pl, height = 12, width = 20, bg = 'white', dpi = 400)
ggsave("re_plnoqa.png", re_plnoqa, height = 12, width = 20, bg = 'white', dpi = 400)
