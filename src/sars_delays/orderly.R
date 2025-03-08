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
orderly_dependency("db_compilation", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
orderly_artefact("sars-specific tables",c("figure_5_delays.png",
                                          "figure_5_delays.pdf",
                                          "figure_5full_delays.png",
                                          "figure_5full_delays.pdf",
                                          "figure_5SI_subgroup_meta.png",
                                          "figure_5SI_delays.png",
                                          "figure_5SI_delays.pdf",
                                          "figure_5SI_onset_to_admission.png",
                                          "figure_S5_funnel.png"))

###################
## DATA CURATION ##
###################
TEXT_SIZE <- 28

articles   <- read_csv("articles.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("parameters.csv")

#dfs <- curation(articles,tibble(),models,parameters, adjust_for_exponents = FALSE )
dfs <- data_curation(articles,tibble(),models,parameters, plotting = TRUE )

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles %>% dplyr::select(covidence_id,qa_score)

models     <- dfs$models
parameters <- dfs$parameters %>% left_join(qa_scores) %>%
  mutate(article_label = make.unique(refs)) %>%
  mutate(article_label = factor(article_label,levels=rev(unique(article_label))))

# sample sd is the same as sd, so need to make this clear somehow / change param field to work for meta analysis

d1 <- parameters %>% filter(parameter_type == 'Human delay - incubation period')
d2 <- parameters %>% filter(parameter_type == 'Human delay - symptom onset>admission to care')
d3 <- parameters %>% filter(parameter_type == 'Human delay - time in care (length of stay)' |
                              parameter_type == 'Human delay - admission to care>discharge/recovery' |
                              parameter_type == 'Human delay - admission to care>death') %>%
  mutate(parameter_value_type = case_when(is.na(parameter_value_type)~'Unspecified',
                                    TRUE ~ parameter_value_type))
d4 <- parameters %>% filter(parameter_type == 'Human delay - symptom onset>discharge/recovery' |
                              parameter_type == 'Human delay - symptom onset>death')
d5 <- parameters %>% filter(parameter_type == 'Human delay - serial interval')
d6 <- parameters %>% filter(parameter_type == 'Human delay - infectious period')
d7 <- parameters %>% filter(parameter_type == 'Human delay - generation time')

# Serial interval sub-plot (both papers have qa above 0.5)
SI_forest <- forest_plot(d5 %>% filter(qa_score>0.5)|> arrange(desc(parameter_value)),'Serial Interval (days)',"parameter_type",c(0,20),text_size = TEXT_SIZE)
IP_forest <- forest_plot(d6 %>% filter(qa_score>0.5)|> arrange(desc(parameter_value)),'Infectious Period (days)',"parameter_type",c(0,30),text_size = TEXT_SIZE)
GT_forest <- forest_plot(d7 %>% filter(qa_score>0.5)|> arrange(desc(parameter_value)),'Generation Time (days)',"parameter_type",c(0,20),text_size = TEXT_SIZE)

SI_forest_noqa <- forest_plot(d5|> arrange(desc(parameter_value)), 'Serial Interval (days)',"parameter_type",c(0,20),text_size = TEXT_SIZE)
IP_forest_noqa <- forest_plot(d6|> arrange(desc(parameter_value)), 'Infectious Period (days)',"parameter_type",c(0,30),text_size = TEXT_SIZE)
GT_forest_noqa <- forest_plot(d7|> arrange(desc(parameter_value)), 'Generation Time (days)',"parameter_type",c(0,20),text_size = TEXT_SIZE)


# Incubation period
#p1 <- forest_plot(d1,'Incubation Period (days)',"parameter_type",c(0,30))
d1_subgroups <- d1 %>% filter(population_group %in% c('General population','Mixed groups', 'Persons under investigation','Healthcare workers')) %>%
  filter(qa_score>0.5) %>%
  mutate(parameter_value = coalesce(parameter_value,central))

d1_forest <- d1 %>% mutate(population_group = replace_na(population_group,'Unspecified'),
                           parameter_value_type = replace_na(parameter_value_type,'Other')) %>%
  filter(qa_score>0.5) %>%
  mutate(parameter_value = coalesce(parameter_value,central))

IncP_forest <- forest_plot(d1_forest |> arrange(population_group,desc(parameter_value)),'Incubation period (days)',"population_group", c(0,21),text_size = TEXT_SIZE)

d1_subgroups_noqa <- d1 %>% filter(population_group %in% c('General population','Mixed groups', 'Persons under investigation','Healthcare workers')) %>%
  mutate(parameter_value = coalesce(parameter_value,central))

set.seed(42)
m1 <- metamean_wrap(dataframe = d1_subgroups, estmeansd_method = "Cai", 
                    plot_study = FALSE, digits = 2, lims = c(0,10), colour = "dodgerblue3", label = "Mean incubation period (days)",
                    width = 7500, height = 5950, resolution = 1000, subgroup = 'population_group', sort_by_subg = TRUE, colgap_shift = 2 )

set.seed(42)
m1_SI <- metamean_wrap(dataframe = d1_subgroups, estmeansd_method = "Cai", 
                       plot_study = TRUE, digits = 2, lims = c(0,10), colour = "dodgerblue3", label = "Mean incubation period (days)",
                       width = 11500, height = 9750, resolution = 1000, subgroup = 'population_group', sort_by_subg = TRUE, colgap_shift = 2 )

set.seed(42)
m1_SI_noqa <- metamean_wrap(dataframe = d1_subgroups_noqa, estmeansd_method = "Cai", 
                       plot_study = FALSE, digits = 2, lims = c(0,10), colour = "dodgerblue3", label = "Mean incubation period (days)",
                       width = 9500, height = 9750, resolution = 1000, subgroup = 'population_group', sort_by_subg = TRUE, colgap_shift = 2 )

png(file = "temp.png", width = 9500, height = 6500, res = 1000)
funnel(m1$result,common = FALSE,#only plots funnel for either common or random for some reason
       pch = 22, bg = "dodgerblue3", level = 0.95, 
       studlab = TRUE, cex.studlab = 0.75, pos.studlab = 2)

dev.off()
gg <- png::readPNG("temp.png", native = TRUE)
file.remove("temp.png")
funnel_m1 <- wrap_elements(plot = rasterGrob(gg, interpolate = TRUE))

#meta-analysis of onset-admission delay
set.seed(42)

d2_forest <- d2 %>% filter(qa_score>0.5) %>% mutate(parameter_value = coalesce(parameter_value,central)) %>% arrange(desc(parameter_value)) %>%
                                          mutate(method_moment_value = replace_na(method_moment_value,'Unspecified'),
                                                 population_group    = replace_na(population_group,'Unspecified'),
                                                 population_sample_type = replace_na(population_sample_type,'Unspecified'))

d2 <- d2 %>% mutate(parameter_value = coalesce(parameter_value,central)) %>% arrange(desc(parameter_value)) %>%
  filter(!is.na(parameter_uncertainty_single_value)|!is.na(parameter_uncertainty_type) ) %>%
  filter(qa_score>0.5) %>%
  mutate(method_moment_value = replace_na(method_moment_value,'Unspecified'),
         population_group    = replace_na(population_group,'Unspecified'),
         population_sample_type = replace_na(population_sample_type,'Unspecified'))

oa_forest_mmv <- forest_plot(d2_forest |> arrange(method_moment_value,desc(parameter_value)),
                             'Onset-Admission Delay (days)','method_moment_value',c(0,20),text_size = TEXT_SIZE)

oa_forest_pc <- forest_plot(d2_forest |> arrange(population_country,desc(parameter_value)),
                             'Onset-Admission Delay (days)','population_country',c(0,20),text_size = TEXT_SIZE)

oa_forest_pg <- forest_plot(d2_forest |> arrange(population_group,desc(parameter_value)),
                            'Onset-Admission Delay (days)','population_group',c(0,20),text_size = TEXT_SIZE)

oa_forest_pst <- forest_plot(d2_forest |> arrange(population_sample_type,desc(parameter_value)),
                            'Onset-Admission Delay (days)','population_sample_type',c(0,20),text_size = TEXT_SIZE)

oa <- (oa_forest_mmv + oa_forest_pc) / (oa_forest_pg + oa_forest_pst ) + theme(text = element_text(size = TEXT_SIZE)) + 
   plot_annotation(tag_levels = 'A') 
ggsave("figure_5SI_onset_to_admission.png", plot = oa, width = 39, height = 22)

m2 <- metamean_wrap(dataframe = d2, estmeansd_method = "Cai",
                    plot_study = TRUE, digits = 2, lims = c(0,10), colour = "dodgerblue3", label = "Mean Onset-Admission Delay (days)",
                    width = 9500, height = 4200, resolution = 1000)


png(file = "temp.png", width = 9500, height = 6500, res = 1000)
funnel(m2$result,common = FALSE,xlim=c(1.8,6),#only plots funnel for either common or random for some reason
       pch = 22, bg = "dodgerblue3", level = 0.95, 
       studlab = TRUE, cex.studlab = 0.75, pos.studlab = 2)

dev.off()
gg <- png::readPNG("temp.png", native = TRUE)
file.remove("temp.png")
funnel_m2 <- wrap_elements(plot = rasterGrob(gg, interpolate = TRUE))

ggsave("figure_S5_funnel.png", plot = funnel_m1 + funnel_m2, width = 12, height = 8)

set.seed(42)
d2_noqa <- d2 %>% mutate(parameter_value = coalesce(parameter_value,central)) %>% arrange(desc(parameter_value)) %>%
  filter(!is.na(parameter_uncertainty_single_value)|!is.na(parameter_uncertainty_type) ) 
m2_noqa <- metamean_wrap(dataframe = d2_noqa, estmeansd_method = "Cai",
                    plot_study = TRUE, digits = 2, lims = c(0,10), colour = "dodgerblue3", label = "Mean Onset-Admission Delay (days)",
                    width = 9500, height = 4200, resolution = 1000)

# admission to outcome... 
outcome_forest <- forest_plot(d3 %>% filter(qa_score>0.5) |> arrange(parameter_type,desc(parameter_value)) %>%
                                mutate(parameter_type = stringr::str_to_title(str_replace(parameter_type,'Human delay - ',''))),
                              'Admission to outcome (days)',"parameter_type",c(0,60),text_size = 22)
outcome_forest_noqa <- forest_plot(d3 |> arrange(parameter_type,desc(parameter_value))  %>% 
                                mutate(parameter_type = stringr::str_to_title(str_replace(parameter_type,'Human delay - ',''))),
                              'Admission to outcome (days)',"parameter_type",c(0,60),text_size = 22)

### Create plots!

layout <- "
AAABBBDDD
EEEEFFFFF
EEEEFFFFF
"
delays_plot <-  (SI_forest + IP_forest + outcome_forest)/(IncP_forest+oa_forest_pg) + plot_annotation(tag_levels = 'A') 
ggsave("figure_5_delays.png", plot = delays_plot, width = 37, height = 25)
ggsave("figure_5_delays.pdf", plot = delays_plot, width = 37, height = 25)

delays_plot <-  SI_forest + IP_forest + outcome_forest + m1$plot + theme(text = element_text(size = TEXT_SIZE)) + m2$plot + theme(text = element_text(size = TEXT_SIZE)) + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A') 
ggsave("figure_5full_delays.png", plot = delays_plot, width = 39, height = 22)
ggsave("figure_5full_delays.pdf", plot = delays_plot, width = 39, height = 22)


delays_plotSI <-  SI_forest_noqa + IP_forest_noqa + outcome_forest_noqa + m1_SI_noqa$plot + theme(text = element_text(size = TEXT_SIZE)) + m2_noqa$plot + theme(text = element_text(size = TEXT_SIZE)) + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A') 
ggsave("figure_5SI_delays.png", plot = delays_plotSI, width = 39, height = 22)
ggsave("figure_5SI_delays.pdf", plot = delays_plotSI, width = 39, height = 22)

# SI figures
ggsave("figure_5SI_subgroup_meta.png", plot = m1_SI$plot, width = 22, height = 22) # note that qa & noqa lets through sames papers


