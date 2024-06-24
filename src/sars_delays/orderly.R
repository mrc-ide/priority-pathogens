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

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("db_compilation", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
orderly_artefact("sars-specific tables",c("figure_delays.png"))

###################
## DATA CURATION ##
###################

articles   <- read_csv("articles.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("parameters.csv")

#dfs <- curation(articles,tibble(),models,parameters, adjust_for_exponents = FALSE )
dfs <- data_curation(articles,tibble(),models,parameters, plotting = TRUE )

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles %>% dplyr::select(covidence_id,qa_score)

models     <- dfs$models
parameters <- dfs$parameters %>% left_join(qa_scores) 

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

# Serial interval sub-plot (both papers have qa above 0.5)
SI_forest <- forest_plot(d5 %>% filter(qa_score>0.5),'Serial Interval (days)',"parameter_type",c(0,20))

# Incubation period
#p1 <- forest_plot(d1,'Incubation Period (days)',"parameter_type",c(0,30))
set.seed(42)
d1_subgroups <- d1 %>% filter(population_group %in% c('General population','Mixed groups', 'Persons under investigation','Healthcare workers')) %>%
  filter(qa_score>0.5) %>%
  mutate(parameter_value = coalesce(parameter_value,central))
m1 <- metamean_wrap(dataframe = d1_subgroups, estmeansd_method = "Cai", 
                    plot_study = FALSE, digits = 2, lims = c(0,10), colour = "dodgerblue3", label = "Incubation Period",
                    width = 7500, height = 5750, resolution = 1000, subgroup = 'population_group', sort_by_subg = TRUE )

#meta-analysis of onset-admission delay
set.seed(42)
d2 <- d2 %>% mutate(parameter_value = coalesce(parameter_value,central)) %>% arrange(desc(parameter_value)) %>%
  filter(!is.na(parameter_uncertainty_single_value)|!is.na(parameter_uncertainty_type) ) %>%
  filter(qa_score>0.5)
m2 <- metamean_wrap(dataframe = d2, estmeansd_method = "Cai",
                    plot_study = TRUE, digits = 2, lims = c(0,10), colour = "dodgerblue3", label = "Mean Onset-Admission Delay (days)",
                    width = 9500, height = 4200, resolution = 1000)

# admission to outcome... 
d3_subgroup <- d3 %>% 
  filter(!is.na(parameter_uncertainty_single_value)|!is.na(parameter_uncertainty_type) ) %>% 
  mutate(group = stringr::str_to_title(str_replace(parameter_type,'Human delay - ','')),
         parameter_type = 'Human delay') %>%  
  filter(qa_score>0.5)

outcome_forest <- forest_plot(d3 %>% filter(qa_score>0.5),'Admission to outcome',"parameter_type",c(0,50))


m3 <- metamean_wrap(dataframe = d3_subgroup, estmeansd_method = "Cai", 
                    plot_study = FALSE, digits = 2, lims = c(8,25), colour = "dodgerblue3", label = "Admission to outcome",
                    width = 9500, height = 4500, resolution = 1000, subgroup = 'group', sort_by_subg = TRUE )

layout <- "
AABBB
CCDDD
CCDDD
"
delays_plot <-  SI_forest + outcome_forest + m1$plot + m2$plot  + plot_layout(design = layout) + plot_annotation(tag_levels = 'A')

ggsave("figure_delays.png", plot = delays_plot, width = 28, height = 18)
