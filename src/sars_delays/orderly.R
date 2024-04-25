#task to create sars latex tables

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
dfs <- data_curation(articles,tibble(),models,parameters, plotting = FALSE )

articles   <- dfs$articles
models     <- dfs$models
parameters <- dfs$parameters


# sample sd is the same as sd, so need to make this clear somehow / change param field to work for meta analysis

d1 <- parameters %>% filter(parameter_type == 'Human delay - incubation period')
d2 <- parameters %>% filter(parameter_type == 'Human delay - symptom onset>admission to care')
d3 <- parameters %>% filter(parameter_type == 'Human delay - time in care (length of stay)' |
                              parameter_type == 'Human delay - admission to care>discharge/recovery' |
                              parameter_type == 'Human delay - admission to care>death')
d4 <- parameters %>% filter(parameter_type == 'Human delay - symptom onset>discharge/recovery' |
                              parameter_type == 'Human delay - symptom onset>death')
d5 <- parameters %>% filter(parameter_type == 'Human delay - serial interval')

p1 <- forest_plot(d1,'Incubation Period (days)',"parameter_type",c(0,30))

#d1$article_label <- d1$refs
#p1_epireview <- epireview::forest_plot_incubation_period(d1,30,FALSE)

p2 <- forest_plot(d5,'Serial Interval (days)',"parameter_type",c(0,20))



#meta-analysis of onset-admission delay
set.seed(42)
m5 <- metamean_wrap(dataframe = d2, estmeansd_method = "Cai",
                    plot_study = TRUE, digits = 2, lims = c(0,6), colour = "dodgerblue3", label = "Mean Onset-Admission Delay (days)",
                    width = 9500, height = 4500, resolution = 1000)

#p5 <- m5$plot

ggsave("figure_delays.png", plot = m5$plot, width = 12, height = 16)
