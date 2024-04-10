#task to plot lassa transmission figures

library(orderly2)
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
  c("articles.csv", "outbreaks.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
orderly_artefact("lassa-specific figures",c("figure_5.png"))

###################
## DATA CURATION ##
###################

articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("parameters.csv")

dfs <- curation(articles,outbreaks,models,parameters, plotting = TRUE)

articles   <- dfs$articles
outbreaks  <- dfs$outbreaks
models     <- dfs$models
parameters <- dfs$parameters

##################
## TRANSMISSION ##
##################

#extract data to be plotted
d1 <- parameters %>% filter(parameter_type == 'Mutations - evolutionary rate')
d2 <- parameters %>% filter(parameter_type == 'Mutations – substitution rate')
d3 <- parameters %>% filter(parameter_class == 'Relative contribution')
d4 <- parameters %>% filter(parameter_class == 'Attack rate')
d5 <- parameters %>% filter(parameter_class == 'Growth rate')
d6 <- parameters %>% filter(parameter_class == 'Reproduction number')

#arrange data and format for plotting
d1 <- d1 %>% mutate(across(c(parameter_value,parameter_lower_bound, parameter_upper_bound,
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value),
                           ~. * 10^4)) #multiply by 10^4
d2 <- d2 %>% mutate(across(c(parameter_value,parameter_lower_bound, parameter_upper_bound,
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value),
                           ~. * 10^4)) #multiply by 10^4
d4 <- d4 %>% mutate_at(vars(c("parameter_value","parameter_lower_bound","parameter_upper_bound",
                              "parameter_uncertainty_lower_value","parameter_uncertainty_upper_value")), 
                       list(~ ifelse(parameter_unit == "No units", . * 100, .))) %>% mutate(parameter_unit = ifelse(parameter_unit == "No units", "Percentage (%)", parameter_unit))

d1 <- d1 %>% arrange(genome_site,-central) 
d2 <- d2 %>% arrange(genome_site,-central)
d3 <- d3 %>% arrange(-central)
d4 <- d4 %>% mutate(arate=c("Primary","Primary","Secondary")) %>%
             arrange(arate,-central)
d5 <- d5 %>% arrange(-central)
d6 <- d6 %>% arrange(parameter_type,-central)

d2 <- d2 %>% mutate(genome_site = factor(genome_site,
                                         levels = unique(genome_site),
                                         labels = c("L","S")))
d6 <- d6 %>% mutate(parameter_type = factor(parameter_type,
                                            levels = unique(parameter_type),
                                            labels = c("Basic (R0)","Effective (Re)")))

#call forest plot function
p1 <- forest_plot(d1,expression(Evolutionary~Rate~(s/s/y ~10^{-4})),"genome_site",c(5,30)) + guides(color = guide_legend(title = "Gene", order = 1))
p2 <- forest_plot(d2,expression(Substitution~Rate~(s/s/y ~10^{-4})),"genome_site",c(5,30)) + guides(color = guide_legend(title = "Segment", order = 1))
p3 <- forest_plot(d3,'Human-Human Transmission Contribution (%)',"pathogen",c(10,35))
p4 <- forest_plot(d4,'Attack Rate (%)',"arate",c(-0.01,1))
p5 <- forest_plot(d5,'Growth Rate (per day)',"pathogen",c(0,1.25))
p6 <- forest_plot(d6,'Reproduction Number',"parameter_type",c(0.5,2))

patchwork <- (p6 + p5 + p4 + p3 + p1 + p2) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A') #+ plot_layout(guides = 'collect')
ggsave("figure_5.png", plot = patchwork, width = 12, height = 10)