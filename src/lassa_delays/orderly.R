#task to plot lassa delay figures

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
library(epireview)
if(packageVersion("epireview") < "1.1.1") {
  error_msg <- 
    paste("Please download the latest development version of epireview using:",
          "devtools::install_github('mrc-ide/epireview@develop')")
  stop(error_msg)
}

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("db_compilation", "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "outbreaks.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
orderly_artefact("lassa-specific figures",c("figure_4.png","figure_S5.png",
                                            "figure_4.pdf","figure_S5.pdf"))

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
## HUMAN DELAYS ##
##################

#extract data to be plotted
d1 <- parameters %>% filter(parameter_type == 'Human delay - incubation period')
d2 <- parameters %>% filter(parameter_type == 'Human delay - symptom onset>admission to care')
d3 <- parameters %>% filter(parameter_type == 'Human delay - time in care (length of stay)' |
                            parameter_type == 'Human delay - admission to care>discharge/recovery' |
                            parameter_type == 'Human delay - admission to care>death')
d4 <- parameters %>% filter(parameter_type == 'Human delay - symptom onset>discharge/recovery' |
                            parameter_type == 'Human delay - symptom onset>death')

#arrange data and format for plotting
d1 <- d1 %>% arrange(-central)
d2 <- d2 %>% arrange(-central)
co <- c('Human delay - admission to care>discharge/recovery',
        'Human delay - admission to care>death',
        'Human delay - time in care (length of stay)')
d3 <- d3 %>% mutate(parameter_type = factor(parameter_type, levels = co)) %>%
             arrange(parameter_type,-central)
co <- c('Human delay - symptom onset>discharge/recovery',
        'Human delay - symptom onset>death')
d4 <- d4 %>% mutate(parameter_type = factor(parameter_type, levels = co)) %>%
             arrange(parameter_type,-central)

d3 <- d3 %>% mutate(parameter_type = factor(parameter_type,
                                            levels = unique(parameter_type),
                                            labels = c("Recovery","Death","Unspecified")))
d4 <- d4 %>% mutate(parameter_type = factor(parameter_type,
                                            levels = unique(parameter_type),
                                            labels = c("Recovery","Death")))

#call forest plot function
p1 <- forest_plot(d1,'Incubation Period (days)',"parameter_type",c(0,40))
p2 <- forest_plot(d2,'Onset-Admission Delay (days)',"parameter_type",c(-0.5,40))
p3 <- forest_plot(d3,'Admission-Outcome Delay (days)',"parameter_type",c(-1,40))
p4 <- forest_plot(d4,'Onset-Outcome Delay (days)',"parameter_type",c(0,40))

#meta-analysis of onset-admission delay
set.seed(42)
m5 <- metamean_wrap(dataframe = d2, estmeansd_method = "Cai",
                    plot_study = TRUE, digits = 2, lims = c(0,14), colour = "dodgerblue3", label = "Mean Onset-Admission Delay (days)",
                    width = 9500, height = 4500, resolution = 1000)

p5 <- m5$plot

png(file = "temp.png", width = 9500, height = 6000, res = 750)
meta::funnel(m5$result,studlab = TRUE)
dev.off()

gg <- png::readPNG("temp.png", native = TRUE)
file.remove("temp.png")
funel_plt <- wrap_elements(plot = rasterGrob(gg, interpolate = TRUE))

patchwork <- ((p1 | p2) / (p3 | p4) / p5)
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_4.png", plot = patchwork, width = 12, height = 16)
ggsave("figure_4.pdf", plot = patchwork, width = 12, height = 16)

#figure_S5: hypothetical probability distributions for onset-admission delay
p1 <- pdf_generic(m5$result,"common",c("Gamma","Lognormal","Weibull"),c(-0.2,30),'Onset-Admission Delay (days)')
p2 <- pdf_generic(m5$result,"random",c("Gamma","Lognormal","Weibull"),c(-0.2,30),'Onset-Admission Delay (days)')

patchwork <- (p1 + p2) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_S5.png", plot = patchwork, width = 12, height = 6)
ggsave("figure_S5.pdf", plot = patchwork, width = 12, height = 6)
ggsave("figure_SI_funnel.png", plot = funel_plt, width = 12, height = 8)
