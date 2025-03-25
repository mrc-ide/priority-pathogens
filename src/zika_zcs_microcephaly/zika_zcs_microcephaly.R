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

# Get data
TEXT_SIZE <- 28
cols<-c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "gold1","black",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "green1","darkturquoise",  "yellow4", "yellow3",
  "darkorange4", "brown",
  "mediumpurple3", "darkgreen", "firebrick3", "royalblue3",
  "cyan3", "sienna3", "lightcoral", "chartreuse3",
  "slateblue1"
)


parameters <- readRDS("parameters_curated.rds") 


# Plotting microcephaly risk
zcs_rate <- parameters %>%
  filter(parameter_type == 'Zika congenital syndrome (microcephaly) risk')  %>%
  filter(parameter_unit == 'Percentage (%)') %>%
  mutate(parameter_value_type = ifelse(is.na(parameter_value_type) & 
                                         (!is.na(parameter_value) | !is.na(parameter_uncertainty_upper_value) | 
                                            !is.na(parameter_lower_bound) | !is.na(central)), 
                                       'Central', parameter_value_type),
         case_definition = ifelse(is.na(case_definition), 'Unspecified', case_definition),
         case_definition = case_when(
           case_definition %in% c('Confirmed','Lab confirmed') ~ "Confirmed",
           case_definition %in% c('Clinically diagnosed/symptomatic','Suspected') ~ "Suspected",
           TRUE ~ case_definition)
         )

zcsplot<-forest_plot(zcs_rate %>% arrange(central) ,
                     label = "Forest plot of microcephaly risk (%)", 
            ycol = 'label_group',
            shape_column = "case_definition",
            color_column = "refs", lims = c(0,100),
            custom_colours = cols)
ggsave(filename = 'zcs_plot_loc_country.png', zcsplot, height =12, width = 8, bg = 'white')

zcsplot<-forest_plot(zcs_rate %>% arrange(refs),label = "Forest plot of microcephaly risk (%)", 
                     ycol = 'urefs',
                     shape_column = "case_definition",
                     color_column = "population_country", lims = c(0,100),
                     custom_colours = cols)
ggsave(filename = 'zcs_plot_refs_country.png', zcsplot, height = 8, width = 7, bg = 'white')

# forest_plot(zcs_rate,label = "Forest plot of microcephaly risk (%)", color_column = "case_definition", lims = c(0,100),
#             custom_colours = cols)
# combine the case_definitions -- suspected as clinicallly diagnosed, and combine confirmed and lab confirmed 
# large variability in brazil, uncertainty in honduras bc n?
# comment about sample sizes -- mnay low estimates 

# Meta-analysis  ZCS
metaanalysis_zcs <- metaprop_wrap(dataframe = zcs_rate, plot_pooled = TRUE, subgroup = NA, 
                              sort_by_subg = FALSE, xlabel = "Microcephaly risk given Zika-infected mother",
                                                        plot_study = TRUE, digits = 2, colour = "dodgerblue3",
                                                        width = 9000, height = 16000, resolution = 1000)
zcs_meta <- metaanalysis_zcs$plot

ggsave("zcs_metaanalysis.png", plot = zcs_meta, width = 9, height = 16)



# Miscarriage ----
miscarriage <- parameters %>%
  filter(parameter_type == 'Miscarriage rate') %>%
  mutate(parameter_value_type = ifelse(is.na(parameter_value_type), "Unspecified",parameter_value_type))

misc1<-forest_plot(miscarriage %>% filter(qa_score > 0.5) %>% arrange(desc(central)),
                  label = "Forest plot of miscarriage rate", 
                  shape_column = "population_sample_type", 
                  color_column = "population_country", 
                  lims = c(0,100),
            custom_colours = cols)
misc1_noqa <-forest_plot(miscarriage  %>% arrange(desc(central)),
                   label = "Forest plot of miscarriage rate", 
                   shape_column = "population_sample_type", 
                   color_column = "population_country", 
                   lims = c(0,100),
                   custom_colours = cols)
ggsave(filename = 'miscarriage_refs_sample_country.png', misc1, height = 8, width = 6, bg = 'white')
ggsave(filename = 'miscarriage_refs_sample_country_noqa.png', misc1_noqa, height = 8, width = 6, bg = 'white')

misc2 <-forest_plot(miscarriage%>% filter(qa_score > 0.5),
                  ycol = 'label_group',
                  label = "Forest plot of miscarriage rate", 
                  shape_column = "parameter_value_type", 
                  color_column = "population_sample_type", 
                  lims = c(0,100),
                  custom_colours = cols)
misc2_noqa <-forest_plot(miscarriage%>% arrange(desc(central)),
                    ycol = 'label_group',
                    label = "Forest plot of miscarriage rate", 
                    shape_column = "parameter_value_type", 
                    color_column = "population_sample_type", 
                    lims = c(0,100),
                    custom_colours = cols)
ggsave(filename = 'miscarriage_loc_sample_type.png', misc2, height = 8, width = 6, bg = 'white')
ggsave(filename = 'miscarriage_loc_sample_type_noqa.png', misc2_noqa, height = 8, width = 6, bg = 'white')

# Meta-analysis  miscarriage
metaanalysis_misc <- metaprop_wrap(dataframe = miscarriage, plot_pooled = TRUE, subgroup = NA, 
                              sort_by_subg = FALSE, xlabel = "Miscarriage rate",
                              plot_study = TRUE, digits = 2, colour = "dodgerblue3",
                              width = 9000, height = 6000, resolution = 1000)
misc_meta <- metaanalysis_misc$plot

ggsave("misc_metaanalysis.png", plot = misc_meta, width = 9, height = 6)