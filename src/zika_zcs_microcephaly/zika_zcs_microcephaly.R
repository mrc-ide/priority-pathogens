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
library(countrycode)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
# orderly_dependency("zika_compilation", "latest(parameter:pathogen == this:pathogen)",
#                    c("articles.rds", "outbreaks.rds", "models.rds", "parameters.rds"))
orderly_dependency("zika_prep_data", "latest(parameter:pathogen == this:pathogen &&
                   parameter:plotting == TRUE)",
                   c("articles_curated.rds", "outbreaks_curated.rds", "models_curated.rds", "parameters_curated.rds"))
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
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
  "cyan4", "sienna3", "lightcoral", "chartreuse3",
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
         ) %>%
  # remove entry that is called 'clearly incorrect' in the paper that reported it (#7050)
  filter(!(covidence_id == 7050 & population_country == 'Argentina'))  %>%
  mutate(continent = countrycode(sourcevar = population_country,
                                 origin = "country.name",
                                 destination = "continent"))

zcs_rate_qa <- zcs_rate %>%
  filter(qa_score >= 0.5)
length(unique(zcs_rate_qa$covidence_id))

zcsplot <- forest_plot(zcs_rate %>% filter(qa_score >= 0.5) %>% arrange(central) ,
                     label = "CZS/Microcephaly risk (%)", 
                     facet_by_country = TRUE,
                     ycol = 'label_group',
            shape_column = "case_definition",
            color_column = "refs", lims = c(0,100),
            custom_colours = cols)
zcsplot_noqa <- forest_plot(zcs_rate %>% arrange(central) ,
                     label = "CZS/Microcephaly risk (%)", 
                     facet_by_country = TRUE,
                     ycol = 'label_group',
                     shape_column = "case_definition",
                     color_column = "refs", lims = c(0,100),
                     custom_colours = cols)
ggsave(filename = 'zcs_plot_loc_country.png', zcsplot, height =13, width = 11, bg = 'white')
ggsave(filename = 'zcs_plot_loc_country.pdf', zcsplot, height =13, width = 11, bg = 'white')
ggsave(filename = 'zcs_plot_loc_country_noqa.png', zcsplot_noqa, height =13, width = 11, bg = 'white')
ggsave(filename = 'zcs_plot_loc_country_noqa.pdf', zcsplot_noqa, height =13, width = 11, bg = 'white')

zcsplot <- forest_plot(zcs_rate %>% filter(qa_score >= 0.5)%>% arrange(refs),
                       label = "CZS/Microcephaly risk (%)", 
                       ycol = 'urefs',
                     shape_column = "case_definition",
                     color_column = "population_country", lims = c(0,100),
                     custom_colours = cols)
zcsplot_noqa <- forest_plot(zcs_rate %>% arrange(refs),
                            label = "CZS/Microcephaly risk (%)", 
                            ycol = 'urefs',
                     shape_column = "case_definition",
                     color_column = "population_country", lims = c(0,100),
                     custom_colours = cols)
ggsave(filename = 'zcs_plot_refs_country.png', zcsplot, height = 12, width = 10, bg = 'white')
ggsave(filename = 'zcs_plot_refs_country.pdf', zcsplot, height = 12, width = 10, bg = 'white')
ggsave(filename = 'zcs_plot_refs_country_noqa.png', zcsplot_noqa, height = 12, width = 10, bg = 'white')
ggsave(filename = 'zcs_plot_refs_country_noqa.pdf', zcsplot_noqa, height = 12, width = 10, bg = 'white')

# forest_plot(zcs_rate,label = "Forest plot of microcephaly risk (%)", color_column = "case_definition", lims = c(0,100),
#             custom_colours = cols)
# combine the case_definitions -- suspected as clinicallly diagnosed, and combine confirmed and lab confirmed 
# large variability in brazil, uncertainty in honduras bc n?
# comment about sample sizes -- mnay low estimates 

# Meta-analysis  ZCS
zcsqa <- zcs_rate %>% filter(qa_score >= 0.5)
metaanalysis_zcs <- metaprop_wrap(dataframe = zcs_rate %>% filter(qa_score >= 0.5), 
                                  plot_pooled = TRUE, subgroup = NA, 
                                  sort_by_subg = FALSE, xlabel = "Microcephaly risk given Zika-infected mother",
                                  plot_study = TRUE, digits = 2, colour = "dodgerblue3",
                                  width = 9000, height = 16000, resolution = 1000)
zcs_meta <- metaanalysis_zcs$plot

ggsave("zcs_metaanalysis.png", plot = zcs_meta, width = 9, height = 16)
ggsave("zcs_metaanalysis.pdf", plot = zcs_meta, width = 9, height = 16)

metaanalysis_zcs_continent <- metaprop_wrap(dataframe = zcs_rate %>% filter(qa_score >= 0.5 & !is.na(continent)), 
                                  plot_pooled = TRUE, subgroup = 'continent', 
                                  sort_by_subg = TRUE, xlabel = "Microcephaly risk given Zika-infected mother",
                                  plot_study = TRUE, digits = 2, colour = "dodgerblue3",
                                  width = 9000, height = 16000, resolution = 1000)
zcs_meta_continent <- metaanalysis_zcs_continent$plot

ggsave("zcs_metaanalysis_continent.png", plot = zcs_meta_continent, width = 9, height = 16)
ggsave("zcs_metaanalysis_continent.pdf", plot = zcs_meta_continent, width = 9, height = 16)


metaanalysis_zcs_country <- metaprop_wrap(dataframe = zcs_rate %>% filter(qa_score >= 0.5 & population_country %in%c('Brazil','Colombia','France','Puerto Rico','United States')), 
                                            plot_pooled = TRUE, subgroup = 'population_country', 
                                            sort_by_subg = TRUE, xlabel = "Microcephaly risk given Zika-infected mother",
                                            plot_study = TRUE, digits = 2, colour = "dodgerblue3",
                                            width = 9000, height =16000, resolution = 1000)
zcs_meta_country <- metaanalysis_zcs_country$plot

ggsave("zcs_metaanalysis_country.png", plot = zcs_meta_country, width = 9, height = 16)
ggsave("zcs_metaanalysis_country.pdf", plot = zcs_meta_country, width = 9, height = 16)

metaanalysis_zcs_noqa <- metaprop_wrap(dataframe = zcs_rate , plot_pooled = TRUE, subgroup = NA, 
                                  sort_by_subg = FALSE, xlabel = "Microcephaly risk given Zika-infected mother",
                                  plot_study = TRUE, digits = 2, colour = "dodgerblue3",
                                  width = 9000, height = 16000, resolution = 1000)
zcs_meta_noqa <- metaanalysis_zcs_noqa$plot

ggsave("zcs_metaanalysis.png", plot = zcs_meta_noqa, width = 9, height = 16)
ggsave("zcs_metaanalysis.pdf", plot = zcs_meta_noqa, width = 9, height = 16)



# Miscarriage ----
miscarriage <- parameters %>%
  filter(parameter_type == 'Miscarriage rate') %>%
  mutate(parameter_value_type = ifelse(is.na(parameter_value_type), "Unspecified",parameter_value_type)) %>%
  mutate(continent = countrycode(sourcevar = population_country,
                                 origin = "country.name",
                                 destination = "continent"),
         continent = ifelse(is.na(continent), 'Americas',continent))# %>%
  # select(continent, central, parameter_value, cfr_ifr_denominator, cfr_ifr_numerator, parameter_value_type, population_country, population_location)
misc_qa <- miscarriage %>%
  filter(qa_score >= 0.5)
length(unique(miscarriage$covidence_id))
length(unique(misc_qa$covidence_id))

misc1<-forest_plot(miscarriage %>% filter(qa_score >= 0.5) %>% arrange(desc(central)),
                  label = "Miscarriage rate (%)", 
                  shape_column = "population_sample_type", 
                  color_column = "population_country", 
                  lims = c(0,30),
            custom_colours = cols)
misc1_noqa <-forest_plot(miscarriage  %>% arrange(desc(central)),
                   label = "Miscarriage rate (%)", 
                   shape_column = "population_sample_type", 
                   color_column = "population_country", 
                   lims = c(0,30),
                   custom_colours = cols)
ggsave(filename = 'miscarriage_refs_sample_country.png', misc1, height = 8, width = 6, bg = 'white')
ggsave(filename = 'miscarriage_refs_sample_country_noqa.png', misc1_noqa, height = 8, width = 6, bg = 'white')
ggsave(filename = 'miscarriage_refs_sample_country.pdf', misc1, height = 8, width = 6, bg = 'white')
ggsave(filename = 'miscarriage_refs_sample_country_noqa.pdf', misc1_noqa, height = 8, width = 6, bg = 'white')

misc2 <-forest_plot(miscarriage%>% filter(qa_score >= 0.5),
                  ycol = 'label_group',
                  label = "Miscarriage rate (%)", 
                  facet_by_country = TRUE,
                  shape_column = 'population_sample_type',
                  # shape_column = "parameter_value_type", 
                  # color_column = "population_sample_type",
                  color_column = "refs",
                  lims = c(0,30),
                  custom_colours = cols)
misc2_noqa <-forest_plot(miscarriage%>% arrange(desc(central)),
                    ycol = 'label_group',
                    facet_by_country = TRUE,
                    label = "Miscarriage rate (%)", 
                    shape_column = 'population_sample_type',
                    # shape_column = "parameter_value_type", 
                    # color_column = "population_sample_type", 
                    color_column = "refs",
                    lims = c(0,30),
                    custom_colours = cols)
ggsave(filename = 'miscarriage_loc_sample_type.png', misc2, height = 8, width = 8, bg = 'white')
ggsave(filename = 'miscarriage_loc_sample_type_noqa.png', misc2_noqa, height = 8, width = 8, bg = 'white')
ggsave(filename = 'miscarriage_loc_sample_type.pdf', misc2, height = 8, width = 8, bg = 'white')
ggsave(filename = 'miscarriage_loc_sample_type_noqa.pdf', misc2_noqa, height = 8, width = 8, bg = 'white')

# Meta-analysis  miscarriage
metaanalysis_misc <- metaprop_wrap(dataframe = miscarriage %>% filter(qa_score >= 0.05), plot_pooled = TRUE, subgroup = NA, 
                              sort_by_subg = FALSE, xlabel = "Miscarriage rate",
                              plot_study = TRUE, digits = 2, colour = "dodgerblue3",
                              width = 9000, height = 6000, resolution = 1000)
misc_meta <- metaanalysis_misc$plot

ggsave("misc_metaanalysis.png", plot = misc_meta, width = 9, height = 6)
ggsave("misc_metaanalysis.pdf", plot = misc_meta, width = 9, height = 6)

metaanalysis_continent <- metaprop_wrap(dataframe = miscarriage %>% filter(qa_score >= 0.05 & !is.na(continent)), 
                                        plot_pooled = TRUE, subgroup = 'continent', 
                                   sort_by_subg = FALSE, xlabel = "Miscarriage rate",
                                   plot_study = TRUE, digits = 2, colour = "dodgerblue3",
                                   width = 9000, height = 8000, resolution = 1000)
misc_meta_continent <- metaanalysis_continent$plot

ggsave("misc_metaanalysis_continent.png", plot = misc_meta_continent, width = 9, height = 8)
ggsave("misc_metaanalysis_continent.pdf", plot = misc_meta_continent, width = 9, height = 8)