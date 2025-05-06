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
  #"gray70", 
  "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "green1","darkturquoise",  "yellow4", "yellow3",
  "darkorange4","cyan4", "brown1",
  "mediumpurple3", "darkgreen", "firebrick3", "royalblue3",
  "sienna3", "lightcoral", "chartreuse3",
  "darkred", "slateblue1", "mediumseagreen", "navy", "magenta", "olivedrab", "lightsalmon",
  "plum3", "turquoise4", "darkgoldenrod1", "tomato3", "aquamarine3",
  "gray30", "hotpink3", "seagreen", "cornflowerblue"
)


parameters <- readRDS("parameters_curated.rds") 


# Plotting microcephaly risk
zcs_rate <- parameters %>%
  filter(parameter_type == 'Zika congenital syndrome (microcephaly) risk')  %>%
  # mutate(parameter_value = ifelse(!is.na(central) & is.na(parameter_value), central, parameter_value)) %>% # those that don't have a central value have a
  mutate(parameter_unit = 'Percentage (%)') %>% # all are percentages
  mutate(parameter_value_type = ifelse(is.na(parameter_value_type) &
                                         (!is.na(parameter_value) | !is.na(parameter_uncertainty_upper_value) | 
                                            !is.na(parameter_lower_bound) | !is.na(central)), 
                                       'Unspecified', parameter_value_type),
         case_definition = ifelse(is.na(case_definition), 'Unspecified', case_definition),
         case_definition = case_when(
           case_definition %in% c('Confirmed','Lab confirmed') ~ "Confirmed",
           case_definition %in% c('Clinically diagnosed/symptomatic','Suspected') ~ "Suspected",
           case_definition == 'Confirmed;Suspected' ~ "Confirmed or suspected",
           TRUE ~ case_definition) 
  ) %>%
  mutate(population_country = ifelse(population_country %in% c('France (French Guiana)',"France (Martinique, Guadeloupe, French Guiana)"), 'France',population_country)) %>%
  # remove entry that is called 'clearly incorrect' in the paper that reported it (#7050)
  filter(!(covidence_id == 7050 & population_country == 'Argentina'))  %>%
  mutate(continent = countrycode(sourcevar = population_country,
                                 origin = "country.name",
                                 destination = "continent")) %>%
  mutate(continent = ifelse(population_country %in% c('France (French Guiana)','France (Martinique, Guadeloupe, French Guiana)'),
                            'Americas', continent)) #%>%
  # select(parameter_type,parameter_value, parameter_unit, continent, population_country, population_location, population_sample_type, population_group,
  #        refs, central, case_definition, parameter_upper_bound, parameter_lower_bound, parameter_uncertainty_lower_value, parameter_uncertainty_upper_value,
  #        parameter_uncertainty_type, population_study_start_day, population_study_start_month,population_study_start_year, population_study_end_day,
  #        population_study_end_month, population_study_end_year, qa_score)

zcs_rate_qa <- zcs_rate %>%
  filter(qa_score >= 0.5)
length(unique(zcs_rate_qa$covidence_id))
table(zcs_rate_qa$continent, useNA = 'ifany')
max(zcs_rate_qa$population_study_end_year, na.rm = TRUE)

zcsplot <- forest_plot(zcs_rate %>% filter(qa_score >= 0.5) %>% arrange(central) ,
                       label = "Microcephaly risk given Zika-infected mother (%)", 
                       facet_by_country = TRUE,
                       ycol = 'label_group',
                       text_size = 25,
                       shape_column = "case_definition",
                       color_column = "refs", lims = c(0,100),
                       custom_colours = cols) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.57, .45))
zcsplot_noqa <- forest_plot(zcs_rate %>% arrange(central) ,
                            label = "Microcephaly risk given Zika-infected mother (%)", 
                            facet_by_country = TRUE,
                            ycol = 'label_group',
                            text_size = 25,
                            shape_column = "case_definition",
                            color_column = "refs", lims = c(0,100),
                            custom_colours = cols) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.57, .45))
ggsave(filename = 'zcs_plot_loc_country.png', zcsplot, height =20, width = 13, bg = 'white')
ggsave(filename = 'zcs_plot_loc_country.pdf', zcsplot, height =20, width = 13, bg = 'white')
ggsave(filename = 'zcs_plot_loc_country_noqa.png', zcsplot_noqa, height =30, width = 16, bg = 'white')
ggsave(filename = 'zcs_plot_loc_country_noqa.pdf', zcsplot_noqa, height =28, width = 16, bg = 'white')

zcsplot_urefs <- forest_plot(zcs_rate %>% filter(qa_score >= 0.5)%>% arrange(refs),
                             label = "Microcephaly risk given Zika-infected mother (%)", 
                             ycol = 'urefs',
                             shape_column = "case_definition",
                             color_column = "population_country", lims = c(0,100),
                             custom_colours = cols) + 
  theme(legend.position = 'inside')
zcsplot__urefsnoqa <- forest_plot(zcs_rate %>% arrange(refs),
                                  label = "Microcephaly risk given Zika-infected mother (%)", 
                                  ycol = 'urefs',
                                  shape_column = "case_definition",
                                  color_column = "population_country", lims = c(0,100),
                                  custom_colours = cols) + 
  theme(legend.position = 'inside')
ggsave(filename = 'zcs_plot_refs_country.png', zcsplot_urefs, height = 12, width = 10, bg = 'white')
ggsave(filename = 'zcs_plot_refs_country.pdf', zcsplot_urefs, height = 12, width = 10, bg = 'white')
ggsave(filename = 'zcs_plot_refs_country_noqa.png', zcsplot__urefsnoqa, height = 12, width = 10, bg = 'white')
ggsave(filename = 'zcs_plot_refs_country_noqa.pdf', zcsplot__urefsnoqa, height = 12, width = 10, bg = 'white')

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
                                  plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                  width = 9000, height = 19000, resolution = 1000)
zcs_meta <- metaanalysis_zcs$plot

ggsave(filename = "zcs_metaanalysis.png", plot = zcs_meta, width = 9, height = 19)
ggsave(filename = "zcs_metaanalysis.pdf", plot = zcs_meta, width = 9, height = 19)

metaanalysis_zcs_continent <- metaprop_wrap(dataframe = zcs_rate %>% filter(qa_score >= 0.5 & !is.na(continent)), 
                                            plot_pooled = TRUE, subgroup = 'continent', 
                                            sort_by_subg = TRUE, xlabel = "Microcephaly risk given Zika-infected mother",
                                            plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                            width = 9000, height = 19000, resolution = 1000)
zcs_meta_continent <- metaanalysis_zcs_continent$plot

ggsave(filename = "zcs_metaanalysis_continent.png", plot = zcs_meta_continent, width = 9, height = 19)
ggsave(filename = "zcs_metaanalysis_continent.pdf", plot = zcs_meta_continent, width = 9, height = 19)


metaanalysis_zcs_country <- metaprop_wrap(dataframe = zcs_rate %>% filter(qa_score >= 0.5 & population_country %in%c('Brazil','Colombia','France','Puerto Rico','United States')), 
                                          plot_pooled = TRUE, subgroup = 'population_country', 
                                          sort_by_subg = TRUE, xlabel = "Microcephaly risk given Zika-infected mother",
                                          plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                          width = 9000, height =19000, resolution = 1000)
zcs_meta_country <- metaanalysis_zcs_country$plot
ggsave(filename = "zcs_metaanalysis_country.png", plot = zcs_meta_country, width = 9, height = 19)
ggsave(filename = "zcs_metaanalysis_country.pdf", plot = zcs_meta_country, width = 9, height = 19)


metaanalysis_zcs_sampletype <- metaprop_wrap(dataframe = zcs_rate %>% filter(qa_score >= 0.5 & !is.na(population_sample_type)), 
                                             plot_pooled = TRUE, subgroup = 'population_sample_type', 
                                             sort_by_subg = TRUE, xlabel = "Microcephaly risk given Zika-infected mother",
                                             plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                             width = 9000, height =19000, resolution = 1000)
zcs_meta_sampletype <- metaanalysis_zcs_sampletype$plot

ggsave(filename = "zcs_meta_sampletype.png", plot = zcs_meta_sampletype, width = 9, height = 19)
ggsave(filename = "zcs_meta_sampletype.pdf", plot = zcs_meta_sampletype, width = 9, height = 19)

metaanalysis_zcs_sampletype_noqa <- metaprop_wrap(dataframe = zcs_rate %>% filter(!is.na(population_sample_type)), 
                                             plot_pooled = TRUE, subgroup = 'population_sample_type', 
                                             sort_by_subg = TRUE, xlabel = "Microcephaly risk given Zika-infected mother",
                                             plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                             width = 9000, height =21000, resolution = 1000)
zcs_meta_sampletype_noqa <- metaanalysis_zcs_sampletype_noqa$plot

# metaanalysis_zcs_popgroup <- metaprop_wrap(dataframe = zcs_rate %>% filter(qa_score >= 0.5 & !is.na(population_group)), 
#                                              plot_pooled = TRUE, subgroup = 'population_group', 
#                                              sort_by_subg = TRUE, xlabel = "Microcephaly risk given Zika-infected mother",
#                                              plot_study = TRUE, digits = 4, colour = "dodgerblue3",
#                                              width = 9000, height =18000, resolution = 1000)
# zcs_meta_popgroup <- metaanalysis_zcs_popgroup$plot
# 
# ggsave("zcs_meta_popgroup.png", plot = zcs_meta_popgroup, width = 9, height = 16)
# ggsave("zcs_meta_popgroup.pdf", plot = zcs_meta_popgroup, width = 9, height = 16)

metaanalysis_zcs_noqa <- metaprop_wrap(dataframe = zcs_rate , plot_pooled = TRUE, subgroup = NA, 
                                       sort_by_subg = FALSE, xlabel = "Microcephaly risk given Zika-infected mother",
                                       plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                       width = 9000, height = 21000, resolution = 1000)
zcs_meta_noqa <- metaanalysis_zcs_noqa$plot

ggsave(filename = "zcs_metaanalysis.png", plot = zcs_meta_noqa, width = 9, height = 21)
ggsave(filename = "zcs_metaanalysis.pdf", plot = zcs_meta_noqa, width = 9, height = 21)



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
metaanalysis_misc <- metaprop_wrap(dataframe = miscarriage %>% filter(qa_score >= 0.5), plot_pooled = TRUE, subgroup = NA, 
                                   sort_by_subg = FALSE, xlabel = "Miscarriage rate",
                                   plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                   width = 9000, height = 6000, resolution = 1000)
misc_meta <- metaanalysis_misc$plot

ggsave(filename = "misc_metaanalysis.png", plot = misc_meta, width = 9, height = 6)
ggsave(filename = "misc_metaanalysis.pdf", plot = misc_meta, width = 9, height = 6)

metaanalysis_continent <- metaprop_wrap(dataframe = miscarriage %>% filter(qa_score >= 0.5 & !is.na(continent)), 
                                        plot_pooled = TRUE, subgroup = 'continent', 
                                        sort_by_subg = FALSE, xlabel = "Miscarriage rate",
                                        plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                        width = 9000, height = 8000, resolution = 1000)
misc_meta_continent <- metaanalysis_continent$plot

ggsave(filename = "misc_metaanalysis_continent.png", plot = misc_meta_continent, width = 9, height = 8)
ggsave(filename = "misc_metaanalysis_continent.pdf", plot = misc_meta_continent, width = 9, height = 8)

metaanalysis_sampletype <- metaprop_wrap(dataframe = miscarriage %>% filter(qa_score >= 0.5 & !is.na(population_sample_type)), 
                                         plot_pooled = TRUE, subgroup = 'population_sample_type', 
                                         sort_by_subg = FALSE, xlabel = "Miscarriage rate",
                                         plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                         width = 9000, height = 8000, resolution = 1000)
misc_meta_sampletype <- metaanalysis_sampletype$plot

ggsave("misc_metaanalysis_sampletype.png", plot = misc_meta_sampletype, width = 9, height = 8)
ggsave("misc_metaanalysis_sampletype.pdf", plot = misc_meta_sampletype, width = 9, height = 8)


# Create and save plot for main analysis 
layout = "
AABBBB
AABBBB"
zcs_plot_combined <- zcsplot + zcs_meta_sampletype +
  plot_layout(design = layout) #+ plot_annotation(tag_levels = 'A')
ggsave(filename = "zcs_plot_combined.png", plot = zcs_plot_combined, width = 35, height = 30, bg = 'white', dpi = 600)
ggsave(filename = "zcs_plot_combined.pdf", plot = zcs_plot_combined, width = 35, height = 30, bg = 'white', dpi = 600)

zcs_plot_combined_noqa <- zcsplot_noqa + zcs_meta_sampletype_noqa +
  plot_layout(design = layout) #+ plot_annotation(tag_levels = 'A')
ggsave(filename = "zcs_plot_combined_noqa.png", plot = zcs_plot_combined_noqa, width = 35, height = 30, bg = 'white', dpi = 600)
ggsave(filename = "zcs_plot_combined_noqa.pdf", plot = zcs_plot_combined_noqa, width = 35, height = 30, bg = 'white', dpi = 600)
