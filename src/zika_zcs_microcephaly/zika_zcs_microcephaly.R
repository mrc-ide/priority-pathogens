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
  "gray70", 
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


# Plotting microcephaly probability

CZS_rate <- parameters %>%
  filter(parameter_type == 'Zika congenital syndrome (microcephaly) probability')  %>%
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
  mutate(population_country = case_when(population_country %in% c('France (French Guiana)',"France (Martinique, Guadeloupe, French Guiana)") ~ 'France',
                                        population_location == 'Guadeloupe, Martinique, French Guiana' ~ NA,
                                        TRUE ~ population_country),
         population_location = case_when(population_location == 'Hospital Universitari Vall d’Hebron Barcelona, Catalonia' ~ 'Barcelona', 
                                         population_location == 'Campina Grande, State of Paraíba' ~ "Campina Grande, Paraíba",
                                         TRUE ~ population_location)) %>%
  # remove entry that is called 'clearly incorrect' in the paper that reported it (#7050)
  filter(!(covidence_id == 7050 & population_country == 'Argentina'))  %>%
  mutate(continent = countrycode(sourcevar = population_country,
                                 origin = "country.name",
                                 destination = "continent")) %>%
  mutate(continent = ifelse(population_location %in% c('French Guiana','Martinique, Guadeloupe, French Guiana'),
                            'Americas', continent)) %>%
  mutate(continent = case_when(
    grepl(population_country, 'Multi') ~ "Multi-country",
    population_country == 'France' & population_location == 'French Guiana' ~ "Americas",
    population_location %in% c('Martinique','Guadeloupe','Saint Martin') ~ "Americas",
    population_country == "France (Martinique, Guadeloupe, French Guiana)" ~ "Americas",
    population_country == "Multi-country: France and Netherlands" ~ "Europe",
    population_country == "Micronesia" ~ "Oceania",
    population_country == 'Multi-country: Brazil and Colombia'  ~ "Americas",
    population_country == "Multi-country: Brazil, Colombia, El Salvador" ~ "Americas",
    population_country == "Multi-country: Americas (n = 32)" ~ "Americas",
    population_country == "Singapore" | population_location == 'Singapore' ~ "Asia",
    population_country == 'Brazil' & population_location != 'Brazil' ~ "Brazil (Americas)",
    # population_country == 'French Polynesia' & population_location != 'French Polynesia' ~ "French Polynesia (Oceania)",
    population_country == 'France' & grepl('French Polynesia', population_location) & population_location != 'French Polynesia' ~ "French Polynesia, France (Oceania)",
    population_location == 'French Polynesia' ~ "Oceania",
    population_location == 'Sous-le-vent Islands' ~ "French Polynesia, France (Oceania)",
    population_country == 'Colombia' & population_location != "Colombia" ~ "Colombia (Americas)",
    population_country == 'France' & population_location == 'New Caledonia' ~ "Oceania",
    population_country == 'France' & population_location == 'French Guiana' ~ "Americas",
    population_country == 'Unspecified' ~ "Unspecified",
    TRUE ~ continent
  )) %>%
  mutate(population_sample_type = case_when(
    is.na(population_sample_type) | population_sample_type %in% c('Mixed settings', 'Community based','Other','Travel based') ~ "Other",
    TRUE ~ population_sample_type
  ),
  population_sample_type = factor(population_sample_type, levels = c('Hospital based','Other','Population based'))) 

# for 6810, am combining the 3 cluster level estimates into 1 overall estimate 
par6810 <- CZS_rate %>%
  filter(covidence_id == 6810) %>%
  mutate(parameter_value = 38.5, # from counting red and grey squares in figure 3a
         population_sample_size = sum(population_sample_size)) %>%
  distinct(covidence_id, .keep_all = TRUE)

CZS_rate <- rbind(CZS_rate %>%
                    filter(covidence_id != 6810), par6810)

# select(parameter_type,parameter_value, parameter_unit, continent, population_country, population_location, population_sample_type, population_group,
#        refs, central, case_definition, parameter_upper_bound, parameter_lower_bound, parameter_uncertainty_lower_value, parameter_uncertainty_upper_value,
#        parameter_uncertainty_type, population_study_start_day, population_study_start_month,population_study_start_year, population_study_end_day,
#        population_study_end_month, population_study_end_year, qa_score)

CZS_rate_qa <- CZS_rate %>%
  filter(qa_score >= 0.5)
length(unique(CZS_rate_qa$covidence_id))
janitor::tabyl(CZS_rate_qa$continent, useNA = 'ifany')
max(CZS_rate_qa$population_study_end_year, na.rm = TRUE)
min(CZS_rate_qa$population_study_start_year, na.rm = TRUE)
table(CZS_rate_qa$trimester_exposed, useNA = 'ifany')

CZSplot <- forest_plot(CZS_rate %>% filter(qa_score >= 0.5) %>% arrange(central) ,
                       label = "CZS probability given Zika-infected mother (%)", 
                       facet_by_poptype = TRUE,
                       ycol = 'label_group',
                       text_size = 23,
                       shape_column = "case_definition",
                       color_column = "refs", lims = c(0,100),
                       custom_colours = cols) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.66, 0.3))
CZSplot_noqa <- forest_plot(CZS_rate %>% arrange(central) ,
                            label = "CZS probability given Zika-infected mother (%)", 
                            facet_by_poptype = TRUE,
                            ycol = 'label_group',
                            text_size = 23,
                            shape_column = "case_definition",
                            color_column = "refs", lims = c(0,100),
                            custom_colours = cols) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.55, 0.3))
ggsave(filename = 'CZS_plot_loc_country.svg', CZSplot, height =26, width = 14, bg = 'white')
ggsave(filename = 'CZS_plot_loc_country.pdf', CZSplot, height =26, width = 14, bg = 'white')
ggsave(filename = 'CZS_plot_loc_country_noqa.svg', CZSplot_noqa, height =32, width = 16, bg = 'white')
ggsave(filename = 'CZS_plot_loc_country_noqa.pdf', CZSplot_noqa, height =32, width = 16, bg = 'white')

CZSplot_urefs <- forest_plot(CZS_rate %>% filter(qa_score >= 0.5)%>% arrange(refs),
                             label = "CZS probability given Zika-infected mother (%)", 
                             ycol = 'urefs',
                             shape_column = "case_definition",
                             color_column = "population_country", lims = c(0,100),
                             custom_colours = cols) + 
  theme(legend.position = 'inside')
CZSplot__urefsnoqa <- forest_plot(CZS_rate %>% arrange(refs),
                                  label = "CZS probability given Zika-infected mother (%)", 
                                  ycol = 'urefs',
                                  shape_column = "case_definition",
                                  color_column = "population_country", lims = c(0,100),
                                  custom_colours = cols) + 
  theme(legend.position = 'inside')
ggsave(filename = 'CZS_plot_refs_country.png', CZSplot_urefs, height = 12, width = 10, bg = 'white')
ggsave(filename = 'CZS_plot_refs_country.pdf', CZSplot_urefs, height = 12, width = 10, bg = 'white')
ggsave(filename = 'CZS_plot_refs_country_noqa.png', CZSplot__urefsnoqa, height = 12, width = 10, bg = 'white')
ggsave(filename = 'CZS_plot_refs_country_noqa.pdf', CZSplot__urefsnoqa, height = 12, width = 10, bg = 'white')

# forest_plot(CZS_rate,label = "Forest plot of microcephaly probability (%)", color_column = "case_definition", lims = c(0,100),
#             custom_colours = cols)
# combine the case_definitions -- suspected as clinicallly diagnosed, and combine confirmed and lab confirmed 
# large variability in brazil, uncertainty in honduras bc n?
# comment about sample sizes -- mnay low estimates 

# Meta-analysis  CZS
CZSqa <- CZS_rate %>% filter(qa_score >= 0.5) %>%
  filter(metaanalysis_inclusion == 'include')

metaanalysis_CZS <- metaprop_wrap(dataframe = CZSqa %>% filter(trimester_exposed %in% c('Unspecified','Whole pregnancy')), 
                                  plot_pooled = TRUE, subgroup = NA, 
                                  sort_by_subg = FALSE, xlabel = "CZS probability given Zika-infected mother",
                                  plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                  width = 6000, height = 6000, resolution = 600)
CZS_meta <- metaanalysis_CZS$plot

ggsave(filename = "CZS_metaanalysis.png", plot = CZS_meta, width = 6, height = 6)
ggsave(filename = "CZS_metaanalysis.pdf", plot = CZS_meta, width = 6, height = 6)

metaanalysis_CZS_continent <- metaprop_wrap(dataframe = CZSqa %>% filter(!is.na(continent) & trimester_exposed %in% c('Unspecified','Whole pregnancy')), 
                                            plot_pooled = TRUE, subgroup = 'continent', 
                                            sort_by_subg = TRUE, xlabel = "CZS probability given Zika-infected mother",
                                            plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                            width = 5000, height = 9000, resolution = 600)
CZS_meta_continent <- metaanalysis_CZS_continent$plot

ggsave(filename = "CZS_metaanalysis_continent.png", plot = CZS_meta_continent, width = 5, height = 9)
ggsave(filename = "CZS_metaanalysis_continent.pdf", plot = CZS_meta_continent, width = 5, height = 9)

# Country meta-analysis 
CZS_rate <- CZSqa %>%
  mutate(label_group = case_when(
    is.na(population_location) | population_location == '' ~ population_country,
    is.na(population_country) | population_country=='' ~ population_location,
    (!is.na(population_location) & population_country == 'Unspecified') | 
      ((population_country == 'Brazil' | population_country == "Colombia" | population_country == 'French Polynesia')) ~ population_location,
    (population_location != "" & !is.na(population_location)) & population_country != 'Unspecified' ~ paste0(population_location,'\n' ,' (',population_country,')'),
    TRUE ~ NA),
    label_group = case_when(
      label_group == 'continental United States and Hawaii\n (United States)' ~ 'United States',
      label_group == 'Singapore\n (Singapore)' ~ 'Singapore',
      # label_group == 'Brazil' ~ "Brazil (national)",
      TRUE ~ label_group),
    label = paste0(population_location, "; ", refs))

#Brazil
metaanalysis_CZS_brazil <- metaprop_wrap(dataframe = CZSqa %>% 
                                           filter(population_country %in% c('Brazil') & trimester_exposed %in% c('Unspecified','Whole pregnancy')),#paste0(refs, ": ", population_location)), 
                                         plot_pooled = TRUE, subgroup = 'population_sample_type', 
                                         sort_by_subg = TRUE, xlabel = "CZS probability given Zika-infected mother",
                                         plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                         width = 6000, height = 5000, resolution = 600)
CZS_meta_brazil <- metaanalysis_CZS_brazil$plot
png(file = "temp.png", width = 4000, height = 5000, res = 600)
funnel_czsmeta_brazil <- funnel(metaanalysis_CZS_brazil$result, common = FALSE,#only plots funnel for either common or random for some reason
                                pch = 22, bg = "dodgerblue3", level = 0.95)#, 
# studlab = TRUE, cex.studlab = 0.75, pos.studlab = 2)
dev.off()
gg <- png::readPNG("temp.png", native = TRUE)
file.remove("temp.png")
funnel_czsbrazil <- wrap_elements(plot = rasterGrob(gg, interpolate = TRUE))

#Colombia
# metaanalysis_CZS_colombia <- metaprop_wrap(dataframe = CZSqa %>% 
#                                              filter(population_country %in%c('Colombia')), 
#                                            plot_pooled = TRUE, subgroup = 'population_sample_type', 
#                                            sort_by_subg = TRUE, xlabel = "CZS probability given Zika-infected mother",
#                                            plot_study = TRUE, digits = 4, colour = "dodgerblue3",
#                                            width = 5000, height =2700, resolution = 600)
# CZS_meta_colombia <- metaanalysis_CZS_colombia$plot
# png(file = "temp.png", width = 4000, height = 5000, res = 600)
# funnel_czsmeta_colombia <- funnel(metaanalysis_CZS_colombia$result, common = FALSE,#only plots funnel for either common or random for some reason
#                                 pch = 22, bg = "dodgerblue3", level = 0.95)#, 
#                                 # studlab = TRUE, cex.studlab = 0.75, pos.studlab = 2)
# dev.off()
# gg <- png::readPNG("temp.png", native = TRUE)
# file.remove("temp.png")
# funnel_czscolombia <- wrap_elements(plot = rasterGrob(gg, interpolate = TRUE))

# France
metaanalysis_CZS_france <- metaprop_wrap(dataframe = CZSqa %>% 
                                           filter(population_country %in%c('France') & trimester_exposed %in% c('Unspecified','Whole pregnancy')), 
                                         # studylabels = 'label',
                                         plot_pooled = TRUE, subgroup = 'population_sample_type', 
                                         sort_by_subg = TRUE, xlabel = "CZS probability given Zika-infected mother",
                                         plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                         width = 6000, height =2600, resolution = 600)
CZS_meta_france <- metaanalysis_CZS_france$plot
png(file = "temp.png", width = 4000, height = 5000, res = 600)
funnel_czsmeta_france <- funnel(metaanalysis_CZS_france$result, common = FALSE,#only plots funnel for either common or random for some reason
                                pch = 22, bg = "dodgerblue3", level = 0.95)#, 
# studlab = TRUE, cex.studlab = 0.75, pos.studlab = 2)
dev.off()
gg <- png::readPNG("temp.png", native = TRUE)
file.remove("temp.png")
funnel_czsfrance <- wrap_elements(plot = rasterGrob(gg, interpolate = TRUE))

ggsave(filename = "CZS_metaanalysis_brazil.pdf", plot = CZS_meta_brazil, width = 6, height = 5)
# ggsave(filename = "CZS_metaanalysis_colombia.pdf", plot = CZS_meta_colombia, width = 5, height = 2.7)
ggsave(filename = "CZS_metaanalysis_france.pdf", plot = CZS_meta_france, width = 6, height = 2.6)
ggsave("CZS_combocountries_funnel.pdf", plot = funnel_czsbrazil  + funnel_czsfrance+ plot_annotation(tag_levels = 'A'), width = 8, height = 4)#+ funnel_czscolombia



metaanalysis_CZS_sampletype <- metaprop_wrap(dataframe = CZSqa %>% filter(!is.na(population_sample_type) & trimester_exposed %in% c('Unspecified','Whole pregnancy')), 
                                             plot_pooled = TRUE, subgroup = 'population_sample_type', 
                                             sort_by_subg = TRUE, xlabel = "CZS probability given Zika-infected mother",
                                             plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                             width = 5000, height =8000, resolution = 600)
CZS_meta_sampletype <- metaanalysis_CZS_sampletype$plot
png(file = "temp.png", width = 7000, height = 6000, res = 600)
funnel_czsmeta_sampletype <- funnel(metaanalysis_CZS_sampletype$result, common = FALSE,#only plots funnel for either common or random for some reason
                                    pch = 22, bg = "dodgerblue3", level = 0.95)#, 
# studlab = TRUE, cex.studlab = 0.75, pos.studlab = 2)
dev.off()
gg <- png::readPNG("temp.png", native = TRUE)
file.remove("temp.png")
funnel_czssampletype <- wrap_elements(plot = rasterGrob(gg, interpolate = TRUE))
ggsave(filename = "CZS_meta_sampletype.svg", plot = CZS_meta_sampletype, width = 5, height = 8)
ggsave(filename = "CZS_meta_sampletype.pdf", plot = CZS_meta_sampletype, width = 5, height = 8)
ggsave("CZS_sampletype_funnel.pdf", plot = funnel_czssampletype, width = 8, height = 6)

metaanalysis_CZS_sampletype_noqa <- metaprop_wrap(dataframe = CZS_rate %>% filter(metaanalysis_inclusion == 'include' & !is.na(population_sample_type) & trimester_exposed %in% c('Unspecified','Whole pregnancy')), 
                                                  plot_pooled = TRUE, subgroup = 'population_sample_type', 
                                                  sort_by_subg = TRUE, xlabel = "CZS probability given Zika-infected mother",
                                                  plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                                  width = 5000, height =8000, resolution = 600)
CZS_meta_sampletype_noqa <- metaanalysis_CZS_sampletype_noqa$plot


metaanalysis_CZS_trimester <- metaprop_wrap(dataframe = CZSqa , plot_pooled = TRUE, subgroup = 'trimester_exposed', 
                                            sort_by_subg = FALSE, xlabel = "CZS probability given Zika-infected mother",
                                            plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                            width = 5000, height = 9000, resolution = 600)
CZS_meta_trimester <- metaanalysis_CZS_trimester$plot

ggsave(filename = "CZS_metaanalysis_trimester.svg", plot = CZS_meta_trimester, width = 5, height = 9)
ggsave(filename = "CZS_metaanalysis_trimester.pdf", plot = CZS_meta_trimester, width = 5, height = 9)



metaanalysis_CZS_noqa <- metaprop_wrap(dataframe = CZSqa %>% filter(trimester_exposed %in% c('Unspecified','Whole pregnancy')) , 
                                       plot_pooled = TRUE, subgroup = NA, 
                                       sort_by_subg = FALSE, xlabel = "CZS probability given Zika-infected mother",
                                       plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                       width = 5000, height = 6000, resolution = 600)
CZS_meta_noqa <- metaanalysis_CZS_noqa$plot

ggsave(filename = "CZS_metaanalysis_noqa.svg", plot = CZS_meta_noqa, width = 9, height = 21)
ggsave(filename = "CZS_metaanalysis_noqa.pdf", plot = CZS_meta_noqa, width = 9, height = 21)





# Miscarriage ----
miscarriage <- parameters %>%
  filter(parameter_type == 'Miscarriage probability') %>%
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

miscarriagemeta <- misc_qa %>% filter(metaanalysis_inclusion == 'include' & trimester_exposed %in% c('Unspecified','Whole pregnancy'))
length(unique(miscarriagemeta$covidence_id))
# misc1<-forest_plot(miscarriage %>% filter(qa_score >= 0.5) %>% arrange(desc(central)),
#                    label = "Miscarriage probability (%)", 
#                    shape_column = "population_sample_type", 
#                    color_column = "population_country", 
#                    lims = c(0,30),
#                    custom_colours = cols)
# misc1_noqa <-forest_plot(miscarriage  %>% arrange(desc(central)),
#                          label = "Miscarriage probability (%)", 
#                          shape_column = "population_sample_type", 
#                          color_column = "population_country", 
#                          lims = c(0,30),
#                          custom_colours = cols)
# ggsave(filename = 'miscarriage_refs_sample_country.png', misc1, height = 10, width = 6, bg = 'white')
# ggsave(filename = 'miscarriage_refs_sample_country_noqa.png', misc1_noqa, height = 10, width = 6, bg = 'white')
# ggsave(filename = 'miscarriage_refs_sample_country.pdf', misc1, height = 10, width = 6, bg = 'white')
# ggsave(filename = 'miscarriage_refs_sample_country_noqa.pdf', misc1_noqa, height = 10, width = 6, bg = 'white')

misc2 <-forest_plot(miscarriage%>% filter(qa_score >= 0.5),
                    ycol = 'label_group',
                    label = "Miscarriage probability (%)", 
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
                         label = "Miscarriage probability (%)", 
                         shape_column = 'population_sample_type',
                         # shape_column = "parameter_value_type", 
                         # color_column = "population_sample_type", 
                         color_column = "refs",
                         lims = c(0,30),
                         custom_colours = cols)
ggsave(filename = 'miscarriage_loc_sample_type.png', misc2, height = 10, width = 8, bg = 'white')
ggsave(filename = 'miscarriage_loc_sample_type_noqa.png', misc2_noqa, height = 10, width = 8, bg = 'white')
ggsave(filename = 'miscarriage_loc_sample_type.pdf', misc2, height = 10, width = 8, bg = 'white')
ggsave(filename = 'miscarriage_loc_sample_type_noqa.pdf', misc2_noqa, height = 10, width = 8, bg = 'white')

# Meta-analysis  miscarriage
metaanalysis_misc <- metaprop_wrap(dataframe = miscarriagemeta , 
                                   plot_pooled = TRUE, subgroup = NA, 
                                   sort_by_subg = FALSE, xlabel = "Miscarriage probability",
                                   plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                   width = 5000, height = 4000, resolution = 600)
misc_meta <- metaanalysis_misc$plot
png(file = "temp.png", width = 7000, height = 6000, res = 600)
funnel_misc <- funnel(metaanalysis_misc$result, common = FALSE,#only plots funnel for either common or random for some reason
                      pch = 22, bg = "dodgerblue3", level = 0.95)#, 
# studlab = FALSE, cex.studlab = 0.75, pos.studlab = 2)
dev.off()
gg <- png::readPNG("temp.png", native = TRUE)
file.remove("temp.png")
funnel_misc <- wrap_elements(plot = rasterGrob(gg, interpolate = TRUE))

ggsave(filename = "misc_metaanalysis.png", plot = misc_meta, width = 5, height = 4)
ggsave(filename = "misc_metaanalysis.pdf", plot = misc_meta, width = 5, height = 4)
ggsave("miscarriage_funnel.pdf", plot = funnel_misc, width = 8, height = 6)

metaanalysis_continent <- metaprop_wrap(dataframe = miscarriagemeta %>% filter(!is.na(continent)), 
                                        plot_pooled = TRUE, subgroup = 'continent', 
                                        sort_by_subg = FALSE, xlabel = "Miscarriage probability",
                                        plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                        width = 5000, height = 4500, resolution = 600)
misc_meta_continent <- metaanalysis_continent$plot

ggsave(filename = "misc_metaanalysis_continent.png", plot = misc_meta_continent, width = 5, height = 4.5)
ggsave(filename = "misc_metaanalysis_continent.pdf", plot = misc_meta_continent, width = 5, height = 4.5)

metaanalysis_sampletype <- metaprop_wrap(dataframe = miscarriagemeta %>% filter(!is.na(population_sample_type)), 
                                         plot_pooled = TRUE, subgroup = 'population_sample_type', 
                                         sort_by_subg = FALSE, xlabel = "Miscarriage probability",
                                         plot_study = TRUE, digits = 4, colour = "dodgerblue3",
                                         width = 5000, height = 4500, resolution = 600)
misc_meta_sampletype <- metaanalysis_sampletype$plot

ggsave("misc_metaanalysis_sampletype.png", plot = misc_meta_sampletype, width = 5, height = 5.5)
ggsave("misc_metaanalysis_sampletype.pdf", plot = misc_meta_sampletype, width = 5, height = 5.5)


# Create and save plot for main analysis 
layout = "
AABBBB
AABBBB"
CZS_plot_combined <- CZSplot + CZS_meta_sampletype +
  plot_layout(design = layout) #+ plot_annotation(tag_levels = 'A')
ggsave(filename = "CZS_plot_combined.png", plot = CZS_plot_combined, width = 35, height = 30, bg = 'white', dpi = 600)
ggsave(filename = "CZS_plot_combined.pdf", plot = CZS_plot_combined, width = 35, height = 30, bg = 'white', dpi = 600)

CZS_plot_combined_noqa <- CZSplot_noqa + CZS_meta_sampletype_noqa +
  plot_layout(design = layout) #+ plot_annotation(tag_levels = 'A')
ggsave(filename = "CZS_plot_combined_noqa.png", plot = CZS_plot_combined_noqa, width = 35, height = 30, bg = 'white', dpi = 600)
ggsave(filename = "CZS_plot_combined_noqa.pdf", plot = CZS_plot_combined_noqa, width = 35, height = 30, bg = 'white', dpi = 600)
