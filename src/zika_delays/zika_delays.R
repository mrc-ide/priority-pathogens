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
# orderly_artefact(description = "zika-specific tables",
#                  files = c("onset_recovery.png",
#                            "infectious_period.png",
#                            "serial_interval.png",
#                            "incubation_period.png",
#                            "latent_period.png",
#                            "admission_to_outcome.png", 
#                            "extrinsic_incubation_period.png"))

###################
## DATA CURATION ##
###################
TEXT_SIZE <- 36

articles   <- readRDS("articles_curated.rds")
models     <- readRDS("models_curated.rds")
parameters <- readRDS("parameters_curated.rds") 

#dfs <- curation(articles,tibble(),models,parameters, adjust_for_exponents = FALSE )
# dfs <- data_curation(articles,tibble(),models,parameters, plotting = TRUE )
# 
# articles   <- dfs$articles
# # articles   <- epireview::assign_qa_score(articles = articles)$articles
# qa_scores  <- articles %>% dplyr::select(covidence_id,qa_score)
# 
# models     <- dfs$models
# parameters <- dfs$parameters %>% left_join(qa_scores) %>%
#   mutate(article_label = make.unique(refs)) %>%
#   mutate(article_label = factor(article_label,levels=rev(unique(article_label))))

# sample sd is the same as sd, so need to make this clear somehow / change param field to work for meta analysis

#Find total numbers of delays 
delays <- parameters %>% 
  filter(parameter_class == 'Human delay' & qa_score >= 0.5)
mosq <- parameters %>%
  filter(grepl('Mosquito delay',parameter_type) & qa_score >= 0.5)
table(delays$parameter_type)
length(unique(delays$covidence_id))
length(unique(mosq$covidence_id))

delaysum <- delays %>%
  group_by(parameter_type) %>%
  mutate(n = n()) %>%
  filter(central == max(central, na.rm = TRUE) | central == min(central, na.rm = TRUE)) %>%
  filter(n >1) %>%
  select(central, parameter_type:case_definition, method_r, method_moment_value, method_disaggregated, population_age_max:population_sex, 
         survey_start_date, survey_end_date, article_label) 


d1 <- parameters %>% filter(parameter_type == 'Human delay - incubation period' |
                              parameter_type == 'Human delay - incubation period (inverse parameter)') %>%
  mutate(parameter_type = 'Human delay - incubation period')# %>%
  # filter(parameter_value_type != 'Shape') # There are two entries with shape (in param 1) and scale (in param 2)
# Deal with the parameter 2s (shape ans cale for Weibull distributions)
mean1 = round(mean(rweibull(n = 10000, shape = 2.39, scale = 7.19)),2)
mean2 <- round(mean(rweibull(n = 10000, shape = 2.69, scale = 6.70)),2)
d1 <- d1 %>%
  # Update all the other variables for 2362
   mutate(estmean = ifelse(covidence_id == 2362& parameter_value == 2.39, mean1,  
                           ifelse(covidence_id == 2362& parameter_value == 2.69, mean2, NA)),
          parameter_value = ifelse(parameter_value_type == 'Shape', estmean, parameter_value),
          parameter_value_type = ifelse(parameter_value_type == 'Shape','Mean',parameter_value_type),
          central = ifelse(parameter_value_type == 'Shape', estmean, central),
          parameter_uncertainty_lower_value = ifelse(covidence_id == 2362, NA, parameter_uncertainty_lower_value),
          parameter_uncertainty_upper_value = ifelse(covidence_id == 2362, NA, parameter_uncertainty_upper_value),
          parameter_2_uncertainty_lower_value = ifelse(covidence_id == 2362, NA, parameter_2_uncertainty_lower_value),
          parameter_2_uncertainty_upper_value = ifelse(covidence_id == 2362, NA, parameter_2_uncertainty_upper_value),
          parameter_unit = ifelse(covidence_id == 2362, "Days", parameter_unit))

d2 <- parameters %>% filter(parameter_type == 'Human delay - latent period (inverse parameter)')
d3 <- parameters %>% filter(parameter_type == 'Human delay - time in care (length of stay)' |
                              parameter_type == 'Human delay - admission to care to discharge/recovery' |
                              parameter_type == 'Human delay - Admission to Care/Hospitalisation to Discharge from Care/Hospital') %>%
  mutate(parameter_value_type = case_when(is.na(parameter_value_type)~'Unspecified',
                                          TRUE ~ parameter_value_type))
d4 <- parameters %>% filter(parameter_type == 'Human delay - Symptom Onset/Fever to Recovery/non-Infectiousness')
d5 <- parameters %>% filter(parameter_type == 'Human delay - serial interval')
d6 <- parameters %>% filter(parameter_type == 'Human delay - infectious period' | parameter_type == "Human delay - infectious period (inverse parameter)")
d7 <- parameters %>% filter(parameter_type == 'Human delay - generation time')
d8 <- parameters %>% filter(parameter_type == "Human delay - symptom onset to admission to care")

m1 <- parameters %>% filter(grepl("Mosquito delay", parameter_type)) %>%
  filter(parameter_type != "Mosquito delay - extrinsic incubation period (EIP10)") %>%
  mutate(parameter_type = "Mosquito delay - extrinsic incubation period") %>%
  filter(parameter_unit == 'Days') %>%
  filter(!(covidence_id == 6106 & parameter_value == 14)) # this is for albopicture but isn't really EIP 
# 5892 have gamma distribution information as well as the central estimate 

# Sample sizes for all datasets
# delays <- rbind(d1, d2,d3,d4,d5,d6,d7,d8,m1)
# dnon <- delays %>% filter(is.na(population_sample_size))# 3738 -- no sample size 
# table(dnon$name_data_entry, dnon$covidence_id)
# 390 - unclear sample size, 4444 cases in FL but with dates that fall outside of stated data used for fitting
# 430, 588, 946, 947, 1373 - no sample size reported 
# didn't look at rest

# Symptom onset to recovery
onset_recovery_forest <- forest_plot(d4 %>% filter(qa_score>0.5)|> arrange(desc(parameter_value)),
                                     ycol = 'label_group',
                                     label = 'Symptom onset to recovery (days)',
                                     color_column="population_sample_type",
                                     shape_column = "parameter_value_type",
                                     lims = c(0,8),
                                     text_size = TEXT_SIZE, 
                                     point_size = 4)
ggsave("onset_recovery.png", plot =onset_recovery_forest, width = 10, height = 6, bg = 'white')


# Serial interval
SI_forest <- forest_plot(d5 %>% filter(qa_score>0.5)|> arrange(desc(parameter_value)),
                         ycol = 'label_group',
                         label = 'Serial Interval (days)',
                         color_column = 'population_sample_type',
                         shape_column = "parameter_value_type",
                         lims = c(0,35),
                         text_size = TEXT_SIZE, 
                         point_size = 4)
ggsave('serial_interval.png', plot =SI_forest, width = 10, height = 6, bg = 'white')

# Infectious period 
IP_forest <- forest_plot(d6 %>% filter(qa_score>0.5)|> arrange(desc(parameter_value)),
                         ycol = 'label_group',
                         label = 'Infectious Period (days)',
                         color_column = 'population_sample_type',
                         shape_column = "population_group",
                         lims =c(0,50),
                         text_size = TEXT_SIZE, 
                         point_size = 4)
IP_forest_noqa <- forest_plot(d6 |> arrange(desc(parameter_value)),
                              ycol = 'label_group',
                              label = 'Infectious Period (days)',
                              color_column = 'population_sample_type',
                              shape_column = "population_group",
                              lims =c(0,50),
                              text_size = TEXT_SIZE, 
                              point_size = 4)

# ip <- (ip_forest_mmv + ip_forest_pc) / (ip_forest_pg + ip_forest_pst ) + 
#   theme(text = element_text(size = TEXT_SIZE)) + 
#   plot_annotation(tag_levels = 'A') 
ggsave("infectious_period.png", plot =IP_forest, width = 14, height = 12, bg = 'white')
ggsave("infectious_period_noqa.png", plot =IP_forest_noqa, width = 14, height = 12, bg = 'white')

# most papers don't have a sample size which is why these don't work (should double check the ns)
# set.seed(42)
# d6 <- d6 %>% mutate(parameter_value = coalesce(parameter_value,central)) %>% arrange(desc(parameter_value)) %>%
#   filter(!is.na(parameter_uncertainty_single_value)|!is.na(parameter_uncertainty_type) ) %>%
#   filter(qa_score>0.5) %>%
#   mutate(method_moment_value = replace_na(method_moment_value,'Unspecified'),
#          population_group    = replace_na(population_group,'Unspecified'),
#          population_sample_type = replace_na(population_sample_type,'Unspecified'),
#          parameter_type = 'Human delay - infectious period')

# meta6 <- metamean_wrap(dataframe = d6, estmeansd_method = "Cai",
#                        plot_study = TRUE, digits = 2, lims = c(0,10), colour = "dodgerblue3",
#                        label = "Mean infectious period (days)",
#                        width = 9500, height = 4200, resolution = 1000)

# set.seed(42)
# d6_noqa <- d6 %>% mutate(parameter_value = coalesce(parameter_value,central)) %>% arrange(desc(parameter_value)) %>%
#   filter(!is.na(parameter_uncertainty_single_value)|!is.na(parameter_uncertainty_type) )
# meta6_noqa <- metamean_wrap(dataframe = d2_noqa, estmeansd_method = "Cai",
#                             plot_study = TRUE, digits = 2, lims = c(0,10), colour = "dodgerblue3", 
#                             label = "Mean infectious period (days)",
#                             width = 9500, height = 4200, resolution = 1000)


# Generation time 
# GT_forest <- forest_plot(d7 %>% filter(qa_score>0.5)|> arrange(desc(parameter_value)),
#                          label = 'Generation Time (days)',
#                          color_column = "population_sample_type",
#                          shape_column = "parameter_value_type",
#                          lims=c(0,20),
#                          text_size = TEXT_SIZE)
# ggsave("generation_time.png", plot = GT_forest, width = 8, height = 6, bg = 'white')

# Incubation period
incp1 <- forest_plot(d1 %>% filter(qa_score>0.5) |> arrange(parameter_value,desc(parameter_value)),
                  label = 'Incubation Period (days)',
                  ycol = 'label_group',
                  color_column = "population_sample_type",
                  shape_column = "parameter_value_type", 
                  lims = c(0,30),
                  text_size = TEXT_SIZE, 
                  point_size = 4)

incp2 <- forest_plot(d1 %>% filter(qa_score>0.5) |> arrange(parameter_value,desc(parameter_value)),
                    label = 'Incubation Period (days)',
                    ycol = 'label_group',
                    color_column = "population_sample_type",
                    shape_column = "population_group", 
                    lims = c(0,30),
                    text_size = TEXT_SIZE, 
                    point_size = 4)

incp1_noqa <- forest_plot(d1  |> arrange(parameter_value,desc(parameter_value)),
                     label = 'Incubation Period (days)',
                     ycol = 'label_group',
                     color_column = "population_sample_type",
                     shape_column = "parameter_value_type", 
                     lims = c(0,30),
                     text_size = TEXT_SIZE, 
                     point_size = 4)

incp2_noqa <- forest_plot(d1 |> arrange(parameter_value,desc(parameter_value)),
                     label = 'Incubation Period (days)',
                     ycol = 'label_group',
                     color_column = "population_sample_type",
                     shape_column = "population_group", 
                     lims = c(0,30),
                     text_size = TEXT_SIZE, 
                     point_size = 4)


ggsave("incubation_period1.png", plot = incp1, width = 14, height = 12, bg = 'white')
ggsave("incubation_period2.png", plot = incp2, width = 14, height = 12, bg = 'white')
ggsave("incubation_period1_noqa.png", plot = incp1_noqa, width = 14, height = 12, bg = 'white')
ggsave("incubation_period2_noqa.png", plot = incp2_noqa, width = 14, height = 12, bg = 'white')

# Plot with latent, symptom onst to recovery, serial 
lat_onset_serial_admission <- rbind(d2, d5, d4, d8) %>% mutate(parameter_unit = 'Days') %>%
  mutate(parameter_type = case_when(
    parameter_type == "Human delay - latent period (inverse parameter)" ~ "Latent period",
    parameter_type == "Human delay - serial interval" ~ 'Serial interval',
    parameter_type == 'Human delay - Symptom Onset/Fever to Recovery/non-Infectiousness' ~ "Symptom onset to recovery",
    parameter_type == "Human delay - symptom onset to admission to care" ~ "Symptom onset to admission",
    TRUE ~ parameter_type
  ))
lat_onset_serialplt <- forest_plot(lat_onset_serial_admission %>% filter(qa_score > 0.5) |> arrange(parameter_value,desc(parameter_value)),
            label = 'Delay in days',
            ycol = 'label_group',
            shape_column = "parameter_type",
            color_column = "population_sample_type", 
            lims = c(0,33),
            text_size = TEXT_SIZE, 
            point_size = 4)
lat_onset_serialplt_noqa <- forest_plot(lat_onset_serial_admission |> arrange(parameter_value,desc(parameter_value)),
                                   label = 'Delay in days',
                                   ycol = 'label_group',
                                   shape_column = "parameter_type",
                                   color_column = "population_sample_type", 
                                   lims = c(0,50),
                                   text_size = TEXT_SIZE, 
                                   point_size = 4)
ggsave("latent_serial_onsetrecov.png", plot = lat_onset_serialplt, width= 14, height = 10, bg = 'white')
ggsave("latent_serial_onsetrecov_noqa.png", plot = lat_onset_serialplt_noqa, width= 14, height = 10, bg = 'white')

# latent period
p2 <- forest_plot(d2 %>% mutate(parameter_unit = 'Days'),
                  label = 'Latent Period (days)',
                  color_column = "population_country",
                  shape_column = 'parameter_value_type',
                  lims = c(0,51),
                  text_size = TEXT_SIZE, 
                  point_size = 4)
ggsave("latent_period.png", plot = p2, width = 6, height = 4, bg = 'white')



# admission to outcome... 
outcome_forest <- forest_plot(d3 %>% filter(qa_score>0.5) |> arrange(parameter_type,desc(parameter_value)) %>%
                                mutate(parameter_type = stringr::str_to_title(str_replace(parameter_type,'Human delay - ',''))),
                              ycol = 'label_group',
                              label = 'Admission to discharge/recovery (days)',
                              color_column = 'population_sample_type',
                              shape_column = "population_group",
                              lims = c(0,215),
                              text_size = TEXT_SIZE, 
                              point_size = 4) 
outcome_forest_noqa <- forest_plot(d3  |> arrange(parameter_type,desc(parameter_value)) %>%
                                     mutate(parameter_type = stringr::str_to_title(str_replace(parameter_type,'Human delay - ',''))),
                                   ycol = 'label_group',
                                   label = 'Admission to discharge/recovery (days)',
                                   color_column = 'population_sample_type',
                                   shape_column = "population_group",
                                   lims = c(0,215),
                                   text_size = TEXT_SIZE, 
                                   point_size = 4)
ggsave("admission_to_outcome.png", plot = outcome_forest, width = 12, height = 6, bg = 'white')
ggsave("admission_to_outcome_noqa.png", plot = outcome_forest_noqa, width = 12, height = 6, bg = 'white')
outcome_forest_noqa_type <- forest_plot(d3  |> arrange(parameter_type,desc(parameter_value)) %>%
                                     mutate(parameter_type = stringr::str_to_title(str_replace(parameter_type,'Human delay - ',''))),
                                   ycol = 'label_group',
                                   label = 'Admission to discharge/recovery (days)',
                                   color_column = 'parameter_type',
                                   shape_column = "parameter_value_type",
                                   lims = c(0,215),
                                   text_size = TEXT_SIZE, 
                                   point_size = 4)
ggsave("admission_to_outcome_partype.png", plot = outcome_forest_noqa_type, width = 12, height = 6, bg = 'white')

# Mosquito delay - EIP
eip_forest <- forest_plot(m1 %>% filter(qa_score>0.5)|> arrange(population_sample_type,desc(parameter_value)),
                             label = 'Extrinsic incubation period (days)',
                          ycol = 'label_group',
                          # shape_column = 'popultio',
                          color_column = 'refs',
                          lims = c(0,25),
                          text_size = TEXT_SIZE,
                          custom_colours = c( "#00468B","#ED0000", "#42B540","#0099B4", "#925E9F", "#FDAF91","#DF8F44",
                                              "#6A6599","#CD534C","#A20056","#1B1919"), 
                          point_size = 4)
eip_forest_noqa <- forest_plot(m1 |> arrange(population_sample_type,desc(parameter_value)),
                          label = 'Extrinsic incubation period (days)',
                          ycol = 'label_group',
                          # shape_column = 'popultio',
                          color_column = 'refs',
                          lims = c(0,25),
                          text_size = TEXT_SIZE,
                          custom_colours = c( "#00468B","#ED0000", "#42B540","#0099B4", "#925E9F", "#FDAF91","#DF8F44",
                                              "#6A6599","#CD534C","#A20056","#1B1919"), 
                          point_size = 4)

ggsave("extrinsic_incubation_period.png", plot = eip_forest, width = 16, height = 10, bg = 'white')

## Create plots!
layout <- "
AABB
CCDD
EEEE"
delays_plot <-  IP_forest  + incp2 +  outcome_forest+lat_onset_serialplt + eip_forest+
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')
ggsave("delays.png", plot = delays_plot, width = 35, height = 30, bg = 'white')
ggsave("delays.pdf", plot = delays_plot, width = 35, height = 30, bg = 'white')

delays_plotSI <-  IP_forest_noqa + incp2_noqa +  outcome_forest_noqa + lat_onset_serialplt_noqa +eip_forest_noqa + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')
ggsave("SI_delays.png", plot = delays_plotSI, width = 35, height = 30)
ggsave("SI_delays.pdf", plot = delays_plotSI, width = 35, height = 30)

# SI figures
# ggsave("figure_5SI_subgroup_meta.png", plot = m1_SI$plot, width = 22, height = 22) # note that qa & noqa lets through sames papers

# Meta- analysis 

