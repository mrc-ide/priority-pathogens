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
orderly_dependency("zika_compilation", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "outbreaks.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
# orderly_artefact(description = "zika-specific tables",
#                  files = c("figure_5_delays.png",
#                                           "figure_5_delays.pdf",
#                                           "figure_5SI_subgroup_meta.png",
#                                           "figure_5SI_delays.png",
#                                           "figure_5SI_delays.pdf",
#                                           "figure_5SI_onset_to_admission.png"))

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
# articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles %>% dplyr::select(covidence_id,qa_score)

models     <- dfs$models
parameters <- dfs$parameters %>% left_join(qa_scores) %>%
  mutate(article_label = make.unique(refs)) %>%
  mutate(article_label = factor(article_label,levels=rev(unique(article_label))))

# sample sd is the same as sd, so need to make this clear somehow / change param field to work for meta analysis

d1 <- parameters %>% filter(parameter_type == 'Human delay - incubation period' |
                              parameter_type == 'Human delay - incubation period (inverse parameter)') %>%
  mutate(parameter_type = 'Human delay - incubation period') %>%
  filter(parameter_value_type != 'Shape') # There are two entries with shape (in param 1) and scale (in param 2)
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
  filter(parameter_unit == 'Days')

# Symptom onset to recovery
onset_recovery_forest <- forest_plot(d4 %>% filter(qa_score>0.5)|> arrange(desc(parameter_value)),
                                     'Symptom onset to recovery (days)',"parameter_type",c(0,8),text_size = TEXT_SIZE)
ggsave("onset_recovery.png", plot =onset_recovery_forest, width = 8, height = 6)


# Serial interval
SI_forest <- forest_plot(d5 %>% filter(qa_score>0.5)|> arrange(desc(parameter_value)),'Serial Interval (days)',"parameter_value_type",c(0,35),text_size = TEXT_SIZE)
SI_forest_noqa <- forest_plot(d5|> arrange(desc(parameter_value)), 'Serial Interval (days)',"parameter_type",c(0,20),text_size = TEXT_SIZE)

# Infectious period 
IP_forest <- forest_plot(d6 %>% filter(qa_score>0.5)|> arrange(desc(parameter_value)),'Infectious Period (days)',"parameter_value_type",c(-22,52),text_size = TEXT_SIZE)
IP_forest_noqa <- forest_plot(d6|> arrange(desc(parameter_value)), 'Infectious Period (days)',"parameter_type",c(0,52),text_size = TEXT_SIZE)

ip_forest_mmv <- forest_plot(d6 |> arrange(method_moment_value,desc(parameter_value)),
                             'Infectious Period (days)','method_moment_value',c(0,52),text_size = TEXT_SIZE)

ip_forest_pc <- forest_plot(d6 |> arrange(population_country,desc(parameter_value)),
                            'Infectious Period (days)','population_country',c(0,52),text_size = TEXT_SIZE)

ip_forest_pg <- forest_plot(d6 |> arrange(population_group,desc(parameter_value)),
                            'Infectious Period (days)','population_group',c(0,52),text_size = TEXT_SIZE)

ip_forest_pst <- forest_plot(d6 |> arrange(population_sample_type,desc(parameter_value)),
                             'Infectious Period (days)','population_sample_type',c(0,52),text_size = TEXT_SIZE)

ip <- (ip_forest_mmv + ip_forest_pc) / (ip_forest_pg + ip_forest_pst ) + 
  theme(text = element_text(size = TEXT_SIZE)) + 
  plot_annotation(tag_levels = 'A') 
ggsave("infectious_period.png", plot =ip, width = 39, height = 22)

# most papers don't have a sample size which is why these don't work (should double check the ns)
set.seed(42)
d6 <- d6 %>% mutate(parameter_value = coalesce(parameter_value,central)) %>% arrange(desc(parameter_value)) %>%
  filter(!is.na(parameter_uncertainty_single_value)|!is.na(parameter_uncertainty_type) ) %>%
  filter(qa_score>0.5) %>%
  mutate(method_moment_value = replace_na(method_moment_value,'Unspecified'),
         population_group    = replace_na(population_group,'Unspecified'),
         population_sample_type = replace_na(population_sample_type,'Unspecified'),
         parameter_type = 'Human delay - infectious period')

meta6 <- metamean_wrap(dataframe = d6, estmeansd_method = "Cai",
                       plot_study = TRUE, digits = 2, lims = c(0,10), colour = "dodgerblue3",
                       label = "Mean infectious period (days)",
                       width = 9500, height = 4200, resolution = 1000)

# set.seed(42)
# d6_noqa <- d6 %>% mutate(parameter_value = coalesce(parameter_value,central)) %>% arrange(desc(parameter_value)) %>%
#   filter(!is.na(parameter_uncertainty_single_value)|!is.na(parameter_uncertainty_type) )
# meta6_noqa <- metamean_wrap(dataframe = d2_noqa, estmeansd_method = "Cai",
#                             plot_study = TRUE, digits = 2, lims = c(0,10), colour = "dodgerblue3", 
#                             label = "Mean infectious period (days)",
#                             width = 9500, height = 4200, resolution = 1000)


# Generation time 
GT_forest <- forest_plot(d7 %>% filter(qa_score>0.5)|> arrange(desc(parameter_value)),'Generation Time (days)',"parameter_value_type",c(0,20),text_size = TEXT_SIZE)
GT_forest_noqa <- forest_plot(d7|> arrange(desc(parameter_value)), 'Generation Time (days)',"parameter_type",c(0,20),text_size = TEXT_SIZE)
ggsave("generation_time.png", plot = GT_forest, width = 8, height = 6)

# Incubation period
p1 <- forest_plot(d1,'Incubation Period (days)',"parameter_type",c(0,30))

incp_forest_mmv <- forest_plot(d1 |> arrange(method_moment_value,desc(parameter_value)),
                             'Incubation Period (days)','method_moment_value',c(0,30),text_size = TEXT_SIZE)

incp_forest_pc <- forest_plot(d1 |> arrange(population_country,desc(parameter_value)),
                            'Incubation Period (days)','population_country',c(0,30),text_size = TEXT_SIZE)

incp_forest_pg <- forest_plot(d1 |> arrange(population_group,desc(parameter_value)),
                            'Incubation Period (days)','population_group',c(0,30),text_size = TEXT_SIZE)

incp_forest_pst <- forest_plot(d1 |> arrange(population_sample_type,desc(parameter_value)),
                             'Incubation Period (days)','population_sample_type',c(0,30),text_size = TEXT_SIZE)

incp <- (incp_forest_mmv + incp_forest_pc) / (incp_forest_pg + incp_forest_pst ) + theme(text = element_text(size = TEXT_SIZE)) +
  plot_annotation(tag_levels = 'A')
ggsave("incubation_period.png", plot = incp, width = 39, height = 22)

# d1_subgroups <- d1 %>% filter(population_group %in% c('General population','Mixed groups', 'Persons under investigation','Healthcare workers')) %>%
#   filter(qa_score>0.5) %>%
#   mutate(parameter_value = coalesce(parameter_value,central))
# 
# d1_subgroups_noqa <- d1 %>% filter(population_group %in% c('General population','Mixed groups', 'Persons under investigation','Healthcare workers')) %>%
#   mutate(parameter_value = coalesce(parameter_value,central))

# set.seed(42)
# m1 <- metamean_wrap(dataframe = d1_subgroups, estmeansd_method = "Cai",
#                     plot_study = FALSE, digits = 2, lims = c(0,10), colour = "dodgerblue3", label = "Mean incubation period (days)",
#                     width = 7500, height = 5950, resolution = 1000, subgroup = 'population_group', sort_by_subg = TRUE, colgap_shift = 2 )
# 
# set.seed(42)
# m1_SI <- metamean_wrap(dataframe = d1_subgroups, estmeansd_method = "Cai",
#                        plot_study = TRUE, digits = 2, lims = c(0,10), colour = "dodgerblue3", label = "Mean incubation period (days)",
#                        width = 11500, height = 9750, resolution = 1000, subgroup = 'population_group', sort_by_subg = TRUE, colgap_shift = 2 )
# 
# set.seed(42)
# m1_SI_noqa <- metamean_wrap(dataframe = d1_subgroups_noqa, estmeansd_method = "Cai",
#                             plot_study = FALSE, digits = 2, lims = c(0,10), colour = "dodgerblue3", label = "Mean incubation period (days)",
#                             width = 9500, height = 9750, resolution = 1000, subgroup = 'population_group', sort_by_subg = TRUE, colgap_shift = 2 )


#meta-analysis of latent period
p2 <- forest_plot(d2 %>% mutate(parameter_unit = 'Days'),'Latent Period (days)',"parameter_type",c(0,51))
ggsave("latent_period.png", plot = p2, width = 8, height = 6)



# admission to outcome... 
outcome_forest <- forest_plot(d3 %>% filter(qa_score>0.5) |> arrange(parameter_type,desc(parameter_value)) %>%
                                mutate(parameter_type = stringr::str_to_title(str_replace(parameter_type,'Human delay - ',''))),
                              'Admission to outcome (days)',"parameter_type",c(0,40),text_size = 22)
outcome_forest_noqa <- forest_plot(d3 |> arrange(parameter_type,desc(parameter_value))  %>% 
                                     mutate(parameter_type = stringr::str_to_title(str_replace(parameter_type,'Human delay - ',''))),
                                   'Admission to outcome (days)',"parameter_type",c(0,),text_size = 22)
ggsave("admission_to_outcome.png", plot = outcome_forest, width = 12, height = 6)


# Mosquito delay - EIP
eip_forest <- forest_plot(m1 |> arrange(population_sample_type,desc(parameter_value)),
                             'Extrinsic incubation period (days)','parameter_type',c(0,25),text_size = TEXT_SIZE)
eip_forest_pc <- forest_plot(m1 |> arrange(population_country,desc(parameter_value)),
                              'Extrinsic incubation period (days)','population_country',c(0,25),text_size = TEXT_SIZE,
                             custom_colours = c( "#00468B","#ED0000", "#42B540","#0099B4", "#925E9F", "#FDAF91","#DF8F44",
                                                "#6A6599","#CD534C","#A20056","#1B1919"))
eip_forest_ploc <- forest_plot(m1 |> arrange(population_country,desc(parameter_value)),
                             'Extrinsic incubation period (days)','population_location',c(0,25),text_size = TEXT_SIZE,
                             custom_colours = c( "#00468B","#ED0000", "#42B540","#0099B4", "#925E9F", "#FDAF91","#DF8F44",
                                                "#6A6599","#CD534C","#A20056","#1B1919"))
eip <- (eip_forest + eip_forest_pc) / (eip_forest_ploc) + theme(text = element_text(size = TEXT_SIZE)) +
  plot_annotation(tag_levels = 'A')
ggsave("extrinsic_incubation_period.png", plot = incp, width = 39, height = 22)

### Create plots!
# 
# layout <- "
# AAABBB
# CCCDDD
# EEEFFF
# "
# delays_plot <-  SI_forest + ip + GT_forest + incp + outcome_forest + p2 +
#   plot_layout(design = layout) + plot_annotation(tag_levels = 'A') 
# ggsave("delays.png", plot = delays_plot, width = 39, height = 22)
# ggsave("delays.pdf", plot = delays_plot, width = 39, height = 22)
# 
# delays_plotSI <-  SI_forest_noqa + IP_forest_noqa + outcome_forest_noqa + m1_SI_noqa$plot + theme(text = element_text(size = TEXT_SIZE)) + m2_noqa$plot + theme(text = element_text(size = TEXT_SIZE)) + 
#   plot_layout(design = layout) + plot_annotation(tag_levels = 'A') 
# ggsave("SI_delays.png", plot = delays_plotSI, width = 39, height = 22)
# ggsave("SI_delays.pdf", plot = delays_plotSI, width = 39, height = 22)

# SI figures
# ggsave("figure_5SI_subgroup_meta.png", plot = m1_SI$plot, width = 22, height = 22) # note that qa & noqa lets through sames papers


