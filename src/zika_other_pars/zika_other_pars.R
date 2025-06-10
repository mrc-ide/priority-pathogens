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


orderly_shared_resource("zika_functions.R" = "zika_functions.R")
source("zika_functions.R")

TEXT_SIZE <- 28

params_clean <- readRDS("parameters_curated.rds") 

# params_clean <- filter(params_clean, !qa_score < 0.5)

# Filter data by parameter types
params_clean.d1 <- filter(params_clean, parameter_type == "Attack rate")
length(unique(params_clean.d1$covidence_id))
nrow(params_clean.d1)

params_clean.d2 <- filter(params_clean, parameter_type %in% c("Relative contribution - sexual", 
                                                              "Relative contribution - vector-borne"))
length(unique(params_clean.d2$covidence_id))
nrow(params_clean.d2)

params_clean.d3 <- filter(params_clean, parameter_type == "Growth rate (r)")
length(unique(params_clean.d3$covidence_id))
nrow(params_clean.d3)

params_clean.d4 <- filter(params_clean, parameter_type == "Overdispersion")

params_clean.d5 <- filter(params_clean, parameter_type == "Severity - case fatality rate (CFR)")
length(unique(params_clean.d5$covidence_id))
nrow(params_clean.d5)

params_clean.d6 <- filter(params_clean, parameter_type == "Severity - proportion of symptomatic cases")
length(unique(params_clean.d6$covidence_id))
nrow(params_clean.d6)


# Cleaning and plotting for Relative Contribution
df_rel_contrib <- params_clean.d2 #clean_data(params_clean.d2, adjust_values = TRUE)
df_rel_contrib <- df_rel_contrib %>%
  mutate(parameter_unit = ifelse(covidence_id == 422, 
                                 "Percentage (%)", parameter_unit))
# parameter_uncertainty_upper_value = ifelse(covidence_id == 422 & parameter_type %in% c("Relative contribution - human to human",
#                                                                                        "Relative contribution - zoonotic to human"), parameter_uncertainty_upper_value * 100 , parameter_uncertainty_upper_value),
# parameter_uncertainty_lower_value = ifelse(covidence_id == 422 & parameter_type %in% c("Relative contribution - human to human",
# "Relative contribution - zoonotic to human"), parameter_uncertainty_lower_value * 100 , parameter_uncertainty_lower_value))
# p1 <- save_plot(df_rel_contrib, "Relative contribution", height = 100, width = 200)
p1_qa<- forest_plot(df_rel_contrib %>% filter(!qa_score<0.5)|> arrange(desc(parameter_value)),
                                     ycol = 'label_group',
                                     label = 'Relative contribution to transmission',
                                     color_column="population_sample_type",
                                     shape_column = "parameter_value_type",
                                     lims = c(0,100),
                                     text_size = TEXT_SIZE, 
                                     point_size = 4) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.7, .7))
ggsave("Relative_contribution_QA.png", plot =p1_qa, width = 10, height = 6, bg = 'white')

# p1_qa

p1 <- forest_plot(df_rel_contrib |> arrange(desc(parameter_value)),
                    ycol = 'label_group',
                    label = 'Relative contribution to transmission',
                    color_column="population_sample_type",
                    shape_column = "parameter_value_type",
                    lims = c(0,100),
                    text_size = TEXT_SIZE, 
                    point_size = 4) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.7, .7))
ggsave("Relative_contribution.png", plot =p1, width = 10, height = 6, bg = 'white')

# p1

# length(unique(df_rel_contrib$covidence_id))
# View(df_rel_contrib[, c("covidence_id", "central", "parameter_value", "parameter_uncertainty_type","parameter_uncertainty_lower_value", "parameter_uncertainty_upper_value",
#                         "parameter_lower_bound", "parameter_upper_bound", "population_sample_size", "population_country", "population_location", 
#                         "population_group",
#                         "population_sample_type", "population_study_start_year", 
#                         "population_study_end_year")])


# Cleaning and plotting for Attack Rate (with exponent adjustments)
df_attack_rate <- params_clean.d1 #clean_data(params_clean.d1, adjust_exponent = TRUE)
df_attack_rate <- df_attack_rate %>% 
  filter(!covidence_id == 6272) %>%  # Remove rows where no values were extracted
  mutate(parameter_value_type = ifelse(covidence_id %in% c(890, 1154, 1308, 1821, 3221, 3337, 5671, 6670, 7047, 7336, 10749, 11866) & is.na(parameter_value_type), 
                                       "Unspecified", parameter_value_type),
         parameter_unit = ifelse(covidence_id %in% c(1941, 6321, 6670) & parameter_unit == "No units", 
                                 "Unspecified", parameter_unit),
         parameter_unit = ifelse(covidence_id %in% c(890, 890, 1154, 1308, 1821, 3337,
                                                     3221,  5671, 6670,  7047,
                                                     7336, 10749, 11866) & is.na(parameter_unit), 
                                 "Unspecified", parameter_unit),
         population_location = ifelse(population_country == "Netherlands", "travellers from ", population_location),
         population_location = ifelse(population_location == "Arauca, Armenia, Barranquilla, Bucaramanga, Cali, Cartagena, Cúcuta, Florencia, Ibagué, Inírida, Leticia, Medellín, Mitú, Mocoa, Montería, Neiva, Pereira, Popayán, Puerto Carreño, Quibdó, Riohacha, San Andrés, San José del Guaviare, Santa Marta, Sincelejo, Valledupar, Villavicencio, Yopal", "Multi-location (n = 28)", population_location),
         population_location = ifelse(population_location == "Wynwood Neighborhood, Miami-Dade County, Florida", "Miami-Dade County", population_location),
         population_location = ifelse(population_location == "\"Other four archipelagos\"", "archipelagos", population_location),
         population_country = ifelse(population_country == "Multi-country: Central and South America (n = 8)", "Central and South America \n (n = 8)", population_country))

df_attack_rate <- df_attack_rate %>% 
  mutate(
    #transform all unit to %
    parameter_unit = ifelse(covidence_id %in% c(1679, 4268, 4032, 3221, 5671, 11866, 5949, 7336, 6542,6670,6321, 1941,3337 ), "Percentage (%)", parameter_unit),
    central = ifelse(covidence_id %in% c(6321, 7336,6670), (parameter_value * 100 ), central), #because calculated as cases / population or as probability
    central = ifelse(covidence_id == 3337, (parameter_value * 100 / 1000), central), #reported an exponential but not true, unit of measure per 1,000  
    central = ifelse(covidence_id == 1941, parameter_value, central), #reported an exponential but not true, unit of measure percentage
    central = ifelse(covidence_id %in% c(4032, 5949), (central * 100 / 10000), central),
    central = ifelse(covidence_id %in% c(1679, 4268), (central * 100 / 100000), central),
    parameter_upper_bound = ifelse(covidence_id %in% c(1679), (parameter_upper_bound * 100 / 100000), parameter_upper_bound),
    parameter_lower_bound = ifelse(covidence_id %in% c(1679), (parameter_upper_bound * 100 / 100000), parameter_lower_bound),
    
    parameter_upper_bound = ifelse(covidence_id %in% c(6321, 7336,6670), (parameter_upper_bound * 100 ), parameter_upper_bound), #because calculated as cases / population or as probability
    parameter_upper_bound = ifelse(covidence_id == 3337, (parameter_upper_bound * 100 / 1000), parameter_upper_bound), #reported an exponential but not true, unit of measure per 1,000  
    parameter_upper_bound = ifelse(covidence_id %in% c(4032, 5949), (parameter_upper_bound * 100 / 10000), parameter_upper_bound),
    parameter_upper_bound = ifelse(covidence_id %in% c(1679, 4268), (parameter_upper_bound * 100 / 100000), parameter_upper_bound),
    parameter_lower_bound = ifelse(covidence_id %in% c(6321, 7336,6670), (parameter_lower_bound * 100 ), parameter_lower_bound), #because calculated as cases / population or as probability
    parameter_lower_bound = ifelse(covidence_id == 3337, (parameter_lower_bound * 100 / 1000), parameter_lower_bound), #reported an exponential but not true, unit of measure per 1,000 
    parameter_lower_bound = ifelse(covidence_id %in% c(4032, 5949), (parameter_lower_bound * 100 / 10000), parameter_lower_bound),
    parameter_lower_bound = ifelse(covidence_id %in% c(1679, 4268), (parameter_lower_bound * 100 / 100000), parameter_lower_bound),
    
    parameter_uncertainty_upper_value = ifelse(covidence_id %in% c(6321, 7336,6670), (parameter_uncertainty_upper_value * 100 ), parameter_uncertainty_upper_value), #because calculated as cases / population or as probability
    parameter_uncertainty_upper_value = ifelse(covidence_id == 3337, (parameter_uncertainty_upper_value * 100 / 1000), parameter_uncertainty_upper_value), #reported an exponential but not true, unit of measure per 1,000  
    parameter_uncertainty_upper_value = ifelse(covidence_id %in% c(4032, 5949), (parameter_uncertainty_upper_value * 100 / 10000), parameter_uncertainty_upper_value),
    parameter_uncertainty_upper_value = ifelse(covidence_id %in% c(1679, 4268), (parameter_uncertainty_upper_value * 100 / 100000), parameter_uncertainty_upper_value),
    parameter_uncertainty_lower_value = ifelse(covidence_id %in% c(6321, 7336,6670), (parameter_uncertainty_lower_value * 100 ), parameter_uncertainty_lower_value), #because calculated as cases / population or as probability
    parameter_uncertainty_lower_value = ifelse(covidence_id == 3337, (parameter_uncertainty_lower_value * 100 / 1000), parameter_uncertainty_lower_value), #reported an exponential but not true, unit of measure per 1,000  
    parameter_uncertainty_lower_value = ifelse(covidence_id %in% c(4032, 5949), (parameter_uncertainty_lower_value * 100 / 10000), parameter_uncertainty_lower_value),
    parameter_uncertainty_lower_value = ifelse(covidence_id %in% c(1679, 4268), (parameter_uncertainty_lower_value * 100 / 100000), parameter_uncertainty_lower_value))   
# %>%
# filter( !parameter_lower_bound > 100) 
# %>%


# p2 <- save_plot(df_attack_rate, "Attack rate", height = 200, width = 100)
# p2
p1_qa<- forest_plot(df_attack_rate %>% filter(!qa_score<0.5)|> arrange(desc(parameter_value)),
                    ycol = 'label_group',
                    label = 'Attack rate',
                    color_column="population_sample_type",
                    shape_column = "parameter_value_type",
                    lims = c(0,100),
                    text_size = TEXT_SIZE, 
                    point_size = 4) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.7, .7))
ggsave("Attack_rate_QA.png", plot =p1_qa, width = 15, height = 25, bg = 'white')

# p1_qa

p1 <- forest_plot(df_attack_rate |> arrange(desc(parameter_value)),
                  ycol = 'label_group',
                  label = 'Attack rate',
                  color_column="population_sample_type",
                  shape_column = "parameter_value_type",
                  lims = c(0,100),
                  text_size = TEXT_SIZE, 
                  point_size = 4) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.7, .7))
ggsave("Attack_rate.png", plot =p1, width = 15, height = 30, bg = 'white')

# p1


# length(unique(df_attack_rate$covidence_id))
# View(df_attack_rate[, c("covidence_id", "central", "parameter_value","parameter_uncertainty_type", "parameter_uncertainty_lower_value", "parameter_uncertainty_upper_value",
#                         "parameter_lower_bound", "parameter_upper_bound", "population_sample_size", "population_country", "population_location",
#                         "population_group",
#                         "population_sample_type", "population_study_start_year",
#                         "population_study_end_year")])

# Cleaning and plotting for Growth Rate
# df_growth_rate <- clean_data(params_clean.d3)
df_growth_rate <- params_clean.d3
df_growth_rate <- df_growth_rate %>%
  mutate(parameter_unit = ifelse(covidence_id == 2876, 
                                 "Per day", parameter_unit),
         parameter_unit = ifelse(covidence_id == 422, 
                                 "Per day", parameter_unit),
         parameter_unit = ifelse(covidence_id == 268, #change unit to make per day
                                 "Per day", parameter_unit),
         central = ifelse(covidence_id == 268, 
                          (central/7), central),
         parameter_uncertainty_upper_value = ifelse(covidence_id == 268, 
                                                    (parameter_uncertainty_upper_value/7), parameter_uncertainty_upper_value),
         parameter_uncertainty_lower_value = ifelse(covidence_id == 268, 
                                                    (parameter_uncertainty_lower_value/7), parameter_uncertainty_lower_value),
         population_group = ifelse(covidence_id == 268, 
                                   "Unspecified", population_group),
         population_sample_type = ifelse(covidence_id == 268, 
                                   "Unspecified", population_sample_type))
# p3 <- save_plot(df_growth_rate, "Growth_rate", height = 100, width = 200)
# p3
# Calculate doubling times ln(2) / ln(r) https://mathcentral.uregina.ca/QQ/database/QQ.09.04/nick2.html https://en.wikipedia.org/wiki/Doubling_time 
# log(2) / 0.08
# log(2) / 0.066
# log(2) / 0.087
# 
# log(2) / 0.82
# log(2) / 5.8

p1_qa<- forest_plot(df_growth_rate %>% filter(!qa_score<0.5)|> arrange(desc(parameter_value)),
                    ycol = 'label_group',
                    label = 'Growth rate',
                    color_column="population_sample_type",
                    shape_column = "parameter_value_type",
                    lims = c(0,1),
                    text_size = TEXT_SIZE, 
                    point_size = 4) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.7, .7))
ggsave("Growth_rate_QA.png", plot =p1_qa, width = 10, height = 6, bg = 'white')

# p1_qa

p1 <- forest_plot(df_growth_rate |> arrange(desc(parameter_value)),
                  ycol = 'label_group',
                  label = 'Growth rate',
                  color_column="population_sample_type",
                  shape_column = "parameter_value_type",
                  lims = c(0,1),
                  text_size = TEXT_SIZE, 
                  point_size = 4) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.7, .7))
ggsave("Growth_rate.png", plot =p1, width = 10, height = 6, bg = 'white')

# p1


# length(unique(df_growth_rate$covidence_id))
# View(df_growth_rate[, c("central", "parameter_value", "parameter_uncertainty_lower_value", "parameter_uncertainty_upper_value",
#                         "parameter_lower_bound", "parameter_upper_bound", "population_sample_size", "population_country", "population_location",
#                         "population_group",
#                         "population_sample_type", "population_study_start_year",
#                         "population_study_end_year")])

# Cleaning and plotting for Overdispersion (assuming no units)
# df_overdispersion <- clean_data(params_clean.d4)
# df_overdispersion <- df_overdispersion %>% mutate(parameter_unit = "No units")
# save_plot(df_overdispersion, "Overdispersion")

# Cleaning and plotting for Severity
df_severity1 <- params_clean.d5 %>% #clean_data(params_clean.d5) %>%
  filter(!covidence_id == 7437) %>%  
  mutate(
    # Tidier titles for plot
    population_location = ifelse(population_location == "Congenital Zika Program at Children’s National (CZPCN) in Washington DC", 
                                 "Washington DC", 
                                 population_location),
    population_location = ifelse(population_location == "West Indies: Guadeloupe and Martinique",  
                                 "West Indies", 
                                 population_location),
    # Updating parameter_unit and parameter_value_type
    parameter_unit = ifelse(covidence_id == 12128, "Percentage (%)", parameter_unit),
    parameter_value_type = ifelse(covidence_id == 12128, "Unspecified", parameter_value_type)
  )

# length(unique(df_severity1$covidence_id))
# View(df_severity1[, c("central", "parameter_value", "parameter_uncertainty_type", "parameter_uncertainty_lower_value", "parameter_uncertainty_upper_value",
#                       "parameter_lower_bound", "parameter_upper_bound", "population_sample_size", "population_country", "population_location", 
#                       "population_group",
#                       "population_sample_type", "population_study_start_year", 
#                       "population_study_end_year")])

# p4 <- save_plot(df_severity1, "CFR", height = 100, width = 200)
# p4
p1_qa<- forest_plot(df_severity1 %>% filter(!qa_score<0.5)|> arrange(desc(parameter_value)),
                    ycol = 'label_group',
                    label = 'CFR',
                    color_column="population_sample_type",
                    shape_column = "parameter_value_type",
                    lims = c(0,50),
                    text_size = TEXT_SIZE, 
                    point_size = 4) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.7, .7))
ggsave("CFR_QA.png", plot =p1_qa, width = 10, height = 12, bg = 'white')

# p1_qa

p1 <- forest_plot(df_severity1 |> arrange(desc(parameter_value)),
                  ycol = 'label_group',
                  label = 'CFR',
                  color_column="population_sample_type",
                  shape_column = "parameter_value_type",
                  lims = c(0,50),
                  text_size = TEXT_SIZE, 
                  point_size = 4) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.7, .7))
ggsave("CFR.png", plot =p1, width = 10, height = 12, bg = 'white')

# p1

df_severity2 <- params_clean.d6  %>% #clean_data(params_clean.d6) %>%
  filter(!covidence_id == 7437) %>%  
  mutate(
    # Tidier titles for plot
    population_location = ifelse(population_location == "Congenital Zika Program at Children’s National (CZPCN) in Washington DC", 
                                 "Washington DC", 
                                 population_location),
    population_location = ifelse(population_location == "West Indies: Guadeloupe and Martinique",  
                                 "West Indies", 
                                 population_location),
    # Updating parameter_unit and parameter_value_type
    parameter_unit = ifelse(covidence_id == 12128, "Percentage (%)", parameter_unit),
    parameter_value_type = ifelse(covidence_id == 12128, "Unspecified", parameter_value_type)
  )


# p5 <- save_plot(df_severity2, "Proportion of Symptomatic", height = 100, width = 200)
# p5
p1_qa<- forest_plot(df_severity2 %>% filter(!qa_score<0.5)|> arrange(desc(parameter_value)),
                    ycol = 'label_group',
                    label = "Proportion of Symptomatic",
                    color_column="population_sample_type",
                    shape_column = "parameter_value_type",
                    lims = c(0,100),
                    text_size = TEXT_SIZE, 
                    point_size = 4) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.7, .85))
ggsave("sympt_prop_QA.png", plot =p1_qa, width = 10, height = 12, bg = 'white')

# p1_qa

p1 <- forest_plot(df_severity2 |> arrange(desc(parameter_value)),
                  ycol = 'label_group',
                  label = "Proportion of Symptomatic",
                  color_column="population_sample_type",
                  shape_column = "parameter_value_type",
                  lims = c(0,100),
                  text_size = TEXT_SIZE, 
                  point_size = 4) + 
  theme(legend.position = 'inside', legend.position.inside =  c(.7, .85))
ggsave("sympt_prop.png", plot =p1, width = 10, height = 12, bg = 'white')

# p1

# length(unique(df_severity2$covidence_id))
# View(df_severity2[, c("central", "parameter_value", "parameter_uncertainty_lower_value", "parameter_uncertainty_upper_value",
#                       "parameter_lower_bound", "parameter_upper_bound", "population_sample_size", "population_country", "population_location", 
#                       "population_group",
#                       "population_sample_type", "population_study_start_year", 
#                       "population_study_end_year")])


# Meta-analysis  CFR and prop sympt
# metaanalysis_CFR <- metaprop_wrap(dataframe = df_severity1, plot_pooled = TRUE, subgroup = NA, 
#                                   sort_by_subg = FALSE, x_label = "Severity - case fatality rate (CFR)",
#                                   plot_study = TRUE, digits = 2, colour = "dodgerblue3",
#                                   width = 9000, height = 16000, resolution = 1000)
# CFR_meta <- metaanalysis_CFR$plot
# CFR_meta
# ggsave("CFR_metaanalysis.png", plot = CFR_meta, width = 9, height = 7)
# 
# 
# metaanalysis_symp <- metaprop_wrap(dataframe = df_severity2, plot_pooled = TRUE, subgroup = NA, 
#                                   sort_by_subg = FALSE, x_label = "Severity - proportion of symptomatic cases",
#                                   plot_study = TRUE, digits = 2, colour = "dodgerblue3",
#                                   width = 9000, height = 16000, resolution = 1000)
# symp_meta <- metaanalysis_symp$plot
# symp_meta
# ggsave("Sympt_metaanalysis.png", plot = symp_meta, width = 9, height = 7)
