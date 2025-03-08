#task to plot SARS transmission figures
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
library(countrycode)
library(ggrepel)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("db_compilation", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "outbreaks.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
orderly_artefact("sars-specific figures",c("figure_3_r_plot.png",
                                           "figure_3_r_plot.pdf",
                                           "figure_3SI_r_plot.png",
                                           "figure_3SI_r_plot.pdf",
                                           "figure_3SI_r_plot_cnt.png",
                                           "figure_3SI_r_plot_cnt.pdf",
                                           "figure_3SI_r_plot_method.png",
                                           "figure_3SI_r_plot_method.pdf",
                                           "figure_4_r_other.png",
                                           "figure_4_r_other.pdf",
                                           "figure_4SI_r_other.png",
                                           "figure_4SI_r_other.pdf"))

###################
## DATA CURATION ##
###################

articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("parameters.csv")

dfs <- data_curation(articles,outbreaks,models,parameters, plotting = TRUE)

articles   <- dfs$articles

articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles %>% dplyr::select(covidence_id,qa_score)

outbreaks  <- dfs$outbreaks
models     <- dfs$models

parameters <- dfs$parameters %>% left_join(qa_scores)
  
##################
## TRANSMISSION ##
##################

#extract data to be plotted
#
# NEED TO SPLIT ATTACK RATES INTO ATTACK RATES + SECONDARY ATTACK RATES
parameters[parameters$parameter_type=='Secondary attack rate',]$parameter_class = 'Attack rate'

d1 <- parameters %>% filter(parameter_type == 'Mutations – mutation rate')
d2 <- parameters %>% filter(parameter_type == 'Mutations – substitution rate')
d3 <- parameters %>% filter(parameter_class == 'Overdispersion')
d4 <- parameters %>% filter(parameter_class == 'Attack rate')
d5 <- parameters %>% filter(parameter_class == 'Growth rate')
d6 <- parameters %>% filter(parameter_class == 'Reproduction number')

d_beta <- parameters %>% filter(parameter_type == 'beta - per capita contact rate per unit of time')

### Arrange data and format for plotting across variables of interest
d1 <- d1 %>% 
  mutate(parameter_value = case_when(parameter_unit=='Percentage (%)'~parameter_value/100, TRUE ~ parameter_value)) %>%
  mutate(across(c(parameter_value,parameter_lower_bound, parameter_upper_bound,
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value),
                           ~. * 10^4)) #multiply by 10^4
d1 <- d1 %>% arrange(genome_site,-central) 
d1 <- d1 %>% 
  mutate(genome_site          = replace_na(genome_site,'Unspecified'),
         parameter_value_type = replace_na(parameter_value_type, 'Unspecified') ) |>
  mutate(genome_site = factor(genome_site,
                              levels = unique(genome_site),
                              labels = c("L","S", "Unspecified"))) 
# post process extracted data for covidence id 6909
d1[d1$covidence_id == 6909,]$parameter_value <- d1[d1$covidence_id == 6909,]$parameter_value * 365
d1[d1$covidence_id == 6909,]$parameter_unit  <- 'Substitutions/site/year'


d2 <- d2 %>% mutate(across(c(parameter_value,parameter_lower_bound, parameter_upper_bound,
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value),
                           ~. * 10^4)) #multiply by 10^4
d2 <- d2 %>% arrange(genome_site,-central)
d2 <- d2 %>% 
  mutate(genome_site          = replace_na(genome_site,'Unspecified'),
         parameter_value_type = replace_na(parameter_value_type, 'Unspecified') ) |>
  mutate(genome_site = factor(genome_site,
                              levels = unique(genome_site),
                              labels = c("L","S", "Unspecified"))) 

d3 <- d3 %>% arrange(-central)

d4 <- d4 %>% 
  mutate(parameter_unit = ifelse(parameter_unit == "No units", "Percentage (%)", parameter_unit),
         parameter_unit = replace_na(parameter_unit , "Percentage (%)"),
         parameter_value_type = replace_na(parameter_value_type, 'Unspecified')) %>%
  mutate_at(vars(c("parameter_value","parameter_lower_bound","parameter_upper_bound",
                   "parameter_uncertainty_lower_value","parameter_uncertainty_upper_value")), 
                       list(~ ifelse(parameter_unit == "No units", . * 100, .))) 
  

d4 <- d4 %>% mutate(arate=ifelse(str_starts(d4$parameter_type,"Secondary"), 'Secondary', 'Primary')) %>%
  arrange(arate,-central)

d5 <- d5 %>% mutate(parameter_value_type = replace_na(parameter_value_type,'Unspecified')) %>%
  arrange(-central)

d6 <- d6 %>% arrange(parameter_type,-central)
d6 <- d6 %>% mutate(parameter_type = factor(parameter_type,
                                            levels = unique(parameter_type),
                                            labels = c("Basic (R0)","Effective (Re)")),
                    parameter_value = coalesce(parameter_value,central),
                    parameter_unit  = replace_na(parameter_unit, "No units"),
                    parameter_value_type  = replace_na(parameter_value_type, "Unspecified"),
                    population_country    = replace_na(population_country,"Unspecified"),
                    population_country_v2 = case_when(!str_detect(population_country,",")~population_country,
                                                      population_country=='Hong Kong SAR,  China'~'Greater China',
                                                      str_detect(population_country,'Canada, Hong Kong')|str_detect(population_country,'Canada;Singapore')~'Multi Region',
                                                      str_detect(population_country,'Taiwan,  China')~'Taiwan + China',
                                                      TRUE ~ population_country),
                    population_country_ISO = case_when(str_detect(population_country,'Unspecified')~'Unspecified',
                                                       !str_detect(population_country,",")~countrycode::countrycode(population_country,'country.name','iso3c'),
                                                       population_country=='Hong Kong SAR,  China'~'CHN+HKG',
                                                       str_detect(population_country,'Canada, Hong Kong')|str_detect(population_country,'Canada;Singapore')~'Multi Region',
                                                       str_detect(population_country,'Taiwan,  China')~'CHN+TWN',
                                                       TRUE ~ countrycode::countrycode(population_country,'country.name','iso3c')),
                    population_sample_type = replace_na(population_sample_type, "Unspecified")) %>%
  filter(!(parameter_value>10|(!is.na(parameter_upper_bound) & parameter_upper_bound>10))) # exclude Kwok (2007) and Moser (2015) due to extreme values

### Additional computations for overdispersion plot
# overdispersion
##  Convert k params into the same type to be consistent between models.
##  need to get betas for covidence 3685:   parameters %>% filter(covidence_id==3685), will need to get N & S for this from paper
d3_tmp <- d3 %>% mutate(parameter_unit = replace_na(parameter_unit,'No units')) %>%
  filter(parameter_unit == 'No units') %>%
  filter(qa_score > 0.5)  # drop Chowell 2015 with this!

d_beta_val   <- d_beta %>% filter(covidence_id==3685) |> pull(parameter_value)
total_cases  <- d_beta %>% filter(covidence_id==3685) |> pull(population_sample_size)
cases_phase1 <- 1114
N_beijing    <- 1.4564e07

tt <- d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_value <- (tt$parameter_value * N_beijing)/(d_beta_val*cases_phase1)
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_lower_bound <- (tt$parameter_lower_bound * N_beijing)/(d_beta_val*cases_phase1)
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_upper_bound <- (tt$parameter_upper_bound * N_beijing)/(d_beta_val*cases_phase1)
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_uncertainty_lower_value <- NA
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_uncertainty_upper_value <- NA

tt <- d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_value <- (tt$parameter_value * N_beijing)/(d_beta_val*(total_cases - cases_phase1))
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_lower_bound <- (tt$parameter_lower_bound * N_beijing)/(d_beta_val*(total_cases - cases_phase1))
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_upper_bound <- (tt$parameter_upper_bound * N_beijing)/(d_beta_val*(total_cases - cases_phase1))
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_uncertainty_lower_value <- NA
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_uncertainty_upper_value <- NA

d3_with_qa <- d3_tmp

d3_tmp <- d3 %>% mutate(parameter_unit = replace_na(parameter_unit,'No units')) %>%
  filter(parameter_unit == 'No units')

tt <- d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_value <- (tt$parameter_value * N_beijing)/(d_beta_val*cases_phase1)
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_lower_bound <- (tt$parameter_lower_bound * N_beijing)/(d_beta_val*cases_phase1)
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_upper_bound <- (tt$parameter_upper_bound * N_beijing)/(d_beta_val*cases_phase1)
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_uncertainty_lower_value <- NA
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_uncertainty_upper_value <- NA

tt <- d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_value <- (tt$parameter_value * N_beijing)/(d_beta_val*(total_cases - cases_phase1))
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_lower_bound <- (tt$parameter_lower_bound * N_beijing)/(d_beta_val*(total_cases - cases_phase1))
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_upper_bound <- (tt$parameter_upper_bound * N_beijing)/(d_beta_val*(total_cases - cases_phase1))
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_uncertainty_lower_value <- NA
d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_uncertainty_upper_value <- NA

d3_without_qa <- d3_tmp

### Construct subplots

# Evolutionary mutation rates
p1 <- forest_plot(d1 |> filter(qa_score>0.5),expression(Evolutionary~Rate~(s/s/y ~10^{-4})),"genome_site",c(0,50),
                  custom_colours = c('L'='brown', 'S' = 'yellow2','Unspecified'='grey'),text_size = 16) + 
  guides(color = guide_legend(title = "Segment", order = 1))
p2 <- forest_plot(d2 |> filter(qa_score>0.5),expression(Substitution~Rate~(s/s/y ~10^{-4})),"genome_site",c(0,100),
                  custom_colours = c('L'='brown', 'S' = 'yellow2','Unspecified'='grey'),text_size = 16) + 
  guides(color = guide_legend(title = "Segment", order = 1))

p1_noqa <- forest_plot(d1|>mutate(parameter_unit='Unspecified'), expression(Evolutionary~Rate~(s/s/y ~10^{-4})),"genome_site",c(-5,200),
                  custom_colours = c('L'='brown', 'S' = 'yellow2','Unspecified'='grey'),text_size = 16) + 
  guides(color = guide_legend(title = "Segment", order = 1))
p2_noqa <- forest_plot(d2, expression(Substitution~Rate~(s/s/y ~10^{-4})),"genome_site",c(0,100),
                  custom_colours = c('L'='brown', 'S' = 'yellow2','Unspecified'='grey'),text_size = 16) + 
  guides(color = guide_legend(title = "Segment", order = 1))

#Overdispersion
p3 <- forest_plot( d3_with_qa, 'Overdispersion',"method_moment_value",c(-0.1,1.5),text_size = 16)      # Add location to plot??
p3_noqa <- forest_plot( d3_without_qa, 'Overdispersion',"method_moment_value",c(-0.1,1.5),text_size = 16)      # Add location to plot??

# Attack rates
p4_primary   <- forest_plot(d4|> filter(qa_score>0.5) |> filter(arate=='Primary'),'Attack Rate (%)',"population_group",c(-5,75),
                            custom_colours = c('General population'='darkblue', 'Healthcare workers' = 'darkred','Other'='lightgreen', 'Persons under investigation'='purple', 'Household contacts of survivors'='yellow3'),
                            text_size = 16)
p4_secondary <- forest_plot(d4|> filter(qa_score>0.5) |> filter(arate=='Secondary'),'Secondary Attack Rate (%)',"population_group",c(-5,100),
                            custom_colours = c('General population'='darkblue', 'Healthcare workers' = 'darkred','Other'='lightgreen', 'Persons under investigation'='purple', 'Household contacts of survivors'='yellow3'),
                            text_size = 16)

p4_primary_noqa   <- forest_plot(d4 |> filter(arate=='Primary'),'Attack Rate (%)',"population_group",c(-5,75),
                            custom_colours = c('General popultion'='darkblue', 'Healthcare workers' = 'darkred','Other'='lightgreen', 'Persons under investigation'='purple2', 'Household contacts of survivors'='yellow3', 'Mixed groups' = 'pink2'),
                            text_size = 16)
p4_secondary_noqa <- forest_plot(d4 |> filter(arate=='Secondary'),'Secondary Attack Rate (%)',"population_group",c(-5,100),
                            custom_colours = c('General population'='darkblue', 'Healthcare workers' = 'darkred','Other'='lightgreen', 'Persons under investigation'='purple2', 'Household contacts of survivors'='yellow3', 'Mixed groups' = 'pink2'),
                            text_size = 16)


# Growth rate -- display in percentage terms as requested
d5$parameter_unit <- 'Per day'
d5$parameter_value <- d5$parameter_value *100
d5$parameter_lower_bound <- d5$parameter_lower_bound *100
d5$parameter_upper_bound <- d5$parameter_upper_bound *100
p5 <- forest_plot(d5|> filter(qa_score>0.5) ,'Growth rate (r per day in %)',"pathogen",c(0,25),text_size = 16)
p5_noqa <- forest_plot(d5, 'Growth rate (r per day in %)',"pathogen",c(0,25),text_size = 16)


# make colours across plots the same to simplify legend!!! + add in consideration for setting
# meta-analysis ?? how to deal with lack of sample size especially for CI rows etc?  --> double check 3468, 5722, 1023 to see if we can get sample sizes!
# sd = sqrt(N) * (upper - lower ) / 3.92
p6_1 <- forest_plot( d6 |> 
                       filter(qa_score>0.5) |>
                       filter(str_detect(parameter_type,('Basic'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value))|> 
                       arrange(population_country_v2,desc(parameter_value)),
                     'Basic Reproduction Number',"population_country_v2",c(0,6),
                     custom_colours = c('Canada'='darkblue', 'China' = 'darkred', 'Greater China'='red2','Multi Region'='lightgreen', 'Singapore'='purple4', 'Vietnam'='yellow3','Unspecified'='grey','Taiwan'='pink','Taiwan + China'= 'pink4','Hong Kong SAR' = 'brown4','Canada + Singapore' = 'darkgreen'),
                     text_size = 16)

p6_1_noqa <- forest_plot( d6 |> 
                       filter(str_detect(parameter_type,('Basic'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value))|> 
                       arrange(population_country_v2,desc(parameter_value)),
                     'Basic Reproduction Number',"population_country_v2",c(0,6),
                     custom_colours = c('Canada'='darkblue', 'China' = 'darkred', 'Greater China'='red2','Multi Region'='lightgreen', 'Singapore'='purple4', 'Vietnam'='yellow3','Unspecified'='grey','Taiwan'='pink','Taiwan + China'= 'pink4','Hong Kong SAR' = 'brown4','Canada + Singapore' = 'darkgreen'),
                     text_size = 16) 

p6_2 <- forest_plot( d6 |>
                       filter(qa_score>0.5) |>
                       filter(str_detect(parameter_type,('Effective'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) |> 
                       arrange(population_country_v2,desc(parameter_value)),
                     'Effective Reproduction Number',"population_country_v2",c(-0.5,6),
                     custom_colours = c('Canada'='darkblue', 'China' = 'darkred', 'Greater China'='red2','Multi Region'='lightgreen', 'Singapore'='purple4', 'Vietnam'='yellow3','Unspecified'='grey','Taiwan'='pink','Taiwan + China'= 'pink4','Hong Kong SAR' = 'brown4','Canada + Singapore' = 'darkgreen'),
                     text_size = 16)

p6_2_noqa <- forest_plot( d6 |>
                       filter(str_detect(parameter_type,('Effective'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value))|> 
                         arrange(population_country_v2,desc(parameter_value)),
                     'Effective Reproduction Number',"population_country_v2",c(-0.5,6),
                     custom_colours = c('Canada'='darkblue', 'China' = 'darkred', 'Greater China'='red2','Multi Region'='lightgreen', 'Singapore'='purple4', 'Vietnam'='yellow3','Unspecified'='grey','Taiwan'='pink','Taiwan + China'= 'pink4','Hong Kong SAR' = 'brown4','Canada + Singapore' = 'darkgreen'),
                     text_size = 16)


d6$method_moment_value <- factor(d6$method_moment_value,levels = c('Start outbreak','Mid outbreak', 'Post outbreak', 'Unspecified'))
p6_3 <- forest_plot( d6 |> 
                       filter(qa_score>0.5) |>
                       filter(str_detect(parameter_type,('Basic'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) |>
                       mutate(method_moment_value = replace_na(method_moment_value,'Unspecified')) |> 
                       arrange(method_moment_value,desc(parameter_value)),
                     'Basic Reproduction Number',"method_moment_value",c(-0.5,6), show_label = TRUE,
                     text_size = 16)

p6_3_noqa <- forest_plot( d6 |> 
                       filter(str_detect(parameter_type,('Basic'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) |>
                       mutate(method_moment_value = replace_na(method_moment_value,'Unspecified')) |> 
                       arrange(method_moment_value,desc(parameter_value)),
                     'Basic Reproduction Number',"method_moment_value",c(-0.5,6), show_label = TRUE,
                     custom_colours = c('Start outbreak'='steelblue', 'Mid outbreak' = 'darkred', 'Post outbreak'='lightgreen','Unspecified' = 'grey'),
                     text_size = 16)


p6_4e <- forest_plot( d6 |> 
                       filter(qa_score>0.5) |>
                       filter(str_detect(parameter_type,('Effective'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) |>
                       mutate(method_r = replace_na(method_r,'Unspecified')) |> 
                       arrange(method_r,desc(parameter_value)),
                     'Effective Reproduction Number',"method_r",c(-0.5,6),
                     text_size = 16)

p6_4e_noqa <- forest_plot( d6 |> 
                            filter(str_detect(parameter_type,('Effective'))) |> 
                            filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) |>
                            mutate(method_r = replace_na(method_r,'Unspecified')) |> 
                            arrange(method_r,desc(parameter_value)),
                          'Effective Reproduction Number',"method_r",c(-0.5,6),
                          text_size = 16) + theme(legend.position = 'none')

p6_4 <- forest_plot( d6 |> 
                       filter(qa_score>0.5) |>
                       filter(str_detect(parameter_type,('Basic'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) |>
                       mutate(method_r = replace_na(method_r,'Unspecified')) |> 
                       arrange(method_r,desc(parameter_value)),
                     'Basic Reproduction Number',"method_r",c(-0.5,6),
                     text_size = 16)

p6_4_noqa <- forest_plot( d6 |> 
                       filter(str_detect(parameter_type,('Basic'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) |>
                       mutate(method_r = replace_na(method_r,'Unspecified')) |> 
                       arrange(method_r,desc(parameter_value)),
                     'Basic Reproduction Number',"method_r",c(-0.5,6),
                     text_size = 16) 

p6_5 <- forest_plot( d6 |>
                       filter(qa_score>0.5) |>
                       mutate(method_moment_value = replace_na(method_moment_value,'Unspecified') ) |> 
                       filter(str_detect(parameter_type,('Effective'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)),
                     'Effective Reproduction Number',"method_moment_value",c(-0.5,6), show_label = TRUE,
                     text_size = 16)

p6_5_noqa <- forest_plot( d6 |>
                       mutate(method_moment_value = replace_na(method_moment_value,'Unspecified') ) |> 
                       filter(str_detect(parameter_type,('Effective'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) |>
                         arrange(method_moment_value,desc(parameter_value)),
                     'Effective Reproduction Number',"method_moment_value",c(-0.5,6), show_label = TRUE,
                     custom_colours = c('Start outbreak'='steelblue', 'Mid outbreak' = 'darkred', 'Post outbreak'='lightgreen','Unspecified' = 'grey'),
                     text_size = 16) + theme(legend.position = 'none')


# Meta-analysis not possible as too few data points
# R0_meta <- d6 |> 
#   filter(str_detect(parameter_type,('Basic'))) |> 
#   filter(qa_score>0.2) |>
#   filter(parameter_value < 10 ) |>
#   filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) |>
#   mutate(parameter_unit       = replace_na(parameter_unit,'No units'),
#          parameter_value_type = replace_na(parameter_value_type, 'Unspecified'))
# 
# r0_ma <- metagen_wrap(dataframe = R0_meta, estmeansd_method = "Cai",
#                     plot_study = TRUE, digits = 2, lims = c(0,6), colour = "dodgerblue3", label = "Basic Reproduction number",
#                     width = 9500, height = 4200, resolution = 1000)


### Compile plots into figures (maintext + SI)
R_plot <- p6_3 + p6_5 + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect")
ggsave("figure_3_r_plot.png", plot = R_plot, width = 20, height = 10)
ggsave("figure_3_r_plot.pdf", plot = R_plot, width = 20, height = 10)


R_plot_SI <- p6_3_noqa + p6_5_noqa + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect")
ggsave("figure_3SI_r_plot.png", plot = R_plot_SI, width = 20, height = 10)
ggsave("figure_3SI_r_plot.pdf", plot = R_plot_SI, width = 20, height = 10)

R_plot_SI_cnt <- p6_1_noqa + p6_2_noqa + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect")
ggsave("figure_3SI_r_plot_cnt.png", plot = R_plot_SI_cnt, width = 20, height = 10)
ggsave("figure_3SI_r_plot_cnt.pdf", plot = R_plot_SI_cnt, width = 20, height = 10)

R_plot_SI_method <- p6_4_noqa + p6_4e_noqa + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect")
ggsave("figure_3SI_r_plot_method.png", plot = R_plot_SI_method, width = 20, height = 10)
ggsave("figure_3SI_r_plot_method.pdf", plot = R_plot_SI_method, width = 20, height = 10)


layout3 <- '
AB
AB
AB
CD
EF
'

p1              <- p1 + theme(legend.position = 'none')
p1_noqa         <- p1_noqa + theme(legend.position = 'none')
p4_primary      <- p4_primary + theme(legend.position = 'none')
p4_primary_noqa <- p4_primary_noqa + theme(legend.position = 'none')

r_other <- p4_primary + p4_secondary + p5+p3 + p1+p2+plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect",design=layout3)
ggsave("figure_4_r_other.png", plot = r_other, width = 20, height = 12)
ggsave("figure_4_r_other.pdf", plot = r_other, width = 20, height = 12)

r_other_SI <- p4_primary_noqa + p4_secondary_noqa + p5_noqa+p3_noqa + p1_noqa+p2_noqa+plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect",design=layout3)
ggsave("figure_4SI_r_other.png", plot = r_other_SI, width = 20, height = 12)
ggsave("figure_4SI_r_other.pdf", plot = r_other_SI, width = 20, height = 12)
