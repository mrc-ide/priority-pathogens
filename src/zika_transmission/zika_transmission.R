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
library(epireview)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("zika_compilation", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "outbreaks.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
orderly_artefact(description = "zika-specific figures", files = c("figure_5.png"))

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

parameters <- dfs$parameters %>% left_join(qa_scores) %>% 
  mutate(parameter_value = coalesce(parameter_value,central)) %>% arrange(desc(parameter_value))


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

#arrange data and format for plotting
d1 <- d1 %>% mutate(across(c(parameter_value,parameter_lower_bound, parameter_upper_bound,
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value),
                           ~. * 10^4)) #multiply by 10^4
d2 <- d2 %>% mutate(across(c(parameter_value,parameter_lower_bound, parameter_upper_bound,
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value),
                           ~. * 10^4)) #multiply by 10^4
d4 <- d4 %>% 
  mutate(parameter_unit = ifelse(parameter_unit == "No units", "Percentage (%)", parameter_unit),
         parameter_unit = replace_na(parameter_unit , "Percentage (%)"),
         parameter_value_type = replace_na(parameter_value_type, 'Unspecified')) %>%
  mutate_at(vars(c("parameter_value","parameter_lower_bound","parameter_upper_bound",
                   "parameter_uncertainty_lower_value","parameter_uncertainty_upper_value")), 
            list(~ ifelse(parameter_unit == "No units", . * 100, .))) 


d1 <- d1 %>% arrange(genome_site,-central) 
d2 <- d2 %>% arrange(genome_site,-central)
d3 <- d3 %>% arrange(-central)
d4 <- d4 %>% mutate(arate=ifelse(str_starts(d4$parameter_type,"Secondary"), 'Secondary', 'Primary')) %>%
  arrange(arate,-central)
d5 <- d5 %>% mutate(parameter_value_type = replace_na(parameter_value_type,'Unspecified')) %>%
  arrange(-central)
d6 <- d6 %>% arrange(parameter_type,-central)

d1 <- d1 %>% 
  mutate(genome_site          = replace_na(genome_site,'Unspecified'),
         parameter_value_type = replace_na(parameter_value_type, 'Unspecified') ) |>
  mutate(genome_site = factor(genome_site,
                              levels = unique(genome_site),
                              labels = c("L","S", "Unspecified"))) 

# post process extracted data for covidence id 6909
d1[d1$covidence_id == 6909,]$parameter_value <- d1[d1$covidence_id == 6909,]$parameter_value * 365
d1[d1$covidence_id == 6909,]$parameter_unit  <- 'Substitutions/site/year'

d2 <- d2 %>% 
  mutate(genome_site          = replace_na(genome_site,'Unspecified'),
         parameter_value_type = replace_na(parameter_value_type, 'Unspecified') ) |>
  mutate(genome_site = factor(genome_site,
                              levels = unique(genome_site),
                              labels = c("L","S", "Unspecified"))) 



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
                    population_sample_type = replace_na(population_sample_type, "Unspecified")) %>%
  filter(!(parameter_value>10|(!is.na(parameter_upper_bound) & parameter_upper_bound>10))) # exclude Kwok (2007) and Moser (2015) due to extreme values

#call forest plot function
p1 <- forest_plot(d1 |> filter(qa_score>0.5),expression(Evolutionary~Rate~(s/s/y ~10^{-4})),"genome_site",c(5,50)) + guides(color = guide_legend(title = "Segment", order = 1))
p2 <- forest_plot(d2 |> filter(qa_score>0.5),expression(Substitution~Rate~(s/s/y ~10^{-4})),"genome_site",c(0,100)) + guides(color = guide_legend(title = "Segment", order = 1))


# # overdispersion
# ##  Convert k params into the same type to be consistent between models.
# ##  need to get betas for covidence 3685:   parameters %>% filter(covidence_id==3685), will need to get N & S for this from paper
# d3_tmp <- d3 %>% mutate(parameter_unit = replace_na(parameter_unit,'No units')) %>%
#   filter(parameter_unit == 'No units') %>%
#   filter(qa_score > 0.5)  # drop Chowell 2015 with this!
# 
# d_beta_val   <- d_beta %>% filter(covidence_id==3685) |> pull(parameter_value)
# total_cases  <- d_beta %>% filter(covidence_id==3685) |> pull(population_sample_size)
# cases_phase1 <- 1114
# N_beijing    <- 1.4564e07
# 
# tt <- d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]
# d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_value <- (tt$parameter_value * N_beijing)/(d_beta_val*cases_phase1)
# d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_lower_bound <- (tt$parameter_lower_bound * N_beijing)/(d_beta_val*cases_phase1)
# d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_upper_bound <- (tt$parameter_upper_bound * N_beijing)/(d_beta_val*cases_phase1)
# d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_uncertainty_lower_value <- NA
# d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='Start outbreak',]$parameter_uncertainty_upper_value <- NA
# 
# tt <- d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]
# d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_value <- (tt$parameter_value * N_beijing)/(d_beta_val*(total_cases - cases_phase1))
# d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_lower_bound <- (tt$parameter_lower_bound * N_beijing)/(d_beta_val*(total_cases - cases_phase1))
# d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_upper_bound <- (tt$parameter_upper_bound * N_beijing)/(d_beta_val*(total_cases - cases_phase1))
# d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_uncertainty_lower_value <- NA
# d3_tmp[d3_tmp$covidence_id==3685 & d3_tmp$method_moment_value=='End outbreak',]$parameter_uncertainty_upper_value <- NA
# 
# p3 <- forest_plot( d3_tmp, 'Overdispersion',"method_moment_value",c(-0.1,1.5))      # Add location to plot??


# for plotting need to separate groups!
p4_gp  <- forest_plot(d4|> filter(qa_score>0.5) |> filter(population_group=="General population"),'Attack Rate (%) [General Population]',"arate",c(-5,30))
p4_hcw <- forest_plot(d4|> filter(qa_score>0.5) |> filter(population_group=="Healthcare workers"),'Attack Rate (%) [HCW]',"arate",c(-5,100))
p4_pui <- forest_plot(d4|> filter(qa_score>0.5) |> filter(population_group=="Persons under investigation"),'Attack Rate (%) [Persons under investigation]',"arate",c(-5,100))
#p4_mix <- forest_plot(d4|> filter(qa_score>0.5) |> filter(population_group=="Mixed groups"),'Attack Rate (%)',"arate",c(-5,100))

#p4_gp + p4_hcw + p4_pui + p4_mix + plot_annotation(tag_levels = 'A')  
p4_gp + p4_hcw + p4_pui + plot_annotation(tag_levels = 'A')  

p4_primary   <- forest_plot(d4|> filter(qa_score>0.5) |> filter(arate=='Primary'),'Attack Rate (%)',"population_group",c(-5,30))
p4_secondary <- forest_plot(d4|> filter(qa_score>0.5) |> filter(arate=='Secondary'),'Secondary Attack Rate (%)',"population_group",c(-5,100))

p4_primary + p4_secondary + plot_annotation(tag_levels = 'A') 

# Growth rate
d5$parameter_unit <- 'Per day'
p5 <- forest_plot(d5|> filter(qa_score>0.5) ,'Growth rate (r)',"pathogen",c(0,.25))

(p1+p2+ plot_layout(guides = "collect"))/ (p4_primary + p4_secondary + plot_layout(guides = "collect") ) / (p5+p3) + plot_annotation(tag_levels = 'A') 


# make colours across plots the same to simplify legend!!! + add in consideration for setting
# meta-analysis ?? how to deal with lack of sample size especially for CI rows etc?  --> double check 3468, 5722, 1023 to see if we can get sample sizes!
# sd = sqrt(N) * (upper - lower ) / 3.92
p6 <- forest_plot(d6,'Reproduction Number',"parameter_type",c(0,10))
p6_1 <- forest_plot( d6 |> 
                       filter(qa_score>0.5) |>
                       filter(str_detect(parameter_type,('Basic'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) #|>
                     #arrange(population_country,desc(parameter_value))
                     ,
                     'Basic Reproduction Number',"population_country_v2",c(0,6))
p6_2 <- forest_plot( d6 |>
                       filter(qa_score>0.5) |>
                       filter(str_detect(parameter_type,('Effective'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) #|>
                     #arrange(population_country,desc(parameter_value))
                     ,
                     'Effective Reproduction Number',"population_country_v2",c(-0.5,6))

d6$method_moment_value <- factor(d6$method_moment_value,levels = c('Start outbreak','Mid outbreak', 'Post outbreak', 'Unspecified'))
p6_3 <- forest_plot( d6 |> 
                       filter(qa_score>0.5) |>
                       filter(str_detect(parameter_type,('Basic'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) |>
                       mutate(method_moment_value = replace_na(method_moment_value,'Unspecified')) |> 
                       arrange(method_moment_value,desc(parameter_value))
                     ,
                     'Basic Reproduction Number',"method_moment_value",c(-0.5,10), show_label = TRUE)

p6_4 <- forest_plot( d6 |> 
                       filter(qa_score>0.5) |>
                       filter(str_detect(parameter_type,('Basic'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) |>
                       mutate(method_r = replace_na(method_r,'Unspecified')) |> 
                       arrange(method_r,desc(parameter_value))
                     ,
                     'Basic Reproduction Number',"method_r",c(-0.5,6))

p6_5 <- forest_plot( d6 |>
                       filter(qa_score>0.5) |>
                       mutate(method_moment_value = replace_na(method_moment_value,'Unspecified') ) |> 
                       filter(str_detect(parameter_type,('Effective'))) |> 
                       filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) #|>
                     #arrange(population_country,desc(parameter_value))
                     ,
                     'Effective Reproduction Number',"method_moment_value",c(-0.5,10), show_label = TRUE)

# add Panel C with meta analysis for R0 but need to work out how to do! 
# This could be a good figure 4 with more transmission stuff in figure 5.
#p6_1 + p6_2 + p6_3 + p6_4 + plot_annotation(tag_levels = 'A')  
p6_3 + p6_5 + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect")


R0_meta <- d6 |> 
  filter(str_detect(parameter_type,('Basic'))) |> 
  filter(qa_score>0.2) |>
  filter(parameter_value < 10 ) |>
  filter(parameter_uncertainty_upper_value<7|is.na(parameter_uncertainty_upper_value)) |>
  mutate(parameter_unit       = replace_na(parameter_unit,'No units'),
         parameter_value_type = replace_na(parameter_value_type, 'Unspecified'))

r0_ma <- metagen_wrap(dataframe = R0_meta, estmeansd_method = "Cai",
                      plot_study = TRUE, digits = 2, lims = c(0,6), colour = "dodgerblue3", label = "Basic Reproduction number",
                      width = 9500, height = 4200, resolution = 1000)
r0_ma$plot
