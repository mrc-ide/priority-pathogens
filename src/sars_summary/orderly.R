#task to plot sars-specific figures (mostly in SI)
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
library(ggbreak)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("db_compilation", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")

orderly_artefact("sars-specific figures",
                 c("figure_S1.png","figure_S2.png","figure_S3.png"))

###################
## DATA CURATION ##
###################

articles   <- read_csv("articles.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("parameters.csv")

dfs <- data_curation(articles,tibble(),models,parameters, plotting = TRUE)

articles   <- dfs$articles

articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles %>% dplyr::select(covidence_id,qa_score)

outbreaks  <- dfs$outbreaks
models     <- dfs$models

parameters <- dfs$parameters %>% left_join(qa_scores) %>% 
  mutate(parameter_value = coalesce(parameter_value,central)) %>% arrange(desc(parameter_value))

#####################
## PARAMETER STATS ##                Leave out for now but may be used later
#####################
if( FALSE )
{
  param_count_overall <- parameters %>% filter(parameter_type!='Risk factors') %>%   # count without risk factors
    summarise(n_param   = n(),
              n_article = length(unique(refs)))
  
  param_count_noqa <- parameters %>% group_by(parameter_class) %>% 
    summarise(n_param   = n(),
              n_article = length(unique(refs)))
  
  param_count_qa <- parameters %>% group_by(parameter_class) %>%
    filter(qa_score>0.5) %>%
    summarise(n_param   = n(),
              n_article = length(unique(refs)))
  
  param_count <- param_count_noqa %>% left_join(param_count_qa,by=c('parameter_class'),suffix = c('_noqa','_qa')) %>% 
    mutate(param_post_qa_filter = paste0(round(replace_na(n_param_qa,0) / n_param_noqa * 100,0),'%'),
           article_post_qa_filter = paste0(round(replace_na(n_article_qa,0) / n_article_noqa * 100,0),'%'))
  
  param_count_noqa_type <- parameters %>% group_by(parameter_type) %>% 
    summarise(n_param   = n(),
              n_article = length(unique(refs)))
  
  param_count_qa_type <- parameters %>% group_by(parameter_type) %>%
    filter(qa_score>0.5) %>%
    summarise(n_param   = n(),
              n_article = length(unique(refs)))
  
  param_count_type <- param_count_noqa_type %>% left_join(param_count_qa_type,by=c('parameter_type'),suffix = c('_noqa','_qa'))%>% 
    mutate(param_post_qa_filter = paste0(round(replace_na(n_param_qa,0) / n_param_noqa * 100,0),'%'),
           article_post_qa_filter = paste0(round(replace_na(n_article_qa,0) / n_article_noqa * 100,0),'%'))
  
  parameters %>% 
    filter(parameter_class == 'Severity') %>%
    mutate(cfr_ifr_method = replace_na(cfr_ifr_method,'Unspecified'),
           cfr_ifr_method = case_when(cfr_ifr_method=="Naïve" ~ 'Naive',
                                      TRUE ~ cfr_ifr_method)) %>%
    group_by(parameter_type,population_country) %>%   
    mutate(parameter_uncertainty_lower_value = case_when(is.infinite(parameter_uncertainty_lower_value) ~ NA,
                                                         TRUE ~ parameter_uncertainty_lower_value ), 
           parameter_uncertainty_upper_value = case_when(is.infinite(parameter_uncertainty_upper_value) ~ NA,
                                                         TRUE ~ parameter_uncertainty_upper_value )) %>%
    summarise(n_param       = n(),
              n_article     = length(unique(refs)),
              mean          = mean(parameter_value, na.rm = TRUE),
              central_min     = min(parameter_value,na.rm = TRUE ),
              central_max     = max(parameter_value,na.rm = TRUE ),
              #weighted.mean = weighted.mean(parameter_value, population_sample_size, na.rm = TRUE),
              minimum       = min(parameter_uncertainty_lower_value,na.rm = TRUE ),
              maximum       = max(parameter_uncertainty_upper_value,na.rm = TRUE )) 
  
  parameters %>% 
    filter(parameter_class == 'Severity') %>%
    mutate(cfr_ifr_method = replace_na(cfr_ifr_method,'Unspecified'),
           cfr_ifr_method = case_when(cfr_ifr_method=="Naïve" ~ 'Naive',
                                      TRUE ~ cfr_ifr_method)) %>% group_by(cfr_ifr_method) %>%  
    summarise(n_param   = n(),
              n_article = length(unique(refs)))
  
  
  parameters %>% 
    filter(parameter_class == 'Human delay') %>%
    group_by(parameter_type) %>%   
    mutate(parameter_uncertainty_lower_value = case_when(is.infinite(parameter_uncertainty_lower_value) ~ NA,
                                                         TRUE ~ parameter_uncertainty_lower_value ), 
           parameter_uncertainty_upper_value = case_when(is.infinite(parameter_uncertainty_upper_value) ~ NA,
                                                         TRUE ~ parameter_uncertainty_upper_value )) %>%
    summarise(n_param         = n(),
              n_article       = length(unique(refs)),
              central_min     = min(parameter_value,na.rm = TRUE ),
              central_max     = max(parameter_value,na.rm = TRUE ),
              uncertainty_min = min(parameter_uncertainty_lower_value,na.rm = TRUE ),
              uncertainty_max = max(parameter_uncertainty_upper_value,na.rm = TRUE )) 
  
  
  parameters %>% filter(parameter_type == 'Reproduction number (Basic R0)') %>% 
    filter(qa_score>0.5) %>% 
    summarise(min=min(parameter_value),
              max=max(parameter_value))
  
  
  riskfactor_count_noqa <- parameters %>% filter(parameter_type=='Risk factors') %>% group_by(riskfactor_outcome) %>% 
    summarise(n_param   = n(),
              n_article = length(unique(refs)))
  
  parameters %>% filter(parameter_type=='Risk factors' & 
                          riskfactor_outcome %in% c('Death','Severe disease','Infection') & 
                          riskfactor_significant == 'Significant' ) %>% 
    filter(str_detect(riskfactor_name,'Sex')) %>%
    group_by(riskfactor_outcome) %>% 
    summarise(n_param   = n(),
              n_article = length(unique(refs)))
  
  parameters %>% filter(parameter_type=='Risk factors' ) %>%
    mutate(qa_label = case_when(qa_score>0.5~'high_qa', TRUE ~ 'low_qa')) %>%
    group_by(riskfactor_significant,qa_label) %>% 
    summarise(n_param   = n(),
              n_article = length(unique(refs)))
  
  parameters %>% filter(parameter_type=='Risk factors' ) %>%
    mutate(qa_label = case_when(qa_score>0.5~'high_qa', TRUE ~ 'low_qa')) %>%
    group_by(riskfactor_significant,qa_label) %>% 
    summarise(n_param   = n(),
              n_article = length(unique(refs)))
  
  parameters %>% filter(str_starts(parameter_type,'Seroprevalence ') ) %>%
    filter(qa_score>0.5) %>%
    #  filter(population_group %in% c("Children","General Population","Mixed Groups")) %>%
    #  filter(population_group %in% c("Healthcare Workers")) %>%
    filter(population_group %in% c("Persons Under Investigation")) %>%
    summarise(n_param   = n(),
              n_article = length(unique(refs)))
  
  parameters %>% filter(parameter_type == 'Human delay - incubation period' ) %>%
    filter(qa_score>0.5) %>%
    #  filter(population_group %in% c("Children","General Population","Mixed Groups")) %>%
    #  filter(population_group %in% c("Healthcare Workers")) %>%
    #filter(population_group %in% c("Persons Under Investigation")) %>%
    summarise(n_param   = n(),
              n_article = length(unique(refs)))
  
  models %>% group_by(model_type,stoch_deter ) %>%
    summarise(n_param   = n()) %>%
    mutate(stoch_deter=replace_na(stoch_deter,'Unspecified'),
           stoch_deter=case_when(stoch_deter=="Deterministic;Stochastic"~'Both',TRUE~stoch_deter)) %>%
    pivot_wider(names_from = stoch_deter,values_from = n_param,values_fill = 0)
}

##############
## ARTICLES ##
##############

quality <- articles %>% 
  mutate_at(vars(starts_with("qa")), funs(replace(., . == "Yes", 1))) %>%
  mutate_at(vars(starts_with("qa")), funs(replace(., . == "No", 0))) %>%
  mutate_at(vars(starts_with("qa")), as.numeric)
quality <- quality %>% rowwise() %>% mutate(score = 100*qa_score)
quality <- quality %>% mutate(category = ifelse(covidence_id %in% models$covidence_id, "Modelling Studies", "Non-Modelling Studies"))
quality$category <- factor(quality$category, levels = c("Non-Modelling Studies", "Modelling Studies"))

answers <- quality %>% 
  filter(!is.na(year_publication) & !is.na(pathogen)) %>% 
  dplyr::select(covidence_id,qa_m1,qa_m2,qa_a3,qa_a4,qa_d5,qa_d6,qa_d7) %>%
  pivot_longer(-covidence_id,names_to = "Question",values_to= "Assessment") %>% 
  mutate(Assessment=as.factor(as.character(Assessment)),
         Assessment=case_when(Assessment == '1' ~ 'Yes',
                              Assessment == '0' ~ 'No')) %>%
  mutate(Question=case_when(Question=="qa_m1"~"Q1 Method: \nClear & \nReproducible",
                            Question=="qa_m2"~"Q2 Method: \nRobust & \nAppropriate",
                            Question=="qa_a3"~"Q3 Assumptions: \nClear & \nReproducible",
                            Question=="qa_a4"~"Q4 Assumptions: \nRobust & \nAppropriate",
                            Question=="qa_d5"~"Q5 Data: \nClear & \nReproducible",
                            Question=="qa_d6"~"Q6 Data: \nIssues \nAcknowledged",
                            Question=="qa_d7"~"Q7 Data: \nIssues \nAccounted For")) 

answers$Question <- factor(answers$Question, levels=rev(c("Q1 Method: \nClear & \nReproducible",
                                                          "Q2 Method: \nRobust & \nAppropriate",
                                                          "Q3 Assumptions: \nClear & \nReproducible",
                                                          "Q4 Assumptions: \nRobust & \nAppropriate",
                                                          "Q5 Data: \nClear & \nReproducible",
                                                          "Q6 Data: \nIssues \nAcknowledged",
                                                          "Q7 Data: \nIssues \nAccounted For")))

answers$Assessment[is.na(answers$Assessment)] <- "NA"
answers$Assessment <- factor(answers$Assessment,levels=c("NA","No","Yes"))

p1 <- ggplot(data=articles, aes(x = year_publication)) +
  geom_histogram(binwidth = 1, fill = "steelblue4", color = "black", alpha = 0.7) +
  scale_x_continuous(limits = c(2000,2025), breaks = seq(2000, 2023, by = 5), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0,75), expand = c(0, 0)) +
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA)) + 
  labs(x = "Year of Publication", y = "Article Count")

p2 <- ggplot() +
  geom_histogram(data=quality, aes(x = score), binwidth = 20, boundary = 0, fill = "steelblue4", color = "black", alpha = 0.7) +
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20)) + 
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA)) + 
  labs(x = "Quality Assessment Score (%)", y = "Article Count")

p3 <- ggplot() +
  geom_point(data = quality, aes(x=year_publication,y=score,color=category)) +
  geom_smooth(data = subset(quality, category == "Non-Modelling Studies"), aes(x=year_publication,y=score), span = 2, color = "springgreen3", fill = "springgreen3") +
  geom_smooth(data = subset(quality, category == "Modelling Studies"), aes(x=year_publication,y=score), span = 2, color = "red", fill = "red") +
  scale_x_continuous(limits = c(2000,2025), breaks = seq(2000, 2023, by=5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20)) +
  xlab("Year of Publication") + ylab("Quality Assessment Score (%)") +  
  scale_color_manual(values = c("Non-Modelling Studies" = "springgreen3","Modelling Studies" = "red"), name = NULL) +
  theme_minimal() +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = 'bottom')

p4 <- answers %>%
  group_by(Question,Assessment) %>% summarize(count=n()) %>% ungroup() %>%
  ggplot(aes(fill=Assessment, y=count, x=Question)) + 
  geom_bar(position="stack", stat="identity") + theme_bw() +
  scale_y_continuous(limits = c(0,300), breaks = seq(0,300,by=40), expand = c(0, 0)) +
  scale_fill_manual(values = c("darkolivegreen2","coral1","grey70"),aesthetics = "fill",name="",breaks=c('Yes', 'No','NA')) +
  xlab("") + ylab("Article Count") + 
  coord_flip() +
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA)) +
  theme(legend.position = 'bottom')

patchwork <- (p1 + p2 + p3 + p4) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_S1.png", plot = patchwork, width = 12, height = 12)

############
## MODELS ##
############

models <- models %>% mutate(stoch_deter = replace_na(stoch_deter,'Unspecified'),
                            assumptions = replace_na(assumptions,'Unspecified'),
                            compartmental_type = replace_na(compartmental_type,'Unspecified'),
                            interventions_type = replace_na(interventions_type,'Unspecified'),
                            transmission_route = replace_na(transmission_route,'Unspecified')) %>%
                     mutate(model_type = str_replace_all(model_type,"Branching process","Branching Process"),
                            assumptions = str_replace_all(assumptions,";Latent period is same as incubation period",""),
                            compartmental_type = str_replace_all(compartmental_type,";SIR",""),
                            interventions_type = str_replace_all(interventions_type,"Unspecified","Other"),
                            stoch_deter = str_replace(stoch_deter,'Deterministic;Stochastic','Other')) %>%
                     mutate(model_type = case_when(model_type=="Agent / Individual based" ~ 'IBM',
                                                   model_type%in%c("Agent / Individual based;Branching Process","Compartmental;Other" ) ~ 'Other',
                                                   TRUE ~ model_type),
                            assumptions = case_when( assumptions %in% c("Age dependent susceptibility;Heterogenity in transmission rates - between groups",
                                                                        "Heterogenity in transmission rates - over time;Homogeneous mixing",
                                                                        "Heterogenity in transmission rates - between groups;Heterogenity in transmission rates - over time;Homogeneous mixing",
                                                                        "Heterogenity in transmission rates - between groups;Homogeneous mixing",
                                                                        "Age dependent susceptibility;Heterogenity in transmission rates - between groups;Homogeneous mixing",
                                                                        "Heterogenity in transmission rates - over time;Unspecified") ~ 'Several assumptions',
                                                     TRUE ~ assumptions),
                            compartmental_type = case_when(compartmental_type %in% c("Other compartmental;SEIR", "SIR;SIS") ~ 'Other compartmental',
                                                           TRUE ~ compartmental_type))

p1 <- ggplot() + 
  geom_bar(data = models, aes(x = model_type, fill = stoch_deter), color = "black") + 
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,by = 5), expand = c(0,0)) +
  xlab("Model Type") + ylab("Model Count") +
  scale_fill_manual(values = c("Deterministic" = "steelblue4","Stochastic" = "red", 'Unspecified' = 'grey40', 'Other'='grey80'), name = NULL) +
  theme_minimal() +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = c(0,1), legend.justification = c(0,1), legend.box.just = "left")

p2 <- ggplot() + 
  geom_bar(data = models, aes(x = factor(transmission_route, levels = c("Human to human (direct contact)", "Airborne or close contact",
                                                                        "Airborne or close contact;Human to human (direct contact)",
                                                                        "Unspecified")), fill = model_type), color = "black") + 
  scale_x_discrete(labels = c("Airborne or close contact;Human to human (direct contact)" = "Airborne\n& Human-Human",
                              "Human to human (direct contact)" = "Human-Human Only")) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,by = 5), expand = c(0,0)) +
  xlab("Transmission Route(s)") + ylab("Model Count") +
  scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3", 'IBM' = 'steelblue' , 'Unspecified' = 'grey'), name = NULL) +
  theme_minimal() +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = c(0,1), legend.justification = c(0,1), legend.box.just = "left")

p3 <- ggplot() + 
  geom_bar(data = models, aes(x = factor(assumptions, levels = c("Homogeneous mixing",
                                                                 "Heterogenity in transmission rates - over time",
                                                                 "Heterogenity in transmission rates - between groups",
                                                                 "Age dependent susceptibility",
                                                                 "Heterogenity in transmission rates - between groups;Heterogenity in transmission rates - over time",
                                                                 'Unspecified', 'Several assumptions')), fill = model_type), color = "black") +
  scale_x_discrete(labels = c("Homogeneous mixing" = "Homogeneous",
                              "Heterogenity in transmission rates - over time" = "Time-\nHeterogeneous",
                              "Heterogenity in transmission rates - between groups" = "Subgroup-\nHeterogeneous",
                              "Age dependent susceptibility" = "Age-\nHeterogeneous",
                              "Heterogenity in transmission rates - between groups;Heterogenity in transmission rates - over time" = "Time- & Subgroup-\nHeterogeneous")) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,by = 5), expand = c(0,0)) +
  xlab("Human Transmission Heterogeneity") + ylab("Model Count") +
  scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3", 'IBM' = 'steelblue' , 'Unspecified' = 'grey'), name = NULL) +
  theme_minimal() +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none")

p4 <- ggplot() + 
  geom_bar(data = models, aes(x = factor(compartmental_type, levels = c( "SIS",
                                                                         "SIR",
                                                                        "SEIR",
                                                                        "Other compartmental",
                                                                        "Not compartmental",
                                                                        "Unspecified")), fill = model_type), color = "black") + 
  scale_x_discrete(labels = c("Other compartmental" = "Other")) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,by = 5), expand = c(0,0)) +
  xlab("Human Compartments") + ylab("Model Count") +
  scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3", 'IBM' = 'steelblue' , 'Unspecified' = 'grey'), name = NULL) +
  theme_minimal() +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none")

p5 <- ggplot() + 
  geom_bar(data = models, aes(x = factor(theoretical_model, levels = c("FALSE",
                                                                       "TRUE")), fill = model_type), color = "black") +
  scale_x_discrete(labels = c("FALSE" = "Fitted to Data",
                              "TRUE" = "Theoretical")) +
  scale_y_continuous(limits = c(0,60), breaks = seq(0,60,by = 5), expand = c(0,0)) +
  xlab("Model Calibration") + ylab("Model Count") +
  scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3", 'IBM' = 'steelblue' , 'Unspecified' = 'grey'), name = NULL) +
  theme_minimal() +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none")

p6 <- ggplot() + 
  geom_bar(data = models %>% separate_rows(interventions_type, sep = ";"), 
           aes(x = factor(interventions_type, levels = c("Vector/Animal control","Behaviour changes","Quarantine",
                                                         "Contact tracing","Treatment","Hospitals",
                                                         "Vaccination","Other")), fill = model_type), color = "black") +
  scale_x_discrete(labels = c("Behaviour changes" = "Behaviour\nChanges",
                              "Contact tracing" = "Contact\nTracing",
                              "Other" = "Other &\nUnspecified")) +
  scale_y_continuous(limits = c(0,80), breaks = seq(0,80,by = 5), expand = c(0,0)) +
  xlab("Interventions") + ylab("Model Count") +
  scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3", 'IBM' = 'steelblue' , 'Unspecified' = 'grey'), name = NULL) +
  theme_minimal() +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none")

patchwork <- (p1 + p2 + p3 + p4 + p5 + p6) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_S2.png", plot = patchwork, width = 12, height = 16)

################
## PARAMETERS ##
################

parameters <- parameters %>% mutate(parameter_class = case_when(
  parameter_class == "Reproduction number" ~ "Reproduction Numbers",
  parameter_class %in% c("Relative contribution","Attack rate","Growth rate") ~ "Other Transmission Parameters",
  parameter_class == "Human delay" ~ "Delays",
  parameter_class == "Risk factors" ~ "Risk Factors",
  TRUE ~ parameter_class),
  parameter_type = case_when(
    parameter_type == "Attack rate" ~ "Attack Rate",  
    parameter_type == "Growth rate (r)" ~ "Growth Rate",  
    parameter_type == "Human delay - admission to care>death" ~ "Time Admission to Outcome",  
    parameter_type == "Human delay - admission to care>discharge/recovery" ~ "Time Admission to Outcome",  
    parameter_type == "Human delay - incubation period" ~ "Incubation Period",  
    parameter_type == "Human delay - other human delay (go to section)" ~ "Other Delay",  
    parameter_type == "Human delay - symptom onset>admission to care" ~ "Time Onset to Admission",  
    parameter_type == "Human delay - symptom onset>death" ~ "Time Onset to Outcome",  
    parameter_type == "Human delay - symptom onset>discharge/recovery" ~ "Time Onset to Outcome",  
    parameter_type == "Human delay - time in care (length of stay)" ~ "Time Onset to Outcome",  
    parameter_type == "Mutations - evolutionary rate" ~ "Evolutionary Rate",  
    parameter_type == "Mutations – substitution rate" ~ "Substitution Rate",  
    parameter_type == "Relative contribution - human to human" ~ "Relative Transmission Contribution",  
    parameter_type == "Reproduction number (Basic R0)" ~ "Basic (R0)",  
    parameter_type == "Reproduction number (Effective, Re)" ~ "Effective (Re)",  
    parameter_type == "Risk factors" ~ "Risk Factors",  
    parameter_type == "Seroprevalence - IFA" ~ "IFA",  
    parameter_type == "Seroprevalence - IgG" ~ "IgG", 
    parameter_type == "Seroprevalence - IgM" ~ "IgM",  
    parameter_type == "Seroprevalence - PRNT" ~ "PRNT",  
    parameter_type == "Seroprevalence - Unspecified" ~ "Unspecified",  
    parameter_type == "Severity - case fatality rate (CFR)" ~ "Case Fatality Ratio (CFR)",  
    TRUE ~ parameter_type),
  population_country = ifelse(is.na(population_country),"Unspecified",population_country), 
  study_midyear = ifelse(!is.na(population_study_start_year) & !is.na(population_study_end_year),
                         round((population_study_start_year + population_study_end_year) / 2),
                         population_study_start_year),
  study_midyear_cat = case_when(
    study_midyear %in% 2000:2004 ~ "2000-2004",
    study_midyear %in% 2005:2009 ~ "2005-2009",
    study_midyear %in% 2010:2014 ~ "2010-2014",
    study_midyear %in% 2015:2019 ~ "2015-2019",
    study_midyear %in% 2020:2029 ~ "2020-Present",
    TRUE ~ "Unspecified"),
  population_sample_type = case_when(
    population_sample_type == "Mixed settings" ~ "Mixed Settings",   
    population_sample_type == "Trade / business based" ~ "Trade/Business-Based",
    is.na(population_sample_type) ~ "Unspecified",
    TRUE ~ str_replace_all(population_sample_type, " based", "-Based"))) %>%
  mutate(parameter_type = factor(parameter_type, levels = unique(parameter_type[order(parameter_class,parameter_type)])))

p1 <- ggplot() + 
  geom_bar(data = parameters,
           aes(x = reorder(fct_infreq(parameter_type), parameter_class), fill = parameter_class), color = "black") + 
  scale_x_discrete(limits = rev) + 
  scale_y_break(c(125,240), scales = 0.1) +
  scale_y_continuous(limits = c(0,270), breaks = seq(0,270,by = 30), expand = c(0,0)) +
  xlab("Parameter Type") + ylab("Parameter Count") +
  scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
  theme_minimal() +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = 'top' ) +
        #legend.position = c(1,1), legend.justification = c(1,1), legend.box.just = "left") +
  coord_flip()

p2 <- ggplot() + 
  geom_bar(data = parameters %>% separate_rows(population_country, sep = ";") %>%
             mutate(population_country = str_trim(population_country)),           
           aes(x = fct_infreq(population_country), fill = parameter_class), color = "black") + 
  scale_x_discrete(limits = rev) + 
  scale_y_continuous(limits = c(0,180), breaks = seq(0,180,by = 30), expand = c(0,0)) +
  xlab("Study Country") + ylab("Parameter Count") +
  scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
  theme_minimal() +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = 'none') +
        #legend.position = 'bottom', legend.justification = c(1,1), legend.box.just = "left") +
  coord_flip()

p3 <- ggplot() + 
  geom_bar(data = parameters, aes(x = factor(study_midyear_cat, levels = c("2000-2004", "2005-2009", "2010-2014",
                                                                           "2015-2019", "2020-Present",'Unspecified')), fill = parameter_class), color = "black") + 
  scale_x_discrete(limits = rev) + 
  scale_y_continuous(limits = c(0,180), breaks = seq(0,180,by = 30), expand = c(0,0)) +
  xlab("Study Year") + ylab("Parameter Count") +
  scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
  theme_minimal() +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none") +
  coord_flip()

p4 <- ggplot() + 
  geom_bar(data = parameters, aes(x = fct_infreq(population_sample_type), fill = parameter_class), color = "black") + 
  scale_x_discrete(limits = rev) + 
  scale_y_continuous(limits = c(0,300), breaks = seq(0,300,by = 30), expand = c(0,0)) +
  xlab("Study Setting") + ylab("Parameter Count") +
  scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
  theme_minimal() +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none") +
  coord_flip()

patchwork <- p1 / p2 / p3 / p4 + plot_layout(ncol = 1, 
                                             heights = c(2.5,2,0.5,0.7), 
                                             guides = "collect") + plot_annotation(tag_levels = 'A')
dev.set(dev.next())
ggsave("figure_S3.png", plot = patchwork, width = 14, height = 18)

dev.set(dev.next())