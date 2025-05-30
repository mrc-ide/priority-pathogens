# Task to make summary figures and tables 

library(dplyr) 
library(stringr)
library(tibble)
library(purrr)
library(ggbreak)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("zika_prep_data", "latest(parameter:pathogen == this:pathogen &&
                   parameter:plotting == TRUE)",
                   c("articles_curated.rds", "outbreaks_curated.rds", "models_curated.rds", "parameters_curated.rds"))

text_size = 14
##################
## DATA CURATION ##
###################

articles   <- readRDS("articles_curated.rds")
models     <- readRDS("models_curated.rds")
parameters <- readRDS("parameters_curated.rds")
outbreaks <- readRDS('outbreaks_curated.rds')


## Articles ----
quality <- articles %>%
  mutate(across(starts_with("qa"), ~as.numeric(replace(replace(., . == "Yes", 1), . == "No", 0))))

quality <- quality %>% rowwise() %>% 
  mutate(score = 100*qa_score)
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
  scale_x_continuous(limits = c(1970,2025), breaks = seq(1970, 2023, by = 5), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0,95), expand = c(0, 0)) +
  theme_minimal(base_size = text_size) + 
  theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA)) + 
  labs(x = "Year of Publication", y = "Article Count")

p2 <- ggplot() +
  geom_histogram(data=quality, aes(x = score), binwidth = 20, boundary = 0, fill = "steelblue4", color = "black", alpha = 0.7) +
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20)) + 
  scale_y_continuous(limits = c(0,210), expand = c(0, 0)) +
  theme_minimal(base_size = text_size) + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA)) + 
  labs(x = "Quality Assessment Score (%)", y = "Article Count")

p3 <- ggplot() +
  geom_point(data = quality, aes(x=year_publication, y=score,color=category)) +
  geom_smooth(data = subset(quality, category == "Non-Modelling Studies"), aes(x=year_publication,y=score), span = 2, color = "springgreen3", fill = "springgreen3") +
  geom_smooth(data = subset(quality, category == "Modelling Studies"), aes(x=year_publication,y=score), span = 2, color = "red", fill = "red") +
  scale_x_continuous(limits = c(1969,2025), breaks = seq(1970, 2024, by=5), expand = c(0, 0.5)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20)) +
  xlab("Year of Publication") + ylab("Quality Assessment Score (%)") +  
  scale_color_manual(values = c("Non-Modelling Studies" = "springgreen3","Modelling Studies" = "red"), name = NULL) +
  theme_minimal(base_size = text_size) +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = 'bottom')

p4 <- answers %>%
  group_by(Question,Assessment) %>% summarize(count=n()) %>% ungroup() %>%
  ggplot(aes(fill=Assessment, y=count, x=Question)) + 
  geom_bar(position="stack", stat="identity") + theme_bw() +
  scale_y_continuous(limits = c(0,580), breaks = seq(0,580,by=40), expand = c(0, 0)) +
  scale_fill_manual(values = c("darkolivegreen2","coral1","grey70"),aesthetics = "fill",name="",breaks=c('Yes', 'No','NA')) +
  xlab("") + ylab("Article Count") + 
  coord_flip() +
  theme_minimal(base_size = text_size) + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA)) +
  theme(legend.position = 'bottom')

patchwork <- (p1 + p2 + p3 + p4) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_qa_articles.png", plot = patchwork, width = 18, height = 12)


############
## MODELS ##
############

models <- models %>% mutate(stoch_deter = replace_na(stoch_deter,'Unspecified'),
                            assumptions = replace_na(assumptions,'Unspecified'),
                            model_type = replace_na(model_type, 'Unspecified'),
                            compartmental_type = replace_na(compartmental_type,'Unspecified'),
                            interventions_type = replace_na(interventions_type,'None'),
                            transmission_route = replace_na(transmission_route,'Unspecified')) %>%
  mutate(model_type = str_replace_all(model_type,"Branching process","Branching Process"),
         # assumptions = str_replace_all(assumptions,";Latent period is same as incubation period",""),
         # compartmental_type = str_replace_all(compartmental_type,";SIR",""),
         interventions_type = str_replace_all(interventions_type,"Unspecified","Other"),
         stoch_deter = str_replace(stoch_deter,'Deterministic;Stochastic','Other')) %>%
  mutate(model_type = case_when(model_type=="Agent / Individual based" ~ 'IBM',
                                model_type == 'Branching Process' ~ "Branching\nProcess",
                                model_type%in%c("Agent / Individual based;Branching Process","Compartmental;Other",
                                                'Agent / Individual based;Compartmental','Branching Process;Compartmental') ~ 'Multiple\ntypes',
                                TRUE ~ model_type),
         compartmental_type = case_when(compartmental_type %in% c("Other compartmental;SEIR", "SIR;SIS","SEIR;SIR","SAIR-SEI",
                                                                  "SIRS-SI","SLIR-SLI","SEIR-SI", "SEI-SI"
         ) ~ 'Other compartmental',
         compartmental_type %in% c("Not compartmental;SEIR-SEI", "Other compartmental;SEIR-SEI") ~ "SEIR-SEI",
         compartmental_type == 'Other compartmental;SIR' ~ "SIR",
         TRUE ~ compartmental_type),
         assumptions = ifelse(grepl(";", assumptions), "Several assumptions", assumptions))

p1 <- ggplot() + 
  geom_bar(data = models %>%
             mutate(model_type = fct_infreq(model_type)), 
           aes(x = model_type, fill = stoch_deter), color = "black") + 
  scale_y_continuous(limits = c(0,40), breaks = seq(0,40,by = 10), expand = c(0,0)) +
  xlab("Model Type") + ylab("Model Count") +
  scale_fill_manual(values = c("Deterministic" = "steelblue4","Stochastic" = "red", 'Unspecified' = 'grey40', 'Other'='grey80'), name = NULL) +
  theme_minimal(base_size = text_size) +      
  theme(panel.border = element_rect(color = "black", linewidth =  1.25, fill = NA),
        legend.position = 'inside',
        legend.position.inside = c(1, 1), legend.justification = c(1,1), legend.box.just = "left")
# p1
p2 <- ggplot() + 
  geom_bar(data = models %>%
             mutate(transmission_route = factor(transmission_route, levels = c("Human to human (direct contact)", 
                                                                               "Human to human (direct contact);Sexual;Vector/Animal to human",
                                                                               "Human to human (direct contact);Vector/Animal to human",
                                                                               "Sexual;Vector/Animal to human",
                                                                               "Unspecified;Vector/Animal to human",
                                                                               "Sexual", "Vector/Animal to human",
                                                                               "Unspecified"))) %>%
             mutate(transmission_route = fct_infreq(transmission_route)),
           aes(x = transmission_route, fill = model_type), color = "black") + 
  scale_x_discrete(labels = c("Human to human (direct contact)" = "Human to human",
                              "Vector/Animal to human" = "Vector to human",
                              "Human to human (direct contact);Sexual;Vector/Animal to human" = 'Sexual and\nvector to human',
                              "Sexual;Vector/Animal to human" = 'Sexual and\nvector to human',
                              "Human to human (direct contact);Vector/Animal to human" = 'Human to human\nand\nvector to human',
                              "Unspecified;Vector/Animal to human" = "Vector to human")) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,by = 10), expand = c(0,0)) +
  xlab("Transmission Route(s)") + ylab("Model Count") +
  scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3", 'IBM' = 'steelblue' , 'Unspecified' = 'grey'), name = NULL) +
  theme_minimal(base_size = text_size) +   
  coord_flip() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = 'inside',
        legend.position.inside = c(1,1), legend.justification = c(1,1), legend.box.just = "left")
# p2
p3 <- ggplot() + 
  geom_bar(data = models %>%
             mutate(assumptions = factor(assumptions, levels = c("Homogeneous mixing",
                                                                 "Heterogenity in transmission rates - over time",
                                                                 "Heterogenity in transmission rates - between human groups",
                                                                 "Heterogenity in transmission rates - between human and vector",
                                                                 "Latent period is same as incubation period",
                                                                 "Cross-immunity between Zika and dengue",
                                                                 "Age dependent susceptibility","Several assumptions",
                                                                 'Other', 'Unspecified'))) %>%
             mutate(assumptions = fct_infreq(assumptions)),
           aes(x = assumptions,
               fill = model_type), color = "black") +
  scale_x_discrete(labels = c("Homogeneous mixing" = "Homogeneous",
                              "Heterogenity in transmission rates - over time" = "Time-Heterogeneous",
                              "Heterogenity in transmission rates - between human and vector" = "Human-to-Vector-\nHeterogeneous",
                              "Age dependent susceptibility" = "Age-\nHeterongeneous",
                              "Latent period is same as incubation period" = "Latent same as\nincubation period",
                              "Cross-immunity between Zika and dengue" = "Cross-immunity\nwith dengue",
                              "Heterogenity in transmission rates - between human groups" = "Human-to-Human-\nHeterogeneous",
                              "Several assumptions" = "Several\nassumptions",
                              "Other" = "Other",
                              "Unspecified" = "Unspecified")) +
  scale_y_continuous(limits = c(0,102), breaks = seq(0,102,by = 10), expand = c(0,0)) +
  xlab("Human Transmission Heterogeneity") + ylab("Model Count") +
  scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3", 'IBM' = 'steelblue' , 'Unspecified' = 'grey'), name = NULL) +
  theme_minimal(base_size = text_size) +      
  coord_flip() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        # axis.text.x = element_text(angle = 45),
        legend.position = "none")
# p3
p4 <- ggplot() + 
  geom_bar(data = models %>%
             mutate(compartmental_type = factor(compartmental_type, levels = c( "SEIR-SEI","Other SEIR-SEI",
                                                                                "SIR","SIR-SI",
                                                                                "SEIR","SI-SI","SIR-SEI",
                                                                                "Other compartmental",
                                                                                "Not compartmental",
                                                                                "Unspecified"))) %>%
             mutate(compartmental_type = fct_infreq(compartmental_type)),  # Reorder by frequency (descending), 
           aes(x = compartmental_type, fill = model_type), color = "black") + 
  # scale_x_discrete(labels = c("Other compartmental" = "Other\ncompart-\nmental",
  #                             'Not compartmental' = "Not\ncompart-\nmental")) +
  scale_y_continuous(limits = c(0,70), breaks = seq(0,70,by = 10), expand = c(0,0)) +
  xlab("Human Compartments") + ylab("Model Count") +
  coord_flip() +
  scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3", 'IBM' = 'steelblue' , 'Unspecified' = 'grey'), name = NULL) +
  theme_minimal(base_size = text_size) +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none")
# p4
p5 <- ggplot() + 
  geom_bar(data = models, aes(x = factor(theoretical_model, levels = c("FALSE",
                                                                       "TRUE")), fill = model_type), color = "black") +
  scale_x_discrete(labels = c("FALSE" = "Fitted to Data",
                              "TRUE" = "Theoretical")) +
  scale_y_continuous(limits = c(0,130), breaks = seq(0,130,by = 10), expand = c(0,0)) +
  xlab("Model Calibration") + ylab("Model Count") +
  scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3", 'IBM' = 'steelblue' , 'Unspecified' = 'grey'), name = NULL) +
  theme_minimal(base_size = text_size) +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none")
# p5
p6 <- ggplot() + 
  geom_bar(data = models %>% separate_rows(interventions_type, sep = ";") %>%
             mutate(interventions_type = factor(interventions_type, levels = c("Vector/Animal control","Behaviour changes","Quarantine",
                                                                               "Indoor residual spraying", "Pesticides/larvicides", 
                                                                               "Insecticide-treated nets","Wolbachia replacement",
                                                                               "Wolbachia suppression","Genetically modified mosquitoes",
                                                                               "Mechanical removal of breeding sites ",
                                                                               "Contact tracing","Treatment","Hospitals",
                                                                               "Vaccination","Other", "None"))) %>%
             mutate(interventions_type = fct_infreq(interventions_type)),  # Reorder by frequency (descending), 
           aes(x = interventions_type, fill = model_type), color = "black") +
  # scale_x_discrete(labels = c("Behaviour changes" = "Behaviour\nChanges",
  #                             "Contact tracing" = "Contact\nTracing",
  #                             "Vector/Animal control" = "Vector\nControl",
  #                             'Indoor residual spraying' = "Indoor\nResidual\nSpraying",
  #                             'Pesticides/larvicides' = 'Pesticides\n/Larvicides',
  #                             "Insecticide-treated nets" = "Insecticide-\ntreated Nets",
  #                             "Wolbachia replacement" = "Wolbachia\nReplacement",
  #                             "Wolbachia suppression" = "Wolbachia\nSuppression",
  #                             "Genetically modified mosquitoes" = "Genetically\nmodified\nmosquitoes",
  #                             "Mechanical removal of breeding sites " = "Mechanical\nremoval of\nbreeding sites",
  #                             "Other" = "Other",
  #                             "None" = 'None')) +
  # scale_y_break(c(50,125), scales = 0.1) +
  scale_y_continuous(limits = c(0,140), breaks = seq(0,190,by = 10), expand = c(0,0)) +
  xlab("Interventions") + ylab("Model Count") +
  scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3", 'IBM' = 'steelblue' , 'Unspecified' = 'grey'), name = NULL) +
  theme_minimal(base_size = text_size) +   
  coord_flip() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        # axis.text.x = element_text(angle = 45),
        legend.position = "none")
# p6

layout <- "
AABB
CCDD
EEFF"
patchwork <- p1 + p2 + p3 + p4 + p5 + p6 + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')
ggsave("models_summary.png", plot = patchwork, width = 18, height = 15)

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
    parameter_type == "Human delay - serial interval" ~ "Serial Interval",
    parameter_type %in% c('Human delay - infectious period (inverse parameter)', "Human delay - infectious period") ~ "Infectious Period",
    parameter_type == "Human delay - admission to care>discharge/recovery" ~ "Time Admission to Outcome",  
    parameter_type %in% c("Human delay - incubation period","Human delay - incubation period (inverse parameter)") ~ "Incubation Period",  
    parameter_type == "Human delay - other human delay (go to section)" ~ "Other Delay",  
    parameter_type %in% c("Human delay - Symptom Onset/Fever to Admission to care", "Human delay - symptom onset to admission to care"
    )~ "Time Onset to Admission", 
    grepl('Symptom Onset/Fever to', parameter_type) ~ "Symptom Onset to Outcome",  
    grepl("Human delay", parameter_type ) ~ "Other human delay",
    parameter_type %in% c("Mosquito delay - extrinsic incubation period","Mosquito delay - extrinsic incubation period (EIP10)",
                          "Mosquito delay - extrinsic incubation period (inverse parameter)") ~ "Extrinsic Incubation Period",
    parameter_type == "Mutations - evolutionary rate" ~ "Evolutionary Rate",  
    parameter_type %in% c("Mutations – substitution rate","Mutations - substitution rate") ~ "Substitution Rate",
    parameter_type == "Mutations – mutation rate" ~ "Mutation Rate",  
    parameter_type == "Relative contribution - human to human" ~ "Relative Transmission Contribution",  
    parameter_type == "Relative contribution - vector to human" ~ "Relative Transmission Contribution",  
    parameter_type %in% c("Reproduction number (Basic R0)", "Reproduction number (Basic R0) - Human", "Reproduction number (Basic R0) - Mosquito"
    ) ~ "Basic (R0)",  
    parameter_type %in% c("Reproduction number (Effective, Re)", "Reproduction number (Effective; Re) - Mosquito", "Reproduction number (Effective; Re) - Human"
    )  ~ "Effective (Re)",  
    grepl("Mosquito delay", parameter_type) ~ "EIP", 
    parameter_type == "Risk factors" ~ "Risk Factors",  
    parameter_type == "Seroprevalence - IgG" ~ "IgG", 
    parameter_type == "Seroprevalence - IgM" ~ "IgM",  
    parameter_type == "Seroprevalence - Neutralisation/PRNT" ~ "PRNT",  
    parameter_type == "Seroprevalence - Unspecified" ~ "Unspecified seroprevalence",
    parameter_type %in% c("Seroprevalence - IFA", "Seroprevalence - Biotinylated-EDIII antigen capture ELISA", "Seroprevalence - HAI/HI","Seroprevalence - MIA",
                          "Seroprevalence - IgG and IgM","Seroprevalence - NS1 BOB ELISA","Seroprevalence - Western blot") ~ "Other seroprevalence",
    parameter_type == "Severity - case fatality rate (CFR)" ~ "Case Fatality Ratio (CFR)",  
    parameter_type == "Zika congenital syndrome (microcephaly) risk" ~ 'Zika Congenital Syndrome (microcephaly) Risk',
    parameter_type == 'Miscarriage rate' ~ "Miscarriage Rate",
    TRUE ~ parameter_type),
  population_country = ifelse(is.na(population_country),"Unspecified",population_country), 
  study_midyear = ifelse(!is.na(population_study_start_year) & !is.na(population_study_end_year),
                         round((population_study_start_year + population_study_end_year) / 2),
                         population_study_start_year),
  study_midyear_cat = case_when(
    study_midyear %in% 1965:1999 ~ "pre-2000",
    study_midyear %in% 2000:2004 ~ "2000-2004",
    study_midyear %in% 2005:2009 ~ "2005-2009",
    study_midyear %in% 2010:2014 ~ "2010-2014",
    study_midyear %in% 2015:2019 ~ "2015-2019",
    study_midyear %in% 2020:2029 ~ "2020-Present",
    TRUE ~ "Unspecified"),
  population_sample_type = case_when(
    population_sample_type == "Mixed settings" ~ "Mixed Settings",   
    is.na(population_sample_type) ~ "Unspecified",
    TRUE ~ str_replace_all(population_sample_type, " based", "-Based"))) %>%
  mutate(parameter_type = factor(parameter_type, levels = unique(parameter_type[order(parameter_class,parameter_type)]))) %>%
  mutate(population_country_original = str_replace_all(population_country_original, ";", ", ")) 

p1 <- ggplot() + 
  geom_bar(data = parameters,
           aes(x = reorder(fct_infreq(parameter_type), parameter_class), fill = parameter_class), color = "black") + 
  scale_x_discrete(limits = rev) + 
  # scale_y_break(c(125,240), scales = 0.1) +
  scale_y_continuous(limits = c(0,300), breaks = seq(0,270,by = 30), expand = c(0,0)) +
  xlab("Parameter Type") + ylab("Parameter Count") +
  scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
  theme_minimal(base_size = text_size) +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = 'top' ) +
  #legend.position = c(1,1), legend.justification = c(1,1), legend.box.just = "left") +
  coord_flip()
# p1
p2a <- ggplot() + 
  geom_bar(data = parameters %>%
             separate_rows(population_country_original, sep = ", ") %>%
             mutate(population_country_original = str_trim(population_country_original)) %>%
             mutate(population_country_original = case_when(
               population_country_original %in% c("1966-2016)","2007-2016) and the Americas (n = 22","2014-2015)",
                                                  "Multi-country: Americas","Multi-country: Pacific","The","Pacific (n = 23",
                                                  "Southeastern Asia (n = 20","other Americas") ~ "Other",
               is.na(population_country_original) ~ "Unspecified",
               TRUE ~ population_country_original
             )) %>%
             mutate(population_country_original = fct_lump_min(population_country_original, 
                                                               min = 3, w = NULL, other_level = "Other")) %>%
             mutate(country_brazil = ifelse(population_country_original == 'Brazil', 'Brazil','Rest of World')) %>%
             filter(country_brazil == 'Brazil'),           
           aes(x = fct_infreq(population_country_original), fill = parameter_class), color = "black") + 
  scale_x_discrete(limits = rev) + 
  ggforce::facet_col(vars(country_brazil), scales = 'free', space = 'free') +
  scale_y_continuous(limits = c(0,330), breaks = seq(0,330,by = 30), expand = c(0,0)) +
  xlab("Study Country") + ylab("Parameter Count") +
  scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
  theme_minimal(base_size = text_size) +      
  theme(panel.border = element_blank(),
        legend.position = 'none',
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  #legend.position = 'bottom', legend.justification = c(1,1), legend.box.just = "left") +
  coord_flip()
p2b <- ggplot() + 
  geom_bar(data = parameters %>%
             separate_rows(population_country_original, sep = ", ") %>%
             mutate(population_country_original = str_trim(population_country_original)) %>%
             mutate(population_country_original = case_when(
               population_country_original %in% c("1966-2016)","2007-2016) and the Americas (n = 22","2014-2015)",
                                                  "Multi-country: Americas","Multi-country: Pacific","The","Pacific (n = 23",
                                                  "Southeastern Asia (n = 20","other Americas") ~ "Other",
               is.na(population_country_original) ~ "Unspecified",
               TRUE ~ population_country_original
             )) %>%
             mutate(population_country_original = fct_lump_min(population_country_original, 
                                                               min = 3, w = NULL, other_level = "Other")) %>%
             mutate(country_brazil = ifelse(population_country_original == 'Brazil', 'Brazil','Rest of World')) %>%
             filter(country_brazil != 'Brazil'),           
           aes(x = fct_infreq(population_country_original), fill = parameter_class), color = "black") + 
  scale_x_discrete(limits = rev) + 
  ggforce::facet_col(vars(country_brazil), scales = 'free', space = 'free') +
  scale_y_continuous(limits = c(0,140), breaks = seq(0,140,by = 30), expand = c(0,0)) +
  xlab("Study Country") + ylab("Parameter Count") +
  scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
  theme_minimal(base_size = text_size) +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = 'none',
        strip.text = element_blank()) +
  #legend.position = 'bottom', legend.justification = c(1,1), legend.box.just = "left") +
  coord_flip()
p2 <- p2a / p2b + plot_layout(heights = c(0.1, 6.5))
p2
p3 <- ggplot() + 
  geom_bar(data = parameters, aes(x = factor(study_midyear_cat, levels = c("pre-2000", "2000-2004", "2005-2009", "2010-2014",
                                                                           "2015-2019", "2020-Present",'Unspecified')), fill = parameter_class), color = "black") + 
  scale_x_discrete(limits = rev) + 
  scale_y_continuous(limits = c(0,180), breaks = seq(0,180,by = 30), expand = c(0,0)) +
  xlab("Study Year") + ylab("Parameter Count") +
  scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
  theme_minimal(base_size = text_size) +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none") +
  coord_flip()
# p3
p4 <- ggplot() + 
  geom_bar(data = parameters, aes(x = fct_infreq(population_sample_type), fill = parameter_class), color = "black") + 
  scale_x_discrete(limits = rev) + 
  scale_y_continuous(limits = c(0,300), breaks = seq(0,300,by = 30), expand = c(0,0)) +
  xlab("Study Setting") + ylab("Parameter Count") +
  scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
  theme_minimal(base_size = text_size) +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none") +
  coord_flip()
# p4
layout <- "
AAABB
AAACC
AAADD"
p2 <- p2a / p2b + plot_layout(heights = c(0.1, 6.5))
patchwork <- (p2 | (p1 / p3 / p4)) + plot_layout(#heights = c(1,1,1), 
  guides = 'collect') + plot_annotation(tag_levels = 'A') &
  theme(legend.position='bottom')
# patchwork <- p2 + p1 + p3 + p4 + plot_layout(design = layout, 
#                                              # ncol = 1, 
#                                              # heights = c(2,4,0.5,0.7), 
#                                              guides = "collect") + plot_annotation(tag_levels = 'A') &
# theme(legend.position='bottom')
dev.set(dev.next())
ggsave("parameters_summary.png", plot = patchwork, width = 16, height = 16)

