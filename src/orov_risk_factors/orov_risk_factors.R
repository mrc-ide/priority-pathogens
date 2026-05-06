# OROV risk factors

library(tidyverse)
library(forcats)
library(orderly2)

orderly_strict_mode()
orderly2::orderly_parameters(pathogen = "OROV")

orderly2::orderly_artefact(description = "inputs folder",
                           files = "inputs/")

# did not like latest(parameter:pathogen == this:pathogen) - fix in future - seems to have resolved itself for now...
orderly_dependency(
  name = "db_compilation_orov",
  query = "latest(parameter:pathogen == this:pathogen)",
  files = c("inputs/articles.csv"="articles.csv",
            "inputs/parameters.csv"="parameters.csv",
            "inputs/outbreaks.csv"="outbreaks.csv"))


# read in data
articles <- read.csv("inputs/articles.csv")
outbreaks <- read.csv("inputs/outbreaks.csv")
parameters <- read.csv("inputs/parameters.csv")

## risk factors
# parameters %>% filter(parameter_type_broad=="Risk factors") %>% View()
# parameters %>% filter(parameter_type_broad=="Risk factors") %>% nrow()
# parameters %>% filter(parameter_type_broad=="Risk factors") %>% select(parameter_context_human) %>% unique()
# parameters %>% filter(parameter_type_broad=="Risk factors") %>% select(population_country) %>% unique()
# parameters %>% filter(parameter_type_broad=="Risk factors") %>% select(riskfactor_adjusted) %>% mutate(riskfactor_adjusted=factor(riskfactor_adjusted)) %>% summary()
# parameters %>% filter(parameter_type_broad=="Risk factors") %>% select(riskfactor_outcome) %>% mutate(riskfactor_outcome = factor(riskfactor_outcome)) %>% summary()

risk_params <- parameters %>%
  filter(grepl("Risk factors", parameter_type, ignore.case = TRUE)) %>%
  select(covidence_id,access_param_id,riskfactor_outcome,
         riskfactor_name,	riskfactor_occupation, riskfactor_significant,
         riskfactor_adjusted, population_sample_size,
         population_country, #dates,
         population_sample_type, population_group,parameter_context_human,
        article_qa_score,population_study_start_year,
        population_study_end_year,
        article_label,parameter_notes)

risk_params <- merge(risk_params,
  articles %>% select(covidence_id,year_publication),
  by="covidence_id") 
# write.csv(risk_params,row.names = FALSE,"risk_factors_for_writing.csv")

# what is happening in terms of number of obs per paper
risk_params %>% group_by(covidence_id) %>% 
  summarise(n = length(covidence_id)) %>% 
  arrange(-n)

## risk factor specific categorisations can go here
risk_params <- risk_params %>% mutate(riskfactor_name = case_when(
  covidence_id==9~"Environmental factors",
  covidence_id==28&riskfactor_name=="Age,Sex,Other"~"Age,Sex,Symptoms",
  covidence_id==28&riskfactor_name=="Other"~"Symptoms",
  covidence_id==42&riskfactor_name=="Other"~"Symptoms",
  covidence_id==42&riskfactor_name=="Sex,Other"~"Sex,Time,Symptoms,Arbovirus co-infection",
  covidence_id==42&riskfactor_name=="Age,Other"~"Age,Location,Symptoms,Non-arbovirus co-infection",
  covidence_id==47&riskfactor_name=="Age,Other"~"Age,Time",
  covidence_id==64 ~ "Environmental factors",
  covidence_id==128&riskfactor_name=="Sex,Other"~"Sex,Location",
  covidence_id==136&riskfactor_name=="Age,Occupation,Other"~"Age,Occupation,Location",
  covidence_id==136&riskfactor_adjusted=="Not adjusted"&riskfactor_significant=="Significant" ~ "Race,Other",
  covidence_id==167&riskfactor_significant=="Not significant"&riskfactor_adjusted=="Not adjusted"~"Sex,Agricultural activities,Housing type,Other",
  covidence_id==167&riskfactor_significant=="Significant"&riskfactor_adjusted=="Not adjusted"~"Housing type,Travel,Other",
  covidence_id==167&riskfactor_significant=="Significant"&riskfactor_adjusted=="Adjusted"~"Age,Travel,Other",
  covidence_id==184&riskfactor_significant=="Not significant"&riskfactor_adjusted=="Not adjusted"~ "Location",
  covidence_id==231 ~ "Location",
  # covidence_id==179&riskfactor_adjusted=="Adjusted"&riskfactor_significant=="Significant" ~ "Age,Sex,Location,Animal contact",
  # covidence_id==179&riskfactor_adjusted=="Not adjusted"&riskfactor_significant=="Significant"~"Age,Sex,Location",
  # covidence_id==179&riskfactor_adjusted=="Not adjusted"&riskfactor_significant=="Not significant" ~ "Occupation,Animal contact,Other",
  covidence_id==521~"Environmental factors",
  covidence_id==525 ~ "Agricultural activities",
  covidence_id==580 ~ "Location,Time",
  covidence_id==665&population_country=="Peru"&riskfactor_name=="Other"~"Location,Time",
  covidence_id==665&riskfactor_outcome=="Serology"&riskfactor_adjusted=="Not adjusted"&riskfactor_significant=="Significant"&riskfactor_name=="Other"~"Location",
  covidence_id==665&riskfactor_outcome=="Serology"&riskfactor_significant=="Unspecified"&riskfactor_name=="Other"~"Environmental factors",
  covidence_id==665&riskfactor_outcome=="Serology"&riskfactor_significant=="Significant"&riskfactor_name=="Other"~"Environmental factors,Other",
  covidence_id==665&riskfactor_outcome=="Presence of Oropouche"&riskfactor_significant=="Unspecified"&riskfactor_name=="Other"~"Location,Environmental factors",
  covidence_id==665&riskfactor_outcome=="Presence of Oropouche"&riskfactor_significant=="Significant"&riskfactor_name=="Other"~"Detection of vectors",
  covidence_id==681~"Environmental factors",
  covidence_id==690~"Location,Time",
  covidence_id==714&riskfactor_significant=="Significant" ~ "Symptoms,Animal/vector contact,Travel,Other",
  covidence_id==714&riskfactor_significant=="Not significant" ~ "Symptoms,Animal/vector contact,Comorbidities,Housing type,Other",
  .default = riskfactor_name
)
)


# risk_table <- risk_params %>%
#   separate_longer_delim(riskfactor_name, delim = ";") %>% 
#   mutate(population_sample_size = replace_na(as.double(population_sample_size),0),
#          riskfactor_adjusted    = case_when(riskfactor_adjusted=='' ~ 'Unspecified',
#                                             TRUE ~ riskfactor_adjusted)) %>%
#   group_by(riskfactor_outcome,riskfactor_name,riskfactor_significant,riskfactor_adjusted,
#            parameter_context_human) %>%
#   summarise(n=n(),
#             pop_size = sum(population_sample_size)) %>%
#   unite(`Significant / Adjusted`,riskfactor_significant:riskfactor_adjusted, remove = FALSE, sep = " / ") %>% arrange(-n)

#text_size <- 20
custom_colours <- c('Significant / Adjusted'='blue4', 'Significant / Not adjusted' = 'lightblue', 'Significant / Unspecified'='blue',
                    'Not significant / Adjusted'='darkred', 'Not significant / Not adjusted' = 'pink', 'Not significant / Unspecified'='red',
                    'Unspecified / Adjusted'='grey70', 'Unspecified / Not adjusted' = 'grey50', 'Unspecified / Unspecified'='grey30')



# # Infection
# risk_table_plt_infection <- risk_table %>% filter(riskfactor_outcome=='Infection') %>%
#   ggplot(aes(x=riskfactor_name,y=n,col=`Significant / Adjusted`, fill=`Significant / Adjusted`)) + 
#   geom_bar( stat='identity',position = position_dodge(preserve = "single")) +
#   scale_color_manual(values = custom_colours) +
#   scale_fill_manual(values = custom_colours) + xlab('') + ylab('') + theme_light() + 
#   theme( axis.text.x = element_text( angle = 25, hjust = 1, size = text_size ),
#          strip.text = element_text( color = "black"),          
#          strip.background =element_rect(fill="grey90"),
#          text = element_text(size = text_size),
#          legend.position = 'none')
# 
# 
# # Serology
# risk_table_plt_serology <- risk_table %>% filter(riskfactor_outcome=='Serology') %>%
#   ggplot(aes(x=riskfactor_name,y=n,col=`Significant / Adjusted`, fill=`Significant / Adjusted`)) + 
#   geom_bar( stat='identity',position = position_dodge(preserve = "single")) + 
#   theme_light()+
#   scale_color_manual(values = custom_colours) +
#   scale_fill_manual(values = custom_colours) + xlab('') + ylab('')  + 
#   theme( axis.text.x = element_text( angle = 25, hjust = 1, size = text_size ),
#          strip.text = element_text( color = "black"),          
#          strip.background =element_rect(fill="grey90"),
#          text = element_text(size = text_size),
#          legend.position = 'none') 
# 
# cowplot::plot_grid(risk_table_plt_infection,
#                    risk_table_plt_serology,
#                    nrow=2)


# ## how many of each of the different outcomes
# risk_params %>% select(riskfactor_outcome) %>% group_by(riskfactor_outcome) %>%
#   summarise(length(riskfactor_outcome))
# 
# risk_params %>% filter(parameter_context_human=="Human") %>% select(riskfactor_outcome) %>% group_by(riskfactor_outcome) %>%
#   summarise(length(riskfactor_outcome))
# 
# # how many adjusted
# risk_params %>% select(riskfactor_adjusted) %>% group_by(riskfactor_adjusted) %>%
#   summarise(length(riskfactor_adjusted))

# 
# # whats happening with infection
# risk_params %>% filter(riskfactor_outcome=="Infection")
# 
# # whats happening with serology
# risk_params %>% filter(riskfactor_outcome=="Serology")
# 
# # what is happening with occupation
# risk_params %>% filter(grepl("Occupation",riskfactor_name))
# 
# # what is the deal with animals
# risk_params %>% filter(parameter_context_human=="Not human")
# 
# # how is quality looking 
# risk_params %>% select(article_qa_score) %>% unlist() %>% mean()

## want to have a table where we have the risk factors individually, and tell us how many times it appears and in which of the 4 categorisations
#risk_table$riskfactor_name %>% unique()
# Age, Sex, Occupation, Symptoms, Environmental factors, Time, Location, Agricultural activities, comorbidities

risk_table <- risk_params %>% mutate(
  age_ind = case_when(
    grepl("Age",riskfactor_name) ~ 1
  ),
  sex_ind = case_when(
    grepl("Sex",riskfactor_name) ~ 1
  ),
  occ_ind = case_when(
    grepl("Occupation",riskfactor_name) ~ 1
  ),
  symptoms_ind = case_when(
    grepl("Symptoms",riskfactor_name) ~ 1
  ),
  time_ind = case_when(
    grepl("Time",riskfactor_name) ~ 1
  ),
  location_ind = case_when(
    grepl("Location",riskfactor_name) ~ 1
  ),
  env_ind = case_when(
    grepl("Environmental factors",riskfactor_name) ~ 1
  ),
  agr_ind = case_when(
    grepl("Agricultural activities",riskfactor_name) ~ 1
  ),
  arbo_ind = case_when(
    grepl("Arbovirus co-infection",riskfactor_name) ~ 1
  ),
  non_arbo_ind = case_when(
    grepl("Non-arbovirus co-infection",riskfactor_name) ~ 1
  ),
  vect_det_ind = case_when(
    grepl("Detection of vectors",riskfactor_name) ~ 1
  ),
  animal_cont_ind = case_when(
    grepl("Animal/vector contact",riskfactor_name) ~ 1
  ),
  race_ind = case_when(
    grepl("Race",riskfactor_name) ~ 1
  ),
  travel_ind = case_when(
    grepl("Travel",riskfactor_name) ~ 1
  ),
  housing_ind = case_when(
    grepl("Housing type",riskfactor_name) ~ 1
  ),
  comorb_ind = case_when(
    grepl("Comorbidities",riskfactor_name) ~ 1
  ),
  other_ind = case_when(
    grepl("Other",riskfactor_name) ~ 1
  )
) %>% mutate(riskfactor_adjusted    = case_when(
  riskfactor_adjusted=='' ~ 'Unspecified',
  TRUE ~ riskfactor_adjusted)) %>%
  group_by(riskfactor_outcome,riskfactor_name,riskfactor_significant,
           riskfactor_adjusted,
           parameter_context_human) %>%
           unite(`Significant / Adjusted`,riskfactor_significant:riskfactor_adjusted, remove = FALSE, sep = " / ")
write.csv(risk_table,"risk_table.csv",row.names = FALSE) 

## what are the most common risk factors considered
risk_table %>% ungroup() %>% 
  select(age_ind,sex_ind,occ_ind,time_ind,location_ind,
         symptoms_ind,arbo_ind,non_arbo_ind,animal_cont_ind,race_ind,
         env_ind,agr_ind,vect_det_ind,travel_ind,housing_ind,comorb_ind,other_ind) %>% colSums(.,na.rm=TRUE)

risk_table %>% ungroup() %>% 
  select(age_ind,sex_ind,occ_ind,time_ind,location_ind,
         symptoms_ind,arbo_ind,non_arbo_ind,animal_cont_ind,race_ind,
         env_ind,agr_ind,vect_det_ind,travel_ind,housing_ind,comorb_ind,other_ind) %>% colSums(.,na.rm=TRUE) %>% sum()

### do analysis primarily with risk_table
# removing 'other'
risk_table %>% ungroup() %>% 
  select(age_ind,sex_ind,occ_ind,time_ind,location_ind,
         symptoms_ind,arbo_ind,non_arbo_ind,animal_cont_ind,race_ind,
         env_ind,agr_ind,vect_det_ind,travel_ind,housing_ind,comorb_ind) %>% colSums(.,na.rm=TRUE) %>% sum()

# filter to adjusted and then do previous sum
risk_table %>% ungroup() %>% filter(riskfactor_adjusted=="Not adjusted") %>%
  select(age_ind,sex_ind,occ_ind,time_ind,location_ind,
         symptoms_ind,arbo_ind,non_arbo_ind,animal_cont_ind,race_ind,
         env_ind,agr_ind,vect_det_ind,travel_ind,housing_ind,comorb_ind) %>% colSums(.,na.rm=TRUE) %>% sum()

# filter to post 2020 and then do previous sum
risk_table %>% ungroup() %>% filter(year_publication>=2020) %>%
  select(age_ind,sex_ind,occ_ind,time_ind,location_ind,
         symptoms_ind,arbo_ind,non_arbo_ind,animal_cont_ind,race_ind,
         env_ind,agr_ind,vect_det_ind,travel_ind,housing_ind,comorb_ind) %>% colSums(.,na.rm=TRUE) %>% sum()

risk_table %>% ungroup() %>% select(riskfactor_outcome) %>% mutate(riskfactor_outcome=factor(riskfactor_outcome)) %>% summary()

## how many studies have an other left
risk_table %>% ungroup() %>% filter(other_ind==1) %>% select(covidence_id) %>% unique()

## number of age risk factors
risk_table %>% ungroup() %>% filter(age_ind==1)
## and from how many papers
risk_table %>% ungroup() %>% filter(age_ind==1) %>% select(covidence_id) %>% unique()


## number of sex risk factors
risk_table %>% ungroup() %>% filter(sex_ind==1)
## and from how many papers
risk_table %>% ungroup() %>% filter(sex_ind==1) %>% select(covidence_id) %>% unique()


risk_table_filt <- risk_table %>% 
  filter(
    riskfactor_outcome %in% c("Infection","Serology","Presence of Oropouche"))
rf_for_plot <- rbind(
  risk_table_filt %>% filter(age_ind==1) %>% 
  group_by(`Significant / Adjusted`,
           riskfactor_outcome) %>%
  mutate(count = nrow(`Significant / Adjusted`),
         label = "Age"),
  risk_table_filt %>% filter(sex_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Sex"),
  risk_table_filt %>% filter(occ_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Occupation"),
  risk_table_filt %>% filter(symptoms_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Symptoms"),
  risk_table_filt %>% filter(time_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Time"),
  risk_table_filt %>% filter(location_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Location"),
  risk_table_filt %>% filter(env_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Environmental factors"),
  risk_table_filt %>% filter(agr_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Agricultural activities"),
  risk_table_filt %>% filter(arbo_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Arbovirus co-infection"),
  risk_table_filt %>% filter(non_arbo_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Non-arbovirus co-infection"),
  risk_table_filt %>% filter(vect_det_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Detection of vectors"),
  risk_table_filt %>% filter(animal_cont_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Animal/vector contact"),
  risk_table_filt %>% filter(race_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Race"),
  risk_table_filt %>% filter(travel_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Travel"),
  risk_table_filt %>% filter(housing_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Housing type"),
  risk_table_filt %>% filter(comorb_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Non-infectious comorbidities"),
  risk_table_filt %>% filter(other_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Other")
) %>%
  mutate(label = factor(label,
                        levels=c("Age","Agricultural activities",
                                 "Animal/vector contact",
                                 "Arbovirus co-infection",
                                 "Non-arbovirus co-infection",
                                 "Non-infectious comorbidities",
                                 "Detection of vectors",
                                 "Environmental factors","Housing type",
                                 "Location",
                                 "Occupation",
                                 "Race","Sex","Symptoms","Time","Travel",
                                 "Other")),
         riskfactor_outcome = case_when(
           riskfactor_outcome=="Presence of Oropouche" ~ "Occurrence of Oropouche",
           .default = riskfactor_outcome
         ),
         riskfactor_outcome = factor(riskfactor_outcome,
                                     levels=c("Infection","Serology",
                                              "Occurrence of Oropouche")),
         riskfactor_broad_cat = case_when(
           riskfactor_outcome %in% c("Infection","Serology") ~ "Individual-level",
           riskfactor_outcome=="Occurrence of Oropouche" ~ "Population-level"),
         riskfactor_broad_cat = factor(
           riskfactor_broad_cat,
           levels = c("Individual-level","Population-level"))
         )


ggplot(rf_for_plot %>% filter(label!="Other"),
       aes(y=label,fill=`Significant / Adjusted`))+
  geom_bar()+
  facet_wrap(riskfactor_broad_cat~riskfactor_outcome)+theme_bw()+
  scale_fill_manual(values = custom_colours)+
  labs(y="Risk factor",x="Number of risk factors extracted")+
  theme(strip.background = element_rect(fill="white"),
        legend.position = "bottom")+
  scale_x_continuous(breaks = c(0,2,4,6,8,10))+
  scale_y_discrete(limits=rev)+
  guides(fill=guide_legend(nrow=3,byrow=TRUE))
ggsave("rf_plot.png",width=7,height=5)


## analysis

# how many risk factors
rf_for_plot %>% nrow()

# remove other and then check how many adjusted


risk_params %>% ungroup() %>% select(article_label) %>% filter(article_label>=2020) %>% nrow()




