# OROV risk factors

library(tidyverse)
library(forcats)
library(orderly)

orderly_strict_mode()
pars <- orderly::orderly_parameters(pathogen = "RVF")


orderly_artefact(description="RVF risk factor figures",
                 c("risk_factors_count.pdf",
                   "risk_factors_count.png",
                   "risk_factors_for_writing.csv"))


# read in files

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "params.csv"))

articles   <- read_csv("articles.csv")
outbreaks <- tibble()
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

# # read in data
# articles <- read.csv("inputs/articles.csv")
# outbreaks <- read.csv("inputs/outbreaks.csv")
# parameters <- read.csv("inputs/parameters.csv")

# # covidence 
# covidence <- read.csv("covidence/orov_covidence.csv") %>%
#   rename(CovID = Covidence..)
# covidence$CovID %>% unique() %>% length()
# 
# # do we have all the articles we are expecting?
# testthat::expect_true(
#   nrow(articles) == length(unique(covidence$CovID)))
# #which(paste0("#",articles$covidence_id) %in% covidence$CovID)
# #articles[which(duplicated(articles$covidence_id)),]
# testthat::expect_length(
#   which(!(paste0("#",articles$covidence_id) %in% covidence$CovID)),
#   0)



## risk factors
#parameters %>% filter(parameter_class=="Risk factors") %>% View()

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

risk_params <- parameters %>%
  filter(grepl("Risk factors", parameter_type, ignore.case = TRUE)) %>%
  select(covidence_id,access_param_id,riskfactor_outcome, 
         riskfactor_name,	riskfactor_occupation, riskfactor_significant, 
         riskfactor_adjusted, population_sample_size,
         population_country, #dates,
         population_sample_type, population_group,
         parameter_context_location_type,
         qa_score,population_study_start_year,
         population_study_end_year,
         refs,parameter_notes)
write.csv(risk_params,row.names = FALSE,"risk_factors_for_writing.csv")

# what is happening in terms of number of obs per paper
risk_params %>% group_by(covidence_id) %>% 
  summarise(n = length(covidence_id)) %>% 
  arrange(-n)

# ## risk factor specific categorisations can go here
# risk_params <- risk_params %>% mutate(riskfactor_name = case_when(
#   covidence_id==9~"Environmental factors",
#   covidence_id==28&riskfactor_name=="Age,Sex,Other"~"Age,Sex,Symptoms",
#   covidence_id==28&riskfactor_name=="Other"~"Symptoms",
#   covidence_id==42&riskfactor_name=="Other"~"Symptoms",
#   covidence_id==42&riskfactor_name=="Sex,Other"~"Sex,Time,Symptoms,Arbovirus co-infection",
#   covidence_id==42&riskfactor_name=="Age,Other"~"Age,Location,Symptoms,Non-arbovirus co-infection",
#   covidence_id==47&riskfactor_name=="Age,Other"~"Age,Time",
#   covidence_id==64 ~ "Environmental factors",
#   covidence_id==128&riskfactor_name=="Sex,Other"~"Sex,Location",
#   covidence_id==136&riskfactor_name=="Age,Occupation,Other"~"Age,Occupation,Location",
#   covidence_id==136&riskfactor_adjusted=="Not adjusted"&riskfactor_significant=="Significant" ~ "Race,Other",
#   covidence_id==179&riskfactor_adjusted=="Adjusted"&riskfactor_significant=="Significant" ~ "Age,Sex,Location,Animal contact",
#   covidence_id==179&riskfactor_adjusted=="Not adjusted"&riskfactor_significant=="Significant"~"Age,Sex,Location",
#   covidence_id==179&riskfactor_adjusted=="Not adjusted"&riskfactor_significant=="Not significant" ~ "Occupation,Animal contact,Other",
#   covidence_id==525 ~ "Agricultural activities",
#   covidence_id==580 ~ "Location,Time",
#   covidence_id==665&population_country=="Peru"&riskfactor_name=="Other"~"Location,Time",
#   covidence_id==665&riskfactor_outcome=="Serology"&riskfactor_adjusted=="Not adjusted"&riskfactor_significant=="Significant"&riskfactor_name=="Other"~"Location",
#   covidence_id==665&riskfactor_outcome=="Serology"&riskfactor_significant=="Unspecified"&riskfactor_name=="Other"~"Environmental factors",
#   covidence_id==665&riskfactor_outcome=="Serology"&riskfactor_significant=="Significant"&riskfactor_name=="Other"~"Environmental factors,Other",
#   covidence_id==665&riskfactor_outcome=="Presence of Oropouche"&riskfactor_significant=="Unspecified"&riskfactor_name=="Other"~"Location,Environmental factors",
#   covidence_id==665&riskfactor_outcome=="Presence of Oropouche"&riskfactor_significant=="Significant"&riskfactor_name=="Other"~"Detection of vectors",
#   covidence_id==681~"Environmental factors",
#   covidence_id==690~"Location,Time",
#   covidence_id==521~"Environmental factors",
#   .default = riskfactor_name
# )
# )



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
                    'Unspecified / Adjusted'='grey30', 'Unspecified / Not adjusted' = 'grey50', 'Unspecified / Unspecified'='grey70')



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


## how many of each of the different outcomes
risk_params %>% select(riskfactor_outcome) %>% group_by(riskfactor_outcome) %>%
  summarise(length(riskfactor_outcome))


# how many adjusted
risk_params %>% select(riskfactor_adjusted) %>% group_by(riskfactor_adjusted) %>%
  summarise(length(riskfactor_adjusted))

# whats happening with infection
risk_params %>% filter(riskfactor_outcome=="Infection")

# whats happening with serology
risk_params %>% filter(riskfactor_outcome=="Serology")

# what is happening with occupation
risk_params %>% filter(grepl("Occupation",riskfactor_name))

# how is quality looking 
risk_params %>% select(qa_score) %>% unlist() %>% mean()

## want to have a table where we have the risk factors individually, and tell us how many times it appears and in which of the 4 categorisations
#risk_table$riskfactor_name %>% unique()
# Age, Sex, Occupation, Symptoms, Environmental factors, Time, Location, Agricultural activities, comorbidities
unique_risk_factors <- unique(unlist(strsplit(risk_params$riskfactor_name, ",")))
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
  HH_contact_ind = case_when(
    grepl("Household contact",riskfactor_name) ~ 1
  ),
  close_contact_ind = case_when(
    grepl("Close contact",riskfactor_name) ~ 1
  ),
  location_ind = case_when(
    grepl("Location (geographic)",riskfactor_name) ~ 1
  ),
  env_ind = case_when(
    grepl("Environmental",riskfactor_name) ~ 1
  ),
  consumption_ind = case_when(
    grepl("Consumption of animal product",riskfactor_name) ~ 1
  ),
  animal_contact_ind = case_when(
    grepl("Contact with animal",riskfactor_name) ~ 1
  ),
  comorbidity_ind = case_when(
    grepl("Comorbidity",riskfactor_name) ~ 1
  ),
  camel_contact_ind = case_when(
    grepl("Camel contact",riskfactor_name) ~ 1
  ),
  temp_ind = case_when(
    grepl("Temperature",riskfactor_name) ~ 1
  ),
  humidity_cont_ind = case_when(
    grepl("Humidity",riskfactor_name) ~ 1
  ),
  rainfall_ind = case_when(
    grepl("Rainfall",riskfactor_name) ~ 1
  ),
  other_ind = case_when(
    grepl("Other",riskfactor_name) ~ 1
  )
) %>% mutate(riskfactor_adjusted    = case_when(
  riskfactor_adjusted=='' ~ 'Unspecified',
  TRUE ~ riskfactor_adjusted)) %>%
  group_by(riskfactor_outcome,
           riskfactor_name,
           riskfactor_significant,
           riskfactor_adjusted) %>%
  unite(`Significant / Adjusted`,riskfactor_significant:riskfactor_adjusted, remove = FALSE, sep = " / ")


## what are the most common risk factors considered
# risk_table %>% ungroup() %>% 
#   select(age_ind,sex_ind,occ_ind,location_ind,
#          comorbidity_ind,close_contact_ind,HH_contact_ind,
#          camel_contact_ind,animal_contact_ind,consumption_ind,
#          env_ind,temp_ind,humidity_cont_ind,rainfall_ind,
#          other_ind) %>% colSums(.,na.rm=TRUE)|>view()


risk_table_filt <- risk_table %>% 
  filter(
    riskfactor_outcome %in% c("Infection","Serology","Death", "Other"))

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
  risk_table_filt %>% filter(animal_contact_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Contact with animals"),
  risk_table_filt %>% filter(env_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Environmental factors"),
  risk_table_filt %>% filter(consumption_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Consumption of animal products"),
  risk_table_filt %>% filter(temp_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Temperature"),
  risk_table_filt %>% filter(rainfall_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Rainfall"),
  risk_table_filt %>% filter(other_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Other")
) %>%
  mutate(label = factor(label,
                        levels=c("Age","Sex", "Contact with animals",
                                 "Occupation","Consumption of animal products",
                                 "Environmental factors","Temperature","Rainfall",
                                 "Other")),
         riskfactor_outcome = factor(riskfactor_outcome,
                                     levels=c("Serology","Infection",
                                              "Death", "Severe disease","Other"))
         )


risk_factors_count <- ggplot(rf_for_plot,aes(y=label,fill=`Significant / Adjusted`))+
  geom_bar()+
  facet_wrap(~riskfactor_outcome, scales="free_x", ncol=4)+theme_bw()+
  scale_fill_manual(values = custom_colours)+
  labs(title="Risk factors",y=NULL,x="Number of risk factors extracted")+
  theme(strip.background = element_rect(fill="white"),
        legend.position = "bottom")+
#  scale_x_continuous(breaks = c(0,2,4,6,8,10))+
  scale_y_discrete(limits=rev)+
  guides(fill=guide_legend(nrow=3,byrow=FALSE))
ggsave("risk_factors_count.png", plot = risk_factors_count, width = 10, height = 5)
ggsave("risk_factors_count.pdf", plot = risk_factors_count, width = 10, height = 5)

