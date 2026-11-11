# RVF risk factors

library(tidyverse)
library(forcats)
library(orderly)
library(dplyr)
library(janitor)
library(stringr)

orderly_strict_mode()
pars <- orderly::orderly_parameters(pathogen = "RVF")

orderly_shared_resource("rvf_functions.R"="rvf_functions.R")

source("rvf_functions.R")

orderly_artefact(description="RVF risk factor figures",
                 c("risk_factors_count.pdf",
                   "risk_factors_count.png",
                   "risk_factors_for_writing.csv"))


# read in files

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "params.csv", "outbreaks.csv"))

articles   <- read_csv("articles.csv")
outbreaks  <-  read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

text_size <- 14


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

# NOW MOVED TO CLEANING
# ##### Unpick "Other" risk factor specifics here #####
# 
# ## Write .csv with "other" risk factors 
# risk_params_others <- risk_params|>filter(grepl("Other",riskfactor_name))
# 
# # save data set for fixing 
# write.csv(risk_params_others,row.names = FALSE,"risk_params_others.csv")
# 
# # read in fixed version 
# risk_params_others_FIXED <- read_csv("risk_params_others_FIXED.csv")
# 
# ## clean up risk factor names
# # replace ; with , so lists match
# risk_params_others_FIXED <- risk_params_others_FIXED |> mutate(other_names=gsub("; ", ",", other_names, fixed = TRUE))
# # combine orginal risk_factor names and fixed names for others
# risk_params_others_FIXED <- risk_params_others_FIXED |> mutate(riskfactor_names_all=ifelse(is.na(other_names),riskfactor_name,paste(riskfactor_name, other_names, sep=",")))
# # If other has been fixed then remove from list 
# risk_params_others_FIXED <- risk_params_others_FIXED |> mutate(riskfactor_names_all=ifelse(is.na(other_names),riskfactor_names_all,gsub("Other,", "", riskfactor_names_all, fixed = TRUE)))
# # De-duplicate list 
# risk_params_others_FIXED <- risk_params_others_FIXED |>
#   mutate(
#     riskfactor_names_all = sapply(riskfactor_names_all, function(x) {
#       paste(unique(trimws(strsplit(x, ",")[[1]])), collapse = ",")
#     })
#   )
# 
# risk_params_other_clean <- risk_params_others_FIXED |> mutate(riskfactor_name=riskfactor_names_all, riskfactor_names_all=NULL, other_names = NULL)
# 
# risk_params <- rbind(risk_params|>filter(!grepl("Other",riskfactor_name)), risk_params_other_clean)

# risk_params_others_FIXED$refs[which(risk_params_others_FIXED$covidence_id==5395)] <- "Özcelik (2023)"

# what is happening in terms of number of obs per paper
risk_params %>% group_by(covidence_id) %>% 
  summarise(n = length(covidence_id)) %>% 
  arrange(-n)


# risk factors for each outcome
risk_params|>group_by(riskfactor_outcome)|>summarise(count=n())


#### Tables of risk factor counts ############

risk_table <- risk_params %>%
  separate_longer_delim(riskfactor_name, delim = ",") %>%
  mutate(population_sample_size = replace_na(as.double(population_sample_size),0),
         riskfactor_adjusted    = case_when(riskfactor_adjusted=='' ~ 'Unspecified',
                                            TRUE ~ riskfactor_adjusted)) %>%
  group_by(riskfactor_outcome,riskfactor_name,riskfactor_significant,riskfactor_adjusted,population_group) %>%
  summarise(n=n(),
            pop_size = sum(population_sample_size)) %>%
  unite(`Significant / Adjusted`,riskfactor_significant:riskfactor_adjusted, remove = FALSE, sep = " / ") %>% arrange(-n)

#text_size <- 20
custom_colours <- c('Significant / Adjusted'='darkred', 'Significant / Not adjusted' = 'pink', 'Significant / Unspecified'= 'red',
                 'Not significant / Adjusted'='blue4', 'Not significant / Not adjusted' = 'lightblue', 'Not significant / Unspecified'= 'blue',
                    'Unspecified / Adjusted'='grey30', 'Unspecified / Not adjusted' = 'grey50', 'Unspecified / Unspecified'='grey70')



# Number of studies
length(unique(risk_params$covidence_id))

# Number of risk factors for each outcome
rf_count <- risk_table|>group_by(riskfactor_outcome)|>
  summarise(count=n()) |>
  arrange(desc(count))

# unique studies for each outcome 
study_count <- risk_params |>
  distinct(covidence_id, riskfactor_outcome) |>
  count(riskfactor_outcome, name = "study_count") |>
  arrange(desc(study_count))

# combine into table summary 
summary_table_count <- full_join(rf_count, study_count, by = "riskfactor_outcome")
colnames(summary_table_count) <- c("Outcome", "Risk factor", "Studies")
write.csv(summary_table_count,row.names = FALSE,"risk_factors_studies_table.csv")

# unique studies for each outcome + adjusted 
risk_params |>
  distinct(covidence_id, riskfactor_outcome, riskfactor_adjusted) |>
  group_by(riskfactor_outcome, riskfactor_adjusted)|>
  summarise(count=n())


# Count of risk factors type for each outcome
unique_rf_count <- risk_table|>group_by(riskfactor_outcome, riskfactor_name, riskfactor_significant)|>
  summarise(count=n()) |>
  arrange((riskfactor_outcome))|>pivot_wider(names_from=riskfactor_significant, values_from=count,values_fill=0)|>
  mutate(`Number of times extracted`=`Not significant`+`Significant`, `Percentage of extracted significant (%)`=round((`Significant`/(`Not significant`+`Significant`))*100), 
         `Not significant`=NULL, `Significant`=NULL)
colnames(unique_rf_count)[1:2] <- c("Outcome", "Risk factor")
write.csv(unique_rf_count,row.names = FALSE,"risk_factors_significance_table.csv")


# 2 studies other outcome: confirmed/unconfirmed case, RVF activity (animal and human)

# Serology
risk_table_plt_serology <- risk_table %>% filter(riskfactor_outcome=='Serology') %>%
  ggplot(aes(x=riskfactor_name,y=n,col=`Significant / Adjusted`, fill=`Significant / Adjusted`)) +
  geom_bar( stat='identity') +
  theme_bw()+
  labs(title="Serology")+
  scale_color_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) + xlab('') + ylab('')  +
  theme(# axis.text.x = element_text( angle = 90, hjust = 1, vjust = 1 , size = text_size ),
         strip.text = element_text( color = "black"),
         strip.background =element_rect(fill="grey90"),
         text = element_text(size = text_size),
         legend.position = 'bottom')+coord_flip()

# Infection
risk_table_plt_infection <- risk_table %>% filter(riskfactor_outcome=='Infection') %>%
  ggplot(aes(x=riskfactor_name,y=n,col=`Significant / Adjusted`, fill=`Significant / Adjusted`)) +
  geom_bar( stat='identity',position = position_dodge(preserve = "single")) +
  scale_color_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) + xlab('') + ylab('') + theme_light() +
  labs(title="Infection")+
  theme( axis.text.x = element_text( angle = 90, hjust = 1, size = text_size ),
         strip.text = element_text( color = "black"),
         strip.background =element_rect(fill="grey90"),
         text = element_text(size = text_size),
         legend.position = 'none')

# Death
risk_table_plt_death <- risk_table %>% filter(riskfactor_outcome=='Death') %>%
  ggplot(aes(x=riskfactor_name,y=n,col=`Significant / Adjusted`, fill=`Significant / Adjusted`)) +
  geom_bar( stat='identity',position = position_dodge(preserve = "single")) +
  theme_light()+
  labs(title="Death")+
  scale_color_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) + xlab('') + ylab('')  +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, size = text_size ),
         strip.text = element_text( color = "black"),
         strip.background =element_rect(fill="grey90"),
         text = element_text(size = text_size),
         legend.position = 'none')

# Severe disease
risk_table_plt_severe <- risk_table %>% filter(riskfactor_outcome=='Severe disease') %>%
  ggplot(aes(x=riskfactor_name,y=n,col=`Significant / Adjusted`, fill=`Significant / Adjusted`)) +
  geom_bar( stat='identity',position = position_dodge(preserve = "single")) +
  theme_light()+
  labs(title="Severe disease")+
  scale_color_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) + xlab('') + ylab('')  +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, size = text_size ),
         strip.text = element_text( color = "black"),
         strip.background =element_rect(fill="grey90"),
         text = element_text(size = text_size),
         legend.position = 'none')

# Other
risk_table_plt_other <- risk_table %>% filter(riskfactor_outcome=='Other') %>%
  ggplot(aes(x=riskfactor_name,y=n,col=`Significant / Adjusted`, fill=`Significant / Adjusted`)) +
  geom_bar( stat='identity',position = position_dodge(preserve = "single")) +
  theme_light()+
  labs(title="Other")+
  scale_color_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) + xlab('') + ylab('')  +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, size = text_size ),
         strip.text = element_text( color = "black"),
         strip.background =element_rect(fill="grey90"),
         text = element_text(size = text_size),
         legend.position = 'none')

# ALL 
risk_table_plt_grid <- risk_table %>%
  ggplot(aes(x=riskfactor_name,y=n,col=`Significant / Adjusted`, fill=`Significant / Adjusted`)) +
  geom_bar( stat='identity',position = position_dodge(preserve = "single")) +
  theme_light()+
  labs(title="Other")+
  scale_color_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) + xlab('') + ylab('')  +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, size = text_size ),
         strip.text = element_text( color = "black"),
         strip.background =element_rect(fill="white"),
         text = element_text(size = text_size),
         legend.position = 'none')+ facet_wrap(~riskfactor_outcome, ncol=1, scales="free_y")


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


####### summarise risk factors 

ggplot(risk_params)+
  geom_bar(aes(y=population_country, fill=riskfactor_outcome))+
  xlab("Risk factor count") + ylab(NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "left")


ggplot(risk_params)+
  geom_bar(aes(y=population_country, fill=population_sample_type))+
  xlab("Risk factor count") + ylab(NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "left")


ggplot(risk_params)+
  geom_bar(aes(y=population_sample_type, fill=riskfactor_outcome))+
  xlab("Risk factor count") + ylab(NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "left")

ggplot(risk_params)+
geom_bar(aes(y=population_group, fill=riskfactor_outcome))+
  xlab("Risk factor count") + ylab(NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "left")


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
risk_table %>% ungroup() %>%
  select(age_ind,sex_ind,occ_ind,location_ind,
         comorbidity_ind,close_contact_ind,HH_contact_ind,
         camel_contact_ind,animal_contact_ind,consumption_ind,
         env_ind,temp_ind,humidity_cont_ind,rainfall_ind,
         other_ind) %>% colSums(.,na.rm=TRUE)|>sort()


risk_table_filt <- risk_table %>% 
  filter(
    riskfactor_outcome %in% c("Infection","Serology","Death","Severe disease", "Other"))

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
  risk_table_filt %>% filter(rainfall_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Rainfall"),
  risk_table_filt %>% filter(close_contact_ind==1|HH_contact_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Human contact"),
  risk_table_filt %>% filter(temp_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Temperature"),
  risk_table_filt %>% filter(other_ind==1) %>% 
    group_by(`Significant / Adjusted`,
             riskfactor_outcome) %>%
    mutate(count = nrow(`Significant / Adjusted`),
           label = "Other")
) %>%
  mutate(label = factor(label,
                        levels=c("Age","Sex", "Contact with animals",
                                 "Occupation","Consumption of animal products",
                                 "Environmental factors","Rainfall","Temperature","Human contact",
                                 "Other")),
         riskfactor_outcome = factor(riskfactor_outcome,
                                     levels=c("Serology","Infection",
                                              "Death", "Severe disease","Other"))
         )


risk_factors_count <- ggplot(rf_for_plot,aes(y=label,fill=`Significant / Adjusted`))+
  geom_bar()+
  facet_wrap(~riskfactor_outcome, scales="free_x", ncol=5)+theme_bw()+
  scale_fill_manual(values = custom_colours)+
  labs(title="Risk factors",y=NULL,x="Number of risk factors extracted")+
  theme(strip.background = element_rect(fill="white"),
        legend.position = "bottom")+
#  scale_x_continuous(breaks = c(0,2,4,6,8,10))+
  scale_y_discrete(limits=rev)+
  guides(fill=guide_legend(nrow=3,byrow=FALSE))
ggsave("risk_factors_count.png", plot = risk_factors_count, width = 12, height = 5)
ggsave("risk_factors_count.pdf", plot = risk_factors_count, width = 12, height = 5)

## Save all risk params
write.csv(risk_params,row.names = FALSE,"risk_factors_for_writing.csv")


# Heatmap
library(tidyverse)
# replace occupation with occupation name
risk_params_occupation <- risk_params %>%
  mutate(riskfactor_name_occupation = str_replace(riskfactor_name, "Occupation", riskfactor_occupation))
risk_params_occupation$riskfactor_name_occupation[which(risk_params_occupation$access_param_id=="0630_0005")] <-
 "Age groups,Location (geographic),Settlement type,Education,Formal occupation,Travel,Vector prevention"
  

# Reshape data
risk_heatmap_data <- risk_params_occupation %>%
  separate_longer_delim(riskfactor_name_occupation, delim = ",") %>%
  mutate(#population_sample_size = replace_na(as.double(population_sample_size),0),
         riskfactor_adjusted    = case_when(riskfactor_adjusted=='' ~ 'Unspecified',
                                            TRUE ~ riskfactor_adjusted)) %>%
  unite(`Significant / Adjusted`,riskfactor_significant:riskfactor_adjusted, remove = FALSE, sep = " / ") 

# Optional: keep risk factors in frequency order
risk_heatmap_data <- risk_heatmap_data %>%
  mutate(
    riskfactor_name_occupation = forcats::fct_infreq(riskfactor_name_occupation)
    # ,
    # refs = forcats::fct_rev(factor(refs))
  )|>mutate(
    
    riskfactor_name_occupation_new = case_when( riskfactor_name_occupation %in% c(" Veterinary", "Veterinary") ~ "Veterinary",
                                           
                                             riskfactor_name_occupation %in% c(" Abattoir services/butchery", "Abattoir services") ~ "Abattoir services/butchery",
                                        
                                        riskfactor_name_occupation %in% c("Other", "Unspecified") ~ "Other occupation",
                                        
                                        riskfactor_name_occupation %in% c("Job description") ~ "Other occupation",
                                        
                                        riskfactor_name_occupation %in% c("Livestock/animal herders and farm workers", "Livestock and animal herders") ~ "Livestock/animal herders and farm workers",
                                        
                                        riskfactor_name_occupation %in% c("Age", "Age groups") ~ "Age",
                                        
                                        riskfactor_name_occupation %in% c("Rainfall","River discharge", "Water source", "Dams on farm", "Distance to water source", "Environmental (water)") ~ "Water sources",
                                        
                                        riskfactor_name_occupation %in% c("Household contact", "Close contact") ~ "Contact with human case",
                                        
                                        riskfactor_name_occupation %in% c("Animal with symptoms", "Animal with RVF") ~ "Contact with animal case",
                                        
                                        riskfactor_name_occupation %in% c( "Birthplace", "Ethnicity") ~ "Community group",
                                        
                                        riskfactor_name_occupation %in% c("HIV status", "Yellow fever vaccine","Q-fever co-infection", "Brucellosis co-infection",
                                                                          "Symptoms",  "Comorbidity",
                                                                          "Outcome of Malaria parasite test", "Recovery following malaria treatment") ~ "Comorbidity",
                                        
                                        
                                        .default = riskfactor_name_occupation
                                        
    ))|>mutate(
      population_group_animal = case_when(population_group %in% c("Abattoir workers","Animal workers","Farmers", "Animal workers incl. veterinary") ~ "Animal workers",
                                          population_group %in% c("Persons under investigation","Persons with symptoms") ~ "Other",
                                          population_group %in% c("Children", "Other") ~ "Other",
                                          .default=population_group),
    riskfactor_name_groups = case_when(  riskfactor_name_occupation_new %in% c("Contact with human case", "Neighbours with RVF", "Recent illness") ~ "Individual background",
                                            
                                         riskfactor_name_occupation_new %in% c("Abattoir services/butchery") ~ "Contact with animal",
                                        
                                         riskfactor_name_occupation_new %in% c(" Veterinary", "Veterinary", "Animal surgery") ~ "Contact with animal",
                                            
                                         riskfactor_name_occupation_new %in% c("Livestock/animal herders and farm workers", "Livestock and animal herders") ~ "Contact with animal",
                                        
                                         riskfactor_name_occupation_new %in% c("Working hours", "Time of residency","Time in work", "Permanance of job contract","Formal occupation",
                                                                              "Job description","Job duties","Job training", "Wear PPE", "Cuts/wounds", "Time", "Type of Livestock",
                                                                              "Patient outcome", "Comorbidity", "Other occupation", "PCR positive", "Presence of flies", "Healthcare") ~ "Other",
                                        
                                         riskfactor_name_occupation_new %in% c("Age", "Sex", "Education", "Socioeconomic status", "Travel", "Knowledge of RVF", 
                                                                              "Vector bite", "Community group") ~ "Individual background",
                                        
                                         riskfactor_name_occupation_new %in% c("Housing with electricity", "Housing material", "Roofing material", "Settlement type",
                                                                              "Seminomadic", "Water from public well", "Household size", "Vector prevention") ~ "Housing",
                                            
                                         riskfactor_name_occupation_new %in% c("Temperature", "Humidity", "Location (geographic)", "Environmental","Water sources") ~ "Environment",
                                        
                                         riskfactor_name_occupation_new %in% c("Animal slaughter", "Contact with carcass/aborted fetus","Disposed of carcass/aborted fetus",
                                                                   "Handling raw meat/cooking","Animal autopsy","Animal birthing","Handling raw meat","Skinned animal"
                                                                   ) ~ "Animal material",
                                        
                                         riskfactor_name_occupation_new %in% c("Consumption of animal found dead","Milk source", "Consumption of animal product") ~ "Animal material",
                                        
                                         riskfactor_name_occupation_new %in% c("Sleeping outside with herd", "Milking", "Housing with animal water basins", "Contact with animal", "Contact with animal case", "Sheltering animals in home") ~ "Contact with animal",
                                        
                                         riskfactor_name_occupation_new %in% c( "Type of livestock", "Livestock density", "Graze with wild animals", "Farm type", "Spending time outdoors") ~ "Farming",
                                            
                                            
                                            .default = riskfactor_name_occupation_new
      
    )
  )

## SAVE SUMMARY TABLES 
rf_groups <- risk_heatmap_data %>%
  group_by(riskfactor_name_groups) %>%
  summarise(
    Risk_factors = paste(unique(riskfactor_name_occupation_new), collapse = ", "),
    .groups = "drop"
  )
write.csv(rf_groups,row.names = FALSE,"risk_factors_groups_heatmap.csv")

rf_names <- risk_heatmap_data %>%
  group_by(riskfactor_name_occupation_new) %>%
  summarise(
    Risk_factors = paste(unique(riskfactor_name_occupation), collapse = ", "),
    .groups = "drop"
  ) 
rf_names <- rf_names|> filter(grepl(",", rf_names$Risk_factors))

write.csv(rf_names,row.names = FALSE,"risk_factors_names_fix.csv")

# Colour palette 
custom_colours_adjusted <- c('Significant'='darkred',   'Not significant'='blue4')

# Plot all outcomes on heatmap  
all_outcomes_heatmap <- 
  ggplot(risk_heatmap_data|> mutate(urefs = make.unique(refs) ),
       aes(x = riskfactor_name_groups,
           y = refs)) +
  
  geom_point( aes(
      size = population_sample_size,
      fill=`Significant / Adjusted`
    ),
    shape = 21,
    colour = "grey30",
    alpha = 0.5
  ) +
  
  scale_fill_manual(values = custom_colours) +
  
  scale_size_continuous(
    range = c(2, 14),
    name = "Sample size"
  ) +
  
  labs(
    x = "Risk factor",
    y = "Reference",
    fill = "Significance"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    panel.grid.major = element_line(colour = "grey85"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  ) + facet_wrap( ~riskfactor_outcome,
    scales = "free_y",
    space = "free_y") +
  theme(
      strip.placement = "outside",
      strip.background = element_blank()
    )

all_outcomes_heatmap
  
ggsave("risk_factors_all_outcomes.png", plot = all_outcomes_heatmap, width = 10, height = 20)
ggsave("risk_factors_all_outcomes.pdf", plot = all_outcomes_heatmap, width = 10, height = 20)

    
risk_heatmap_data_sero <- risk_heatmap_data |> filter(riskfactor_outcome=="Serology")

risk_heatmap_data_sero <- risk_heatmap_data_sero |> mutate(urefs = make.unique(refs) )

study_order <- risk_heatmap_data_sero |>
  distinct(urefs, refs, population_sample_size) |>
  arrange(population_sample_size) |>
  pull(urefs)

risk_heatmap_data_sero <- risk_heatmap_data_sero |>
  mutate(
    refs = factor(refs, levels = study_order)
  )  |>
  mutate(
    sample_size_missing = ifelse(
      is.na(population_sample_size),
      "Unknown",
      "Reported"
    ), 
    sample_size_na = ifelse(
      is.na(population_sample_size),
      100,
      population_sample_size
    )
  )|>
  filter(riskfactor_adjusted=="Adjusted")


# unpick doubles of the same risk factor
doubles <- risk_heatmap_data_sero %>%
  group_by(covidence_id, riskfactor_name_occupation_new, riskfactor_adjusted, population_group) %>%
  filter(n() >= 2) %>%
  ungroup()
    
### ADD SERO
## map to sero studies

sero_studies <- parameters |>
  filter(parameter_class == 'Seroprevalence')

sero_no_unit_row_count <- sero_studies |>
  filter(parameter_unit=="No units" | is.na(parameter_unit)) |>
  NROW()

cat("Number of extracted serology rows with no unit: ", sero_no_unit_row_count)
#7 no units, and 17 NAs
sero_studies_not_percentage <- sero_studies |>
  filter(parameter_unit=="No units" | is.na(parameter_unit))

# All the others are cases of having numerator and denominator, but not specific central value (though a couple of people have put 0)

# Assumption: parameter unit is always a percentage
pop_group_levels <- unique(sero_studies$population_group)#, "Other", "Unspecified")

##### SERO STUDIES FOR RISK FACTOR PLOT #########
sero_studies <- sero_studies |>
  mutate(parameter_value = coalesce(parameter_value,central),
         parameter_unit="Percentage (%)",
         population_group = factor(
           population_group,
           levels = pop_group_levels))

# for plot remove age disag extraction for covidence #1617
sero_studies <- sero_studies |>
  filter(access_param_id!="0603_0002") 

# for covidence #1619 - use sero that matches risk factor population 
sero_studies <- sero_studies |>
  filter(access_param_id!="0680_0006") 
# # Population group is outpatients - mark as other 
# sero_studies$population_group[which(sero_studies$access_param_id=="680_001")] <- "Other"


## for covidence 1823 use IgG total % (from paper)
sero_studies$parameter_value[which(sero_studies$covidence_id==1823)] <- 4.5
sero_studies$parameter_type[which(sero_studies$covidence_id==1823)] <- "Seroprevalence - IgG"

## for covidence 5866 use sero that matches risk factor location
sero_studies$parameter_value[which(sero_studies$covidence_id==5866)] <- as.numeric(sero_studies$parameter_value[which(sero_studies$access_param_id=="0702_0001")])
sero_studies$parameter_type[which(sero_studies$covidence_id==5866)] <- "Seroprevalence - IgG or IgM"

## for covidence 741 - fix population group
sero_studies$population_group[which(((sero_studies$covidence_id==741)&(sero_studies$access_param_id!="559_002")))] <- "Mixed groups" # non-refugees - should be other?

## for covidence 741 - report sero associated with risk factors 
sero_studies_741_both <- sero_studies|>filter(access_param_id=="514_001")
sero_studies_741_both$access_param_id <- "514_00001" 
sero_studies_741_both$parameter_value <- 61/202
sero_studies_741_both$cfr_ifr_numerator <- 61
sero_studies_741_both$parameter_type <- "Seroprevalence - IgG or IgM"
sero_studies_741_both$population_group <- "Other" # non refugee#
sero_studies_741_both$id <- ids::random_id(1) # new random id

# just keep sero related to risk factor 
sero_studies <- rbind(sero_studies|>filter(covidence_id!=741),sero_studies_741_both)

## for covidence 1660 - report sero associated with risk factors 
sero_studies_1660_rf <- sero_studies|>filter(access_param_id=="0683_0001")
sero_studies_1660_rf$access_param_id <- "683_00001" 
sero_studies_1660_rf$parameter_value <- 15.7
sero_studies_1660_rf$cfr_ifr_numerator <- 36
sero_studies_1660_rf$cfr_ifr_numerator <- 230
sero_studies_1660_rf$population_sample_size <- 230
sero_studies_1660_rf$population_country <- "Kenya" # non refugee
sero_studies_1660_rf$population_location <- "Eastern Kenya" # non refugee
sero_studies_1660_rf$population_group <- "Animal workers incl. veterinary" # non refugee
sero_studies_1660_rf$id <- ids::random_id(1) # new random id

# just keep sero related to risk factor 
sero_studies <- rbind(sero_studies|>filter(covidence_id!=1660),sero_studies_1660_rf)

# Sero - other is "IgG or IgM" (according to spreadsheet)
# covidence #5561
sero_studies$parameter_type[which(sero_studies$access_param_id=="0624_0001")]  <- "Seroprevalence - IgG or IgM" # from "Other"
# covidence #5009
sero_studies$parameter_type[which(sero_studies$access_param_id=="0575_0001")]  <- "Seroprevalence - IgG or IgM" # from "Other"



# covidence #5130 - just keep sero related to risk factors 
sero_studies <- sero_studies|>filter(access_param_id!="0607_0002")

# covidence #666 - just keep sero related to risk factors 
sero_studies <- sero_studies|>filter(access_param_id!="473_004")
sero_studies <- sero_studies|>filter(access_param_id!="473_005")

# covidence #5866 - remove Magugu as no risk factors 
sero_studies <- sero_studies|>filter(access_param_id!="0702_0002")
# NOTE - check parameter value is correct in sero plots - should be 0 for magugu (is correct in sero spreadsheet)

# covidence 1728 - remove IgM test among IgG seropositives
sero_studies <- sero_studies|>filter(access_param_id!="0597_0002")


## for covidence 1742 - report sero associated with risk factors - Gumarey
sero_studies_1742_gumarey <- sero_studies|>filter(access_param_id=="0598_0001")
sero_studies_1742_gumarey$access_param_id <- "598_00001" 
sero_studies_1742_gumarey$parameter_value <- 20
sero_studies_1742_gumarey$parameter_uncertainty_lower_value <- 14
sero_studies_1742_gumarey$parameter_uncertainty_upper_value <- 29.2
sero_studies_1742_gumarey$cfr_ifr_numerator <- 25
sero_studies_1742_gumarey$cfr_ifr_numerator <- 122
sero_studies_1742_gumarey$population_sample_size <- 122
sero_studies_1742_gumarey$population_country <- "Kenya"
sero_studies_1742_gumarey$population_location <- "Gumarey" 
sero_studies_1742_gumarey$population_group <- "General population" 
sero_studies_1742_gumarey$id <- ids::random_id(1) # new random id

## for covidence 1742 - report sero associated with risk factors - Sogan-Godud
sero_studies_1742_sogan <- sero_studies|>filter(access_param_id=="0598_0001")
sero_studies_1742_sogan$access_param_id <- "598_00002" 
sero_studies_1742_sogan$parameter_value <- 6
sero_studies_1742_sogan$parameter_uncertainty_lower_value <- 2.7
sero_studies_1742_sogan$parameter_uncertainty_upper_value <- 11.8
sero_studies_1742_sogan$cfr_ifr_numerator <- 8
sero_studies_1742_sogan$cfr_ifr_numerator <- 126
sero_studies_1742_sogan$population_sample_size <- 126
sero_studies_1742_sogan$population_country <- "Kenya"
sero_studies_1742_sogan$population_location <- "Sogan-Godud" 
sero_studies_1742_sogan$population_group <- "General population" 
sero_studies_1742_sogan$id <- ids::random_id(1) # new random id

# covidence 1742 - remove PRNT test of IgG positives
# just keep sero related to risk factor 
sero_studies <- rbind(sero_studies|>filter(access_param_id!="0598_0002"),sero_studies_1742_gumarey,sero_studies_1742_sogan)

# get rid of seroprevalence from test type
sero_studies$parameter_type <- sub(".*?Seroprevalence - ", "", sero_studies$parameter_type , perl = TRUE)

#### PLOT SERO 
# plot multiple test types
sero_studies_wider <- sero_studies |> mutate(urefs = make.unique(refs) ) |>
  pivot_wider(names_from = parameter_type, values_from = parameter_value)|>
  mutate(riskfactor_name_groups="Seroprevalence")|>
  mutate(
    population_group_animal = case_when(population_group %in% c("Abattoir workers","Animal workers","Farmers", "Animal workers incl. veterinary") ~ "Animal workers",
                                        population_group %in% c("Persons under investigation","Persons with symptoms") ~ "Other",
                                        # check they have symptoms?
                                        population_group %in% c("Children", "Other") ~ "Other",
                                        .default=population_group))|> 
  mutate( riskfactor_name_groups= factor(riskfactor_name_groups,
                                         levels=c("Seroprevalence",
                                                  "Individual background",
                                                  "Contact with animal",
                                                  "Animal material",
                                                  "Environment", "Farming",
                                                  "Housing", "Other" )))|>
            filter(covidence_id %in% unique(risk_heatmap_data_sero$covidence_id) )

# combine sero and riskfactors
#seroprevalence_dataframe <- left_join(risk_heatmap_data_sero, sero_studies)


library(ggnewscale)
# add sample size bins 
risk_heatmap_data_sero <- mutate(risk_heatmap_data_sero, sample_size_bin = cut(population_sample_size, breaks=c(0,50,100,200,400,1000,4500), 
                                                                                 labels=c("(0,50]","(50,100]","(100,200]","(200,400]","(400,1000]","(1000,4500]")))|>
  mutate(sample_size_bin=factor(sample_size_bin, levels = c("Unknown","(0,50]","(50,100]","(100,200]","(200,400]","(400,1000]","(1000,4500]")))

risk_heatmap_data_sero$sample_size_bin[is.na(risk_heatmap_data_sero$sample_size_bin)] <- "Unknown"
sample_size_quide <- c("Unknown"=2, 
                       "(100,200]"=4,
                       "(200,400]"=5,
                       "(400,1000]"=6, 
                       "(1000,4500]"=8)       


risk_heatmap_data_sero <- risk_heatmap_data_sero |> 
  mutate(refs.size = paste(refs, population_sample_size))

risk_heatmap_data_sero_refs <- risk_heatmap_data_sero |> mutate(new.refs = as.character(refs))
sero_studies_wider <- sero_studies_wider |> mutate(new.refs = as.character(refs))

## FIX REFS - when study has more than 1 population group
# covidence #503 - Cook (2017)
# sample size 1861
risk_heatmap_data_sero_refs$new.refs[which(risk_heatmap_data_sero_refs$access_param_id=="492_006")] <- paste(risk_heatmap_data_sero_refs$refs[which(risk_heatmap_data_sero_refs$access_param_id=="492_006")], 1, sep=" ")
sero_studies_wider$new.refs[which(sero_studies_wider$access_param_id%in%c("492_001","492_002","492_003"))] <- paste(sero_studies_wider$refs[which(sero_studies_wider$access_param_id%in%c("492_001","492_002","492_003"))], 1, sep=" ")

# samples size 553
risk_heatmap_data_sero_refs$new.refs[which(risk_heatmap_data_sero_refs$access_param_id%in%c("492_012","492_013"))] <- paste(risk_heatmap_data_sero_refs$refs[which(risk_heatmap_data_sero_refs$access_param_id%in%c("492_012","492_013"))], 2, sep=" ")
sero_studies_wider$new.refs[which(sero_studies_wider$access_param_id%in%c("492_007","492_008","492_009"))] <- paste(sero_studies_wider$refs[which(sero_studies_wider$access_param_id%in%c("492_007","492_008","492_009"))], 2, sep=" ")

# covidence #1742 - Labeaud (2)
#248 - all regions
risk_heatmap_data_sero_refs$new.refs[which(risk_heatmap_data_sero_refs$access_param_id=="0598_0005")] <- paste(risk_heatmap_data_sero_refs$new.refs[which(risk_heatmap_data_sero_refs$access_param_id=="0598_0005")] , 1, sep=" ")
sero_studies_wider$new.refs[which(sero_studies_wider$access_param_id=="0598_0001")] <- paste(sero_studies_wider$refs[which(sero_studies_wider$access_param_id=="0598_0001")], 1, sep=" ")
#126
risk_heatmap_data_sero_refs$new.refs[which(risk_heatmap_data_sero_refs$access_param_id=="0598_0007")] <- paste(risk_heatmap_data_sero_refs$new.refs[which(risk_heatmap_data_sero_refs$access_param_id=="0598_0007")] , 2, sep=" ")
sero_studies_wider$new.refs[which(sero_studies_wider$access_param_id=="598_00001")] <- paste(sero_studies_wider$refs[which(sero_studies_wider$access_param_id=="598_00001")], 2, sep=" ")
#122
risk_heatmap_data_sero_refs$new.refs[which(risk_heatmap_data_sero_refs$access_param_id=="0598_0006")] <- paste(risk_heatmap_data_sero_refs$refs[which(risk_heatmap_data_sero_refs$access_param_id=="0598_0006")], 3, sep=" ")
sero_studies_wider$new.refs[which(sero_studies_wider$access_param_id=="598_00002")] <- paste(sero_studies_wider$refs[which(sero_studies_wider$access_param_id=="598_00002")], 3, sep=" ")

# remove estimated sero (if observed given)
sero_studies_wider <- sero_studies_wider|>filter(!access_param_id%in%c("492_002","492_003","492_008","492_009","0619_0002"))

# add column to identify multiple risk factors with same name and assign jitter
risk_heatmap_data_sero_refs <- risk_heatmap_data_sero_refs |>
  group_by(new.refs, riskfactor_name_occupation_new) |>
  mutate(duplicate_rf = n() >= 2 ) |>
  ungroup()

make_jitter <- function(n) {
  switch(as.character(n),
         "1" = 0,
         "2" = c(-1, 1),
         "3" = c(-1, 0, 1))
}

risk_heatmap_data_sero_refs <- risk_heatmap_data_sero_refs %>%
  group_by(new.refs, riskfactor_name_occupation_new) %>%
  mutate(
    duplicate_rf = n() >= 2,
    jitter = make_jitter(n())
  ) %>%
  ungroup()

# # order heatmap
# count risk factors realted to each study 
risk_heatmap_data_sero_refs <- risk_heatmap_data_sero_refs |>
  group_by(new.refs) |>
  mutate(rf_count = n()) |>
  ungroup()

study_order <- risk_heatmap_data_sero_refs |>
  distinct(new.refs, rf_count) |>
  arrange((rf_count)) |>
  pull(new.refs)

# count studies realted to each risk factor
risk_heatmap_data_sero_refs <- risk_heatmap_data_sero_refs |>
  group_by(riskfactor_name_occupation_new) |>
  mutate(study_count = n()) |>
  ungroup()

riskfactor_order <- risk_heatmap_data_sero_refs |>
  distinct(riskfactor_name_occupation_new, study_count) |>
  arrange(desc(study_count)) |>
  pull(riskfactor_name_occupation_new)

# order data frame 
risk_heatmap_data_sero_refs <- risk_heatmap_data_sero_refs |>
  mutate(
 #   refs = factor(refs, levels = study_order),
    riskfactor_name_occupation_new = factor(riskfactor_name_occupation_new, levels=riskfactor_order),
    new.refs = factor(new.refs, levels = study_order),
  ) |> 
  mutate( riskfactor_name_groups= factor(riskfactor_name_groups,
                                         levels=c("Seroprevalence",
                                                  "Individual background",
                                                  "Contact",  "Contact with animal",
                                                  "Animal workers","Animal material",
                                                  "Environment", "Farming",
                                                  "Housing", "Other" )))



# sero studies

# # study order IgG?
# study_order <- sero_studies_wider |>
#   distinct(new.refs, IgG) |>
#   arrange((IgG)) |>
#   pull(new.refs)
# 
# study_order[duplicated(study_order)]

sero_studies_wider <- sero_studies_wider |> 
  mutate(new.refs = factor(new.refs, levels = study_order))#



# heat map with grouped risk factors
serology_heatmap <-  ggplot(risk_heatmap_data_sero_refs,
                            aes(x = riskfactor_name_occupation_new,
                                y = new.refs)) +
  # geom_quasirandom(
  #   varwidth =TRUE,
  #   width = 0.3,
  #   groupOnX = TRUE,
  #   aes(
  #     size = sample_size_bin,
  #     fill= riskfactor_significant,
  #     colour= riskfactor_significant,
  #     shape = sample_size_bin
  #   ),
  #   alpha = 0.5
  # ) +
  # geom_point(
  #   aes(
  #     size = sample_size_bin,
  #     fill= riskfactor_significant,
  #     colour= riskfactor_significant,
  #     shape = sample_size_bin
  #   ),
  #   alpha = 0.5,
  #   position = position_manual(x = jitter * 0.15)
  # ) +
  geom_point(
    data = subset(risk_heatmap_data_sero_refs, jitter == 0),
    aes(size = sample_size_bin,
        fill = riskfactor_significant,
        colour = riskfactor_significant,
        shape = sample_size_bin),
    alpha = 0.5
  ) +
  
  geom_point(
    data = subset(risk_heatmap_data_sero_refs, jitter == -1),
    aes(size = sample_size_bin,
        fill = riskfactor_significant,
        colour = riskfactor_significant,
        shape = sample_size_bin),
    position = position_nudge(x = -0.15),
    alpha = 0.5
  ) +
  
  geom_point(
    data = subset(risk_heatmap_data_sero_refs, jitter == 1),
    aes(size = sample_size_bin,
        fill = riskfactor_significant,
        colour = riskfactor_significant,
        shape = sample_size_bin),
    position = position_nudge(x = 0.15),
    alpha = 0.5
  ) +
  
  scale_colour_manual(values = custom_colours_adjusted) +
  scale_fill_manual(values = custom_colours_adjusted) +
  scale_shape_manual(
    name = "Sample size",
    values = c(
      "(100,200]" = 21,  # circle
      "(200,400]" = 21,  # circle
      "(400,1000]" = 21,  # circle
      "(1000,4500]" = 21,  # circle
      "Unknown" = 24    # triangle
    )) +
  scale_size_manual(
   values = sample_size_quide,
    name = "Sample size",
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Significance",
    colour = "Significance",
  ) +theme_minimal(base_size = 12) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    panel.grid.major = element_line(colour = "grey85"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  ) + #guides(shape="none") +
  guides( fill = guide_legend(
    override.aes = list(
      size = 5
    )
  ),
  colour = guide_legend(
    override.aes = list(
      size = 5
    )
  ),
    shape = guide_legend(
      override.aes = list(
        colour = "black",
        fill = "white",
        alpha = 1
      )
    ),
    size = guide_legend(
      override.aes = list(
        colour = "black",
        fill = "white",
        alpha = 1
      )
    )
  ) +
  facet_grid(population_group_animal~riskfactor_name_groups, scales="free", space="free",switch = "y")

serology_heatmap

serology_heatmap_sero <- serology_heatmap + guides(fill = "none") + new_scale_fill() + 
  geom_tile(
    data = sero_studies_wider,# |> mutate(seroprevalence= factor(riskfactor_name_groups, levels=c(unique(risk_heatmap_data_sero$riskfactor_name_groups), "seroprevalence (%)"))) ,
    aes(
      x = "% Seroprevalence - IgG",
      y = new.refs,
      fill = `IgG`,
    ),
    width = 0.9,
    height = 0.9
  )  + 
  geom_tile(
    data = sero_studies_wider,# |> mutate(seroprevalence= factor(riskfactor_name_groups, levels=c(unique(risk_heatmap_data_sero$riskfactor_name_groups), "seroprevalence (%)"))) ,
    aes(
      x = "% Seroprevalence - IgM",
      y = new.refs,
      fill = `IgM`,
    ),
    width = 0.9,
    height = 0.9
  )  + 
  geom_tile(
    data = sero_studies_wider,# |> mutate(seroprevalence= factor(riskfactor_name_groups, levels=c(unique(risk_heatmap_data_sero$riskfactor_name_groups), "seroprevalence (%)"))) ,
    aes(
      x = "% Seroprevalence - IgG or IgM",
      y = new.refs,
      fill = `IgG or IgM`,
    ),
    width = 0.9,
    height = 0.9
  )  + 
  geom_tile(
    data = sero_studies_wider,# |> mutate(seroprevalence= factor(riskfactor_name_groups, levels=c(unique(risk_heatmap_data_sero$riskfactor_name_groups), "seroprevalence (%)"))) ,
    aes(
      x = "% Seroprevalence - IgG or IgM",
      y = new.refs,
      fill = `IgG or IgM`,
    ),
    width = 0.9,
    height = 0.9
  ) + 
  geom_tile(
    data = sero_studies_wider,# |> mutate(seroprevalence= factor(riskfactor_name_groups, levels=c(unique(risk_heatmap_data_sero$riskfactor_name_groups), "seroprevalence (%)"))) ,
    aes(
      x = "% Seroprevalence - other",
      y = new.refs,
      fill = `Other`,
    ),
    width = 0.9,
    height = 0.9
  ) + scale_fill_viridis_c(na.value = NA,
    name = "Seroprevalence", option="A"
  ) #+theme(legend.position = "bottom", legend.direction = "vertical")


serology_heatmap_sero

ggsave("serology_heatmap.png", plot = serology_heatmap_sero, width = 14.5, height = 11)
ggsave("serology_heatmap.pdf", plot = serology_heatmap_sero, width = 14.5, height = 11)

# save to read into figure
save(serology_heatmap_sero, file="riskfactor_heatmap.RData")


extreme_sero_refs_low <- sero_studies_wider|>filter(IgG%in%(sort(sero_studies_wider$IgG)[1:5]))|>mutate(extreme=TRUE)
extreme_sero_refs_high <- sero_studies_wider|>filter(IgG%in%(sort(sero_studies_wider$IgG, decreasing=TRUE)[1:5]))|>mutate(extreme=TRUE)

extreme_rf_low <- risk_heatmap_data_sero_refs|>filter(new.refs%in%extreme_sero_refs_low$new.refs)
extreme_rf_high <- risk_heatmap_data_sero_refs|>filter(new.refs%in%extreme_sero_refs_high$new.refs)
extreme_rf_summarise_low <- extreme_rf_low|>group_by(new.refs, riskfactor_significant)|>summarise(count=n())|>pivot_wider(names_from = riskfactor_significant, values_from = count)
extreme_rf_summarise_low[is.na(extreme_rf_summarise_low)]<-0
extreme_rf_summarise_low <- extreme_rf_summarise_low|>mutate(percent_sig = Significant/(Significant+ `Not significant`))
extreme_rf_summarise_high <- extreme_rf_high|>group_by(new.refs, riskfactor_significant)|>summarise(count=n())|>pivot_wider(names_from = riskfactor_significant, values_from = count)
extreme_rf_summarise_high[is.na(extreme_rf_summarise_high)]<-0
extreme_rf_summarise_high <- extreme_rf_summarise_high|>mutate(percent_sig = Significant/(Significant+ `Not significant`))

# top five IgG
mean(extreme_rf_summarise_high$percent_sig)
# bottom five IgG
mean(extreme_rf_summarise_low$percent_sig)
