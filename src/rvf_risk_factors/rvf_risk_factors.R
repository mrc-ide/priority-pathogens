# RVF risk factors

library(tidyverse)
library(forcats)
library(orderly)

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


##### Unpick "Other" risk factor specifics here #####

## Write .csv with "other" risk factors 
risk_params_others <- risk_params|>filter(grepl("Other",riskfactor_name))

# save data set for fixing 
write.csv(risk_params_others,row.names = FALSE,"risk_params_others.csv")

# read in fixed version 
risk_params_others_FIXED <- read_csv("risk_params_others_FIXED.csv")

## clean up risk factor names
# replace ; with , so lists match
risk_params_others_FIXED <- risk_params_others_FIXED |> mutate(other_names=gsub("; ", ",", other_names, fixed = TRUE))
# combine orginal risk_factor names and fixed names for others
risk_params_others_FIXED <- risk_params_others_FIXED |> mutate(riskfactor_names_all=ifelse(is.na(other_names),riskfactor_name,paste(riskfactor_name, other_names, sep=",")))
# If other has been fixed then remove from list 
risk_params_others_FIXED <- risk_params_others_FIXED |> mutate(riskfactor_names_all=ifelse(is.na(other_names),riskfactor_names_all,gsub("Other,", "", riskfactor_names_all, fixed = TRUE)))
# De-duplicate list 
risk_params_others_FIXED <- risk_params_others_FIXED |>
  mutate(
    riskfactor_names_all = sapply(riskfactor_names_all, function(x) {
      paste(unique(trimws(strsplit(x, ",")[[1]])), collapse = ",")
    })
  )

risk_params_other_clean <- risk_params_others_FIXED |> mutate(riskfactor_name=riskfactor_names_all, riskfactor_names_all=NULL, other_names = NULL)

risk_params_clean <- rbind(risk_params|>filter(!grepl("Other",riskfactor_name)), risk_params_other_clean)


# what is happening in terms of number of obs per paper
risk_params_clean %>% group_by(covidence_id) %>% 
  summarise(n = length(covidence_id)) %>% 
  arrange(-n)




#### Tables of risk factor counts ############

risk_table <- risk_params_clean %>%
  separate_longer_delim(riskfactor_name, delim = ",") %>%
  mutate(population_sample_size = replace_na(as.double(population_sample_size),0),
         riskfactor_adjusted    = case_when(riskfactor_adjusted=='' ~ 'Unspecified',
                                            TRUE ~ riskfactor_adjusted)) %>%
  group_by(riskfactor_outcome,riskfactor_name,riskfactor_significant,riskfactor_adjusted,population_group) %>%
  summarise(n=n(),
            pop_size = sum(population_sample_size)) %>%
  unite(`Significant / Adjusted`,riskfactor_significant:riskfactor_adjusted, remove = FALSE, sep = " / ") %>% arrange(-n)

#text_size <- 20
custom_colours <- c('Significant / Adjusted'='blue4', 'Significant / Not adjusted' = 'lightblue', 'Significant / Unspecified'='blue',
                    'Not significant / Adjusted'='darkred', 'Not significant / Not adjusted' = 'pink', 'Not significant / Unspecified'='red',
                    'Unspecified / Adjusted'='grey30', 'Unspecified / Not adjusted' = 'grey50', 'Unspecified / Unspecified'='grey70')



# Serology
risk_table_plt_serology <- risk_table %>% filter(riskfactor_outcome=='Serology') %>%
  ggplot(aes(x=riskfactor_name,y=n,col=`Significant / Adjusted`, fill=`Significant / Adjusted`)) +
  geom_bar( stat='identity',position = position_dodge(preserve = "single")) +
  theme_light()+
  labs(title="Serology")+
  scale_color_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) + xlab('') + ylab('')  +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, size = text_size ),
         strip.text = element_text( color = "black"),
         strip.background =element_rect(fill="grey90"),
         text = element_text(size = text_size),
         legend.position = 'bottom')

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

riskfactor_outcome_plots <- cowplot::plot_grid(risk_table_plt_infection,
                   risk_table_plt_serology,
                   rel_heights = c(1,1),
                   nrow=2)

ggsave(riskfactor_outcome_plots, file="riskfactor_outcome_plots.pdf", height=15, width=12)

## how many of each of the different outcomes
risk_params_clean %>% select(riskfactor_outcome) %>% group_by(riskfactor_outcome) %>%
  summarise(length(riskfactor_outcome))


# how many adjusted
risk_params_clean %>% select(riskfactor_adjusted) %>% group_by(riskfactor_adjusted) %>%
  summarise(length(riskfactor_adjusted))

# whats happening with infection
risk_params_clean %>% filter(riskfactor_outcome=="Infection")

# whats happening with serology
risk_params_clean %>% filter(riskfactor_outcome=="Serology")

# what is happening with occupation
risk_params_clean %>% filter(grepl("Occupation",riskfactor_name))

# how is quality looking 
risk_params_clean %>% select(qa_score) %>% unlist() %>% mean()


####### summarise risk factors 

ggplot(risk_params_clean)+
  geom_bar(aes(y=population_country, fill=riskfactor_outcome))+
  xlab("Risk factor count") + ylab(NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "left")


ggplot(risk_params_clean)+
  geom_bar(aes(y=population_country, fill=population_sample_type))+
  xlab("Risk factor count") + ylab(NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "left")


ggplot(risk_params_clean)+
  geom_bar(aes(y=population_sample_type, fill=riskfactor_outcome))+
  xlab("Risk factor count") + ylab(NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "left")

ggplot(risk_params_clean)+
geom_bar(aes(y=population_group, fill=riskfactor_outcome))+
  xlab("Risk factor count") + ylab(NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "left")


## want to have a table where we have the risk factors individually, and tell us how many times it appears and in which of the 4 categorisations
#risk_table$riskfactor_name %>% unique()
# Age, Sex, Occupation, Symptoms, Environmental factors, Time, Location, Agricultural activities, comorbidities
unique_risk_factors <- unique(unlist(strsplit(risk_params_clean$riskfactor_name, ",")))
risk_table <- risk_params_clean %>% mutate(
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

## Save all risk params
write.csv(risk_params_clean,row.names = FALSE,"risk_factors_for_writing.csv")


# Heatmap
library(tidyverse)
# replace occupation with occupation name
risk_params_clean_occupation <- risk_params_clean %>%
  mutate(riskfactor_name_occupation = str_replace(riskfactor_name, "Occupation", riskfactor_occupation))
risk_params_clean_occupation$riskfactor_name_occupation[which(risk_params_clean_occupation$access_param_id=="630_005")] <-
 "Age groups,Location (geographic),Settlement type,Education,Formal occupation,Travel,Vector prevention"
  

# Reshape data
risk_heatmap_data <- risk_params_clean_occupation %>%
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
    riskfactor_name_groups = case_when( riskfactor_name_occupation %in% c("Household contact", "Close contact") ~ "Human contact",
                                            
                                            riskfactor_name_occupation %in% c(" Abattoir services/butchery", "Abattoir services") ~ "Abattoir services/butchery",
                                        
                                            riskfactor_name_occupation %in% c(" Veterinary", "Veterinary") ~ "Veterinary",
                                            
                                           riskfactor_name_occupation %in% c("Other", "Unspecified") ~ "Other occupation",
                                        
                                           riskfactor_name_occupation %in% c("Livestock/animal herders and farm workers", "Livestock and animal herders") ~ "Livestock/animal herders and farm workers",
                                        
                                            riskfactor_name_occupation %in% c("Age", "Age groups") ~ "Age",
                                            
                                            riskfactor_name_occupation %in% c("Recent illness", "PCR positive", "HIV status", "Q-fever co-infection", "Brucellosis co-infection",
                                                                   "Yellow fever vaccine", "Outcome of Malaria parasite test", "Recovery following malaria treatment", "Comorbidity",
                                                                   "Patient outcome", "Healthcare", "Birthplace", "Ethnicity", "Education", "Socioeconomic status") ~ "Personal risk factors",
                                            
                                            riskfactor_name_occupation %in% c("Working hours", "Job duties", 
                                                                   "Time in work", "Permanance of job contract", "Job training","Wear PPE", "Farm type", "Formal occupation") ~ "Occuaption related",
                                            
                                            riskfactor_name_occupation %in% c("Time of residency", "Housing with electricity", "Housing material", "Roofing material", "Settlement type",
                                                                   
                                                                   "Seminomadic", "Water from public well", "Distance to water source", "Household size") ~ "Housing related",
                                            
                                            riskfactor_name_occupation %in% c("Temperature", "Rainfall", "Humidity", "River discharge", "Water source", "Dams on farm") ~ "Environmental",
                                            
                                            riskfactor_name_occupation %in% c("Animal slaughter", "Contact with carcass/aborted fetus","Disposed of carcass/aborted fetus",
                                                                   "Handling raw meat/cooking","Animal autopsy","Animal birthing","Handling raw meat","Skinned animal"
                                                                   ) ~ "Contact with animal material",
                                           riskfactor_name_occupation %in% c("Sleeping outside with herd", "Milking", "Housing with animal water basins", "Contact with animal",
                                                                             "Type of livestock", "Livestock density", "Graze with wild animals", "Milk source") ~ "Contact with animal",
                                            
                                            
                                            .default = riskfactor_name_occupation
      
    )
  )


all_outcomes_heatmap <- 
  ggplot(risk_heatmap_data,
       aes(x = riskfactor_name_groups,
           y = refs)) +
  
  geom_point(
    aes(
      size = population_sample_size,
      fill=`Significant / Adjusted`
    ),
    shape = 21,
    colour = "grey30",
    alpha = 0.8
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
  ) + facet_grid(
    rows = vars(riskfactor_outcome),
    scales = "free_y",
    space = "free_y") +
  theme(
      strip.placement = "outside",
      strip.background = element_blank()
    )

all_outcomes_heatmap
    
    
risk_heatmap_data_sero <- risk_heatmap_data|>filter(riskfactor_outcome=="Serology")

risk_heatmap_data_sero <- risk_heatmap_data_sero |> mutate(urefs = make.unique(refs), riskfactor_name_groups= factor(riskfactor_name_groups, levels=c(unique(risk_heatmap_data_sero$riskfactor_name_groups), seroprevalence))) 

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
  )
    
serology_heatmap <-  ggplot(risk_heatmap_data_sero|>filter(riskfactor_adjusted=="Adjusted"),
           aes(x = riskfactor_name_groups,
               y = refs)) +
      
      geom_point(
        aes(
          size = sample_size_na,
          fill=`Significant / Adjusted`,
          colour=`Significant / Adjusted`,
          shape = sample_size_missing
        ),
        alpha = 0.65
      ) +
      scale_colour_manual(values = custom_colours) +
      scale_fill_manual(values = custom_colours) +
      scale_shape_manual(
        name = "Sample size",
        values = c(
          "Reported" = 21,  # circle
          "Unknown" = 24    # triangle
        )) +
      scale_size_continuous(
        range = c(2, 10),
        name = "Sample size",
      ) +
      labs(
        x = "Risk factor",
        y = "Reference",
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
      ) + facet_wrap(
        ~ population_sample_type,
        scales = "free_y",
       space = "free_y"
      )
    
serology_heatmap


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
sero_studies <- sero_studies |>
  mutate(parameter_value = coalesce(parameter_value,central),
         parameter_unit="Percentage (%)",
         population_group = factor(
           population_group,
           levels = c(sort(setdiff(unique(population_group),
                                   c("Other", "Unspecified"))),
                      "Other", "Unspecified")))


prevalence_dataframe <- risk_heatmap_data_sero|>mutate(seroprevalence=0)
  for(i in 1: nrow(prevalence_dataframe)){
    sero_data <- filter(sero_studies, covidence_id==prevalence_dataframe$covidence_id[i])
    if(nrow(sero_data)==1){
      # if there is one seroprevalence rate use it 
      prevalence_dataframe$seroprevalence[i] <- as.numeric(sero_data$parameter_value) 
    }else if(nrow(sero_data)==0){
      # if there is no seroprevalnce put NA
      prevalence_dataframe$seroprevalence[i] <- NA
    }else{
      # If there are different tests priorities IgG then IgM
      if(length(sero_data$parameter_type)==length(unique(sero_data$parameter_type))){
        sero_data2 <- filter(sero_studies, covidence_id==prevalence_dataframe$covidence_id[i], parameter_type == "Seroprevalence - IgG")
        if(nrow(sero_data2)==1){
          # if there is one seroprevalence rate use it 
          prevalence_dataframe$seroprevalence[i] <- as.numeric(sero_data2$parameter_value) 
        }else if(nrow(sero_data2)>1){
          # if there is no seroprevalnce put NA
         
        }
      }else{
        
        print(paste(sero_data$covidence_id, sero_data$parameter_type, sep=", ") )
        
      }
    }
    
  }

library(ggnewscale)
serology_heatmap <- serology_heatmap + new_scale_fill() + 
geom_tile(
  data = prevalence_dataframe|>filter(riskfactor_adjusted=="Adjusted"),# |> mutate(seroprevalence= factor(riskfactor_name_groups, levels=c(unique(risk_heatmap_data_sero$riskfactor_name_groups), "seroprevalence (%)"))) ,
  aes(
    x = "% Seroprevalence",
    y = refs,
    fill = seroprevalence
  ),
  width = 0.9,
  height = 0.9
) + scale_fill_viridis_c(
    name = "Percentage", option="A"
  )


ggsave("serology_heatmap.png", plot = serology_heatmap, width = 10, height = 15)
ggsave("serology_heatmap.pdf", plot = serology_heatmap, width = 10, height = 15)

# save to read into figure
save(serology_heatmap, file="riskfactor_heatmap.RData")


