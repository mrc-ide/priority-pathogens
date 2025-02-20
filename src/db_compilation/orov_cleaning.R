orov_cleaning_articles <- function(df) {
  df <- df %>% 
    # dummy entry but marked as OROV 
    filter(
      covidence_id!="999999",
           # duplicate entries for 30 and 109 - the ID gets regenerated each run so not applicable for filtering
           #id!="6e85a97a-c427-4eef-b0e8-f55142802ef1",
           #id!="23d7b6a2-bbbe-4f94-984f-da9fd0bab2d5"
           !(article_id %in% c(104,59))
           ) %>%
    mutate(
      year_publication = case_when(
        year_publication == 16 & covidence_id ==114 ~ 2021,
        covidence_id != 114 ~ year_publication
      ),
      volume = case_when(
        covidence_id == 114 ~ 16,
        covidence_id != 114 ~ volume
      ),
      issue = case_when(
        covidence_id == 114 ~ 2,
        covidence_id != 114 ~ issue
      ),
      article_label = as.character(
      paste0(first_aauthor_surname, " ", year_publication))
      )
}


orov_cleaning_parameters <- function(df){
  
  df <- df %>% mutate(
    parameter_type_broad = factor(case_when(
      grepl("Infection prevalence",parameter_type) ~ "Infection prevalence",
      parameter_type=="Attack rate" ~ "Attack rate",
      grepl("Human delay",parameter_type) ~ "Human delay",
      grepl("Mutations",parameter_type) ~ "Genomic parameters",
      parameter_type=="Risk factors" ~ "Risk factors",
      grepl("Seroprevalence",parameter_type) ~ "Seroprevalence",
      grepl("Severity",parameter_type) ~ "Severity"
    )),
    parameter_context_human = case_when(
      covidence_id==65&is.na(parameter_context_human) ~ "Human",
      covidence_id==29&is.na(parameter_context_human) ~ "Both",
      !is.na(parameter_context_human) ~ parameter_context_human
    ),
    riskfactor_outcome = case_when(
      riskfactor_outcome == "Presence of Oropouch [Oropouche ONLY]" ~ "Presence of Oropouche",
      riskfactor_outcome != "Presence of Oropouch [Oropouche ONLY]" ~ riskfactor_outcome
      ),
    parameter_statistical = case_when(
      access_param_id %in% c(330,365,368,379) ~ "Observed sample statistic",
      access_param_id==321 ~ "Case study [Oropouche ONLY]",
      !(access_param_id %in% c(321,330,365,368,379)) ~ parameter_statistical
    ),
    population_sample_type = case_when(
      parameter_context_human=="Not human" ~ "Non-human",
      parameter_context_human!="Not human" ~ population_sample_type
    ),
    population_group = case_when(
      parameter_context_human=="Not human" ~ "Non-human",
      parameter_context_human!="Not human" ~ population_group
    )
    ) %>% filter(
      !(access_param_id %in% c(147,149))
    ) %>%
    filter(
      !(covidence_id %in% c("136","127") & parameter_type=="Attack rate")
    )

  
  # logic to sort the missing parameter contexts 
  for(i in 1:nrow(df)){
    
    if(is.na(df$parameter_context_unchanged[i])|df$parameter_context_unchanged[i]=="Yes [first time filling out context OR context is different]"){
      
    } else if(df$parameter_context_unchanged[i]=="No [context is the same as previous]"){
      
      df$population_sex[i] = df$population_sex[i-1]
      df$population_sample_size[i] = df$population_sample_size[i-1]
      df$parameter_context_sample_type[i] = df$parameter_context_sample_type[i-1]
      df$population_sample_type[i] = df$population_sample_type[i-1]
      df$population_group[i] = df$population_group[i-1]
      df$population_age_min[i] = df$population_age_min[i-1]
      df$population_age_max[i] = df$population_age_max[i-1]
      df$population_country[i] = df$population_country[i-1]
      df$population_location[i] = df$population_location[i-1]
      df$parameter_context_timing[i] = df$parameter_context_timing[i-1]
      df$parameter_context_zika[i] = df$parameter_context_zika[i-1]
      df$parameter_context_urban[i] = df$parameter_context_urban[i-1]
      df$parameter_context_data[i] = df$parameter_context_data[i-1]
      df$population_study_start_day[i] = df$population_study_start_day[i-1]
      df$population_study_start_month[i] = df$population_study_start_month[i-1]
      df$population_study_start_year[i] = df$population_study_start_year[i-1]
      df$population_study_end_day[i] = df$population_study_end_day[i-1]
      df$population_study_end_month[i] = df$population_study_end_month[i-1]
      df$population_study_end_year[i] = df$population_study_end_year[i-1]
    }
    
    
  }
  
  return(df)
  
}

# test <- parameter_all %>% orov_cleaning_parameters()
# 
# for(i in 1:nrow(test)){
#   
#   if(is.na(test$parameter_context_unchanged[i])|test$parameter_context_unchanged[i]=="Yes [first time filling out context OR context is different]"){
#     
#   } else if(test$parameter_context_unchanged[i]=="No [context is the same as previous]"){
#     
#     test$population_sex[i] = test$population_sex[i-1]
#     test$population_sample_size[i] = test$population_sample_size[i-1]
#     test$parameter_context_sample_type[i] = test$parameter_context_sample_type[i-1]
#     test$population_sample_type[i] = test$population_sample_type[i-1]
#     test$population_group[i] = test$population_group[i-1]
#     test$population_age_min[i] = test$population_age_min[i-1]
#     test$population_age_max[i] = test$population_age_max[i-1]
#     test$population_country[i] = test$population_country[i-1]
#     test$population_location[i] = test$population_location[i-1]
#     test$parameter_context_timing[i] = test$parameter_context_timing[i-1]
#     test$parameter_context_zika[i] = test$parameter_context_zika[i-1]
#     test$parameter_context_urban[i] = test$parameter_context_urban[i-1]
#     test$parameter_context_data[i] = test$parameter_context_data[i-1]
#     test$population_study_start_day[i] = test$population_study_start_day[i-1]
#     test$population_study_start_month[i] = test$population_study_start_month[i-1]
#     test$population_study_start_year[i] = test$population_study_start_year[i-1]
#     test$population_study_end_day[i] = test$population_study_end_day[i-1]
#     test$population_study_end_month[i] = test$population_study_end_month[i-1]
#     test$population_study_end_year[i] = test$population_study_end_year[i-1]
#   }
#  
# }
# 



## sort anything that says OROV only in the title 



# risk factors 
# cov id 136
# adjusted and significant can stay 
# not adjusted & sig - age, occupation, sex, other
# not adjusted & not sig - occupation, other
## remove: other, not sig, not adj - 147
## remove: age, sex, sig, not adj - 149

