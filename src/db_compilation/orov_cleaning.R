orov_cleaning_articles <- function(df) {
  df <- df %>% 
    # dummy entry but marked as OROV 
    filter(
      covidence_id!="999999",
           # duplicate entries for cov ID 30 and 109
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
      paste0(first_aauthor_surname, " ", year_publication)),
      article_preprint = case_when(
        covidence_id == 690 ~ "No",
        covidence_id == 901 ~ "No",
        covidence_id == 665 ~ "No",
    .default = article_preprint
    )
    )
  
  df <- df %>%
    mutate(refs = paste(first_aauthor_surname," (",year_publication,")",sep="")) %>% #define references
    group_by(refs) %>% mutate(counter = row_number()) %>% ungroup() %>% #distinguish same-author-same-year references
    mutate(new_refs = ifelse(refs %in% refs[duplicated(refs)], paste0(sub("\\)$", "", refs),letters[counter],")"), refs)) %>%
    select(-counter,-refs) %>% rename(refs = new_refs)
  
}


orov_cleaning_parameters <- function(df){
  
  df <- df %>% mutate(
    parameter_type_broad = factor(case_when(
      grepl("Infection prevalence",parameter_type) ~ "Infection prevalence",
      parameter_type=="Attack rate" ~ "Attack rate",
      grepl("delay",parameter_type) ~ "Delays",
      grepl("Mutations",parameter_type) ~ "Genomic parameters",
      parameter_type=="Risk factors" ~ "Risk factors",
      grepl("Seroprevalence",parameter_type) ~ "Seroprevalence",
      grepl("Severity",parameter_type) ~ "Severity",
      grepl("Reproduction",parameter_type) ~ "Transmissibility",
      parameter_type=="Delay - human to mosquito generation time" ~ "Delays",
      parameter_type=="Delay - mosquito to human generation time" ~ "Delays"
    )),
    parameter_from_figure = ifelse(parameter_from_figure=="No",
                                   FALSE,
                                   ifelse(is.na(parameter_from_figure),
                                          FALSE,TRUE)),
    parameter_from_figure = ifelse(is.na(parameter_from_figure),
                                   FALSE,parameter_from_figure),
    inverse_param = ifelse(inverse_param=="No",
                                   FALSE,
                                   ifelse(is.na(inverse_param),
                                          FALSE,TRUE)),
    inverse_param = ifelse(is.na(inverse_param),
                           FALSE,inverse_param),
    exponent = case_when(
      is.na(exponent) ~ 0,
      .default = exponent
    ),
    parameter_context_human = case_when(
      covidence_id==65&is.na(parameter_context_human) ~ "Human",
      covidence_id==29&is.na(parameter_context_human) ~ "Both",
      covidence_id==665&is.na(parameter_context_human) ~ "Human",
      !is.na(parameter_context_human) ~ parameter_context_human
    ),
    riskfactor_outcome = case_when(
      riskfactor_outcome == "Presence of Oropouch [Oropouche ONLY]" ~ "Presence of Oropouche",
      covidence_id==9&access_param_id==1 ~ "Presence of Oropouche",
      covidence_id==64&access_param_id==192 ~ "Presence of Oropouche",
      covidence_id==525&access_param_id==315 ~ "Presence of Oropouche",
      covidence_id==525&access_param_id==316 ~ "Presence of Oropouche",
      .default = riskfactor_outcome
      ),
    riskfactor_name = case_when(
      covidence_id==28&access_param_id==68 ~ "Age,Sex,Other",
      covidence_id==128&access_param_id==110 ~ "Sex,Other",
      covidence_id==542&access_param_id==319 ~ "Sex",
      .default = riskfactor_name
    ),
    riskfactor_adjusted = case_when(
      covidence_id==64&access_param_id==192 ~ "Adjusted",
      covidence_id==542&access_param_id==319 ~ "Not adjusted",
      .default = riskfactor_adjusted
    ),
    riskfactor_significant = case_when(
      covidence_id==64&access_param_id==192 ~ "Significant",
      .default = riskfactor_significant
    ),
    parameter_statistical = case_when(
      access_param_id %in% c(330,365,368,379) ~ "Observed sample statistic",
      access_param_id == 321 ~ "Case study [Oropouche ONLY]",
      covidence_id == "571" ~ "Case study [Oropouche ONLY]",
      covidence_id %in% c(682,704) ~ "Observed sample statistic",
      .default = parameter_statistical
    ),
    parameter_value_type = case_when(
      # only applicable to delays anyway
      parameter_statistical=="Case study [Oropouche ONLY]" ~ "Case Study",
      grepl("delay",parameter_type) & is.na(parameter_value_type) ~ "Unspecified",
      grepl("Infection prevalence",parameter_type) & is.na(parameter_value_type) ~ "Central - unspecified",
      grepl("Seroprevalence",parameter_type) & is.na(parameter_value_type) ~ "Central - unspecified",
      .default = parameter_value_type
    ),
    population_sample_type = case_when(
      parameter_context_human=="Not human" ~ "Non-human",
      parameter_context_human!="Not human" ~ population_sample_type
    ),
    population_group = case_when(
      parameter_context_human=="Not human" ~ "Non-human",
      parameter_context_human!="Not human" ~ population_group
    ),
    parameter_type = case_when(
      covidence_id==54&parameter_type=="Human delay - symptom onset>admission to care" ~ "Human delay - other human delay (go to section)",
      covidence_id==571 & parameter_type=="Human delay - admission to care>discharge/recovery" ~ "Human delay - time in care (length of stay)",
      covidence_id%in%c(17,109) & parameter_type == "Human delay - other human delay (go to section)" ~ "Human delay - symptom onset>discharge/recovery",
      .default = parameter_type
    ),
    parameter_hd_from = case_when(
      covidence_id==54&parameter_type=="Human delay - other human delay (go to section)" ~ "Symptom Onset/Fever",
      covidence_id %in% c(208,551,35) & parameter_type=="Human delay - symptom onset>discharge/recovery" ~ NA,
      covidence_id %in% c(571,17,109) & parameter_type_broad=="Human delay" ~ NA,
      covidence_id==718&parameter_type=="Human delay - other human delay (go to section)"&is.na(parameter_hd_from) ~ "Symptom Onset/Fever",
      .default = parameter_hd_from
    ),
    parameter_hd_to = case_when(
      covidence_id==54&parameter_type=="Human delay - other human delay (go to section)" ~ "Seeking Care",
      covidence_id %in% c(208,551,35) & parameter_type=="Human delay - symptom onset>discharge/recovery" ~ NA,
      covidence_id %in% c(571,17,109) & parameter_type_broad=="Human delay" ~ NA,
      covidence_id==718&parameter_type=="Human delay - other human delay (go to section)"&is.na(parameter_hd_to) ~ "Symptom Resolution",
      .default = parameter_hd_to
    ),
    parameter_statistical = case_when(
      covidence_id==571 & parameter_type_broad=="Human delay" ~ "Case study [Oropouche ONLY]",
      .default = parameter_statistical
    ),
    parameter_unit = case_when(
      covidence_id==704&is.na(parameter_unit) ~ "Days",
      .default = parameter_unit
    ),
    population_country = case_when(
      covidence_id==169 & is.na(population_country) & parameter_type=="Seroprevalence - HAI/HI" ~ "Brazil",
      .default = population_country
    ),
    parameter_context_unchanged = case_when(
      covidence_id==321 & is.na(parameter_context_unchanged) ~ "No [context is the same as previous]",
      .default = parameter_context_unchanged
    ),
    parameter_notes = case_when(
      covidence_id==9 & access_param_id==1 ~ "'Other' is 15 environmental variables including annual mean temperature (BIO1); temperature annual range (BIO7); and annual mean specific humidity, other 'MERRAclim' variables, change in vegetation coverage",
      covidence_id==28 & access_param_id==68 ~ "'Other' is specific symptoms",
      covidence_id==28 & access_param_id==69 ~ "'Other' is specific symptoms",
      covidence_id==42 & access_param_id==259 ~ "'Other' is specific symptoms",
      covidence_id==64 & access_param_id==192 ~ "'Other' is vegetation coverage",
      covidence_id==167 & access_param_id==375 ~ "'Other' occupations are those working in agriculture and housewives. 'Other' factors are place of travel, length of residence, and the number of reported malaria episodes",
      covidence_id==128 & access_param_id==110 ~ "'Other' is clinic location",
      covidence_id==179 & access_param_id==388 ~ "'Other' is neighbourhood and contact with pigs",
      covidence_id==128 & access_param_id==110 ~ "'Other' is clinic location",
      covidence_id==521 & access_param_id==233 ~ "based on ~500 genomes sampled; 'Other' is: population density; land use; annual mean temperature; elevation;
 vegetation cover;	Ae. aegypti ecological suitability; land type (e.g. grass, shrub, savannah); agricultural use; forest loss; water occurrence; annual mean precipitation.",
      covidence_id==525 & access_param_id==315 ~ "'Other' is agricultural activities",
      covidence_id==525 & access_param_id==316 ~ "'Other' is agricultural activities",
      covidence_id==179 & access_param_id==389 ~ "'Other' is region",
      covidence_id==179 & access_param_id==390 ~ "'Other' is occupation; type of housing; travel or contact with chickens or rodents. No further details on specific occupation.",
      .default = parameter_notes
    ),
    cfr_ifr_denominator = case_when(
      covidence_id==77 & access_param_id ==199 ~ 113,
      covidence_id==77 & access_param_id ==200 ~ 296,
      covidence_id==77 & access_param_id ==201 ~ 693,
      .default = cfr_ifr_denominator
    )
    ) %>% filter(
      !(access_param_id %in% c(147,149,363,365,368,379,70,198,111,234))
    ) %>%
    filter(
      !(covidence_id %in% c(136,127) & parameter_type=="Attack rate")
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


orov_cleaning_outbreaks <- function(df){
  
  df <- df %>%
    mutate(
      outbreak_case_report = case_when(
        covidence_id %in% c(81,64,28,318,) ~ "No",
        .default = outbreak_case_report
      )
    )
  
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

