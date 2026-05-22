# *=========================== Manual RVF cleaning ===========================*
library(tidyr)

article_cleaning <- function(df){
  
  # Update Not Applicable so that epireview assign_qa_score works
  qa_cols <- c("qa_m1", "qa_m2", "qa_a3", "qa_a4", "qa_d5", "qa_d6", "qa_d7")
  df <- df |>
    mutate(across(all_of(qa_cols), ~ ifelse(.=="Not Applicable", NA, .)))
  
  # Leave first_author_first_name as is incase there are initials as the first
  # name
  df <- df |>
    mutate(journal = str_to_title(journal),
           first_author_surname = str_to_title(first_author_surname)) |>
    mutate(journal = sub("^The\\s+", "", journal, useBytes = TRUE))
  
  return (df)
}

model_cleaning <- function(df){
  # fix model type
  df$model_type[which(df$covidence_id==1669)] <- "Compartmental"
  
  return (df)
}

outbreak_cleaning <- function(df){

  return (df)
}

param_cleaning <- function(df){
  
  na_replacement <- list(parameter_from_figure = "No",
                         inverse_param = "No",
                         exponent=0,
                         parameter_value_type="Unspecified")
  
  df <- replace_na(df, na_replacement)
  
  no_true_false <- c("parameter_from_figure",
                     "inverse_param")
  
  df[no_true_false] <- lapply(no_true_false,
                              function(col) df[[col]] != "No")
  
  # Removed parameter_hd_to, parameter_hd_from since these were introduced in
  # Redcap and somewhat duplicates capturing non standard human delays.
  # The field other_delay_* is included in epireview, so combine columns and
  # keep other_delay_*
  df <- df |>
    mutate(other_delay_start = ifelse(
      parameter_hd_from=="Other" | is.na(parameter_hd_from),
      other_delay_start, parameter_hd_from),
      other_delay_end = ifelse(
        parameter_hd_to=="Other" | is.na(parameter_hd_to),
        other_delay_end, parameter_hd_to),
      parameter_value_type = ifelse(parameter_value_type=="Central - unspecified",
                                    "Unspecified", parameter_value_type)
    ) |>
    select(-c(parameter_hd_to, parameter_hd_from))
  
  df <- df |>
    mutate(parameter_upper_bound = ifelse(
      parameter_upper_bound=="infinity", NA, parameter_upper_bound),
      parameter_upper_bound=as.numeric(parameter_upper_bound))
  
  #########################################################
  # Some rows have text in the population_sample_size column.
  # Move the text to the notes column and convert the column to a numeric as intended.
  # Needs to be numeric for downstream tasks.
  
  # Covidence 1168 to be removed. No sampling time risk factor.
  df <- df[-which(df$population_sample_size=="various"),]
  
  df <- df %>%
    mutate(
      is_numeric = is.na(population_sample_size) |
        grepl("^\\s*\\d+(\\.\\d+)?\\s*$", population_sample_size),
      parameter_notes = if_else(
        !is_numeric,
        paste0(
          coalesce(parameter_notes, ""),
          if_else(is.na(parameter_notes) | parameter_notes == "", "", " "),
          "population_sample_size: ",
          population_sample_size
        ),
        parameter_notes
      ),
      population_sample_size = if_else(
        is_numeric,
        as.numeric(population_sample_size),
        NA_real_
      )
    ) %>%
    select(-is_numeric)
  
  #######################################################################
  ### Some dates entered as "XX" or "00" instead of "x" when unknown. 
  
  df <- df %>%
    mutate(population_study_start_day=
             case_when(toupper(population_study_start_day)=="XX" ~ NA,
                       population_study_start_day=="00" ~ NA,
                       TRUE ~ population_study_start_day),
           population_study_end_day=
             case_when(toupper(population_study_end_day)=="XX" ~ NA,
                       population_study_end_day=="00" ~ NA,
                       TRUE ~ population_study_end_day),
           population_study_start_month=
             case_when(toupper(population_study_start_month)=="XX" ~ NA,
                       TRUE ~ population_study_start_month),
           population_study_end_month=
             case_when(toupper(population_study_end_month)=="XX" ~ NA,
                       TRUE ~ population_study_end_month),
           population_study_start_year=
             case_when(toupper(population_study_start_year)=="XXXX" ~ NA,
                       TRUE ~ population_study_start_year),
           population_study_end_year=
             case_when(toupper(population_study_end_year)=="XXXX" ~ NA,
                       TRUE ~ population_study_end_year)
    )
  
  ### fix typos in dates 
  df$population_study_start_year[which(df$covidence_id==1038)] <- 2012
  df$population_study_start_year[which(df$covidence_id==6385)] <- 2019
  
  
  #########################################################################
  ## Fixing exponents in attack rate parameter
  
  # article 643 notes say per 100,000 people so needs -5 exponent - attack rate
  df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- -5
  df$parameter_value[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- df$parameter_value[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] *10^(df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))])*100
  df$parameter_lower_bound[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- df$parameter_lower_bound[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] *10^(df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))])*100
  df$parameter_upper_bound[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- df$parameter_upper_bound[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] *10^(df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))])*100
  df$parameter_uncertainty_lower_value[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- df$parameter_uncertainty_lower_value[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] *10^(df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))])*100
  df$parameter_uncertainty_upper_value[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- df$parameter_uncertainty_upper_value[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] *10^(df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))])*100
  df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- 0
  df$parameter_unit[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- "Percentage (%)"
  
  
  ##############################################################################
  ## Fix incorrect population group for covidence 5255
  
  df$population_group[which(df$covidence_id==5255)] <- "Persons under investigation"
  
  ##############################################################################
  ## Create delay name for "Other human delay" 
  
  # Update parameter_type names for "Other human delay"
  df <- mutate(df, 
               parameter_type= case_when(parameter_type=="Human delay - other human delay (go to section)" ~ 
                                           paste(tolower(other_delay_start), ">", tolower(other_delay_end), sep=""),
                                   .default=parameter_type))
 
   # Symptom Onset/Fever to Discharge or death - covidence 1767 -  "Other" population group is 
  # HIV positive individuals
  df <-mutate(df, 
              population_group=case_when((population_group=="Other")&tolower(parameter_type) %in% 
                                           c('symptom onset/fever>discharge or death') ~
                                           "Persons with confirmed HIV",
      .default = population_group))
  
  
  
  #########################################################################################
  # Calculate central value from CFR/IFR/Attack rate nom and denom if parameter value is NA
  
  df <- mutate(df, 
               parameter_value = case_when(is.na(parameter_value)&!is.na(cfr_ifr_numerator)&!is.na(cfr_ifr_denominator) ~ (cfr_ifr_numerator/cfr_ifr_denominator)*100,
                                               .default = parameter_value), 
               parameter_unit = case_when(is.na(parameter_value)&!is.na(cfr_ifr_numerator)&!is.na(cfr_ifr_denominator) ~ "Percentage (%)",
                                           .default = parameter_unit), 
               parameter_statistical_approach = case_when(is.na(parameter_statistical_approach) ~ "Unspecified",
                                                          .default = parameter_statistical_approach))
  

  
  return (df)
}
# *============================================================================*