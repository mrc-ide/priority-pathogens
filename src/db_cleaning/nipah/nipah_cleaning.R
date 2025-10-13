# *========================== Manual Niaph cleaning ===========================*
library(tidyr)

article_cleaning <- function(df){
  # Update Not Applicable so that epireview assign_qa_score works
  df[df$covidence_id==207, "first_author_first_name"] <- "Khean Jin"
  df[df$covidence_id==207, "first_author_surname"] <- "Goh"

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
  return (df)
}

outbreak_cleaning <- function(df){

  df <- df %>%
    mutate(outbreak_location = str_to_title(outbreak_location),
           outbreak_location = str_replace_all(outbreak_location, " And", ";" ),
           outbreak_location = str_replace_all(outbreak_location, ",", ";" ),
           outbreak_location = str_replace_all(outbreak_location, "Kerela", "Kerala" ),
           outbreak_location = case_when(outbreak_location == "Serembna Hospital" ~ "Seremban Hospital",
                                         outbreak_location == "University Of Malaya Medical Center" ~ "University Of Malaya Medical Centre",
                                         outbreak_location == "Suspected To Be Infected With The Virus From Pig Farms Situated In Bukit Pelandok" ~ "Bukit Pelandok" ,
                                         TRUE ~ outbreak_location))

  return (df)
}

param_cleaning <- function(df){
  na_replacement <- list(parameter_from_figure = "No",
                         inverse_param = "No",
                         exponent=0,
                         exponent_2=0,
                         parameter_value_type="Unspecified")

  df <- replace_na(df, na_replacement)

  no_true_false <- c("parameter_from_figure",
                     "inverse_param")

  df[no_true_false] <- lapply(no_true_false,
                              function(col) df[[col]] != "No")

  df <- df |>
    mutate(population_location = str_to_title(population_location),
           population_location = str_replace_all(population_location, " And", ";"),
           population_location = str_replace_all(population_location, ",", ";"),
           population_location = str_replace_all(population_location, "[(]Bangladesh[)]", ";"),
           population_location = str_replace_all(population_location, "[(]India[)]", ";"),
           population_location = str_replace_all(population_location, "Serembna Hospital", "Seremban Hospital"),
           population_location = str_replace_all(population_location, "Kerela", "Kerala"),
           population_location = str_replace_all(population_location, "Suspected To Be Infected With The Virus From Pig Farms Situated In Bukit Pelandok", "Bukit Pelandok"),
           population_location = str_trim(population_location, side = "both")
    )

  # TODO: Decide whether to remove other_delay_*
  df <- df |>
    mutate(parameter_hd_from = ifelse(parameter_hd_from %in% c("Other", ""),
                                      other_delay_start, parameter_hd_from),
           parameter_hd_to = ifelse(parameter_hd_to %in% c("Other", ""),
                                    other_delay_end, parameter_hd_to),
           parameter_value_type = ifelse(parameter_value_type=="Central - unspecified",
                                         "Unspecified", parameter_value_type)
           ) |>
    select(-c(other_delay_start, other_delay_end))

  # TODO: Derived rows such as parameter_bounds should be created after cleaning
  # Transmission
  # R0
  # CovID 1030
  # Row extracted as variability paired parameter - paired range should be
  Basic_R_1030_filter <- ((df$covidence_id == 1030) &
                            (df$parameter_type=="Reproduction number (Basic R0)"))
  filtered_row <- df[Basic_R_1030_filter, ]
  filtered_row[,"parameter_lower_bound"] <- filtered_row[, "parameter_2_sample_paired_lower"]
  filtered_row[,"parameter_upper_bound"] <- filtered_row[, "parameter_2_sample_paired_upper"]
  filtered_row[,"parameter_bounds"] <- filtered_row[, "parameter_2_sample_bounds"]
  filtered_row[,"parameter_paired"] <- NA

  filtered_row[grepl("parameter_2", colnames(filtered_row))] <- NA

  df[Basic_R_1030_filter, ] <- filtered_row

  Basic_R_3051_filter <- ((df$covidence_id == 3051) &
                           (df$parameter_type=="Reproduction number (Basic R0)"))
  df[Basic_R_3051_filter, "population_group"] <- "Persons under investigation"

  # Substitution rates
  # CovID 2760
  evolutionary_rate_2760_filter <- ((df$covidence_id == 2760) &
                                      (df$parameter_type=="Mutations - substitution rate"))

  # Parameter bounds for the below will be incorrect
  # Convert to exp -4
  df[evolutionary_rate_2760_filter, "parameter_value"] <- 11
  df[evolutionary_rate_2760_filter, "parameter_uncertainty_lower_value"] <- 7.37
  df[evolutionary_rate_2760_filter, "parameter_uncertainty_upper_value"] <- 15
  df[evolutionary_rate_2760_filter, "exponent"] <- -4
  df[evolutionary_rate_2760_filter, "genome_site"] <- "Nucleocapsid"

  # CovID 3058 substitution rate
  evolutionary_rate_3058_filter <- ((df$covidence_id == 3058) &
                                      (df$parameter_type=="Mutations - substitution rate"))
  df[evolutionary_rate_3058_filter, "parameter_uncertainty_lower_value"] <- 2.9
  df[evolutionary_rate_3058_filter, "parameter_uncertainty_upper_value"] <- 6
  df[evolutionary_rate_3058_filter, "genome_site"] <- "Nucleocapsid"

  df[(df$covidence_id==1171) & (df$parameter_type=="Mutations - evolutionary rate"),
     "genome_site"] <- "Nucleocapsid"

  df[(df$covidence_id==906) & (df$parameter_type=="Overdispersion"),
     "parameter_unit"] <- "Max. nr. of cases superspreading (related to case)"

  # Proportion symptomatic
  prop_symp_filter <- df$parameter_type=="Severity - proportion of symptomatic cases"

  # CovID: 906
  prop_symp_906_filter <- (df$covidence_id==906 & prop_symp_filter)

  # Value is 0 based on CFR but not yet assigned
  df[prop_symp_906_filter, "parameter_value"] <- 100
  df[prop_symp_906_filter, "inverse_param"] <- FALSE
  df[prop_symp_906_filter, "parameter_notes"] <-
    paste0("Proportion asymptomatic is reported in the paper.",
           "Denominator is contacts of Nipah patients who gave blood specimen")

  # CovID: 3025
  prop_symp_3025_filter <- (df$covidence_id==3025 & prop_symp_filter)

  df[prop_symp_3025_filter, "parameter_value"] <- 100 - df[prop_symp_3025_filter,
                                                           "parameter_value"]
  df[prop_symp_3025_filter, "parameter_notes"] <- ("Proportion asymptomatic is
                                                  reported in the paper")

  # CovID: 828
  df[df$covidence_id==828, "population_group"] <- "Persons under investigation"

  # CovID: 2886
  df[df$covidence_id==2886 &
       df$parameter_type=="Seroprevalence - IgG", "cfr_ifr_denominator"] <- 18

  # CovID: 207, Nadir delay filter
  nadir_delay_filter <- (
    df$covidence_id==207 &
      df$parameter_type=="Human delay - other human delay (go to section)"
    )

  df[nadir_delay_filter, "parameter_notes"] <-
  paste0("Nadir defined as the worst conscious level or the need for,
         mechanical ventilation")

  return (df)
}
# *============================================================================*
