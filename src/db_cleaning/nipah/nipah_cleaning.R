# *========================== Manual Niaph cleaning ===========================*
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

  df <- df |>
    mutate(parameter_hd_from = ifelse(parameter_hd_from %in% c("Other", ""),
                                      other_delay_start, parameter_hd_from),
           parameter_hd_to = ifelse(parameter_hd_to %in% c("Other", ""),
                                    other_delay_end, parameter_hd_to),
           parameter_value_type = ifelse(parameter_value_type=="Central - unspecified",
                                         "Unspecified", parameter_value_type)
           )

  return (df)
}
# *============================================================================*
