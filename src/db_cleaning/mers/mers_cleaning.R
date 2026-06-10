# *=========================== Manual MERS cleaning ===========================*
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


  #Paper 12025 has NA as surname, replace as "Korea CDC"
  df[is.na(article_df$first_author_surname),
     "first_author_surname"] <- "Korea CDC"

  return (df)
}

model_cleaning <- function(df){
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


  ## Covidence #12025 has an error, we need to set the risk factor outcome to "Death" for all 4:
  df[(df$covidence_id==12025) & (df$parameter_type=="Risk factors"),
     "riskfactor_outcome"] <- "Death"

  #Kim (2015) has extracted the lower bounds incorrectly
  df[(df$access_param_id == "055_001"),
     "parameter_2_lower_bound"] <- 0
  df[(df$access_param_id == "055_002"),
     "parameter_2_lower_bound"] <- 0
  df[(df$access_param_id == "055_002"),
     "population_group"] <- "Healthcare workers"

  #As is this central value, they've taken a subset rather than the overall average attack rate
  df[(df$access_param_id == "029_003"),
     "parameter_value"] <- 0.39

  # 229_003 has the wrong "genome" info.
  # They wrote "GenBank", change to "Unspecified"
  df[(df$access_param_id == "229_003"),
     "genome_site"] <- "Unspecified"

  ## 320_002 has population_group as NA
  ## This breaks forest_plot, so we change it to "Unspecified"
  df[df$access_param_id=="320_002",
             "population_group"] <- "Unspecified"

  #390_002 and 390_003 are "per days", we have to change their parameter_unit to "Days" to not break forest_plot
  df[df$access_param_id=="390_002",
             "parameter_unit"] <- "Days"
  df[df$access_param_id=="390_003",
             "parameter_unit"] <- "Days"

  #271-001 is a gamma distribution but with reported mean 6.99 (unspecified units)
  #It's also low-QA. For now I'm going to manually change it to days though
  df[df$access_param_id=="271_001",
             "parameter_unit"] <- "Days"

  #032-001 is also "Unspecified"
  #It's a decent study, but never explicity SAYS "Days"
  df[df$access_param_id=="032_001",
             "parameter_unit"] <- "Days"

  #255_014 lists a delay of -5.9 days. That's because this individual had symptom onset AFTER hospital admission
  # Let's convert this to an "Other" human delay.
  #parameters <- filter(parameters, access_param_id != "255_014")
  #test <- filter(parameters, access_param_id == "255_014")

  df[df$access_param_id=="255_014",
             "parameter_type"] <- "Human delay - other human delay (go to section)"
  df[df$access_param_id=="255_014",
             "other_delay_start"] <- "Admission to hospital"
  df[df$access_param_id=="255_014",
             "other_delay_end"] <- "symptom onset"
  df[df$access_param_id=="255_014",
             "parameter_value"] <- -1*df[df$access_param_id=="255_014",
                                                 "parameter_value"]


  # 037_004 should be removed. It just says a patient "died within 2 weeks", not clear enough to extract:
  df <- filter(df, access_param_id != "037_004")

  #360_010 has been extracted as an "other" human delay, but it's hospital length of stay.
  df[(df$access_param_id == "360_010"),
     "parameter_type"] <- "Human delay - time in care (length of stay)"

  #228_007 is incorrectly marked in the parameter_type
  df[(df$access_param_id == "228_007"),
     "parameter_type"] <- "Human delay - other human delay (go to section)"
  #035_001 is incorrectly marked in the parameter_type
  df[(df$access_param_id == "035_001"),
     "parameter_type"] <- "Human delay - other human delay (go to section)"

  #141_011, 164_020, 164_019 needs rewording it's other human delays:
  df[(df$access_param_id == "141_011"),
     "other_delay_start"] <- "Viral RNA detected"
  df[(df$access_param_id == "141_011"),
     "other_delay_end"] <- "viral RNA clearance"
  df[(df$access_param_id == "164_020"),
     "other_delay_start"] <- "Viral RNA detected"
  df[(df$access_param_id == "164_020"),
     "other_delay_end"] <- "viral RNA clearance"
  df[(df$access_param_id == "164_019"),
     "other_delay_start"] <- "Viral RNA detected"
  df[(df$access_param_id == "164_019"),
     "other_delay_end"] <- "viral RNA clearance"

  #298_001 and _002 are "isolation duration"s
  df[(df$access_param_id == "298_001"),
     "other_delay_start"] <- "Isolation"
  df[(df$access_param_id == "298_001"),
     "other_delay_end"] <- "End of isolation"
  df[(df$access_param_id == "298_002"),
     "other_delay_start"] <- "Isolation"
  df[(df$access_param_id == "298_002"),
     "other_delay_end"] <- "End of isolation"
  #401_003 is strange, trying to capture a range of PRNT50 values,
  #It shouldn't be extracted
  df <- filter(df, access_param_id != "401_003")

  #064_004 is including an upper value of infinity. This was a modelling scenario and should be ignored as a value
  # delete 064_004, and change the boundaries of 064_002
  df <- filter(df, access_param_id != "064_004")
  df[(df$access_param_id == "064_002"),
     "parameter_lower_bound"] <- 0.019
  df[(df$access_param_id == "064_002"),
     "parameter_upper_bound"] <- 0.078

  # 391_001 is serology of blood donors, but pop_group is currently NA
  df[(df$access_param_id == "391_001"),
     "population_group"] <- "General population"

  return (df)
}
# *============================================================================*
