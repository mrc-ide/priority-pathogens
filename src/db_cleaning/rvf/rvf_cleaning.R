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
  
  # 391_001 is serology of blood donors, but pop_group is currently NA
  df[(df$access_param_id == "391_001"),
     "population_group"] <- "General population"
  
  return (df)
}
# *============================================================================*