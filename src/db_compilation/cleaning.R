# Functions for cleaning data and adding QA scores
library(dplyr)
library(janitor)
library(rio)
library(tidyr)
library(stringr)
library(ids)

##########################
# Main cleaning function #
##########################
clean_params <- function(df, pathogen) {
  df <- df %>%
    mutate(
      # Change variable types
      cfr_ifr_numerator = as.integer(cfr_ifr_numerator),
      population_study_start_day = as.numeric(population_study_start_day),
      method_disaggregated_by = str_replace_all(method_disaggregated_by, ";", ", "),
      population_location = str_replace_all(population_location, ";", ","),
  parameter_type = 
    ifelse(df$parameter_type == "Growth rate ®", "Growth rate (r)",
      ifelse(df$parameter_type == "Reproduction number (Effective; Re)", "Reproduction number (Effective, Re)",
        ifelse(df$parameter_type %in% 
          c("Mutations ‚Äì substitution rate", "Mutations \x96 substitution rate"),
          "Mutations – substitution rate", df$parameter_type
        )
      )
    ), 
      # Group parameters
      parameter_class = case_when(
        grepl("Human delay", parameter_type) ~ "Human delay",
        grepl("Seroprevalence", parameter_type) ~ "Seroprevalence",
        grepl("Mutations", parameter_type) ~ "Mutations",
        grepl("Risk factors", parameter_type) ~ "Risk factors",
        grepl("Reproduction number", parameter_type) ~ "Reproduction number",
        grepl("Severity", parameter_type) ~ "Severity",
        grepl("Mosquito", parameter_type) ~ "Mosquito",
        grepl("Relative", parameter_type) ~ "Relative contribution",
        grepl("Overdispersion", parameter_type) ~ "Overdispersion",
        grepl("Risk factors", parameter_type) ~ "Risk factors",
        grepl("Attack rate", parameter_type) ~ "Attack rate",
        grepl("Doubling time", parameter_type) ~ "Doubling time",
        grepl("Growth rate", parameter_type) ~ "Growth rate",
        TRUE ~ "Other transmission parameters"
      ),
      # Population country
      population_country = str_replace_all(population_country, ";", ","),
      population_country = str_replace(
        population_country, "Congo, Rep.", "Republic of the Congo"
      ),
      population_country = str_replace(
        population_country, "Congo, Dem. Rep.",
        "Democratic Republic of the Congo"
      ),
      population_country = str_replace(
        population_country, "Democratic Republic of the Congo",
        "DRC"
      ),
      population_country = str_replace(
        population_country, "Yuogslavia", "Yugoslavia"
      ),
      population_country = str_replace(
        population_country, "Gambia, The", "The Gambia"
      ),
      population_country = str_replace_all(population_country, ",", ", "),
    
) %>%
    relocate(c(id, parameter_data_id, covidence_id, pathogen)) %>%
    arrange(covidence_id)
  
  df <- df %>%
    mutate(
      # across(
      #   c(
      #     parameter_value, parameter_lower_bound, parameter_upper_bound,
      #     parameter_uncertainty_single_value,
      #     parameter_uncertainty_lower_value,
      #     parameter_uncertainty_upper_value
      #   ),
      #   ~ ifelse(!parameter_class %in% c("Mutations", "Attack rate", "Overdispersion"), round(., digits = 2),
      #     ifelse(parameter_class %in% c("Attack rate", "Overdispersion"), round(., digits = 3), .)
      #   )
      # ),
      # Combine central upper and lower bounds
      parameter_bounds =
        ifelse(!is.na(parameter_lower_bound) & !is.na(parameter_upper_bound),
          paste(parameter_lower_bound, "-", parameter_upper_bound),
          NA
        ),
      # Uncertainty type
      parameter_uncertainty_type = case_when(
        parameter_uncertainty_type %in% "CI95%" ~ "95% CI",
        parameter_uncertainty_type %in% "CRI95%" ~ "95% CrI",
        parameter_uncertainty_type %in% "CI90%" ~ "90% CI",
        parameter_uncertainty_type %in% "CRI90%" ~ "90% CrI",
        parameter_uncertainty_type %in%
          "Highest Posterior Density Interval 95%" ~ "HPDI 95%",
        parameter_uncertainty_type %in%
          "Inter Quartile Range (IQR)" ~ "IQR",
        TRUE ~ parameter_uncertainty_type
      ),
      # Single uncertainty type
      parameter_uncertainty_singe_type = case_when(
        parameter_uncertainty_singe_type %in%
          "Standard deviation (Sd)" ~ "Standard Deviation",
        parameter_uncertainty_singe_type %in%
          "Standard Error (SE)" ~ "Standard Error",
        TRUE ~ parameter_uncertainty_singe_type
      ),

      # Combine uncertainty types and values
      comb_uncertainty_type =
        case_when(
          !is.na(parameter_uncertainty_lower_value) ~
            paste(parameter_uncertainty_type),
          !is.na(parameter_uncertainty_single_value) ~
            paste(parameter_uncertainty_singe_type),
          TRUE ~ NA
        ), comb_uncertainty =
        case_when(
          !is.na(parameter_uncertainty_lower_value) & !is.na(parameter_uncertainty_upper_value) ~
            paste(parameter_uncertainty_lower_value, "-", parameter_uncertainty_upper_value),
          !is.na(parameter_uncertainty_single_value) ~
            paste(parameter_uncertainty_single_value),
          TRUE ~ NA
        ),

      # shorten a longer method_r name
      method_r =
        ifelse(method_r %in% "Renewal equations / Branching process",
          "Branching process", method_r
        )
    )



  df
}
clean_outbreaks <- function(df, pathogen) {
  df <- df %>%
    select(-c("article_id", "outbreak_id", "name_data_entry")) %>%
    relocate(c(id, outbreak_data_id, covidence_id, pathogen)) %>%
    arrange(covidence_id) %>%
    mutate(
      # Format start/stop months for outbreaks
      outbreak_start_month = substring(outbreak_start_month, 1, 3),
      outbreak_end_month = substring(outbreak_end_month, 1, 3),
      # Outbreak country names
      outbreak_country = str_replace(
        outbreak_country, "Congo, Rep.",
        "Republic of the Congo"
      ),
      outbreak_country = str_replace(
        outbreak_country, "Congo, Dem. Rep.",
        "Democratic Republic of the Congo"
      ),
      outbreak_country = str_replace(
        outbreak_country, "Yuogslavia",
        "Yugoslavia"
      )
    )
  df
}
clean_models <- function(df, pathogen) {
  df <- df %>%
    select(-c("article_id", "name_data_entry")) %>%
    relocate(c(id, model_data_id, covidence_id, pathogen)) %>%
    arrange(covidence_id)


  df
}
clean_articles <- function(df, pathogen) {
  df <- df %>%
    select(-c("article_id", "name_data_entry")) %>%
    rename(first_author_surname = first_aauthor_surname) %>%
    relocate(c(
      id, covidence_id, pathogen,
      first_author_first_name, first_author_surname
    )) %>%
    arrange(covidence_id) %>%
    # Add article label
    mutate(
      article_label = as.character(
        paste0(first_author_surname, " ", year_publication)
      )
    )
  df
}

#' Input: article/model/parameter/outbreak data frame to clean
#' Process: clean names, removes old ids and extractor names, adds a grouping
#' variable for classification of the parameter type, fix entry errors
#' output: clean article/model/parameter/outbreak df
#'
clean_dfs <- function(df, pathogen) {
  ####################
  # Article cleaning #
  ####################

  if ("article_title" %in% colnames(df)) {
    df <- clean_articles(df, pathogen)

    ######################################
    # Pathogen-specific article cleaning #
    ######################################
    if (pathogen == "EBOLA") df <- ebola_cleaning(df)
    if (pathogen == "LASSA") df <- lassa_cleaning(df)
    if (pathogen == "SARS") df <- sars_cleaning(df)
  }
  ##################
  # Model cleaning #
  ##################

  if ("model_type" %in% colnames(df)) {
    df <- clean_models(df, pathogen)
    if (pathogen == "EBOLA") ebola_models_cleaning(df)
    if (pathogen == "LASSA") lassa_models_cleaning(df)
    if (pathogen == "SARS") {
      df <- df %>% filter(!is.na(id))

    }

    df <- df %>% select(-c("access_model_id"))
  }

  #####################
  # Outbreak cleaning #
  #####################

  if ("outbreak_id" %in% colnames(df)) {
    df <- clean_outbreaks(df, pathogen)

    if (pathogen == "LASSA") lassa_outbreaks_cleaning(df)
    df <- df %>% select(-c("access_outbreak_id"))
  }







  ######################
  # Parameter cleaning #
  ######################

  if ("parameter_type" %in% colnames(df)) {
    df <- clean_params(df, pathogen)
    if (pathogen == "MARBURG") {
      df <- df %>%
        mutate(
          population_study_start_month = substring(population_study_start_month, 1, 3),
          population_study_end_month = substring(population_study_end_month, 1, 3)
        )
    }

    if (pathogen == "EBOLA") {
      df <- ebola_params_cleaning(df)
      df <- more_ebola_params_cleaning(df)
    }
    if (pathogen == "SARS") {
      df <- sars_params_cleaning(df)
    }
    if (pathogen == "LASSA") {
      df <- lassa_params_cleaning(df)
      df <- more_lassa_cleaning_generic(df)
    }
    df <- select(df, -c("article_id", "access_param_id", "name_data_entry")) %>%
          relocate(c(id, parameter_data_id, covidence_id, pathogen)) %>%
          arrange(covidence_id)
  }


  df
}



#################################
# Add QA scores to article data #
#################################

# input: articles data and parameter data
# output: articles data with two new variables: model_only and article_qa_score

add_qa_scores <- function(articles_df, params_df) {
  # add a model_only variable to article data (as denominator for qa will be different)
  articles_df <- articles_df %>%
    mutate(
      model_only =
        as.numeric(!id %in% params_df$id)
    ) %>%
    # if an article is model_only, make 5-7 NA for consistency between scores
    mutate(
      qa_d5 = ifelse(model_only %in% 1, NA, qa_d5),
      qa_d6 = ifelse(model_only %in% 1, NA, qa_d6),
      qa_d7 = ifelse(model_only %in% 1, NA, qa_d7)
    ) %>%
    # add qa score to article data
    mutate(
      total_qa =
        rowSums(!is.na(
          select(., qa_m1, qa_m2, qa_a3, qa_a4, qa_d5, qa_d6, qa_d7)
        )),
      yes_score = rowSums(
        select(., qa_m1, qa_m2, qa_a3, qa_a4, qa_d5, qa_d6, qa_d7) == "Yes",
        na.rm = TRUE
      ),
      article_qa_score = ifelse(total_qa > 0, yes_score / total_qa * 100, NA)
    ) %>%
    select(-c(total_qa, yes_score))

  articles_df
}
