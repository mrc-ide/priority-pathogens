######################################
# Pathogen-specific ARTICLE cleaning #
######################################

clean_articles <- function(articles) {
  if (pathogen == "EBOLA") {
    articles <- articles %>%
      filter(Article_ID != 14 | Name_data_entry != "Christian") %>%
      # For some reason surname and first name are the wrong way around
      rename(
        temp_col = FirstAuthor_FirstName,
        FirstAuthor_FirstName = FirstAauthor_Surname
      ) %>%
      rename(FirstAauthor_Surname = temp_col) %>%
      filter(!(Covidence_ID %in% c(5349, 1850, 1860, 1863, 2205, 2202, 483))) %>%
      mutate_at(
        vars(QA_M1, QA_M2, QA_A3, QA_A4, QA_D5, QA_D6, QA_D7),
        ~ ifelse(Name_data_entry == "Anne" & Covidence_ID == 6346, "Yes", .)
      ) %>%
      mutate(Pathogen = ifelse(Pathogen == "Sheppard", "Ebola virus",
        ifelse(Pathogen == "Unwin", "Ebola virus",
          Pathogen
        )
      ))
  }
  articles
}

####################################
# Pathogen-specific MODEL cleaning #
####################################

clean_models <- function(models) {
  if (pathogen == "EBOLA") {
    models <- models %>%
      mutate_if(is.character, list(~ na_if(., ""))) %>%
      mutate(Pathogen = ifelse(Pathogen == "Sheppard", "Ebola virus",
        ifelse(Pathogen == "Unwin", "Ebola virus",
          Pathogen
        )
      ))
    model_cols <- colnames(models)
    check_model_cols <- model_cols[!model_cols %in%
      c(
        "Article_ID", "ID", "Pathogen",
        "Covidence_ID", "Name_data_entry",
        "Model_data_ID", "Theoretical_model",
        "Code_available", "access_model_id"
      )]
    models <- models %>%
      filter_at(vars(all_of(check_model_cols)), any_vars(!is.na(.)))
  }
  models
}

########################################
# Pathogen-specific PARAMETER cleaning #
########################################

clean_params <- function(params) {
  if (pathogen == "EBOLA") {
    params$Covidence_ID <- as.numeric(params$Covidence_ID)
    params <- params %>%
      mutate_if(is.character, list(~ na_if(., ""))) %>%
      mutate(Pathogen = ifelse(Pathogen == "Sheppard", "Ebola virus",
        ifelse(Pathogen == "Unwin", "Ebola virus",
          Pathogen
        )
      ))
    # Add parameter type for accidental extractor errors
    params$Parameter_type[
      params$Covidence_ID == "16757" &
        params$Name_data_entry == "Ruth"
    ] <- "Seroprevalence - IgG"
    params$Parameter_type[
      params$Covidence_ID == "4764" &
        params$Name_data_entry == "Kelly M"
    ] <- "Risk factors"
    # Remove remaining blank parameter type entries
    params <- params %>%
      filter_at(vars(Parameter_type), any_vars(!is.na(.)))
    # Remove entries where all variables that aren't prepopulated are empty
    param_cols <- colnames(params)
    check_param_cols <- param_cols[!param_cols %in%
      c(
        "Article_ID", "ID", "Pathogen",
        "Covidence_ID", "Name_data_entry",
        "Parameter_data_ID", "Exponent",
        "Distribution_par1_uncertainty",
        "Distribution_par2_uncertainty",
        "Method_from_supplement",
        "Method_disaggregated",
        "Method_disaggregated_only",
        "Genomic_sequence_available",
        "Inverse_param", "Parameter_FromFigure"
      )]
    params <- params %>%
      filter_at(vars(all_of(check_param_cols)), any_vars(!is.na(.)))
  }
  params
}
