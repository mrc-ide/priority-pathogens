# Early cleaning of easily identifiable errors

############################################
# Add missing Covidence IDs before joining #
############################################

fix_cov_ids <- function(articles) {
  if (pathogen == "EBOLA") {
    articles$Covidence_ID[articles$DOI == "10.1016/j.rinp.2020.103593" &
      articles$Name_data_entry == "Christian"] <- 19880
    articles$Covidence_ID[articles$DOI == "10.1142/s1793524517500577" &
      articles$Name_data_entry == "Thomas Rawson"] <- 11565
    articles$Covidence_ID[articles$DOI == "10.1038/nature14594" &
      articles$Name_data_entry == "Ettie"] <- 5197
  }
  articles
}

######################################
# Pathogen-specific ARTICLE cleaning #
######################################

clean_articles <- function(articles) {
  articles$Year_publication <- as.numeric(articles$Year_publication)

  if (pathogen == "EBOLA") {
    articles <- articles %>%
      filter(Article_ID != 14 | Name_data_entry != "Christian") %>%
      # For some reason surname and first name are the wrong way around
      rename(
        temp_col = FirstAuthor_FirstName,
        FirstAuthor_FirstName = FirstAauthor_Surname
      ) %>%
      rename(FirstAauthor_Surname = temp_col) %>%
      mutate(
        FirstAauthor_Surname = case_when(
          Name_data_entry == "Cyril" & Covidence_ID == 2921 ~ "Leroy",
          Name_data_entry == "Patrick" & Covidence_ID == 6472 ~ "Emond",
          TRUE ~ FirstAauthor_Surname
        )) %>%
      mutate(
        FirstAuthor_FirstName = case_when(
          Name_data_entry == "Cyril" & Covidence_ID == 2921 ~ "Eric M.",
          Name_data_entry == "Patrick" & Covidence_ID == 6472 ~ "RT",
          TRUE ~ FirstAuthor_FirstName
        )
      ) %>%
      filter(!(Covidence_ID %in% c(
        5349, 1850, 1860, 1863, 2205, 2202, 483,
        5870, 12100
      ))) %>%
      mutate_at(
        vars(QA_M1, QA_M2, QA_A3, QA_A4, QA_D5, QA_D6, QA_D7),
        ~ ifelse(Name_data_entry == "Anne" & Covidence_ID == 6346, "Yes", .)
      ) %>%
      mutate(Pathogen = ifelse(Pathogen == "Sheppard", "Ebola virus",
        ifelse(Pathogen == "Unwin", "Ebola virus",
          Pathogen
        )
      )) %>%
      mutate(
        Year_publication = case_when(
          Name_data_entry == "Christian" & Covidence_ID == 23390 ~ 2020,
          Name_data_entry == "Christian" & Covidence_ID == 1586 ~ 1999,
          Name_data_entry == "Ruth" & Covidence_ID == 4969 ~ 2016,
          TRUE ~ Year_publication
        )
      )

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
    params <- params %>%
      mutate_if(is.character, list(~ na_if(., ""))) %>%
      mutate(Pathogen = ifelse(Pathogen == "Sheppard", "Ebola virus",
        ifelse(Pathogen == "Unwin", "Ebola virus",
          Pathogen
        )
      ))
    # Add missing parameter type variables
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
