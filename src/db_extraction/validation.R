# check that none of the fields contain a comma
# Implement other checks here.
validate <- function(df) {
 
  mutate(df, across(everything(), function(x) {
    gsub(pattern = ",", replacement = ";", x = x)
  }))
    
}

# Check for generic article errors
validate_articles <- function(article_df) {
  # 1) Check for duplicate article entries by the same extractor
  article_dupes <- article_df[
    (duplicated(article_df[c("Covidence_ID","Name_data_entry")]) |
       duplicated(article_df[c("Covidence_ID","Name_data_entry")],
                  fromLast = TRUE)), ]
  if(nrow(article_dupes) > 0) {
    warning("There are duplicate article entries by the same extractor")
  }
  # 2) Check for missing covidence IDs
  if(any(is.na(articles$Covidence_ID))) {
    missing_ids <- articles[is.na(articles$Covidence_ID),]
    warning("There are article entries missing Covidence IDs")
  } else {
    missing_ids <- NA
  }
  ## Returning data.frames so that people can process programmatically if needed.
  list(
    duplicate_articles = article_dupes,
    missing_covidence_ids = missing_ids
  )
}

# Check for generic model errors. Doesn't need to accept pathogen as an argument
# becase orderly makes it available in the parent scope.
validate_models <- function(model_df) {
  # Check for empty model entries
  if(pathogen == 'EBOLA'){
    check_model_cols <- c("Model_type", "Compartmental_type", "Stoch_Deter",
                          "Interventions_type", "Transmission_route",
                          "Assumptions", "Ebola variant")  
  } else {
    check_model_cols <- c("Model_type", "Compartmental_type", "Stoch_Deter",
                          "Interventions_type", "Transmission_route",
                          "Assumptions")  
  }
  
  check_empty_models <- model_df %>%
    filter_at(vars(all_of(check_model_cols)), all_vars(is.na(.)))
  if(nrow(check_empty_models) > 0) {
    warning("There are empty model entries")
  }
  check_empty_models
}

# Check for generic model errors
validate_outbreaks <- function(outbreak_df) {
  # Check for empty model entries
  if(pathogen == 'LASSA'){
    check_outbreak_cols <- c("Outbreak_start_day", "Outbreak_start_month", "Outbreak_start_year",
                             "Outbreak_end_day", "Outbreak_end_month", "Outbreak_date_year",
                          "Outbreak_country", "Outbreak_location", "Cases_confirmed", "Cases_mode_detection",
                          "Deaths", "Pre-Outbreak")  
    
    check_empty_outbreaks <- outbreak_df %>%
      filter_at(vars(all_of(check_outbreak_cols)), all_vars(is.na(.)))
    if(nrow(check_empty_outbreaks) > 0) {
      warning("There are empty model entries")
    }
    check_empty_outbreaks
  } 
}

# Check for generic parameter errors
validate_params <- function(param_df) {
  ## Check for empty parameter entries
  param_cols <- colnames(param_df)
  check_param_cols <- param_cols[! param_cols %in%
                                   c("Article_ID", "ID", "Pathogen",
                                     "Covidence_ID", "Name_data_entry",
                                     "Parameter_data_ID", "Exponent",
                                     "Distribution_par1_uncertainty",
                                     "Distribution_par2_uncertainty",
                                     "Method_from_supplement",
                                     "Method_disaggregated",
                                     "Method_disaggregated_only",
                                     "Genomic_sequence_available",
                                     "Inverse_param", "Parameter_FromFigure")]
  check_empty_params <- param_df %>%
    filter_at(vars(all_of(check_param_cols)), all_vars(is.na(.)))
  if(nrow(check_empty_params) > 0) {
    warning("There are empty parameter entries")
  }
  # Check for entries where parameter type has not been entered (check if empty
  # and should be removed or if this was accidentally missed by the extractor)
  check_param_type <- param_df %>% filter(is.na(param_df$Parameter_type))
  if(nrow(check_param_type) > 0) {
    warning("The 'Parameter_type' variable is empty in some parameter entries")
  }
  list(
    empty_params = check_empty_params,
    empty_param_type = check_param_type
  )
}
