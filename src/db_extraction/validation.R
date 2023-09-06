# check that none of the fields contain a comma
# Implement other checks here.
validate <- function(df) {
  out <- apply(df, 2, function(x) {
    gsub(pattern = ",", replacement = ";", x = x)
  })
  as.data.frame(out)
}

# Check for generic article errors
validate_articles <- function(article_df) {
  # 1) Check for duplicate article entries by the same extractor
  article_dupes <- article_df[
    (duplicated(article_df[c("Covidence_ID","Name_data_entry")]) |
       duplicated(article_df[c("Covidence_ID","Name_data_entry")],
                  fromLast = TRUE)), ]
  if(nrow(article_dupes) > 0) {
    ## SB notes: suggest warning instead of an error.
    warning("There are duplicate article entries by the same extractor")
  }
  # 2) Check for missing covidence IDs
  if(any(is.na(articles$Covidence_ID))) {
    missing_ids <- articles[is.na(articles$Covidence_ID),]
    ## SB notes: suggest warning instead of an error.
    warning("The above article entry or entries are missing Covidence IDs")
  }
  ## Suggest returning data.frames so that people can process
  ## programatically if needed.
  list(
    duplicate_articles = article_dupes,
    missing_covidence_ids = missing_ids
  )
}

# Check for generic model errors
validate_models <- function(model_df) {
                                        # Check for empty model entries
  ## TODO Replace the numbers with column names, numeric indices are
  ## fragile.
  check_model_cols <- colnames(model_df)[c(7:9, 11, 13:ncol(model_df))]
  check_empty_models <- model_df %>%
    filter_at(vars(all_of(check_model_cols)), all_vars(is.na(.)))
  if(nrow(check_empty_models) > 0) {
    warning("The above model entries are empty")
  }
  check_empty_models
}

# Check for generic parameter errors
validate_params <- function(param_df) {
  ## Check for empty parameter entries
  ## Same comments as above.
  check_param_cols <- colnames(param_df)[7:ncol(param_df)]
  check_param_cols <- check_param_cols[! check_param_cols %in%
                                         c('Exponent',
                                           'Distribution_par1_uncertainity',
                                           'Distribution_par2_uncertainty',
                                           'Method_from_supplement',
                                           'Method_disaggregated',
                                           'Method_disaggregated_only',
                                           'Genomic_sequence_available',
                                           'Inverse_param',
                                           'Parameter_FromFigure')]
  check_empty_params <- param_df %>%
    filter_at(vars(all_of(check_param_cols)), all_vars(!is.na(.)))
  if(nrow(check_empty_params) > 0) {
    warning("The above parameter entries are empty")
  }
  # Check for entries where parameter type has not been entered (check if empty
  # and should be removed or if this was accidentally missed by the extractor)
  check_param_type <- param_df %>% filter(is.na(param_df$Parameter_type))
  if(nrow(check_param_type) > 0) {
    stop("The 'Parameter_type' variable is empty in the above parameter entries")
  }
  list(
    empty_params = check_empty_params,
    empty_param_type = check_param_type
  )
}
