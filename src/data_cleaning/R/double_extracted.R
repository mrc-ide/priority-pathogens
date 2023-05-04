## identifying double extracted entries

# details df contains the covidence ID, article ID and extractor name from the article dataframe
find_double_extraction <- function(dataframe, details_df) {
  double_id <- details_df$covidence_id[duplicated(details_df$covidence_id) 
                                              & !duplicated(details_df$article_id)]
  
  double_extracted <- dataframe %>%
    dplyr::filter(covidence_id %in% double_id)
  
  return(double_extracted)
}
