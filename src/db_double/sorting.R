#' TO DO: make filter_qa and filter_extracted one function (only difference is
#' number of id_name arguments)

library(dplyr)

filter_table <- function(df,
                         matching = FALSE,
                         exlcude_cols = "NA",
                         notes_col=NULL,
                         id_col=NULL) {
  #' input: data frame
  #' process: matching determines whether it'll give matches (w/0 duplicates) 
  #' or disconcordant dfs
  #' output: desired data frame based on the parameters
  df <- df %>%
    group_by(Covidence_ID) %>%
    group_by(across(-c(all_of(exlcude_cols)))) %>%
    mutate(num_rows = sum(n())) %>%
    ungroup() %>%
    mutate(matching = ifelse(num_rows == 2, 1, 0))

  if(matching == TRUE) {
    df <- df %>% 
      filter(matching == 1) %>%
      distinct() %>%
      arrange(Covidence_ID)

  } else if(matching == FALSE) {
    not_matching_df <- df %>%
      filter(matching == 0) %>%
      arrange(Covidence_ID) %>%
      mutate(fixed = "NA", .before = "Covidence_ID")

    if (!is.null(notes_col) & !is.null(id_col)){
      exlcude_no_notes <- c(exlcude_cols, notes_col)

      no_notes_matching_ids <- df %>%
        group_by(Covidence_ID) %>%
        group_by(across(-c(all_of(exlcude_no_notes)))) %>%
        mutate(num_rows = sum(n())) %>%
        ungroup() %>%
        filter(num_rows == 2) |>
        pull(all_of(id_col))

      not_matching_df <- not_matching_df |>
        mutate(only_notes_mismatch=FALSE,
               .after = "fixed")

      only_notes_mismatch_filter <- (not_matching_df[[id_col]] %in%
                                       no_notes_matching_ids)
      not_matching_df[only_notes_mismatch_filter, "only_notes_mismatch"] <- TRUE
      }

    df <- not_matching_df
    }
  return(df)
}
