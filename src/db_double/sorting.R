#' TO DO: make filter_qa and filter_extracted one function (only difference is
#' number of id_name arguments)

library(dplyr)

filter_qa <- function(df, matching = FALSE,
                      exlcude_cols = "NA") {
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
    df <- df %>%
      filter(matching == 0) %>%
      arrange(Covidence_ID) %>%
      mutate(fixed = "NA", .before = "Covidence_ID")
  }
}

filter_extracted <- function(df, matching = FALSE,
                             exlcude_cols = "NA") {
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
    df <- df %>%
      filter(matching == 0) %>%
      arrange(Covidence_ID) %>%
      mutate(fixed = "NA", .before = "Covidence_ID")
  }
  return(df)
}
