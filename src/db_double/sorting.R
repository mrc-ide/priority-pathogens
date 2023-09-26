#' TO DO: make filter_qa and filter_extracted one function (only difference is
#' number of id_name arguments)

filter_qa <- function(df, matching = FALSE,
                      id_name1 = "NA", id_name2 = "NA") {
  df <- df %>%
    dplyr::group_by(Covidence_ID) %>%
    dplyr::group_by(across(-c(all_of(id_name1), all_of(id_name2)))) %>%
    dplyr::mutate(num_rows = sum(n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(matching = ifelse(num_rows == 2, 1, 0))
  if(matching == TRUE) {
    df <- df %>% 
      dplyr::filter(matching == 1) %>%
      distinct() %>%
      dplyr::arrange(Covidence_ID)
  } else if(matching == FALSE) {
    df <- df %>%
      dplyr::filter(matching == 0) %>%
      dplyr::arrange(Covidence_ID) %>%
      dplyr::mutate(fixed = "NA", .before = "Covidence_ID")
  }
}

filter_extracted <- function(df, matching = FALSE,
                             id_name1 = "NA", id_name2 = "NA",
                             id_name3 = "NA", id_name4 = "NA") {
  #' input: data frame
  #' process: matching determines whether it'll give matches (w/0 duplicates) 
  #' or disconcordant dfs
  #' output: desired data frame based on the parameters
  df <- df %>%
    dplyr::group_by(Covidence_ID) %>%
    dplyr::group_by(across(-c(all_of(id_name1), all_of(id_name2),
                              all_of(id_name3), all_of(id_name4)))) %>%
    dplyr::mutate(num_rows = sum(n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(matching = ifelse(num_rows == 2, 1, 0))
  
  if(matching == TRUE) {
    df <- df %>% 
      dplyr::filter(matching == 1) %>%
      dplyr::distinct() %>%
      dplyr::arrange(Covidence_ID)
  } else if(matching == FALSE) {
    df <- df %>%
      dplyr::filter(matching == 0) %>%
      dplyr::arrange(Covidence_ID) %>%
      dplyr::mutate(fixed = "NA", .before = "Covidence_ID")
  }
  return(df)
}
