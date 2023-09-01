# Task to identify entries of double extracted data that match or do not match 
# between extractors

library(orderly2)
orderly_strict_mode()

orderly_artefact(
  "Double extraction matches and mismatches as csv files",
  c("qa_fixing.csv", "model_fixing.csv", "parameter_fixing.csv",
    "outbreak_fixing.csv", "qa_matching.csv", "model_matching.csv",
    "parameter_matching.csv", "outbreak_matching.csv"))

orderly_resource(c("double_extraction_articles.csv",
                   "double_extraction_params.csv",
                   "double_extraction_models.csv"))

articles <- read_csv("double_extraction_articles.csv")
parameters <- read_csv("double_extraction_params.csv")
models <- read_csv("double_extraction_models.csv")
outbreaks <- NULL


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
        distinct()
    } else if(matching == FALSE) {
      df <- df %>%
        dplyr::filter(matching == 0)
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
        dplyr::distinct()
    } else if(matching == FALSE) {
      df <- df %>%
        dplyr::filter(matching == 0)
    }
  return(df)
}


qa_only <- articles %>% dplyr::select(Covidence_ID,
                                      ID,
                                      Name_data_entry,
                                      starts_with("QA"))

qa_match <- filter_qa(qa_only, matching = TRUE,
                      id_name1 = "Name_data_entry", id_name2 = "ID")
qa_discordant <- filter_qa(qa_only, matching = FALSE,
                           id_name1 = "Name_data_entry", id_name2 = "ID") %>%
  dplyr::mutate(fixed = "NA", .before = "Covidence_ID")

param_match <- filter_extracted(parameters, matching = TRUE,
                                id_name1 = "Name_data_entry", id_name2 = "ID",
                                id_name3 = "Parameter_data_ID", id_name4 = "Article_ID")
param_discordant <- filter_extracted(parameters, matching = FALSE,
                                     id_name1 = "Name_data_entry", id_name2 = "ID",
                                     id_name3 = "Parameter_data_ID", id_name4 = "Article_ID")

model_match <- filter_extracted(models, matching = TRUE,
                                id_name1 = "Name_data_entry", id_name2 = "ID",
                                id_name3 = "Model_data_ID", id_name4 = "Article_ID")
model_discordant <- filter_extracted(models, matching = FALSE,
                                     id_name1 = "Name_data_entry", id_name2 = "ID",
                                     id_name3 = "Model_data_ID", id_name4 = "Article_ID")



needs_fixing <- function(df_disconcordant, df_detail) {
  #' input: dataframe of disconcordant double-extracted values and the details df
  #' process: join the duplicated papers in details with df_dis and order to make the fixing process straightforward
  #' output: rds of the data that needs fixing ordered in a sensible way
  dis_detail <- df_detail %>%
    dplyr::filter(double_extracted == 1) %>% 
    dplyr::filter(covidence_id %in% unique(df_disconcordant$covidence_id)) %>%
    dplyr::arrange(covidence_id)
  dis_detail$fixed = NA
  
  df <- dplyr::full_join(df_disconcordant, dis_detail) %>%
    dplyr::relocate(c(fixed, covidence_id, name_data_entry)) %>%
    dplyr::arrange(covidence_id) 
  return(df)
}




