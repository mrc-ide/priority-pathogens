# Task to compile single and double extraction databases together
library(dplyr)
library(janitor)
library(orderly2)
library(readr)
library(yaml)

# *============================= Helper functions =============================*
read_clean_arrange <- function(table_name, col_types){
  df <- read_csv(table_name, col_types=col_types)
  df <- clean_names(df)
  df <- arrange(df, covidence_id)

  return (df)
}

create_file_list <- function(table_name_list, col_type_list){
  table_list <- lapply(
    names(table_name_list),
    function(table_name)
      read_clean_arrange(table_name_list[[table_name]],
                         col_types = col_type_list[[table_name]])
  )

  table_list <- setNames(table_list,names(table_name_list))

  return (table_list)
}

filter_cols <- function(df, cols_to_remove){
  return (df[!colnames(df) %in% cols_to_remove])
}

join_qa_and_article <- function(article_single_df,
                                article_double_df,
                                qa_matching_df,
                                qa_fixing_df,
                                fixing_join_cols,
                                matching_join_cols){
  article_double_details_df <- select(article_double_df, -c(starts_with("qa")))

  article_matching_df <- left_join(qa_matching_df,
                                article_double_details_df,
                                by = matching_join_cols)
  article_matching_df <- distinct(article_matching_df,
                                  covidence_id, .keep_all = TRUE)
  article_matching_df$double_extracted <- 1

  article_fixed_df <- left_join(qa_fixing_df,
                                article_double_details_df,
                                by = fixing_join_cols)
  article_fixed_df$double_extracted <- 1

  article_single_df$double_extracted <- 0

  article_all_df <- rbind(
    article_single_df,
    article_matching_df,
    article_fixed_df)

  return (article_all_df)
}

join_and_create_extraction_table <- function(single_df,
                                             double_df,
                                             fixed_df,
                                             matching_df,
                                             join_cols){
  double_ids <- double_df[colnames(double_df) %in% join_cols]
  fixed_df <- left_join(fixed_df,
                        double_ids)

  all_df <- rbind(single_df,
                  matching_df,
                  fixed_df)

  return (all_df)
}

# *============================================================================*
# *------------------------------ Orderly config ------------------------------*
orderly_strict_mode()

orderly_parameters(pathogen = NULL)

config_file_path <- file.path(
  "config", paste(tolower(pathogen), "compilation_config.yaml", sep="_"))
orderly_resource(config_file_path)

config_list <- yaml.load_file(config_file_path)

tables <- config_list[["tables"]]
recon <- config_list[["recon"]]
info_table <- "articles"
qa_table <- "qa"
extraction_tables <- tables[tables!=info_table]

csv_artefacts <- sapply(tables,
                    function(table) paste0(table, ".csv"))
rds_artefacts <- sapply(tables,
                        function(table) paste0(table, ".rds"))

double_tables <- sapply(tables,
                    function(table)
                      paste0("double_extraction_", table, ".csv"))

single_tables <- sapply(tables,
                     function(table)
                       paste0("single_extraction_", table, ".csv"))

fixing_files <- sapply(
  recon, function(recon_table) paste(tolower(pathogen), recon_table,
                                     "fixing.csv", sep="_"))
matching_files<- sapply(
  recon, function(recon_table) paste(recon_table, "matching.csv",
                                    sep="_"))

# Get results from db_extraction
orderly_dependency(
  "db_extraction",
  "latest(parameter:pathogen == this:pathogen)",
  c(
    setNames(single_tables,single_tables),
    setNames(double_tables,double_tables)
  )
)

# Get results from db_double
# db_double also produces the fixing files that need to be manually changed and
# supplied as resources below
orderly_dependency(
  "db_double",
  "latest(parameter:pathogen == this:pathogen)",
  setNames(matching_files, matching_files)
)


# Manually fixed files and "cleaning" script - these need to be in the
# src/db_compilation folder
orderly_resource(fixing_files)

## Outputs
orderly_artefact(
  description="Merged single and double extracted data as csv and rds files",
  c(setNames(csv_artefacts, csv_artefacts),
    setNames(rds_artefacts, rds_artefacts)
    )
  )

# *--------------------------- Hard-coded variables ---------------------------*
col_type_list <- list("qa" = cols(Covidence_ID = col_integer()),
                      "articles" = cols(Covidence_ID = col_integer(),
                                        Article_ID = col_integer()),
                      "models" = cols(Covidence_ID = col_integer(),
                                      Article_ID = col_integer(),
                                      access_model_id = col_integer()),
                      "params" = cols(Covidence_ID = col_integer(),
                                      Article_ID = col_integer(),
                                      access_param_id = col_integer()),
                      "outbreaks"= cols(Covidence_ID = col_integer(),
                                        Article_ID = col_integer(),
                                        access_outbreak_id = col_integer()))

double_join_colnames <- list(
  "outbreaks" =  c("article_id", "name_data_entry", "access_outbreak_id",
                   "covidence_id", "id", "outbreak_data_id"),
  "models" = c("article_id", "name_data_entry", "access_model_id",
               "covidence_id", "id", "model_data_id"),
  "params" = c("article_id", "name_data_entry", "access_param_id",
               "covidence_id", "id", "parameter_data_id"),
  "qa_matching" =  c("id", "covidence_id", "name_data_entry"),
  "qa_fixing" = c("covidence_id", "name_data_entry")
)


# *------------------------------- Read in data -------------------------------*
# Single extractions
single_extraction_list <- create_file_list(single_tables, col_type_list)

# Double extractions - matched between extractors
double_extraction_list <- create_file_list(double_tables, col_type_list)

# Matching
matching_list <- create_file_list(matching_files, col_type_list)

#Fixing
fixing_list <- create_file_list(fixing_files, col_type_list)

# *------------------------------- Filter data --------------------------------*
# Matching
matching_list <- lapply(
  matching_list,
  function(matching_df) filter_cols(matching_df,
                                    c("num_rows", "matching")))

# Fixing
fixing_list <- lapply(fixing_list,
                      function(fixing_df) filter(fixing_df, fixed==1))

fixing_list <- lapply(
  fixing_list,
  function(fixing_df) filter_cols(fixing_df,
                                  c("only_notes_mismatch",
                                    "flag_column",
                                    "fixed",
                                    "num_rows",
                                    "matching")))

# *--------------------------- Create final tables ----------------------------*
# Extraction tables
final_tables <- Map(
  function(single_df, double_df, fixed_df, matching_df, table_name)
    join_and_create_extraction_table(single_df,
                                     double_df,
                                     fixed_df,
                                     matching_df,
                                     double_join_colnames[[table_name]]),
  single_extraction_list[extraction_tables],
  double_extraction_list[extraction_tables],
  fixing_list[extraction_tables],
  matching_list[extraction_tables],
  extraction_tables)

# Articles
article_table <- join_qa_and_article(single_extraction_list[[info_table]],
                                     double_extraction_list[[info_table]],
                                     matching_list[[qa_table]],
                                     fixing_list[[qa_table]],
                                     double_join_colnames[["qa_fixing"]],
                                     double_join_colnames[["qa_matching"]])

final_tables[[info_table]] <- article_table

for (table in names(csv_artefacts)){
  write_csv(final_tables[[table]], csv_artefacts[table])
  saveRDS(final_tables[[table]], rds_artefacts[table])
}
