library(cli)
library(readr)
library(uuid)
library(yaml)
library(orderly2)

# *============================= Helper functions =============================*
# *----------------------------- Filter functions -----------------------------*
# Filter out rows that are completely except for having a record_id
filter_empty_rows <- function(df, name){
  filtered_df <-  df[, !colnames(df) %in% "record_id"]
  rows_to_keep <- rowSums(filtered_df!= "" & !is.na(filtered_df)) != 0

  cli_inform(paste0("Keeping ",
                    sum(rows_to_keep), "/", NROW(df),
                    " non-empty rows for ", name))

  return (df[rows_to_keep, ])
}

filter_linked_rows <- function(df, col, name, count_filter_cond=5){
  n_rows <- NROW(df)

  linked_rows <- grepl("\\byes\\b", df[[col]], ignore.case = TRUE)
  value_count_check <- rowSums(df != "" & !is.na(df)) > count_filter_cond

  rows_to_keep = (linked_rows & value_count_check)

  cli_inform(paste0("Keeping ",
                    sum(rows_to_keep), "/", NROW(df),
                    " rows with answers for ", name))

  return (df[rows_to_keep, ])
}

# TODO: filter incomplete rows
# *------------------------------ Key functions -------------------------------*
add_uuid_id <- function(input_df, uuid_col){
  if (NROW(input_df) > 0) {
    input_df[uuid_col] <- replicate(NROW(input_df), UUIDgenerate())
  } else {
    input_df[uuid_col] <- character(0)
  }

  return (input_df)
}

add_pks <- function(input_df, pk_col){
  if (NROW(input_df) > 0) {
    input_df[pk_col] <- 1:NROW(input_df)
  } else {
    input_df[pk_col] <- character(0)
  }
  return(input_df)
}

# *------------------------ Name-replacement functions ------------------------*
col_list_key_map <- function(df, col, mapping_list){
  # assumes all matches are accounted for, wrap in
  mapping_df[col]  <- unlist(sapply(mapping_df[col],
                                    function (key) mapping_list[key]))
  return (mapping_df)
}

df_cols_key_map <- function(input_vec,
                            mapping_df,
                            input_col_name="input_col",
                            target_col_name="target_col"){
  mapping_non_na_df <- mapping_df[!is.na(mapping_df[[target_col_name]]),]
  mapping_vector <- setNames(mapping_non_na_df[[target_col_name]],
                             mapping_non_na_df[[input_col_name]])

  updated_vec <- ifelse(input_vec %in% names(mapping_vector),
                        mapping_vector[input_vec],
                        input_vec)

  names_not_updated <- updated_vec[input_vec %in% updated_vec]
  n_no_updated <- length(names_not_updated)
  if (n_no_updated > 0) {
    cli_alert_info(paste("No mappings found for the following", n_no_updated,"columns: ",
                         paste0(names_not_updated, collapse=", ")))
  }

  return (updated_vec)
}

generate_target_table <- function(input_df_list, mapping_df, target_table,
                                  input_table_col_name="input_table",
                                  target_table_col_name="target_table",
                                  input_col_name = "input_col",
                                  target_col_name = "target_col"){
  cli_inform(paste("Generating table:", target_table))
  filtered_mapping_df <- mapping_df[mapping_df[target_table_col_name] == target_table,]

  input_table_names <- unlist(unique(filtered_mapping_df[input_table_col_name]))

  tables_to_join <- list()

  for (table_name in input_table_names){
    table_filter <- filtered_mapping_df[[input_table_col_name]] == table_name
    table_columns <- filtered_mapping_df[table_filter,][[input_col_name]]

    # select relevant columns
    input_df <- input_df_list[[table_name]]
    input_df <- input_df[, names(input_df) %in% table_columns]
    tables_to_join[[table_name]] <- input_df
  }

  target_df <- Reduce(function(x, y) merge(x, y, by = "record_id", all = TRUE),
                      tables_to_join)

  # record_id may be duplicated
  dedup_mapping_df <- unique(filtered_mapping_df[, c(input_col_name,
                                                     target_col_name)])
  colnames(target_df) <- df_cols_key_map(input_vec=colnames(target_df),
                                         mapping_df=dedup_mapping_df,
                                         input_col_name=input_col_name,
                                         target_col_name=target_col_name)

  return (target_df)
}


# *------------------------- Target format functions --------------------------*
add_article_cols <- function(input_df, article_df, cols_to_add, join_col){
  cols_to_add <- c(cols_to_add, join_col)
  updated_df <- merge(input_df, article_df[cols_to_add], by=join_col,
                      all.x = TRUE)

  return(updated_df)
}

get_pathogen_ids <- function(df, pathogen, id_col, pathogen_col="Pathogen"){
  # Update for Pathogen other
  pathogen_lower_vec <- sapply(df[pathogen_col], tolower)
  record_vec <- df[pathogen_lower_vec==tolower(pathogen), ][[id_col]]
  record_vec <- unique(record_vec)
  return (record_vec)
}

clean_dates <- function(date){
  # Correct possible common user input corrects
  date <- gsub("-{2,}", "-", date)
  date <- gsub("(\\d{4})-(xx)(\\d{2})", "\\1-\\2-\\3", date)
  date <- gsub("(\\d{4})-(\\d{2})(xx)", "\\1-\\2-\\3", date)
  date <- gsub("(\\d{2})(xx)-(\\d{4})", "\\1-\\2-\\3", date)
  date <- gsub("(xx)(\\d{2})-(\\d{4})", "\\1-\\2-\\3", date)

  return (date)
}

split_data_column <- function(input_df, date_col){
  # dynamically determines whether day is in the first or last column
  # assumes month is always second
  input_date_vec <- as.character(input_df[[date_col]])
  input_date_vec <- clean_dates(input_date_vec)

  split_date_df <- do.call(rbind, strsplit(input_date_vec,"-", fixed = TRUE))

  split_date_df[split_date_df == "xx" | is.na(input_date_vec)] <- ""
  col_1_is_year <- grepl("^\\d{4}$", split_date_df[,1])
  base_select_vec <- rep(2, NROW(split_date_df))
  year_offset_vec <- converted_vector <- ifelse(col_1_is_year, -1, 1)

  year_select_vec <- base_select_vec + year_offset_vec
  day_select_vec <- base_select_vec + (year_offset_vec * -1)

  updated_df <- input_df[names(input_df)!=date_col]
  updated_df[paste0(date_col, "_day")] <- sapply(1:length(day_select_vec),
                                                 function (i) split_date_df[i, day_select_vec[i]])
  updated_df[paste0(date_col, "_month")] <- split_date_df[,2]
  updated_df[paste0(date_col, "_year")] <- sapply(1:length(year_select_vec),
                                                  function (i) split_date_df[i, year_select_vec[i]])

  return (updated_df)
}

check_cols_not_mapped <- function(output_df,
                                  mapping_df,
                                  mapping_col_name,
                                  table_col_name,
                                  table_name,
                                  target_map=FALSE){
  output_colnames <- colnames(output_df)

  mapping_filtered_df <- mapping_df[mapping_df[[table_col_name]]==table_name,]

  if (target_map){
    cols_not_mapped_filter <- !(mapping_filtered_df[[mapping_col_name]] %in%
                                  output_colnames)
  }else{
    cols_not_mapped_filter <- (output_colnames %in%
                                 mapping_filtered_df[[mapping_col_name]])
  }

  cols_not_mapped <- (mapping_filtered_df[[mapping_col_name]][cols_not_mapped_filter])

  return (cols_not_mapped)
}

generate_report <- function(df_list,
                            inputs_not_mapped,
                            targets_not_mapped,
                            pathogen){
  # browser()
  # Prepare log file content as a character vector
  table_names <- names(df_list)

  header_string <- paste0("### Report for ", pathogen, " ###")
  pad_length <- max(max(nchar(table_names)), nchar(header_string)-8)
  sprintf_pad <- paste0("%-", pad_length, "s")

  process_time <- Sys.time()
  log_content <- c(toupper(header_string),
                   paste("Processed:", format(process_time, "%Y-%m-%d %H:%M:%S"),
                         "\n"))

  formatted_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  for (table in table_names) {
    pathogen_header <- sprintf(paste0("### ",sprintf_pad," ###"), toupper(table))
    rows_extracted <- paste("Number of rows extracted:", NROW(df_list[[table]]))
    log_content <- c(log_content,
                     pathogen_header,
                     rows_extracted)

    if(length(inputs_not_mapped[[table]]) > 0){
      inm_header <- sprintf(paste0("--- ",sprintf_pad," --"), "Inputs not mapped")
      inm_text <- paste0(sprintf("%2d", seq_along(inputs_not_mapped[[table]])),
                         ". ",
                         inputs_not_mapped[[table]],
                         collapse = "\n")

      log_content <- c(log_content,
                       inm_header,
                       inm_text)
    }

    if(length(targets_not_mapped[[table]]) > 0){
      tnm_header <- sprintf(paste0("\n--- ",sprintf_pad," --"), "Targets not mapped")
      tnm_text <- paste0(seq_along(targets_not_mapped[[table]]),
                         ". ",
                         targets_not_mapped[[table]],
                         collapse = "\n")

      log_content <- c(log_content,
                       tnm_header,
                       tnm_text)
    }

    log_content <- c(log_content,
                     "\n\n")
  }

  # Write to the log file
  filename <- paste0("process_report_",
                     pathogen,
                     "_", format(process_time, "%Y-%m-%d_%H-%M-%S"),
                     ".txt")

  writeLines(log_content, filename)

  return (filename)
}
# *============================================================================*
# *------------------------------ Orderly config ------------------------------*
orderly_parameters(pathogen = NULL)

config_file_path <- file.path("task_inputs", pathogen, "config.yaml")
orderly_resource(c(config_file_path))

# Shared resources are assigned dynamically based on config file, so it is
# loaded in the orderly config section
config_list <- yaml.load_file(config_file_path)

# Note: table_filenames_vec needs to be a named vector, with names corresponding
# to config_list[["table_filepaths"]] names
table_filenames_vec <- sapply(config_list[["table_filepaths"]], basename)
table_filepath_list <- setNames(config_list[["table_filepaths"]],
                                table_filenames_vec)

do.call(orderly_shared_resource, table_filepath_list)

mapping_filename <- basename(config_list[["mapping_filepath"]])

do.call(orderly_shared_resource,
        setNames(config_list["mapping_filepath"], mapping_filename))

target_filename <-  basename(config_list[["target_filepath"]])

do.call(orderly_shared_resource,
        setNames(config_list["target_filepath"], target_filename))


# *---------------------------- Config parameters -----------------------------*
# Required
table_instrument_source_list <- config_list[["instrument_source_table"]]
target_table_names <- config_list[["target_table_names"]]
table_names_vec <- names(config_list[["table_filenames"]]) # main controller keys

# Optional
data_table_names <- config_list[["data_table_names"]]
linked_rows_list <- config_list[["linked_row_col_names"]]
uuid_col_names <- config_list[["uuid_col_names"]]
pk_col_names <- config_list[["pk_col_names"]]
date_cols_to_split <- config_list[["date_cols_to_split"]]
article_cols_to_add <- config_list[["article_cols_to_add"]]

# *------------------------------- Read in data -------------------------------*
df_raw_list <- lapply(table_filenames_vec,function(x) read_csv(x))

mapping_df <- read_csv(mapping_filename)
mapping_df <- col_list_key_map(df=mapping_df,
                               col="input_table",
                               mapping_list=table_instrument_source_list)

target_names_df <- read_csv(target_filename)

# *------------------------- Clean & generate targets -------------------------*
df_clean_list <- Map(filter_empty_rows,
                     df=df_raw_list,
                     name=names(df_raw_list))

# Filter out rows with an Answer of No for linked parameter
if (!is.null(linked_rows_list)){
  df_clean_list[names(linked_rows_list)] <- Map(filter_linked_rows,
                                                df=df_clean_list[names(linked_rows_list)],
                                                col=linked_rows_list,
                                                name=names(linked_rows_list))
}

# extractors quick fix:
extractor_df <- df_clean_list[["extractors"]]

df_clean_list[["extractors"]] <- aggregate(
  extractor_df[, !(names(extractor_df) %in% "record_id")],
  by = list(record_id = extractor_df[["record_id"]]),
  FUN = function(x) paste(unique(na.omit(x)), collapse = ", ")
)



# Filter out irrelevant records
record_ids <- get_pathogen_ids(df=df_clean_list[["extractors"]],
                               pathogen="Oropouche",
                               id_col="record_id",
                               pathogen_col = "pathogen")

df_clean_list <- lapply(df_clean_list,
                        function(df) df[df[["record_id"]] %in% record_ids, ])

target_df_raw_list <- setNames(
  lapply(target_table_names,
         function (target) generate_target_table(input_df_list=df_clean_list,
                                                 mapping_df=mapping_df,
                                                 target_table=target)),
  target_table_names)

# *------------------------------ Process target ------------------------------*
# Split date columns into day, month, year
if (!is.null(date_cols_to_split)){
  for (name in names(date_cols_to_split)){
    cli_inform(paste("Expanding the following ", name, "table date columns:"))
    for (col in date_cols_to_split[[name]]){
      cli_inform(col)
      target_df_clean_list[[name]] <- split_data_column(target_df_raw_list[[name]],
                                                        col)
    }
  }
}

# Add UUID columns
if (!is.null(uuid_col_names)){
  target_df_clean_list <- setNames(
    lapply(target_table_names,
           function(name) add_uuid_id(input_df=target_df_raw_list[[name]],
                                      uuid_col=uuid_col_names[[name]])
           ),
    target_table_names)
}

# Add target article columns
if (!is.null(article_cols_to_add)){
  target_df_clean_list[data_table_names] <- lapply(
    data_table_names,
    function(name) add_article_cols(target_df_clean_list[[name]],
                                    target_df_clean_list[["articles"]],
                                    cols_to_add=article_cols_to_add,
                                    join_col="Article_ID"
    )
  )
}

# Add PK columns
if (!is.null(pk_col_names)){
target_df_clean_list[data_table_names] <- lapply(
  data_table_names,
  function(name) add_pks(input_df=target_df_clean_list[[name]],
                         pk_col=pk_col_names[[name]])
  )
}

# *-------------------------- Create process report ---------------------------*
targets_not_mapped <- Map(
  function (df, name) check_cols_not_mapped(output_df=df,
                                            mapping_df=target_names_df,
                                            mapping_col_name="target_col_name",
                                            table_col_name="target_table",
                                            table_name=name,
                                            target_map=TRUE),
  target_df_clean_list,
  names(target_df_clean_list)
)

inputs_not_mapped <- Map(
  function (df, name) check_cols_not_mapped(output_df=df,
                                            mapping_df=mapping_df,
                                            mapping_col_name="input_col",
                                            table_col_name="target_table",
                                            table_name=name,
                                            target_map=FALSE),
  target_df_clean_list,
  names(target_df_clean_list)
)

log_filename <- generate_report(target_df_clean_list,
                inputs_not_mapped,
                targets_not_mapped,
                pathogen)

orderly_artefact(description="Text file with the results of the extraction process",
                 log_filename)
# *============================================================================*

