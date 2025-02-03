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

  rows_to_keep <- (linked_rows & value_count_check)

  cli_inform(paste0("Keeping ",
                    sum(rows_to_keep), "/", NROW(df),
                    " rows with answers for ", name))

  return (df[rows_to_keep, ])
}

filter_incomplete_rows <- function(df, incomplete_df, name){
  record_match <- df[["record_id"]] %in% incomplete_df[["record_id"]]
  redcap_repeat_match <- df[["redcap_repeat_instance"]] %in% incomplete_df[["redcap_repeat_instance"]]

  rows_to_keep <- !(record_match & redcap_repeat_match)

  cli_inform(paste0("Keeping ",
                    sum(rows_to_keep), "/", NROW(df),
                    " complete rows for ", name))

  return (df[rows_to_keep,])
}

filter_record_ids <- function(df, record_id_vec, pathogen_filter, name){
  record_match <- df[["record_id"]] %in% record_id_vec

  rows_to_keep <- !record_match

  cli_inform(paste0("Keeping ",
                    sum(rows_to_keep), "/", NROW(df), " ", pathogen_filter,
                    " rows with complete article and qa information for ",
                    name))

  return (df[rows_to_keep,])
}
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

get_not_pathogen_ids <- function(df, pathogen, id_col, pathogen_col="Pathogen"){
  # Update for Pathogen other
  pathogen_lower_vec <- sapply(df[pathogen_col], tolower)
  record_vec <- df[pathogen_lower_vec!=tolower(pathogen), ][[id_col]]
  record_vec <- unique(record_vec)
  return (record_vec)
}

pretty_format_incomplete_text <- function(incomplete_df){
  incomplete_reduced_df <- aggregate(
    incomplete_df[, "redcap_repeat_instance"],
    by = list(record_id = incomplete_df[["record_id"]]),
    FUN = function(x) paste(unique(na.omit(x)), collapse = ", ")
  )

  incomplete_combined_text <- apply(
    incomplete_reduced_df, MARGIN=1,
    function(row) paste(row["record_id"],
                        row["redcap_repeat_instance"],
                        sep=" | ")
  )

  return (incomplete_combined_text)
}

get_incomplete_rows <- function(df, col, name, incomplete_key="Incomplete"){
  incomplete_df <- df[df[[col]]==incomplete_key,
                      c("record_id", "redcap_repeat_instance")]

  if (NROW(incomplete_df)>0){
    incomplete_combined_text <- pretty_format_incomplete_text(incomplete_df)
    cli_alert_warning(
      c(paste("The following", sub("s$", "", name),
              "rows are incomplete (record id | redcap repeat instance):"),
        incomplete_combined_text)
      )
  }

  return (incomplete_df)
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

create_comment_header <- function(header_text, decorator_char, header_length,
                                  newline=FALSE){
  if (header_text == ""){
    header <- paste0(decorator_char, decorator_char)
  }else{
    # paste to add spaces between decorator char and header text
    header <- paste(decorator_char, header_text, decorator_char)
  }

  pre_post_pad <- (header_length / 2 - nchar(header) / 2) - 2

  pre_post_pad <- floor(pre_post_pad)

  decorator_char_pad <- paste0(rep(decorator_char, pre_post_pad), collapse="")
  header <- paste0(decorator_char_pad, header, decorator_char_pad)

  if ((header_length - nchar(header_text)) %% 2 != 0){
    header <- paste0(header, decorator_char)
  }

  if (newline){
    header <- paste0("\n", header)
  }

  return (header)
}

generate_report <- function(df_list,
                            incomplete_df_list,
                            inputs_not_mapped,
                            targets_not_mapped,
                            pathogen){
  # Prepare log file content as a character vector
  table_names <- names(df_list)
  header_length <- 75

  create_comment_header
  header_string <- create_comment_header(paste0("Report for ", pathogen),
                                         decorator_char="=",
                                         header_length)

  process_time <- Sys.time()
  log_content <- c(toupper(header_string),
                   paste("Processed:", format(process_time, "%Y-%m-%d %H:%M:%S"),
                         "\n"))

  formatted_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  # The incomplete report code has a dependency on the input name
  record_extractor_list <- setNames(
    as.list(df_list[["articles"]][["Name_data_entry"]]),
    df_list[["articles"]][["Article_ID"]])
  write_warning_header <- TRUE
  for (name in names(incomplete_rows_df_list)){
    if (NROW(incomplete_rows_df_list[[name]]) > 0){
      if (write_warning_header){
        write_warning_header <- FALSE
        warning_header <- paste0(
        "!WARNING!",
        "\nThe following rows are incomplete and were excluded from the analysis.",
        "\n(Reminder: if QA or article data is incomplete, all rows corresponding",
        "\nto that RecordID will be excluded, including data extraction rows.)",
        "\nRows are reported as: record id | redcap repeat instance | extractor")
        log_content <- c(log_content, warning_header)
      }
      incomplete_df <- incomplete_rows_df_list[[name]]

      incomplete_combined_text <- pretty_format_incomplete_text(incomplete_df)
      unique_incomplete_record_ids <- unique(incomplete_df[["record_id"]])
      incomplete_combined_text <- sapply(
        1:NROW(unique_incomplete_record_ids),
        function(i) paste(incomplete_combined_text[i],
                          record_extractor_list[[unique_incomplete_record_ids[i]]],
                          sep =" | ")
             )
      incomplete_combined_text <- paste(incomplete_combined_text, collapse = "\n")
      table_name_text <- paste0("\n",sub("s$", "", name), " rows")
      log_text <- paste0(table_name_text, ":\n",
                         incomplete_combined_text)
      log_content <- c(log_content, log_text)
    }
  }

  for (table in table_names) {
    pathogen_header <- create_comment_header(toupper(table),
                                             decorator_char="=",
                                             header_length,
                                             newline=TRUE)
    rows_extracted <- paste("Number of rows extracted:", NROW(df_list[[table]]))
    log_content <- c(log_content,
                     pathogen_header,
                     rows_extracted)

    if(length(inputs_not_mapped[[table]]) > 0){
      inm_header <- create_comment_header("Inputs not mapped",
                                          decorator_char="-",
                                          header_length,
                                          newline=TRUE)
      inm_text <- paste0(sprintf("%2d", seq_along(inputs_not_mapped[[table]])),
                         ". ",
                         inputs_not_mapped[[table]],
                         collapse = "\n")

      log_content <- c(log_content,
                       inm_header,
                       inm_text)
    }

    if(length(targets_not_mapped[[table]]) > 0){
      tnm_header <- create_comment_header("Targets not mapped",
                                          decorator_char="-",
                                          header_length,
                                          newline=TRUE)
      tnm_text <- paste0(seq_along(targets_not_mapped[[table]]),
                         ". ",
                         targets_not_mapped[[table]],
                         collapse = "\n")

      log_content <- c(log_content,
                       tnm_header,
                       tnm_text)
    }
    log_content <- c(log_content,
                     "")
  }

  blank_end_string <- create_comment_header("",
                                            decorator_char="=",
                                            header_length)

  log_content <- c(log_content, blank_end_string)

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
config_file_path <- file.path("redcap_task", tolower(pathogen), "config.yaml")
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
pathogen_filter_name <- config_list[["pathogen_filter_name"]]

# Optional
data_table_names <- config_list[["data_table_names"]]
incomplete_cols  <- config_list[["incomplete_col_names"]]

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

# The rows below are dependant on exact table names
# extractors quick fix:
extractor_df <- df_clean_list[["extractors"]]

df_clean_list[["extractors"]] <- aggregate(
  extractor_df[, !(names(extractor_df) %in% "record_id")],
  by = list(record_id = extractor_df[["record_id"]]),
  FUN = function(x) paste(unique(na.omit(x)), collapse = ", ")
)

# Filter out irrelevant records
incomplete_rows_df_list <- Map(get_incomplete_rows,
                               df_clean_list[names(incomplete_cols)],
                               incomplete_cols,
                               names(incomplete_cols))

df_clean_list[data_table_names] <- Map(filter_incomplete_rows,
                                       df_clean_list[data_table_names],
                                       incomplete_rows_df_list[data_table_names],
                                       data_table_names)


detail_qa_table_names <- names(incomplete_rows_df_list)[!(names(incomplete_rows_df_list) %in% data_table_names)]

# If an article or qa is incomplete all rows corresponding to that record are
# removed. For data tables only the offending record is removed
incomplete_record_ids <- unlist(
  sapply(detail_qa_table_names,
         function(name) incomplete_rows_df_list[[name]][["record_id"]])
  )

not_path_record_ids <- get_not_pathogen_ids(df=df_clean_list[["extractors"]],
                                            pathogen=pathogen_filter_name,
                                            id_col="record_id",
                                            pathogen_col = "pathogen")

records_to_remove <- c(not_path_record_ids, incomplete_record_ids)

df_clean_list <-  Map(function(df, name) filter_record_ids(df,
                                                           records_to_remove,
                                                           pathogen_filter_name,
                                                           name),
                      df_clean_list,
                      names(df_clean_list))

target_df_raw_list <- setNames(
  lapply(target_table_names,
         function (target) generate_target_table(input_df_list=df_clean_list,
                                                 mapping_df=mapping_df,
                                                 target_table=target)),
  target_table_names)

# *------------------------------ Process target ------------------------------*
# Split date columns into day, month, year
target_df_clean_list <- list()
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

# Expected by orderly in db_extraction_prep
saveRDS(target_df_clean_list,"extracted_tables.rds")

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
                                incomplete_df_list,
                                inputs_not_mapped,
                                targets_not_mapped,
                                pathogen)

orderly_artefact(description="Text file with the results of the extraction process",
                 log_filename)
# *============================================================================*

