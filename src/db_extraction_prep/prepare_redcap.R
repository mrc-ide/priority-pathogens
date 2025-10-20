library(cli)
library(ids)
library(orderly2)
library(readr)
library(tools)
library(yaml)


# *============================= Helper functions =============================*
# *----------------------------- Filter functions -----------------------------*
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

filter_incomplete_data_rows <- function(df, incomplete_df, name){
  record_match <- df[["record_id"]] %in% incomplete_df[["record_id"]]

  redcap_repeat_match <- (df[["redcap_repeat_instance"]] %in%
                            incomplete_df[["redcap_repeat_instance"]])

  rows_to_keep <- !(record_match & redcap_repeat_match)

  cli_inform(paste0("Keeping ",
                    sum(rows_to_keep), "/", NROW(df),
                    " complete rows for ", name))

  return (df[rows_to_keep,])
}

filter_record_ids <- function(df, record_id_vec, name, cli_text){
  record_match <- df[["record_id"]] %in% record_id_vec

  rows_to_keep <- !record_match

  cli_inform(paste0("Keeping ",
                    sum(rows_to_keep), "/", NROW(df), " ", cli_text, " ", name))

  return (df[rows_to_keep,])
}

get_not_pathogen_ids <- function(df, pathogen, id_col, pathogen_col="pathogen"){
  # Update for Pathogen other
  pathogen_lower_vec <- sapply(df[pathogen_col], tolower)
  not_pathogen_filter <- (pathogen_lower_vec!=tolower(pathogen) |
                            is.na(pathogen_lower_vec))
  record_vec <- df[not_pathogen_filter, ][[id_col]]
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

get_incomplete_rows <- function(df, col, name, redcap_repeat_instances,
                                incomplete_key="Incomplete"){
  if (redcap_repeat_instances){
    cols <- c("record_id", "redcap_repeat_instance")
    alert_text <- "(record id | redcap repeat instance)"

  }else{
    cols <- "record_id"
    alert_text <- "(record_id)"
  }

  incomplete_df <- df[df[[col]]==incomplete_key,
                      cols]

  if (NROW(incomplete_df)>0){
    if (redcap_repeat_instances){
      incomplete_combined_text <- pretty_format_incomplete_text(incomplete_df)
    } else{
      incomplete_combined_text <- paste(incomplete_df[["record_id"]],
                                        collapse=", ")
    }

    cli_alert_warning(
      c(paste("The following", sub("s$", "", name),
              "rows are incomplete", alert_text, ": "),
        incomplete_combined_text)
    )
  }

  return (incomplete_df)
}

# *------------------------------ Key functions -------------------------------*
add_uuid_id <- function(input_df, uuid_col){
  if (NROW(input_df) > 0) {
    input_df[uuid_col] <- random_id(n = NROW(input_df), use_openssl = FALSE)
  } else {
    input_df[uuid_col] <- character(0)
  }

  return (input_df)
}

add_pks <- function(input_df, pk_col, base_id, combine_id){
  if (NROW(input_df) > 0) {
    if (!is.null(base_id) & !is.null(combine_id)){
      base_digits <- floor(log10(max(input_df[[base_id]]))) + 1
      combined_digits <- floor(log10(max(input_df[[base_id]]))) + 1
      input_df[pk_col] <- paste(
        sprintf("%.*d", base_digits, as.numeric(input_df[[base_id]])),
        sprintf("%.*d", combined_digits, as.numeric(input_df[[combine_id]])),
        sep="_")
    }
  } else {
    input_df[pk_col] <- character(0)
  }
  return(input_df)
}

# *------------------------ Name-replacement functions ------------------------*
col_list_key_map <- function(df, col, mapping_list){
  # assumes all matches are accounted for
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
    # No mappings means that the name is the same as the input
    cli_alert_info(paste("No mappings found for the following", n_no_updated,
                         "columns: ", paste0(names_not_updated, collapse=", ")))
  }

  return (updated_vec)
}

# *--------------- REDCap wide format transformation functions ----------------*
subset_long_df <- function(input_df, mapping_df, table_name){
  colnames_no_underscore <- gsub("_\\d+", "", colnames(input_df))

  table_input_cols <- mapping_df[mapping_df[["input_table"]]==table_name,][["input_col"]]
  subset_df <- input_df[colnames_no_underscore %in% table_input_cols]

  return (subset_df)
}

stack_rows <- function(df, id_col = "record_id"){
  colnames_table <- colnames(df)
  colnames_table <- colnames_table[!(colnames_table == id_col)]
  repeat_ids <- sub("^.*_(\\d+)$", "\\1", colnames_table)

  split_colname_list <- split(colnames_table,
                              sub("^.*_(\\d+)$", "\\1", colnames_table))

  combined_df <- data.frame()

  for (repeat_id in names(split_colname_list)){
    split_colnames <- split_colname_list[[repeat_id]]
    temp_df <- df[, c(id_col, split_colnames)]

    # list sorts 1, 10, so next rather than break
    # Only filter out completely blank blocks => this approach will create empty
    # rows that will be filtered out later
    if (all(is.na(temp_df[split_colnames]))) next

    # get names time to make reduce dependency on column order
    colnames(temp_df) <- gsub("_\\d+", "", colnames(temp_df))

    repeat_id_vec <- rep(repeat_id, NROW(temp_df))

    df_to_rbind <- cbind(temp_df[,1, drop=FALSE],
                         "repeat_id"=repeat_id_vec,
                         temp_df[-1])

    combined_df <- rbind(combined_df, df_to_rbind)
  }

  return(combined_df)
}

get_overflow_mapping_table <- function(df,
                                       id_col,
                                       cov_id_col,
                                       continuation_col,
                                       extractor_col,
                                       incomplete_col,
                                       incomplete_key="Incomplete"){
  non_na_df <- df[!is.na(df[[extractor_col]]) & !is.na(df[[cov_id_col]]), ]
  continuations_df <- non_na_df[!is.na(non_na_df[[continuation_col]]),
                                c(cov_id_col, extractor_col)]
  continuations_df <- unique(continuations_df)

  id_mapping_list <- list()
  cov_id_vec <- c()
  cov_id_error_list <- list()

  for (i in 1:nrow(continuations_df)){
    extractor <- continuations_df[[extractor_col]][i]
    cov_id <- continuations_df[[cov_id_col]][i]
    extractor_filter <- non_na_df[[extractor_col]]==extractor
    cov_id_filter <- non_na_df[[cov_id_col]]==cov_id

    temp_df <- non_na_df[cov_id_filter & extractor_filter, ]

    if (NROW(temp_df) == 1){
      cli_alert_warning(
        paste(extractor, "has indicated that Covidence ID: ", cov_id,
              "is a continuation, but it only has 1 REDCap entry")
      )
      cov_id_error_list[[extractor]] <- cov_id
      next
    }

    # We map to the latest complete row
    id_vec <- temp_df[[id_col]]
    id_to_map_to <- max(id_vec[!(temp_df[[incomplete_col]]==incomplete_key)])

    id_update_vec <- id_vec[id_vec!=id_to_map_to]
    id_mapping_list[[as.character(id_to_map_to)]] <- id_update_vec
    cov_id_vec <- c(cov_id_vec, cov_id)

    cli_alert_info(
      paste0(extractor, " has indicated that Covidence ID: ", cov_id,
            " is a continuation. There are ", NROW(temp_df), " REDCap entries with this ID.\n",
            "The following ", id_col, " will be mapped to ", id_to_map_to, ": ",
            paste(id_update_vec, collapse=", "))
    )

  }
  prog_report_list <- list("cov_id_vec"=cov_id_vec,
                           "cov_id_error_list"=cov_id_error_list,
                           "id_mapping_list"=id_mapping_list)
  return (prog_report_list)
}

update_records <- function(df, id_mapping_list, id_col){
  id_vec <- df[[id_col]]

  for(name in names(id_mapping_list)){
    df[id_vec %in% id_mapping_list[[name]], id_col] <- as.numeric(name)
  }

  return (df)
}

update_id_rows_to_keep <- function(df, id_to_map_to, ids_to_map,
                                   notes_cols, continuation_col, id_col,
                                   incomplete_col, incomplete_col_key,
                                   continuation_table){
  ids_to_combine <- c(as.numeric(id_to_map_to), ids_to_map)
  to_combine_df <- df[df[[id_col]] %in% ids_to_combine, ]

  cli_alert_info(
    paste0("Attempting to merge ", continuation_table, " rows with ", id_col,
           ": ", paste(ids_to_combine, collapse=", ")))

  notes_list <- lapply(
    notes_cols,
    function(col) paste(get_unique_na_preserve(to_combine_df[[col]]),
                        collapse="; ")
  )
  names(notes_list) <- notes_cols
  id_list <- setNames(list(as.numeric(id_to_map_to)), id_col)
  continuation_list <- setNames(list(NA), continuation_col)

  unqiue_col_df <- to_combine_df[, !(colnames(to_combine_df) %in%
                                       c(notes_cols, continuation_col, id_col))]
  unique_value_list <- lapply(unqiue_col_df,
                              function(col) get_unique_na_preserve(col))

  unique_incomplete_col <- unique_value_list[[incomplete_col]]
  unique_value_list[[incomplete_col]] <- ifelse(
    length(unique_incomplete_col)>1,
    unique_incomplete_col[unique_incomplete_col!=incomplete_col_key],
    unique_incomplete_col)

  for (name in names(unique_value_list)){
    values <- unique_value_list[[name]]
    if (NROW(values) > 1){
      cli_alert_danger(
        paste0("Found a mismatch for ", name, ". It has the following values: ",
               paste(values, collapse=", "), ". The final value '", values[-1],
               "' will be used."))
    }
    unique_value_list[[name]] <- values[NROW(values)]
  }

  final_return_list <- c(id_list, unique_value_list, notes_list, continuation_list)
  return (final_return_list)
}

# *----------------------- Target generation functions ------------------------*
generate_target_table <- function(input_df_list, mapping_df, target_table,
                                  input_table_col_name="input_table",
                                  target_table_col_name="target_table",
                                  input_col_name = "input_col",
                                  target_col_name = "target_col"){
  cli_h3(paste("Generating table:", target_table))

  mapping_non_na_df <- mapping_df[!is.na(mapping_df[target_table_col_name]), ]
  filtered_mapping_df <- (mapping_non_na_df[
    mapping_non_na_df[target_table_col_name] == target_table,])

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

add_article_cols <- function(input_df, article_df, cols_to_add, join_col, start){
  cols_to_add <- c(cols_to_add, join_col)

  if (start ==TRUE){
    updated_df <- merge(article_df[cols_to_add], input_df, by=join_col,
                        all.y = TRUE)
  }else{
    updated_df <- merge(input_df, article_df[cols_to_add], by=join_col,
                        all.x = TRUE)
  }

  return(updated_df)
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
  if (!all(is.na(input_date_vec))){
    split_date_df <- do.call(rbind, strsplit(input_date_vec,"-", fixed = TRUE))

    split_date_df[split_date_df == "xx" | is.na(input_date_vec)] <- ""
    col_1_is_year <- grepl("^\\d{4}$", split_date_df[,1])
    base_select_vec <- rep(2, NROW(split_date_df))
    year_offset_vec <- converted_vector <- ifelse(col_1_is_year, -1, 1)

    year_select_vec <- base_select_vec + year_offset_vec
    day_select_vec <- base_select_vec + (year_offset_vec * -1)

    day_vec <- sapply(1:length(day_select_vec),
                      function (i) split_date_df[i, day_select_vec[i]])
    month_vec <- split_date_df[,2]
    year_vec <- sapply(1:length(year_select_vec),
                       function (i) split_date_df[i, year_select_vec[i]])
  }else{
    day_vec <- rep(NA, length(input_date_vec))
    month_vec <- day_vec
    year_vec <- day_vec
  }

  updated_df <- input_df[names(input_df)!=date_col]
  updated_df[paste0(date_col, "_day")] <- day_vec
  updated_df[paste0(date_col, "_month")] <- month_vec
  updated_df[paste0(date_col, "_year")] <- year_vec

  return (updated_df)
}

# *------------------------- Process report functions -------------------------*
check_raw_cols_mapping <- function(raw_df_list,
                                   mapping_df,
                                   tables_to_stack,
                                   raw_in_mapping=TRUE){
  if(!is.null(tables_to_stack)){
    colnames_vec <- unlist(
      sapply(df_raw_list, function(df) unique(gsub("_\\d+", "", colnames(df)))))
  }else{
    colnames_vec <- unlist(sapply(df_raw_list, colnames))
  }

  input_col_vec <- mapping_df[["input_col"]]

  if (raw_in_mapping){
    not_contained <- colnames_vec[!(colnames_vec %in% input_col_vec)]
    table_source <- "input"
    table_target <- "mapping file"
  }else{
    not_contained <- input_col_vec[!(input_col_vec %in% colnames_vec)]
    table_source <- "mapping file"
    table_target <- "input"
  }

  if (length(not_contained) > 1){
    cli_alert_info(paste0("The following ", NROW(not_contained), " columns are",
                          " in the ", table_source ," but not in the ",
                          table_target," and will be exlcuded in the final
                          output: ", paste0(not_contained, collapse=", ")))
  }
}

check_cols_not_mapped <- function(output_df,
                                  mapping_df,
                                  mapping_col_name,
                                  table_col_name,
                                  table_name,
                                  target_map=FALSE){
  output_colnames <- colnames(output_df)

  mapping_filtered_df <- mapping_df[mapping_df[[table_col_name]]==table_name,]
  mapping_filtered_df <- mapping_filtered_df[!is.na(mapping_filtered_df[mapping_col_name]), ]

  if (target_map){
    cols_not_mapped_filter <- !(mapping_filtered_df[[mapping_col_name]] %in%
                                  output_colnames)

    cols_not_mapped <- mapping_filtered_df[[mapping_col_name]][cols_not_mapped_filter]
  }else{
    cols_not_mapped_filter <- (output_colnames %in%
                                 mapping_filtered_df[[mapping_col_name]])

    cols_not_mapped <- output_colnames[cols_not_mapped_filter]
  }


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
                            unfiltered_article_df,
                            incomplete_rows_df_list,
                            inputs_not_mapped,
                            targets_not_mapped,
                            pathogen,
                            redcap_repeat_instances,
                            id_mapping_rep_list){
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
    as.list(unfiltered_article_df[["extractor_name"]]),
    unfiltered_article_df[["record_id"]])
  write_warning_header <- TRUE
  for (name in names(incomplete_rows_df_list)){
    if (NROW(incomplete_rows_df_list[[name]]) > 0){
      if (write_warning_header){
        write_warning_header <- FALSE
        warning_header <- paste0(
        "!WARNING!",
        "\nThe following rows are incomplete and were excluded from the analysis.")

        if(redcap_repeat_instances){
          warning_header <- paste0(
            warning_header,
            "\n(Reminder: if QA or article data is incomplete, all rows corresponding",
            "\nto that RecordID will be excluded, including data extraction rows.)",
            "\nRows are reported as: record id | redcap repeat instance | extractor")
        }
        log_content <- c(log_content, warning_header)
      }
      incomplete_df <- incomplete_rows_df_list[[name]]

      if (redcap_repeat_instances){
        incomplete_combined_text <- pretty_format_incomplete_text(incomplete_df)
      }else{
        incomplete_combined_text <- incomplete_df[["record_id"]]
      }
      unique_incomplete_record_ids <- unique(incomplete_df[["record_id"]])
      incomplete_combined_text <- sapply(
        1:NROW(unique_incomplete_record_ids),
        function(i) paste(incomplete_combined_text[i],
                          record_extractor_list[[as.character(unique_incomplete_record_ids[i])]],
                          sep =" | ")
             )
      incomplete_combined_text <- paste(incomplete_combined_text, collapse = "\n")
      table_name_text <- paste0("\n",sub("s$", "", name), " rows")
      log_text <- paste0(table_name_text, ":\n",
                         incomplete_combined_text)
      log_content <- c(log_content, log_text)
    }
  }

  if(length(id_mapping_rep_list[["cov_id_vec"]]) > 0 |
     length(id_mapping_rep_list[["cov_id_error_list"]]) > 0 ){
    tnm_header <- create_comment_header("Form continuations",
                                        decorator_char="-",
                                        header_length,
                                        newline=TRUE)
    if(length(id_mapping_rep_list[["cov_id_error_list"]])>0){
      cov_id_error_ids <- unlist(id_mapping_rep_list[["cov_id_error_list"]])
      cov_id_error_names <- names(cov_id_error_ids)
      tnm_text <- paste0(
        "An extractor indicated that the the following Covidence IDs were ",
        "\ncontinuations but only a single form was found.\n\n",
        paste0(
          seq_along(cov_id_error_names), ". ",
          paste(cov_id_error_names, "| Covidence ID: ", cov_id_error_ids),
          collapse = "\n")
      )
    }

    if(length(id_mapping_rep_list[["cov_id_vec"]])>0){
      tnm_text <- paste0(tnm_text,
        "\n\nArticles with the following Covidence IDs were extracted over ",
        "multiple forms.", "\nWe merged the corresponding article rows to a ",
        "single RecordID.\nIf there were any conflicts we chose the latest ",
        "entry.\nAny merging conflict are  highlighted in the script cli ",
        "output.\nFor all data tables we map all RecordIDs to the merged ",
        "value.\n"
        )

      id_mapping_from_text <- sapply(id_mapping_rep_list[["id_mapping_list"]],
                                     function(x) paste(x,collapse=", "))

      tnm_id_text <- paste0(
        seq_along(id_mapping_rep_list[["cov_id_vec"]]), ". ",
        paste0(
          "Covidence_ID: ", id_mapping_rep_list[["cov_id_vec"]],
          " | RecordID: ",names(id_mapping_rep_list[["id_mapping_list"]]),
          " <- ", id_mapping_from_text),
        collapse = "\n")
    }

    log_content <- c(log_content,
                     tnm_header,
                     tnm_text,
                     tnm_id_text)
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
if (orderly_download_dependency==TRUE){
  table_filenames_vec <- unlist(config_list[["table_filepaths"]])

  orderly_dependency(
    "db_redcap_download", "latest(parameter:pathogen == this:pathogen)",
    unname(table_filenames_vec)
  )
} else{
  table_filenames_vec <- sapply(config_list[["table_filepaths"]], basename)
  table_filepath_list <- setNames(config_list[["table_filepaths"]],
                                  table_filenames_vec)

  do.call(orderly_shared_resource, table_filepath_list)
}

mapping_filename <- basename(config_list[["mapping_filepath"]])

do.call(orderly_shared_resource,
        setNames(config_list["mapping_filepath"], mapping_filename))

target_filename <-  basename(config_list[["target_filepath"]])

do.call(orderly_shared_resource,
        setNames(config_list["target_filepath"], target_filename))

# *---------------------------- Config parameters -----------------------------*
# Required
target_table_names <- config_list[["target_table_names"]]
pathogen_filter_name <- config_list[["pathogen_filter_name"]]

# Optional
table_instrument_source_list <- config_list[["instrument_source_table"]]
tables_to_stack <- config_list[["tables_to_stack"]]

continuation_table <- config_list[["continuation_config"]][["continuation_table"]]
id_col <- config_list[["continuation_config"]][["id_col"]]
cov_id_col <- config_list[["continuation_config"]][["cov_id_col"]]
continuation_col <- config_list[["continuation_config"]][["continuation_col"]]
extractor_col <- config_list[["continuation_config"]][["extractor_col"]]
incomplete_col  <- config_list[["continuation_config"]][["incomplete_col"]]
notes_cols <-  config_list[["continuation_config"]][["notes_cols"]]

article_id <- config_list[["article_id"]]
# Assumes if there are not tables to stack then data is in a REDCap repeat
# instance format
redcap_repeat_instances <- is.null(tables_to_stack)

data_table_names <- config_list[["data_table_names"]]
incomplete_cols  <- config_list[["incomplete_col_names"]]
incomplete_col_key  <- config_list[["incomplete_col_key"]]

linked_rows_list <- config_list[["linked_row_col_names"]]
uuid_col_names <- config_list[["uuid_col_names"]]
pk_col_names <- config_list[["pk_col_names"]]
pk_base_id <- config_list[["pk_base_id"]]

date_cols_to_split <- config_list[["date_cols_to_split"]]
article_cols_to_add_start <- config_list[["article_cols_to_add_start"]]
article_cols_to_add_end <- config_list[["article_cols_to_add_end"]]


# *------------------------------- Read in data -------------------------------*
mapping_df <- read_csv(mapping_filename, show_col_types = FALSE)

if (!is.null(table_instrument_source_list)){
  mapping_df <- col_list_key_map(df=mapping_df,
                                 col="input_table",
                                 mapping_list=table_instrument_source_list)
}

target_names_df <- read_csv(target_filename, show_col_types = FALSE)

df_raw_list <- lapply(table_filenames_vec,
                      function(x) read_csv(x, show_col_types = FALSE))

cli_h1("Comparing REDCap input and mapping file")
check_raw_cols_mapping(df_raw_list, mapping_df, tables_to_stack,
                       raw_in_mapping=TRUE)
check_raw_cols_mapping(df_raw_list, mapping_df, tables_to_stack,
                       raw_in_mapping=FALSE)

get_unique_na_preserve  <- function(vec){
  unique_vec <- unique(na.omit(vec))
  if (length(unique_vec) == 0){
    return (NA)
  }else{
    return (unique_vec)
  }
}

if (!is.null(tables_to_stack)){
  distinct_input_tables <- unique(mapping_df[["input_table"]])

  # if transforming wide to long, df_raw_list should only have one element
  df_raw_list <- setNames(lapply(
    distinct_input_tables,
    function(table_name) subset_long_df(df_raw_list[[1]], mapping_df, table_name)
    ), distinct_input_tables)

  cli_h1("Form continuations")
  cli_h3("Checking for form continuations")
  id_mapping_rep_list <- get_overflow_mapping_table(
    df=df_raw_list[[continuation_table]],
    id_col=id_col,
    cov_id_col=cov_id_col,
    continuation_col=continuation_col,
    extractor_col=extractor_col,
    incomplete_col=incomplete_col,
    incomplete_key=incomplete_col_key)

  id_mapping_list <- id_mapping_rep_list$id_mapping_list

  # Data tables
  if(NROW(id_mapping_list)>0){
    df_raw_list[tables_to_stack] <- lapply(
      df_raw_list[tables_to_stack],
      function(df) update_records(df=df,
                                  id_mapping_lis=id_mapping_list,
                                  id_col=id_col))
  }

  df_raw_list[tables_to_stack] <- lapply(df_raw_list[tables_to_stack], stack_rows)

  # Continuation table
  if(NROW(id_mapping_list)>0){
    cli_h3(paste("Merging", continuation_table,
                 "rows repeated due to continuation"))
    ids_to_keep_filter <- !(
      df_raw_list[[continuation_table]][[id_col]] %in%
          unlist(id_mapping_list)
      )
    cli_alert_warning(paste("If there are any conflicts we will highlight them",
                            "and choose the latest entry")
                      )

    list_of_updated_records <- lapply(
      names(id_mapping_list), function (id_to_map_to)
        update_id_rows_to_keep(df_raw_list[[continuation_table]],
                               id_to_map_to, id_mapping_list[[id_to_map_to]],
                               notes_cols, continuation_col, id_col,
                               incomplete_col, incomplete_col_key,
                               continuation_table)
    )

    for (updated_record in list_of_updated_records){
      id_filter <- (df_raw_list[[continuation_table]][[id_col]] ==
                      updated_record[[id_col]])

      df_raw_list[[continuation_table]][
        id_filter, names(updated_record)] <- updated_record
    }

    df_raw_list[[continuation_table]] <- (
      df_raw_list[[continuation_table]][ids_to_keep_filter,])
    }
  }

# *------------------------- Clean & generate targets -------------------------*
cli_h1("Keeping non-empty rows")

df_clean_list <- Map(filter_empty_rows,
                     df=df_raw_list,
                     name=names(df_raw_list))

# Filter out rows with an Answer of No for linked parameter
if (!is.null(linked_rows_list)){
  cli_h1("Keeping rows with answers")

  df_clean_list[names(linked_rows_list)] <- Map(filter_linked_rows,
                                                df=df_clean_list[names(linked_rows_list)],
                                                col=linked_rows_list,
                                                name=names(linked_rows_list))
}

# The rows below are dependant on exact table names
# (RedCap orov extractor report does not export rows correctly => quick fix:
if ("extractors" %in% names(df_clean_list)){
  extractor_df <- df_clean_list[["extractors"]]

  df_clean_list[["extractors"]] <- aggregate(
    extractor_df[, !(names(extractor_df) %in% "record_id")],
    by = list(record_id = extractor_df[["record_id"]]),
    FUN = function(x) paste(unique(na.omit(x)), collapse = ", ")
  )
  pathogen_table <- "extractors"
}else{
  pathogen_table <- "articles"
}

not_pathogen_ids <- get_not_pathogen_ids(df=df_clean_list[[pathogen_table]],
                                          pathogen=pathogen_filter_name,
                                          id_col="record_id",
                                          pathogen_col = "pathogen")

cli_h1(paste("Keeping", toTitleCase(tolower(pathogen)), "rows"))

df_clean_list <-  Map(
  function(df, name) filter_record_ids(df,
                                       not_pathogen_ids,
                                       name,
                                       paste(pathogen_filter_name, "rows for")),
  df_clean_list,
  names(df_clean_list))

# contains a list of all articles before they are filtered
unfiltered_article_df <- df_clean_list[[pathogen_table]]

# Filter out irrelevant records
if (!is.null(incomplete_cols)){
  cli_h1("Keeping complete rows")
  incomplete_rows_df_list <- Map(get_incomplete_rows,
                                 df_clean_list[names(incomplete_cols)],
                                 incomplete_cols,
                                 names(incomplete_cols),
                                 redcap_repeat_instances)

  if (redcap_repeat_instances){
    df_clean_list[data_table_names] <- Map(filter_incomplete_data_rows,
                                           df_clean_list[data_table_names],
                                           incomplete_rows_df_list[data_table_names],
                                           data_table_names)
  }

  detail_qa_table_names <- (names(incomplete_rows_df_list)[
    !(names(incomplete_rows_df_list) %in% data_table_names)])

  # If an article or qa is incomplete all rows corresponding to that record are
  # removed. For data tables only the offending record is removed
  incomplete_record_ids <- unlist(
    sapply(detail_qa_table_names,
           function(name) incomplete_rows_df_list[[name]][["record_id"]])
  )

  incomplete_cli_text <- ifelse(pathogen_table=="articles",
                                "complete rows for",
                                "rows with complete qa and article information")

  df_clean_list <-  Map(
    function(df, name) filter_record_ids(df,
                                         incomplete_record_ids,
                                         name,
                                         incomplete_cli_text),
    df_clean_list,
    names(df_clean_list))
}

cli_h1("Generating target tables")

if (!is.null(pk_col_names)){
  # Assumes fixed column names for mapping file
  input_names <- sapply(
    data_table_names, function(name) unique(
      na.omit(mapping_df[mapping_df$target_table==name, "input_table"])
      )
    )

  input_names <- unname(unlist(input_names))
  new_repeat_id_name <- "Repeat_ID"
  # repeat ids are created within this script and are required for pk generation
  # new name to suppress mapping warning (quick fix)
  repeat_id_rows_df <- data.frame(
    rep("repeat_id", length(data_table_names)),
    input_names,
    rep(new_repeat_id_name, length(data_table_names)),
    data_table_names)
  colnames(repeat_id_rows_df) <- colnames(mapping_df)

  # original mapping df is used for the progress report
  mapping_updated_df <- rbind(mapping_df, repeat_id_rows_df)
}else{
  mapping_updated_df <- mapping_df
}

target_df_raw_list <- setNames(
  lapply(target_table_names,
         function (target) generate_target_table(input_df_list=df_clean_list,
                                                 mapping_df=mapping_updated_df,
                                                 target_table=target)),
  target_table_names)

# *------------------------------ Process target ------------------------------*
target_df_clean_list <- target_df_raw_list

# Split date columns into day, month, year
if (!is.null(date_cols_to_split)){
  cli_h1("Expanding date columns")
  for (name in names(date_cols_to_split)){
    cli_inform(paste("Expanding the following ", name, "table date columns:",
                     paste(date_cols_to_split[[name]], collapse=", ")))
    for (col in date_cols_to_split[[name]]){
      target_df_clean_list[[name]] <- split_data_column(target_df_clean_list[[name]],
                                                        col)
    }
  }
}

# Add PK columns
if (!is.null(pk_col_names)){
  target_df_clean_list[data_table_names] <- lapply(
    data_table_names,
    function(name) add_pks(input_df=target_df_clean_list[[name]],
                           pk_col=pk_col_names[[name]],
                           base_id=article_id,
                           combine_id=new_repeat_id_name)
  )

  # Remove repeat ids that were included for cleaning
  target_df_clean_list[data_table_names] <- lapply(
    target_df_clean_list[data_table_names],
    function(df) df[!colnames(df) %in% new_repeat_id_name])
}

# Add UUID columns
if (!is.null(uuid_col_names)){
  set.seed(3141)
  # Order by ArticleID and additional cols to ensure uuid is always assigned
  # in the same order
  sort_order <- do.call(
    what = order,
    args = target_df_clean_list[[continuation_table]][,article_id])
  target_df_clean_list[[continuation_table]] <-
    target_df_clean_list[[continuation_table]][sort_order, ]

  # Order by new pk to ensure uuid is always assigned in the same order
  target_df_clean_list[data_table_names] <- lapply(
    data_table_names,
    function(name) target_df_clean_list[[name]][
      order(target_df_clean_list[[name]][[pk_col_names[[name]][1]]]), ])

  target_df_clean_list <- setNames(
    lapply(target_table_names,
           function(name) add_uuid_id(input_df=target_df_clean_list[[name]],
                                      uuid_col=uuid_col_names[[name]])
    ),
    target_table_names)
}

# Add target article columns
if (!is.null(article_cols_to_add_start)){
  target_df_clean_list[data_table_names] <- lapply(
    data_table_names,
    function(name) add_article_cols(target_df_clean_list[[name]],
                                    target_df_clean_list[["articles"]],
                                    cols_to_add=article_cols_to_add_start,
                                    join_col=article_id,
                                    start=TRUE
    )
  )
}

if (!is.null(article_cols_to_add_end)){
  target_df_clean_list[data_table_names] <- lapply(
    data_table_names,
    function(name) add_article_cols(target_df_clean_list[[name]],
                                    target_df_clean_list[["articles"]],
                                    cols_to_add=article_cols_to_add_end,
                                    join_col=article_id,
                                    start=FALSE
    )
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
                                unfiltered_article_df,
                                incomplete_rows_df_list,
                                inputs_not_mapped,
                                targets_not_mapped,
                                pathogen,
                                redcap_repeat_instances,
                                id_mapping_rep_list)

orderly_artefact(
  description="Text file with the results of the extraction process",
  log_filename)
# *============================================================================*

