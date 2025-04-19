# Functions for cleaning data and adding QA scores
library(dplyr)
library(janitor)
library(stringr)

# *============================= Helper functions =============================*
map_values <- function(update_vec, lookup_vec){
  return (ifelse(update_vec %in% names(lookup_vec),
                 lookup_vec[update_vec], update_vec))
}

assign_parameter_class <- function(x,
                                   pattern_map,
                                   default = "Other transmission parameters") {
  out_vec <- rep(default, length(x))
  for (pattern in names(pattern_map)) {
    # if there is an overlap between pattern names the final match will be used
    match_idx <- grepl(pattern, x)
    out_vec[match_idx] <- pattern_map[pattern]
  }

  return(out_vec)
}

combine_interval <- function(df, lower_col, upper_col, new_col) {
  out_vec <- ifelse(!is.na(df[[lower_col]]) & !is.na(df[[upper_col]]),
                    paste(df[[lower_col]], "-", df[[upper_col]]), NA)

    return(out_vec)
}

combine_cols <- function(df, cols_to_check, cols_to_combine){
  out_vec <- rep(NA, NROW(df))

  for (i in 1:NROW(cols_to_check)){
    match_idx <- which(!is.na(df[cols_to_check[i]]))

    out_vec[match_idx] <- df[[cols_to_combine[i]]][match_idx]
  }

  return (out_vec)
}

convert_column_types <- function(df, type_map) {
  for (col in names(type_map)) {
    type_fn <- type_map[[col]]
    df[[col]] <- type_fn(df[[col]])
  }

  return(df)
}

relocate_and_arrange <- function(df, relocate_cols, arrange_col){
  df <- relocate(df, relocate_cols)
  df <- arrange(df, arrange_col)

  return (df)
}

filter_cols <- function(df, cols_to_remove){
  return (df[!colnames(df) %in% cols_to_remove])
}