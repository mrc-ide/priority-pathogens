library(dplyr)
library(orderly2)
library(readr)
library(stringr)

orderly_parameters(pathogen = NULL, debug_mode=FALSE)

pathogen_config_case <- tolower(pathogen)
pathogen_config_filename <- file.path(
  pathogen_config_case, paste0(pathogen_config_case, "_cleaning_config.R"))
pathogen_cleaning_filename <- file.path(
  pathogen_config_case, paste0(pathogen_config_case, "_cleaning.R"))

orderly_resource(c("cleaning.R",
                   pathogen_config_filename,
                   pathogen_cleaning_filename))
source("cleaning.R")
source(pathogen_config_filename)
source(pathogen_cleaning_filename)


# pathogen config needs to needs to have a vector called tables that has
# table names which match the input rds filenames
csv_artefacts <- sapply(tables, function(table) paste0(table, ".csv"))
rds_inputs <- sapply(tables, function(table) paste0(table, ".rds"))

orderly_dependency("db_compilation",
                   "latest(parameter:pathogen == this:pathogen)",
                   setNames(rds_inputs, rds_inputs))

orderly_artefact(
  description="Merged single and double extracted data as csv and rds files",
  setNames(csv_artefacts, csv_artefacts)
)

# The names on the left will always be used in this script to check if a table
# exists. Declaring a list allows us to change the table name we look for - e.g.
# if we decide to name a table parameters instead of params we can use
# "params"="parameters" in the list
table_check_list <- list("articles" = "articles",
                         "models" = "models",
                         "outbreaks" = "outbreaks",
                         "params" = "params")

cleaned_df_list <- list()
# *============================= Article cleaning =============================*
article_table_name <- table_check_list[["articles"]]
if (article_table_name %in% tables){
  article_rds_filename <- rds_inputs[[article_table_name]]
  article_df <- readRDS(article_rds_filename)

  # Legacy bug
  article_df <- rename(article_df,
                       first_author_surname = first_aauthor_surname)

# *----------------------------- Custom cleaning ------------------------------*
  article_df <- article_cleaning(article_df)

# *----------------------------- Generic cleaning -----------------------------*
  article_df$article_label <-  paste(article_df$first_author_surname,
                                     article_df$year_publication)

  if(debug_mode!=TRUE){
    article_filter_cols <- c("article_id", "name_data_entry")
    article_df <- filter_cols(article_df, article_filter_cols)
  }else{
    article_filter_cols <- c("")
  }

  base_relocate_cols <- c("id", "covidence_id", "pathogen")
  order_col <- "covidence_id"
  article_df <- relocate_and_arrange(
    article_df,
    c(base_relocate_cols, "first_author_first_name", "first_author_surname"),
    order_col
  )

  cleaned_df_list[[article_table_name]] <- article_df
} else{
  cli_alert_danger("You need to have an article table (or similar information
                   table)")
}



# *============================ Outbreak cleaning =============================*
oubreak_table_name <- table_check_list[["outbreaks"]]
if (oubreak_table_name %in% tables){
  outbreaks_rds_filename <- rds_inputs[[oubreak_table_name]]
  outbreak_df <- readRDS(outbreaks_rds_filename)

  outbreak_df <- outbreak_cleaning(outbreak_df)

# *----------------------------- Generic cleaning -----------------------------*
  outbreak_df <- filter_cols(outbreak_df,
                             c(article_filter_cols, "outbreak_id"))
  outbreak_df <- relocate_and_arrange(outbreak_df,
                                      c(base_relocate_cols, "outbreak_data_id"),
                                      order_col)


  cleaned_df_list[[oubreak_table_name]] <- outbreak_df
}

# *============================== Model cleaning ==============================*
model_table_name <- table_check_list[["models"]]

if (model_table_name %in% tables){
  models_rds_filename <- rds_inputs[[model_table_name]]
  model_df <- readRDS(models_rds_filename)

# *----------------------------- Custom cleaning ------------------------------*
  model_df <- model_cleaning(model_df)

# *----------------------------- Generic cleaning -----------------------------*
  model_df <- filter_cols(model_df,
                          c(article_filter_cols, "access_model_id"))

  model_df <- relocate_and_arrange(model_df,
                                   c(base_relocate_cols, "model_data_id"),
                                   order_col)

  cleaned_df_list[[model_table_name]] <- model_df
}

# *============================ Parameter cleaning ============================*
param_table_name <- table_check_list[["params"]]

if (param_table_name %in% tables){
  param_rds_filename <- rds_inputs[[param_table_name]]
  param_df <- readRDS(param_rds_filename)

# *----------------------------- Custom cleaning ------------------------------*
  param_df <- param_cleaning(param_df)

# *--------------------------- Update column types ----------------------------*
  if (!is.null(type_map_list)){
    param_df <- convert_column_types(param_df, type_map_list)
  }

# *---------------------------- Update punctuation ----------------------------*
  if (!is.null(cols_to_punctuate_vec)){
    param_df[cols_to_punctuate_vec] <- lapply(
      param_df[cols_to_punctuate_vec], function(x) str_replace_all(x, ",", "; "))
  }

# *------------------------------ Update values -------------------------------*
  if (!is.null(update_values_list)){
    for (name in names(update_values_list)){
      param_df[name] <- map_values(param_df[[name]],
                                   update_values_list[[name]])
    }
  }

# *------------------------------- Group types --------------------------------*
  # name is what we search for and value is what we map to
  if (!is.null(group_mapping_vec)){
    param_df$parameter_class <- assign_parameter_class(
      param_df$parameter_type,
      group_mapping_vec,
      default = "Other transmission parameters"
      )
  }

# *---------------------------- Combine intervals -----------------------------*
  if (!is.null(interval_combine_list)){
    for (name in names(interval_combine_list)){
      param_df[name] <- combine_interval(param_df,
                                         interval_combine_list[[name]][1],
                                         interval_combine_list[[name]][2])
    }
  }

# *----------------------------- Combine columns ------------------------------*
  if (!is.null(col_combine_list)){
    for (name in names(col_combine_list)){
      param_df[name] <- combine_cols(param_df,
                                     col_combine_list[[name]][[1]],
                                     col_combine_list[[name]][[2]])
    }
  }

# *--------------------------- Filter and relocate ----------------------------*
  param_df <- filter_cols(param_df,
                          c(article_filter_cols, "access_param_id"))
  param_df <- relocate_and_arrange(
    param_df,
    c(base_relocate_cols, "parameter_data_id"),
    order_col)

  cleaned_df_list[[param_table_name]] <- param_df
}

# *=============================== Save output ================================*
for (name in names(cleaned_df_list)){
  write_csv(cleaned_df_list[[name]], csv_artefacts[name])
}
