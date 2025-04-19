library(dplyr)
library(stringr)

orderly_parameters(pathogen = NULL)

orderly_dependency("nipah_compilation",
                   "latest(parameter:pathogen == this:pathogen)",
                   c("articles.rds"="articles.rds",
                     "models.rds"="models.rds",
                     "params.rds"="params.rds",
                     "outbreaks.rds"="outbreaks.rds"))

orderly_dependency("nipah_compilation",
                   "latest(parameter:pathogen == this:pathogen)",
                   c("articles.rds"="articles.rds",
                     "models.rds"="models.rds",
                     "params.rds"="params.rds",
                     "outbreaks.rds"="outbreaks.rds"))

orderly_resource("cleaning.R")

orderly_artefact(
  description="Merged single and double extracted data as csv and rds files",
  c("articles.csv"="articles.csv",
    "models.csv"="models.csv",
    "params.csv"="params.csv",
    "outbreaks.csv"="outbreaks.csv"
    )
  )

source("cleaning.R")

# *============================= Article cleaning =============================*
article_df <- readRDS("articles.rds")

article_df <- rename(article_df,
                     first_author_surname = first_aauthor_surname)
article_df$article_label <-  paste(article_df$first_author_surname,
                                   article_df$year_publication)

article_filter_cols <- c("article_id", "name_data_entry")
article_df <- filter_cols(article_df,
                          article_filter_cols)

base_relocate_cols <- c("id", "covidence_id", "pathogen")
order_col <- "covidence_id"
article_df <- relocate_and_arrange(
  article_df,
  c(base_relocate_cols, "first_author_first_name", "first_author_surname"),
  order_col
)

# *============================ Outbreak cleaning =============================*
outbreak_df <- readRDS("outbreaks.rds")
outbreak_df <- filter_cols(outbreak_df,
                           c(article_filter_cols, "outbreak_id"))
outbreak_df <- relocate_and_arrange(outbreak_df,
                                    c(base_relocate_cols, "outbreak_data_id"),
                                    order_col)

# *============================== Model cleaning ==============================*
model_df <- readRDS("models.rds")
model_df <- filter_cols(model_df,
                        c(article_filter_cols, "access_model_id"))
model_df <- relocate_and_arrange(model_df,
                                 c(base_relocate_cols, "model_data_id"),
                                 order_col)

# *============================ Parameter cleaning ============================*
param_df <- readRDS("params.rds")

# *--------------------------- Update column types ----------------------------*
type_map <- list("cfr_ifr_numerator" = as.integer,
                 "population_study_start_day" = as.numeric)

param_df <- convert_column_types(param_df, type_map)

# *---------------------------- Update punctuation ----------------------------*
cols_to_punctuate <- c("method_disaggregated_by",
                       "population_location",
                       "population_country")
param_df[cols_to_punctuate] <- lapply(param_df[cols_to_punctuate],
                                      function(x) str_replace_all(x, ";", ", "))

# *------------------------------ Update values -------------------------------*
uncertainty_interval_lookup_values <- c(
  "CI95%" = "95% CI",
  "CRI95%" = "95% CrI",
  "CI90%" = "90% CI",
  "CRI90%" = "90% CrI",
  "Highest Posterior Density Interval 95%" = "HPDI 95%",
  "Inter Quartile Range (IQR)" = "IQR"
)

uncertainty_single_lookup_values <- c(
  "Standard deviation (Sd)" = "Standard Deviation",
  "Standard Error (SE)" = "Standard Error"
)

update_values_list <- list(
  "parameter_uncertainty_type"=uncertainty_interval_lookup_values,
  "parameter_uncertainty_single_type"=uncertainty_single_lookup_values,
  "parameter_2_uncertainty_type"=uncertainty_interval_lookup_values,
  "parameter_2_uncertainty_single_type"=uncertainty_single_lookup_values,
  "method_r"=  c("Renewal equations / Branching process" = "Branching process")
)

for (name in names(update_values_list)){
  param_df[name] <- map_values(param_df[[name]],
                               update_values_list[[name]])
}

# *------------------------------- Group types --------------------------------*
# name is what we search for and value is what we map to
group_mapping <- c(
  "Human delay" = "Human delay",
  "Seroprevalence" = "Seroprevalence",
  "Mutations" = "Mutations",
  "Risk factors" = "Risk factors",
  "Reproduction number" = "Reproduction number",
  "Severity" = "Severity",
  "Mosquito" = "Mosquito",
  "Relative" = "Relative contribution",
  "Overdispersion" = "Overdispersion",
  "Attack rate" = "Attack rate",
  "Doubling time" = "Doubling time",
  "Growth rate" = "Growth rate"
)

param_df$parameter_class <- assign_parameter_class(
  param_df$parameter_type,
  group_mapping,
  default = "Other transmission parameters"
  )

# *---------------------------- Combine intervals -----------------------------*
interval_combine_list <- list(
  "parameter_bounds"=c("parameter_lower_bound", "parameter_upper_bound"),
  "uncertainty_bounds"=c("parameter_uncertainty_lower_value",
                         "parameter_uncertainty_upper_value"),
  "parameter_2_bounds"=c("parameter_2_lower_bound", "parameter_2_upper_bound"),
  "uncertainty_2_bounds"=c("parameter_2_uncertainty_lower_value",
                         "parameter_2_uncertainty_upper_value"),
  "parameter_2_sample_bounds"=c("parameter_2_sample_paired_lower",
                                "parameter_2_sample_paired_upper")
  )

for (name in names(interval_combine_list)){
  param_df[name] <- combine_interval(param_df,
                                     interval_combine_list[[name]][1],
                                     interval_combine_list[[name]][2])
}
# *----------------------------- Combine columns ------------------------------*
col_combine_list <- list(
  "comb_par1_uncertainty_type"=list(
    c("parameter_uncertainty_lower_value",
      "parameter_uncertainty_upper_value",
      "parameter_uncertainty_single_value"),
    c("parameter_uncertainty_type",
      "parameter_uncertainty_type",
      "parameter_uncertainty_single_type")
    ),
  "comb_par1_uncertainty"=list(
    c("uncertainty_bounds",
      "parameter_uncertainty_single_value"),
    c("uncertainty_bounds",
      "parameter_uncertainty_single_value")
    ),
  "comb_par2_uncertainty_type"=list(
    c("parameter_2_uncertainty_lower_value",
      "parameter_2_uncertainty_upper_value",
      "parameter_2_uncertainty_single_value"),
    c("parameter_2_uncertainty_type",
      "parameter_2_uncertainty_type",
      "parameter_2_uncertainty_single_type")
    ),
  "comb_par2_uncertainty"=list(
    c("uncertainty_2_bounds",
      "parameter_uncertainty_single_value"),
    c("uncertainty_bounds",
      "parameter_2_uncertainty_single_value")
  )
)

for (name in names(col_combine_list)){
  param_df[name] <- combine_cols(param_df,
                                 col_combine_list[[name]][[1]],
                                 col_combine_list[[name]][[2]])
}
# *--------------------------- Filter and relocate ----------------------------*
param_df <- filter_cols(param_df,
                        c(article_filter_cols, "access_param_id"))
param_df <- relocate_and_arrange(
  param_df,
  c(base_relocate_cols, "parameter_data_id"),
  order_col)

# *------------------------------- Save output --------------------------------*
write_csv(article_df, "articles.csv")
write_csv(model_df, "models.csv")
write_csv(outbreak_df, "outbreaks.csv")
write_csv(param_df, "params.csv")
