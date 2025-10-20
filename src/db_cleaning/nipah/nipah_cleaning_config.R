# *================================== Nipah ===================================*
# *---------------------------- Tables to include -----------------------------*
# NB! this needs to match the output and rds filenames
tables <- c("articles", "outbreaks", "models", "params")

# *------------------------ Update column types action ------------------------*
# Update column types; the list element names are column names
# Columns which are completely NA will default to logical
# (e.g. exponent 2 for Nipah)
type_map_list <- list("cfr_ifr_numerator" = as.integer,
                      "population_study_start_day" = as.numeric,
                      "exponent_2"=as.numeric)

# *---------------------------- Update punctuation ----------------------------*
# Replace commas with semi-colons
cols_to_punctuate_vec <- c("method_disaggregated_by",
                           "population_location",
                           "population_country")

# *--------------------------- Update values action ---------------------------*
# The name of the entries in update_values_list are the names of the columns
# each column will related to a named vector, where names are the name to map
# to and the values are values to map

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

# *---------------------------- Group types action ----------------------------*
#
group_mapping_vec <- c(
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

# *------------------------- Combine intervals action -------------------------*
# The name of list item is the new column names
# The values in the vector are the columns to combine
# Combined as "value_1 - value_2", only if both col values are not NA

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

# *-------------------------- Combine columns action --------------------------*
# The name of list item is the new column names
# The first vector is the values to check and the the second vector is the
# corresponding value to coalesce if non-NA
# For cases where the vector is repeated (e.g. comb_par1_uncertainty), this is
# the same as a standard coalesce
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
