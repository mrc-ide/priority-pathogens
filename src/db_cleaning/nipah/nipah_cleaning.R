# *========================== Manual Niaph cleaning ===========================*
library(tidyr)

PARAMETER_2_BASE <- c(
  "parameter_2_unit"="parameter_unit",
  "exponent_2"="exponent",
  "inverse_param_2"="inverse_param",
  "method_2_from_supplement"="method_from_supplement",
  "parameter_2_statistical_approach"="parameter_statistical_approach")

SINLGE_MAP_FROM_TO_VEC <- c(
  "parameter_2_value_type"="parameter_uncertainty_single_type",
  "parameter_2_value"="parameter_uncertainty_single_value",
  PARAMETER_2_BASE
)

SINLGE_SET_TO_NA_VEC <- c("parameter_uncertainty_single_type",
                          "parameter_uncertainty_single_value")

PAIRED_MAP_FROM_TO_VEC <- c(
  "parameter_2_value_type"="parameter_uncertainty_type",
  "parameter_2_lower_bound"="parameter_uncertainty_lower_value",
  "parameter_2_upper_bound"="parameter_uncertainty_upper_value",
  PARAMETER_2_BASE)

PAIRED_SET_TO_NA_VEC <- c("parameter_uncertainty_type",
                          "parameter_uncertainty_lower_value",
                          "parameter_uncertainty_upper_value")

uncert_to_varb <- function(df, map_to_from_vec, set_to_na_vec){
  df[,names(map_to_from_vec)] <- df[,map_to_from_vec]

  df[,set_to_na_vec] <- NA

  df[, "parameter_paired"] <- "Yes"

  return(df)
}

article_cleaning <- function(df){
  # Update Not Applicable so that epireview assign_qa_score works
  df[df$covidence_id==207, "first_author_first_name"] <- "Khean Jin"
  df[df$covidence_id==207, "first_author_surname"] <- "Goh"

  qa_cols <- c("qa_m1", "qa_m2", "qa_a3", "qa_a4", "qa_d5", "qa_d6", "qa_d7")
  df <- df |>
    mutate(across(all_of(qa_cols), ~ ifelse(.=="Not Applicable", NA, .)))

  # Leave first_author_first_name as is incase there are initials as the first
  # name
  df <- df |>
    mutate(journal = str_to_title(journal),
           first_author_surname = str_to_title(first_author_surname)) |>
    mutate(journal = sub("^The\\s+", "", journal, useBytes = TRUE))

  return (df)
}

model_cleaning <- function(df){

  df[(df$covidence_id==2800) & (df$stoch_deter=="Deterministic model"),
     "stoch_deter"] <- "Stochastic model"

  df[(df$covidence_id==4124) & is.na(df$theoretical_model),
     "theoretical_model"] <- "Yes"

  return (df)
}

outbreak_cleaning <- function(df){

  df <- df %>%
    mutate(outbreak_location = str_to_title(outbreak_location),
           outbreak_location = str_replace_all(outbreak_location, " And", ";" ),
           outbreak_location = str_replace_all(outbreak_location, ",", ";" ),
           outbreak_location = str_replace_all(outbreak_location, "Kerela", "Kerala" ),
           outbreak_location = case_when(outbreak_location == "Serembna Hospital" ~ "Seremban Hospital",
                                         outbreak_location == "University Of Malaya Medical Center" ~ "University Of Malaya Medical Centre",
                                         outbreak_location == "Suspected To Be Infected With The Virus From Pig Farms Situated In Bukit Pelandok" ~ "Bukit Pelandok" ,
                                         TRUE ~ outbreak_location))

  return (df)
}

param_cleaning <- function(df){
  na_replacement <- list(parameter_from_figure = "No",
                         inverse_param = "No",
                         exponent=0,
                         parameter_value_type="Unspecified")

  df <- replace_na(df, na_replacement)

  no_true_false <- c("parameter_from_figure",
                     "inverse_param")

  df[no_true_false] <- lapply(no_true_false,
                              function(col) df[[col]] != "No")

  # do the same for non empty variability
  non_empty_param_2_filter <- (!is.na(df$parameter_2_lower_bound) |
                                 !is.na(df$parameter_2_upper_bound) |
                                 !is.na(df$parameter_2_value)|
                                 !is.na(df$parameter_2_value_type))

  non_empty_param_2_df <- df[non_empty_param_2_filter, ]
  non_empty_param_2_df <- replace_na(non_empty_param_2_df,
                                     list(inverse_param_2 = "No",
                                          exponent_2=0))
  df[non_empty_param_2_filter,] <- non_empty_param_2_df

  df[["inverse_param_2"]] <- unname(sapply(
    df[["inverse_param_2"]], function(x) ifelse(!is.na(x), x != "No", NA)))

  df <- df |>
    mutate(population_location = str_to_title(population_location),
           population_location = str_replace_all(population_location, " And", ";"),
           population_location = str_replace_all(population_location, ",", ";"),
           population_location = str_replace_all(population_location, "[(]Bangladesh[)]", ";"),
           population_location = str_replace_all(population_location, "[(]India[)]", ";"),
           population_location = str_replace_all(population_location, "Serembna Hospital", "Seremban Hospital"),
           population_location = str_replace_all(population_location, "Kerela", "Kerala"),
           population_location = str_replace_all(population_location, "Suspected To Be Infected With The Virus From Pig Farms Situated In Bukit Pelandok", "Bukit Pelandok"),
           population_location = str_trim(population_location, side = "both")
    )

  # TODO: Decide whether to remove other_delay_*
  df <- df |>
    mutate(parameter_hd_from = ifelse(parameter_hd_from %in% c("Other", ""),
                                      other_delay_start, parameter_hd_from),
           parameter_hd_to = ifelse(parameter_hd_to %in% c("Other", ""),
                                    other_delay_end, parameter_hd_to),
           parameter_value_type = ifelse(parameter_value_type=="Central - unspecified",
                                         "Unspecified", parameter_value_type)
           ) |>
    select(-c(other_delay_start, other_delay_end))

  # TODO: Derived rows such as parameter_bounds should be created after cleaning
  # Transmission
  # R0
  # CovID 1030
  # Row extracted as variability paired parameter - paired range should be
  Basic_R_1030_filter <- ((df$covidence_id == 1030) &
                            (df$parameter_type=="Reproduction number (Basic R0)"))

  # Reverse direction so not using uncert_to_varb
  filtered_row <- df[Basic_R_1030_filter, ]
  filtered_row[,"parameter_lower_bound"] <- filtered_row[, "parameter_2_sample_paired_lower"]
  filtered_row[,"parameter_upper_bound"] <- filtered_row[, "parameter_2_sample_paired_upper"]
  filtered_row[,"parameter_paired"] <- NA
  filtered_row[,"exponent_2"] <- NA
  filtered_row[,"inverse_param_2"] <- NA

  filtered_row[grepl("parameter_2", colnames(filtered_row))] <- NA

  df[Basic_R_1030_filter, ] <- filtered_row

  Basic_R_3051_filter <- ((df$covidence_id == 3051) &
                           (df$parameter_type=="Reproduction number (Basic R0)"))
  df[Basic_R_3051_filter, "population_group"] <- "Persons under investigation"

  # Substitution rates
  # CovID 2760
  evolutionary_rate_2760_filter <- ((df$covidence_id == 2760) &
                                      (df$parameter_type=="Mutations - substitution rate"))

  # Parameter bounds for the below will be incorrect
  # Convert to exp -4
  df[evolutionary_rate_2760_filter, "parameter_value"] <- 11
  df[evolutionary_rate_2760_filter, "parameter_uncertainty_lower_value"] <- 7.37
  df[evolutionary_rate_2760_filter, "parameter_uncertainty_upper_value"] <- 15
  df[evolutionary_rate_2760_filter, "exponent"] <- -4
  df[evolutionary_rate_2760_filter, "genome_site"] <- "Nucleocapsid"

  # CovID 3058 substitution rate
  evolutionary_rate_3058_filter <- ((df$covidence_id == 3058) &
                                      (df$parameter_type=="Mutations - substitution rate"))
  df[evolutionary_rate_3058_filter, "parameter_uncertainty_lower_value"] <- 2.9
  df[evolutionary_rate_3058_filter, "parameter_uncertainty_upper_value"] <- 6
  df[evolutionary_rate_3058_filter, "genome_site"] <- "Nucleocapsid"

  df[(df$covidence_id==1171) & (df$parameter_type=="Mutations - evolutionary rate"),
     "genome_site"] <- "Nucleocapsid"

  df[(df$covidence_id==906) & (df$parameter_type=="Overdispersion"),
     "parameter_unit"] <- "Max. nr. of cases superspreading (related to case)"

  # Proportion symptomatic
  prop_symp_filter <- df$parameter_type=="Severity - proportion of symptomatic cases"

  # CovID: 906
  prop_symp_906_filter <- (df$covidence_id==906 & prop_symp_filter)

  # Value is 0 based on CFR but not yet assigned
  df[prop_symp_906_filter, "parameter_value"] <- 100
  df[prop_symp_906_filter, "inverse_param"] <- FALSE
  df[prop_symp_906_filter, "parameter_notes"] <-
    paste0("Proportion asymptomatic is reported in the paper.",
           "Denominator is contacts of Nipah patients who gave blood specimen")

  # CovID: 3025
  prop_symp_3025_filter <- (df$covidence_id==3025 & prop_symp_filter)

  df[prop_symp_3025_filter, "parameter_value"] <- 100 - df[prop_symp_3025_filter,
                                                           "parameter_value"]
  df[prop_symp_3025_filter, "parameter_notes"] <- ("Proportion asymptomatic is
                                                  reported in the paper")

  # CovID: 828
  df[df$covidence_id==828, "population_group"] <- "Persons under investigation"

  # CovID: 2886
  df[df$covidence_id==2886 &
       df$parameter_type=="Seroprevalence - IgG", "cfr_ifr_denominator"] <- 18

  # CovID: 207, Nadir delay filter
  nadir_delay_filter <- (
    df$covidence_id==207 &
      df$parameter_type=="Human delay - other human delay (go to section)"
    )

  df[nadir_delay_filter, "parameter_notes"] <-
  paste0("Nadir defined as the worst conscious level or the need for,
         mechanical ventilation")


  # CovID: 271: parameter_2_value_type is blank
  df[df$parameter_data_id=="ece79cd53c0bed654a74fdf4b280b8ef",
     "parameter_2_value_type"] <- "Range"

  # CovID: 274
  # parameter_2_value_type mistakenly filled in as uncertainty single type
  # 3f22cc21917075a5f1153c9a8563c57f has both a paired and single varb

  unc_sing_274_filter <- (df$covidence_id==274 &
                            !is.na(df$parameter_uncertainty_single_type))

  df[unc_sing_274_filter, ] <- uncert_to_varb(
    df[unc_sing_274_filter, ],
    c("parameter_2_value_type"="parameter_uncertainty_single_type"),
    "parameter_uncertainty_single_type")

  df[unc_sing_274_filter, "parameter_notes"] <- paste0(
    df[unc_sing_274_filter, "parameter_notes"], ";",
    "Single Other variability value is likely standard deviation, but not ",
    "explicitly stated")

  pair_sing_varb <- df$parameter_data_id=="3f22cc21917075a5f1153c9a8563c57f"

  df[pair_sing_varb, "parameter_notes"] <- paste0(
    df[unc_sing_274_filter & !pair_sing_varb, "parameter_notes"], ";",
    "Also contains an Other (likely range,  but not explicitly stated) paired ",
    "variability of 1 to 32 Days")

  # CovID: 2886,Incubation period relates to cases (confirmed/probable => PUI)
  # df[df$covidence_id==2886, "population_group"] <- "Persons under investigation"

  # Variability vs. uncertainty:
  # CovID 103 - Human delay - symptom onset>death
  # c308153a0234e0da80717c0440f479a1
  delay_filter <- grepl("Human delay", df$parameter_type)
  delay_103_filter <- df$covidence_id==103 & delay_filter
  df[delay_103_filter, ] <- uncert_to_varb(df[delay_103_filter, ],
                                           PAIRED_MAP_FROM_TO_VEC,
                                           PAIRED_SET_TO_NA_VEC)

  # CovID 345: All three rows need the same update
  # Add lower bounds for
  # 1048c95b02719626d563e887d51e690b and 31eee995f6fd838e73fd39f05780f627
  delay_345_filter <- df$covidence_id==345 & delay_filter
  df[delay_345_filter, ] <- uncert_to_varb(df[delay_345_filter, ],
                                           PAIRED_MAP_FROM_TO_VEC,
                                           PAIRED_SET_TO_NA_VEC)
  problem_row_851_filter <- df$parameter_data_id == "4219e8649aeb55c796dc05fa8c7accc2"
  delay_851_filter <- (df$covidence_id==851 &
                         delay_filter &
                         !problem_row_851_filter)
  df[delay_851_filter, ] <- uncert_to_varb(df[delay_851_filter, ],
                                           PAIRED_MAP_FROM_TO_VEC,
                                           PAIRED_SET_TO_NA_VEC)

  # SD removed - favour SD?
  problem_row_851_filter <- df$parameter_data_id == "4219e8649aeb55c796dc05fa8c7accc2"

  # To remove SD: (Also rmeove problem row from filter above)
  # df[problem_row_851_filter,
  #    c("parameter_uncertainty_single_value_type",
  #      "parameter_uncertainty_single_value",
  #      "parameter_2_value")] <- NA
  # df[problem_row_851_filter, "parameter_notes"] <- paste0(
  #   df[problem_row_851_filter, "parameter_notes"],
  #   "; Standard deviation=4.6 also provided.")
  df[problem_row_851_filter, ] <- uncert_to_varb(df[problem_row_851_filter, ],
                                                 SINLGE_MAP_FROM_TO_VEC,
                                                 SINLGE_SET_TO_NA_VEC)

  df[problem_row_851_filter, PAIRED_SET_TO_NA_VEC] <- NA

  df[problem_row_851_filter, "parameter_notes"] <- paste0(
    df[problem_row_851_filter, "parameter_notes"],
    "; Range [2, 36] also provided.")


  # CovID: 271, param data ID 048b23e7de13b62f87c16b04a4887711
  # Incubation period parameter_value_type set to unspecified by cleaning code
  # but should be NA; is this allowed?

  p2_unc_var_167_filter <- (df$parameter_data_id=="a594f96baf779b62d63430eaadfb8d58")

  # Inverse and exponent are blank
  df[p2_unc_var_167_filter, ] <- uncert_to_varb(
    df[p2_unc_var_167_filter, ],
    c("parameter_2_value_type"="parameter_2_uncertainty_type",
      "parameter_2_lower_bound"="parameter_2_uncertainty_lower_value",
      "parameter_2_upper_bound"="parameter_2_uncertainty_upper_value",
      "exponent_2"="exponent",
      "inverse_param_2"="inverse_param",
      "method_2_from_supplement"="method_from_supplement"),
    c("parameter_2_uncertainty_type",
      "parameter_2_uncertainty_lower_value",
      "parameter_2_uncertainty_upper_value"))

  unc_p2_var_167_filter <- (df$parameter_data_id=="858c08fd00f061d9845d1ea5d3b836a3")

  df[unc_p2_var_167_filter, ] <- uncert_to_varb(
    df[unc_p2_var_167_filter, ],
    c("parameter_2_value_type"="parameter_uncertainty_type"),
    "parameter_uncertainty_type")

  delay_4055_filter <- df$covidence_id==4055 & delay_filter
  df[delay_4055_filter, ] <- uncert_to_varb(
    df[delay_4055_filter, ],
    SINLGE_MAP_FROM_TO_VEC,
    SINLGE_SET_TO_NA_VEC)

  df[delay_4055_filter, "parameter_2_value_type"] <- "Other"

  df[delay_4055_filter, "parameter_notes"] <- paste0(
    df[delay_4055_filter, "parameter_notes"], ";",
    "Other variability is likely Standard Deviation, but not explictly stated")


  delay_4055_filter <- df$covidence_id==4047 & delay_filter

  df[delay_4055_filter, ] <- uncert_to_varb(
    df[delay_4055_filter, ],
    c("parameter_2_lower_bound"="parameter_2_sample_paired_lower",
      "parameter_2_upper_bound"="parameter_2_sample_paired_upper",
      "exponent_2"="exponent",
      "inverse_param_2"="inverse_param",
      "method_2_from_supplement"="method_from_supplement"),
    c("parameter_2_sample_paired_lower",
      "parameter_2_sample_paired_upper"))

  delay_1110_filter <- df$covidence_id==1110 & delay_filter

  df[delay_1110_filter, ] <- uncert_to_varb(
    df[delay_1110_filter, ],
    c("parameter_2_lower_bound"="parameter_2_sample_paired_lower",
      "parameter_2_upper_bound"="parameter_2_sample_paired_upper",
      "exponent_2"="exponent",
      "inverse_param_2"="inverse_param"),
    c("parameter_2_sample_paired_lower",
      "parameter_2_sample_paired_upper"))

  delay_2825_filter <- df$covidence_id==2825 & delay_filter

  df[delay_2825_filter, ] <- uncert_to_varb(
    df[delay_2825_filter, ],
    c("parameter_2_lower_bound"="parameter_2_sample_paired_lower",
      "parameter_2_upper_bound"="parameter_2_sample_paired_upper",
      "exponent_2"="exponent",
      "inverse_param_2"="inverse_param",
      "parameter_2_unit"="parameter_unit"),
    c("parameter_2_sample_paired_lower",
      "parameter_2_sample_paired_upper"))

  delay_2886_filter <- df$covidence_id==2886 & delay_filter
  df[delay_2886_filter, ] <- uncert_to_varb(df[delay_2886_filter, ],
                                            PAIRED_MAP_FROM_TO_VEC,
                                            PAIRED_SET_TO_NA_VEC)


  df[(df$covidence_id==1007) & df$population_study_start_year=='xxxx',
     "population_study_start_year"] <- NA

  df[(df$covidence_id==1007) & df$population_study_end_year=='xxxx',
     "population_study_start_year"] <- NA

  df[(df$covidence_id==4046) & as.double(df$population_study_start_day)>31,
     "population_study_start_day"] <- NA

  df[(df$covidence_id==4046) & as.double(df$population_study_end_day)>31,
     "population_study_end_day"] <- NA

  df[(df$covidence_id==4046) & as.double(df$population_study_start_month)>31,
     "population_study_start_month"] <- NA

  df[(df$covidence_id==4046) & as.double(df$population_study_end_month)>31,
     "population_study_end_month"] <- NA

  # CovID 3065, incubation period, 5c96779d606434a611beb693d0d6c2c1
  # Assumes that +- is Other uncertainty; since it's not estimated it must be
  # variability? Also Parameter_2_value_type is blank but a range provided for
  # variability
  df[df$parameter_data_id=="5c96779d606434a611beb693d0d6c2c1",
     "parameter_2_value_type"] <- "Range (paired)"

  # Labels for IQR and Range are different for variability so copying from
  # uncertainty results in different labels
  df <- df |>
    mutate(parameter_2_value_type=case_when(
      parameter_2_value_type=="Range (paired)" ~ "Range",
      parameter_2_value_type=="IQR (paired or unpaired)" ~ "IQR",
      TRUE~parameter_2_value_type))

  return (df)
}
# *============================================================================*
