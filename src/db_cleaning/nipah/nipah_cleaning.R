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

generate_new_id <- function(df, id_col_name, iterations){
  # different seed used to generate ids, try `iterations` seeds and then abort.
  # Assumes that this code has not been used `iterations` times already.
  seed_start <- 1110
  for (i in 1:iterations){
    set.seed(seed_start + i)
    new_row_id <- ids::random_id(1)

    if (!(new_row_id %in% df[[id_col_name]])){
      break
    }
    else{
      new_row_id <- ""
    }
  }

  if (new_row_id == ""){
    cli_abort(paste("Too many clashes, cannot allocate a new", id_col_name))
  }

  return(new_row_id)
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

  # Captured as Airborne or close contact,Human to human (direct contact),
  # Vector/Animal to human, which is incorrect based on the paper
  # Spillover risk mapping - debatable whether to include?
  air_trans_filter <- df$model_data_id == "134c4f546d4fe709e5752e14eba11272"
  df[air_trans_filter,"transmission_route"] <- "Vector/Animal to human,Unspecified"


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

  # Exponent 2 is logical since completely NA
  df[["exponent_2"]] <- as.numeric(df[["exponent_2"]])

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

  # Removed parameter_hd_to, parameter_hd_from since these were introduced in
  # Redcap and somewhat duplicates capturing non standard human delays.
  # The field other_delay_* is included in epireview, so combine columns and
  # keep other_delay_*
  df <- df |>
    mutate(other_delay_start = ifelse(
      parameter_hd_from=="Other" | is.na(parameter_hd_from),
      other_delay_start, parameter_hd_from),
      other_delay_end = ifelse(
        parameter_hd_to=="Other" | is.na(parameter_hd_to),
        other_delay_end, parameter_hd_to),
      parameter_value_type = ifelse(parameter_value_type=="Central - unspecified",
                                    "Unspecified", parameter_value_type)
    ) |>
    select(-c(parameter_hd_to, parameter_hd_from))

  # Risk factors:
  # CovID: issing risk_factor_name 345
  rf_name_345_filter <- df$parameter_data_id=="4fe0bcac648be47c0c8d0e5d3b96d34e"
  df[rf_name_345_filter, "riskfactor_name"] <- "Other"

  # CovID 906 missing risk_factor_outcome
  rf_out_906_filter <- df$parameter_data_id=="d5ec26b742179d129b2756258cad67af"
  df[rf_out_906_filter, "riskfactor_outcome"] <- "Infection"

  rf_906_new_row_filter <- df$parameter_data_id=="3b58c6409e7bdda9c743dbd19584e249"
  new_906_row <- df[rf_906_new_row_filter, ]

  new_906_row$parameter_data_id  <- generate_new_id(df, "parameter_data_id", 10)
  new_906_row$riskfactor_name <- "Environmental;Other"
  new_906_row$riskfactor_significant <- "Not significant"
  df <- rbind(df, new_906_row)

  # Missing a risk factor

  # 1150 - 101 CFR - data entry mistake
  cfr_101_1150_filter <-df$parameter_data_id  %in% c("905153f64953884bd05cd0f5de7552e6",
                                                     "ac8209ed2df561b8c92f0f7243630bff",
                                                     "66a2b5ea97e02365e39eb3b47f730957")
  df[cfr_101_1150_filter, "parameter_value"] <- 100

  # Parameter_statistical approach error (currently Case study [Oropouche ONLY])
  df[df$parameter_data_id=="905153f64953884bd05cd0f5de7552e6",
     "parameter_statistical_approach"] <- "Unspecified"

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

  # CovID: 2886
  df[df$covidence_id==2886 &
       df$parameter_type=="Seroprevalence - IgG", "cfr_ifr_denominator"] <- 18

  delay_filter <- grepl("Human delay", df$parameter_type)

  # CovID: 271: parameter_2_value_type is blank
  df[df$parameter_data_id=="ece79cd53c0bed654a74fdf4b280b8ef",
     "parameter_2_value_type"] <- "Range"
  # Only variability
  df[df$covidence_id==271 & delay_filter, "parameter_value_type"] <- NA

  # CovID: 274
  # parameter_2_value_type mistakenly filled in as uncertainty single type
  # (presumably updated during fixing but uncertainty type not moved)
  # 3f22cc21917075a5f1153c9a8563c57f has both a paired and single varb
  unc_sing_274_filter <- (df$covidence_id==274 &
                            !is.na(df$parameter_uncertainty_single_type))

  df[unc_sing_274_filter, ] <- uncert_to_varb(
    df[unc_sing_274_filter, ],
    c("parameter_2_value_type"="parameter_uncertainty_single_type",
      PARAMETER_2_BASE),
    SINLGE_SET_TO_NA_VEC)

  df[unc_sing_274_filter, "parameter_notes"] <- sapply(
    df[unc_sing_274_filter, "parameter_notes"], function(notes) paste0(
      ifelse(!is.na(notes), paste0(notes, "; "), ""),
      "Single Other variability value is likely standard deviation, but not ",
      "explicitly stated")
  )

  pair_sing_varb <- df$parameter_data_id=="3f22cc21917075a5f1153c9a8563c57f"

  df[pair_sing_varb, "parameter_notes"] <- paste0(
    df[pair_sing_varb, "parameter_notes"], ";",
    "Also contains an Other (likely range,  but not explicitly stated) paired ",
    "variability of 1 to 32 Days")

  # Only the next two delays were reported as means; other delays are likely
  # means but not explicitly stated and so extracted as reported.
  df[pair_sing_varb, "parameter_value_type"] <- "Mean"

  other_delay_vent_274_filter <- (
    df$parameter_data_id=="ab8bc967fe110c5a76b4e6fd0059da71")
  df[other_delay_vent_274_filter, "paramater_value_type"] <- "Mean"


  # 274: Other delays as reported in paper, but paper has a typo
  other_delay_lymp_274_filter <- (
    df$parameter_data_id=="087df63276dc74e9489b4fec1c6f8173")
  df[other_delay_lymp_274_filter, "other_delay_end"] <- "Lymphopenia"

  other_delay_thro_274_filter <- (
    df$parameter_data_id=="8de15b956e5a565cac8795de6a275c3e")
  df[other_delay_thro_274_filter, "other_delay_end"] <- "Thrombocytopenia"

  other_delay_thro_274_filter <- (
    df$parameter_data_id=="8de15b956e5a565cac8795de6a275c3e")
  df[other_delay_thro_274_filter, "other_delay_end"] <- "Thrombocytopenia"

  # CovID: 207, Nadir delay filter
  nadir_delay_filter <- (
    df$covidence_id==207 &
      df$parameter_type=="Human delay - other human delay (go to section)"
  )

  df[nadir_delay_filter, "parameter_notes"] <-
    paste0("Nadir defined as the worst conscious level or the need for,
         mechanical ventilation")
  df[nadir_delay_filter, "parameter_value_type"] <- "Mean"

  delay_207_filter <- df$covidence_id==207 & delay_filter

  # Missing parameter_2_value_type checked from paper
  df[delay_207_filter, "parameter_2_value_type"] <- "Range"

  # CovID: 184
  # Missing parameter_2_value_type checked from paper
  delay_184_filter <- df$covidence_id==184 & delay_filter
  df[delay_184_filter, "parameter_2_value_type"] <- "Range"

  # CovID: 947
  # Missing parameter_2_value_type checked from paper
  delay_947_filter <- df$covidence_id==947 & delay_filter
  df[delay_947_filter, "parameter_2_value_type"] <- "Range"

  # CovID: 956
  # Missing parameter_2_value_type checked from paper
  delay_956_filter <- df$covidence_id==956 & delay_filter
  df[delay_956_filter, "parameter_2_value_type"] <- "Range"

  # CovID 167 so_d row:
  so_d_167_filter <- df$parameter_data_id == "04e6e787b667c1eca4ff110bbb274a82"
  df[so_d_167_filter, "parameter_2_value_type"] <- df[so_d_167_filter, "parameter_uncertainty_type"]
  df[so_d_167_filter, "parameter_uncertainty_type"] <- NA

  # CovID 275 so_d row:
  # Missing parameter_2_value_type checked from paper
  # Upper bound likely 32?
  so_d_275_filter <- df$parameter_data_id == "79ec019c7a249cf86a85a17cd1e12500"
  df[so_d_275_filter, "parameter_2_value_type"] <- "Range"

  # CovID 833 so_d row:
  # Missing parameter_2_value_type checked from paper
  delay_833_filter <- df$covidence_id==833 & delay_filter
  df[delay_833_filter, "parameter_2_value_type"] <- "Range"

  # CovID: 2886,Incubation period relates to cases (confirmed/probable => PUI)
  # df[df$covidence_id==2886, "population_group"] <- "Persons under investigation"
  # Variability vs. uncertainty:
  # CovID 103 - Human delay - symptom onset>death
  # c308153a0234e0da80717c0440f479a1
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

  # Duration between neurological episodes
  delay_345_dne_filter <- df$parameter_data_id == "1048c95b02719626d563e887d51e690b"
  df[delay_345_dne_filter, "parameter_2_lower_bound"] <- 9
  df[delay_345_dne_filter, "parameter_2_upper_bound"] <- 365
  df[delay_345_dne_filter, "parameter_2_unit"] <- "days"
  df[delay_345_dne_filter, "parameter_notes"] <- paste0(
    df[delay_345_dne_filter, "parameter_notes"],
    "Variability upper bound reported as 12 months in the paper.")

  # Time until first neurological episode
  delay_345_fne_filter <- df$parameter_data_id == "31eee995f6fd838e73fd39f05780f627"
  df[delay_345_fne_filter, "parameter_2_lower_bound"] <- 1

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

  df[delay_4055_filter, "parameter_notes"] <- sapply(
    df[delay_4055_filter, "parameter_notes"], function(notes) paste0(
      ifelse(!is.na(notes), paste0(notes, "; "), ""),
      "Single Other variability value is likely standard deviation, but not ",
      "explicitly stated")
  )

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

  # CovID: 828
  df[df$covidence_id==828, "population_group"] <- "Persons under investigation"
  delay_828_filter <- df$covidence_id==828 & delay_filter
  df[delay_828_filter, ] <- uncert_to_varb(
    df[delay_828_filter, ],
    c("parameter_2_lower_bound"="parameter_lower_bound",
      "parameter_2_upper_bound"="parameter_upper_bound",
      "exponent_2"="exponent",
      "inverse_param_2"="inverse_param",
      "parameter_2_unit"="parameter_unit"),
    c("parameter_lower_bound",
      "parameter_upper_bound",
      "parameter_unit",
      "parameter_value_type",
      "parameter_statistical_approach"))

  df[delay_828_filter, "parameter_2_value_type"] <- "Range"

  # CovID: 4365
  df[df$covidence_id==4365, "population_group"] <- "Persons under investigation"

  # across all
  delay_4365_filter <- df$covidence_id==4365 & delay_filter
  df[delay_4365_filter, ] <- uncert_to_varb(
    df[delay_4365_filter, ],
    c("parameter_2_lower_bound"="parameter_lower_bound",
      "parameter_2_upper_bound"="parameter_upper_bound",
      "exponent_2"="exponent",
      "inverse_param_2"="inverse_param",
      "parameter_2_unit"="parameter_unit",
      "method_2_from_supplement"="method_from_supplement"),
    c("parameter_lower_bound",
      "parameter_upper_bound"))

  df[delay_4365_filter, "parameter_2_value_type"] <- "Range"

  # Only variablity
  delay_4365_varb_only_filter<- df$parameter_data_id=="d40430087d526cd7ac3fe1b26e78eb05"
  df[delay_4365_varb_only_filter, "parameter_unit"] <- NA
  df[delay_4365_varb_only_filter, "parameter_value_type"] <- NA
  df[delay_4365_varb_only_filter, "parameter_statistical_approach"] <- NA
  df[delay_4365_varb_only_filter, "parameter_from_figure"] <- FALSE
  df[delay_4365_varb_only_filter, "method_from_supplement"] <- NA

  # CovID: 4358
  # Case report range
  delay_4358_filter <- df$covidence_id==4358 & delay_filter
  df[delay_4358_filter, ] <- uncert_to_varb(
    df[delay_4358_filter, ],
    c("parameter_2_lower_bound"="parameter_lower_bound",
      "parameter_2_upper_bound"="parameter_upper_bound",
      "exponent_2"="exponent",
      "inverse_param_2"="inverse_param",
      "parameter_2_unit"="parameter_unit",
      "method_2_from_supplement"="method_from_supplement"),
    c("parameter_lower_bound",
      "parameter_upper_bound",
      "parameter_unit",
      "parameter_value_type",
      "parameter_statistical_approach",
      "method_from_supplement"))

  df[delay_4358_filter, "parameter_2_value_type"] <- "Range"

  # CovID: 292
  delay_292_onset_test_filter <- (
    df$parameter_data_id=="e997fb68799ea08e3b31281d5b71c19d")
  df[delay_292_onset_test_filter, ] <- uncert_to_varb(
    df[delay_292_onset_test_filter, ],
    c("parameter_2_lower_bound"="parameter_lower_bound",
      "parameter_2_upper_bound"="parameter_upper_bound",
      "exponent_2"="exponent",
      "inverse_param_2"="inverse_param",
      "parameter_2_unit"="parameter_unit",
      "method_2_from_supplement"="method_from_supplement",
      "parameter_2_statistical_approach"="parameter_statistical_approach"),
    c("parameter_lower_bound",
      "parameter_upper_bound",
      "parameter_unit",
      "parameter_value_type",
      "method_from_supplement",
      "parameter_statistical_approach"))

  df[delay_292_onset_test_filter, "parameter_2_value_type"] <- "Range"

  # CovID: 976 - range is empirical
  so_d_delay_976_filter <- df$parameter_data_id=="519adf0939501de6e05bc75c42df4477"

  df[so_d_delay_976_filter, ] <- uncert_to_varb(
    df[so_d_delay_976_filter, ],
    c("parameter_2_lower_bound"="parameter_lower_bound",
      "parameter_2_upper_bound"="parameter_upper_bound",
      "exponent_2"="exponent",
      "inverse_param_2"="inverse_param",
      "parameter_2_unit"="parameter_unit"),
    c("parameter_lower_bound",
      "parameter_upper_bound"))

  df[so_d_delay_976_filter, "parameter_2_value_type"] <- "Range"

  # CovID: 1007
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

  # CovID 3065:
  # "Average duration of hospital stay for the deceased" => admission to death
  # (not time in case as exactracted)
  df[df$parameter_data_id=="69b9c2b889184a964beefb16f39c0b57",
     "parameter_type"] <- "Human delay - admission to care>death"
  df[df$parameter_data_id=="69b9c2b889184a964beefb16f39c0b57",
     "parameter_value_type"] <- "Mean"

  # CovID: 960, Human delay - symptom onset>death,
  # 4c8cb9c84f098338aedd1800f75b7e3f
  so_d_delay_960_filter <- df$parameter_data_id=="4c8cb9c84f098338aedd1800f75b7e3f"

  df[so_d_delay_960_filter, "parameter_value"] <- df[so_d_delay_960_filter,
                                                     "exponent"]

  df[so_d_delay_960_filter, "exponent"] <- 0

  # Range is empirical
  df[so_d_delay_960_filter, ] <- uncert_to_varb(
    df[so_d_delay_960_filter, ],
    c("parameter_2_lower_bound"="parameter_lower_bound",
      "parameter_2_upper_bound"="parameter_upper_bound",
      "exponent_2"="exponent",
      "inverse_param_2"="inverse_param",
      "parameter_2_unit"="parameter_unit"),
    c("parameter_lower_bound",
      "parameter_upper_bound"))

  df[so_d_delay_960_filter, "parameter_2_value_type"] <- "Range"

  # R0 NA - no units CovID: 1030, 3679, 833
  df[df$parameter_data_id %in% c("7bbba6a459e8e222d8bb8665b99fcd0f",
                                 "282ad8bba50737c60d12d597a2531318",
                                 "88e905b58514400524f8f20f8268dc81"),
     "parameter_unit"] <- "No units"

  # CovID 906, missing unit
  df[df$parameter_data_id=="ba6b69d8ccc55f225485449f11818097",
     "parameter_unit"] <- "Percentage (%)"

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
