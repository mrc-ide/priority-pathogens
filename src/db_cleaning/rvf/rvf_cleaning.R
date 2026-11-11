# *=========================== Manual RVF cleaning ===========================*
library(tidyr)

article_cleaning <- function(df){
  
  # Update Not Applicable so that epireview assign_qa_score works
  qa_cols <- c("qa_m1", "qa_m2", "qa_a3", "qa_a4", "qa_d5", "qa_d6", "qa_d7")
  df <- df |>
    mutate(across(all_of(qa_cols), ~ ifelse(.=="Not Applicable", NA, .)))
  
  # Leave first_author_first_name as is incase there are initials as the first
  # name
  df <- df |>
    mutate(journal = str_to_title(journal),
           first_author_surname = str_to_title(first_author_surname)) |>
    mutate(journal = sub("^The\\s+", "", journal, useBytes = TRUE))
  
  ## 741 Author should be Woods, not Olaleye
  df <- df |> mutate(
    first_author_first_name = ifelse(covidence_id == 741 & first_author_first_name == "OD", "Christopher", first_author_first_name),
    first_author_surname = ifelse(covidence_id == 741 & first_author_surname == "Olaleye", "Woods", first_author_surname))
  
  ## 5395 Author is incorrectly input
  df <- df |> mutate(
    first_author_first_name = ifelse(covidence_id == 5395 & first_author_first_name == "Ã–zcelik", "Ranya", first_author_first_name),
    first_author_surname = ifelse(covidence_id == 5395 & first_author_surname == "R", "Özcelik", first_author_surname))
  
  ## 5642 Author is incorrectly input
  df <- df |> mutate(
    first_author_first_name = ifelse(covidence_id == 5642 & first_author_first_name == "Paweska", "JT", first_author_first_name),
    first_author_surname = ifelse(covidence_id == 5642 & first_author_surname == "Jt", "Paweska", first_author_surname))
  
  return (df)
}

model_cleaning <- function(df){
  # fix model type
  # #1669 is compartmental (and network) 
  df$model_type[which(df$covidence_id==1669)] <- "Compartmental"
  # #5101 is a catalytic model of FOI which estimates outbreaks using sero data (fitted HMC)
  df$model_type[which(df$covidence_id==5101)] <- "Catalytic"
  # #5558 is a branching proces
  df$model_type[which(df$covidence_id==5558)] <- "Branching process"
  
  ## Vector type
  # #6182 only models vector (no animal populations)
  df$model_vector_type[which(df$covidence_id==6182)] <- "Vector - mosquito"
  # #1484 has Animal as well as mosquito vectors
  df$model_vector_type[which(df$covidence_id==1484)] <- "Vector - mosquito,Animal - domestic"
  # #1616 models vector as well as animal populations
  df$model_vector_type[which(df$covidence_id==1616)] <- "Vector - mosquito,Animal - domestic"
  
  # transmission route 
  df$transmission_route[which(df$covidence_id==1616)] <- "Animal to human"
  
  # #5766 is stochastic 
  df$stoch_deter[which(df$covidence_id==5766)] <- "Stochastic model"
  
  # covidence #1470 is an animal only model
  df <- df |> filter(!(access_model_id == "480_001"))
  
  
  
  return (df)
}

outbreak_cleaning <- function(df){
  
  ## 1496 The outbreak data is for the year 2011, but there is a confusing note
  ## in the text that makes it appear that the data starts at the end of 2010.
  df <- df |> mutate(
    outbreak_start_year = ifelse(covidence_id == 1496 & outbreak_end_year == 2011, 2011, outbreak_start_year),
    outbreak_start_month = ifelse(covidence_id == 1496 & outbreak_end_year == 2011, 1, outbreak_start_month))
  
  ## 753: Egypt asymptomatic cases should be suspected
  df <- df |> mutate(
    cases_suspected = ifelse(covidence_id %in% 753 & outbreak_country == "Egypt" & outbreak_start_year == 1977 & cases_asymptomatic == 210, cases_asymptomatic, cases_suspected),
    cases_asymptomatic = ifelse(covidence_id %in% 753 & outbreak_country == "Egypt" & outbreak_start_year == 1977 & cases_asymptomatic == 210, NA, cases_asymptomatic))
  
  ## 1853: Kenya location is unspecified: National-level (unspecified)
  df <- df |> mutate(
    outbreak_location = ifelse(covidence_id %in% 1853 & outbreak_country == "Kenya", "National-level (unspecified)", outbreak_location))
  
  ## 1789: Kenya location is unspecified: National-level (unspecified)
  df <- df |> mutate(
    outbreak_location = ifelse(covidence_id %in% 1789 & outbreak_country == "Kenya", "National-level (unspecified)", outbreak_location))
  
  ## 1790: Mauritania location is various regions: National-level (various regions)
  ## 4990: Mauritania location is various regions: National-level (various regions)
  df <- df |> mutate(
    outbreak_location = ifelse(covidence_id %in% c(1790, 4990) & outbreak_country == "Mauritania", "National-level (various regions)", outbreak_location))
  
  ## 770 Mauritanis location in National-level (unspecified)
  df <- df |> mutate(
    outbreak_location = ifelse(covidence_id %in% c(770) & outbreak_country == "Mauritania", "National-level (unspecified)", outbreak_location))
  
  ## 1645 Location missing: National-level (broadly southwestern)
  df <- df |> mutate(
    outbreak_location = ifelse(covidence_id == 1645 & outbreak_country == "Saudi Arabia", "National-level (broadly southwestern)", outbreak_location))
  
  ## 1793 Location missing: National-level
  df <- df |> mutate(
    outbreak_location = ifelse(covidence_id == 1793 & outbreak_country == "United Republic of Tanzania", "National-level", outbreak_location))
  
  ## 6467 The Uganda outbreak is labelled beginning in 2013, but this should be 2023.
  df <- df |> mutate(
    outbreak_start_year = ifelse(covidence_id == 6467 & outbreak_start_year == 2013, 2023, outbreak_start_year))
  
  ## 1402 This outbreak (South Africa 1974-1975) was missed
  df <- df |> add_row(
    data.frame(article_id = 600,
               covidence_id = 1402,
               pathogen = "Rift Valley fever virus",
               name_data_entry = "Richard Sheppard",
               ongoing = "No", 
               outbreak_country = "South Africa", 
               outbreak_location = "National-level (unspecified)",
               outbreak_location_type = "National-level",
               outbreak_source = "Domestic animal", 
               cases_mode_detection = "Molecular (PCR etc)",
               cases_confirmed = 110, 
               deaths = 7, 
               outbreak_start_year = 1974, 
               outbreak_end_year = 1975,
               access_outbreak_id = "600-001",
               outbreak_id = "600-001"
    ))
  
  # 1414: Locations missing: should be National-level (unspecified)
  df <- df |> mutate(
    outbreak_location = ifelse(covidence_id == 1414 & outbreak_country == "South Africa", "National-level (unspecified)", outbreak_location))

  ## 741: deaths were missed
  df <- df |> mutate(
    deaths = ifelse(covidence_id == 741, 170, deaths)) 
  
  ## 5509: deaths were missed
  df <- df |> mutate(
    deaths = ifelse(covidence_id == 5509, 4, deaths)) 
  
  
  ## 1678 Disaggregate Eastern Africa outbreaks
  
  ## Kenya: 700 suspected cases: Starting in 26 November 2006 - 20 Feb 2007; 26 Dec 06-15 Feb 07; 5 Feb 07-30 Mar 07
  ## Somalia: 107 suspected cases: 7 Jan 2007 - 15 Feb 07
  ## Tanzania: 309 suspected cases: 27 Jan 07-24 March 07
  
  ## Total 354 deaths
  
  df <- df |> 
    mutate(cases_mode_detection = ifelse(covidence_id == 1678, NA, cases_mode_detection),
           cases_suspected = ifelse(covidence_id == 1678, NA, cases_suspected)) |> 
    bind_rows(
      tribble(
        ~"outbreak_country", ~"outbreak_location", ~"outbreak_location_type", ~"cases_suspected",
        
        "Kenya", "Garissa;Ijara;Kilifi;Malindi;Baringo", "sub-counties", 700,
        "Somalia", "Middle Juba; Lower Juba; Gedo", "sub-counties", 107,
        "United Republic of Tanzania", "Arusha;Dodoma", "sub-counties", 309,
        
      ) |> mutate(covidence_id = 1678,
                  cases_mode_detection = "Unspecified",
                  deaths = NA) |> 
        left_join(
          df |> 
            filter(covidence_id == 1678) |> 
            select(-c(outbreak_country, outbreak_location, outbreak_location_type, cases_suspected, cases_mode_detection, deaths)),
          by = "covidence_id"))
  
  
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
  
  df <- df |>
    mutate(parameter_upper_bound = ifelse(
      parameter_upper_bound=="infinity", NA, parameter_upper_bound),
      parameter_upper_bound=as.numeric(parameter_upper_bound))
  
  # Make sure all parameters have ids 
  df_id_na <- which(is.na(df$id))
  df$id[df_id_na] <-  ids::random_id(length(df_id_na)) 
  
  
  ######################################################################
  # fix parameter types 
  
  df <- df |>
    mutate(parameter_type = case_when(parameter_type=="symptom onset/fever>discharge or death" ~ "Human delay - symptom onset/fever>discharge or death",
                                      parameter_type=="Relative contribution - zoonotic to human" ~ "Relative contribution - animal to vector",
                                      .default = parameter_type))
  
  #########################################################################################
  # replace "Other" Risk factors
  # Covidence #1168 - no analysis carried out for time of sampling risk factor
  df <- df|>filter(access_param_id!="546_003")
  
  ##### Unpick "Other" risk factor specifics here #####
  
  # read in fixed version 
  risk_params_others_FIXED <- read_csv("rvf/risk_params_others_df_FIXED.csv")
  
  ## clean up risk factor names
  # replace ; with , so lists match
  risk_params_others_FIXED <- risk_params_others_FIXED |> mutate(other_names=gsub("; ", ",", other_names, fixed = TRUE))
  # combine original risk_factor names and fixed names for others
  risk_params_others_FIXED <- risk_params_others_FIXED |> mutate(riskfactor_names_all=ifelse(is.na(other_names),riskfactor_name,paste(riskfactor_name, other_names, sep=",")))
  # If other has been fixed then remove from list 
  risk_params_others_FIXED <- risk_params_others_FIXED |> mutate(riskfactor_names_all=ifelse(is.na(other_names),riskfactor_names_all,gsub("Other,", "", riskfactor_names_all, fixed = TRUE)))
  # De-duplicate list 
  risk_params_others_FIXED <- risk_params_others_FIXED |>
    mutate(
      riskfactor_names_all = sapply(riskfactor_names_all, function(x) {
        paste(unique(trimws(strsplit(x, ",")[[1]])), collapse = ",")
      })
    )
  
  risk_params_other_clean <- risk_params_others_FIXED |> mutate(riskfactor_name=riskfactor_names_all, riskfactor_names_all=NULL, other_names = NULL)
  
  df <- rbind(df|>filter(!grepl("Other",riskfactor_name)), risk_params_other_clean)
  
  
  #######################################################################
  #
  # Delay cleaning 
  #
  
  # # covidence #1539- delay extracted as uncertainty but should be variability 
  # df$parameter_uncertainty_upper_value[which(df$access_param_id=="0618_0004")] <- NA
  # df$parameter_uncertainty_lower_value[which(df$access_param_id=="0618_0005")] <- NA

  # # covidence #1539 - same delay extracted twice and mean and median 
  df <- df[-which(df$access_param_id=="0618_0005"),] 
  
  
  # covidence #2714 - extracted as parameter range (rule of 3) but should be variability range 
  df$parameter_lower_bound[which(df$covidence_id=="2714")] <- NA
  df$parameter_upper_bound[which(df$covidence_id=="2714")] <- NA
  df$parameter_2_lower_bound[which(df$covidence_id=="2714")] <- 4
  df$parameter_2_upper_bound[which(df$covidence_id=="2714")] <- 6
  
  
  #########################################################
  # Some rows have text in the population_sample_size column.
  # Move the text to the notes column and convert the column to a numeric as intended.
  # Needs to be numeric for downstream tasks.
  df <- df %>%
    mutate(
      is_numeric = is.na(population_sample_size) |
        grepl("^\\s*\\d+(\\.\\d+)?\\s*$", population_sample_size),
      parameter_notes = if_else(
        !is_numeric,
        paste0(
          coalesce(parameter_notes, ""),
          if_else(is.na(parameter_notes) | parameter_notes == "", "", " "),
          "population_sample_size: ",
          population_sample_size
        ),
        parameter_notes
      ),
      population_sample_size = if_else(
        is_numeric,
        as.numeric(population_sample_size),
        NA_real_
      )
    ) %>%
    select(-is_numeric)
  
  #######################################################################
  ### Some dates entered as "XX" or "00" instead of "x" when unknown. 
  
  df <- df %>%
    mutate(population_study_start_day=
             case_when(toupper(population_study_start_day)=="XX" ~ NA,
                       population_study_start_day=="00" ~ NA,
                       TRUE ~ population_study_start_day),
           population_study_end_day=
             case_when(toupper(population_study_end_day)=="XX" ~ NA,
                       population_study_end_day=="00" ~ NA,
                       TRUE ~ population_study_end_day),
           population_study_start_month=
             case_when(toupper(population_study_start_month)=="XX" ~ NA,
                       TRUE ~ population_study_start_month),
           population_study_end_month=
             case_when(toupper(population_study_end_month)=="XX" ~ NA,
                       TRUE ~ population_study_end_month),
           population_study_start_year=
             case_when(toupper(population_study_start_year)=="XXXX" ~ NA,
                       TRUE ~ population_study_start_year),
           population_study_end_year=
             case_when(toupper(population_study_end_year)=="XXXX" ~ NA,
                       TRUE ~ population_study_end_year)
    )
  
  
  #######################################################################
  ### Manual serology cleaning 
  
  ## 5843 population group is unspecified: 
  ## for IgG these are Samples from government health facilities (convenience sample), and should therefore be other with an explanation.
  ## for IgM these are seropositive for IgG, and should therefore be persons under investigation.
  
  ## The other population group should be other: referencing samples collected from government health facility
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 5843 & population_country == "Botswana" & parameter_type == "Seroprevalence - IgG", "Other", population_group),
    parameter_notes = ifelse(covidence_id == 5843 & population_country == "Botswana" & parameter_type == "Seroprevalence - IgG", "Samples collected from government health facilities", parameter_notes),
    population_group = ifelse(covidence_id == 5843 & population_country == "Botswana" & parameter_type == "Seroprevalence - IgM", "Persons under investigation", population_group))
  
  
  ## 1320 samples a pygmy population: this should be "other" for population group, not persons under investigation.
  ## parameter upper and lower bounds are missing.
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1320 & population_country == "Cameroon" & parameter_type == "Seroprevalence - IgG", "Other", population_group),
    parameter_lower_bound = ifelse(covidence_id == 1320 & population_country == "Cameroon" & parameter_type == "Seroprevalence - IgG", 0, parameter_lower_bound),
    parameter_upper_bound = ifelse(covidence_id == 1320 & population_country == "Cameroon" & parameter_type == "Seroprevalence - IgG", 50, parameter_upper_bound))
  
  ## 5395: These six should be aggregated into two rows, based on the rule of three:
  sero_5395_update <- df |> filter(covidence_id == 5395 & population_country == "Chad" & grepl("Seroprevalence", parameter_type)) |> 
    summarise(parameter_lower_bound = min(parameter_value), 
              parameter_upper_bound = max(parameter_value),
              .by = c(covidence_id, population_country, parameter_type, parameter_statistical_approach))
  
  df <- df |> filter(!(covidence_id == 5395 & population_country == "Chad" & grepl("Seroprevalence", parameter_type) & population_location != "Yao; Danamadji")) |> 
    rows_update(sero_5395_update, by = c("covidence_id", "population_country", "parameter_type", "parameter_statistical_approach"))
  
  
  ## 1584: A few different data sources: should be "mixed settings".
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 1584 & population_country == "Comoros" & grepl("Seroprevalence", parameter_type), "Mixed settings", population_sample_type))

  ## 331: Remove disag - no values actually given
  df <- df |> 
    mutate(method_disaggregated = ifelse(covidence_id == 331 & population_country == "Djibouti" & grepl("Seroprevalence", parameter_type), "No", method_disaggregated),
           method_disaggregated_only = ifelse(covidence_id == 331 & population_country == "Djibouti" & grepl("Seroprevalence", parameter_type), NA, method_disaggregated_only),
           method_disaggregated_by = ifelse(covidence_id == 331 & population_country == "Djibouti" & grepl("Seroprevalence", parameter_type), NA, method_disaggregated_by))
  
  
  ## 1397 Soldiers in the Israeli army: should be trade based.
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 1397 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type), "Trade / business based", population_sample_type))
  
  
  ## 138 value is /100
  df <- df |> mutate(
    parameter_value = ifelse(covidence_id == 138 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type), parameter_value * 100, parameter_value))
  
  
  ## 1617: I got the wrong parameter value here: should be simply 15%, not 15.4
  ## I also think that the 39 and 62 range are wrong - these are showing at the household level, which we do not extract.
  df <- df |> 
    mutate(parameter_value = ifelse(covidence_id == 1617 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type) & parameter_value == 15.4, 15, parameter_value),
           parameter_lower_bound = ifelse(covidence_id == 1617 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type) & parameter_lower_bound == 39, NA, parameter_lower_bound),
           parameter_upper_bound = ifelse(covidence_id == 1617 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type) & parameter_upper_bound == 62, NA, parameter_upper_bound),
           method_disaggregated = ifelse(covidence_id == 1617 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type) & method_disaggregated == "Yes", "No", method_disaggregated),
           method_disaggregated_by = ifelse(covidence_id == 1617 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type) & method_disaggregated_by == "Region", NA, method_disaggregated_by))

  ## 1464: Egypt locations: 15 governorates now given explicitly.
  df <- df |> mutate(
    population_location = ifelse(covidence_id %in% 1464 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type), "Beheira; Beni Suef; Cairo; Damietta; Daqahliya; Gharbiya; Giza; North Sinai; Ismailia; Kafr El Sheikh; Minufia; Port Said; Qalyubia; Sharqiya; South Sinai", population_location))
  
  
  ## 5469 should be general population
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 5469 & population_country == "France" & grepl("Seroprevalence", parameter_type), "General population", population_group))
  
  
  ## 5828 is a blood bank sample and should not be population based: this should be other.
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 5828 & population_country == "Germany" & grepl("Seroprevalence", parameter_type), "Other", population_sample_type))
  
  ## 5828 Guinea Location is national unspecified.
  df <- df |> mutate(
    population_location = ifelse(covidence_id == 5828 & population_country == "Guinea" & grepl("Seroprevalence", parameter_type), "National-level (unspecified)", population_location))
  
  
  ## 2199 is conducted at a health centre: this should therefore be hospital based
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 2199 & population_country == "Jordan" & grepl("Seroprevalence", parameter_type), "Hospital based", population_sample_type),
    parameter_notes = ifelse(covidence_id == 2199 & population_country == "Jordan" & grepl("Seroprevalence", parameter_type), "Outpatients at a health centre", parameter_notes))
  
  
  ## 741 Two rows incorrectly extract seroprevalence. A summary is given in the text and corrected here.
  ## We remove the IgM value and then replace IgG with the correct value, unit, numerator, denominator and sample size
  df <- df |> 
    filter(!(covidence_id == 741 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgM" & population_study_start_year == 1997)) |> 
    mutate(
      parameter_value = ifelse(covidence_id == 741 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG" & population_study_start_year == 1997, 19, parameter_value),
      parameter_unit = ifelse(covidence_id == 741 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG" & population_study_start_year == 1997, "Percentage (%)", parameter_unit),
      cfr_ifr_numerator = ifelse(covidence_id == 741 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG" & population_study_start_year == 1997, 10, cfr_ifr_numerator),
      cfr_ifr_denominator = ifelse(covidence_id == 741 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG" & population_study_start_year == 1997, 53, cfr_ifr_denominator),
      population_sample_size = ifelse(covidence_id == 741 & population_country == "Kenya" &parameter_type == "Seroprevalence - IgG" & population_study_start_year == 1997, 53, population_sample_size),
      population_sample_type = ifelse(covidence_id == 741 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG" & population_study_start_year == 1997, "Other", population_sample_type)
    )
  
  
  ## 741 The sample for the second set of data are from a non-refugee population
  ## Sampling method is not mixed for these parameters
  # The 18% 31/171: IgM +ve /denominator is IgG -ve: susceptible
  df <- df |> mutate(
    parameter_notes = ifelse(covidence_id == 741 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type) & population_group == "General population", "Non-refugee population", parameter_notes),
    parameter_notes = ifelse(covidence_id == 741 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type) & population_group == "Other", "Non-refugee population AND susceptible (IgG -ve population)", parameter_notes),
    population_sample_type = ifelse(covidence_id == 741 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type) & population_group == "General population", "Community based", population_sample_type),
    population_sample_type = ifelse(covidence_id == 741 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgM" & population_sample_type == "Mixed settings" & population_group == "Other", "Community based", population_sample_type),
    population_group = ifelse(covidence_id == 741 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type) & population_group == "General population", "Other", population_group),
  )
  
  ## 741
  # The 15% 31/202: IgG +ve AND IgM -ve: this should be sero - other
  df <- df |> 
    filter(!(covidence_id == 741 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG" & cfr_ifr_method == "Naive")) |> 
    bind_rows(df |> 
                filter(covidence_id == 741 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG" & cfr_ifr_method == "Naive") |> 
                mutate(parameter_type = "Seroprevalence - Other",
                       parameter_notes = "IgG +ve and IgM -ve as numerator"))
  
  
  ## 1742 has a PRNT sero test that is labelled as General population, but should be persons under investigation (+ve tested)
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1742 & population_country == "Kenya" & parameter_type == "Seroprevalence - PRNT", "Persons under investigation", population_group))
  
  
  ## 1853 is labelled as General population, but should be persons under investigation (+ve tested)
  ## Are we happy with this interpretation of persons under investigation?
  ## Population location should also be National-level
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1853 & population_country == "Kenya" & parameter_notes == "of the outbreak cases", "Persons under investigation", population_group),
    population_location = ifelse(covidence_id == 1853 & population_country == "Kenya" & parameter_notes == "of the outbreak cases", "National-level", population_location),
    
    ### The numerator and denominator are wrong: numerator is not given, denominator is 225
    cfr_ifr_numerator = ifelse(covidence_id == 1853 & population_country == "Kenya" & parameter_notes == "of the outbreak cases", NA, cfr_ifr_numerator),
    cfr_ifr_denominator = ifelse(covidence_id == 1853 & population_country == "Kenya" & parameter_notes == "of the outbreak cases", 225, cfr_ifr_denominator),
    
  )
  
  ## 1853 IgG value is actually IgG +ve, IgM-ve
  ## Are we happy with this interpretation of persons under investigation?
  df <- df |> mutate(
    parameter_notes = ifelse(covidence_id == 1853 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG", "IgG +ve AND IgM -ve numerator", parameter_notes),
    parameter_type = ifelse(covidence_id == 1853 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG", "Seroprevalence - Other", parameter_type)
  )
  
  
  # 1760 The Kenya 2007 sero is IgG +ve and IgM -ve, but labelled as seroprevalence - IgM. We will relabel as "Seroprevalence - Other".
  df <- df |> mutate(
    parameter_type = ifelse(covidence_id == 1760 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG", "Seroprevalence - Other", parameter_type))
  
  
  ## 1786: These three rows should be aggregated into one row, based on the rule of three:
  sero_1786_update <- df |> filter(covidence_id == 1786 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type)) |> 
    summarise(parameter_lower_bound = min(parameter_value), 
              parameter_upper_bound = max(parameter_value),
              .by = c(covidence_id, parameter_type))
  
  df <- df |> filter(!(covidence_id == 1786 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type) & population_location != "Ijara District")) |> 
    rows_update(sero_1786_update, by = c("covidence_id", "parameter_type"))
  
  
  ## 1162 has two rows that can be merged without losing any data
  sero_1162_update <- df |> filter(covidence_id == 1162 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type)) |> 
    summarise(across(everything(), ~type.convert(paste(unique(.x[!is.na(.x)]), collapse = "--"), as.is = TRUE))) |> 
    mutate(central = 15,
           parameter_value_type = "Mean",
           parameter_statistical_approach = "Estimated parameter",
           across(c(contains("year"), contains("month")), ~as.character(.x))) |> 
    select(-c(article_id))
  
  df <- df |> 
    filter(!(covidence_id == 1162 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type))) |> 
    bind_rows(sero_1162_update)
  
  ## 1162 has IgG confirmed with PRNT/population sampled, which should be "Seroprevalence - Other"
  df <- df |> mutate(
    parameter_type = ifelse(covidence_id == 1162 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG", "Seroprevalence - Other", parameter_type))
  
  
  ## 1775 has two rows that can be merged without losing any data
  df <- df |>
    filter(covidence_id == 1775 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type)) |> 
    summarise(across(everything(), ~type.convert(paste(unique(.x[!is.na(.x)]), collapse = "--"), as.is = TRUE))) |> 
    # t()
    mutate(
      parameter_statistical_approach = "Observed sample statistic",
      method_disaggregated_only = "No",
      parameter_notes = "Extracted from the text -pg 4 and range from table 2 according to rule of 3. Note that num/denom doesn't correspond with 21.2% central value given: 170/552 = 30.7%",
      central = 21.2,
      population_study_start_year = as.character(population_study_start_year),
      population_study_end_year = as.character(population_study_end_year),
      population_study_start_month = as.character(population_study_start_month),
      population_study_end_month = as.character(population_study_end_month)) |> 
    bind_rows(df |> filter(!(covidence_id == 1775 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type))))
  
  
  ## 5209 contains two rows: a test with known control samples, and then test samples from symptomatic individuals from Kenya
  ## We will remove the 96 denominator value and correct the 42/93 value to state symptomatic persons and the sampling years to be 2006-2007
  # Location should be National-level (unspecified)
  df <- df |> 
    filter(!(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type) & cfr_ifr_numerator == 96)) |> 
    mutate(
      population_sample_type = ifelse(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), "Other", population_sample_type),
      population_group = ifelse(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), "Persons with symptoms", population_group),
      population_study_start_year = ifelse(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), 2006, population_study_start_year),
      population_study_end_year = ifelse(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), 2007, population_study_end_year),
      population_location = ifelse(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), "National-level (unspecified)", population_location),
      parameter_notes = ifelse(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), "Samples provided by Kenya Medical Research Institute.", parameter_notes)
    )
  
  ## 5350 Add disag upper lower bound values for IgG
  df <- df |> mutate(
    parameter_lower_bound = ifelse(covidence_id == 5350 & population_country == "Kenya" & parameter_type %in% c("Seroprevalence - IgG"), 8.7, parameter_lower_bound),
    parameter_upper_bound = ifelse(covidence_id == 5350 & population_country == "Kenya" & parameter_type %in% c("Seroprevalence - IgG"), 60, parameter_upper_bound))
  
  ## 5350 Seroprevalence PRNT should be persons under investigation
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 5350 & population_country == "Kenya" & parameter_type %in% c("Seroprevalence - PRNT"), "Persons under investigation", population_group))

  ## 565 Add disag upper lower bound values for IgG
  df <- df |> mutate(
    parameter_lower_bound = ifelse(covidence_id == 565 & population_country == "Kenya" & parameter_type %in% c("Seroprevalence - IgG"), 3.4, parameter_lower_bound),
    parameter_upper_bound = ifelse(covidence_id == 565 & population_country == "Kenya" & parameter_type %in% c("Seroprevalence - IgG"), 35, parameter_upper_bound))
  
  
  ## 5362 has a number of households +ve / total households extracted as seroprevalence. We do not extract this. Remove from dataset.
  df <- df |> filter(!(covidence_id == 5362 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type) & population_sample_type == "Household based"))
  
  ## Remaining 5362 should be household based (household based survey) and general population.
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 5362 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), "Household based", population_sample_type),
    population_group = ifelse(covidence_id == 5362 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), "General population", population_group))
  
  ## 5130 PRNT should be persons under investigation for PRNT, not mixed groups.
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 5130 & population_country == "Kenya" & parameter_type == "Seroprevalence - PRNT", "Persons under investigation", population_group))
  
  
  ## 375 Mixed groups was selected, but the sample appears to be general
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 375 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), "General population", population_group))
  
  
  ## 982 population group should be general population
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 982 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG", "General population", population_group))
  
  
  ## 678 There are values missing in the range of central values
  ## The mixed groups here are from different regions, but there is a subset from a hospital, which has much higher values.
  ## It could be worth separating this out from the others
  df <- df |> mutate(
    parameter_lower_bound = ifelse(covidence_id == 678 & population_country == "Madagascar" & parameter_type == "Seroprevalence - IFA" & population_sample_type == "Community based", 3, parameter_lower_bound),
    parameter_upper_bound = ifelse(covidence_id == 678 & population_country == "Madagascar" & parameter_type == "Seroprevalence - IFA" & population_sample_type == "Community based", 27.7, parameter_upper_bound),
    parameter_lower_bound = ifelse(covidence_id == 678 & population_country == "Madagascar" & parameter_type == "Seroprevalence - IgM" & population_sample_type == "Community based", 1.2, parameter_lower_bound),
    parameter_upper_bound = ifelse(covidence_id == 678 & population_country == "Madagascar" & parameter_type == "Seroprevalence - IgM" & population_sample_type == "Community based", 28.5, parameter_upper_bound),
    population_sample_type = ifelse(covidence_id == 678 & population_country == "Madagascar" & population_sample_type == "Community based" , "Mixed settings", population_sample_type)
  )
  
  
  ## 1827 I can't find evidence of mixed groups. Appears to be general population
  # Location is national-level
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1827 & population_country == "Madagascar" & grepl("Seroprevalence", parameter_type), "General population", population_group),
    population_location = ifelse(covidence_id == 1827 & population_country == "Madagascar" & grepl("Seroprevalence", parameter_type), "National-level", population_location))
  
  
  # 1184 Madagascar: Location is national-level
  df <- df |> mutate(
    population_location = ifelse(covidence_id == 1184 & population_country == "Madagascar" & grepl("Seroprevalence", parameter_type), "National-level", population_location))
  
  ## 1038 The Mauritania 2012 sero data from has the incorrect first sampling year. It should be 2012, not 1012
  df <- df |> mutate(
    population_study_start_year = ifelse(covidence_id == 1038 & population_country == "Mauritania" & population_study_start_year == 1012, 2012, population_study_start_year))
  
  ## 1817 is labelled as NA for population group, but should be persons under investigation (suspected cases and contacts of confirmed cases)
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1817 & population_country == "Mauritania" & grepl("Seroprevalence", parameter_type), "Persons under investigation", population_group))
  
  
  ## 1424 start year is 1987
  df <- df |> mutate(
    population_study_start_year = ifelse(covidence_id == 1424 & population_country == "Mauritania" & grepl("Seroprevalence", parameter_type), 1987, population_study_start_year))
  
  
  ## 5492 Explicitly vets: group should be updated accordingly (Palestine)
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 5492 & is.na(population_country) & parameter_type == "Seroprevalence - Unspecified", "Animal workers incl. veterinary", population_group))
  
  
  ## 5921 country is missing: Isiolo is Kenya.
  df <- df |> mutate(
    population_country = ifelse(covidence_id == 5921 & is.na(population_country) & grepl("Seroprevalence", parameter_type), "Kenya", population_country))
  
  ## 6385 Nigeria 1983 Sero start date should be 2019 not 1019
  df <- df |> mutate(
    population_study_start_year = ifelse(covidence_id == 6385 & population_country == "Nigeria" & population_study_start_year == 1019, 2019, population_study_start_year))
  
  
  # 1781 Patients attending health facilities should be other
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1781 & population_country == "Nigeria" & grepl("Seroprevalence", parameter_type), "Other", population_group))
  
  
  ## 1713 Seroprevalence - IgG should be other: IgG +ve AND IgM -ve
  df <- df |> mutate(
    parameter_type = ifelse(covidence_id == 1713 & population_country == "Nigeria" & parameter_type == "Seroprevalence - IgG", "Seroprevalence - Other", parameter_type))
  
  
  ## 5561 Seroprevalence - Unspecified  IgG should be other: IgG +ve OR IgM +ve
  df <- df |> mutate(
    parameter_type = ifelse(covidence_id == 5561 & population_country == "Nigeria" & parameter_type == "Seroprevalence - Unspecified", "Seroprevalence - Other", parameter_type))
  
  
  ## 1429 PRNT was not captured, missing num/denom for IFA parameter.
  df <- df |> 
    mutate(cfr_ifr_numerator = ifelse(covidence_id == 1429 & population_country == "Nigeria" & grepl("Seroprevalence", parameter_type), 42, cfr_ifr_numerator),
           cfr_ifr_denominator = ifelse(covidence_id == 1429 & population_country == "Nigeria" & grepl("Seroprevalence", parameter_type), 1677, cfr_ifr_denominator),
           population_group = ifelse(covidence_id == 1429 & population_country == "Nigeria" & grepl("Seroprevalence", parameter_type), "General population", population_group)) |> 
    bind_rows(df |> filter(covidence_id == 1429 & population_country == "Nigeria" & grepl("Seroprevalence", parameter_type)) |> 
                mutate(parameter_type = "Seroprevalence - PRNT",
                       parameter_value = NA,
                       cfr_ifr_numerator = 14,
                       cfr_ifr_denominator = 42,
                       population_sample_size = 42,
                       population_group = "Persons under investigation"
                )
    )
  
  # 5009 Patients recruited at health facility should be Hospital based
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 5009 & population_country == "Rwanda" & grepl("Seroprevalence", parameter_type), "Hospital based", population_sample_type))
  
  
  ## 1412 has two rows that can be merged without losing any data
  df <- df |>
    filter(covidence_id == 1412 & population_country == "Saudi Arabia" & grepl("Seroprevalence", parameter_type) & parameter_statistical_approach == "Observed sample statistic") |> 
    summarise(across(everything(), ~type.convert(paste(unique(.x[!is.na(.x)]), collapse = "--"), as.is = TRUE))) |> 
    # t()
    mutate(across(c(contains("year"), contains("month"), contains("day"), population_study_end_day), ~as.character(.x)),
           # population_study_start_day = as.numeric(population_study_start_day),
           central = 17) |> 
    # population_study_end_day = as.c(population_study_end_day)) |> 
    # across(c(contains("day")), ~as.numeric(.x))) |> 
    bind_rows(df |> filter(!(covidence_id == 1412 & population_country == "Saudi Arabia" & grepl("Seroprevalence", parameter_type))))
  
  
  ## 1814 Seroprev Unspecified should be other: IgM +ve or Viral +ve
  df <- df |> mutate(
    parameter_type = ifelse(covidence_id == 1814 & grepl("Seroprevalence", parameter_type), "Seroprevalence - Other", parameter_type))
  
  
  # 536 Animal associated should be Animal workers incl. veterinary
  # Persons with symptoms should be persons with symptoms.
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 536 & population_country == "Saudi Arabia" & population_sample_type == "Unspecified", "Animal workers incl. veterinary", population_group),
    population_sample_type = ifelse(covidence_id == 536 & population_country == "Saudi Arabia" & population_sample_type == "Unspecified", "Trade / business based", population_sample_type),
    population_group = ifelse(covidence_id == 536 & population_country == "Saudi Arabia" & population_sample_type == "Hospital based", "Persons with symptoms", population_group))
  
  
  ## 666 Seroprevalence PRNT should be persons under investigation
  ## IgM was tested on PRNT +ve
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 666 & population_country == "Saudi Arabia" & parameter_type %in% c("Seroprevalence - IgM", "Seroprevalence - PRNT"), "Persons under investigation", population_group))
  
  
  # 5206 Sample type should be Other: from Sentinal surveillance service
  # PCR test results also recorded: we do not extract these so this row is filtered
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 5206 & population_country == "Senegal" & grepl("Seroprevalence", parameter_type), "Other", population_sample_type),
    population_location = ifelse(covidence_id == 5206 & population_country == "Senegal" & grepl("Seroprevalence", parameter_type), "All 14 regions", population_location),
    parameter_notes = ifelse(covidence_id == 5206 & population_country == "Senegal" & grepl("Seroprevalence", parameter_type), paste0(parameter_notes, "; samples from Syndromic Sentinel Surveillance System"), parameter_notes)) |> 
    filter(!(covidence_id == 5206 & population_country == "Senegal" & parameter_type == "Seroprevalence - Other"))
  
  
  # 5078 should have a location label for the national sample.
  df <- df |> mutate(
    population_location = ifelse(covidence_id == 5078 & population_country == "Senegal" & grepl("Seroprevalence", parameter_type), "National-level", population_location))
  
  
  # 5598: numerator is missing from extraction
  df <- df |> mutate(
    population_location = ifelse(covidence_id == 5598 & population_country == "Senegal" & grepl("Seroprevalence", parameter_type), "All 14 regions", population_location))

  # 5448: No disaggregation for IgM + IgG
  df <- df |> mutate(
    method_disaggregated = ifelse(covidence_id == 5448 & population_country == "Senegal" & grepl("Seroprevalence", parameter_type) & parameter_value == 21.17, "No", method_disaggregated),
    method_disaggregated_only = ifelse(covidence_id == 5448 & population_country == "Senegal" & grepl("Seroprevalence", parameter_type) & parameter_value == 21.17, NA, method_disaggregated_only),
    method_disaggregated_by = ifelse(covidence_id == 5448 & population_country == "Senegal" & grepl("Seroprevalence", parameter_type) & parameter_value == 21.17, NA, method_disaggregated_by))
  
  
  
  ## 5642: Population group should be mixed: symptomatic and controls
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 5642 & population_country == "South Africa" & grepl("Seroprevalence", parameter_type), "Mixed groups", population_group))
  
  
  # 1402: Locations missing: should be National-level (unspecified)
  # Also, the parameter bound rows can be merged together
  df <- df |> mutate(
    population_location = ifelse(covidence_id == 1402 & population_country == "South Africa" & grepl("Seroprevalence", parameter_type), "National-level (unspecified)", population_location))

  df <- bind_rows(
    
    df |>
      filter(covidence_id == 1402 & population_country == "South Africa" & grepl("Seroprevalence", parameter_type)) |> 
      summarise(across(everything(), ~type.convert(paste(unique(.x[!is.na(.x)]), collapse = "--"), as.is = TRUE))) |> 
      # t()
      mutate(across(c(contains("year"), contains("month"), contains("day")), ~as.character(.x)),
             method_disaggregated_by = "Age,Region,Sex"),
    
    df |> filter(!(covidence_id == 1402 & population_country == "South Africa" & grepl("Seroprevalence", parameter_type))))
  
  
  # 4978 End year should be 2018, not 2028
  df <- df |> mutate(
    population_study_end_year = ifelse(covidence_id == 4978 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence", parameter_type), 2018, population_study_end_year))
  
  
  # 5259 Blood donors should be Other for population group.
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 5259 & population_country == "Tunisia" & grepl("Seroprevalence", parameter_type), "Other", population_group))
  
  # 1664: Locations missing: should be 
  df <- df |> mutate(
    population_location = ifelse(covidence_id == 1664 & population_country == "Tunisia" & grepl("Seroprevalence", parameter_type), "National-level (mostly from Sousse; Sfax and Mahdia)", population_location))
  
  
  # 5974 677 sample is from urban or those dealing with animal husbandry or farm animals: this should be mixed
  # 300 sample is a specific reference group: rural areas.
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 5974 & population_country == "Türkiye" & grepl("Seroprevalence", parameter_type) & population_sample_size == 677, "Mixed groups", population_group),
    parameter_notes = ifelse(covidence_id == 5974 & population_country == "Türkiye" & grepl("Seroprevalence", parameter_type) & population_sample_size == 677, paste(parameter_notes, "; Mix of urban samples and animal husbandry/contact with farm animals"), parameter_notes),
    population_group = ifelse(covidence_id == 5974 & population_country == "Türkiye" & grepl("Seroprevalence", parameter_type) & population_sample_size == 300, "Other", population_group),
    parameter_notes = ifelse(covidence_id == 5974 & population_country == "Türkiye" & grepl("Seroprevalence", parameter_type) & population_sample_size == 300, paste(parameter_notes, "; Rural areas"), parameter_notes))
  
  
  
  # 1480 Hospital convenience sample of healthy individuals should be Other for population group.
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1480 & population_country == "Uganda" & grepl("Seroprevalence", parameter_type), "Other", population_group))

  
  # 5018 Uganda parameter upper and lower bounds are missing
  df <- df |> mutate(
    parameter_lower_bound = ifelse(covidence_id == 5018 & population_country == "Uganda" & grepl("Seroprevalence", parameter_type) & parameter_value == 10.24, 5.1, parameter_lower_bound),
    parameter_upper_bound = ifelse(covidence_id == 5018 & population_country == "Uganda" & grepl("Seroprevalence", parameter_type) & parameter_value == 10.24, 16.2, parameter_upper_bound))
  
  
  # 5015 Some seroprevalence shouldn't have been extracted because they are tied up with other RNA prevalence.
  # Only one value should remain: the one with 0%
  df <- df |> 
    filter(!(covidence_id == 5015 & population_country == "Uganda" & grepl("Seroprevalence", parameter_type) & parameter_value != 0))
  
  
  # 1619 Population sample type is hospital: outpatients is other: Febrile and decedents.
  # Also, other should be persons under investigation: those who previously tested +ve for IgG
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 1619 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence", parameter_type), "Hospital based", population_sample_type),
    population_group = ifelse(covidence_id == 1619 & population_country == "United Republic of Tanzania" & parameter_type == "Seroprevalence - IgG", "Other", population_group),
    population_group = ifelse(covidence_id == 1619 & population_country == "United Republic of Tanzania" & parameter_type == "Seroprevalence - IgM", "Persons under investigation", population_group))
  
  
  # 5686 Tanzania parameter upper and lower bounds are missing
  df <- df |> mutate(
    parameter_lower_bound = ifelse(covidence_id == 5686 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence", parameter_type) & parameter_value == 13.2, 2.9, parameter_lower_bound),
    parameter_upper_bound = ifelse(covidence_id == 5686 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence", parameter_type) & parameter_value == 13.2, 94.3, parameter_upper_bound),
    parameter_notes = ifelse(covidence_id == 5686 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence", parameter_type) & parameter_value == 13.2, paste0(parameter_notes, "; The parameter upper and lower bounds are clearly wrong (%s use the wrong denominators) but I have extracted as presented."), parameter_notes))
  
  
  
  # 4422 parameter numerators were recorded as values: no value was given. This is true for both sero extracted.
  # Location is at the national level, but unspecified within that.
  df <- df |> mutate(
    parameter_value = ifelse(covidence_id == 4422 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type), NA, parameter_value),
    population_location = ifelse(covidence_id == 4422 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type), "National-level (unspecified)", population_location))
  
  
  # 5152 Population group is other: Febrile and decedents.
  # PCR result also extracted: remove this
  # Location is also missing
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 5152 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence", parameter_type), "Hospital based", population_sample_type),
    population_group = ifelse(covidence_id == 5152 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence", parameter_type), "Other", population_group),
    population_location = ifelse(covidence_id == 5152 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence", parameter_type), "Kilimanjaro Christian Medical Centre and Mawenzi Regional Referral Hospital; Moshi; Kilimanjaro region", population_location)) |> 
    filter(!(covidence_id == 5152 & population_country == "United Republic of Tanzania" & parameter_type == "Seroprevalence - Other"))
  
  
  # 5576 Add to notes that parameter range values are not standard
  df <- df |> mutate(parameter_notes = ifelse(covidence_id == 5576 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence", parameter_type), paste0(parameter_notes, "; parameter upper and lower bounds are not standard"), parameter_notes))
  
  
  # 5698 Tanz parameter upper and lower bounds are missing
  df <- df |> mutate(
    parameter_lower_bound = ifelse(covidence_id == 5698 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence - IgG", parameter_type), 0.6, parameter_lower_bound),
    parameter_upper_bound = ifelse(covidence_id == 5698 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence - IgG", parameter_type), 2.5, parameter_upper_bound),
    parameter_lower_bound = ifelse(covidence_id == 5698 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence - IgM", parameter_type), 5.7, parameter_lower_bound),
    parameter_upper_bound = ifelse(covidence_id == 5698 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence - IgM", parameter_type), 15.3, parameter_upper_bound))
  
  
  
  ## 1157: we can merge some of these rows without losing anything important
  df <- bind_rows(
    
    df |>
      filter(covidence_id == 1157 & population_country == "South Africa" & grepl("Seroprevalence", parameter_type) & parameter_statistical_approach == "Observed sample statistic" & (parameter_value == 9.1 | parameter_lower_bound == 2.6)) |> 
      summarise(across(everything(), ~type.convert(paste(unique(.x[!is.na(.x)]), collapse = "--"), as.is = TRUE))) |> 
      # t()
      mutate(across(c(contains("year"), contains("month"), contains("day"), population_study_end_day), ~as.character(.x)),
             parameter_value_type = "Mean", 
             central = 9.1),
    
    df |>
      filter(covidence_id == 1157 & population_country == "South Africa" & grepl("Seroprevalence", parameter_type) & parameter_statistical_approach == "Observed sample statistic" & (parameter_value == 8 | parameter_lower_bound == 1)) |> 
      summarise(across(everything(), ~type.convert(paste(unique(.x[!is.na(.x)]), collapse = "--"), as.is = TRUE))) |> 
      # t()
      mutate(across(c(contains("year"), contains("month"), contains("day"), population_study_end_day), ~as.character(.x)),
             parameter_value_type = "Mean", 
             central = 8),
    
    df |> filter(!(covidence_id == 1157 & population_country == "South Africa" & grepl("Seroprevalence", parameter_type) & parameter_statistical_approach == "Observed sample statistic")))

  # 1244 Zambia parameter upper and lower bounds are missing
  df <- df |> mutate(
    parameter_lower_bound = ifelse(covidence_id == 1244 & population_country == "Zambia" & grepl("Seroprevalence", parameter_type) & parameter_value == 11.4, 0, parameter_lower_bound),
    parameter_upper_bound = ifelse(covidence_id == 1244 & population_country == "Zambia" & grepl("Seroprevalence", parameter_type) & parameter_value == 11.4, 29.4, parameter_upper_bound))
  
  
  #######################################################################
  ### Country disaggregation:
  
  # Disaggregate countries from 4695: Apply the rule of three
  df <- df |> 
    filter(!(covidence_id == 4695 & grepl("Seroprevalence", parameter_type))) |> 
    bind_rows(
      tribble(~"population_country", ~"population_location", ~"parameter_value", ~"cfr_ifr_numerator", ~"cfr_ifr_denominator", ~"population_sample_size", ~"parameter_lower_bound", ~"parameter_upper_bound",
              "Chad", "N'Djamena", 0.3, 1, 334, 334, NA, NA,
              "Cameroon", "Mora;Maroua;Nkongsamba", NA, NA, NA, NA, 0, 1.05,
              "Central African Republic", "Bangui", 0, 0, 327, 327, NA, NA,
              "Equatorial Guinea", "Bioco Island", 0, 0, 308, 308, NA, NA,
              "Equatorial Guinea", "Nsork", 0.26, 1, 380, 380, NA, NA,
              "Congo", "Pointe Noire", 0, 0, 360, 360, NA, NA,
              "Congo", "Brazzaville", 0, 0, 368, 368, NA, NA,
              "Gabon", "Port Gentil;Libreville;Makokou (Ogoue/Ivindo);Franceville (Haut Ogoue);Moula (Ngounie)", NA, NA, NA, NA, 0, 0.53) |> 
        mutate(covidence_id = 4695) |> 
        left_join(
          df |> 
            filter(covidence_id == 4695 & grepl("Seroprevalence", parameter_type)) |> 
            select(-population_country,
                   -population_location,
                   -parameter_value,
                   -cfr_ifr_numerator,
                   -cfr_ifr_denominator,
                   -population_sample_size,
                   -parameter_lower_bound,
                   -parameter_upper_bound),
          by = "covidence_id"))
  
  # Disaggregate countries from 5040:
  df <- df |> 
    filter(!(covidence_id == 5040 & grepl("Seroprevalence", parameter_type))) |> 
    bind_rows(
      tribble(~"population_country", ~"population_location", ~"parameter_value", ~"cfr_ifr_numerator", ~"cfr_ifr_denominator", ~"population_sample_size", ~"population_study_start_day", ~"population_study_start_month", ~"population_study_start_year", ~"population_study_end_day", ~"population_study_end_month", ~"population_study_end_year",
              "Democratic Republic of the Congo", "Goma (Hôpital Général de Référence de Virunga)", 1.4, 19, 1370, 1370, "27", "10", "2021", "13", "07", "2023",
              "Kenya", "Murang’a county (Kigetuini dispensary and Kandara sub-county hospital)", 2, 29, 1468, 1468, "28", "02", "2022", "02", "02", "2024",
              "Uganda", "Kabale; Rubanda and Isingiro districts (Kabale Regional Referral Hospital; Hamurwa Health Centre IV and Rwekubo Health Centre IV)", 10.4, 205, 1968, 1968, "08", "10", "2021", "14", "07", "2023") |> 
        mutate(covidence_id = 5040) |> 
        left_join(
          df |> 
            filter(covidence_id == 5040 & grepl("Seroprevalence", parameter_type)) |> 
            select(-population_country,
                   -population_location,
                   -parameter_value,
                   -cfr_ifr_numerator,
                   -cfr_ifr_denominator,
                   -population_sample_size,
                   -population_study_start_day,
                   -population_study_start_month,
                   -population_study_start_year,
                   -population_study_end_day,
                   -population_study_end_month,
                   -population_study_end_year
            ),
          by = "covidence_id"))
  
  # Disaggregate countries from 1660:
  
  ## Madagascar: 6.3%, 8/127 samples IgG: disag by ruminant exposure, age, gender, other (nomadic, behavioural traits, fever), 
  
  # Not large enough sample size
  ## Madagascar: 2/8 PRNT: sample is +ve IgG
  ## Madagascar: 1/2 IgM: sample is +ve PRNT
  
  ## Kenya (Eastern): 15.7%, 36/230 samples IgG
  ## Kenya (Eastern): 58.3% 21/36 PRNT: sample is +ve IgG: disag by ruminant exposure, age, gender, other (nomadic, behavioural traits, fever), 
  ## Kenya (Eastern): 0% 0/21 IgM: sample is +ve PRNT
  
  ## Kenya (Western): 15/200 samples IgG: disag by ruminant exposure, age, gender, other (nomadic, behavioural traits, fever), 
  ## Kenya (Western): 0% 0/15 PRNT: sample is +ve IgG
  
  df <- df |> 
    filter(!(covidence_id == 1660 & grepl("Seroprevalence", parameter_type))) |> 
    bind_rows(
      tribble(
        ~"population_country", ~"population_location", ~"parameter_context_location_type", ~"population_sample_type", ~"population_group",
        ~"parameter_value", ~"cfr_ifr_numerator", ~"cfr_ifr_denominator", ~"population_sample_size", ~"parameter_type",
        ~"method_disaggregated", ~"method_disaggregated_only", ~"method_disaggregated_by",
        ~"parameter_lower_bound", ~"parameter_upper_bound",
        
        "Madagascar", "Antsirabe; Antsohihy; Ihosy; Miandrivazo; Nosy Be; Toliara; Toliara II; Tsiroanomandidy", "districts", "Community based", "Mixed groups",
        6.3, 8, 127, 127, "Seroprevalence - IgG",
        "Yes", "No", "Age; Sex; Symptoms; Other",
        0, 14.3,
        
        "Kenya", "Gababa; Haji Mohamed; Hathama Chari; Masalani (North Eastern province)", "villages", "Community based", "Mixed groups",
        15.7, 36, 230, 230, "Seroprevalence - IgG",
        "No", NA, NA,
        NA, NA,
        
        "Kenya", "Gababa; Haji Mohamed; Hathama Chari; Masalani (North Eastern province)", "villages", "Community based", "Persons under investigation",
        58.3, 21, 36, 36, "Seroprevalence - PRNT",
        "Yes", "No", "Age; Sex; Symptoms; Other",
        3.9, 23.5,
        
        "Kenya", "Gababa; Haji Mohamed; Hathama Chari; Masalani (North Eastern province)", "villages", "Community based", "Persons under investigation",
        0, 0, 21, 21, "Seroprevalence - IgM",
        "No", NA, NA,
        NA, NA,
        
        "Kenya", "Western Province", "province", "Community based", "Mixed groups",
        NA, 15, 200, 200, "Seroprevalence - IgG",
        "Yes", "No", "Age; Sex; Symptoms; Other",
        0, 13.3,
        
        "Kenya", "Western Province", "province", "Community based", "Persons under investigation",
        0, 0, 15, 15, "Seroprevalence - PRNT",
        "No", NA, NA,
        NA, NA) |> 
        mutate(covidence_id = 1660,
               parameter_unit = "Percentage (%)",
               access_param_id = "0683_0001-0683_0002-0683_0003-0683_0004",
               parameter_data_id = "76cc6e957d99cb3fdfc75bf4757b808e-b6bc9c2514a86735fec3f643bcbca78f-683d71f5a9b2dd093cc02206deb75186-d091d776fcb3e5e1581676e259318a41") |> 
        left_join(
          df |> 
            filter(covidence_id == 1660 & grepl("Seroprevalence", parameter_type)) |> 
            select(-c(population_country, population_location, parameter_context_location_type, population_sample_type, population_group,
                      parameter_value, cfr_ifr_numerator, cfr_ifr_denominator, population_sample_size, parameter_type, parameter_unit,
                      method_disaggregated, method_disaggregated_only, method_disaggregated_by, parameter_notes,
                      access_param_id, parameter_data_id)) |> unique(),
          by = "covidence_id"))
  
  
  #######################################################################
  ### Other general cleaning.
  
  ## Some samples are less than 10!
  df <- df |> 
    filter(coalesce(cfr_ifr_denominator, population_sample_size) >= 10 | is.na(coalesce(population_sample_size, cfr_ifr_denominator)))
  
  #########################################################################################
  # CFR cleaning - gender ratio extracted in covidence 1853 instead of CFR
  df$parameter_value[which(df$covidence_id=="1853"&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- 26
  df$parameter_unit[which(df$covidence_id=="1853"&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Percentage (%)"
  df$parameter_lower_bound[which(df$covidence_id=="1853"&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- 16 # confirmed cases
  df$parameter_upper_bound[which(df$covidence_id=="1853"&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- 57 # table 1
  df$parameter_uncertainty_type[which(df$covidence_id=="1853"&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- NA
  df$parameter_uncertainty_lower_value[which(df$covidence_id=="1853"&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- NA
  df$parameter_uncertainty_upper_value[which(df$covidence_id=="1853"&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- NA
  df$cfr_ifr_numerator[which(df$covidence_id=="1853"&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- 90
  df$cfr_ifr_denominator[which(df$covidence_id=="1853"&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- 340
  # df$parameter_bounds[which(df$covidence_id=="1853"&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "20 - 57"
  # df$comb_par1_uncertainty[which(df$covidence_id=="1853"&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- NA
  # df$comb_par1_uncertainty_type[which(df$covidence_id=="1853"&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- NA
  
  ### fix typos in dates 
  df$population_study_start_year[which(df$covidence_id==1038)] <- 2012
  df$population_study_start_year[which(df$covidence_id==6385)] <- 2019
  df$population_study_end_year[which(df$covidence_id==4978)] <- 2018
  
  # covidence #1790 - all sample sizes for range extracted <10 so remove range
  df$parameter_uncertainty_type[which(df$covidence_id==1790)] <- NA # from "Range"
  df$parameter_uncertainty_lower_value[which(df$covidence_id==1790)] <- NA # from "0"
  df$parameter_uncertainty_upper_value[which(df$covidence_id==1790)] <- NA # from "100"
  
  # covidence #753 - some sample sizes for rule of 3 extracted <10 so update
  df$parameter_upper_bound[which(df$access_param_id=="489_002")] <- 20.7 # from "50"
  
  # covidence #1814 - some sample sizes for rule of 3 extracted <10 so update
  df$parameter_lower_bound[which(df$access_param_id=="665_001")] <- 10.4 # from "8.7"
  df$parameter_upper_bound[which(df$access_param_id=="665_001")] <- 26 # from "57.1"
  
  # covidence #1790 - rule of 3 not extracted 
  df$parameter_lower_bound[which(df$covidence_id==1790&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- 8.3 # from "NA"
  df$parameter_upper_bound[which(df$covidence_id==1790&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- 36.3 # from "NA"
  
  # covidence #5861 - rule of 3 extracted for <10
  df$parameter_upper_bound[which(df$covidence_id==5861&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- 33 # from "100"
  df$parameter_statistical_approach[which(df$covidence_id==5861&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Observed sample statistic" # from "unspecified"
  
  # covidence #1038 - some sample sizes for rule of 3 extracted <10 so update
  df$parameter_lower_bound[which(df$access_param_id=="560_007")] <- 25 # from "0"
  df$parameter_upper_bound[which(df$access_param_id=="560_007")] <- 41.4 # from "100"
  
  # covidence #1774 - add confirmed case CFR calculation as upper value
  df$parameter_upper_bound[which(df$covidence_id==1774)] <- 29 # from "NA"
  
  
  ##############################################################################
  ## Create delay name for "Other human delay" 
  # Update parameter_type names for "Other human delay"
  df <- mutate(df, 
               parameter_type= case_when(parameter_type=="Human delay - other human delay (go to section)" ~ 
                                           paste(tolower(other_delay_start), ">", tolower(other_delay_end), sep=""),
                                         .default=parameter_type))
  
  # Symptom Onset/Fever to Discharge or death - covidence 1767 -  "Other" population group is 
  # HIV positive individuals
  df <-mutate(df, 
              population_group=case_when((population_group=="Other")&tolower(parameter_type) %in% 
                                           c('symptom onset/fever>discharge or death','Human delay - symptom onset>admission to care') ~
                                           "Persons with confirmed HIV",
                                         .default = population_group))
  
  
  
  #########################################################################
  ## Fixing exponents in attack rate parameter
  # article 643 notes say per 100,000 people so needs -5 exponent - attack rate
  df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- -5
  df$parameter_value[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- df$parameter_value[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] *10^(df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))])*100
  df$parameter_lower_bound[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- df$parameter_lower_bound[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] *10^(df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))])*100
  df$parameter_upper_bound[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- df$parameter_upper_bound[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] *10^(df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))])*100
  df$parameter_uncertainty_lower_value[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- df$parameter_uncertainty_lower_value[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] *10^(df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))])*100
  df$parameter_uncertainty_upper_value[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- df$parameter_uncertainty_upper_value[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] *10^(df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))])*100
  df$exponent[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- 0
  df$parameter_unit[which((df$article_id==643)&(df$parameter_type=="Attack rate"))] <- "Percentage (%)"
  
  # Calculate central value from CFR/IFR/Attack rate nom and denom if parameter value is NA
  df <- mutate(df,  parameter_value = case_when(is.na(parameter_value)&!is.na(cfr_ifr_numerator)&!is.na(cfr_ifr_denominator) ~ (cfr_ifr_numerator/cfr_ifr_denominator)*100,
                                                .default = parameter_value), 
               parameter_unit = case_when(is.na(parameter_value)&!is.na(cfr_ifr_numerator)&!is.na(cfr_ifr_denominator) ~ "Percentage (%)",
                                          .default = parameter_unit), 
               parameter_statistical_approach = case_when(is.na(parameter_statistical_approach) ~ "Unspecified",
                                                          .default = parameter_statistical_approach))
  
  
  # covidence #1038 - also extract village level attack rates 
  # Letfatar village
  attack_rate_letfatar <- df|>filter(access_param_id=="560_001")
  # attack_rate_letfatar$id <- 
  # attack_rate_letfatar$parameter_data_id <- 
  attack_rate_letfatar$access_param_id <- "560_0001"
  attack_rate_letfatar$parameter_value <- 0.26
  attack_rate_letfatar$exponent <- 0
  attack_rate_letfatar$parameter_lower_bound <- NA
  attack_rate_letfatar$parameter_upper_bound <- NA
  attack_rate_letfatar$population_location <- "Letfatar"
  attack_rate_letfatar$parameter_context_location_type <- "Village"
  attack_rate_letfatar$method_disaggregated <- "No"
  attack_rate_letfatar$method_disaggregated_by <- NA
  attack_rate_letfatar$id <- ids::random_id(1) #  new random id
  
  # Marvek Village
  attack_rate_marvek <- df|>filter(access_param_id=="560_001")
  # attack_rate_marvek$id <- 
  # attack_rate_marvek$parameter_data_id <- 
  attack_rate_marvek$access_param_id <- "560_0002"
  attack_rate_marvek$parameter_value <- 0.20
  attack_rate_marvek$exponent <- 0
  attack_rate_marvek$parameter_lower_bound <- NA
  attack_rate_marvek$parameter_upper_bound <- NA
  attack_rate_marvek$population_location <- "Marvek"
  attack_rate_marvek$parameter_context_location_type <- "Village"
  attack_rate_marvek$method_disaggregated <- "No"
  attack_rate_marvek$method_disaggregated_by <- NA
  attack_rate_marvek$id <- ids::random_id(1)  # new random id
  
  # add to parameter data frame:
  df <- rbind(df, attack_rate_letfatar, attack_rate_marvek)
  
  #########################################################################################
  # CFR cleaning - population group & sample type
  
  # Population Group
  # covidence 1853
  df$population_group[which(df$covidence_id==1853&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Persons with symptoms" # from "General Population"
  # covidence 1701
  df$population_group[which(df$covidence_id==1701&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Persons with symptoms" # from "Persons under investigation"
  # covidence 5272
  df$population_group[which(df$covidence_id==5272&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Persons with symptoms" # from "Persons under investigation"
  # covidence 1496
  df$population_group[which(df$covidence_id==1496&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Persons with symptoms" # from "Persons under investigation"
  # covidence 1774
  df$population_group[which(df$covidence_id==1774)] <- "Persons with symptoms" # from "Persons under investigation"
  # covidence 1814
  df$population_group[which(df$covidence_id==1814&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Persons with symptoms" # from "Persons under investigation"
  # covidence 1790
  df$population_group[which(df$covidence_id==1790&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Persons with symptoms" # from "Persons under investigation"
  # covidence 5255
  df$population_group[which(df$covidence_id==5255&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Persons with symptoms" # from "General population"
  # covidence 3061
  df$population_group[which(df$covidence_id==3061&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Persons with symptoms" # from "Unspecified"
  # covidence 1760
  df$population_group[which(df$covidence_id==1760&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Persons under investigation" # from "General population"
  # covidence 1817
  df$population_group[which(df$covidence_id==1817&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Persons under investigation" # from "Unspecified"
  
  
  # Population Sample Type
  # covidence 5127
  df$population_sample_type[which(df$covidence_id==5127&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Population based" # from "Unspecified"
  # covidence 5509
  df$population_sample_type[which(df$covidence_id==5509&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Population based" # from "Community based"
  # covidence 3061
  df$population_sample_type[which(df$covidence_id==3061&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Population based" # from "NA"
  # covidence 1817
  df$population_sample_type[which(df$covidence_id==1817&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Mixed settings" # from "Hospital based"
  # covidence 1774
  df$population_sample_type[which(df$covidence_id==1774&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Population based" # from "Hospital based"
  # covidence 5434
  df$population_sample_type[which(df$covidence_id==5434&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Hospital based" # from "Population based"
  # covidence 5255
  df$population_sample_type[which(df$covidence_id==5255&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Mixed settings" # from "Population based"
  # covidence 5861
  df$population_sample_type[which(df$covidence_id==5861&df$parameter_type=="Severity - case fatality ratio (CFR)")] <- "Mixed settings" # from "Population based"
  
  #########################################################################################
  # Risk factors 
  #covidence #6042 population_group and popluation_sample_type is NA but should be "Mixed groups" & "Population based"
  df$population_sample_type[which(df$covidence_id=="6042")] <- "Population based"
  df$population_group[which(df$covidence_id=="6042")] <- "Mixed groups"
  
  # covidence #10 - only district of residence is significant 
  df$riskfactor_name[which(df$access_param_id=="558_005")] <- "Location (geographic)" # from "Education,Roofing material"
  
  # covidence #1742 - population group is seropositives in village
  df$population_group[which(df$covidence_id==1742&df$parameter_context_location_type=="village")] <- "Persons under investigation" # from "General population"
  
  # covidence #741 - population group is Other: non-refugees 
  df$population_group[which((df$covidence_id=="741")&(df$parameter_type=="Risk factors"))] <- "Other" # from NA
  # covidence #741 - population sample size 202
  df$population_sample_size[which(df$access_param_id=="514_009")] <- 202 # from NA
  
  # covidence #1619 population group is outpatients which is other & hospital based
  df$population_group[which((df$covidence_id=="1619")&(df$parameter_type=="Risk factors"))] <- "Other" # from Mixed groups
  df$population_sample_type[which((df$covidence_id=="1619")&(df$parameter_type=="Risk factors"))] <- "Hospital based" # from Mixed settings
  
  # match covidence #1827 to sero population group - appears to be general population
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1827 & population_country == "Madagascar" & grepl("Risk factors", parameter_type), "General population", population_group))
  
  # covidence #1742 should be General population for all risk factors
  df$population_group[which(df$covidence_id=="1742")] <- "General population" #from "Persons under investigation"
  
  # covidence #5429 - population sample size 916
  # df$population_sample_size[which(df$access_param_id=="630_007")] <- 916 # from NA
  # df$riskfactor_name[which(df$access_param_id=="630_007")] <- "Consumption of animal product,Water source" # from NA
  # 
  # ## for covidence 5429 - add Ukundu risk factors
  # riskfactors_5429_ukundu <- df|>filter(access_param_id=="630_007")
  # riskfactors_5429_ukundu$access_param_id <- "630_0001" 
  # riskfactors_5429_ukundu$population_sample_size <-  1254
  # riskfactors_5429_ukundu$population_location <- "Ukunda" 
  # riskfactors_5429_ukundu$riskfactor_name <- "Contact with animal,Sex" 
  # df <- rbind(df,riskfactors_5429_ukundu)
  # 
  # covidence 5429 - only keep risk factors for all regions
  df <- df[-which(df$population_location=="Kisumu"),]
  
  #########################################################################################
  # Risk factor/sero cleaning - match population group
  
  # Population Group
  # covidence 1157 - define population groups 
  # risk factors
  df$population_group[which(df$access_param_id=="561_003")] <- "Farmers" # from "Mixed groups"
  df$population_group[which(df$access_param_id=="561_004")] <- "Farmers" # from "Mixed groups"
  df$population_group[which(df$access_param_id=="561_005")] <- "Animal workers incl. veterinary" # from "Mixed groups"
  df$population_group[which(df$access_param_id=="561_006")] <- "Animal workers incl. veterinary" # from "Mixed groups"
  # sero
  df$population_group[which(df$access_param_id=="529_001--529_006")] <- "Farmers" # from "Mixed groups"
  df$population_group[which(df$access_param_id=="529_002")] <- "Farmers" # from "Mixed groups"
  df$population_group[which(df$access_param_id=="529_003--529_006")] <- "Animal workers incl. veterinary" # from "Mixed groups"
  df$population_group[which(df$access_param_id=="529_004")] <- "Animal workers incl. veterinary" # from "Mixed groups"
  
  
  
  
  #########################################################################################
  # change all Camel contact risk factors to animal contact
  
  df$riskfactor_name[which(df$access_param_id=="0598_0003")] <- "Age,Contact with animal,Other,Location (geographic)"
  df$riskfactor_name[which(df$access_param_id=="0717_0002")] <- "Sex,Contact with animal,Consumption of animal product"
  df$riskfactor_name[which(df$access_param_id=="514_006")] <- "Close contact,Contact with animal,Sex,Environmental,Other"
  
  
  
  
  #########################################################################################
  # Delay cleaning - population group & sample type
  
  # covidence 1402
  # Population Group
  df$population_group[which((df$covidence_id=="1402")&(df$parameter_type=="Human delay - incubation period"))] <- "Persons under investigation" # from "NA"
  # Population Sample Type
  df$population_sample_type[which((df$covidence_id=="1402")&(df$parameter_type=="Human delay - incubation period"))] <- "Unspecified" # from "NA"
  
  # covidence 2714
  # Population Group
  df$population_group[which((df$covidence_id=="2714")&(df$parameter_type=="Human delay - incubation period"))] <- "Persons under investigation" # from "Other"
  # Population Sample Type
  df$population_sample_type[which((df$covidence_id=="2714")&(df$parameter_type=="Human delay - incubation period"))] <- "Community based" # from "Other"
  
  
  
  ##################################################################################################
  ## Genomic cleaning 
  ### Correct covidence #5293 - mutation rate
  all_genomic_data_wang_corrected <- read_csv("rvf/df_wang_corrected.csv")
  all_genomic_data_wang_corrected[all_genomic_data_wang_corrected=="NA"] <- NA
  all_genomic_data_wang_corrected <- all_genomic_data_wang_corrected |> mutate(
    parameter_2_value = suppressWarnings(as.numeric(parameter_2_value)), 
    parameter_2_lower_bound = suppressWarnings(as.numeric(parameter_2_lower_bound)),
    parameter_2_upper_bound = suppressWarnings(as.numeric(parameter_2_upper_bound)),
    exponent_2 = suppressWarnings(as.numeric(exponent_2)),
    cfr_ifr_numerator = suppressWarnings(as.numeric(cfr_ifr_numerator)),
    cfr_ifr_denominator = suppressWarnings(as.numeric(cfr_ifr_denominator)) ,
    population_age_min = suppressWarnings(as.numeric(population_age_min)),
    population_age_max = suppressWarnings(as.numeric(population_age_max)),
    population_study_start_day = suppressWarnings(as.numeric(population_study_start_day)) ,
    population_study_end_day = suppressWarnings(as.character(population_study_end_day)) ,
    population_study_start_year = suppressWarnings(as.numeric(population_study_start_year)) ,
    population_study_end_year = suppressWarnings(as.numeric(population_study_end_year)),
    population_study_start_month = suppressWarnings(as.character(population_study_start_month)) ,
    population_study_end_month = suppressWarnings(as.character(population_study_end_month)),
    central = suppressWarnings(as.numeric(central)) # suppress warnings for coercion
  )
  
  all_genomic_data_wang_corrected$id[which(all_genomic_data_wang_corrected$access_param_id=="632_001")] <- ids::random_id(1) # new random id
  all_genomic_data_wang_corrected$id[which(all_genomic_data_wang_corrected$access_param_id=="632_002")] <- ids::random_id(1) # new random id
  all_genomic_data_wang_corrected$id[which(all_genomic_data_wang_corrected$access_param_id=="632_003")] <- ids::random_id(1) # new random id
  
  df <- rbind(df |> filter(covidence_id!="5293"), all_genomic_data_wang_corrected)
  
  return (df)
}
# *============================================================================*