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
  
  ## 5395 Author is incorrectly input
  df <- df |> mutate(
    first_author_first_name = ifelse(covidence_id == 5395 & first_author_first_name == "Ã–zcelik", "Ranya", first_author_first_name),
    first_author_surname = ifelse(covidence_id == 5395 & first_author_surname == "R", "Özcelik", first_author_surname))
  
  return (df)
}

model_cleaning <- function(df){
  # fix model type
  df$model_type[which(df$covidence_id==1669)] <- "Compartmental"
  
  return (df)
}

outbreak_cleaning <- function(df){
  
  ## 1496 The outbreak data is for the year 2011, but there is a confusing note
  ## in the text that makes it appear that the data starts at the end of 2010.
  df <- df |> mutate(
    outbreak_start_year = ifelse(covidence_id == 1496 & outbreak_end_year == 2011, 2011, outbreak_start_year),
    outbreak_start_month = ifelse(covidence_id == 1496 & outbreak_end_year == 2011, 1, outbreak_start_month))
  
  ## 6467 The Uganda outbreak is labelled beginning in 2013, but this should be 2023.
  df <- df |> mutate(
    outbreak_start_year = ifelse(covidence_id == 6467 & outbreak_start_year == 2013, 2023, outbreak_start_year))
  
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
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1320 & population_country == "Cameroon" & parameter_type == "Seroprevalence - IgG", "Other", population_group))
  
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
  
  
  ## 1397 Soldiers in the Israeli army: should be trade based.
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 1397 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type), "Trade / business based", population_sample_type))
  
  
  ## 1617: I got the wrong parameter value here: should be simply 15%, not 15.4
  ## I also think that the 39 and 62 range are wrong - these are showing at the household level, which we do not extract.
  df <- df |> 
    mutate(parameter_value = ifelse(covidence_id == 1617 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type) & parameter_value == 15.4, 15, parameter_value),
           parameter_lower_bound = ifelse(covidence_id == 1617 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type) & parameter_lower_bound == 39, NA, parameter_lower_bound),
           parameter_upper_bound = ifelse(covidence_id == 1617 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type) & parameter_upper_bound == 62, NA, parameter_upper_bound),
           method_disaggregated = ifelse(covidence_id == 1617 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type) & method_disaggregated == "No", NA, method_disaggregated),
           method_disaggregated_by = ifelse(covidence_id == 1617 & population_country == "Egypt" & grepl("Seroprevalence", parameter_type) & method_disaggregated_by == "Age", NA, method_disaggregated_by))
  
  
  ## 5828 is a blood bank sample and should not be population based: this should be other.
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 5828 & population_country == "Germany" & grepl("Seroprevalence", parameter_type), "Other", population_sample_type))
  
  
  ## 2199 is conducted at a health centre: this should therefore be hospital based
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 2199 & population_country == "Jordan" & grepl("Seroprevalence", parameter_type), "Hospital based", population_sample_type),
    parameter_notes = ifelse(covidence_id == 2199 & population_country == "Jordan" & grepl("Seroprevalence", parameter_type), "Outpatients at a health centre", parameter_notes))
  
  
  ## 741 Two rows incorrectly extract seroprevalence. A summary is given in the text and corrected here.
  ## We remove the IgM value and then replace IgG with the correct value, unit, numerator, denominator and sample size
  df <- df |> 
    filter(!(covidence_id == 741 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgM" & population_study_start_year == 1997)) |> 
    mutate(
      parameter_value = ifelse(covidence_id == 741 & population_country == "Kenya" & parameter_type == "Seroprevalence - IgG" & population_study_start_year == 1997, 19.3, parameter_value),
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
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1853 & population_country == "Kenya" & parameter_notes == "of the outbreak cases", "Persons under investigation", population_group))
  
  
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
  df <- df |> 
    filter(!(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type) & cfr_ifr_numerator == 96)) |> 
    mutate(
      population_sample_type = ifelse(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), "Other", population_sample_type),
      population_group = ifelse(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), "Persons with symptoms", population_group),
      population_study_start_year = ifelse(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), 2006, population_study_start_year),
      population_study_end_year = ifelse(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), 2007, population_study_end_year),
      parameter_notes = ifelse(covidence_id == 5209 & population_country == "Kenya" & grepl("Seroprevalence", parameter_type), "Samples provided by Kenya Medical Research Institute.", parameter_notes)
    )
  
  ## 5350 Seroprevalence PRNT should be persons under investigation
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 5350 & population_country == "Kenya" & parameter_type %in% c("Seroprevalence - PRNT"), "Persons under investigation", population_group))
  
  
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
  
  
  ## 1660 PRNT should be persons under investigation
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1660 & population_country == "Kenya; Madagascar" & parameter_type == "Seroprevalence - PRNT", "Persons under investigation", population_group))
  
  ## 1660 IgM should be other: IgG -ve, PRNT +ve samples + animal workers
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1660 & population_country == "Kenya; Madagascar" & parameter_type == "Seroprevalence - IgM", "Other", population_group))
  
  
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
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1827 & population_country == "Madagascar" & grepl("Seroprevalence", parameter_type), "General population", population_group))
  
  
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
           cfr_ifr_denominator = ifelse(covidence_id == 1429 & population_country == "Nigeria" & grepl("Seroprevalence", parameter_type), 1677, cfr_ifr_denominator)) |> 
    bind_rows(df |> filter(covidence_id == 1429 & population_country == "Nigeria" & grepl("Seroprevalence", parameter_type)) |> 
                mutate(parameter_type = "Seroprevalence - PRNT",
                       population_group = "Persons under investigation",
                       cfr_ifr_numerator = 14,
                       cfr_ifr_denominator = 42,
                       population_sample_size = 42
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
    parameter_notes = ifelse(covidence_id == 5206 & population_country == "Senegal" & grepl("Seroprevalence", parameter_type), paste0(parameter_notes, "; samples from Syndromic Sentinel Surveillance System"), parameter_notes)) |> 
    filter(!(covidence_id == 5206 & population_country == "Senegal" & parameter_type == "Seroprevalence - Other"))
  
  # 5259 Blood donors should be Other for population group.
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 5259 & population_country == "Tunisia" & grepl("Seroprevalence", parameter_type), "Other", population_group))
  
  # 1480 Hospital convenience sample of healthy individuals should be Other for population group.
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 1480 & population_country == "Uganda" & grepl("Seroprevalence", parameter_type), "Other", population_group))
  
  
  # 1619 Population sample type is hospital: outpatients is other: Febrile and decedents.
  # Also, other should be persons under investigation: those who previously tested +ve for IgG
  df <- df |> mutate(
    population_sample_type = ifelse(covidence_id == 1619 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence", parameter_type), "Hospital based", population_sample_type),
    population_group = ifelse(covidence_id == 1619 & population_country == "United Republic of Tanzania" & parameter_type == "Seroprevalence - IgG", "Other", population_group),
    population_group = ifelse(covidence_id == 1619 & population_country == "United Republic of Tanzania" & parameter_type == "Seroprevalence - IgM", "Persons under investigation", population_group))
  
  
  # 4422 parameter numerator was recorded as value: no value was given
  df <- df |> mutate(
    parameter_value = ifelse(covidence_id == 4422 & population_country == "Egypt" & parameter_type == "Seroprevalence - Other", NA, parameter_value))
  
  
  # 5152 Population group is other: Febrile and decedents.
  # PCR result also extracted: remove this
  df <- df |> mutate(
    population_group = ifelse(covidence_id == 5152 & population_country == "United Republic of Tanzania" & grepl("Seroprevalence", parameter_type), "Other", population_group)) |> 
    filter(!(covidence_id == 5152 & population_country == "United Republic of Tanzania" & parameter_type == "Seroprevalence - Other"))
  
  
  
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
  
  
  
  #######################################################################
  ### Other general cleaning.
  
  ## Some samples are less than 10!
  df <- df |> 
    filter(coalesce(cfr_ifr_denominator, population_sample_size) >= 10 | is.na(coalesce(population_sample_size, cfr_ifr_denominator)))
  
  
  return (df)
}
# *============================================================================*