## Function to assign ebola outbreak based on survey start date and country

assign_ebola_outbreak <- function(df) {
  df <- df %>%
    mutate(
      outbreak = case_when(
        population_study_start_year == 1976 & population_study_end_year %in% c(2012, 2014) ~
          "Multiple outbreaks",
        population_study_start_year == 1976 & population_country ==
          "DRC" ~ "DRC, 1976",
        population_study_start_year == 1976 & population_country ==
          "DRC" ~ "DRC, 1976",
        population_study_start_year == 1976 & population_country ==
          "Sudan" ~ "South Sudan, 1976",
        population_study_start_year == 1976 & population_country ==
          "DRC,Sudan" ~ "DRC & South Sudan, 1976",
        population_study_start_year == 1979 & population_country ==
          "South Sudan" ~ "South Sudan, 1979",
        population_study_start_year == 1995 & population_country ==
          "DRC" ~ "DRC, 1995",
        population_study_start_year == 2000 & population_country ==
          "Uganda" ~ "Uganda, 2000-2001",
        population_study_start_year == 2005 & population_country ==
          "Republic of the Congo" ~ "Republic of the Congo, 2005",
        population_study_start_year == 2007 & population_country ==
          "DRC" ~ "DRC, 2007",
        population_study_start_year == 2007 & population_country ==
          "Uganda" ~ "Uganda, 2007",
        population_study_start_year == 2008 & population_country ==
          "DRC" ~ "DRC, 2008-2009",
        population_study_start_year == 2012 & population_country ==
          "DRC" ~ "DRC, 2012",
        population_study_start_year %in% c(2013, 2014, 2015) &
          population_country %in%
            c(
              "Guinea",
              "Guinea, Liberia, Sierra Leone",
              "Sierra Leone",
              "Guinea, Sierra Leone",
              "Liberia, Sierra Leone",
              "USA and Europe",
              "Multi-country (n = 12)",
              "Multi-country (n = 8)",
              "Multi-country (n = 10)",
              "Guinea, Liberia, Nigeria, Sierra Leone",
              "Liberia",
              "Liberia, Nigeria",
              "Nigeria"
            ) ~ "West Africa 2013-2016",
        population_study_start_year == 2014 & population_country ==
          "DRC" ~ "DRC, 2014",
        population_study_start_year == 2016 & population_country ==
          "Sierra Leone" ~ "West Africa 2013-2016",
        population_study_start_year == 2017 & population_country ==
          "DRC" ~ "DRC, 2017",
        # Always check these as there were separate DRC outbreaks in 2018 and 2020 too
        # 2018 - DRC - Équateur Province
        # 2018 - 2020 DRC (and Uganda) - North Kivu, Ituri and South Kivu Provinces
        # 2020 - DRC - Équateur Province
        population_study_start_year == 2018 &
          population_location %in% c(
            "Equateur Province",
            "Equateur province",
            "Équateur Province",
            "Équateur province"
          ) ~ "DRC, 2018",
        population_study_start_year == 2020 &
          population_location %in% c(
            "Equateur Province",
            "Equateur province",
            "Équateur Province",
            "Équateur province"
          ) ~ "DRC, 2020",
        population_study_start_year %in% c(2018, 2019) & population_country ==
          "DRC" ~ "DRC & Uganda, 2018-2020",
        population_study_start_year == 2020 & population_country ==
          "Uganda" ~ "DRC & Uganda, 2018-2020",
        population_study_start_year == 2021 & population_country ==
          "Guinea" ~ "Guinea, 2021",
        population_study_start_year == 2022 & population_country ==
          "Uganda" ~ "Uganda, 2022",
        TRUE ~ "Unspecified"
      )
    )

  df
}

## Order outbreak variable by when they occurred
order_ebola_outbreaks <- function(outbreak_var) {
  outbreak_var <-
    factor(outbreak_var,
      levels = c(
        "DRC, 1976",
        "South Sudan, 1976",
        "DRC & South Sudan, 1976",
        "South Sudan, 1979",
        "DRC, 1995",
        "Uganda, 2000-2001",
        "Republic of the Congo, 2005",
        "DRC, 2007",
        "Uganda, 2007",
        "DRC, 2008-2009",
        "DRC, 2012",
        "West Africa 2013-2016",
        "DRC, 2014",
        "DRC, 2017",
        "DRC, 2018",
        "DRC & Uganda, 2018-2020",
        "DRC, 2020",
        "Guinea, 2021",
        "Uganda, 2022",
        "Multiple outbreaks",
        "Unspecified"
      )
    )

  outbreak_var
}

## Function to assign ebola virus species based on outbreak (if missing)
assign_ebola_species <- function(df) {
  df <- df %>%
    mutate(
      ebola_variant = str_trim(ebola_variant, side = "right"),
      ebola_variant = case_when(is.na(ebola_variant) ~
        "Unspecified", TRUE ~ ebola_variant),
      ebola_species = case_when(
        ebola_variant == "Bundibugyo virus (BDBV)" ~ "Bundibugyo",
        ebola_variant == "Zaire Ebola virus (EBOV)" ~ "Zaire",
        ebola_variant ==
          "Bundibugyo virus (BDBV);Sudan virus (SUDV);Taï Forest virus (TAFV);Zaire Ebola virus (EBOV)" ~
          "Bundibugyo, Sudan, Taï Forest & Zaire",
        ebola_variant == "Sudan virus (SUDV);Zaire Ebola virus (EBOV)" ~ "Zaire & Sudan",
        ebola_variant == "Sudan virus (SUDV)" ~ "Sudan",
        ebola_variant == "Bundibugyo virus (BDBV);Zaire Ebola virus (EBOV)" ~
          "Zaire & Bundibugyo",
        ebola_variant == "Bundibugyo virus (BDBV);Sudan virus (SUDV)" ~
          "Bundibugyo & Sudan",
        outbreak %in% c("DRC, 1976", "DRC, 1995") ~ "Zaire",
        outbreak == "DRC & Uganda, 2018-2020" ~ "Zaire",
        ebola_variant == "Unspecified" &
          outbreak %in% c(
            "DRC, 2007",
            "DRC, 2008-2009",
            "West Africa 2013-2016",
            "DRC, 2014",
            "DRC, 2018-2020",
            "Guinea, 2021",
            "Uganda, 2022"
          ) ~ "Zaire",
        ebola_variant == "Unspecified" &
          outbreak %in% c(
            "South Sudan, 1976",
            "South Sudan, 1979",
            "Uganda, 2000-2001"
          ) ~ "Sudan",
        ebola_variant == "Unspecified" &
          outbreak == "DRC & South Sudan, 1976" ~ "Zaire & Sudan",
        ebola_variant == "Unspecified" &
          outbreak %in% c("Uganda, 2007", "DRC, 2012") ~ "Bundibugyo",
        TRUE ~ "Unspecified"
      )
    )

  df
}
