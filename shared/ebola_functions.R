## Function to assign ebola outbreak based on survey start date and country

assign_ebola_outbreak <- function(df) {
  df <- df %>%
    mutate(
      outbreak = case_when(
        population_study_start_year %in% 1976 & population_study_end_year %in% c(2012, 2014) ~
          "Multiple outbreaks",
        population_study_start_year %in% 2000 & population_study_end_year %in% 2015 ~
          "Multiple outbreaks",
        population_study_start_year %in% 1976 & population_country %in%
          "DRC" ~ "DRC, 1976",
        population_study_start_year %in% 1976 & population_country %in%
          "Sudan" ~ "South Sudan, 1976",
        population_study_start_year %in% 1976 & population_country %in%
          "DRC, Sudan" ~ "DRC & South Sudan, 1976",
        population_study_start_year %in% 1979 & population_country %in%
          "South Sudan" ~ "South Sudan, 1979",
        # Need to mention DRC 1981-1985 not official outbreak in figure legend (*)
        population_study_start_year %in% 1981 & population_country %in%
          "DRC" ~ "DRC 1981-1985*",
        population_study_start_year %in% 1994 & population_country %in%
          "Gabon" ~ "Gabon, 1994",
        population_study_start_year %in% 1995 & population_country %in%
          "DRC" ~ "DRC, 1995",
        population_study_start_year %in% 1995 &
          population_study_end_year %in% 2016 ~ "Multiple outbreaks",
        population_study_start_year %in% 1996 & population_country %in%
          "Gabon" ~ "Gabon, 1996",
        population_study_start_year %in% 2000 & population_country %in%
          "Uganda" ~ "Uganda, 2000-2001",
        population_study_start_year %in% 2005 & population_country %in%
          "Republic of the Congo" ~ "Republic of the Congo, 2005",
        population_study_start_year %in% 2007 & population_country %in%
          "DRC" ~ "DRC, 2007",
        population_study_start_year %in% 2007 & population_country %in%
          "Uganda" ~ "Uganda, 2007",
        population_study_start_year %in% 2008 & population_country %in%
          "DRC" ~ "DRC, 2008-2009",
        population_study_start_year %in% 2012 & population_country %in%
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
              "Multi-country: Africa, Europe, USA (n = 6)",
              "Multi-country: Africa, Europe, USA (n = 8)",
              "Multi-country: Europe & USA (n = 9)",
              "Multi-country: Africa, Europe, USA (n = 10)",
              "Multi-country: Africa, Europe, USA (n = 12)",
              "Guinea, Liberia, Nigeria, Sierra Leone",
              "Liberia",
              "Liberia, Nigeria",
              "Nigeria",
              "Mali"
            ) ~ "West Africa 2013-2016",
        # No specific dates/start dates provided for these, but checked the papers
        covidence_id %in% c(590, 11565, 4568, 11620, 5997) ~
          "West Africa 2013-2016",
        # Seroprevalence survey post WA outbreak also used to calculate CFR
        covidence_id %in% 1044 ~ "West Africa 2013-2016",
        population_study_start_year %in% 2014 & population_country %in%
          "DRC" ~ "DRC, 2014",
        population_study_start_year %in% 2016 & population_country %in%
          "Sierra Leone" ~ "West Africa 2013-2016",
        population_study_start_year %in% 2016 & population_country %in%
          "Guinea" ~ "West Africa 2013-2016",
        population_study_start_year %in% 2017 & population_country %in%
          "DRC" ~ "DRC, 2017",
        # post-outbreak seroprevalence
        population_study_start_year %in% 2017 &
          population_country %in% "Sierra Leone" &
          parameter_class %in% "Seroprevalence" ~ "West Africa 2013-2016",
        # Always check these as there were separate DRC outbreaks in 2018 and 2020 too
        # 2018 - DRC - Équateur Province
        # 2018 - 2020 DRC (and Uganda) - North Kivu, Ituri and South Kivu Provinces
        # 2020 - DRC - Équateur Province
        population_study_start_year %in% 2018 &
          population_location %in% c(
            "Equateur Province",
            "Equateur province",
            "Équateur Province",
            "Équateur province"
          ) ~ "DRC, 2018",
        population_study_start_year %in% 2020 &
          population_location %in% c(
            "Equateur Province",
            "Equateur province",
            "Équateur Province",
            "Équateur province"
          ) ~ "DRC, 2020",
        population_study_start_year %in% c(2018, 2019) & population_country %in%
          "DRC" ~ "DRC, 2018-2020",
        population_study_start_year %in% 2020 & population_country %in%
          "Uganda" ~ "DRC, 2018-2020",
        population_study_start_year %in% 2021 & population_country %in%
          "Guinea" ~ "Guinea, 2021",
        population_study_start_year %in% 2022 & population_country %in%
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
        "DRC 1981-1985*",
        "Gabon, 1994",
        "DRC, 1995",
        "Gabon, 1996",
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
        "DRC, 2018-2020",
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
        ebola_variant %in% "Bundibugyo virus (BDBV)" ~ "Bundibugyo",
        ebola_variant %in% "Zaire Ebola virus (EBOV)" ~ "Zaire",
        ebola_variant %in%
          "Bundibugyo virus (BDBV);Sudan virus (SUDV);Taï Forest virus (TAFV);Zaire Ebola virus (EBOV)" ~
          "Bundibugyo, Sudan, Taï Forest & Zaire",
        ebola_variant %in% "Sudan virus (SUDV);Zaire Ebola virus (EBOV)" ~ "Zaire & Sudan",
        ebola_variant %in% "Sudan virus (SUDV)" ~ "Sudan",
        ebola_variant %in% "Bundibugyo virus (BDBV);Zaire Ebola virus (EBOV)" ~
          "Zaire & Bundibugyo",
        ebola_variant %in% "Bundibugyo virus (BDBV);Sudan virus (SUDV)" ~
          "Bundibugyo & Sudan",
        outbreak %in% c("DRC, 1976", "DRC, 1995") ~ "Zaire",
        outbreak %in% "DRC, 2018-2020" ~ "Zaire",
        ebola_variant %in% "Unspecified" &
          outbreak %in% c(
            "DRC, 2007",
            "DRC, 2008-2009",
            "West Africa 2013-2016",
            "DRC, 2014",
            "DRC, 2018-2020",
            "Guinea, 2021",
            "Uganda, 2022"
          ) ~ "Zaire",
        ebola_variant %in% "Unspecified" &
          outbreak %in% c(
            "South Sudan, 1976",
            "South Sudan, 1979",
            "Uganda, 2000-2001"
          ) ~ "Sudan",
        ebola_variant %in% "Unspecified" &
          outbreak %in% "DRC & South Sudan, 1976" ~ "Zaire & Sudan",
        ebola_variant %in% "Unspecified" &
          outbreak %in% c("Uganda, 2007", "DRC, 2012") ~ "Bundibugyo",
        TRUE ~ "Unspecified"
      )
    )

  df
}
