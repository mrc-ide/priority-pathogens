# *=================== MERS severity meta-analysis & plots ===================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(grid)
library(metafor)
library(meta)
library(orderly)
library(patchwork)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)

# *--------------------------------- Orderly ----------------------------------*
orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "params.csv"))

orderly_shared_resource("mers_functions.R" = "mers_functions.R")
#orderly_shared_resource("NIPAH_Bangladesh_IEDCR.csv"="NIPAH_Bangladesh_IEDCR.csv")
source("mers_functions.R")

# orderly_artefact("MERS severity figures",
#                  c(file.path("figures", "figure_severity.png"),
#                    file.path("figures","figure_severity.pdf")))
# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- tibble()
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles,outbreaks,models,parameters, plotting = FALSE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

# Note, for MERS we have an issue of different parameter type names:
unique(filter(parameters, parameter_class == "Severity")$parameter_type)
# So CF ratio AND CF rate
#"Severity - symptomatic proportion of infections"
#"Severity - asymptomatic proportion of infections"
#"Severity - proportion of asymptomatic cases"
#"Severity - proportion of symptomatic cases"
table(filter(parameters, parameter_class == "Severity")$parameter_type)

#Filter out low QA now
parameters <- filter(parameters, qa_score >= 0.5)

#For now, map ratio to rate and symptomatics together
#Stick asymptomatics together too
parameters$parameter_type <- gsub("case fatality ratio",
                    "case fatality rate",
                    parameters$parameter_type)

parameters$parameter_type <- gsub(" symptomatic proportion of infections",
                    " proportion of symptomatic cases",
                    parameters$parameter_type)

parameters$parameter_type <- gsub("asymptomatic proportion of infections",
                                  "proportion of asymptomatic cases",
                                  parameters$parameter_type)
# *----------------------------- Data preparation -----------------------------*
# Extracted CFRs parameters
d1 <- parameters |>
  filter(parameter_type == 'Severity - case fatality rate (CFR)')
# 3 rows have NA for population_group
# This causes plotting issues, so change the NA to "Unspecified" for now
d1 <- d1 |>
  mutate(population_group = ifelse(is.na(population_group), "Unspecified", population_group))

# Immediately remove the low-QA studies
d1 <- filter(d1, qa_score >= 0.5)

# We have 8 parameters with NA for unit
# d1_no_unit <- d1 |>
#   filter(is.na(parameter_unit))
# We have numerator and denominator for all, so let's include those below

# Create subgroups here
# Lassa examples for study midyear, population_group, cfr_denom_cat
d1 <- d1 |>
  # mutate(population_group = case_when(
  #   population_group == "Persons under investigation" ~ "Persons Under Investigation",
  #   population_group == "Persons with symptoms" ~ "Persons with symptoms",
  #   population_group == "Healthcare workers" ~ "Healthcare workers",
  #   population_group == "Abattoir workers" ~ "Animal workers",
  #   population_group == "Animal workers" ~ "Animal workers",
  #   is.na(population_group) ~ "Other",
  #   TRUE ~ "Mixed Groups")) |>
  mutate(parameter_unit = 'Percentage (%)',
         parameter_value = coalesce(parameter_value, central), #using central where no % was reported
         population_study_start_year = as.numeric(population_study_start_year),
         population_study_end_year = as.numeric(population_study_end_year),
         study_midyear = ifelse(!is.na(population_study_start_year) & !is.na(population_study_end_year),
                                round((population_study_start_year + population_study_end_year) / 2),
                                population_study_start_year)) |>
  mutate(study_midyear_cat = case_when(
    study_midyear %in% 1990:1999 ~ "1990-1999",
    study_midyear %in% 2000:2009 ~ "2000-2009",
    study_midyear %in% 2010:2019 ~ "2010-2019",
    study_midyear %in% 2020:2029 ~ "2020-Present",
    TRUE ~ "Unspecified")) |>
  mutate(cfr_denom_cat = case_when(
    cfr_ifr_denominator %in% 1:29      ~ "Reported Cases < 30",
    cfr_ifr_denominator %in% 30:99     ~ "Reported Cases = 30-99",
    cfr_ifr_denominator %in% 100:329   ~ "Reported Cases = 100-329",
    cfr_ifr_denominator %in% 330:20000   ~ "Reported Cases = 330+",
    TRUE ~ "Unspecified")) |>
  # Deduplicating CFRs
  # For MERS, we might want a closer think about what we want to do here
  # mutate(duplicate_cfr = case_when(
  #   access_param_id %in% c("037_006", "039_001", "081_002",
  #                          "172_004", "172_002") ~ "Known",
  #   access_param_id %in% c(
  #     "002_001", "025_006", "030_003", "037_005", "038_001", "040_008",
  #     "044_002", "044_007", "052_002", "093_001", "052_001", "054_001",
  #     "103_001", "109_001", "109_002", "109_003", "109_006", "109_004",
  #     "109_005", "109_007", "109_008", "109_011", "109_012", "109_013",
  #     "109_014", "109_015", "109_016", "109_017", "109_018", "109_019",
  #     "109_020", "109_021", "113_001", "129_002", "121_001", "129_003",
  #     "138_004", "151_004", "171_004", "173_001", "172_001", "179_001",
  #     "190_001") ~ "Assumed",
  #   TRUE ~ "False")) |>
  mutate(population_group = factor(population_group,
                                   levels = c(sort(setdiff(unique(population_group),
                                                           c("Other", "Unspecified"))),
                                              "Other", "Unspecified")))
#Let's re-assign all the country tags
d1 <- d1 |>
  mutate(population_country=ifelse(population_country=="Algeria; Austria; Bahrain; China; Egypt; France; Germany; Greece; Iran (Islamic Republic of); Italy; Jordan; Kuwait; Lebanon; Malaysia; Netherlands; Oman; Philippines; Qatar; Republic of Korea; Saudi Arabia; Thailand; Tunisia; Türkiye; United Arab Emirates; United Kingdom of Great Britain and Northern Ireland; United States of America; Yemen",
                                   "Global", population_country)) |>
  mutate(population_country=ifelse(population_country=="Austria; France; Germany; Greece; Italy; Netherlands; Spain; United Kingdom of Great Britain and Northern Ireland",
                                   "Europe", population_country)) |>
  mutate(population_country=ifelse(population_country=="Bahrain; Egypt; Iran (Islamic Republic of); Jordan; Kuwait; Lebanon; Oman; Qatar; Saudi Arabia; United Arab Emirates; Yemen",
                                   "Middle East", population_country)) |>
  mutate(population_country=ifelse(population_country=="China; Malaysia; Philippines; Republic of Korea; Thailand; Türkiye",
                                   "Other", population_country)) |>
  mutate(population_country=ifelse(population_country=="France; Iran (Islamic Republic of); Italy; Jordan; Kuwait; Lebanon; Oman; Qatar; Republic of Korea; Saudi Arabia; Tunisia; United Arab Emirates; United Kingdom of Great Britain and Northern Ireland; Yemen",
                                   "Global", population_country)) |>
  mutate(population_country=ifelse(population_country=="Lebanon; Malaysia; Oman; Qatar; Saudi Arabia; United Arab Emirates",
                                   "Middle East", population_country)) |>
  mutate(population_country=ifelse(population_country=="Oman; Saudi Arabia",
                                   "Middle East", population_country)) |>
  mutate(population_country=ifelse(population_country=="Republic of Korea; Saudi Arabia",
                                   "Other", population_country))

# Plot the spread:
# ---------- 1. Build dates ----------
plot_df <- d1 %>%
  mutate(
    start_year  = population_study_start_year,
    start_month = as.integer(population_study_start_month),
    start_day   = as.integer(population_study_start_day),
    end_year    = population_study_end_year,
    end_month   = as.integer(population_study_end_month),
    end_day     = as.integer(population_study_end_day)
  ) %>%
  mutate(
    start_month = if_else(is.na(start_month), 1L, start_month),
    start_day   = if_else(is.na(start_day),   1L, start_day),
    end_month   = if_else(is.na(end_month),   12L, end_month),
    end_day     = if_else(is.na(end_day),     28L, end_day)
  ) %>%
  mutate(
    start_date = make_date(start_year, start_month, start_day),
    end_date   = make_date(end_year,   end_month,   end_day)
  ) %>%
  mutate(
    start_date = pmin(start_date, end_date),
    end_date   = pmax(start_date, end_date)
  )

# ---------- 2. Compute vertical layout ----------
row_spacing <- 0.25   # vertical distance between rows inside country
country_gap <- 0.5    # space between countries

country_layout <- plot_df %>%
  group_by(population_country) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(population_country) %>%  # change ordering here if desired
  mutate(
    block_height = (n - 1) * row_spacing,
    block_start = cumsum(lag(block_height + country_gap, default = 0)),
    y_mid = block_start + block_height / 2
  )

# join and assign deterministic row position
plot_df2 <- plot_df %>%
  left_join(country_layout, by = "population_country") %>%
  group_by(population_country) %>%
  mutate(
    row_id = row_number(),
    y_position = block_start + (row_id - 1) * row_spacing
  ) %>%
  ungroup()

# ---------- 3. Color mapping for the two countries ----------
country_colors <- c(
  "Saudi Arabia"      = "#006C35",  # Saudi flag green (approx)
  "Republic of Korea" = "#C60C30"   # South Korea taegeuk red (approx)
)
plot_df2$population_country <- factor(
  plot_df2$population_country,
  levels = c("Saudi Arabia", "Republic of Korea")
  )
# ---------- 4. Plot ----------
plot_df2 <- plot_df2 |> arrange(start_date)

cfr_periods_forest_style(plot_df2, country_colors = country_colors,
                         lims = c(as.Date("2011-06-01"), as.Date("2023-12-31"))) +
  theme(
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank(),
  legend.position = c(0.80, 0.15),
  legend.background = element_rect(fill = "white", colour = "black"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
) -> cfr_study_periods

ggsave("CFR_Study_Periods.png",
cfr_study_periods,
       width = 10, height = 6)
######################################
# proportion of symptomatic cases
#TODO: Probably need to coalesce with central again here
d2 <- parameters |>
  filter(parameter_type == "Severity - proportion of symptomatic cases")

#165_003 is a Rule of 3 extraction - Alenazi (2017), check it comes out okay.
d2 <- d2 |>
  mutate(parameter_unit = 'Percentage (%)',
         parameter_value = coalesce(parameter_value, central), #using central where no % was reported
         population_study_start_year = as.numeric(population_study_start_year),
         population_study_end_year = as.numeric(population_study_end_year),
         study_midyear = ifelse(!is.na(population_study_start_year) & !is.na(population_study_end_year),
                                round((population_study_start_year + population_study_end_year) / 2),
                                population_study_start_year)) |>
  mutate(study_midyear_cat = case_when(
    study_midyear %in% 1990:1999 ~ "1990-1999",
    study_midyear %in% 2000:2009 ~ "2000-2009",
    study_midyear %in% 2010:2019 ~ "2010-2019",
    study_midyear %in% 2020:2029 ~ "2020-Present",
    TRUE ~ "Unspecified")) |>
  mutate(cfr_denom_cat = case_when(
    cfr_ifr_denominator %in% 1:29      ~ "Reported Cases < 30",
    cfr_ifr_denominator %in% 30:99     ~ "Reported Cases = 30-99",
    cfr_ifr_denominator %in% 100:329   ~ "Reported Cases = 100-329",
    cfr_ifr_denominator %in% 330:20000   ~ "Reported Cases = 330+",
    TRUE ~ "Unspecified")) |>
mutate(population_group = factor(population_group,
                                 levels = c(sort(setdiff(unique(population_group),
                                                         c("Other", "Unspecified"))),
                                            "Other", "Unspecified")))
######################################
# proportion of asymptomatic cases
#TODO: Probably need to coalesce with central again here
d3 <- parameters |>
  filter(parameter_type == "Severity - proportion of asymptomatic cases")

d3 <- d3 |>
  mutate(parameter_unit = 'Percentage (%)',
         parameter_value = coalesce(parameter_value, central), #using central where no % was reported
         population_study_start_year = as.numeric(population_study_start_year),
         population_study_end_year = as.numeric(population_study_end_year),
         study_midyear = ifelse(!is.na(population_study_start_year) & !is.na(population_study_end_year),
                                round((population_study_start_year + population_study_end_year) / 2),
                                population_study_start_year)) |>
  mutate(study_midyear_cat = case_when(
    study_midyear %in% 1990:1999 ~ "1990-1999",
    study_midyear %in% 2000:2009 ~ "2000-2009",
    study_midyear %in% 2010:2019 ~ "2010-2019",
    study_midyear %in% 2020:2029 ~ "2020-Present",
    TRUE ~ "Unspecified")) |>
  mutate(cfr_denom_cat = case_when(
    cfr_ifr_denominator %in% 1:29      ~ "Reported Cases < 30",
    cfr_ifr_denominator %in% 30:99     ~ "Reported Cases = 30-99",
    cfr_ifr_denominator %in% 100:329   ~ "Reported Cases = 100-329",
    cfr_ifr_denominator %in% 330:20000   ~ "Reported Cases = 330+",
    TRUE ~ "Unspecified")) |>
  mutate(population_group = factor(population_group,
                                   levels = c(sort(setdiff(unique(population_group),
                                                           c("Other", "Unspecified"))),
                                              "Other", "Unspecified")))

# *------------------------------ Meta-analysis -------------------------------*
# Plot file structure - many plots created in this task so better to create a
# folder structure
dir.create("figures")
filepath <- "figures"


# Plot colour
imperial_khaki <- "#EFE58B"
text_size <- 15
lanonc_colours <- ggsci::pal_lancet("lanonc")(9)
meta_digits <- 2

# *---------------------- CFR from extracted parameters -----------------------*
# Extracted CFRs
plot_list <- list("qa_filtered"=list("meta"=list(), "forest"=list()))

  # Colours:
  all_pop_groups <- d1 |>
    distinct(population_group) |>
    arrange(population_group == "Other", population_group) |>
    pull()

  custom_colour_pop_groups <- lanonc_colours[seq_along(all_pop_groups)]
  names(custom_colour_pop_groups) <- all_pop_groups

  all_countries <- d1 |>
    distinct(population_country) |>
    arrange(population_country) |>
    pull()

  #Note we can only have up to 8 so this doesn't work at the moment.
  custom_colour_countries <- lanonc_colours[seq_along(all_countries)]
  custom_colour_countries[10] <- "black"
  custom_colour_countries[11] <- "firebrick"
  names(custom_colour_countries) <- all_countries

  plot_list[["qa_filtered"]][["meta"]][["m2"]] <- metaprop_wrap(
    dataframe = d1, subgroup = NA,
    plot_pooled = TRUE, sort_by_subg = TRUE,
    plot_study = TRUE, digits = meta_digits,
    colour = imperial_khaki,
    width = 9500, height = 20000, resolution = 1000)

  ggsave("figures/meta_study_no_cat.pdf",
         plot_list[["qa_filtered"]][["meta"]][["m2"]]$plot,
         width = 8.5, height = 18)
  ggsave("figures/meta_study_no_cat.png",
         plot_list[["qa_filtered"]][["meta"]][["m2"]]$plot,
         width = 8.5, height = 18)

  plot_list[["qa_filtered"]][["meta"]][["m3"]] <- metaprop_wrap(
    dataframe = d1, subgroup = "cfr_denom_cat", plot_pooled = TRUE,
    sort_by_subg = FALSE, plot_study = TRUE, digits = meta_digits,
    colour = imperial_khaki, width = 9500, height = 22000, resolution = 1000)

  ggsave("figures/meta_cfr_denom_cat.pdf",
         plot_list[["qa_filtered"]][["meta"]][["m3"]]$plot,
         width = 8, height = 20)
  ggsave("figures/meta_cfr_denom_cat.png",
         plot_list[["qa_filtered"]][["meta"]][["m3"]]$plot,
         width = 8, height = 20)

  plot_list[["qa_filtered"]][["meta"]][["m4"]] <- metaprop_wrap(
    dataframe = d1, subgroup = "population_group", plot_pooled = TRUE,
    sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
    colour = imperial_khaki, width = 9500, height = 22000, resolution = 1000)

  ggsave("figures/meta_cfr_population_group.pdf",
         plot_list[["qa_filtered"]][["meta"]][["m4"]]$plot,
         width = 8, height = 20)
  ggsave("figures/meta_cfr_population_group.png",
         plot_list[["qa_filtered"]][["meta"]][["m4"]]$plot,
         width = 8, height = 20)

  # And by country,
  plot_list[["qa_filtered"]][["meta"]][["m5"]] <- metaprop_wrap(
    dataframe = filter(d1, population_country %in% c("Republic of Korea", "Saudi Arabia")
                       ), subgroup = "population_country", plot_pooled = TRUE,
    sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
    colour = imperial_khaki, width = 9000, height = 20000, resolution = 1000)

  ggsave("figures/meta_cfr_country.pdf",
         plot_list[["qa_filtered"]][["meta"]][["m5"]]$plot,
         width = 8, height = 14)
  ggsave("figures/meta_cfr_country.png",
         plot_list[["qa_filtered"]][["meta"]][["m5"]]$plot,
         width = 8, height = 14)

  # Forest plot
  #############
  plot_list[["qa_filtered"]][["forest"]][["p_cfr_2"]] <- forest_plot(
    d1, "Case-Fatality Ratio (%)","population_group",
    c(-10,110), custom_colours = custom_colour_pop_groups,
    text_size=text_size, sort=TRUE) +
    guides(shape = guide_legend(title = "Parameter type", order=1),
           fill =  guide_none(),
           linetype = guide_none(),
           color =  guide_legend(title = "Population group", order=2))

  ggsave("figures/forest_cfr_population_group.pdf",
         plot =  plot_list[["qa_filtered"]][["forest"]][["p_cfr_2"]],
         width = 8, height = 14)
  ggsave("figures/forest_cfr_population_group.png",
         plot =  plot_list[["qa_filtered"]][["forest"]][["p_cfr_2"]],
         width = 8, height = 14)

  plot_list[["qa_filtered"]][["forest"]][["p_cfr_3"]] <- forest_plot(
    d1, "Case-Fatality Ratio (%)","population_sample_type",
    c(-10,110), #custom_colours = custom_colour_pop_groups,
    text_size=text_size, sort=TRUE) +
    guides(shape = guide_legend(title = "Parameter type", order=1),
           fill =  guide_none(),
           linetype = guide_none(),
           color =  guide_legend(title = "Population type", order=2))

  ggsave("figures/forest_cfr_population_sample_type.pdf",
         plot =  plot_list[["qa_filtered"]][["forest"]][["p_cfr_3"]],
         width = 8, height = 14)
  ggsave("figures/forest_cfr_population_sample_type.png",
         plot =  plot_list[["qa_filtered"]][["forest"]][["p_cfr_3"]],
         width = 8, height = 14)

  all_countries <- d2 |>
    distinct(population_country) |>
    arrange(population_country) |>
    pull()

  #Note we can only have up to 8 so this doesn't work at the moment.
  custom_colour_countries <- lanonc_colours[seq_along(all_countries)]
  names(custom_colour_countries) <- all_countries

  plot_list[["qa_filtered"]][["forest"]][["p_prop_1"]] <- forest_plot(
    d2, "Percentage of Symptomatic Cases (%)", "population_country",
    c(-10, 110), custom_colours = custom_colour_countries,
    text_size=text_size, sort=TRUE) +
    guides(color = guide_legend(title = "Population country", order=2),
           linetype = guide_none(),
           shape = guide_legend(title = "Parameter type", order=1))

  ggsave("figures/figure_3_forest_prop_country.pdf",
         plot =  plot_list[["qa_filtered"]][["forest"]][["p_prop_1"]],
         width = 6, height = 6)
  ggsave("figures/figure_3_forest_prop_country.png",
         plot =  plot_list[["qa_filtered"]][["forest"]][["p_prop_1"]],
         width = 6, height = 6)

  plot_list[["qa_filtered"]][["forest"]][["p_prop_2"]] <- forest_plot(
    d2, "Percentage of Symptomatic Cases (%)", "population_group",
    c(-10, 110), custom_colours = custom_colour_pop_groups,
    text_size=text_size, sort=TRUE) +
    guides(color = guide_legend(title = "Population group", order=2),
           linetype = guide_none(),
           shape = guide_legend(title = "Parameter type", order=1))

  ggsave("figures/figure_3_forest_prop_pop_group.pdf",
         plot =  plot_list[["qa_filtered"]][["forest"]][["p_prop_2"]],
         width = 6, height = 6)
  ggsave("figures/figure_3_forest_prop_pop_group.png",
         plot =  plot_list[["qa_filtered"]][["forest"]][["p_prop_2"]],
         width = 6, height = 6)

  #And ASYMPTOMATIC

  all_countries <- d3 |>
    distinct(population_country) |>
    arrange(population_country) |>
    pull()

  plot_list[["qa_filtered"]][["forest"]][["p_prop_2"]] <- forest_plot(
    d3, "Percentage of Asymptomatic Cases (%)", "population_group",
    c(-10, 110), custom_colours = custom_colour_pop_groups,
    text_size=text_size, sort=TRUE) +
    guides(color = guide_legend(title = "Population group", order=2),
           linetype = guide_none(),
           shape = guide_legend(title = "Parameter type", order=1))

  ggsave("figures/figure_4_forest_prop_pop_group.pdf",
         plot =  plot_list[["qa_filtered"]][["forest"]][["p_prop_2"]],
         width = 6, height = 6)
  ggsave("figures/figure_4_forest_prop_pop_group.png",
         plot =  plot_list[["qa_filtered"]][["forest"]][["p_prop_2"]],
         width = 6, height = 6)

  ############
  d4 <- parameters |>
    filter(parameter_type == 'Severity - infection fatality ratio (IFR)')
  # 3 rows have NA for population_group
  # This causes plotting issues, so change the NA to "Unspecified" for now
  d4 <- d4 |>
    mutate(population_group = ifelse(is.na(population_group), "Unspecified", population_group))
  d4 <- d4 |>
    mutate(parameter_unit = 'Percentage (%)',
           parameter_value = coalesce(parameter_value, central), #using central where no % was reported
           population_study_start_year = as.numeric(population_study_start_year),
           population_study_end_year = as.numeric(population_study_end_year),
           study_midyear = ifelse(!is.na(population_study_start_year) & !is.na(population_study_end_year),
                                  round((population_study_start_year + population_study_end_year) / 2),
                                  population_study_start_year)) |>
    mutate(study_midyear_cat = case_when(
      study_midyear %in% 1990:1999 ~ "1990-1999",
      study_midyear %in% 2000:2009 ~ "2000-2009",
      study_midyear %in% 2010:2019 ~ "2010-2019",
      study_midyear %in% 2020:2029 ~ "2020-Present",
      TRUE ~ "Unspecified")) |>
    mutate(cfr_denom_cat = case_when(
      cfr_ifr_denominator %in% 1:29      ~ "Reported Cases < 30",
      cfr_ifr_denominator %in% 30:99     ~ "Reported Cases = 30-99",
      cfr_ifr_denominator %in% 100:329   ~ "Reported Cases = 100-329",
      cfr_ifr_denominator %in% 330:20000   ~ "Reported Cases = 330+",
      TRUE ~ "Unspecified")) |>
  mutate(population_group = factor(population_group,
                                   levels = c(sort(setdiff(unique(population_group),
                                                           c("Other", "Unspecified"))),
                                              "Other", "Unspecified")))

  IFR_plot <- forest_plot(
    d4, "Infection Fatality Ratio (%)","population_group",
    c(-10,110), #custom_colours = custom_colour_pop_groups,
    text_size=text_size, sort=TRUE) +
    guides(shape = guide_legend(title = "Parameter type", order=1),
           fill =  guide_none(),
           linetype = guide_none(),
           color =  guide_legend(title = "Population type", order=2))

  ggsave("figures/figure_5_IFR_pop_group.pdf",
         plot =  plot_list[["qa_filtered"]][["forest"]][["p_prop_2"]],
         width = 6, height = 6)
  ggsave("figures/figure_5_IFR_pop_group.png",
         plot =  plot_list[["qa_filtered"]][["forest"]][["p_prop_2"]],
         width = 6, height = 6)
