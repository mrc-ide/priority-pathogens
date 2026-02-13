# *=================== Nipah severity meta-analysis & plots ===================*
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
#TODO: Discuss this as a group
unique(filter(parameters, parameter_class == "Severity")$parameter_type)
# So CF ratio AND CF rate
#"Severity - symptomatic proportion of infections"
#"Severity - asymptomatic proportion of infections"
#"Severity - proportion of asymptomatic cases"
#"Severity - proportion of symptomatic cases"
table(filter(parameters, parameter_class == "Severity")$parameter_type)

#For now, map ratio to rate and symptomatics together
#TODO: Return to asymptomatics
parameters$parameter_type <- gsub("case fatality ratio",
                    "case fatality rate",
                    parameters$parameter_type)

parameters$parameter_type <- gsub("symptomatic proportion of infections",
                    "proportion of symptomatic cases",
                    parameters$parameter_type)

# *----------------------------- Data preparation -----------------------------*
# Extracted CFRs parameters
d1 <- parameters |>
  filter(parameter_type == 'Severity - case fatality rate (CFR)')
# 3 rows have NA for population_group
# This causes plotting issues, so change the NA to "Unspecified" for now
d1 <- d1 |>
  mutate(population_group = ifelse(is.na(population_group), "Unspecified", population_group))

# Unspecified parameter type for CFR with only numerator and denom
# d1 <- d1 |>
#   filter(parameter_unit != "Unspecified")

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
    TRUE ~ "Unspecified")) |>
mutate(population_group = factor(population_group,
                                 levels = c(sort(setdiff(unique(population_group),
                                                         c("Other", "Unspecified"))),
                                            "Other", "Unspecified")))

# *------------------------------ Meta-analysis -------------------------------*
# Plot file structure - many plots created in this task so better to create a
# folder structure
filepath_vec <- c(file.path("figures"),
                  file.path("figures", "extracted_parameters", "all"),
                  file.path("figures","extracted_parameters", "qa_filtered"))

for (filepath in filepath_vec){
  if (!dir.exists(filepath)) {
    dir.create(filepath, recursive = TRUE)
  }
}


# Plot colour
imperial_khaki <- "#EFE58B"
text_size <- 15
lanonc_colours <- ggsci::pal_lancet("lanonc")(9)
meta_digits <- 2

# *---------------------- CFR from extracted parameters -----------------------*
# Extracted CFRs
qa_thresh_vec <- c("all"=-1, "qa"=0.5)
qa_alpha_vec <- c(0.3, 1)
list_label_vec <- c("all", "qa_filtered")
plot_list <- list("all"=list("meta"=list(), "forest"=list()),
                  "qa_filtered"=list("meta"=list(), "forest"=list()))
labels <- c("SI_allqa", "")
cfr_duplicates <- list("no_dups"="False",
                       "no_known_dups"=c("False", "Assumed"),
                       "all"=c("False", "Assumed", "Known"))

for (i in seq_along(qa_thresh_vec)){
  list_label <- list_label_vec[i]
  qa_threshold <- qa_thresh_vec[i]
  qa_alpha <- qa_alpha_vec[i]
  plot_type <- file.path("figures", "extracted_parameters", list_label)

  d1_filtered <- d1 |> filter(qa_score>qa_threshold)#,
                              #duplicate_cfr=="False")
  d2_filtered <- d2 |> filter(qa_score>qa_threshold)

  # Colours:
  all_pop_groups <- d1_filtered |>
    distinct(population_group) |>
    arrange(population_group == "Other", population_group) |>
    pull()

  custom_colour_pop_groups <- lanonc_colours[seq_along(all_pop_groups)]
  names(custom_colour_pop_groups) <- all_pop_groups

  all_countries <- d1_filtered |>
    distinct(population_country) |>
    arrange(population_country) |>
    pull()

  #Note we can only have up to 8 so this doesn't work at the moment.
  custom_colour_countries <- lanonc_colours[seq_along(all_countries)]
  names(custom_colour_countries) <- all_countries

  # plot_list[[list_label]][["meta"]][["m1"]] <- metaprop_wrap(
  #   dataframe = d1_filtered, subgroup = "population_country", plot_pooled = TRUE,
  #   sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
  #   colour = imperial_khaki, width = 10500, height = 9000, resolution = 1000)
  #
  # ggsave(file.path(plot_type,
  #                  paste0("figure_3_meta_population_country.pdf")),
  #        plot_list[[list_label]][["meta"]][["m1"]]$plot,
  #        width = 5.8, height = 5)
  # ggsave(file.path(plot_type,
  #                  paste0("figure_3_meta_population_country.png")),
  #        plot_list[[list_label]][["meta"]][["m1"]]$plot,
  #        width = 5.8, height = 5)

  plot_list[[list_label]][["meta"]][["m2"]] <- metaprop_wrap(
    dataframe = d1_filtered, subgroup = "study_midyear_cat",
    plot_pooled = TRUE, sort_by_subg = TRUE,
    plot_study = TRUE, digits = meta_digits,
    colour = imperial_khaki,
    width = 10500, height = 9000, resolution = 1000)

  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_study_midyear_cat.pdf")),
         plot_list[[list_label]][["meta"]][["m2"]]$plot,
         width = 8.5, height = 7)
  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_study_midyear_cat.png")),
         plot_list[[list_label]][["meta"]][["m2"]]$plot,
         width = 8.5, height = 7)

  # Update to account for dedup:
  #TODO: This is definitely Nipah specific, comment out for now
  # d1_filtered <-  d1_filtered |>
  #   mutate(cfr_denom_cat = ifelse(cfr_denom_cat=="Reported Cases = 100-329",
  #                                 "Reported Cases = 100-250", cfr_denom_cat))

  plot_list[[list_label]][["meta"]][["m3"]] <- metaprop_wrap(
    dataframe = d1_filtered, subgroup = "cfr_denom_cat", plot_pooled = TRUE,
    sort_by_subg = FALSE, plot_study = TRUE, digits = meta_digits,
    colour = imperial_khaki, width = 10500, height = 8000, resolution = 1000)

  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_cfr_denom_cat.pdf")),
         plot_list[[list_label]][["meta"]][["m3"]]$plot,
         width = 8, height = 6)
  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_cfr_denom_cat.png")),
         plot_list[[list_label]][["meta"]][["m3"]]$plot,
         width = 8, height = 6)

  plot_list[[list_label]][["meta"]][["m4"]] <- metaprop_wrap(
    dataframe = d1_filtered, subgroup = "population_group", plot_pooled = TRUE,
    sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
    colour = imperial_khaki, width = 10000, height = 9000, resolution = 1000)

  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_population_group.pdf")),
         plot_list[[list_label]][["meta"]][["m4"]]$plot,
         width = 7.3, height = 6)
  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_population_group.png")),
         plot_list[[list_label]][["meta"]][["m4"]]$plot,
         width = 7.3, height = 6)

  # Forest plot
  #############
  # plot_list[[list_label]][["forest"]][["p_cfr_1"]] <- forest_plot(
  #   d1_filtered, "Case-Fatality Ratio (%)", "population_country",
  #   c(-10,110), custom_colours = custom_colour_countries,
  #   text_size=text_size, qa_alpha=qa_alpha, sort=TRUE) +
  #   guides(shape = guide_legend(title = "Parameter type", order=1),
  #          fill = guide_none(),
  #          linetype = guide_none(),
  #          color = guide_legend(title = "Population country", order=3))
  # ggsave(file.path(plot_type,
  #                  paste0("figure_3_forest_cfr_population_country.pdf")),
  #        plot = plot_list[[list_label]][["forest"]][["p_cfr_1"]],
  #        width = 10, height = 12)

  plot_list[[list_label]][["forest"]][["p_cfr_2"]] <- forest_plot(
    d1_filtered, "Case-Fatality Ratio (%)","population_group",
    c(-10,110), custom_colours = custom_colour_pop_groups,
    text_size=text_size, qa_alpha=qa_alpha, sort=TRUE) +
    guides(shape = guide_legend(title = "Parameter type", order=1),
           fill =  guide_none(),
           linetype = guide_none(),
           color =  guide_legend(title = "Population group", order=2))

  ggsave(file.path(plot_type,
                   paste0("figure_3_forest_cfr_population_group.pdf")),
         plot =  plot_list[[list_label]][["forest"]][["p_cfr_2"]],
         width = 10, height = 12)

  all_countries <- d2_filtered |>
    distinct(population_country) |>
    arrange(population_country) |>
    pull()

  #Note we can only have up to 8 so this doesn't work at the moment.
  custom_colour_countries <- lanonc_colours[seq_along(all_countries)]
  names(custom_colour_countries) <- all_countries

  plot_list[[list_label]][["forest"]][["p_prop_1"]] <- forest_plot(
    d2_filtered, "Percentage of Symptomatic Cases (%)", "population_country",
    c(-10, 110), custom_colours = custom_colour_countries,
    text_size=text_size, qa_alpha=qa_alpha, sort=TRUE) +
    guides(color = guide_legend(title = "Population country", order=2),
           linetype = guide_none(),
           shape = guide_legend(title = "Parameter type", order=1))

  ggsave(file.path(plot_type,
                   paste0("figure_3_forest_prop_country.pdf")),
         plot =  plot_list[[list_label]][["forest"]][["p_prop_1"]],
         width = 8, height = 5)

  plot_list[[list_label]][["forest"]][["p_prop_2"]] <- forest_plot(
    d2_filtered, "Percentage of Symptomatic Cases (%)", "population_group",
    c(-10, 110), custom_colours = custom_colour_pop_groups,
    text_size=text_size, qa_alpha=qa_alpha, sort=TRUE) +
    guides(color = guide_legend(title = "Population group", order=2),
           linetype = guide_none(),
           shape = guide_legend(title = "Parameter type", order=1))

  ggsave(file.path(plot_type,
                   paste0("figure_3_forest_prop_population_group.pdf")),
         plot =  plot_list[[list_label]][["forest"]][["p_prop_2"]],
         width = 8, height = 5)
}

# Additional CFR from extracted parameters results
# Can't do year since the estimates may relate to a range and outbreak location
# is not clean (possible that there's a single estimate for multiple locations)
# No duplicates
d1_dup_false <- d1 ## |> filter(duplicate_cfr=="False")

# meta_dup_false <- metaprop_wrap(
#   dataframe = d1_dup_false, subgroup = "population_country", plot_pooled = TRUE,
#   sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
#   colour = imperial_khaki, width = 10500, height = 9000, resolution = 1000)

# ggsave(file.path("figures", "extracted_parameters",
#                  "SI_CFR_meta_country_param_dup_eq_false.pdf"),
#        meta_dup_false$plot,
#        width = 5.8, height = 5)


# Assumed duplicates included
#TODO: I'm just commenting out the below dupe stuff for now, we'll have a think about what we want to do with this.
# Nipah note: Error when trying to fit this?

# d1_dup_assumed <- d1 ## |>
#   ##filter(duplicate_cfr!="Known")
#
# meta_dup_assumed <- metaprop_wrap(
#   dataframe = d1_dup_assumed, subgroup = "population_country", plot_pooled = TRUE,
#   sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
#   colour = imperial_khaki, width = 10500, height = 18000, resolution = 1000)
#
# ggsave(file.path("figures", "extracted_parameters",
#                  "SI_CFR_meta_country_param_dup_neq_known.pdf"),
#        meta_dup_assumed$plot,
#        width = 5.8, height = 10)

# All
# meta_all <- metaprop_wrap(
#   dataframe = d1, subgroup = "population_country", plot_pooled = TRUE,
#   sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
#   colour = imperial_khaki, width = 10500, height = 20000, resolution = 1000)

# ggsave(file.path("figures", "extracted_parameters",
#                  "SI_CFR_meta_country_param_dup_all.pdf"),
#        meta_all$plot,
#        width = 5.8, height = 10.8)

# Combine figures
# p1 <- plot_list[["all"]][["meta"]][["m1"]]$plot +
#   theme(plot.margin = margin(-50, -250, -250, -50))
# p2 <- cfr_outbreak_country$plot +
#   theme(plot.margin = margin(-250, -250, -250, -250))
# p3 <- cfr_from_bangladesh_surveillance_yc$plot +
#   theme(plot.margin = margin(-50, -250, -50, -250))
# p4 <- plot_list[["all"]][["forest"]][["p_cfr_1"]] +
#   theme(legend.position=c(0.275, 0.775), legend.direction = "vertical")
# p5 <- plot_list[["all"]][["forest"]][["p_prop_1"]] +
#   guides(color = guide_none(),
#          linetype = guide_none(),
#          shape = guide_none())
#
# left_col  <- p4 / p5 + plot_layout(heights = c(26, 3))
# right_col <- p1 / p2 / p3 +
#   plot_layout(heights = c(2.4, 2.4, 2.45))
#
# patchwork <- (left_col | right_col) +
#   plot_layout(widths = c(2.5, 4)) +
#   plot_annotation(tag_levels = "A") &
#   theme(plot.tag.position = c(0, 1), plot.margin = margin(5.5, 0, 0, 5.5),
#         plot.tag = element_text(size = 14))

# ggsave(file.path("figures", "figure_severity_forest.png"),
#        plot = left_col, width = 8, height = 10)
# ggsave(file.path("figures","figure_severity_forest.pdf"),
#        plot = left_col, width = 8, height = 10)
#
# ggsave(file.path("figures", "figure_severity.png"),
#        plot = patchwork, width = 11.5, height = 10, dpi=300)
# ggsave(file.path("figures", "figure_severity.pdf"),
#        plot = patchwork, width = 11.5, height = 10, dpi=300)

# Additional plots for SI
# Since loop only uses deduplicated CFR, recreate the plot (inefficient)
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

custom_colour_countries <- lanonc_colours[seq_along(all_countries)]
names(custom_colour_countries) <- all_countries

p1 <- forest_plot(
  d1, "Case-Fatality Ratio (%)","population_country",
  c(-10,110), custom_colours = custom_colour_countries,
  text_size=text_size, qa_alpha=0.3, sort=TRUE) +
  guides(shape = guide_legend(title = "Parameter type", order=1),
         fill =  guide_none(),
         linetype = guide_none(),
         color =  guide_legend(title = "Country", order=2))

p2 <- forest_plot(
  d1, "Case-Fatality Ratio (%)","population_group",
  c(-10,110), custom_colours = custom_colour_pop_groups,
  text_size=text_size, qa_alpha=0.3, sort=TRUE) +
  guides(shape = guide_legend(title = "Parameter type", order=1),
         fill =  guide_none(),
         linetype = guide_none(),
         color =  guide_legend(title = "Population group", order=2))

patchwork_si <- p1 +
  theme(legend.position=c(0.175, 0.7), legend.direction = "vertical") +
  p2 +
  theme(legend.position=c(0.25, 0.7), legend.direction = "vertical") +
  guides(shape=guide_none()) +
  plot_layout(ncol = 2) + plot_annotation(tag_levels = 'A')

ggsave(file.path("figures", "figure_SI_severity.png"),
       plot = patchwork_si, width = 15, height = 18)
ggsave(file.path("figures", "figure_SI_severity.pdf"),
       plot = patchwork_si, width = 15, height = 18)

p3 <- forest_plot(
  d1, "Case-Fatality Ratio (%)","population_group",
  c(-10,110), custom_colours = custom_colour_pop_groups,
  text_size=text_size,qa_alpha=0.3, sort=TRUE) +
  guides(shape = guide_legend(title = "Parameter type", order=1),
         fill =  guide_none(),
         linetype = guide_none(),
         color =  guide_legend(title = "Population group", order=2)) +
  ggforce::facet_col(facets = vars(population_country),
                     scales = "free_y",
                     space = "free") +
  theme(legend.position=c(0.2, 0.875))

ggsave(file.path("figures", "figure_SI_severity_facet.png"),
       plot = p3, width = 10, height = 18)
ggsave(file.path("figures", "figure_SI_severity_facet.pdf"),
       plot = p3, width = 10, height = 18)
