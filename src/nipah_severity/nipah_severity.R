# *=================== Nipah severity meta-analysis & plots ===================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(grid)
library(metafor)
library(meta)
library(orderly2)
library(patchwork)
library(readr)
library(stringr)
library(tidyr)

# *--------------------------------- Orderly ----------------------------------*
orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "outbreaks.csv", "models.csv", "params.csv"))

orderly_shared_resource("nipah_functions.R" = "nipah_functions.R")
orderly_shared_resource("NIPAH_Bangladesh_IEDCR.csv"="NIPAH_Bangladesh_IEDCR.csv")
orderly_shared_resource("cleaned_outbreak_data.RDS"="cleaned_outbreak_data.RDS")

source("nipah_functions.R")

orderly_artefact("Nipah severity figures",
                 c("figure_severity.png", "figure_severity.pdf"))
# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles,outbreaks,models,parameters, plotting = FALSE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

subcolumns_outbreak <- readRDS("cleaned_outbreak_data.RDS")
# *----------------------------- Data preparation -----------------------------*
# From extracted outbreaks
iso_lookup <- c(
  "India" = "IND",
  "Bangladesh" = "BGD",
  "Malaysia" = "MYS",
  "Philippines" = "PHL",
  "Singapore" = "SGP"
)

cfr_from_outbreaks <- subcolumns_outbreak |>
  mutate(outbreak_source = replace_na(outbreak_source, 'Unknown')) |>
  mutate(cfr_ifr_denominator = round(total_cases),
         cfr_ifr_numerator   = round(deaths),
         CFR                 = cfr_ifr_numerator / cfr_ifr_denominator,
         refs                = paste(outbreak_country, outbreak_location, sep = " |> "),
         parameter_value     = CFR,
         parameter_unit      = 'Percentage') |> arrange(desc(CFR)) |>
  mutate(outbreak_source = case_when(str_detect(outbreak_source, 'Domestic animal' ) ~ 'Domestic animal',
                                     str_detect(outbreak_source, 'Wild animal' ) ~ 'Wild animal',
                                     TRUE ~ outbreak_source),
         refs_full = refs,
           refs = paste0(iso_lookup[trimws(str_extract(refs, "^[^|]+"))],
             " |> ",
             # first 3 letters of each word in location
             str_to_upper(str_replace_all( str_extract(refs, "(?<=\\|>).*$"),
                                           "\\b([A-Za-z]{1,3})[A-Za-z]*\\b",
                                           "\\1")))
         )

# IEDCR
cfr_bangladesh <- read_csv('NIPAH_Bangladesh_IEDCR.csv')

cfr_from_bangladesh_surveillance <- cfr_bangladesh |>
 mutate(year_cat = case_when(
   Year %in% 2000:2004 ~ "2000-2004",
   Year %in% 2005:2009 ~ "2005-2009",
   Year %in% 2010:2014 ~ "2010-2014",
   Year %in% 2015:2019 ~ "2015-2019",
   Year %in% 2020:2029 ~ "2020-Present",
   TRUE ~ "Unspecified")) |>
  filter(Cases!=0) |>
  mutate(cfr_ifr_denominator = Cases,
         cfr_ifr_numerator   = Death,
         CFR                 = cfr_ifr_numerator / cfr_ifr_denominator,
         refs                = Year,
         parameter_value     = CFR,
         parameter_unit      = 'Percentage') |> arrange(desc(CFR))

# Extracted CFRs parameters
d1 <- parameters |>
  filter(parameter_type == 'Severity - case fatality rate (CFR)')

# Unspecified parameter type for CFR with only numerator and denom
# d1 <- d1 |>
#   filter(parameter_unit != "Unspecified")

# Create subgroups here
# Lassa examples for study midyear, population_group, cfr_denom_cat
d1 <- d1 |>
  mutate(population_group = case_when(
    population_group == "Persons under investigation" ~ "Persons Under Investigation",
    population_group == "Persons with symptoms" ~ "Persons with symptoms",
    population_group == "Healthcare workers" ~ "Healthcare workers",
    population_group == "Abattoir workers" ~ "Animal workers",
    population_group == "Animal workers" ~ "Animal workers",
    is.na(population_group) ~ "Other",
    TRUE ~ "Mixed Groups")) |>
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
  mutate(duplicate_cfr = case_when(
    access_param_id %in% c("037_006", "039_001", "081_002",
                           "172_004", "172_002") ~ "Known",
    access_param_id %in% c(
      "002_001", "025_006", "030_003", "037_005", "038_001", "040_008",
      "044_002", "044_007", "052_002", "093_001", "052_001", "054_001",
      "103_001", "109_001", "109_002", "109_003", "109_006", "109_004",
      "109_005", "109_007", "109_008", "109_011", "109_012", "109_013",
      "109_014", "109_015", "109_016", "109_017", "109_018", "109_019",
      "109_020", "109_021", "113_001", "129_002", "121_001", "129_003",
      "138_004", "151_004", "171_004", "173_001", "172_001", "179_001",
      "190_001") ~ "Assumed",
    TRUE ~ "False")) #only identified for estimates passed to meta-analysis (i.e. denominator not NA)

# proportion of symptomatic cases
d2 <- parameters |>
    filter(parameter_type == "Severity - proportion of symptomatic cases")

# *------------------------------ Meta-analysis -------------------------------*
# Plot colour
imperial_khaki <- "#EFE58B"
text_size <- 15
lanonc_colours <- ggsci::pal_lancet("lanonc")(9)

# DONT DEDUPLICATE AS THIS HAS CONTEXT INFORMATION
# cfr <- cfr |>
#   mutate(parameter_context_location_type=replace_na(
#     parameter_context_location_type,'Unspecified'),
#          parameter_notes=replace_na(parameter_notes,''))
# cfr$is_duplicate <- FALSE
# #cfr[cfr$outbreak_duriation_years>5,]$is_duplicate <- TRUE
# cfr[cfr$population_country == 'Malaysia,Singapore',]$is_duplicate <- TRUE
# cfr[cfr$population_country == 'Malaysia' & cfr$parameter_context_location_type != 'State',]$is_duplicate <- TRUE #we take the individual state outbreaks rather than the national aggregate one.
# cfr[str_detect(cfr$parameter_notes,'cluster'),]$is_duplicate <- TRUE #we take the individual outbreaks rather than the cluster
#
# cfr <- cfr |> filter( !is_duplicate ) |>
#   mutate( unique_id = paste(covidence_id, population_country, population_location,
#                             population_study_start_year, str_remove(population_study_start_month, "^0+"), sep = "|"),
#           EXCLUDE   = ifelse(unique_id %in% exclude_ids, 1, 0)) |>
#   filter(!EXCLUDE)

# CFR from outbreaks:
cfr_outbreak_ma <- metaprop_wrap(
  cfr_from_outbreaks, subgroup = 'outbreak_country', plot_pooled = TRUE,
  sort_by_subg = TRUE, plot_study = FALSE, digits = 2, colour = imperial_khaki,
  width = 9500, height = 6000, resolution = 1000)

ggsave(file.path("all", "figure_3_meta_outbreak_country.pdf"),
       cfr_outbreak_ma$plot, width = 12, height = 6)
ggsave(file.path("all", "figure_3_meta_outbreak_country.png"),
       cfr_outbreak_ma$plot, width = 12, height = 6)

# CFR from Bangladesh surveillance:
cfr_from_bangladesh_surveillance_ma <- metaprop_wrap(
  cfr_from_bangladesh_surveillance, subgroup = NA, plot_pooled = TRUE,
  plot_study = TRUE, digits = 2, colour = imperial_khaki,
  width = 9500, height = 6000, resolution = 1000)

ggsave(file.path("all", "figure_3_meta_no_subg_IEDCR.pdf"),
       cfr_from_bangladesh_surveillance_ma$plot, width = 12, height = 10)
ggsave(file.path("all", "figure_3_meta_no_subg_IEDCR.png"),
       cfr_from_bangladesh_surveillance_ma$plot, width = 12, height = 10)

cfr_from_bangladesh_surveillance_yc <- metaprop_wrap(
  cfr_from_bangladesh_surveillance, subgroup = "year_cat", plot_pooled = TRUE,
  sort_by_subg = TRUE, plot_study = FALSE, digits = 2, colour = imperial_khaki,
  width = 9500, height = 7000, resolution = 1000)

ggsave(file.path("all", "figure_3_meta_year_cat_IEDCR.pdf"),
       cfr_from_bangladesh_surveillance_yc$plot, width = 10, height = 8)
ggsave(file.path("all", "figure_3_meta_year_cat_IEDCR.png"),
       cfr_from_bangladesh_surveillance_yc$plot, width = 10, height = 8)

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
  plot_type <- names(qa_thresh_vec)[i]

  d1_filtered <- d1 |> filter(qa_score>qa_threshold,
                              duplicate_cfr=="False")
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

  custom_colour_countries <- lanonc_colours[seq_along(all_countries)]
  names(custom_colour_countries) <- all_countries

  plot_list[[list_label]][["meta"]][["m1"]] <- metaprop_wrap(
    dataframe = d1_filtered, subgroup = "population_country", plot_pooled = TRUE,
    sort_by_subg = TRUE, plot_study = FALSE, digits = 2,
    colour = imperial_khaki,
    width = 9500, height = 6000, resolution = 1000)

  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_population_country.pdf")),
         plot_list[[list_label]][["meta"]][["m1"]]$plot,
         width = 12, height = 10)
  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_population_country.png")),
         plot_list[[list_label]][["meta"]][["m1"]]$plot,
         width = 12, height = 10)

  plot_list[[list_label]][["meta"]][["m2"]] <- metaprop_wrap(
    dataframe = d1_filtered, subgroup = "study_midyear_cat",
                      plot_pooled = TRUE, sort_by_subg = TRUE,
                      plot_study = FALSE, digits = 2, colour = imperial_khaki,
    width = 9500, height = 7000, resolution = 1000)

  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_study_midyear_cat.pdf")),
         plot_list[[list_label]][["meta"]][["m2"]]$plot,
         width = 9, height = 7)
  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_study_midyear_cat.png")),
         plot_list[[list_label]][["meta"]][["m2"]]$plot,
         width = 9, height = 7)

  plot_list[[list_label]][["meta"]][["m3"]] <- metaprop_wrap(
    dataframe = d1_filtered, subgroup = "cfr_denom_cat", plot_pooled = TRUE, sort_by_subg = FALSE,
    plot_study = FALSE, digits = 2, colour = imperial_khaki,
    width = 9500, height = 4200, resolution = 1000)

  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_cfr_denom_cat.pdf")),
         plot_list[[list_label]][["meta"]][["m3"]]$plot,
         width = 12, height = 6)
  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_cfr_denom_cat.png")),
         plot_list[[list_label]][["meta"]][["m3"]]$plot,
         width = 12, height = 6)

  plot_list[[list_label]][["meta"]][["m4"]] <- metaprop_wrap(
    dataframe = d1_filtered, subgroup = "population_group", plot_pooled = TRUE,
    sort_by_subg = TRUE, plot_study = FALSE, digits = 2, colour = imperial_khaki,
    width = 9500, height = 4200, resolution = 1000)

  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_population_group.pdf")),
         plot_list[[list_label]][["meta"]][["m4"]]$plot,
         width = 12, height = 6)
  ggsave(file.path(plot_type,
                   paste0("figure_3_meta_population_group.png")),
         plot_list[[list_label]][["meta"]][["m4"]]$plot,
         width = 12, height = 6)

  plot_list[[list_label]][["meta"]][["m_ind"]] <- metaprop_wrap(
      d1_filtered |> arrange(desc(central)), subgroup = NA, plot_pooled = TRUE,
      plot_study = TRUE, digits = 2, colour = imperial_khaki,
      width = 9500, height = 6000, resolution = 1000)

  ggsave(file.path("all", "figure_3_meta_no_subg_extracted_cfrs.pdf"),
         plot_list[[list_label]][["meta"]][["m_ind"]]$plot,
         width = 12, height = 10)
  ggsave(file.path("all", "figure_3_meta_no_subg_extracted_cfrs.pdf"),
         plot_list[[list_label]][["meta"]][["m_ind"]]$plot,
         width = 12, height = 10)

  # Forest plot
  plot_list[[list_label]][["forest"]][["p_cfr_1"]] <- forest_plot(
    d1_filtered, "Case-Fatality Ratio (%)", "population_country",
    c(-10,110), custom_colours = custom_colour_countries,
    text_size=text_size, qa_alpha=qa_alpha, sort=TRUE) +
    guides(shape = guide_legend(title = "Parameter type", order=1),
           fill = guide_none(),
           linetype = guide_none(),
           color = guide_legend(title = "Population country", order=3))
  ggsave(file.path(plot_type,
                   paste0("figure_3_forest_cfr_population_country.pdf")),
         plot = plot_list[[list_label]][["forest"]][["p_cfr_1"]],
         width = 10, height = 12)

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

# Combine figures
p1 <- plot_list[["all"]][["meta"]][["m1"]]$plot +
  theme(plot.margin = margin(-50, -250, -250, -50))
p2 <- cfr_outbreak_ma$plot +
  theme(plot.margin = margin(-250, -250, -250, -250))
p3 <- cfr_from_bangladesh_surveillance_yc$plot +
  theme(plot.margin = margin(-50, -250, -50, -250))
p4 <- plot_list[["all"]][["forest"]][["p_cfr_1"]] +
  theme(legend.position=c(0.275, 0.775), legend.direction = "vertical")
p5 <- plot_list[["all"]][["forest"]][["p_prop_1"]] +
  guides(color = guide_none(),
         linetype = guide_none(),
         shape = guide_none())

left_col  <- p4 / p5 + plot_layout(heights = c(26, 3))
right_col <- p1 / p2 / p3 +
  plot_layout(heights = c(2.4, 2.4, 2.45))
ggsave("figure_right_col.png", plot = right_col, width = 6, height = 10, dpi=600)

patchwork <- (left_col | right_col) +
  plot_layout(widths = c(2.5, 4)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag.position = c(0, 1), plot.margin = margin(5.5, 0, 0, 5.5),
        plot.tag = element_text(size = 14))

ggsave("figure_severity_forest.png", plot = left_col, width = 8, height = 10)
ggsave("figure_severity_forest.pdf", plot = left_col, width = 8, height = 10)

ggsave("figure_severity.png", plot = patchwork, width = 11.5, height = 10, dpi=300)
ggsave("figure_severity.pdf", plot = patchwork, width = 11.5, height = 10, dpi=300)

# Additional plots for SI
#figure_S6-S10: meta-analysis with all estimates plotted
#figure_S11: meta-analysis with only known duplicates excluded
# db <- d1 |> filter(duplicate_cfr %in% c("False","Assumed"))
#figure_S12: meta-analysis without de-duplication
# dc <- d1
patchwork_si <- plot_list[["all"]][["forest"]][["p_cfr_1"]] +
  theme(legend.position=c(0.175, 0.7), legend.direction = "vertical") +
  plot_list[["all"]][["forest"]][["p_cfr_2"]] +
  theme(legend.position=c(0.225, 0.7), legend.direction = "vertical") +
  guides(shape=guide_none()) +
  plot_layout(ncol = 2) + plot_annotation(tag_levels = 'A')

ggsave("figure_SI_severity.png", plot = patchwork_si, width = 15, height = 10)
ggsave("figure_SI_severity.pdf", plot = patchwork_si, width = 15, height = 10)

