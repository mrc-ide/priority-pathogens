# *=================== Nipah severity meta-analysis & plots ===================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(grid)
library(lubridate)
library(metafor)
library(meta)
library(orderly2)
library(patchwork)
library(readr)
library(stringr)
library(tidyr)

# *--------------------------------- Orderly ----------------------------------*
pathogen <- orderly_parameters(pathogen = "NIPAH")

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "outbreaks.csv", "models.csv", "params.csv"))

orderly_shared_resource("nipah_functions.R" = "nipah_functions.R")
orderly_shared_resource("NIPAH_Bangladesh_IEDCR.csv"="NIPAH_Bangladesh_IEDCR.csv")

source("nipah_functions.R")

orderly_artefact("Nipah severity figures",
                 c(file.path("figures", "figure_severity.png"),
                   file.path("figures","figure_severity.pdf")))

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles,outbreaks,models,parameters, plotting = FALSE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

outbreaks <- dfs$outbreaks |>
  left_join(qa_scores)

parameters <- dfs$parameters |>
  left_join(qa_scores)

# *----------------------------- Data preparation -----------------------------*
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
    population_group == "Persons under investigation" ~ "Persons under investigation",
    population_group == "Persons with symptoms" ~ "Persons with symptoms",
    population_group == "Healthcare workers" ~ "Healthcare workers",
    population_group == "Abattoir workers" ~ "Animal workers",
    population_group == "Animal workers" ~ "Animal workers",
    is.na(population_group) ~ "Unspecified",
    TRUE ~ "Mixed groups")) |>
  mutate(parameter_unit = 'Percentage (%)',
         parameter_value = coalesce(parameter_value, central), #using central where no % was reported  #NOTE
         population_study_start_year = as.numeric(population_study_start_year),
         population_study_end_year = as.numeric(population_study_end_year),
         study_midyear = ifelse(
           !is.na(population_study_start_year) & !is.na(population_study_end_year),
           round((population_study_start_year + population_study_end_year) / 2),
           population_study_start_year)) |>
  mutate(study_midyear_cat = case_when(
    study_midyear %in% 1998:1999 ~ "1998-1999",
    study_midyear %in% 2000:2009 ~ "2000-2009",
    study_midyear %in% 2010:2019 ~ "2010-2019",
    study_midyear %in% 2020:2029 ~ "2020-2025",
    TRUE ~ "Unspecified")) |>
  mutate(cfr_denom_cat = case_when(
    cfr_ifr_denominator %in% 1:9      ~ "Reported Cases < 10",
    cfr_ifr_denominator %in% 10:19      ~ "Reported Cases = 10-19",
    cfr_ifr_denominator %in% 20:29      ~ "Reported Cases = 20-29",
    cfr_ifr_denominator %in% 30:99     ~ "Reported Cases = 30-99",
    cfr_ifr_denominator %in% 100:329   ~ "Reported Cases = 100-329",
    TRUE ~ "Unspecified")) |>
  # Deduplicating CFRs
  # Patson 050_003 vs. Chua 052_002
  mutate(duplicate_cfr = case_when(
    access_param_id %in% c("037_006", "039_001", "081_002",
                           "172_001", "172_002", "172_002", "052_002") ~ "Known",
    access_param_id %in% c(
      "002_001", "025_006", "030_003", "037_005", "038_001", "040_008",
      "044_002", "044_007", "052_001", "054_001", "093_001",
      "103_001", "109_001", "109_002", "109_003", "109_006", "109_004",
      "109_005", "109_007", "109_008", "109_011", "109_012", "109_013",
      "109_014", "109_015", "109_016", "109_017", "109_018", "109_019",
      "109_020", "109_021", "129_002", "121_001", "129_003", "138_004",
      "151_004", "171_004", "173_001", "172_004", "179_001", "190_001") ~
      "Assumed",
    TRUE ~ "False")) |>
  mutate(population_group = factor(population_group,
           levels = c(sort(setdiff(unique(population_group),
                                   c("Other", "Unspecified"))),
                      "Other", "Unspecified")),
         parameter_value_type = factor(
           parameter_value_type,
           levels = c(sort(setdiff(unique(parameter_value_type),
                                   "Unspecified")), "Unspecified"))) |>
  mutate(population_study_start_day = as.numeric(gsub("x+", "", population_study_start_day)),
         population_study_start_month = as.numeric(gsub("x+", "", population_study_start_month)),
         population_study_start_year = as.numeric(gsub("x+", "", population_study_start_year)),
         population_study_end_day = as.numeric(gsub("x+", "", population_study_end_day)),
         population_study_end_month = as.numeric(gsub("x+", "", population_study_end_month)),
         population_study_end_year = as.numeric(gsub("x+", "", population_study_end_year)),
         start_month_abbr = ifelse(!is.na(population_study_start_month),
                                   month.abb[population_study_start_month],
                                   NA),
         start_date = paste(population_study_start_day,
                            start_month_abbr,
                            population_study_start_year),
         start_date=na_if(start_date, "NA NA NA"),
         end_month_abbr = ifelse(!is.na(population_study_end_month),
                                 month.abb[population_study_end_month],
                                 NA),
         end_date = paste(population_study_end_day,
                          end_month_abbr,
                          population_study_end_year),
         end_date=na_if(end_date, "NA NA NA"),
         dates = case_when(
           start_date==end_date~start_date,
           !is.na(start_date) & is.na(end_date)~paste0(start_date, " - Unspecified"),
           is.na(start_date) & !is.na(end_date)~paste0("Unspecified - ", end_date),
           !is.na(start_date) & !is.na(end_date) ~ paste0(start_date, " - ", end_date),
           TRUE ~ "Unspecified"),
         dates = gsub("NA","",dates),
         dates = trimws(gsub("\\s{2,}"," ",dates)),
         dates = gsub("\\b(\\d{4})\\s*-\\s*\\1\\b","\\1",dates,perl = TRUE))

# proportion of symptomatic cases
d2 <- parameters |>
    filter(parameter_type == "Severity - proportion of symptomatic cases")

# *------------------------------ Meta-analysis -------------------------------*
# Plot file structure - many plots created in this task so better to create a
# folder structure
filepath_vec <- c(file.path("figures", "extracted_parameters", "no_dups"),
                  file.path("figures","extracted_parameters", "no_known_dups"),
                  file.path("figures", "extracted_parameters", "all"))

for (filepath in filepath_vec){
  if (!dir.exists(filepath)) {
    dir.create(filepath, recursive = TRUE)
  }
}


# Plot colour
imperial_khaki <- "#EFE58B"
imperial_blue <- rgb(0, 62 / 256, 116 / 256, 0.7)
imperial_light_blue <- "#B9EEFF"
tangerine <- "#EC7300"
crimson <- "#DC143C"

diamond_colour <-"dodgerblue3"
square_colour <- imperial_khaki

text_size <- 13
point_size <- 2.75
lanonc_colours <- ggsci::pal_lancet("lanonc")(9)
bmj_colours <- ggsci::pal_bmj("default")(9)

temp <- bmj_colours[4]
bmj_colours[4] <- bmj_colours[6]
bmj_colours[6] <- temp

temp <- bmj_colours[1]
bmj_colours[1] <- bmj_colours[2]
bmj_colours[2] <- temp

meta_digits <- 3

# Forest plot colours
all_pop_groups <- parameters |>
  filter(!is.na(population_group)) |>
  distinct(population_group) |>
  arrange(desc(population_group == "General population"),
          population_group == "Other",  population_group == "Unspecified",
          population_group) |>
  pull()

custom_colour_pop_groups <- bmj_colours[seq_along(all_pop_groups)]
names(custom_colour_pop_groups) <- all_pop_groups

unique_groups_in_plot <- levels(d1$population_group)

custom_colour_pop_groups <- (
  custom_colour_pop_groups[all_pop_groups %in% (unique_groups_in_plot)])

# Colour overlap...
all_countries <- d1 |>
  distinct(population_country) |>
  arrange(population_country == "Bangladesh; India", population_country) |>
  pull()

custom_colour_countries <- lanonc_colours[seq_along(all_countries)]
names(custom_colour_countries) <- all_countries
custom_colour_countries["Bangladesh; India"] <- lanonc_colours[9]

# *---------------------- CFR from extracted parameters -----------------------*
# Extracted CFRs
plot_list <- list("no_dups"=list("meta"=list(), "forest"=list()),
                  "no_known_dups"=list("meta"=list(), "forest"=list()),
                  "all"=list("meta"=list(), "forest"=list()))
list_label_vec <- c("no_dups", "no_known_dups", "all")
labels <- c("no_dups", "no_known_dups", "all")

cfr_duplicates <- list("no_dups"="False",
                       "no_known_dups"=c("False", "Assumed"),
                       "all"=c("False", "Assumed", "Known"))

qa_threshold <- -1
qa_alpha <- 0.3

for (i in seq_along(labels)){
  # Number of cases reported not shown since, in some cases, multiple outbreaks
  # are captured by the same study
  if (i == 3){
    add_save_height <- 4.5
    add_png_height <- 10000
    less_height <- 0
  }else if(i==2){
    add_save_height <- 3.5
    add_png_height <- 8000
    less_height <- 0
  }else{
    add_save_height <- 0
    add_png_height <- 0
    less_height <- -500
  }

  list_label <- list_label_vec[i]
  cfr_duplicate <- cfr_duplicates[[i]]
  plot_type <- file.path("figures", "extracted_parameters", list_label)

  d1_filtered <- d1 |>
    filter(duplicate_cfr %in% cfr_duplicate) |>
    arrange(central)

  if (list_label!="all"){
  # Colours:
  plot_list[[list_label]][["meta"]][["m1"]] <- metaprop_wrap(
    dataframe = d1_filtered, subgroup = "population_country", plot_pooled = TRUE,
    sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
    colour = diamond_colour, colour_square = square_colour,
    width = 10500, height = 9000+add_png_height, resolution = 1000)

  ggsave(file.path(plot_type,
                   paste0("CFR_meta_population_country_",list_label,".pdf")),
         plot_list[[list_label]][["meta"]][["m1"]]$plot+
           theme(plot.margin = margin(-1, -1, -1, -1)),
         width = 5.5, height = 5+add_save_height)

  plot_list[[list_label]][["meta"]][["m2"]]  <- metaprop_wrap(
    dataframe = d1_filtered, subgroup = "study_midyear_cat",
    plot_pooled = TRUE, sort_by_subg = TRUE,
    plot_study = TRUE, digits = meta_digits,
    colour = diamond_colour, colour_square = square_colour,
    width = 10500, height = 9000, resolution = 1000)

  study_midyear_cat_custom <- plot_list[[list_label]][["meta"]][["m2"]]

  # Update plot to allow for new format
  d1_filtered$dates <- gsub(" - Unspecified", "", d1_filtered$dates)

  # Remove day numbers before months
  d1_filtered$dates <- gsub("\\b\\d{1,2}\\s+", "", d1_filtered$dates)

  if (list_label=="no_known_dups"){
    d1_problematic_rows <- d1_filtered |>
      filter(!is.na(cfr_ifr_denominator))
  }else{
    d1_problematic_rows <- d1_filtered
  }

  study_midyear_cat_custom$result$article_refs <- d1_problematic_rows$refs
  study_midyear_cat_custom$result$studlab <- d1_problematic_rows$dates


  png(file = "temp.png", width = 13250, height = 9000+add_png_height+less_height,
      res = 1000)

  par(mar = c(2, 2, 2, 1))
  forest(study_midyear_cat_custom$result, layout = "Revman5",
         leftcols = c("studlab", "article_refs", "event", "n", "effect.ci"),
         leftlabs = c("Study period", "Study", "Events", "Total",
                      ""),
         colgap.forest.left = "8mm",
         colgap.left = "16mm",
         smlab = "GLMM, Fixed + Random, 95% CI",
         smlab.pos = -0.1,
         # just.addcols = "left",
         overall = TRUE, pooled.events = TRUE,
         print.subgroup.name = FALSE, sort.subgroup = TRUE,
         study.results = TRUE,
         digits = 3,
         col.diamond.lines = "black",
         col.diamond.common = diamond_colour,
         col.diamond.random = diamond_colour,
         col.square = square_colour,
         col.square.lines = "black",
         col.study = "black", col.subgroup = "black",
         col.inside = "black", weight.study = "same",
         at = seq(0,1,by=0.2), xlim = c(0,1), xlab="Case Fatality Ratio",
         fs.predict.labels = 11.5,
         fs.hetstat=11,
         fs.test.subgroup = 11,
         fs.axis = 11,
         fontsize = 14,
         comb.random = TRUE,
         squaresize = 0.85,
         plotwidth = "72.5mm")
  dev.off()

  pg <- png::readPNG("temp.png", native = TRUE)
  file.remove("temp.png")
  plot_list[[list_label]][["meta"]][["m2"]]$plot <- wrap_elements(
    plot = rasterGrob(pg, interpolate = TRUE))
  ggsave(file.path(plot_type,
                   paste0("CFR_meta_study_midyear_cat_",list_label,".pdf")),
         plot_list[[list_label]][["meta"]][["m2"]]$plot+
           theme(plot.margin = margin(-1, -1, -1, -1)),
         width = 6, height = 4+add_save_height)

  plot_list[[list_label]][["meta"]][["m3"]] <- metaprop_wrap(
    dataframe = d1_filtered, subgroup = "population_group",
    plot_pooled = TRUE,
    sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
    colour = diamond_colour, colour_square = square_colour,
    width = 10500, height = 9000+add_png_height, resolution = 1000)

  if (i==1){
    h=4.75
  }else{
    h=5.25
  }
  ggsave(file.path(plot_type,
                   paste0("CFR_meta_population_group_",list_label,".pdf")),
         plot_list[[list_label]][["meta"]][["m3"]]$plot+
           theme(plot.margin = margin(-1, -1, -1, -1)),
         width = 5.5, height = h+add_save_height)
  }

  # Forest plot
  plot_list[[list_label]][["forest"]][["p_cfr_1"]] <- forest_plot(
    d1_filtered, "Case-Fatality Ratio (%)", "population_country",
    c(-10,110), custom_colours = custom_colour_countries,
    text_size=text_size, qa_alpha=qa_alpha, sort=TRUE,
    point_size=point_size) +
    guides(shape = guide_legend(title = "Parameter type", order=1),
           fill = guide_none(),
           linetype = guide_none(),
           color = guide_legend(title = "Population country", order=3))
  ggsave(file.path(plot_type,
                   paste0("forest_cfr_population_country_",list_label,".pdf")),
         plot = plot_list[[list_label]][["forest"]][["p_cfr_1"]],
         width = 10, height = 8+add_save_height)

  plot_list[[list_label]][["forest"]][["p_cfr_2"]] <- forest_plot(
    d1_filtered, "Case-Fatality Ratio (%)","population_group",
    c(-10,110), custom_colours = custom_colour_pop_groups,
    text_size=text_size, qa_alpha=qa_alpha, sort=TRUE,
    point_size=point_size) +
    ggforce::facet_col(facets = vars(population_country),
                       scales = "free_y",
                       space = "free") +
    guides(shape = guide_legend(title = "Parameter type", order=1),
           fill =  guide_none(),
           linetype = guide_none(),
           color =  guide_legend(title = "Population group", order=2)) +
    theme(legend.position=c(0.2, 0.8575),
          legend.direction = "vertical",
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          legend.spacing=unit(2, "mm"),
          legend.key.height = unit(0.5, "cm"),
          legend.margin=margin(0, 0, 0, 0))

  ggsave(file.path(plot_type,
                   paste0("forest_cfr_population_group_",list_label,".pdf")),
         plot =  plot_list[[list_label]][["forest"]][["p_cfr_2"]],
         width = 7, height = 8+add_save_height)
}

all_forest_cfr_2 <- forest_plot(
  d1 |> arrange(central), "Case-Fatality Ratio (%)","population_group",
  c(-10,110), custom_colours = custom_colour_pop_groups,
  text_size=16, qa_alpha=qa_alpha, sort=TRUE,
  point_size=point_size) +
  ggforce::facet_col(facets = vars(population_country),
                     scales = "free_y",
                     space = "free") +
  guides(shape = guide_legend(title = "Parameter type", order=1),
         fill =  guide_none(),
         linetype = guide_none(),
         color =  guide_legend(title = "Population group", order=2)) +
  theme(legend.position=c(0.2, 0.875),
        legend.direction = "vertical")
        # legend.text = element_text(size = 9),
        # legend.title = element_text(size = 10),
        # legend.spacing=unit(2, "mm"),
        # legend.key.height = unit(0.5, "cm"),
        # legend.margin=margin(0, 0, 0, 0))

ggsave(file.path("figures", "extracted_parameters", "all",
                 paste0("forest_cfr_population_group_all.pdf")),
       plot = all_forest_cfr_2,
       width = 10, height = 15)

  # Prop symptomatic forest plots
# Population group is all other so only plotting by country
forest_prop_symp_country <- forest_plot(
  d2, "Symptomatic Cases (%)", "population_country",
  c(-10, 110), custom_colours = custom_colour_countries,
  text_size=text_size, qa_alpha=qa_alpha, sort=TRUE,
  point_size=point_size) +
  guides(color = guide_legend(title = "Population country", order=2),
         linetype = guide_none(),
         shape = guide_legend(title = "Parameter type", order=1))

ggsave(file.path("figures","extracted_parameters",
                 paste0("forest_prop_symp_country.pdf")),
       plot =  forest_prop_symp_country,
       width = 8, height = 5)

# ------------------------------------------------------------------------------
# Updated severity plot
meta_country_result_to_format <- plot_list[["no_dups"]][["meta"]][["m1"]]$result
png(file = "temp.png", width = 13000, height = 15000, res = 1000)
par(mar = c(2, 2, 2, 1))
p1 <- forest(
  meta_country_result_to_format, layout = "Revman5",
  colgap.forest.left = "8mm",
  colgap.left = "16mm",
  leftcols = c("studlab", "event", "n", "effect.ci"),
  leftlabs = c("Outbreak Country, Study", "Events", "Total", ""),
  smlab = "GLMM, Fixed + Random, 95% CI",
  smlab.pos = -0.1,
  overall = TRUE, pooled.events = TRUE,
  print.subgroup.name = FALSE, sort.subgroup = TRUE,
  study.results = TRUE,
  digits = 3,
  col.diamond.lines = "black",
  col.diamond.common = diamond_colour,
  col.diamond.random = diamond_colour,
  col.square = square_colour, col.square.lines = "black",
  col.study = "black", col.subgroup = "black",
  col.inside = "black", weight.study = "same",
  at = seq(0,1,by=0.2), xlim = c(0,1), xlab="Case Fatality Ratio",
  fs.predict.labels = 14.5,
  fs.hetstat=14,
  fs.test.subgroup = 14,
  fs.axis = 14,
  fontsize = 19,
  spacing=1.65,
  comb.random = TRUE,
  squaresize = 0.85,
  plotwidth = "72.5mm")
dev.off()

pg <- png::readPNG("temp.png", native = TRUE)
file.remove("temp.png")
p1 <- wrap_elements(
  plot = rasterGrob(pg, interpolate = TRUE))


p4 <- plot_list[["no_dups"]][["forest"]][["p_cfr_1"]] +
  # theme(legend.position="top",
  #       legend.box = "vertical",
  #       legend.text = element_text(size = 9),
  #       legend.title = element_text(size = 10),
  #       legend.key.height = unit(0.1, "cm"),
  #       # legend.box.just = "left",
  #       legend.justification = c(1, 0)) +
  # guides(color = guide_legend(title="Population group",
  #                             nrow = 2)) +
  theme(legend.direction = "vertical",
        # legend.position=c(0.3, 0.83),
        legend.position=c(0.22, 0.51),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.spacing=unit(2, "mm"),
        legend.key.height = unit(0.5, "cm"),
        legend.margin=margin(0, 0, 0, 0)) +
  ggforce::facet_col(facets = vars(population_group),
                     scales = "free_y",
                     space = "free")
p5 <- forest_prop_symp_country +
  guides(color = guide_none(),
          color = guide_legend(title="Country"),
         linetype = guide_none(),
         shape = guide_none())
  # theme(legend.position="top",
  #       # legend.box.just = "left",
  #       # legend.justification = c(-1, 0),
  #       legend.text = element_text(size = 9),
  #       legend.title = element_text(size = 10))
  # theme(legend.position=c(0.2, 0.5),
  #       legend.direction = "vertical",
  #       legend.text = element_text(size = 9),
  #       legend.title = element_text(size = 10),
  #       legend.key.height = unit(0.5, "cm"),
  #       legend.margin=margin(0, 0, 0, 0))

left_col  <-  p5/ plot_spacer() / p4  + plot_layout(heights = c(2.8, 0.15, 18))
right_col <- p1

patchwork_new <- (left_col | plot_spacer() | right_col) +
  plot_layout(widths = c(2.6, 0.15, 5)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag.position = "topleft",
        plot.margin = margin(5.5, 0, 0, 5.5),
        plot.tag = element_text(size = 16))

ggsave(file.path("figures", "figure_severity.png"),
       plot = patchwork_new, width = 12.5, height = 8, dpi=300)
ggsave(file.path("figures", "figure_severity.pdf"),
       plot = patchwork_new, width = 12.5, height = 8)

ggsave(file.path("figures", "figure_3_panel_C.pdf"),
       plot = p1, width = 7.25, height = 8)
ggsave(file.path("figures", "figure_3_panel_B.pdf"),
       plot = p4 +theme(legend.position=c(0.17, 0.51)), width = 6, height = 7)
ggsave(file.path("figures", "figure_3_panel_A.pdf"),
       plot = p5, width = 6, height = 1.75)


# -------------- Funnel plot
png("dedup_cfr_funnel_plot.png", width = 3000, height = 2000, res = 300)
par(mar = c(4, 4, 1, 1))
funnel(plot_list[["no_dups"]][["meta"]][["m1"]]$result,
       common = FALSE,
       pch = 22,
       cex=1.4,
       bg = imperial_khaki,
       level = 0.95,
       studlab = FALSE,
       ylim=c(1.52, 0),
       xlim=c(-2.6, 3.5))

TE   <- plot_list[["no_dups"]][["meta"]][["m1"]]$result$TE
seTE <- plot_list[["no_dups"]][["meta"]][["m1"]]$result$seTE
lab  <- plot_list[["no_dups"]][["meta"]][["m1"]]$result$studlab

seTE_j <- seTE
set.seed(1)
seTE_j[14] <- jitter(seTE[14], amount = 0.2)
seTE_j[10] <- jitter(seTE[10], amount = 0.4)
seTE_j[15] <- seTE[15]-0.09

text(TE, seTE_j, labels = lab, cex = 0.9, pos = 1)
dev.off()

# ------------ Dedup timeline plot
# Helper to get dates:
fix_to_date <- function(x, which = c("start","end")) {
  which <- match.arg(which)
  x <- str_squish(x)
  x[x %in% c("", "NA")] <- NA_character_

  year_to_date <- function(yr) {
    if (which == "start") as.Date(sprintf("%04d-01-01", yr))
    else                 as.Date(sprintf("%04d-12-31", yr))
  }

  out <- rep(NA_Date_, length(x))
  ok  <- !is.na(x)
  if (!any(ok)) return(out)

  xs <- x[ok]

  # 1) year-only: "2007" OR "NA NA 2007"
  is_year_only <- str_detect(xs, "^\\d{4}$")
  is_na_na_yr  <- str_detect(xs, "^NA\\s+NA\\s+\\d{4}$")
  yr_idx <- is_year_only | is_na_na_yr
  if (any(yr_idx)) {
    yrs <- as.integer(str_extract(xs[yr_idx], "\\d{4}$"))
    out[ok][yr_idx] <- year_to_date(yrs)
  }

  # 2) month-year: "Mar 2007" OR "NA Mar 2007"
  is_mon_year <- str_detect(xs, "^(?:NA\\s+)?[A-Za-z]{3,9}\\s+\\d{4}$")
  if (any(is_mon_year)) {
    mon_year_str <- str_replace(xs[is_mon_year], "^NA\\s+", "")  # drop leading NA
    start_of_month <- suppressWarnings(dmy(str_c("1 ", mon_year_str)))

    out[ok][is_mon_year] <-
      if (which == "start") start_of_month
    else (ceiling_date(start_of_month, "month") - days(1))
  }

  # 3) full date: "9 Apr 2007"
  is_full <- str_detect(xs, "^(?:NA\\s+)?\\d{1,2}\\s+[A-Za-z]{3,9}\\s+\\d{4}$")
  if (any(is_full)) {
    full_str <- xs[is_full]
    out[ok][is_full] <- suppressWarnings(dmy(full_str))
  }

  out
}

d1_plot <- d1 |>
  mutate(population_country = factor(population_country,
                                     levels=c( "Philippines", "India", "Bangladesh",
                                               "Bangladesh; India",
                                               "Singapore", "Malaysia")),
         duplicate_cfr = factor(duplicate_cfr,
                                levels=c("False", "Assumed", "Known")),
         population_study_start_year=coalesce(population_study_start_year,
                                              population_study_end_year),
         urefs = make.unique(refs)) |>
  filter(!is.na(population_study_start_year)) |>
  arrange(population_country, population_study_start_year, urefs) |>
  mutate(urefs = factor(urefs, levels = rev(unique(urefs))))

d1_plot <- d1_plot  |>
  mutate(
    start_date = fix_to_date(start_date, "start"),
    end_date   = fix_to_date(end_date,   "end")
  )

deduplication_timeline_plot <- d1_plot |>
  ggplot(aes(colour = population_group, fill=population_group,
             shape= duplicate_cfr, alpha = duplicate_cfr)) +
  geom_point(aes(x=start_date, y=urefs, size=duplicate_cfr)) +
  geom_segment(aes(x=start_date, xend=end_date, y = urefs, yend = urefs),
               show.legend = NA, size=2) +
  ggforce::facet_col(facets = vars(population_country),
                     scales = "free_y", space = "free") +
  scale_alpha_manual(values = c(False = 0.9, Assumed = 0.4, Known = 0.4),
                     name = "Duplicate") +
  scale_shape_manual(values = c(False = 21, Assumed = 24, Known = 23),
                     name = "Duplicate") +
  scale_size_manual(values = c(False = 2.5, Assumed = 2.5, Known = 2.5),
                    name = "Duplicate") +
  scale_color_manual(name="Population group",
                     values = custom_colour_pop_groups,
                     breaks = sort(levels(d1_plot$population_group))) +
  scale_fill_manual(name="Population group",
                    values = custom_colour_pop_groups,
                    breaks = sort(levels(d1_plot$population_group))) +
  scale_y_discrete(expand = expansion(add = 0.6),
                   labels = setNames(d1_plot$refs, d1_plot$urefs)) +
  guides(colour = guide_legend(override.aes = list(shape = NA)),
         alpha=guide_legend(override.aes = list(linetype = 0, linewidth = 0,
                                                fill="black"))) +
  labs(x="Study year", y="") +
  scale_x_date(breaks = seq(from = as.Date("2000-01-01"),
                            to   = as.Date("2030-01-01"),
                            by   = "5 years"),
               date_labels = "%Y") +
  coord_cartesian(xlim = as.Date(c("1999-01-01", "2025-01-01"))) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
        text = element_text(size = 15),
        legend.position = c(0.82,  0.085),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.spacing=unit(2, "mm"),
        legend.key.height = unit(0.3, "cm"),
        legend.margin=margin(0, 0, 0, 0))

ggsave(file.path("figures", "deduplication_timeline_plot.pdf"),
       deduplication_timeline_plot, width = 8, height = 11)

# By sample size:
breaks <- c(1, 12, 20, 28, 60, 100, 180, 200, 250, 300, 325)
step_cols <- c(
  "red",
  "#ff7034",
  "#FFD801",
  "#EE82EE",
  "#7F00FF",
  "#00BFFF",
  "#003E74",
  "#006400",
  "#80EF80"
)
deduplication_timeline_plot <- d1_plot |>
  mutate(cases = coalesce(cfr_ifr_denominator, population_sample_size),
         cases_cat = case_when(
           cases %in% 5:11      ~ "[5, 12)",
           cases %in% 12:26     ~ "[12, 27)",
           cases %in% 27:59   ~ "[27, 60)",
           cases %in% 60:139   ~ "[60, 140)",
           cases %in% 140:325   ~ "[140, 325)",
           TRUE ~ "Unspecified")) |>
  ggplot(aes(colour = cases, fill=cases,
             shape= duplicate_cfr, alpha = duplicate_cfr)) +
  geom_point(aes(x=start_date, y=urefs, size=duplicate_cfr)) +
  geom_segment(aes(x=start_date, xend=end_date, y = urefs, yend = urefs),
               show.legend = NA, size=2) +
  ggforce::facet_col(facets = vars(population_country),
                     scales = "free_y", space = "free") +
  scale_fill_gradientn(
    colours = step_cols, values  = scales::rescale(breaks),
    breaks = breaks, limits  = range(breaks), name = "Sample size") +
  scale_color_gradientn(
    colours = step_cols, values  = scales::rescale(breaks),
    breaks = breaks, limits  = range(breaks), name = "Sample size") +
  scale_alpha_manual(values = c(False = 0.9, Assumed = 0.4, Known = 0.4),
                     name = "Duplicate") +
  scale_shape_manual(values = c(False = 21, Assumed = 24, Known = 23),
                     name = "Duplicate") +
  scale_size_manual(values = c(False = 2.5, Assumed = 2.5, Known = 2.5),
                    name = "Duplicate")+
  # scale_color_stepsn(
  #   breaks  = breaks,
  #   limits  = range(breaks),
  #   colours = lanonc_colours,
  #   name = "Sample size"
  # ) +
  # scale_fill_stepsn(
  #   breaks  = breaks,
  #   limits  = range(breaks),
  #   colours = lanonc_colours,
  #   name = "Sample size"
  # ) +
  # scale_color_manual(name="Population group",
  #                    values = custom_colour_pop_groups,
  #                    breaks = sort(levels(d1_plot$population_group))) +
  # scale_fill_manual(name="Population group",
  #                   values = custom_colour_pop_groups,
  #                   breaks = sort(levels(d1_plot$population_group))) +
  scale_y_discrete(expand = expansion(add = 0.6),
                   labels = setNames(d1_plot$refs, d1_plot$urefs)) +
  labs(x="Study year", y="") +
  scale_x_date(breaks = seq(from = as.Date("2000-01-01"),
                            to   = as.Date("2030-01-01"),
                            by   = "5 years"),
               date_labels = "%Y") +
  coord_cartesian(xlim = as.Date(c("1999-01-01", "2025-01-01"))) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
        text = element_text(size = 15),
        legend.direction = "horizontal",
        # legend.position = c(0.7,  0.085),
        legend.position = "top",
        legend.box = "vertical",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 11),
        legend.spacing=unit(2, "mm"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(3.25, "cm"),
        legend.justification = c(1.25, 0),
        legend.margin=margin(0, 0, 0, 0)) +
  guides(shape=guide_legend(order=1),
         size=guide_legend(order=1),
         alpha=guide_legend(override.aes = list(linetype = 0, linewidth = 0,
                                                fill="black"),
                            order=1))

ggsave(file.path("figures", "deduplication_timeline_plot_v2.pdf"),
       deduplication_timeline_plot, width = 8, height = 11)

# ------------------------------------------------------------------------------
# Kerala vs. West Bengal + misc analysis
d1 |> filter(population_country=="India", duplicate_cfr=="False") |>
  select(central, population_study_start_year, population_location,
         cfr_ifr_denominator, cfr_ifr_numerator) |>
  mutate(population_location=case_when(population_location=="Kozhikode; Kerala"~"West Bengal",
                                       population_location=="Kozhikode"~"Kerala",
                                       population_location=="Siliguri; West Bengal"~"West Bengal",
                                       population_location=="Siliguri"~"West Bengal",
                                       population_location=="Nadia"~"West Bengal",
                                       TRUE~population_location)) |>
  group_by(population_location) |>
  summarise(cfr=mean(central),
            cfr_ifr_denominator=mean(cfr_ifr_denominator, na.rm=T),
            cfr_ifr_numerator=mean(cfr_ifr_numerator, na.rm=T))


d1 |>
  filter(population_country == "India", duplicate_cfr == "False") |>
  select(central, population_study_start_year, population_location,
         cfr_ifr_denominator, cfr_ifr_numerator) |>
  mutate(population_location = case_when(
    population_location == "Kozhikode; Kerala" ~ "West Bengal",
    population_location == "Kozhikode" ~ "Kerala",
    population_location == "Siliguri; West Bengal" ~ "West Bengal",
    population_location == "Siliguri" ~ "West Bengal",
    population_location == "Nadia" ~ "West Bengal",
    TRUE ~ population_location
  )) |>
  group_by(population_location) |>
  summarise(
    cfr_ifr_denominator = sum(cfr_ifr_denominator, na.rm = TRUE),
    cfr_ifr_numerator   = sum(cfr_ifr_numerator, na.rm = TRUE),
    cfr = cfr_ifr_numerator / cfr_ifr_denominator,
    ci_low  = binom.test(cfr_ifr_numerator, cfr_ifr_denominator)$conf.int[1],
    ci_high = binom.test(cfr_ifr_numerator, cfr_ifr_denominator)$conf.int[2],
    .groups = "drop"
  )

d1 |> filter(duplicate_cfr=="False", population_country=="India") |>
  select(central, population_study_start_year, population_location,
         cfr_ifr_denominator, cfr_ifr_numerator, population_group)


d1 |> filter(duplicate_cfr=="False") |>
  select(central, population_study_start_year, population_study_end_year,
         population_country, cfr_ifr_denominator, cfr_ifr_numerator) |>
  group_by(population_country, population_study_start_year, population_study_end_year) |>
  summarise(cfr=mean(central),
            cfr_ifr_denominator=mean(cfr_ifr_denominator, na.rm=T),
            cfr_ifr_numerator=mean(cfr_ifr_numerator, na.rm=T))
# --------------------

