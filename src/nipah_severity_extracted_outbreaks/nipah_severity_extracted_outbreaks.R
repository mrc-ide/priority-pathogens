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
orderly_shared_resource("cleaned_outbreak_data.RDS"="cleaned_outbreak_data.RDS")

source("nipah_functions.R")

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
  mutate(cfr_ifr_denominator = total_cases,
         cfr_ifr_numerator = deaths,
         CFR = cfr_ifr_numerator / cfr_ifr_denominator,
         article_refs = refs,
         refs = paste(outbreak_country, outbreak_location, sep = " |> "),
         parameter_value = CFR,
         parameter_unit = 'Percentage') |> arrange(desc(CFR)) |>
  mutate(outbreak_source = case_when(str_detect(outbreak_source, 'Domestic animal' ) ~ 'Domestic animal',
                                     str_detect(outbreak_source, 'Wild animal' ) ~ 'Wild animal',
                                     TRUE ~ outbreak_source),
         refs_full = refs,
         refs = paste0(
           iso_lookup[trimws(str_extract(refs, "^[^|]+"))], " |> ",
           # first 3 letters of each word in location
           str_to_upper(str_replace_all(str_extract(refs, "(?<=\\|>).*$"),
                                        "\\b([A-Za-z]{1,3})[A-Za-z]*\\b",
                                        "\\1")))
  )

# *------------------------------ Meta-analysis -------------------------------*
# Plot file structure - many plots created in this task so better to create a
# folder structure
dir.create(file.path("figures"))


# Plot colour
imperial_khaki <- "#EFE58B"
imperial_blue <- rgb(0, 62 / 256, 116 / 256, 0.7)
imperial_light_blue <- "#B9EEFF"
tangerine <- "#EC7300"
crimson <- "#DC143C"

diamond_colour <-"dodgerblue3"
square_colour <- imperial_khaki

text_size <- 13
lanonc_colours <- ggsci::pal_lancet("lanonc")(9)
meta_digits <- 3

# Colour overlap...
all_countries <- unique(cfr_from_outbreaks$outbreak_country)
custom_colour_countries <- lanonc_colours[seq_along(all_countries)]
names(custom_colour_countries) <- all_countries

# *----------------------------------------------------------------------------*
# *----------------------- CFR from extracted outbreaks -----------------------*
# *----------------------------------------------------------------------------*
# With distinct locations
# Overall
cfr_outbreak_country <- metaprop_wrap(
  cfr_from_outbreaks, subgroup = 'outbreak_country', plot_pooled = TRUE,
  sort_by_subg = TRUE, plot_study = FALSE, digits = meta_digits,
  colour = diamond_colour, colour_square = square_colour,
  width = 9500, height = 6000, resolution = 1000)

ggsave(file.path("figures",
                 "CFR_extracted_outbreak_country_ind_locs_overall.pdf"),
       cfr_outbreak_country$plot, width = 9, height = 6)

# With study breakdown
cfr_outbreak_country_study <- metaprop_wrap(
  cfr_from_outbreaks,
  subgroup = 'outbreak_country', plot_pooled = TRUE,
  sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
  colour = diamond_colour, colour_square = square_colour,
  width = 11000, height = 17000, resolution = 1000)

# Update plot to allow for new format
cfr_outbreak_country_study$result$article_refs <- cfr_from_outbreaks$article_refs
cfr_outbreak_country_study$result$studlab <- paste0(
  cfr_from_outbreaks$outbreak_start_year, ", ", cfr_from_outbreaks$outbreak_location)

png(file = "temp.png", width = 13000, height = 17000, res = 1000)
par(mar = c(2, 2, 2, 1))
cfr_outbreak_country_study_plot <- forest(
  cfr_outbreak_country_study$result, layout = "Revman5",
  leftcols = c("studlab", "article_refs", "event", "n", "effect.ci"),
  leftlabs = c("Outbreak", "Study", "Events", "Total", "GLMM, Fixed + Random, 95% CI"),
  just.addcols = "left",
  colgap.forest.left = "3mm",
  overall = TRUE, pooled.events = TRUE,
  print.subgroup.name = FALSE, sort.subgroup = TRUE,
  study.results = TRUE,
  digits = 2,
  col.diamond.lines = "black",
  col.diamond.common = diamond_colour,
  col.diamond.random = diamond_colour,
  col.square = square_colour, col.square.lines = "black",
  col.study = "black", col.subgroup = "black",
  col.inside = "black", weight.study = "same",
  at = seq(0,1,by=0.2), xlim = c(0,1), xlab="Case Fatality Ratio",
  fs.predict.labels = 11.5,
  fs.hetstat=11,
  fs.test.subgroup = 11,
  fs.axis = 11,
  fontsize = 14,
  plotwidth = "72.5mm")
dev.off()

pg <- png::readPNG("temp.png", native = TRUE)
file.remove("temp.png")
cfr_outbreak_country_study_plot <- wrap_elements(
  plot = rasterGrob(pg, interpolate = TRUE))

ggsave(file.path("figures",
                 "CFR_extracted_outbreak_country_ind_locs_study.pdf"),
       cfr_outbreak_country_study_plot,width = 5, height = 6.5)

# --------------- REDUCED
# Issue with rounding....
# Create a new dataframe of outbreak by country by year

# Overall
cfr_from_outbreaks_reduced <- cfr_from_outbreaks |>
  group_by(outbreak_country, outbreak_start_year, parameter_unit, article_refs) |>
  summarise(cfr_ifr_denominator=sum(total_cases),
            cfr_ifr_numerator=sum(deaths),
            parameter_value=cfr_ifr_numerator/cfr_ifr_denominator,
            outbreak_location=paste(outbreak_location, collapse=","),
            outbreak_start_year=unique(outbreak_start_year)) |>
  mutate(outbreak_period = case_when(
    outbreak_start_year %in% 1990:1999 ~ "1998-2000",
    outbreak_start_year %in% 2001:2005 ~ "2001-2005",
    outbreak_start_year %in% 2006:2010 ~ "2006-2010",
    outbreak_start_year %in% 2011:2015 ~ "2011-2015",
    outbreak_start_year %in% 2016:2020 ~ "2016-2020",
    outbreak_start_year %in% 2021:2029 ~ "2021-Present",
    TRUE ~ "Unspecified"),
    outbreak_size = case_when(
    cfr_ifr_denominator %in% 1:9      ~ "Reported Cases < 10",
    cfr_ifr_denominator %in% 10:19      ~ "Reported Cases = 10-19",
    cfr_ifr_denominator %in% 20:49      ~ "Reported Cases = 20-49",
    cfr_ifr_denominator %in% 50:99     ~ "Reported Cases = 50-99",
    cfr_ifr_denominator %in% 100:329   ~ "Reported Cases = 100-329",
    TRUE ~ "Unspecified"),
    outbreak_size=factor(outbreak_size,
                         levels=c("Reported Cases < 10",
                                  "Reported Cases = 10-19",
                                  "Reported Cases = 20-49",
                                  "Reported Cases = 50-99",
                                  "Reported Cases = 100-329")),
    refs=article_refs)

# Plots
# By country
cfr_outbreak_country <- metaprop_wrap(
  cfr_from_outbreaks_reduced, subgroup = "outbreak_country", plot_pooled = TRUE,
  sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
  colour = diamond_colour, colour_square = square_colour,
  width = 11000, height = 11000, resolution = 1000)


forest(
  cfr_outbreak_country$result, layout = "Revman5",
  leftcols = c("studlab", "event", "n", "effect.ci"),
  leftlabs = c("Study", "Events", "Total",
               "GLMM, Fixed + Random, 95% CI"),
  just.addcols = "left",
  colgap.forest.left = "3mm",
  overall = TRUE,
  pooled.events = TRUE,
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
  fs.predict.labels = 11.5,
  fs.hetstat=11,
  fs.test.subgroup = 11,
  fs.axis = 11,
  fontsize = 14,
  plotwidth = "72.5mm")

ggsave(file.path("figures",
                 "CFR_extracted_outbreak_country_reduced_study.pdf"),
       cfr_outbreak_country$plot, width = 11, height = 11)

# Outbreak size
cfr_outbreak_size <- metaprop_wrap(
  cfr_from_outbreaks_reduced, subgroup = 'outbreak_size', plot_pooled = TRUE,
  sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
  colour = diamond_colour, colour_square = square_colour,
  width = 11000, height = 11000, resolution = 1000)

ggsave(file.path("figures",
                 "CFR_extracted_outbreak_size_reduced_study.pdf"),
       cfr_outbreak_size$plot, width = 11, height = 11)

# Outbreak period
cfr_outbreak_period <- metaprop_wrap(
  cfr_from_outbreaks_reduced, subgroup = 'outbreak_period', plot_pooled = TRUE,
  sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
  colour = diamond_colour, colour_square = square_colour,
  width = 11000, height = 12000, resolution = 1000)

# Update plot to allow for new format
cfr_outbreak_period$result$article_refs <- cfr_from_outbreaks_reduced$article_refs
cfr_outbreak_period$result$studlab <- paste0(cfr_from_outbreaks_reduced$outbreak_start_year)

png(file = "temp.png", width = 13000, height = 12000, res = 1000)
par(mar = c(2, 2, 2, 1))
cfr_outbreak_period_plot <- forest(
  cfr_outbreak_period$result, layout = "Revman5",
  leftcols = c("studlab", "article_refs", "event", "n", "effect.ci"),
  leftlabs = c("Outbreak year", "Study", "Events", "Total",
               "GLMM, Fixed + Random, 95% CI"),
  just.addcols = "left",
  colgap.forest.left = "3mm",
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
  fs.predict.labels = 11.5,
  fs.hetstat=11,
  fs.test.subgroup = 11,
  fs.axis = 11,
  fontsize = 14,
  plotwidth = "72.5mm")
dev.off()

pg <- png::readPNG("temp.png", native = TRUE)
file.remove("temp.png")
cfr_outbreak_period_plot <- wrap_elements(
  plot = rasterGrob(pg, interpolate = TRUE))

ggsave(file.path("figures",
                 "CFR_extracted_outbreak_period_reduced_study.pdf"),
       cfr_outbreak_period_plot, width = 4, height = 4)

# Alternative
dedup_outbreak_alternative <- cfr_from_outbreaks_reduced |>
  ungroup() |>
  group_by(outbreak_start_year, outbreak_country) |>
  summarise(cfr_ifr_denominator=sum(cfr_ifr_denominator),
            cfr_ifr_numerator=sum(cfr_ifr_numerator),
            cfr=cfr_ifr_numerator/cfr_ifr_denominator) |>
  mutate(outbreak_size = case_when(
    cfr_ifr_denominator %in% 1:9      ~ "Reported Cases < 10",
    cfr_ifr_denominator %in% 10:19      ~ "Reported Cases = 10-19",
    cfr_ifr_denominator %in% 20:49      ~ "Reported Cases = 20-49",
    cfr_ifr_denominator %in% 50:99     ~ "Reported Cases = 50-99",
    cfr_ifr_denominator %in% 100:329   ~ "Reported Cases = 100-329",
    TRUE ~ "Unspecified"),
    outbreak_size=factor(outbreak_size,
                         levels=c("Reported Cases < 10",
                                  "Reported Cases = 10-19",
                                  "Reported Cases = 20-49",
                                  "Reported Cases = 50-99",
                                  "Reported Cases = 100-329"))) |>
  ggplot(aes(x=outbreak_start_year, y=cfr, color=outbreak_country)) +
  geom_point() +
  ggforce::facet_col(facets = vars(outbreak_size),
                     space = "free") +
  scale_color_manual(name="Country",
                     values = custom_colour_countries) +
  theme_minimal() +
  guides(colour = guide_legend(title="Country")) +
  labs(x="Study year", y="")  +
  theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
        text = element_text(size = 15),
        legend.position = c(0.82,  0.085),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.spacing=unit(2, "mm"),
        legend.key.height = unit(0.3, "cm"),
        legend.margin=margin(0, 0, 0, 0))

ggsave(file.path("figures",
                 "dedup_outbreak_alternative.pdf"),
       dedup_outbreak_alternative, width = 6, height = 10)

# -------------------------
png("dedup_outbreak_cfr_funnel_plot.png", width = 3000, height = 2000, res = 300)
par(mar = c(4, 4, 1, 1))
funnel(cfr_outbreak_country$result,
       common = FALSE,
       pch = 22,
       cex=1.4,
       bg = imperial_khaki,
       level = 0.95,
       studlab = FALSE,
       ylim=c(1.52, 0),
       xlim=c(-2.6, 3.5))

TE   <- cfr_outbreak_country$result$TE
seTE <- cfr_outbreak_country$result$seTE
lab  <- cfr_outbreak_country$result$studlab

seTE_j <- seTE
TE_j <- TE

seTE_j[7] <- seTE[7]-0.09

seTE_j[10] <- seTE[10]-0.08

seTE_j[21] <- seTE[21]-0.09
TE_j[22] <- TE[22]-0.3

seTE_j[25] <- seTE[25]-0.09

seTE_j[16] <- seTE[16]-0.035
TE_j[16] <- TE[16]+0.4

seTE_j[26] <- seTE[26]-0.1

seTE_j[27] <- seTE[27]-0.035
TE_j[27] <- TE[27]+0.55

text(TE_j, seTE_j, labels = lab, cex = 0.8, pos = 1)
dev.off()

# -----------------------------------------------------------------------------#
# Kerala vs. West Bengal + misc analysis
cfr_from_outbreaks |>
  filter(outbreak_country=="India") |>
  select(outbreak_start_year, CFR, outbreak_location,
         cfr_ifr_numerator, cfr_ifr_denominator) |>
  mutate(outbreak_location=case_when(outbreak_location=="Kozhikode"~"Kerala",
                                     outbreak_location=="Siliguri"~"West Bengal",
                                     TRUE~outbreak_location)) |>
  group_by(outbreak_location) |>
  summarise(cfr=mean(CFR))

cfr_from_outbreaks |>
  filter(outbreak_country == "India") |>
  select(outbreak_start_year, CFR, outbreak_location,
         cfr_ifr_numerator, cfr_ifr_denominator) |>
  mutate(outbreak_location = case_when(
    outbreak_location == "Kozhikode" ~ "Kerala",
    outbreak_location == "Siliguri" ~ "West Bengal",
    TRUE ~ outbreak_location
  )) |>
  group_by(outbreak_location) |>
  summarise(
    cfr_ifr_denominator = sum(cfr_ifr_denominator, na.rm = TRUE),
    cfr_ifr_numerator   = sum(cfr_ifr_numerator, na.rm = TRUE),
    cfr = cfr_ifr_numerator / cfr_ifr_denominator,
    ci_low = binom.test(cfr_ifr_numerator, cfr_ifr_denominator)$conf.int[1],
    ci_high = binom.test(cfr_ifr_numerator, cfr_ifr_denominator)$conf.int[2]
  )

cfr_from_outbreaks |>
  select(outbreak_start_year, CFR, outbreak_location, outbreak_country,
         cfr_ifr_numerator, cfr_ifr_denominator) |>
  mutate(outbreak_location=case_when(outbreak_location=="Kozhikode"~"Kerala",
                                     outbreak_location=="Siliguri"~"West Bengal",
                                     TRUE~outbreak_location)) |>
  group_by(outbreak_country, outbreak_start_year) |>
  summarise(cfr=sum(cfr_ifr_numerator)/sum(cfr_ifr_denominator)) |>
  ggplot(aes(x=outbreak_start_year, y=cfr, colour = outbreak_country)) +
  geom_point() +
  theme_bw()

# Outbreak size
cfr_from_outbreaks |>
  select(outbreak_start_year, CFR, outbreak_country,
         cfr_ifr_denominator, cfr_ifr_numerator) |>
  group_by(outbreak_country, outbreak_start_year) |>
  summarise(cfr=mean(CFR),
            cfr_ifr_denominator=sum(cfr_ifr_denominator, na.rm=T),
            cfr_ifr_numerator=sum(cfr_ifr_numerator, na.rm=T)) |>
  ggplot(aes(x=outbreak_start_year, y=cfr_ifr_denominator, color=outbreak_country)) +
  geom_point() + geom_line() + labs(x="Outbreak year", y="Cases",
                                    title="Number of cases by country based on deduplicated outbreaks",
                                    subtitle="Dashed line=30 case") +
  geom_hline(aes(yintercept=30), linetype="dashed") +
  theme_bw()
# -----------------------------------------------------------------------------#
