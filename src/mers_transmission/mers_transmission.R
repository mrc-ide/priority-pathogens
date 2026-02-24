# *========================= MERS transmission plots ==========================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(orderly)
library(patchwork)
library(readr)
library(stringr)
library(ggbreak)

# *--------------------------------- Orderly ----------------------------------*
orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "params.csv"))

orderly_shared_resource("mers_functions.R" = "mers_functions.R")
source("mers_functions.R")

orderly_artefact("MERS transmission figures",
                 c("figure_trans.png", "figure_trans.pdf",
                   "R_by_country.png", "R_by_country.pdf",
                   "R_by_sample_type.png", "R_by_sample_type.pdf"))

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks <- tibble()
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

# *----------------------------- Data preparation -----------------------------*
# Sort population sample type to match legend where Other, Unspecified, or NA
# are included. A neater solution would be to do this during cleaning or for the
# relevant subset of population groups included in the params considered in
# this script.
parameters <- parameters |>
  mutate(population_group = ifelse(
    is.na(population_group), "Unspecified",
    population_group
  ))

parameters <- parameters  |>
  mutate(population_group = factor(
    population_group,
    levels = c(sort(setdiff(unique(population_group),
                            c("Other", "Unspecified"))),
               "Other", "Unspecified")))

d1 <- parameters |> filter(parameter_type == 'Mutations - evolutionary rate')
d2 <- parameters |> filter(parameter_type == 'Mutations - substitution rate')
d3 <- parameters |> filter(parameter_class == 'Overdispersion')
d4 <- parameters |> filter(parameter_class == 'Attack rate')
d5 <- parameters |> filter(parameter_class == 'Reproduction number')
d6 <- parameters |> filter(parameter_type == 'Severity - proportion of symptomatic cases')

d7 <- parameters |> filter(parameter_type == 'Secondary attack rate')
d8 <- parameters |> filter(parameter_type == 'Growth rate (r)')
#d9 <- parameters |> filter(parameter_type == 'Doubling time')


#arrange data and format for plotting
variables_to_mutate <- c("parameter_value",
                         "parameter_lower_bound",
                         "parameter_upper_bound",
                         "parameter_uncertainty_lower_value",
                         "parameter_uncertainty_upper_value")

d1 <- d1 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4
d2 <- d2 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4
d4 <- d4 |> mutate(across(all_of(variables_to_mutate),
                   ~ ifelse(parameter_unit == "Unspecified", . * 100, .)) #(Attack rate)
                   ) |>
  mutate(parameter_unit = ifelse(parameter_unit == "Unspecified",
                                 "Percentage (%)", parameter_unit))

d1 <- d1 |> arrange(genome_site,central)
d2 <- d2 |> arrange(genome_site, central)
d3 <- d3 |> arrange(population_group, central)

d4 <- d4 |>  arrange(population_group, central)
d5 <- d5 |> arrange(population_group, central)
d6 <- d6 |> arrange(population_group, central)

# *---------------------------------- Plots -----------------------------------*
# Set to 28 for other pathogens
text_size <- 14
full_levels <- c("General population",
                 "Mixed groups",
                 "Healthcare workers",
                 "Persons under investigation",
                 "Other",
                 "Unspecified")

# Generate plots
#################

# MERS hack:
# We don't want NA in the p1 legend. Rather than reach into forest_plot, it's easier to just set Paden(2018) to "Whole Genome". It's not plotted anyway, just hides an NA from the plot.
d1$genome_site[d1$covidence_id == 2350] <- "Whole Genome"

p1 <- forest_plot(d1,expression(Evolutionary~Rate~(s/s/y ~10^{-4})),
                  "genome_site", c(-0.01,15), text_size=text_size) +
  guides(color = guide_legend(title = "Gene", order = 1)) +
  scale_fill_lancet(
    labels = function(x) {
      x[x == "genome coverage of over 30%"] <-
        "Over 30% of \ngenome"
      x
    }
  ) +
  scale_color_lancet(
    labels = function(x) {
      x[x == "genome coverage of over 30%"] <-
        "Over 30% of \ngenome"
      x
    }
  )

# For d2; error is being caused by different units:
unique(d2$parameter_unit[!is.na(d2$parameter_unit)])
d2 <- d2 |>
  mutate(parameter_unit=ifelse(parameter_unit=="Substitutions/site/year",
                               parameter_unit, NA))
# Rename the very long list of countries
d2 <- d2 |>
  mutate(population_country=ifelse(population_country=="China; Egypt; France; Jordan; Qatar; Republic of Korea; Saudi Arabia; United Arab Emirates; United Kingdom of Great Britain and Northern Ireland; United States of America",
                               ">9 countries", population_country))
d2 <- d2 |>
  mutate(population_country=ifelse(population_country=="Qatar; Saudi Arabia; United Arab Emirates",
                                   "Qatar; Saudi Arabia;\nUnited Arab Emirates", population_country))
# 229_003 is set to "GenBank".
d2$genome_site[d2$access_param_id == "229_003"] <- "Unspecified"

p2_all_qa <- forest_plot(d2,
                  expression(Substitution~Rate~(s/s/y ~10^{-4})),
                  "population_country",c(-5,55),
                  qa_alpha = 0.3,
                  text_size=text_size) +
  guides(color = guide_legend(title = "Population Country", order = 1))

p2 <- forest_plot(filter(d2, qa_score >= 0.5),
                         expression(Substitution~Rate~(s/s/y ~10^{-4})),
                         "genome_site",c(-0.01,15),
                         text_size=text_size) +
  guides(color = guide_legend(title = "Gene", order = 1))

## Overdispersion
# access_id 064_004 has an upper limit of infinity, having checked the original paper, better to just remove it:
d3 <- d3 |>
  filter(access_param_id != "064_004")

unique(d3$parameter_unit[!is.na(d3$parameter_unit)])
d3 <- d3 |>
  mutate(parameter_unit=ifelse(
    parameter_unit=="Max. nr. of cases superspreading (related to case)",
    parameter_unit, "Unspecified")) |>
  mutate(parameter_value_type = ifelse(
    parameter_value_type=="Maximum likelihood",
    "Unspecified", parameter_value_type
  ))

# Now, Park (2016c) is a very different type of measure. It's not a k parameter.
# It shouldn't be plotted alongside this, and rather, should be probably just mentioned in the text instead
d3_Park <- filter(d3, access_param_id == "038_001")
d3 <- d3 |>
  filter(access_param_id != "038_001")

p3_all_qa <- forest_plot(d3,'Overdispersion',"population_group", c(-0.1,7),
                         qa_alpha = 0.3,
                  text_size=text_size) +
  guides(color = guide_legend(title = "Population Group", order = 1))
p3 <- forest_plot(filter(d3, qa_score >= 0.5),
                  'Overdispersion',"population_group", c(-0.1,7),
                         text_size=text_size) +
  guides(color = guide_legend(title = "Population Group", order = 1)) +
  scale_fill_lancet(
    palette = "lanonc",
    limits = full_levels,
    breaks = full_levels[c(4,6)],
    drop = TRUE,
    labels = function(x) {
      x[x == "Persons under investigation"] <-
        "Persons under\ninvestigation"
      x
    }
  ) +
  scale_color_lancet(
    palette = "lanonc",
    limits = full_levels,
    breaks = full_levels[c(4,6)],
    drop = TRUE,
    labels = function(x) {
      x[x == "Persons under investigation"] <-
        "Persons under\ninvestigation"
      x
    }
  )

# Attack Rate
#############

unique(d4$parameter_unit[!is.na(d4$parameter_unit)])
d4 <- d4 |>
  mutate(parameter_unit=ifelse(parameter_unit=="Percentage (%)",
                               parameter_unit, NA))
p4_all_qa <- forest_plot(d4, 'Attack Rate (%)', "population_group", c(-1,100),
                  qa_alpha = 0.3,
                  text_size=text_size) +
  guides(color = guide_legend(title = "Population Group", order = 1))
p4 <- forest_plot(filter(d4, qa_score >= 0.5),
                  'Attack Rate (%)', "population_group", c(-1,35),
                         text_size=text_size) +
  guides(color = guide_legend(title = "Population Group", order = 1)) +
  scale_fill_lancet(
    palette = "lanonc",
    limits = full_levels,
    breaks = full_levels[c(2,3,4)],
    drop = TRUE,
    labels = function(x) {
      x[x == "Persons under investigation"] <-
        "Persons under\ninvestigation"
      x
    }
  ) +
  scale_color_lancet(
    palette = "lanonc",
    limits = full_levels,
    breaks = full_levels[c(2,3,4)],
    drop = TRUE,
    labels = function(x) {
      x[x == "Persons under investigation"] <-
        "Persons under\ninvestigation"
      x
    }
  )

# Reproduction Number
####################
unique(d5$parameter_unit[!is.na(d5$parameter_unit)])
d5$parameter_unit <- "No units"
p5_all_qa <- forest_plot(d5,'Reproduction Number',"population_group", c(-0.1,30),
                  text_size=text_size,
                  qa_alpha = 0.3)
p5 <- forest_plot(filter(d5, qa_score >= 0.5),'Reproduction Number',"population_group", c(-0.1,30),
                  text_size=text_size) +
  guides(color = guide_legend(title = "Population Group", order = 1)) +
  scale_fill_lancet(
    palette = "lanonc",
    limits = full_levels,
    breaks = full_levels[c(1,2,4,5,6)],
    drop = TRUE,
    labels = function(x) {
      x[x == "Persons under investigation"] <-
        "Persons under\ninvestigation"
      x
    }
  ) +
  scale_color_lancet(
    palette = "lanonc",
    limits = full_levels,
    breaks = full_levels[c(1,2,4,5,6)],
    drop = TRUE,
    labels = function(x) {
      x[x == "Persons under investigation"] <-
        "Persons under\ninvestigation"
      x
    }
  )

# Symptomatic Proportion
#########################

unique(d6$parameter_unit[!is.na(d6$parameter_unit)])
p6 <- forest_plot(d6,'Proportion of Symptomatic Cases (%)', "population_group",
                  c(0, 110),
                  text_size=text_size) +
  scale_fill_lancet(
    palette = "lanonc",
    limits = full_levels,
    breaks = full_levels[c(1,2,4)],
    drop = TRUE
  ) +
  scale_color_lancet(
    palette = "lanonc",
    limits = full_levels,
    breaks = full_levels[c(1,2,4)],
    drop = TRUE
  )

# Don't include this because we have to include the asymptomatics too which are in the _severity task

# Secondary Attack Rate
#######################

unique(d7$parameter_unit[!is.na(d7$parameter_unit)])
p7_all_qa <- forest_plot(d7,'Secondary Attack Rate (%)', "population_group",
                  c(0, 25),
                  text_size=text_size,
                  qa_alpha =0.3)
p7 <- forest_plot(filter(d7, qa_score >= 0.5),'Secondary Attack Rate (%)', "population_group",
                         c(0, 25),
                         text_size=text_size)

#Growth rate (r)
################
unique(d8$parameter_unit[!is.na(d8$parameter_unit)])
# These are all low QA so just ignore


# Save plots
design <- "ACE
BDE"
patchwork_trans <- p3+p1+p4+p2+p5+plot_layout(design = design)
patchwork_trans <- patchwork_trans + plot_annotation(tag_levels = 'A')
ggsave("figure_trans.png", plot = patchwork_trans, width = 20, height = 10)
ggsave("figure_trans.pdf", plot = patchwork_trans, width = 20, height = 10)
# *============================================================================*

# Now we make some fancier R number plots
table(d5$parameter_type)
# First, for simplicity, we map the R numbers that specify "human" to the general
d5$parameter_type <- sub(" - Human$", "", d5$parameter_type)

# We will make two plots, one for R0 and one for Re
dR0 <- filter(d5, parameter_type == "Reproduction number (Basic R0)")
dRe <- filter(d5, parameter_type == "Reproduction number (Effective, Re)")

dRe <- dRe |>
  mutate(population_country=ifelse(population_country=="Republic of Korea; Saudi Arabia",
                                   "Republic of Korea; \nSaudi Arabia" , population_country)) |>
  # To fix the NA in Blumberg (2014)
  mutate(population_country=ifelse(is.na(population_country),
                                   "Arabian Peninsula" , population_country)) |>
  mutate(population_country=ifelse(population_country=="Algeria; Austria; China; Egypt; France; Germany; Greece; Iran (Islamic Republic of); Italy; Lebanon; Malaysia; Netherlands; Philippines; Republic of Korea; Spain; Thailand; Tunisia; TÃ¼rkiye; United Kingdom of Great Britain and Northern Ireland; United States of America",
                                   "Global", population_country)) |>
  mutate(population_country=ifelse(population_country=="France; Germany; Italy; Jordan; Qatar; Saudi Arabia; Tunisia; United Arab Emirates; United Kingdom of Great Britain and Northern Ireland",
                                   "Global", population_country)) |>
  mutate(population_country=ifelse(population_country=="Bahrain; Kuwait; Qatar; Saudi Arabia; United Arab Emirates; Yemen" ,
                                   "GCC Countries \nand Yemen", population_country)) |>
  mutate(parameter_value_type = ifelse(
    parameter_value_type=="Maximum likelihood",
    "Unspecified", parameter_value_type
  ))
dRe <- dRe |> arrange(population_country, central)
full_levels <- unique(filter(dRe, qa_score >= 0.5)$population_country)

pRe <- forest_plot(filter(dRe, qa_score >= 0.5),'Effective Reproduction Number (R_e)',
                   "population_country", #"method_moment_value",
                   c(0, 30),
                   text_size=text_size)  +
  scale_x_continuous(breaks = seq(0,30, by = 2),
                     limits = c(0,29)) +
  scale_x_break(c(10, 25)) +
  scale_fill_lancet(
    palette = "lanonc",
    limits = full_levels
  ) +
  scale_color_lancet(
    palette = "lanonc",
    limits = full_levels
  )

# Rename the very long list of countries
dR0 <- dR0 |>
  mutate(population_country=ifelse(population_country=="Algeria; Austria; China; Egypt; France; Germany; Greece; Iran (Islamic Republic of); Italy; Lebanon; Malaysia; Netherlands; Philippines; Republic of Korea; Spain; Thailand; Tunisia; TÃ¼rkiye; United Kingdom of Great Britain and Northern Ireland; United States of America",
                                   "Global", population_country)) |>
  mutate(population_country=ifelse(population_country=="Algeria; Austria; China; Egypt; France; Germany; Greece; Iran (Islamic Republic of); Italy; Jordan; Kuwait; Lebanon; Malaysia; Netherlands; Oman; Philippines; Republic of Korea; Thailand; Tunisia; TÃ¼rkiye; United Kingdom of Great Britain and Northern Ireland; United States of America; Yemen",
                                   "Global", population_country)) |>
  mutate(population_country=ifelse(population_country=="Algeria; Austria; China; France; Germany; Greece; Italy; Malaysia; Netherlands; Philippines; Republic of Korea; Thailand; Tunisia; United Kingdom of Great Britain and Northern Ireland; United States of America",
                                   "Global", population_country)) |>
  mutate(parameter_value_type = ifelse(
    parameter_value_type=="Maximum likelihood",
    "Unspecified", parameter_value_type
  ))

dR0 <- dR0 |> arrange(population_country, central)
pR0 <- forest_plot(filter(dR0, qa_score >= 0.5),'Basic Reproduction Number (R_0)', "population_country", #"method_moment_value",
                  c(0, 30),
                  text_size=text_size) +
  scale_x_continuous(breaks = seq(0,30, by = 2),
                     limits = c(0,29)) +
  scale_x_break(c(10, 25)) +
  guides(color = guide_legend(title = "Population Country", order = 1)) +
  scale_fill_lancet(
    palette = "lanonc",
    limits = full_levels,
    breaks = full_levels[c(3,4,6)],
    drop = TRUE,
  ) +
  scale_color_lancet(
    palette = "lanonc",
    limits = full_levels,
    breaks = full_levels[c(3,4,6)],
    drop = TRUE
  )

patchwork_country <- (pR0 + pRe) +
  plot_layout(nrow = 2, heights = c(1,1))

ggsave("R_by_country.png", plot = patchwork_country, width = 7, height = 8)
ggsave("R_by_country.pdf", plot = patchwork_country, width = 7, height = 8)

################
sample_type_levels <- c("Hospital based",
                        "Population based",
                        "Travel based",
                        "Other",
                        "Unspecified")

dRe <- dRe |>
  mutate(population_sample_type = ifelse(
    is.na(population_sample_type), "Unspecified",
    population_sample_type
  ))
dRe <- dRe |> arrange(population_sample_type, central)

pRe <- forest_plot(filter(dRe, qa_score >= 0.5),'Effective Reproduction Number (R_e)',
                   "population_sample_type", #"method_moment_value",
                   c(0, 30),
                   text_size=text_size)  +
  scale_x_continuous(breaks = seq(0,31, by = 2),
                     limits = c(0,29)) +
  scale_x_break(c(10, 25)) +
scale_fill_lancet(
  palette = "lanonc",
  limits = sample_type_levels
) +
scale_color_lancet(
  palette = "lanonc",
  limits = sample_type_levels
)
# ) +
#   theme(
#     panel.grid.major.x = element_line(colour = "grey85"),
#     axis.ticks.length = unit(-0.15, "cm")
#   )

dR0 <- dR0 |> arrange(population_sample_type, central)
pR0 <- forest_plot(filter(dR0, qa_score >= 0.5),'Basic Reproduction Number (R_0)',
                   "population_sample_type", #"method_moment_value",
                   c(0, 30),
                   text_size=text_size)  +
  scale_x_continuous(breaks = seq(0,31, by = 2),
                     limits = c(0,29)) +
  scale_x_break(c(10, 25)) +
  scale_fill_lancet(
    palette = "lanonc",
    limits = sample_type_levels
  ) +
  scale_color_lancet(
    palette = "lanonc",
    limits = sample_type_levels
  )

patchwork_sample_type <- (pR0 + pRe) +
  plot_layout(nrow = 2, heights = c(1,1))

ggsave("R_by_sample_type.png", plot = patchwork_sample_type, width = 7, height = 8)
ggsave("R_by_sample_type.pdf", plot = patchwork_sample_type, width = 7, height = 8)

dev.off()
