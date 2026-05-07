# *========================= RVF transmission plots ==========================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(orderly)
library(patchwork)
library(readr)
library(stringr)
library(ggbreak)

# *--------------------------------- Orderly ----------------------------------*
pars <- orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "params.csv"))

orderly_shared_resource("rvf_functions.R" = "rvf_functions.R")
source("rvf_functions.R")

orderly_artefact("RVF transmission figures",
                 c("figure_trans_severity.png", "figure_trans_severity.pdf",
                   "figure_trans_all_qa.png", "figure_trans_all_qa.pdf",
                   "R_by_country_all_qa.png", "R_by_country_all_qa.pdf",
                   "figure_severity_all_qa.png", "figure_severity_all_qa.pdf"))

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks <- tibble()
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles, ignore_errors=TRUE)$articles
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
d4 <- parameters |> filter(parameter_class == 'Attack rate')
d5 <- parameters |> filter(parameter_class == 'Reproduction number')

d6 <- parameters |> filter(parameter_type == 'Severity - symptomatic proportion of infections')
d7 <- parameters |> filter(parameter_type == 'Severity - case fatality ratio (CFR)')
d8 <- parameters |> filter(parameter_type == 'Relative contribution - zoonotic to human')

# For numerator/denominator calculations use central value 
d6 <- d6 |> mutate(parameter_value=ifelse(is.na(parameter_value)&!is.na(cfr_ifr_numerator),central,parameter_value)) 


# article 643 notes say per 100,000 people so needs -5 exponent 
d4$exponent[which(d4$article_id==643)] <- -5
d4$parameter_lower_bound[which(d4$article_id==643)] <- d4$parameter_lower_bound[which(d4$article_id==643)] *10^(d4$exponent[which(d4$article_id==643)])
d4$parameter_upper_bound[which(d4$article_id==643)] <- d4$parameter_upper_bound[which(d4$article_id==643)] *10^(d4$exponent[which(d4$article_id==643)])
d4$parameter_value[which(d4$article_id==643)] <- d4$parameter_value[which(d4$article_id==643)] *10^(d4$exponent[which(d4$article_id==643)])
d4$parameter_uncertainty_lower_value[which(d4$article_id==643)] <- d4$parameter_uncertainty_lower_value[which(d4$article_id==643)] *10^(d4$exponent[which(d4$article_id==643)])
d4$parameter_uncertainty_upper_value[which(d4$article_id==643)] <- d4$parameter_uncertainty_upper_value[which(d4$article_id==643)] *10^(d4$exponent[which(d4$article_id==643)])


#arrange data and format for plotting
variables_to_mutate <- c("parameter_value",
                         "parameter_lower_bound",
                         "parameter_upper_bound",
                         "parameter_uncertainty_lower_value",
                         "parameter_uncertainty_upper_value")

d1 <- d1 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4
d2 <- d2 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4

d4 <- d4 |> mutate(across(all_of(variables_to_mutate),
                          ~ ifelse(parameter_unit != "Percentage (%)", . * 100, .)) #(Attack rate)
) |>
  mutate(parameter_unit = ifelse(parameter_unit != "Percentage (%)",
                                 "Percentage (%)", parameter_unit))



d1 <- d1 |> arrange(genome_site,central)
d2 <- d2 |> arrange(genome_site, central)
#d3 <- d3 |> arrange(population_group, central)

d4 <- d4 |> arrange(population_group, central)
d5 <- d5 |> arrange(population_group, central)
d6 <- d6 |> arrange(population_group, central)
d7 <- d7 |> arrange(population_group, central)
d8 <- d8 |> arrange(population_group, central)

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

# Evolution rate
##################
p1 <- forest_plot(filter(d1, qa_score >= 0.5),expression(Evolutionary~Rate~(s/s/y ~10^{-4})),
                  "genome_site", c(-0.01,10), text_size=text_size) +
  guides(color = guide_legend(title = "Gene", order = 1)) 

p1_all_qa <- forest_plot(d1,expression(Evolutionary~Rate~(s/s/y ~10^{-4})),
                  "genome_site", c(-0.01,10), text_size=text_size,
                  qa_alpha =0.3) +
  guides(color = guide_legend(title = "Gene", order = 1)) 


# Subsitution Rate
##################
# For d2; error is being caused by different units:
unique(d2$parameter_unit[!is.na(d2$parameter_unit)])
d2 <- d2 |>
  mutate(parameter_unit=ifelse(parameter_unit=="Substitutions/site/year",
                               parameter_unit, NA))


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

# Attack Rate
#############

unique(d4$parameter_unit[!is.na(d4$parameter_unit)])
d4 <- d4 |>
  mutate(parameter_unit=ifelse(parameter_unit=="Percentage (%)",
                               parameter_unit, NA))

# not sure how central value for missing studies are calculated? so reluctant to use
p4_all_qa <- forest_plot(d4, 'Attack Rate (%)', "population_group", 
                         c(-3,10),
                         qa_alpha = 0.3,
                         text_size=text_size) 

p4 <- forest_plot(filter(d4, qa_score >= 0.5),
                  'Attack Rate (%)', "population_group",
                  c(-3,50),
                  text_size=text_size) +
  guides(color = guide_legend(title = "Population Group", order = 1)) 


# Reproduction Number
####################
unique(d5$parameter_unit[!is.na(d5$parameter_unit)])
d5$parameter_unit <- "No units"
p5_all_qa <- forest_plot(d5,'Reproduction Number',"population_group",
                         c(-0.1,5),
                         text_size=text_size,
                         qa_alpha = 0.3)
p5 <- forest_plot(filter(d5, qa_score >= 0.5),'Reproduction Number',"population_group",
                  c(-0.1,5),
                  text_size=text_size) +
  guides(color = guide_legend(title = "Population Group", order = 1)) 


#Symptomatic Proportion
#########################
# FIX: HOW TO PLOT CENTRAL VALUE?
unique(d6$parameter_unit[!is.na(d6$parameter_unit)])
d6 <- d6 |>
  mutate(parameter_unit="Percentage (%)")
# parameter value is under central (calculated from proportion)
p6_all_qa <- forest_plot(d6,'Proportion of Symptomatic Cases (%)', "population_group",
                  c(0, 100),
                  text_size=text_size,
                  qa_alpha =0.3)

# Don't include this because we have to include the asymptomatics too which are in the _severity task

# Severity - CFR
#######################

unique(d7$parameter_unit[!is.na(d7$parameter_unit)])
d7$parameter_unit <- "Percentage (%)"
p7_all_qa <- forest_plot(d7,'Case-Fatality Ratio (%)', "population_group",
                         c(0, 100),
                         text_size=text_size,
                         qa_alpha =0.3)
p7 <- forest_plot(filter(d7, qa_score >= 0.5),'Case-Fatality Ratio (%)', "population_group",
                  c(0, 100),
                  text_size=text_size)

# Relative contribution - zoonotic to human
###########################################
unique(d8$parameter_unit[!is.na(d8$parameter_unit)])
# These are all low QA so just ignore
p8_all_qa <- forest_plot(d8,' Relative contribution - zoonotic to human (%)', "population_group",
                         c(-5, 100),
                         text_size=text_size,
                         qa_alpha =0.3)
p8 <- forest_plot(filter(d8, qa_score >= 0.5),' Relative contribution - zoonotic to human (%)', "population_group",
                  c(-5, 100),
                  text_size=text_size)



# Save transmission plots
design <- "ACE
BDF"
patchwork_trans <- p7+p4+p5+p8+p2+p1+plot_layout(design = design)
patchwork_trans <- patchwork_trans + plot_annotation(tag_levels = 'A')
ggsave("figure_trans_severity.png", plot = patchwork_trans, width = 20, height = 10)
ggsave("figure_trans_severity.pdf", plot = patchwork_trans, width = 20, height = 10)

## ALL QA PLOTS

# Save transmission plots
design <- "ACE
BDE"
patchwork_trans_all_qa <- p4_all_qa+p5_all_qa+p8_all_qa+p2_all_qa+p1_all_qa+plot_layout(design = design)
patchwork_trans_all_qa <- patchwork_trans_all_qa + plot_annotation(tag_levels = 'A')
ggsave("figure_trans_all_qa.png", plot = patchwork_trans_all_qa, width = 20, height = 10)
ggsave("figure_trans_all_qa.pdf", plot = patchwork_trans_all_qa, width = 20, height = 10)

# Save severity plots
design <- "A
B"
patchwork_sev_all_qa <- p6_all_qa+p7_all_qa+plot_layout(design = design)
patchwork_sev_all_qa <- patchwork_sev_all_qa + plot_annotation(tag_levels = 'A')
ggsave("figure_severity_all_qa.png", plot = patchwork_sev_all_qa, width = 10, height = 10)
ggsave("figure_severity_all_qa.pdf", plot = patchwork_sev_all_qa, width = 10, height = 10)
