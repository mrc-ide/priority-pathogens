# *========================= RVF transmission plots ==========================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(orderly)
library(patchwork)
library(readr)
library(stringr)
library(ggbreak)
library(forcats)
library(binom)
library(metafor)
library(meta)
library(tidyverse)
library(grid)
library(cowplot)
library(scales)
library(wesanderson)

# *--------------------------------- Orderly ----------------------------------*
pars <- orderly_parameters(pathogen = "RVF")

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "params.csv","outbreaks.csv"))

orderly_dependency("rvf_delays", "latest(parameter:pathogen == this:pathogen)",
                   c("figure_delays.RData"))

orderly_shared_resource("rvf_functions.R" = "rvf_functions.R")
source("rvf_functions.R")

orderly_artefact("RVF transmission figures",
                 c("figure_trans_severity.png", "figure_trans_severity.pdf",
                   "figure_trans_all_qa.png", "figure_trans_all_qa.pdf",
                   "R_by_country_all_qa.png", "R_by_country_all_qa.pdf",
                   "figure_severity_all_qa.png", "figure_severity_all_qa.pdf"))



# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

# ### Correct covidence #5293 - mutation rate
# library(readxl)
# all_genomic_data_wang_corrected <- read_excel("all_genomic_data_wang_corrected.xlsx")
# all_genomic_data_wang_corrected[all_genomic_data_wang_corrected=="NA"] <- NA
# all_genomic_data_wang_corrected <- all_genomic_data_wang_corrected |> mutate(
#   parameter_2_value = suppressWarnings(as.numeric(parameter_2_value)), 
#                                        parameter_2_lower_bound = suppressWarnings(as.numeric(parameter_2_lower_bound)),
#   parameter_2_upper_bound = suppressWarnings(as.numeric(parameter_2_upper_bound)),
#   exponent_2 = suppressWarnings(as.numeric(exponent_2)),
#   cfr_ifr_numerator = suppressWarnings(as.numeric(cfr_ifr_numerator)),
#   cfr_ifr_denominator = suppressWarnings(as.numeric(cfr_ifr_denominator)) ,
#   population_age_min = suppressWarnings(as.numeric(population_age_min)),
#   population_age_max = suppressWarnings(as.numeric(population_age_max)),
#   population_study_start_day = suppressWarnings(as.numeric(population_study_start_day)) ,
#   population_study_end_day = suppressWarnings(as.character(population_study_end_day)) ,
#   population_study_start_year = suppressWarnings(as.numeric(population_study_start_year)) ,
#   population_study_end_year = suppressWarnings(as.numeric(population_study_end_year)),
#   population_study_start_month = suppressWarnings(as.character(population_study_start_month)) ,
#   population_study_end_month = suppressWarnings(as.character(population_study_end_month)),
#   central = suppressWarnings(as.numeric(central)) # suppress warnings for coercion
#   )
# 
# parameters_632_001 <- parameters|>filter(access_param_id=="632_001")
# parameters_wang_fixed <- rbind(parameters_632_001,parameters_632_001,parameters_632_001)
# parameters_wang_fixed[1,c(1:17,21:23,47:ncol(parameters))] <- all_genomic_data_wang_corrected[which(all_genomic_data_wang_corrected$access_param_id=="632_001"),c(1:17,21:23,47:ncol(all_genomic_data_wang_corrected))]
# parameters_wang_fixed[2,c(1:17,21:23,47:ncol(parameters))] <- all_genomic_data_wang_corrected[which(all_genomic_data_wang_corrected$access_param_id=="632_002"),c(1:17,21:23,47:ncol(all_genomic_data_wang_corrected))]
# parameters_wang_fixed[3,c(1:17,21:23,47:ncol(parameters))] <- all_genomic_data_wang_corrected[which(all_genomic_data_wang_corrected$access_param_id=="632_003"),c(1:17,21:23,47:ncol(all_genomic_data_wang_corrected))]
# parameters_wang_fixed$ids[which(df$access_param_id=="632_001")] <- ids::random_id(1) # new random id
# parameters_wang_fixed$ids[which(df$access_param_id=="632_002")] <- ids::random_id(1) # new random id
# parameters_wang_fixed$ids[which(df$access_param_id=="632_003")] <- ids::random_id(1) # new random id
# 
# parameters <- rbind(parameters |> filter(access_param_id!="632_001"), parameters_wang_fixed)

# curation function 
dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles, ignore_errors=TRUE)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

outbreaks <- dfs$outbreaks |>
  left_join(qa_scores)

## *----------------------------- Data preparation -----------------------------*
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
               "Other", "Unspecified")))|>
  mutate(population_country=ifelse(population_country=="France", "France (Mayotte)", population_country))|>
  mutate(population_country=ifelse(population_country=="Madagascar; South Africa", "Madagascar", population_country))|>
  mutate(population_country=ifelse(is.na(population_country), "Unspecified", population_country))

parameters <- parameters[order(parameters$population_country),]


d1 <- parameters |> filter(parameter_type == 'Mutations - evolutionary rate')
d2 <- parameters |> filter(parameter_type == 'Mutations - substitution rate')
d4 <- parameters |> filter(parameter_class == 'Attack rate')
d5 <- parameters |> filter(parameter_class == 'Reproduction number')

d6 <- parameters |> filter(parameter_type == 'Severity - symptomatic proportion of infections')
d7 <- parameters |> filter(parameter_type == 'Severity - case fatality ratio (CFR)')
d8 <- parameters |> filter(parameter_type == 'Relative contribution - animal to vector')

# For numerator/denominator calculations use central value 
d6 <- d6 |> mutate(parameter_value=ifelse(is.na(parameter_value)&!is.na(cfr_ifr_numerator),central,parameter_value)) 

#arrange data and format for plotting
variables_to_mutate <- c("parameter_value",
                         "parameter_lower_bound",
                         "parameter_upper_bound",
                         "parameter_uncertainty_lower_value",
                         "parameter_uncertainty_upper_value")

# Multiply evolutionary/substitution rates by 10^4 for plotting 
d1 <- d1 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4
d2 <- d2 |> mutate(across(all_of(variables_to_mutate), ~ . * 10^4)) #multiply by 10^4

# Convert non-percentage attack rate into percentage 
d4 <- d4 |> mutate(parameter_value = case_when(is.na(parameter_unit) ~ parameter_value*100,
                                                    .default = parameter_value)) |>
  mutate(parameter_unit = ifelse(is.na(parameter_unit),
                                 "Percentage (%)", parameter_unit))



d1 <- d1 |> arrange(genome_site,central)
d2 <- d2 |> arrange(genome_site, central)
#d3 <- d3 |> arrange(population_group, central)

d4 <- d4 |> arrange(population_group, central)
d5 <- d5 |> arrange(population_group, central)
d6 <- d6 |> arrange(population_group, central)
d7 <- d7 |> arrange(population_group, central)
d8 <- d8 |> arrange(population_group, central)

write.csv(d4,row.names = FALSE,"all_attack_rate_data.csv")


#Country colour palette 
country_names <- sort(c(unique(d7$population_country), "France (Mayotte)", "Rwanda"))
# ggplot2 default discrete palette with 12 colours
country_palette <- hue_pal()(length(country_names))
# create palette
names(country_palette) <- country_names
country_palette <- c(country_palette, "Unspecified"="grey", "Various"="black" )
# save colour palette to share

save(country_palette, file="~/priority-pathogens/shared/country_palette.RData")

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


# Substitution Rate
#####################
unique(d2$parameter_unit[!is.na(d2$parameter_unit)])
d2 <- d2 |>
  mutate(parameter_unit=ifelse(parameter_unit=="Substitutions/site/year",
                               parameter_unit, NA))


p2_all_qa <- forest_plot(d2,
                         expression(Substitution~Rate~(s/s/y ~10^{-4})),
                         "genome_site",c(-5,30),
                         qa_alpha = 0.3,
                         text_size=text_size) +
  guides(color = guide_legend(title = "Gene", order = 1))

p2 <- forest_plot(filter(d2, qa_score >= 0.5),
                  expression(Substitution~Rate~(s/s/y ~10^{-4})),
                  "genome_site",c(-0.01,5),
                  text_size=text_size) +
  guides(color = guide_legend(title = "Gene", order = 1))

##' all genomic data
genomic_da <- full_join(d1,d2)

genomic_all_qa <- forest_plot(genomic_da,
                         expression(paste("Evolutionary/Substitution")~Rate~(s/s/y ~10^{-4})),
                         "genome_site",c(-5,30),
                         qa_alpha = 0.3,
                         text_size=text_size) +
  guides(color = guide_legend(title = "Gene", order = 1))+
  facet_wrap( ~parameter_type, ncol=1,scales="free_y")


## Save all risk params
write.csv(genomic_da,row.names = FALSE,"all_genomic_data.csv")


# Attack Rate
###############
# d4 <- mutate(d4, parameter_value = case_when(is.na(parameter_value)&!is.na(cfr_ifr_numerator)&!is.na(cfr_ifr_denominator) ~ (cfr_ifr_numerator/cfr_ifr_denominator)*100,
#                                              .default = parameter_value), 
#              parameter_statistical_approach = case_when(is.na(parameter_statistical_approach) ~ "Unknown",
#                                                         .default = parameter_statistical_approach))

unique(d4$parameter_unit[!is.na(d4$parameter_unit)])

# not sure how central value for missing studies are calculated? so reluctant to use
attackrate_all_qa <- forest_plot_approach(d4, 'Attack Rate (%)', "population_group", 
                         c(-3,20),
                         qa_alpha = 0.3,
                         text_size=text_size) + facet_wrap(~parameter_statistical_approach, 
                                                           ncol=1, scales="free_y")

p4 <- forest_plot_approach(filter(d4, qa_score >= 0.5),
                  'Attack Rate (%)', "population_group",
                  c(-3,15),
                  text_size=text_size) +
  guides(color = guide_legend(title = "Population Group", order = 1)) 


# Reproduction Number
####################
unique(d5$parameter_unit[!is.na(d5$parameter_unit)])
d5$parameter_unit <- "No units"
p5_all_qa <- forest_plot_approach(d5,'Reproduction Number',"population_group",
                         c(-0.1,5),
                         text_size=text_size,
                         qa_alpha = 0.3)
p5 <- forest_plot_approach(filter(d5, qa_score >= 0.5),'Reproduction Number',"population_group",
                  c(-0.1,5),
                  text_size=text_size) +
  guides(color = guide_legend(title = "Population Group", order = 1)) 


#Symptomatic Proportion
#########################
unique(d6$parameter_unit[!is.na(d6$parameter_unit)])
d6 <- d6 |>
  mutate(parameter_unit="Percentage (%)")
# parameter value is under central (calculated from proportion)
p6_all_qa <- forest_plot_approach(d6,'Proportion of Symptomatic Cases (%)', "population_group",
                  c(0, 100),
                  text_size=text_size,
                  qa_alpha =0.3)


# Severity - CFR
#######################
unique(d7$parameter_unit[!is.na(d7$parameter_unit)])

d7$cases <- d7$cfr_ifr_denominator
d7$cases[is.na(d7$cases)] <-0
d7$deaths <- d7$cfr_ifr_numerator
d7$deaths[is.na(d7$deaths)] <-0
d7$parameter_uncertainty_lower_value_og <- d7$parameter_uncertainty_lower_value

# Calculate binomial confidence interval for CFR
d7$binom_ci_upper <- binom::binom.confint(
  x = d7$deaths,
  n = d7$cases,
  methods = "wilson"
)$upper

d7$binom_ci_lower <- binom::binom.confint(
  x = d7$deaths,
  n = d7$cases,
  methods = "wilson"
)$lower


d7 <- mutate(d7, parameter_value = case_when(is.na(parameter_value)&!is.na(cfr_ifr_numerator)&!is.na(cfr_ifr_denominator) ~ (cfr_ifr_numerator/cfr_ifr_denominator)*100,
                                                  .default = parameter_value),
             parameter_statistical_approach = case_when(is.na(parameter_statistical_approach) ~ "Unknown",
                                        .default = parameter_statistical_approach),
             parameter_uncertainty_lower_value = case_when(is.na(parameter_uncertainty_lower_value_og)&!is.na(cfr_ifr_numerator)&!is.na(cfr_ifr_denominator) ~
                                                             binom_ci_lower*100,
                                                           .default = parameter_uncertainty_lower_value
                                                             ),
             parameter_uncertainty_upper_value = case_when(is.na(parameter_uncertainty_lower_value_og)&!is.na(cfr_ifr_numerator)&!is.na(cfr_ifr_denominator) ~
                                                            binom_ci_upper*100,
                                                           .default = parameter_uncertainty_upper_value
             )
             
             )

d7$parameter_unit <- "Percentage (%)"

### forest plot - sample size faceted 
cfr_qa_alpha <- 0.3
cfr_all_qa_grid <- forest_plot_approach(df=d7, label='Case-Fatality Ratio (%)', 
                                   color_column = "population_group",
                                   lims=c(-3, 110),
                                   text_size=text_size,
                                   qa_alpha =cfr_qa_alpha) + facet_grid(rows = vars(population_sample_size),
                                                                        scales = "free_y",
                                                                        space = "free_y")


### forest plot - sample size ordering 

d7$plot_alpha <- 1
d7$segment_alpha <- 0.75
  d7[d7$qa_score<=0.5, ]$plot_alpha <- cfr_qa_alpha
  d7[d7$qa_score<=0.5, ]$segment_alpha <- 0.65 * cfr_qa_alpha

d7   <- d7 |> mutate(urefs = make.unique(refs)) 
  
study_order <- d7 |>
  distinct(urefs, refs, parameter_value) |>
  arrange(parameter_value) |>
  pull(urefs)

d7 <- d7 |>
  mutate(
    urefs = factor(urefs, levels = study_order)
  )|>
  mutate(uncertainty_present = (
           !(is.na(parameter_uncertainty_lower_value) &
               is.na(parameter_uncertainty_upper_value)))
  )

write.csv(d7,row.names = FALSE,"all_CFR_data.csv")


cfr_all_qa  <- ggplot(d7) +
  geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                   y = urefs, yend = urefs, color = .data[["population_group"]],),
               linewidth=3, alpha = d7$segment_alpha, show.legend = NA) +
  geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                    y = urefs, linetype="Uncertainty"),
                width = 0.25, lwd=0.5, color = "black", alpha=d7$plot_alpha) +
  geom_errorbar(data= d7[!d7$uncertainty_present,],
                aes(xmin=parameter_2_lower_bound, xmax=parameter_2_upper_bound,
                    y = urefs, linetype="Variability"),
                width = 0.25, lwd=0.5, color = "black",
                lineend = "square", alpha=d7[!(d7$uncertainty_present),]$plot_alpha) +
  geom_errorbar(data= d7[d7$uncertainty_present,],
                aes(xmin=parameter_2_lower_bound, xmax=parameter_2_upper_bound,
                    y = urefs, linetype="Variability"),
                width = 0.25, lwd=0.5, color = "black",
                lineend = "square", position = position_nudge(y=-0.25),
                alpha=d7[d7$uncertainty_present,]$plot_alpha) +
  geom_point(aes(x = parameter_value, y = urefs,
                 shape = parameter_statistical_approach, fill = .data[["population_group"]]),
             alpha=d7$plot_alpha, size = 3, stroke = 1, color = "black") +
  scale_shape_manual(name = "Statistical approach",
                     values = c(`Observed sample statistic` = 21,
                                `Estimated parameter` = 22,            
                                `Unknown` = 23, 
                                `Unspecified` = 24, `Other`=25),
                     breaks = c("Observed sample statistic", "Estimated parameter", "Unspecified", "Other",
                                "Unknown")) +
  scale_linetype_manual(name   = "Variation Type",
                        values = c("Uncertainty" = "solid","Variability" = "dashed"),
                        breaks = c("Uncertainty", "Variability")) +
  scale_x_continuous(expand = c(0, 0), limits=c(-5,105)) +
  scale_y_discrete(labels = setNames(d7$refs, d7$urefs)) +
  labs(x = 'Case-Fatality Ratio (%)', y = NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
        text = element_text(size = text_size)) + 
  guides(fill = "none", color = guide_legend(title = NULL,order = 1),
                                                         shape = guide_legend(title = NULL,order = 2),
                                                         linetype=guide_legend(title = NULL, order = 3))

ggsave("cfr_all_qa_sample_size.pdf", plot = cfr_all_qa, width = 10, height = 10)

## Forest plot - order by country
cfr_all_qa_country <- forest_plot_approach(d7,'Case-Fatality Ratio (%)', "population_sample_type",
                         c(-3, 110),
                         text_size=text_size,
                         qa_alpha =0.3)+
  facet_wrap(~population_country, 
             ncol=1, scales="free_y")
#ggsave("cfr_all_qa_country.pdf", plot = cfr_all_qa_country, width = 10, height = 10)


## Forest plot - order by time
cfr_all_qa_time <- forest_plot_approach(d7,'Case-Fatality Ratio (%)', "population_group",
                                           c(-3, 110),
                                           text_size=text_size,
                                           qa_alpha =0.3)+ facet_grid(rows = vars(population_study_start_year),
                                                                  scales = "free_y",
                                                                  space = "free_y")

#ggsave("cfr_all_qa_time.pdf", plot = cfr_all_qa_time, width = 10, height = 10)


# NOTE: all cfr_ifr_method is "Unspecified" NA or "Naive"

p7 <- forest_plot_approach(filter(d7, qa_score >= 0.5),'Case-Fatality Ratio (%)', "population_group",
                  c(0, 100),
                  text_size=text_size)

# Relative contribution - animal to vector
###########################################
unique(d8$parameter_unit[!is.na(d8$parameter_unit)])
# These are all low QA so just ignore
p8_all_qa <- forest_plot_approach(d8,
                         label='Relative contribution - animal to vector (%)', 
                         color_column = "population_group",
                         lims=c(-5, 100),
                         text_size=text_size,
                         qa_alpha =0.3)
p8 <- forest_plot_approach(filter(d8, qa_score >= 0.5),
                  label = 'Relative contribution - animal to vector (%)',
                  color_column = "population_group",
                  lims=c(-5, 100),
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
patchwork_trans_all_qa <- attackrate_all_qa+p5_all_qa+p8_all_qa+p2_all_qa+p1_all_qa+plot_layout(design = design)
patchwork_trans_all_qa <- patchwork_trans_all_qa + plot_annotation(tag_levels = 'A')
ggsave("figure_trans_all_qa.png", plot = patchwork_trans_all_qa, width = 20, height = 10)
ggsave("figure_trans_all_qa.pdf", plot = patchwork_trans_all_qa, width = 20, height = 10)

# Save severity plots
design <- "A
B"
patchwork_sev_all_qa <- p6_all_qa+cfr_all_qa+plot_layout(design = design)
patchwork_sev_all_qa <- patchwork_sev_all_qa + plot_annotation(tag_levels = 'A')
ggsave("figure_severity_all_qa.png", plot = patchwork_sev_all_qa, width = 10, height = 10)
ggsave("figure_severity_all_qa.pdf", plot = patchwork_sev_all_qa, width = 10, height = 10)



# Extracted CFRs parameters
# d1 <- parameters |>
#   filter(parameter_type == 'Severity - case fatality rate (CFR)')
# Unspecified parameter type for CFR with only numerator and denom
# d1 <- d1 |>
#   filter(parameter_unit != "Unspecified")

# Create subgroups here
# Lassa examples for study midyear, population_group, cfr_denom_cat
extracted_cfr <- d7 |>
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
    study_midyear %in% 1950:1999 ~ "1950-1999",
    study_midyear %in% 2000:2009 ~ "2000-2009",
    study_midyear %in% 2010:2019 ~ "2010-2019",
    study_midyear %in% 2020:2027 ~ "2020-Present",
    TRUE ~ "Unspecified")) |>
  mutate(cfr_denom_cat = case_when(
    cfr_ifr_denominator %in% 10:199      ~ "Reported Cases = 10-199",
    cfr_ifr_denominator %in% 200:399   ~ "Reported Cases = 200-399",
    cfr_ifr_denominator %in% 400:18000   ~ "Reported Cases = 400-18,000",
    TRUE ~ "Unspecified")) |>
  mutate(duplicate_cfr = "False")|>
  # # Deduplicating CFRs
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
  mutate(
    cfr_denom_cat = factor(cfr_denom_cat, levels=c("Reported Cases = 10-199",
                                                               "Reported Cases = 200-399",
                                                               "Reported Cases = 400-18,000") ),
    population_group = factor(population_group,
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
  
  extracted_cfr_filtered <- extracted_cfr |> filter(qa_score>qa_threshold,
                              duplicate_cfr=="False")
  d2_filtered <- d2 |> filter(qa_score>qa_threshold)
  
  # Colours:
  all_pop_groups <- extracted_cfr_filtered |>
    distinct(population_group) |>
    arrange(population_group == "Other", population_group) |>
    pull()
  
  custom_colour_pop_groups <- lanonc_colours[seq_along(all_pop_groups)]
  names(custom_colour_pop_groups) <- all_pop_groups
  
  all_countries <- extracted_cfr_filtered |>
    distinct(population_country) |>
    arrange(population_country) |>
    pull()
  
  custom_colour_countries <- lanonc_colours[seq_along(all_countries)]
  names(custom_colour_countries) <- all_countries
  
  plot_list[[list_label]][["meta"]][["m1"]] <- metaprop_wrap(
    dataframe = extracted_cfr_filtered, subgroup = "population_country", plot_pooled = TRUE,
    sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
    colour = imperial_khaki, width = 12000, height = 14000, resolution = 1000)
  
  ggsave(file.path(plot_type,
                   paste0("CFR_meta_population_country.pdf")),
         plot_list[[list_label]][["meta"]][["m1"]]$plot,
         width = 5.8, height = 5)
  ggsave(file.path(plot_type,
                   paste0("CFR_meta_population_country.png")),
         plot_list[[list_label]][["meta"]][["m1"]]$plot,
         width = 5.8, height = 5)
  
  plot_list[[list_label]][["meta"]][["m2"]] <- metaprop_wrap(
    dataframe = extracted_cfr_filtered, subgroup = "study_midyear_cat",
    plot_pooled = TRUE, sort_by_subg = TRUE,
    plot_study = TRUE, digits = meta_digits,
    colour = imperial_khaki,
    width = 10500, height = 12000, resolution = 1000)
  
  ggsave(file.path(plot_type,
                   paste0("CFR_meta_study_midyear_cat.pdf")),
         plot_list[[list_label]][["meta"]][["m2"]]$plot,
         width = 8.5, height = 7)
  ggsave(file.path(plot_type,
                   paste0("CFR_meta_study_midyear_cat.png")),
         plot_list[[list_label]][["meta"]][["m2"]]$plot,
         width = 8.5, height = 7)
  
  plot_list[[list_label]][["meta"]][["m3"]] <- metaprop_wrap(
    dataframe = extracted_cfr_filtered, subgroup = "population_sample_type",
    plot_pooled = TRUE, sort_by_subg = TRUE,
    plot_study = TRUE, digits = meta_digits,
    colour = imperial_khaki,
    width = 10500, height = 12000, resolution = 1000)
  
  ggsave(file.path(plot_type,
                   paste0("CFR_meta_study_sample_type.pdf")),
         plot_list[[list_label]][["meta"]][["m3"]]$plot,
         width = 8.5, height = 7)
  ggsave(file.path(plot_type,
                   paste0("CFR_meta_study_sample_type.png")),
         plot_list[[list_label]][["meta"]][["m3"]]$plot,
         width = 8.5, height = 7)
  
  
  # plot_list[[list_label]][["meta"]][["m4"]] <- metaprop_wrap(
  #   dataframe = extracted_cfr_filtered, subgroup = "case_def",
  #   plot_pooled = TRUE, sort_by_subg = TRUE,
  #   plot_study = TRUE, digits = meta_digits,
  #   colour = imperial_khaki,
  #   width = 10500, height = 12000, resolution = 1000)
  # 
  # ggsave(file.path(plot_type,
  #                  paste0("CFR_meta_study_?.pdf")),
  #        plot_list[[list_label]][["meta"]][["m4"]]$plot,
  #        width = 8.5, height = 7)
  # ggsave(file.path(plot_type,
  #                  paste0("CFR_meta_study_?.png")),
  #        plot_list[[list_label]][["meta"]][["m4"]]$plot,
  #        width = 8.5, height = 7)
  
  
  
  plot_list[[list_label]][["meta"]][["m3"]] <- metaprop_wrap(
    dataframe = extracted_cfr_filtered, subgroup = "cfr_denom_cat", plot_pooled = TRUE,
    sort_by_subg = FALSE, plot_study = TRUE, digits = meta_digits,
    colour = imperial_khaki, width = 9000, height = 9000, resolution = 1100)
  
  ggsave(file.path(plot_type,
                   paste0("CFR_meta_cfr_denom_cat.pdf")),
         plot_list[[list_label]][["meta"]][["m3"]]$plot,
         width = 9, height = 9)
  ggsave(file.path(plot_type,
                   paste0("CFR_meta_cfr_denom_cat.png")),
         plot_list[[list_label]][["meta"]][["m3"]]$plot,
         width = 9, height = 9)
  
  
  plot_list[[list_label]][["meta"]][["m4"]] <- metaprop_wrap(
    dataframe = extracted_cfr_filtered, subgroup = "population_group", plot_pooled = TRUE,
    sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
    colour = imperial_khaki, width = 12000, height = 12000, resolution = 1000)
  
  ggsave(file.path(plot_type,
                   paste0("CFR_meta_population_group.pdf")),
         plot_list[[list_label]][["meta"]][["m4"]]$plot,
         width = 7, height = 6 )
  ggsave(file.path(plot_type,
                   paste0("CFR_meta_population_group.png")),
         plot_list[[list_label]][["meta"]][["m4"]]$plot,
         width = 7, height = 6)
  
  # Forest plot
  plot_list[[list_label]][["forest"]][["p_cfr_1"]] <- forest_plot(
    extracted_cfr_filtered, "Case-Fatality Ratio (%)", "population_country",
    c(-10,110), custom_colours = custom_colour_countries,
    text_size=text_size, qa_alpha=qa_alpha, sort=TRUE) +
    guides(shape = guide_legend(title = "Parameter type", order=1),
           fill = guide_none(),
           linetype = guide_none(),
           color = guide_legend(title = "Population country", order=3))
  ggsave(file.path(plot_type,
                   paste0("CFR_forest_cfr_population_country.pdf")),
         plot = plot_list[[list_label]][["forest"]][["p_cfr_1"]],
         width = 10, height = 12)
  
  plot_list[[list_label]][["forest"]][["p_cfr_2"]] <- forest_plot(
    extracted_cfr_filtered, "Case-Fatality Ratio (%)","population_group",
    c(-10,110), custom_colours = custom_colour_pop_groups,
    text_size=text_size, qa_alpha=qa_alpha, sort=TRUE) +
    guides(shape = guide_legend(title = "Parameter type", order=1),
           fill =  guide_none(),
           linetype = guide_none(),
           color =  guide_legend(title = "Population group", order=2))
  
  ggsave(file.path(plot_type,
                   paste0("CFR_forest_cfr_population_group.pdf")),
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
                   paste0("CFR_forest_prop_country.pdf")),
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
                   paste0("CFR_forest_prop_population_group.pdf")),
         plot =  plot_list[[list_label]][["forest"]][["p_prop_2"]],
         width = 8, height = 5)
}


# # *----------------------- CFR from extracted outbreaks -----------------------*
# 
# # Plot colour
# imperial_khaki <- "#EFE58B"
# text_size <- 15
# lanonc_colours <- ggsci::pal_lancet("lanonc")(9)
# meta_digits <- 2
# 
# # data frame
# cfr_from_outbreaks <- outbreaks
# # get rid of NA cases;  cases_confirmed, outbreak_probable, cases_suspected, cases_unspecified, cases_asymptomatic
# cfr_from_outbreaks$cases_confirmed_0 <- cfr_from_outbreaks$cases_confirmed 
# cfr_from_outbreaks$cases_confirmed_0[is.na(cfr_from_outbreaks$cases_confirmed_0)] <- 0
# cfr_from_outbreaks$outbreak_probable_0 <- cfr_from_outbreaks$outbreak_probable 
# cfr_from_outbreaks$outbreak_probable_0[is.na(cfr_from_outbreaks$outbreak_probable_0)] <- 0
# cfr_from_outbreaks$cases_suspected_0 <- cfr_from_outbreaks$cases_suspected 
# cfr_from_outbreaks$cases_suspected_0[is.na(cfr_from_outbreaks$cases_suspected_0)] <- 0
# cfr_from_outbreaks$cases_unspecified_0 <- cfr_from_outbreaks$cases_unspecified 
# cfr_from_outbreaks$cases_unspecified_0[is.na(cfr_from_outbreaks$cases_unspecified_0)] <- 0
# cfr_from_outbreaks$cases_asymptomatic_0 <- cfr_from_outbreaks$cases_asymptomatic 
# cfr_from_outbreaks$cases_asymptomatic_0[is.na(cfr_from_outbreaks$cases_asymptomatic_0)] <- 0
# 
# cfr_from_outbreaks <- cfr_from_outbreaks |>
#   mutate(outbreak_source = replace_na(outbreak_source, 'Unknown'),
#          total_cases = cases_confirmed_0 + outbreak_probable_0 + cases_suspected_0 + cases_unspecified_0 + cases_asymptomatic_0) |>
#   mutate(cfr_ifr_denominator = round(total_cases),
#          cfr_ifr_numerator = round(deaths),
#          CFR = cfr_ifr_numerator / cfr_ifr_denominator,
#          article_refs = refs,
#          refs = paste(outbreak_country, outbreak_location, sep = " |> "),
#          parameter_value = CFR,
#          parameter_unit = 'Percentage') |> arrange(desc(CFR)) |>
#   mutate(outbreak_source = case_when(str_detect(outbreak_source, 'Domestic animal' ) ~ 'Domestic animal',
#                                      str_detect(outbreak_source, 'Wild animal' ) ~ 'Wild animal',
#                                      TRUE ~ outbreak_source),
#          refs_full = refs,
#          refs = paste0(
#            trimws(str_extract(refs, "^[^|]+")), " |> ",
#            # first 3 letters of each word in location
#            str_to_upper(str_replace_all(str_extract(refs, "(?<=\\|>).*$"),
#                                         "\\b([A-Za-z]{1,3})[A-Za-z]*\\b",
#                                         "\\1")))
#   )
# 
# # CFR so need deaths 
# cfr_from_outbreaks <- cfr_from_outbreaks|>filter(cfr_ifr_numerator>0, !is.na(cfr_ifr_numerator), !covidence_id%in%c(1774, 1767,1678))
# # Overall
# cfr_outbreak_country <- metaprop_wrap(
#   cfr_from_outbreaks, subgroup = 'outbreak_country', plot_pooled = TRUE,
#   sort_by_subg = TRUE, plot_study = FALSE, digits = meta_digits,
#   colour = imperial_khaki,
#   width = 2700, height = 2500, resolution = 200)
# cfr_meta_analysis <- cfr_outbreak_country$plot
# 
# #save figure
# ggsave(file.path("figure_meta_country_extracted_outbreak.pdf"),
#        cfr_outbreak_country$plot, width = 12, height = 6)
# ggsave(file.path("figure_meta_country_extracted_outbreak.png"),
#        cfr_outbreak_country$plot, width = 12, height = 6)
# 
# # With study breakdown
# # Update refs for nicer study printing
# # max_width <- max(nchar(paste(cfr_from_outbreaks$outbreak_location,
# #                              cfr_from_outbreaks$outbreak_start_year)))
# # cfr_from_outbreaks <- cfr_from_outbreaks |>
# #   mutate(refs = paste0(str_pad(paste(outbreak_location, outbreak_start_year),
# #                                width = 40, side = "right"), " | ",
# #                        article_refs))
# 
# cfr_outbreak_country_study <- metaprop_wrap(
#   cfr_from_outbreaks,
#   subgroup = 'outbreak_country', plot_pooled = TRUE,
#   sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
#   colour = imperial_khaki,
#   width = 11000, height = 17000, resolution = 1000)
# 
# # Update plot to allow for new format
# cfr_outbreak_country_study$result$article_refs <- cfr_from_outbreaks$article_refs
# cfr_outbreak_country_study$result$studlab <- paste0(
#   cfr_from_outbreaks$outbreak_start_year, ", ", cfr_from_outbreaks$outbreak_location)
# 
# colour <- imperial_khaki
# png(file = "temp.png", width = 13000, height = 17000, res = 1000)
# par(mar = c(2, 2, 2, 1))
# cfr_outbreak_country_study_plot <- forest(cfr_outbreak_country_study$result, layout = "Revman5",
#                                           leftcols = c("studlab", "article_refs", "event", "n", "effect.ci"),
#                                           leftlabs = c("Outbreak", "Study", "Events", "Total"),
#                                           just.addcols = "left",
#                                           colgap.forest.left = "3mm",
#                                           overall = TRUE, pooled.events = TRUE,
#                                           print.subgroup.name = FALSE, sort.subgroup = TRUE,
#                                           study.results = TRUE,
#                                           digits = 2,
#                                           col.diamond.lines = "black",col.diamond.common = colour,
#                                           col.diamond.random = colour,
#                                           col.square = colour, col.square.lines = "black",
#                                           col.study = "black", col.subgroup = "black",
#                                           col.inside = "black", weight.study = "same",
#                                           at = seq(0,1,by=0.2), xlim = c(0,1), xlab="Case Fatality Ratio",
#                                           fs.predict.labels = 11.5,
#                                           fs.hetstat=11,
#                                           fs.test.subgroup = 11,
#                                           fs.axis = 11,
#                                           fontsize = 14,
#                                           plotwidth = "72.5mm")
# dev.off()
# 
# pg <- png::readPNG("temp.png", native = TRUE)
# file.remove("temp.png")
# cfr_outbreak_country_study_plot <- wrap_elements(
#   plot = rasterGrob(pg, interpolate = TRUE))
# 
# ggsave(file.path("figures", "SI_CFR_meta_country_extracted_outbreak.pdf"),
#        cfr_outbreak_country_study_plot, width = 5, height = 6.5)
# ggsave(file.path("figures", "SI_CFR_meta_country_extracted_outbreak.png"),
#        cfr_outbreak_country_study_plot,width = 5, height = 6.5)


  




##########################################################################################################
### Main figure plot  - FIGURE 4

## Ideal order:
# attack rate
# delays
# cfr
# cfr_meta_analysis
# genomic_all_qa 

#


################    CRF meta analysis     #############
cfr_meta_analysis <- plot_list[["all"]][["meta"]][["m1"]]$plot


#############      Attack rates       ##################

# Build the base plot first
df <- d4 |> mutate(urefs = make.unique(refs)) |>
  mutate(urefs = factor(urefs, levels = rev(unique(urefs))),
         uncertainty_present = (
           !(is.na(parameter_uncertainty_lower_value) &
               is.na(parameter_uncertainty_upper_value)))
  )|>
  arrange(desc(population_country)) |>
  mutate(
    urefs = fct_inorder(urefs)
  )

attackrate_all_qa_base_plot <-  ggplot(df) +
  geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                   y = urefs, yend = urefs, color = population_country),
               linewidth=3) +
  geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                    y = urefs, linetype="Uncertainty"),
                width = 0.25, lwd=0.5, color = "black") +
  geom_errorbar(data= df[!df$uncertainty_present,],
                aes(xmin=parameter_2_lower_bound, xmax=parameter_2_upper_bound,
                    y = urefs, linetype="Variability"),
                width = 0.25, lwd=0.5, color = "black",
                lineend = "square") +
  geom_errorbar(data= df[df$uncertainty_present,],
                aes(xmin=parameter_2_lower_bound, xmax=parameter_2_upper_bound,
                    y = urefs, linetype="Variability"),
                width = 0.25, lwd=0.5, color = "black",
                lineend = "square", position = position_nudge(y=-0.25)) +
  geom_point(aes(x = parameter_value, y = urefs,
                 shape = parameter_statistical_approach, fill = population_country),
             size = 3, stroke = 1, color = "black") +
  # FIX? commented out to keep colours even if NA
  scale_shape_manual(name = "Statistical approach",
                     values = c(`Observed sample statistic` = 21,
                                `Estimated parameter` = 22, 
                                `Unknown` = 23, 
                                `Unspecified` = 24, `Other`=25),
                     breaks = c("Observed sample statistic", "Estimated parameter", "Unspecified", "Other",
                                "Unknown")) +
  scale_linetype_manual(name   = "Variation Type",
                        values = c("Uncertainty" = "solid","Variability" = "dashed"),
                        breaks = c("Uncertainty", "Variability")) +
  scale_x_continuous(limits = c(-1,30), expand = c(0, 0)) +
  scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
  labs(x = 'Attack Rate (%)', y = NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
        text = element_text(size = text_size))

# same urefs factor, same row order
attackrate_all_qa_plot_df <- attackrate_all_qa_base_plot$data

y_positions_location <- attackrate_all_qa_plot_df |>
  ## this reflects the within facet order 
  mutate(y_key = as.character(interaction(population_country, sep = "--"))) |>
  # factor level order is preserved because country_location_label is already a factor
  arrange(
    desc(population_country),
    interaction(factor(sub("--.*", "", y_key)), population_country),
    .by_group = TRUE
  ) |>
  mutate(y_pos = row_number()) |>
  ungroup()


country_separators <- y_positions_location |>
  group_by(population_country) |>
  summarise(y_min = min(y_pos), y_max = max(y_pos), .groups = "drop") |>
  arrange(y_min,desc(population_country), .by_group = TRUE) |>
  mutate(y_sep = y_min - 0.5) |>
  ungroup()

attackrate_all_qa_country <- attackrate_all_qa_base_plot +
  geom_segment(
    data = country_separators |>
      filter(y_min > min(y_min)) |>
      ungroup(),
    aes(y = y_sep, yend = y_sep, x = -Inf, xend = Inf),
    color = "grey50", linewidth = 0.2, linetype = "solid",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = country_separators, size = 2.5,
    aes(y = y_max + 0.35, x = 0, label = population_country),
    hjust = 0, vjust = 1, size = 1.1, fontface = "bold",
    inherit.aes = FALSE
  )

attackrate_all_qa_country


# attackrate_all_qa <- forest_plot_approach(d4, 'Attack Rate (%)', "population_country", 
#                                           c(-1,20),
#                                           custom_colours = country_palette,
#                                           text_size=text_size) 
# # + 
#   facet_wrap(~parameter_statistical_approach, ncol=1,scales="free_y", space="free_y")
  #facet_grid(parameter_statistical_approach ~ ., scales = "free_y", space = "free_y")





#################                  CFR PLOT             ################################################

# Build the base plot first
df <- d7 |> mutate(study_midyear = ifelse(!is.na(population_study_start_year) & !is.na(population_study_end_year),
                                  round((population_study_start_year + population_study_end_year) / 2),
                                  population_study_start_year)) |>
  arrange((population_sample_size)) |>
  mutate(
    urefs = fct_inorder(urefs)
  )

base_plot <-  ggplot(df) +
  geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                   y = urefs, yend = urefs, color = population_country),
               linewidth=3, alpha=0.75) +
  geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                    y = urefs, linetype="Uncertainty"),
                width = 0.25, lwd=0.5, color = "black") +
  geom_errorbar(data= df[!df$uncertainty_present,],
                aes(xmin=parameter_2_lower_bound, xmax=parameter_2_upper_bound,
                    y = urefs, linetype="Variability"),
                width = 0.25, lwd=0.5, color = "black",
                lineend = "square") +
  geom_errorbar(data= df[df$uncertainty_present,],
                aes(xmin=parameter_2_lower_bound, xmax=parameter_2_upper_bound,
                    y = urefs, linetype="Variability"),
                width = 0.25, lwd=0.5, color = "black",
                lineend = "square", position = position_nudge(y=-0.25)) +
  geom_point(aes(x = parameter_value, y = urefs,
                 shape = parameter_statistical_approach, fill = population_country),
             size = 3, stroke = 1, color = "black") +
 # facet_grid(population_sample_type ~ ., scales = "free_y", space = "free_y") + # scale_fill_lancet(palette = "lanonc") + scale_color_lancet(palette = "lanonc") +
  facet_wrap(~population_sample_type, scales = "free_y", space = "free_y") + # scale_fill_lancet(palette = "lanonc") + scale_color_lancet(palette = "lanonc") +
  # FIX? commented out to keep colours even if NA
  scale_shape_manual(name = "Statistical approach",
                     values = c(`Observed sample statistic` = 21,
                                `Estimated parameter` = 22, 
                                `Unknown` = 23, 
                                `Unspecified` = 24, `Other`=25),
                     breaks = c("Observed sample statistic", "Estimated parameter", "Unspecified", "Other",
                                "Unknown")) +
  scale_linetype_manual(name   = "Variation Type",
                        values = c("Uncertainty" = "solid","Variability" = "dashed"),
                        breaks = c("Uncertainty", "Variability")) +
  scale_x_continuous(limits = c(-6,110), expand = c(0, 0)) +
  scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
  labs(x = "Case-Fatality Ratio (%)", y = NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
        text = element_text(size = text_size))

# same urefs factor, same row order
  plot_df <- base_plot$data

y_positions_location <- plot_df |>
  ## this reflects the within facet order 
 mutate(y_key = as.character(interaction(population_sample_size, sep = "--"))) |>
  distinct(population_sample_type,population_sample_size, y_key, population_country) |>
  group_by(population_sample_type) |>
  
  # factor level order is preserved because country_location_label is already a factor
  arrange(
    population_sample_size, population_sample_type,
    interaction(factor(sub("--.*", "", y_key)), population_sample_size),
    .by_group = TRUE
  ) |>
  mutate(y_pos = row_number()) |>
  ungroup()


country_separators <- y_positions_location |>
  group_by(population_sample_type, population_sample_size, population_country) |>
  summarise(y_min = min(y_pos), y_max = max(y_pos), .groups = "drop") |>
  group_by(population_sample_type) |>
  arrange(population_sample_size,y_min, .by_group = TRUE) |>
  mutate(y_sep = y_min - 0.5) |>
  ungroup()

cfr_all_final <- base_plot +
  geom_segment(
    data = country_separators |>
      group_by(population_sample_type) |>
      filter(y_min > min(y_min)) |>
      ungroup(),
    aes(y = y_sep, yend = y_sep, x = -Inf, xend = Inf),
    color = "grey50", linewidth = 0.2, linetype = "solid",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = country_separators, size = 2.5,
    aes(y = y_max + 0.35, x = -5, label = population_country),
    hjust = 0, vjust = 1, size = 1.1, fontface = "bold",
    inherit.aes = FALSE
  )

cfr_all_final

######################     Genomics plot      ############################################

# Build the base plot first
# fix country name issue - samples compared to sample from range of countries 
genomic_da$population_country[which(genomic_da$access_param_id=="526_001")] <- "Various"
genomic_da$population_country[which(genomic_da$access_param_id=="526_002")] <- "Various"
genomic_da$population_country[which(genomic_da$access_param_id=="526_003")] <- "Various"
genomic_da$population_country[which(genomic_da$access_param_id=="541_001")] <- "Various"
genomic_da$population_country[which(genomic_da$access_param_id=="0688_0002")] <- "Various"
genomic_da$population_country[which(genomic_da$access_param_id=="0688_0001")] <- "Various"
genomic_da$population_country[which(genomic_da$access_param_id=="0688_0003")] <- "Various"
genomic_da$population_country[which(genomic_da$access_param_id=="0716_0001")] <- "Various"

genomic_da <- genomic_da|> mutate(genome_site_clean = case_when(genome_site%in%c("'Large' segment RNA-dependent RNA polymerase (RdRp",
                                                                                 "L segment", "gerne - L segment" ,"L") ~ "L segment",
                                                                genome_site%in%c("gene - M Segment", "M segment","M" ) ~ "M segment",
                                                                genome_site%in%c("gene - S segment", "S segment", "S" ) ~ "S segment",
                                                                genome_site%in%c("Rates estimated per segment", "L; M; S" ) ~ "L/M/S"))|>
  mutate(genome_site_clean = factor(genome_site_clean, levels = c( "L segment","M segment", "S segment", "L/M/S")))|>
  mutate(urefs = make.unique(refs)) |>
  mutate(urefs = factor(urefs, levels = rev(unique(urefs))),
         uncertainty_present = (
           !(is.na(parameter_uncertainty_lower_value) &
               is.na(parameter_uncertainty_upper_value)))
  )|>
  mutate(
    urefs = factor(urefs, levels = study_order)
  )|>
  arrange(desc(population_country)) |>
  mutate(
    urefs = (fct_inorder(urefs))
  )



# 
# base_plot <-  ggplot(genomic_da) +
#   geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
#                    y = urefs, yend = urefs, color = population_country),
#                linewidth=3, alpha =  0.75) +
#   geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
#                     y = urefs, linetype="Uncertainty"),
#                 width = 0.25, lwd=0.5, color = "black") +
#   geom_errorbar(data= genomic_da[!genomic_da$uncertainty_present,],
#                 aes(xmin=parameter_2_lower_bound, xmax=parameter_2_upper_bound,
#                     y = urefs, linetype="Variability"),
#                 width = 0.25, lwd=0.5, color = "black",
#                 lineend = "square") +
#   geom_errorbar(data= genomic_da[genomic_da$uncertainty_present,],
#                 aes(xmin=parameter_2_lower_bound, xmax=parameter_2_upper_bound,
#                     y = urefs, linetype="Variability"),
#                 width = 0.25, lwd=0.5, color = "black",
#                 lineend = "square", position = position_nudge(y=-0.25)) +
#   geom_point(aes(x = parameter_value, y = urefs,
#                  shape = parameter_statistical_approach, fill = population_country),
#              size = 3, stroke = 1, color = "black") +
#   facet_grid(genome_site_clean ~ ., scales = "free_y", space = "free_y") + # scale_fill_lancet(palette = "lanonc") + scale_color_lancet(palette = "lanonc") +
#   # FIX? commented out to keep colours even if NA
#   scale_shape_manual(name = "Statistical approach",
#                      values = c(`Observed sample statistic` = 21,
#                                 `Estimated parameter` = 22, 
#                                 `Unknown` = 23, 
#                                 `Unspecified` = 24, `Other`=25),
#                      breaks = c("Observed sample statistic", "Estimated parameter", "Unspecified", "Other",
#                                 "Unknown")) +
#   scale_linetype_manual(name   = "Variation Type",
#                         values = c("Uncertainty" = "solid","Variability" = "dashed"),
#                         breaks = c("Uncertainty", "Variability")) +
#   scale_x_continuous(limits = c(-6,110), expand = c(0, 0)) +
#   scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
#   labs(x = "Case-Fatality Ratio (%)", y = NULL) +
#   theme_minimal() +
#   theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
#         text = element_text(size = text_size))


genomic_all_qa <- forest_plot_approach(genomic_da,
                               expression(paste("Evolutionary/Substitution")~Rate~(s/s/y ~10^{-4})),
                               "population_country",c(-1,30),
                                custom_colours = country_palette,
                               text_size=text_size) +
   facet_grid(genome_site_clean ~ ., scales = "free_y", space = "free_y")

 

# same urefs factor, same row order
plot_df_genom <- genomic_all_qa$data

y_positions_location <- plot_df_genom |>
  ## this reflects the within facet order 
  mutate(y_key = as.character(interaction(genome_site_clean,population_country, sep = "--"))) |>
  #distinct(genome_site_clean, y_key, population_country) |>
  group_by(genome_site_clean) |>
  
  # factor level order is preserved because country_location_label is already a factor
  arrange(
    desc(population_country), genome_site_clean,
    interaction(factor(sub("--.*", "", y_key)), population_country),
    .by_group = TRUE
  ) |>
  mutate(y_pos = rev(seq_len(dplyr::n()))) |>
  ungroup()


country_separators <- y_positions_location |>
  group_by(genome_site_clean, population_country) |>
  summarise(y_min = min(y_pos), y_max = max(y_pos), .groups = "drop") |>
  group_by(genome_site_clean) |>
  arrange(y_min,desc(population_country), .by_group = TRUE) |>
  mutate(y_sep = y_min - 0.5) |>
  ungroup()

genomic_all_qa_country <- genomic_all_qa +
  geom_segment(
    data = country_separators |>
      group_by(genome_site_clean) |>
      filter(y_min > min(y_min)) |>
      ungroup(),
    aes(y = y_sep, yend = y_sep, x = -Inf, xend = Inf),
    color = "grey50", linewidth = 0.2, linetype = "solid",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = country_separators, size = 2.5,
    aes(y = y_max + 0.35, x = 0, label = population_country),
    hjust = 0, vjust = 1, size = 1.1, fontface = "bold",
    inherit.aes = FALSE
  )

genomic_all_qa_country


##########            Delay plot            ####################
# load from delay task
load("figure_delays.RData")

# fix legend issue
delay_legend <- cowplot::get_legend(delays_final+theme(legend.position = "bottom")+ guides( color = "none")+labs(colour=NULL))

delay_plot<- cowplot::plot_grid(delays_final+theme(legend.position = "bottom") + guides( color = "none")+labs(colour=NULL) +
                                  theme(legend.position = "bottom",                # Legend at bottom
                                        axis.title.x = element_text(margin = margin(t = 10, b = 0)), # Add space above label
                                        plot.margin = margin(10, 10, 10, 10)       # Adjust plot margins
                                  ), 
                                delay_legend,
                                ncol=1, labels=c("","") , align = c("h"), 
                                rel_heights = c(10,1))



#####################         COMBINE all plots                 ###################################################################################################################

# figure_4_top <- cowplot::plot_grid(attackrate_all_qa + theme(legend.box = "horizontal", legend.position = "bottom", legend.direction = "vertical")+ guides(colour = "none"), 
#                                    genomic_all_qa+theme(legend.position = "bottom") + guides(shape = "none", linetype = "none",
#                                                                                             color = guide_legend(ncol = 2))+labs(colour=NULL),
#                                    nrow=1, labels=c("A","C") , align = c("h"))
# 
# figure_4_bottom <- cowplot::plot_grid(delays_all_qa_facet+theme(legend.position = "bottom") + guides(shape = "none", linetype = "none",
#                                                                                                      color = "none")+labs(colour=NULL), 
#                                                       cfr_all_qa_country+theme(legend.position = "bottom") + guides(shape = "none", linetype = "none",
#                                                                                                                color = guide_legend(ncol = 3))+labs(colour=NULL),
#                                                       nrow=1, labels=c("B","D"), align = c("h") )
# 
# 
# figure_4_left <-  cowplot::plot_grid(figure_4_top, 
#                                      figure_4_bottom,
#                                      labels=NULL, nrow=2, ncol=1, 
#                                      rel_heights = c(2,3),
#                                      align = c("hv"))
# 


# Figure 4 left hand column
figure_4_L <- cowplot::plot_grid(attackrate_all_qa_country + theme(legend.position = "none")+ guides(shape = "none", linetype = "none",
                                                                                                                                                         colour = "none"), 
                                 delays_final + guides( color = "none")+labs(colour=NULL),
                                   ncol=1, labels=c("A","B") , align = c("h"), 
                                  rel_heights = c(1.75,3))

# Figure 4 right hand column
cfr_C <- cfr_all_final+theme(legend.position = "none") + guides(shape = "none", linetype = "none",
                                                             color = "none")+labs(colour=NULL)
text_box <- textGrob("place holder", gp = gpar(fontsize = 16))

figure_4_M <- cowplot::plot_grid(text_box,
                                 genomic_all_qa_country+theme(legend.position = "none") + guides(shape = "none", linetype = "none",
                                                                                           color = "none")+labs(colour=NULL),
                                 ncol=1, labels=c("D","E"), align = c("h"), 
                                 rel_heights = c(7.5,5) )


# Plot for full colour legend
colour_legend <- forest_plot_approach(full_join(d4,d7),
                                      'All parameters', "population_country",
                                      c(-3, 110),
                                      text_size=text_size,
                                      custom_colours = country_palette,
                                      qa_alpha =0.3)+
  facet_wrap(~parameter_type, 
             ncol=1, scales="free_y") +
  theme(legend.position = "bottom" , legend.box = "horizontal") + 
  guides(color = guide_legend(ncol = 4)) + 
  labs(colour=NULL)

# Save CFR meta analysis
ggsave("figures/colour_legend_full.png", plot = colour_legend, width = 17, height = 10)

# Place holder for CFR meta analysis (add after in powerpoint)
ggsave(file.path("figures", "cfr_meta_analysis_country.pdf"),
       cfr_meta_analysis, width = 5, height = 6.5)
ggsave(file.path("figures", "cfr_meta_analysis_country.png"),
       cfr_meta_analysis,width = 5, height = 6.5)


# Figure 4
figure_4 <-  cowplot::plot_grid(figure_4_L,
                                cfr_C,
                                figure_4_M,
                                     nrow=1, labels=c("","C",""), rel_widths = c(1,1,1.25))
# Save Figure 4
ggsave("figures/figure_4.png", plot = figure_4, width = 20, height = 13)
ggsave("figures/figure_4.pdf", plot = figure_4, width = 20, height = 13)

