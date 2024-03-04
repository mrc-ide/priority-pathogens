## Supplementary tables and figures

library(png)
library(officer)
library(orderly2)
library(flextable)
library(grid)
library(gridExtra)
library(ggplot2)
library(cowplot)

orderly_strict_mode()

orderly_parameters(pathogen = "EBOLA")

orderly_artefact(
  "Figures and tables for the supplementary material",
  c(
    # R
    "Supplementary_figures/basic_r_table.pdf",
    "Supplementary_figures/effective_r_table.pdf",
    # CFR
    "Supplementary_figures/severity_table.pdf",
    # Seroprevalence
    "Supplementary_figures/seroprevalence_history.png",
    "Supplementary_figures/seroprevalence_no_history.png",
    # Mutations
    "Supplementary_figures/mutations_table.png",
    # Attack rate
    "Supplementary_figures/primary_attack_rate_table.png",
    "Supplementary_figures/secondary_attack_rate_table.png",
    # Growth rate
    "Supplementary_figures/growth_rate_table.png",
    # Overdispersion
    "Supplementary_figures/overdispersion_table.png",
    # Doubling time
    "Supplementary_figures/doubling_time_table.png",
    # Risk factors
    "Supplementary_figures/risk_factors_infection.png",
    "Supplementary_figures/risk_factors_serology.png",
    "Supplementary_figures/risk_factors_severity.png",
    "Supplementary_figures/risk_factors_severity_text.png", # can remove this later
    "Supplementary_figures/risk_factors_onward_transmission.png",
    # Delays
    "Supplementary_figures/delays_symptom_onset_to_X.png",
    "Supplementary_figures/delays_admission_to_X.png",
    "Supplementary_figures/delays_infection_process.png",
    "Supplementary_figures/delays_death_to_burial.png",
    "Supplementary_figures/delays_meta_analysis_species.png",
    "Supplementary_figures/delays_meta_analysis_unfiltered.png",
    # Models
    "Supplementary_figures/models_table.pdf",
    "Supplementary_figures/models_assumptions.png",
    "Supplementary_figures/models_types.png",
    # Summary
    "Supplementary_figures/parameter_type_table.png",
    "Supplementary_figures/parameter_group_table.png",
    "Supplementary_figures/parameter_qa_scores.png"
  )
)

# Get figures/tables from tasks
orderly_dependency("ebola_reproduction_number",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("R_tables/unfiltered/paginate_basic_r_tab_all.pdf",
                             "R_tables/unfiltered/paginate_effective_r_tab_all.pdf"))
orderly_dependency("ebola_seroprevalence",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Seroprevalence_tables/seroprevalence_history.png",
                             "Seroprevalence_tables/seroprevalence_no_history.png"))
orderly_dependency("ebola_severity",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Severity_tables/unfiltered/paginate_severity_all.pdf"))
orderly_dependency("ebola_mutations",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Mutation_results/qa_unfiltered/table.png"))
orderly_dependency("ebola_attack_rate",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Attack_rate_results/primary_table_unfiltered.png",
                             "Attack_rate_results/secondary_table_unfiltered.png"))
orderly_dependency("ebola_growth_rate",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Growth_rate_results/table.png"))
orderly_dependency("ebola_overdispersion",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Overdispersion_results/table.png"))
orderly_dependency("ebola_doubling_time",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Doubling_time_results/table.png"))
orderly_dependency("ebola_risk_factors",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Risk_tables/risk_factors_for_death.png",
                             "Risk_tables/risk_factors_for_infection.png",
                             "Risk_tables/risk_factors_for_serology.png",
                             "Risk_tables/protective_factors_for_recovery.png",
                             "Risk_tables/risk_factors_for_severe_disease.png",
                             "Risk_tables/risk_factors_for_symptoms.png",
                             "Risk_tables/other_risk_factor_outcomes.png",
                             "Risk_tables/risk_factors_for_onward_transmission.png"))
orderly_dependency("ebola_delays",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Delay_plots/unfiltered/symp_plot_unfiltered_SPLIT.png",
                             "Delay_plots/unfiltered/adm_plot_unfiltered.png",
                             "Delay_plots/unfiltered/infp_plot_unfiltered.png",
                             "Delay_plots/unfiltered/dtb_plot_unfiltered.png",
                             "Meta_plots/meta_delays_species_subgroups.png",
                             "Meta_plots/meta_delays_unfiltered.png"))
orderly_dependency("ebola_models",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Model_results/overview_table.pdf",
                             "Model_results/assumptions_table.png",
                             "Model_results/model_type_table.png"))
orderly_dependency("ebola_summary",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Summary_results/parameter_type_table.png",
                             "Summary_results/parameter_group_table.png",
                             "Summary_results/parameter_qa_scores.png"))

dir.create("Supplementary_figures") # copy this folder across to overleaf

##################
## TRANSMISSION ##
##################

# Basic reproduction number unfiltered table
file.copy(from = "R_tables/unfiltered/paginate_basic_r_tab_all.pdf",
          to = "Supplementary_figures/basic_r_table.pdf")

# Effective reproduction number unfiltered table
file.copy(from = "R_tables/unfiltered/paginate_effective_r_tab_all.pdf",
          to = "Supplementary_figures/effective_r_table.pdf")

# Growth rate unfiltered table
file.copy(from = "Growth_rate_results/table.png",
          to = "Supplementary_figures/growth_rate_table.png")

# Doubling time unfiltered table
file.copy(from = "Doubling_time_results/table.png",
          to = "Supplementary_figures/doubling_time_table.png")

# Attack rate unfiltered table (primary and secondary separate)
file.copy(from = "Attack_rate_results/primary_table_unfiltered.png",
          to = "Supplementary_figures/primary_attack_rate_table.png")

file.copy(from = "Attack_rate_results/secondary_table_unfiltered.png",
          to = "Supplementary_figures/secondary_attack_rate_table.png")

# Overdispersion unfiltered table
file.copy(from = "Overdispersion_results/table.png",
          to = "Supplementary_figures/overdispersion_table.png")

# Risk factors for infection
file.copy(from = "Risk_tables/risk_factors_for_infection.png",
          to = "Supplementary_figures/risk_factors_infection.png")
file.copy(from = "Risk_tables/risk_factors_for_onward_transmission.png",
          to = "Supplementary_figures/risk_factors_onward_transmission.png")

# Other risk factor outcomes considered in papers
file.copy(from = "Risk_tables/other_risk_factor_outcomes.png",
          to = "Supplementary_figures/risk_factors_other_outcomes.png")

############
## DELAYS ##
############

# Unfiltered delay plots for symptom onset to X, admission to X, infection process and death to burial
file.copy(from = "Delay_plots/unfiltered/symp_plot_unfiltered_SPLIT.png",
          to = "Supplementary_figures/delays_symptom_onset_to_X.png")

file.copy(from = "Delay_plots/unfiltered/adm_plot_unfiltered.png",
          to = "Supplementary_figures/delays_admission_to_X.png")

file.copy(from = "Delay_plots/unfiltered/infp_plot_unfiltered.png",
          to = "Supplementary_figures/delays_infection_process.png")

file.copy(from = "Delay_plots/unfiltered/dtb_plot_unfiltered.png",
          to = "Supplementary_figures/delays_death_to_burial.png")

# Unfiltered delays meta-analysis (to compare to main text)
file.copy(from = "Meta_plots/meta_delays_species_subgroups.png",
          to = "Supplementary_figures/delays_meta_analysis_species.png")

file.copy(from = "Meta_plots/meta_delays_unfiltered.png",
          to = "Supplementary_figures/delays_meta_analysis_unfiltered.png")

####################
## SEROPREVALENCE ##
####################

# Unfiltered table for countries with history of officially reported EVD cases
file.copy(from = "Seroprevalence_tables/seroprevalence_history.png",
          to = "Supplementary_figures/seroprevalence_history.png")

# Unfiltered table for countries with NO history of officially reported EVD cases
# OR multi-country analyses (including countries with and without history)
file.copy(from = "Seroprevalence_tables/seroprevalence_no_history.png",
          to = "Supplementary_figures/seroprevalence_no_history.png")

# Risk factors for serology
file.copy(from = "Risk_tables/risk_factors_for_serology.png",
          to = "Supplementary_figures/risk_factors_serology.png")

##############
## SEVERITY ##
##############

# Unfiltered table for CFR
file.copy(from = "Severity_tables/unfiltered/paginate_severity_all.pdf",
          to = "Supplementary_figures/severity_table.pdf")

# Risk factors associated with severity: death, symptoms, severe disease, recovery
rf_symptoms <- png::readPNG("Risk_tables/risk_factors_for_symptoms.png")
rf_severe_disease <- png::readPNG("Risk_tables/risk_factors_for_severe_disease.png")
rf_death <- png::readPNG("Risk_tables/risk_factors_for_death.png")
rf_recovery <- png::readPNG("Risk_tables/protective_factors_for_recovery.png")

# Risk factors to refer to in text (saving them to remember to do this!)
tab1 <- rasterGrob(rf_symptoms)
tab2 <- rasterGrob(rf_severe_disease)
ggsave("Supplementary_figures/risk_factors_severity_text.png",
       grid.arrange(tab1, tab2, ncol = 1), width = 3, height = 2)

# Risk factors for death and protective factors for recovery
tab3 <- rasterGrob(rf_death, width = 1)
tab4 <- rasterGrob(rf_recovery, width = 0.8)

labeled_sev <- plot_grid(
  tab3, tab4,
  labels = c("A", "B"),
  label_size = 12,
  label_x = 0, label_y = c(1.1, 0.75),
  hjust = -0.5, vjust = -0.5,
  ncol = 1) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(30, 0, 0, 0)
    )

ggsave("Supplementary_figures/risk_factors_severity.png", labeled_sev, width = 8, height = 4.5)


###############
## MUTATIONS ##
###############

# unfiltered table
file.copy(from = "Mutation_results/qa_unfiltered/table.png",
          to = "Supplementary_figures/mutations_table.png")


############
## MODELS ##
############

# unfiltered table
file.copy(from = "Model_results/overview_table.pdf",
          to = "Supplementary_figures/models_table.pdf")

# table of model assumptions
file.copy(from = "Model_results/assumptions_table.png",
          to = "Supplementary_figures/models_assumptions.png")

# table of model types
file.copy(from = "Model_results/model_type_table.png",
          to = "Supplementary_figures/models_types.png")


#############
## SUMMARY ##
#############

# parameter type table
file.copy(from = "Summary_results/parameter_type_table.png",
          to = "Supplementary_figures/parameter_type_table.png")

# parameter group table
file.copy(from = "Summary_results/parameter_group_table.png",
          to = "Supplementary_figures/parameter_group_table.png")

# qa scores
file.copy(from = "Summary_results/parameter_qa_scores.png",
          to = "Supplementary_figures/parameter_qa_scores.png")

