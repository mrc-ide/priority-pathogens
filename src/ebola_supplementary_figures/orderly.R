## Supplementary tables and figures

library(png)
library(officer)
library(orderly2)
library(flextable)
library(grid)
library(gridExtra)
library(ggplot2)

orderly_strict_mode()

orderly_parameters(pathogen = "EBOLA")

orderly_artefact(
  "Figures and tables for the supplementary material",
  c(
    "Supplementary_figures/basic_r_table.docx",
    "Supplementary_figures/effective_r_table.docx",
    "Supplementary_figures/seroprevalence_table.png",
    "Supplementary_figures/mutations_table.png",
    "Supplementary_figures/primary_attack_rate_table.png",
    "Supplementary_figures/secondary_attack_rate_table.png",
    "Supplementary_figures/growth_rate_table.png",
    "Supplementary_figures/overdispersion_table.png",
    "Supplementary_figures/doubling_time_table.png",
    "Supplementary_figures/risk_factors_infection.png",
    "Supplementary_figures/risk_factors_serology.png",
    "Supplementary_figures/risk_factors_severity.png",
    "Supplementary_figures/risk_factors_severity_text.png", # can remove this later
    "Supplementary_figures/risk_factors_onward_transmission.png",
    "Supplementary_figures/delays_symptom_onset_to_X.png",
    "Supplementary_figures/delays_admission_to_X.png",
    "Supplementary_figures/delays_infection_process.png",
    "Supplementary_figures/delays_death_to_burial.png",
    "Supplementary_figures/delays_meta_analysis.png"
  )
)

# Get figures/tables from tasks
orderly_dependency("ebola_reproduction_number",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("R_tables/unfiltered/paginate_basic_r_tab_all.docx",
                             "R_tables/unfiltered/paginate_effective_r_tab_all.docx"))
orderly_dependency("ebola_seroprevalence",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Seroprevalence_tables/tab_unfiltered.png"))
orderly_dependency("ebola_severity",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Severity_tables/unfiltered/paginate_severity_all.docx"))
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
                             "Meta_plots/meta_delays_variance_unfiltered.png"))

dir.create("Supplementary_figures") # copy this folder across to overleaf

##################
## TRANSMISSION ##
##################

# Basic reproduction number unfiltered table
file.copy(from = "R_tables/unfiltered/paginate_basic_r_tab_all.docx",
          to = "Supplementary_figures/basic_r_table.docx")

# Effective reproduction number unfiltered table
file.copy(from = "R_tables/unfiltered/paginate_effective_r_tab_all.docx",
          to = "Supplementary_figures/effective_r_table.docx")

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
file.copy(from = "Meta_plots/meta_delays_variance_unfiltered.png",
          to = "Supplementary_figures/delays_meta_analysis.png")

####################
## SEROPREVALENCE ##
####################

# Unfiltered table
file.copy(from = "Seroprevalence_tables/tab_unfiltered.png",
          to = "Supplementary_figures/seroprevalence_table.png")

# Risk factors for serology
file.copy(from = "Risk_tables/risk_factors_for_serology.png",
          to = "Supplementary_figures/risk_factors_serology.png")

##############
## SEVERITY ##
##############

# Unfiltered table for CFR

# This doesn't work for me so will have to manually convert to pdf:
#docx2pdf("Severity_tables/unfiltered/paginate_severity_all.docx")

file.copy(from = "Severity_tables/unfiltered/paginate_severity_all.docx",
          to = "Supplementary_figures/severity_table.docx")

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
ggsave("Supplementary_figures/risk_factors_severity.png",
       grid.arrange(tab3, tab4, ncol = 1), width = 8, height = 5)

###############
## MUTATIONS ##
###############

# unfiltered table
file.copy(from = "Mutation_results/qa_unfiltered/table.png",
          to = "Supplementary_figures/mutations_table.png")

