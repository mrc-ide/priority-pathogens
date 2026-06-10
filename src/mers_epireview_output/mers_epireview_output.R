# *=============================== MERS outputs ===============================*
library(dplyr)
library(orderly)
library(readr)
library(stringr)
# *--------------------------------- Orderly ----------------------------------*
orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "params.csv"))

orderly_shared_resource("mers_functions.R" = "mers_functions.R")
source("mers_functions.R")

orderly_artefact(description="MERS outputs",
                 c("mers_articles.csv",
                   "mers_parameters.csv",
                   "mers_models.csv"))

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- tibble()
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)


mers_articles <- articles[,c("id", "covidence_id","pathogen",
                             "first_author_first_name", "first_author_surname",
                             "article_title", "doi", "journal", "year_publication",
                             "volume", "issue", "page_first", "page_last",
                             "paper_copy_only", "notes",
                             "qa_m1", "qa_m2", "qa_a3", "qa_a4", "qa_d5", "qa_d6", "qa_d7",
                             "covidence_id", #This will need to be renamed
                             "double_extracted", "article_label"
                             )]

colnames(mers_articles) <- c("id", "covidence_id","pathogen",
                             "first_author_first_name", "first_author_surname",
                             "article_title", "doi", "journal", "year_publication",
                             "volume", "issue", "page_first", "page_last",
                             "paper_copy_only", "notes",
                             "qa_m1", "qa_m2", "qa_a3", "qa_a4", "qa_d5", "qa_d6", "qa_d7",
                             "covidence_id_text",
                             "double_extracted", "article_label"
)

write.csv(mers_articles, "mers_articles.csv", row.names = FALSE)

parameters <- dfs$parameters
mers_parameters <- parameters[,c("id", "parameter_data_id", "covidence_id", "pathogen",
                             "parameter_type", "parameter_value", "exponent",
                             "parameter_unit",	"parameter_lower_bound",	"parameter_upper_bound",
                             "parameter_value_type",	"parameter_uncertainty_single_value",
                             "parameter_uncertainty_single_type",
                             "parameter_uncertainty_lower_value",	"parameter_uncertainty_upper_value",
                             "parameter_uncertainty_type",
                             "cfr_ifr_numerator",	"cfr_ifr_denominator",
                             "distribution_type",	"distribution_par1_value",
                             "distribution_par1_type",	"distribution_par1_uncertainty",
                             "distribution_par2_value",
                             "distribution_par2_type",	"distribution_par2_uncertainty",
                             "method_from_supplement",	"method_moment_value",
                             "cfr_ifr_method",	"method_r",
                             "method_disaggregated_by",	"method_disaggregated",
                             "method_disaggregated_only",
                             "riskfactor_outcome",	"riskfactor_name",	"riskfactor_occupation",
                             "riskfactor_significant",	"riskfactor_adjusted",
                             "population_sex",	"population_sample_type",	"population_group",
                             "population_age_min",	"population_age_max",
                             "population_sample_size",	"population_country",
                             "population_location",
                             "population_study_start_day",	"population_study_start_month",
                             "population_study_start_year",	"population_study_end_day",
                             "population_study_end_month",	"population_study_end_year",
                             "genome_site",	"genomic_sequence_available",
                             "other_delay_start",	"other_delay_end",
                             "inverse_param",	"parameter_from_figure",
                             "r_pathway",	"parameter_class",	"parameter_bounds",
                             "comb_par1_uncertainty_type",	"comb_par1_uncertainty"
)]

colnames(mers_parameters) <- c("id", "parameter_data_id", "covidence_id", "pathogen",
                                 "parameter_type", "parameter_value", "exponent",
                                 "parameter_unit",	"parameter_lower_bound",	"parameter_upper_bound",
                                 "parameter_value_type",	"parameter_uncertainty_single_value",
                                 "parameter_uncertainty_single_type",
                                 "parameter_uncertainty_lower_value",	"parameter_uncertainty_upper_value",
                                 "parameter_uncertainty_type",
                                 "cfr_ifr_numerator",	"cfr_ifr_denominator",
                                 "distribution_type",	"distribution_par1_value",
                                 "distribution_par1_type",	"distribution_par1_uncertainty",
                                 "distribution_par2_value",
                                 "distribution_par2_type",	"distribution_par2_uncertainty",
                                 "method_from_supplement",	"method_moment_value",
                                 "cfr_ifr_method",	"method_r",
                                 "method_disaggregated_by",	"method_disaggregated",
                                 "method_disaggregated_only",
                                 "riskfactor_outcome",	"riskfactor_name",	"riskfactor_occupation",
                                 "riskfactor_significant",	"riskfactor_adjusted",
                                 "population_sex",	"population_sample_type",	"population_group",
                                 "population_age_min",	"population_age_max",
                                 "population_sample_size",	"population_country",
                                 "population_location",
                                 "population_study_start_day",	"population_study_start_month",
                                 "population_study_start_year",	"population_study_end_day",
                                 "population_study_end_month",	"population_study_end_year",
                                 "genome_site",	"genomic_sequence_available",
                                 "other_delay_start",	"other_delay_end",
                                 "inverse_param",	"parameter_from_figure",
                                 "r_pathway",	"parameter_class",	"parameter_bounds",
                                 "comb_uncertainty_type",	"comb_uncertainty"
)

mers_parameters$distribution_par1_uncertainty <- mers_parameters$distribution_par1_uncertainty == "yes (ONLY tick if this applies)"
mers_parameters$distribution_par2_uncertainty <- mers_parameters$distribution_par2_uncertainty == "yes (ONLY tick if this applies)"
mers_parameters$method_from_supplement <- mers_parameters$method_from_supplement == "Yes"
mers_parameters$method_disaggregated <- mers_parameters$method_disaggregated == "Yes"
mers_parameters$method_disaggregated_only <- mers_parameters$method_disaggregated_only == "Yes"
mers_parameters$genomic_sequence_available <- mers_parameters$genomic_sequence_available == "Yes"


write.csv(mers_parameters, "mers_parameters.csv", row.names = FALSE)

models <- dfs$models

mers_models <- models[,c(
  "id",	"model_data_id",	"covidence_id",	"pathogen",	"model_type",
  "compartmental_type",	"stoch_deter",
  "theoretical_model",	#This needs to be converted to TRUE/FALSE
  "interventions_type",
  "model_uncertainty", "model_spatial", "model_spillover", #These have been newly added
  "code_available",	#This needs to be converted to TRUE/FALSE
  "transmission_route",	"assumptions"
)]

mers_models$theoretical_model <- mers_models$theoretical_model == "Yes"
mers_models$model_uncertainty <- mers_models$model_uncertainty == "Yes"
mers_models$model_spatial <- mers_models$model_spatial == "Yes"
mers_models$model_spillover <- mers_models$model_spillover == "Yes"
mers_models$code_available <- mers_models$code_available == "Yes"

write.csv(mers_models, "mers_models.csv", row.names = FALSE)
