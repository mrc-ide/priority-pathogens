# Task to identify entries of double extracted data that match or do not match 
# between extractors

library(orderly2)
orderly_strict_mode()

orderly_artefact(
  "Double extraction matches and mismatches as csv files",
  c("qa_fixing.csv", "models_fixing.csv", "params_fixing.csv",
    "outbreaks_fixing.csv", "qa_matching.csv", "models_matching.csv",
    "params_matching.csv", "outbreaks_matching.csv"))

# Update this to take the results from db_extraction taskid
orderly_resource(c("double_extraction_articles.csv",
                   "double_extraction_params.csv",
                   "double_extraction_models.csv",
                   "sorting.R"))


library(dplyr)
library(readr)
articles <- read_csv("double_extraction_articles.csv")
parameters <- read_csv("double_extraction_params.csv")
models <- read_csv("double_extraction_models.csv")
outbreaks <- NULL

source("sorting.R")

# Quality assessment
qa_only <- articles %>% select(Covidence_ID, ID,
                               Name_data_entry, starts_with("QA"))

qa_match <- filter_qa(qa_only, matching = TRUE,
                      id_name1 = "Name_data_entry", id_name2 = "ID")

qa_discordant <- filter_qa(qa_only, matching = FALSE,
                           id_name1 = "Name_data_entry", id_name2 = "ID")

# Parameters
param_match <- filter_extracted(parameters, matching = TRUE,
                                id_name1 = "Name_data_entry",
                                id_name2 = "ID",
                                id_name3 = "Parameter_data_ID",
                                id_name4 = "Article_ID")

param_discordant <- filter_extracted(parameters, matching = FALSE,
                                     id_name1 = "Name_data_entry",
                                     id_name2 = "ID",
                                     id_name3 = "Parameter_data_ID",
                                     id_name4 = "Article_ID")

# Models
model_match <- filter_extracted(models, matching = TRUE,
                                id_name1 = "Name_data_entry",
                                id_name2 = "ID",
                                id_name3 = "Model_data_ID",
                                id_name4 = "Article_ID")

model_discordant <- filter_extracted(models, matching = FALSE,
                                     id_name1 = "Name_data_entry",
                                     id_name2 = "ID",
                                     id_name3 = "Model_data_ID",
                                     id_name4 = "Article_ID")

# Create files
write_csv(qa_match, "qa_matching.csv")
write_csv(qa_discordant, "qa_fixing.csv")

write_csv(param_match, "params_matching.csv")
write_csv(param_discordant, "params_fixing.csv")

write_csv(model_match, "models_matching.csv")
write_csv(model_discordant, "models_fixing.csv")

# Empty outbreaks for Ebola - amend this for other pathogens
file.create("outbreaks_matching.csv")
file.create("outbreaks_fixing.csv")
