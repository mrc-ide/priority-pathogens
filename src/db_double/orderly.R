# Task to identify entries of double extracted data that match or do not match
# between extractors

library(orderly2)
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_artefact(
  "Double extraction matches and mismatches as csv files",
  c(
    "qa_fixing.csv", "models_fixing.csv", "params_fixing.csv",
    "outbreaks_fixing.csv", "qa_matching.csv", "models_matching.csv",
    "params_matching.csv", "outbreaks_matching.csv"
  )
)

orderly_resource("sorting.R")

# Take the results from db_extraction
orderly_dependency(
  "db_extraction", "latest(parameter:pathogen == this:pathogen)",
  c(
    "double_extraction_articles.csv",
    "double_extraction_params.csv",
    "double_extraction_models.csv",
    "double_extraction_outbreaks.csv"
  )
)
## pathogen should be set to one of our priority-pathogens
## Downstream tasks can query on this parameter to
## pull in the correct files as dependancies.



library(dplyr)
library(readr)
articles <- read_csv("double_extraction_articles.csv")
parameters <- read_csv("double_extraction_params.csv")
models <- read_csv("double_extraction_models.csv")
if (pathogen %in% c('LASSA', 'OROV', 'NIPAH')) {
  outbreaks <- read_csv("double_extraction_outbreaks.csv")
} else {
  outbreaks <- NULL
}

source("sorting.R")

qa_exlcude_cols <- c("Name_data_entry", "ID")
qa_notes_col <- NULL
qa_id_col <- NULL

param_exclude_cols <- c("Name_data_entry",
                        "ID",
                        "Parameter_data_ID",
                        "Article_ID")
param_notes_col <- NULL
param_id_col <- NULL

outbreak_exclude_cols <- c("Name_data_entry",
                           "ID",
                           "Outbreak_ID",
                           "Article_ID")
outbreak_notes_col <- NULL
outbreak_id_col <- NULL

model_exclude_cols <- c("Name_data_entry",
                        "ID",
                        "Model_data_ID",
                        "Article_ID")

model_notes_col <- NULL
model_id_col <- NULL

if (pathogen %in% c("NIPAH")){
  outbreak_exclude_cols <- c(outbreak_exclude_cols, "Outbreak_data_ID")

  qa_notes_col <- "qa_notes"
  qa_id_col <- "ID"
  param_notes_col <- "parameter_notes"
  param_id_col <- "Parameter_data_ID"
  outbreak_notes_col <- "outbreak_notes"
  outbreak_id_col <- "Outbreak_data_ID"
  model_notes_col <- "model_notes"
  model_id_col <- "Model_data_ID"
}


# Quality assessment
qa_only <- articles %>% select(
  Covidence_ID, ID,
  Name_data_entry, starts_with("QA")
)

qa_match <- filter_table(qa_only,
  matching = TRUE,
  exlcude_cols = qa_exlcude_cols)

qa_discordant <- filter_table(qa_only,
  matching = FALSE,
  exlcude_cols = qa_exlcude_cols,
  notes_col = qa_notes_col,
  id_col = qa_id_col
)

# Parameters
# parameters <- replace("No")
param_match <- filter_table(parameters,
  matching = TRUE,
  exlcude_cols = param_exclude_cols
)

param_discordant <- filter_table(parameters,
  matching = FALSE,
  exlcude_cols = param_exclude_cols,
  notes_col = param_notes_col,
  id_col = param_id_col
)

# Models
model_match <- filter_table(models,
  matching = TRUE,
  exlcude_cols=model_exclude_cols
)

model_discordant <- filter_table(models,
  matching = FALSE,
  exlcude_cols=model_exclude_cols,
  notes_col = model_notes_col,
  id_col = model_id_col
)

if (pathogen %in% c('LASSA', 'OROV', 'NIPAH')){
  outbreak_match <- filter_table(outbreaks,
                                  matching = TRUE,
                                  exlcude_cols=outbreak_exclude_cols
  )

  outbreak_discordant <- filter_table(outbreaks,
                                      matching = FALSE,
                                      exlcude_cols=outbreak_exclude_cols,
                                      notes_col = outbreak_notes_col,
                                      id_col = outbreak_id_col
  )
}

# The fixing files don't need the new IDs for now as they change each time
# db_extraction is run and they complicate merging the fixing files
if (pathogen != 'LASSA') {
  qa_discordant <- qa_discordant %>% select(-ID)
  param_discordant <- param_discordant %>% select(-c(ID, Parameter_data_ID))
  model_discordant <- model_discordant %>% select(-c(ID, Model_data_ID))
  }

# Create files
write_csv(qa_match, "qa_matching.csv")
write_csv(qa_discordant, "qa_fixing.csv")

write_csv(param_match, "params_matching.csv")
write_csv(param_discordant, "params_fixing.csv")

write_csv(model_match, "models_matching.csv")
write_csv(model_discordant, "models_fixing.csv")

# Empty outbreaks for Ebola - amend this for other pathogens
if (pathogen %in% c('EBOLA','SARS')) {
  file.create("outbreaks_matching.csv")
  file.create("outbreaks_fixing.csv")
}

if (pathogen %in% c('LASSA', 'OROV', 'NIPAH')){
  write_csv(outbreak_match, "outbreaks_matching.csv")
  write_csv(outbreak_discordant, "outbreaks_fixing.csv")
}
