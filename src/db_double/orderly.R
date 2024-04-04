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
if (pathogen == 'LASSA' | pathogen == 'ZIKA') {
  outbreaks <- read_csv("double_extraction_outbreaks.csv")
} else {
  outbreaks <- NULL
}

source("sorting.R")

# Quality assessment
qa_only <- articles %>% select(
  Covidence_ID, ID,
  Name_data_entry, starts_with("QA")
)

qa_match <- filter_qa(qa_only,
  matching = TRUE,
  id_name1 = "Name_data_entry", id_name2 = "ID"
)

qa_discordant <- filter_qa(qa_only,
  matching = FALSE,
  id_name1 = "Name_data_entry", id_name2 = "ID"
)

# Parameters
param_match <- filter_extracted(parameters,
  matching = TRUE,
  id_name1 = "Name_data_entry",
  id_name2 = "ID",
  id_name3 = "Parameter_data_ID",
  id_name4 = "Article_ID"
)

param_discordant <- filter_extracted(parameters,
  matching = FALSE,
  id_name1 = "Name_data_entry",
  id_name2 = "ID",
  id_name3 = "Parameter_data_ID",
  id_name4 = "Article_ID"
)

# Models
model_match <- filter_extracted(models,
  matching = TRUE,
  id_name1 = "Name_data_entry",
  id_name2 = "ID",
  id_name3 = "Model_data_ID",
  id_name4 = "Article_ID"
)

model_discordant <- filter_extracted(models,
  matching = FALSE,
  id_name1 = "Name_data_entry",
  id_name2 = "ID",
  id_name3 = "Model_data_ID",
  id_name4 = "Article_ID"
)

if (pathogen == 'LASSA' | pathogen == 'ZIKA') {
  outbreak_match <- filter_extracted(outbreaks,
                                  matching = TRUE,
                                  id_name1 = "Name_data_entry",
                                  id_name2 = "ID",
                                  id_name3 = "Outbreak_ID",
                                  id_name4 = "Article_ID"
  )
  
  outbreak_discordant <- filter_extracted(outbreaks,
                                       matching = FALSE,
                                       id_name1 = "Name_data_entry",
                                       id_name2 = "ID",
                                       id_name3 = "Outbreak_ID",
                                       id_name4 = "Article_ID"
  )
}

# The fixing files don't need the new IDs for now as they change each time
# db_extraction is run and they complicate merging the fixing files
if (pathogen != 'LASSA' & pathogen != 'ZIKA') {
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

if (pathogen == 'LASSA' | pathogen == 'ZIKA') {
  write_csv(outbreak_match, "outbreaks_matching.csv")
  write_csv(outbreak_discordant, "outbreaks_fixing.csv")
}
