# Task to compile single and double extraction databases together
library(readr)
library(orderly2)
library(dplyr)
library(janitor)
orderly_strict_mode()

## Outputs
orderly_artefact(
  "Merged single and double extracted data as csv",
  c("articles.csv", "models.csv", "parameters.csv"))

# Get results from db_extraction
orderly_dependency("db_extraction", "20230926-174305-b4c7951e",
  c("single_extraction_articles.csv" = "single_extraction_articles.csv",
    "single_extraction_params.csv" = "single_extraction_params.csv",
    "single_extraction_models.csv" = "single_extraction_models.csv",
    "double_extraction_articles.csv" = "double_extraction_articles.csv")
)

# Get results from db_double
# db_double also produces the fixing files that need to be manually changed and
# supplied as resources below
orderly_dependency("db_double", "20230926-175626-bc9cd6d0",
                   c("qa_matching.csv" = "qa_matching.csv")
)

# Manually fixed files and "cleaning" script - these need to be in the
# src/db_compilation folder
orderly_resource(
  c("qa_fixing.csv",
    "params_fixing.csv",
    "models_fixing.csv",
    "cleaning.R"
  )
)

source("cleaning.R")

# Single extractions
article_single <- read_csv("single_extraction_articles.csv")
model_single <- read_csv("single_extraction_models.csv")
parameter_single <- read_csv("single_extraction_params.csv")
outbreak_single <- NULL

article_single <- article_single %>% clean_names()

model_single <- model_single %>% clean_names()

parameter_single <- parameter_single %>% clean_names()

# Double extractions - matched between extractors
article_double <- read_csv("double_extraction_articles.csv")
qa_matching <- read_csv("qa_matching.csv")
model_matching <- NULL
parameter_matching <- NULL
outbreak_matching <- NULL

article_double <- article_double %>% clean_names() %>% arrange(covidence_id)

qa_matching <- qa_matching %>% clean_names()

# Double extractions - needed to be resolved between extractors
qa_fixed <- read_csv("qa_fixing.csv")
model_fixed <- read_csv("models_fixing.csv")
parameter_fixed <- read_csv("params_fixing.csv")
outbreak_fixed <- NULL

## create final datasets
# TO DO: Add outbreak_fixed for next pathogen
qa_fixed <- qa_fixed %>%
  filter(fixed == TRUE) %>%
  select(names(qa_matching))

parameter_fixed <- parameter_fixed %>%
  filter(fixed == TRUE) %>%
  select(names(parameter_single))

model_fixed <- model_fixed %>%
  filter(fixed == TRUE) %>%
  select(names(model_single))

# join article data to qa files
article_double_details <- article_double %>% select(-c(starts_with("qa")))

article_matching <- qa_matching %>%
  select(-c("num_rows", "matching")) %>%
  left_join(article_double_details,
            by = c("id", "covidence_id", "name_data_entry")) %>%
  distinct(covidence_id, .keep_all = TRUE) %>%
  mutate(double_extracted = 1)

article_fixed <- qa_fixed %>%
  select(-c("num_rows", "matching")) %>%
  left_join(article_double_details,
            by = c("id", "covidence_id", "name_data_entry")) %>%
  mutate(double_extracted = 1)

article_single <- article_single %>%
  mutate(double_extracted = 0)

# bind single and double together
article_all <- rbind(article_single,
                     article_matching,
                     article_fixed)

parameter_all <- rbind(parameter_single,
                       parameter_matching,
                       parameter_fixed)

model_all <- rbind(model_single,
                   model_matching,
                   model_fixed)

# Cleaning
parameter_all <- clean_dfs(parameter_all, "parameter_type")
model_all <- clean_dfs(model_all, "model_type")

write_csv(parameter_all, "parameters.csv")
write_csv(model_all, "models.csv")
write_csv(article_all, "articles.csv")
