# Task to compile single and double1 and double2 Zika extraction databases together
# install.packages('rio')
library(dplyr)
library(janitor)
library(orderly2)
library(readr)
library(stringr)
library(stringi)
library(rio)

orderly_strict_mode()

# Change this to match local locations
onedrivepath <- 'C:/Users/kem22/OneDrive - Imperial College London/outbreaks/'
sharedpath <- 'P:/Zika/'

## pathogen should be set to one of our priority-pathogens
## use capital case
## orderly_parameters(pathogen = 'EBOLA')
orderly_parameters(pathogen = NULL)

## Outputs
orderly_artefact(
  "Merged single and double extracted data as csv",
  c(
    "articles.csv",
    "models.csv",
    "parameters.csv",
    "outbreaks.csv"
  )
)

# Get results from db_extraction
orderly_dependency(
  "db_extraction",
  "latest(parameter:pathogen == this:pathogen)",
  c(
    "single_extraction_articles.csv",
    "single_extraction_params.csv",
    "single_extraction_models.csv",
    "single_extraction_outbreaks.csv",
    "double_extraction_articles.csv",
    "double_extraction_params.csv",
    "double_extraction_models.csv",
    "double_extraction_outbreaks.csv"
  )
)

# Get results from db_double
# db_double also produces the fixing files that need to be manually changed and
# supplied as resources below
orderly_dependency(
  "db_double",
  "latest(parameter:pathogen == this:pathogen)",
  c(
    "qa_matching.csv", "models_matching.csv",
    "params_matching.csv", "outbreaks_matching.csv"
  )
)

# Manually fixed files and "cleaning" script - these need to be in the
# src/db_compilation folder
# second round of double 
orderly_resource(
  c('zika_cleaning.R',
    # First round of double extraction fixing files
    'db1_qa_fixing.xlsx',
    "db1_params_fixing.xlsx",
    "db1_models_fixing.xlsx",
    "db1_outbreaks_fixing.xlsx"
    # Second round of double extraction fixing files 
  )
)

source('zika_cleaning.R')

# Get article information into qa_fixing files for double extraction round 1
qa_matching1 <- readr::read_csv('P:/Zika/priority-pathogens/archive/db_double/20240411-094214-9cdb9e2e/qa_matching.csv') %>%
  distinct(Covidence_ID, .keep_all = TRUE)
qa_fixing1 <- readxl::read_xlsx('db1_qa_fixing.xlsx')
qa_all1 <- readr::read_csv('P:/Zika/priority-pathogens/archive/db_double/20240411-094214-9cdb9e2e/double_extraction_articles.csv') %>%
  select(-starts_with('QA'))

qa_combined_pt1 <- qa_all1 %>%
  filter(Covidence_ID %in% qa_matching1$Covidence_ID) %>%
  left_join(qa_matching1) 
qa_combined_pt2 <- qa_all1 %>%
  filter(Covidence_ID %in% qa_fixing1$Covidence_ID) %>%
  left_join(qa_fixing1 %>% select(-fixed)) 
db1_articles <- rbind(qa_combined_pt1, qa_combined_pt2)
# qa_combined will be missing #1587 (compared to qa_all1) because it was excluded after originally being extracted


# pull in the other files for db 1 (model, outbreak, params)
db1_models <- readxl::read_xlsx('db1_models_fixing.xlsx')
db1_outbreaks <- readxl::read_xlsx('db1_outbreaks_fixing.xlsx')
db1_params <- readxl::read_xlsx('db1_params_fixing.xlsx')

#' Get files for second round of double extractions 


# Get files for single extractions 


# Combine double 1, double 2, and single extractions together 
articles_all <- db1_articles
models_all <- db1_models
outbreaks_all <- db1_outbreaks
params_all <- db1_params

#' cleaning script 
articles_clean <- clean_articles(articles_all)
models_clean <- clean_models(models_all)
outbreaks_clean <- clean_outbreaks(outbreaks_all)
params_clean <- clean_params(params_all)
