# Task to compile single and double1 and double2 Zika extraction databases together (to be run after db_extraction)
# orderly2::orderly_run(name = 'zika_compilation', parameters = list(pathogen = 'ZIKA'))
# install.packages('rio')
library(dplyr)
library(janitor)
library(orderly2)
library(readr)
library(stringr)
library(stringi)
library(rio)
library(orderly.sharedfile)
library(epireview)
library(readxl)

## pathogen should be set to one of our priority-pathogens
## use capital case
## orderly_parameters(pathogen = 'EBOLA')
orderly_parameters(pathogen = NULL)

## Outputs
orderly_artefact(
  description = "Merged single and double extracted data as rds",
  c(
    "articles.rds",
    "models.rds",
    "parameters.rds",
    "outbreaks.rds"
  )
)

# Get results from db_extraction
orderly_dependency(
  "db_extraction",
  "latest(parameter:pathogen == this:pathogen)",
  c(
    "single_extraction_articles.csv" = "single_extraction_articles.csv",
    "single_extraction_params.csv" = "single_extraction_params.csv",
    "single_extraction_models.csv" = "single_extraction_models.csv",
    "single_extraction_outbreaks.csv" = "single_extraction_outbreaks.csv",
    "db2_double_extraction_articles.csv" = "double_extraction_articles.csv",
    "db2_double_extraction_params.csv" = "double_extraction_params.csv",
    "db2_double_extraction_models.csv" = "double_extraction_models.csv",
    "db2_double_extraction_outbreaks.csv" = "double_extraction_outbreaks.csv"
  )
)

# Get results from db_double
# db_double also produces the fixing files that need to be manually changed and
# supplied as resources below
orderly_dependency(
  "db_double",
  "latest(parameter:pathogen == this:pathogen)",
  c(
    "db2_qa_matching.csv" = 'qa_matching.csv', 
    "db2_models_matching.csv" = 'models_matching.csv',
    "db2_params_matching.csv" = 'params_matching.csv', 
    "db2_outbreaks_matching.csv" = 'outbreaks_matching.csv'
  )
)

# Manually fixed files and "cleaning" script - these need to be in the
# src/zika_compilation folder
# second round of double 
orderly_resource(
  c('zika_cleaning.R' = 'zika_cleaning.R',
    # 'cleaning.R' = 'cleaning.R',
    "zika_db1_qa_fixing.xlsx" = "zika_db1_qa_fixing.xlsx",
    "zika_db1_params_fixing.xlsx" = "zika_db1_params_fixing.xlsx",
    "zika_db1_models_fixing.xlsx" = "zika_db1_models_fixing.xlsx",
    "zika_db1_outbreaks_fixing.csv" = "zika_db1_outbreaks_fixing.csv",
    "zika_db1_qa_matching.csv" = "zika_db1_qa_matching.csv", # matching file for round 1 double extractions from here: 'P:/Zika/priority-pathogens/archive/db_double/20240411-094214-9cdb9e2e/qa_matching.csv'
    "zika_db1_double_extraction_articles.csv" = "zika_db1_double_extraction_articles.csv", # article information from round 1 double extractions
    # Second round of double extraction fixing files
    "zika_db2_qa_fixing.xlsx" = "zika_db2_qa_fixing.xlsx",
    "zika_db2_params_fixing.xlsx" = "zika_db2_params_fixing.xlsx",
    "zika_db2_models_fixing.xlsx" = "zika_db2_models_fixing.xlsx",
    "zika_db2_outbreaks_fixing.csv" = "zika_db2_outbreaks_fixing.csv",
    # Cleaning file for CZS/microcephaly and miscarriage probabilities
    "czs_misc_checked.xlsx" = "czs_misc_checked.xlsx"
    )
)

# source('cleaning.R')
source('zika_cleaning.R')

# Get article information into qa_fixing files for double extraction round 1
db1_articles_matching <- read_csv('zika_db1_qa_matching.csv') 
db1_double_articles <- read_csv('zika_db1_double_extraction_articles.csv')

qa_matching1 <- db1_articles_matching %>%
  distinct(Covidence_ID, .keep_all = TRUE) 
qa_fixing1 <- readxl::read_xlsx('zika_db1_qa_fixing.xlsx')
qa_all1 <- db1_double_articles %>%
  select(-starts_with('QA'))

qa_combined_pt1 <- qa_all1 %>%
  filter(Covidence_ID %in% qa_matching1$Covidence_ID) %>%
  left_join(qa_matching1) %>%
  filter(!is.na(QA_M1)) # this removes the rows with missing quality assessment values 
qa_combined_pt2 <- qa_all1 %>%
  filter(Covidence_ID %in% qa_fixing1$Covidence_ID) %>%
  left_join(qa_fixing1 %>% select(-fixed)) %>%
  filter(!is.na(QA_M1)) # this removes the rows with missing quality assessment values 
db1_articles <- rbind(qa_combined_pt1, qa_combined_pt2) %>%
  select(-num_rows, -matching) %>%
  select(sort(names(.)))
# qa_combined will be missing #1587 (compared to qa_all1) because it was excluded after originally being extracted

# pull in the other files for db 1 (model, outbreak, params)
db1_models <- readxl::read_xlsx('zika_db1_models_fixing.xlsx')[,2:16]  %>%# removing NA columns and fixed column
  select(sort(names(.)))
db1_outbreaks <- read_csv('zika_db1_outbreaks_fixing.csv')%>%
  select(-fixed, -num_rows, -matching) %>%
  select(sort(names(.)))
db1_params <- readxl::read_xlsx('zika_db1_params_fixing.xlsx')%>%
  select(-fixed, -num_rows, -matching) %>%
  select(sort(names(.)))

#' Get files for second round of double extractions 
db2_double_articles <- read_csv("db2_double_extraction_articles.csv") %>%
  select(sort(names(.))) %>%
  # fix incorrect covidence ID 
  mutate(Covidence_ID = case_when(
    Covidence_ID == 6882 & FirstAauthor_Surname == "Sasmono" ~ 6886,
    TRUE ~ Covidence_ID),
    Covidence_ID_text = case_when(
      Covidence_ID_text == 6882 & FirstAauthor_Surname == "Sasmono" ~ 6886,
      TRUE ~ Covidence_ID_text)
  )
db2_double_params <- read_csv("db2_double_extraction_params.csv") %>%
  select(sort(names(.))) 
db2_double_models <- read_csv("db2_double_extraction_models.csv") %>%
  select(sort(names(.)))
db2_double_outbreaks <- read_csv("db2_double_extraction_outbreaks.csv") %>%
  select(sort(names(.)))

db2_matching_articles <- read_csv('db2_qa_matching.csv') %>%
  select(-num_rows, -matching) %>%
  select(sort(names(.)))
db2_matching_params <- read_csv('db2_params_matching.csv')%>%
  select(-num_rows, -matching) %>%
  select(sort(names(.)))
db2_matching_models <- read_csv('db2_models_matching.csv')%>%
  select(-num_rows, -matching) %>%
  select(sort(names(.)))
db2_matching_outbreaks <- read_csv('db2_outbreaks_matching.csv')%>%
  select(-num_rows, -matching) %>%
  select(sort(names(.)))

db2_fixing_articles <- readxl::read_xlsx('zika_db2_qa_fixing.xlsx') %>%
  select(-num_rows, -matching, -fixed) %>%
  select(sort(names(.))) %>%
  rbind(db2_matching_articles)
db2_fixing_params <- readxl::read_xlsx('zika_db2_params_fixing.xlsx')%>%
  select(-num_rows, -matching, -fixed) %>%
  select(sort(names(.)))
db2_fixing_models <- readxl::read_xlsx('zika_db2_models_fixing.xlsx')%>%
  select(-num_rows, -matching, -fixed) %>%
  select(sort(names(.)))
db2_fixing_outbreaks <- read_csv('zika_db2_outbreaks_fixing.csv')%>%
  select(-num_rows, -matching, -fixed) %>%
  select(sort(names(.)))


# For the second round of double extractions, need to get article information 
qa_2 <- db2_fixing_articles %>%
  distinct(Covidence_ID, .keep_all = TRUE)
qa_all2 <- db2_double_articles %>%
  filter(Covidence_ID %in% qa_2$Covidence_ID) %>%
  select(-starts_with('QA'))

db2_articles <- qa_all2 %>%
  left_join(qa_2 %>% select(-ID), by = c('Covidence_ID', 'Name_data_entry')) %>%
  filter(!is.na(QA_M1)) # this removes the rows with missing quality assessment values 

# Add in those that still have article information (from doing 'double' extraction in single dbs)
db2_articles <- rbind(db2_articles, db2_double_articles %>% filter(!(Covidence_ID %in% qa_2$Covidence_ID) )) %>%
  filter(!(Covidence_ID == 7174 & is.na(QA_M1)))

# Make de-duplicated df of articles (these will miss some notes)
db1distinct <- distinct(db1_articles, Covidence_ID, .keep_all = TRUE)
db2distinct <- distinct(db2_articles, Covidence_ID, .keep_all = TRUE)
double_articles <- rbind(db1_articles, db2_articles) %>%
  distinct(Covidence_ID, .keep_all = TRUE)

# Get files for single extractions 
single_articles <- read_csv("single_extraction_articles.csv") %>%
  select(sort(names(.)))
single_params <- read_csv("single_extraction_params.csv") %>%
  select(sort(names(.)))
single_models <- read_csv("single_extraction_models.csv") %>%
  select(sort(names(.)))
single_outbreaks <- read_csv("single_extraction_outbreaks.csv") %>%
  select(sort(names(.)))

# Remove articles that have nothing extracted
single_articles <- single_articles %>%
  filter(!(Covidence_ID %in% c(6893, 1369, 1652, 1658, 23300)))

# Make a file with article notes 
articlenotes <- rbind(db1_articles, db2_articles, single_articles) %>%
  filter(!is.na(Notes))
write_csv(articlenotes, 'articlenotes.csv')

# Combine double 1, double 2, and single extractions together 
articles_all <- rbind(double_articles, single_articles) %>%
  clean_names()
models_all <- rbind(db1_models, db2_fixing_models, single_models) %>%
  clean_names()
outbreaks_all <- rbind(db1_outbreaks, db2_fixing_outbreaks, single_outbreaks) %>%
  clean_names()
params_all <- rbind(db1_params, db2_fixing_params, single_params) %>%
  clean_names()

# Read in the cleaned file for CZS/miscarriage probabilities 
czs_misc_cleaned <- readxl::read_excel("czs_misc_checked.xlsx")

#' cleaning script for zika
articles_clean <- zika_clean_articles(articles_all, pathogen = 'ZIKA')
models_clean <- zika_clean_models(models_all, pathogen = 'ZIKA')
outbreaks_clean <- zika_clean_outbreaks(outbreaks_all, pathogen = 'ZIKA')
params_clean <- zika_clean_params(params_all, pathogen = 'ZIKA')

# Add qa scores to article df
articles_qa <- assign_qa_score(articles_clean, ignore_errors = TRUE)#add_qa_scores(articles_clean, params_clean)
articles_qa <- as.data.frame(articles_qa$articles)



# save cleaned dfs
saveRDS(articles_qa, 'articles.rds')
saveRDS(models_clean, 'models.rds')
saveRDS(outbreaks_clean, 'outbreaks.rds')
saveRDS(params_clean, 'parameters.rds')
write_csv(articles_qa, 'articles.csv')
write_csv(models_clean, 'models.csv')
write_csv(outbreaks_clean, 'outbreaks.csv')
write_csv(params_clean, 'parameters.csv')