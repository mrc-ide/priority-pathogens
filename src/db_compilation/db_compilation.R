
# Task to compile single and double extraction databases together
library(dplyr)
library(janitor)
library(orderly2)
library(readr)
library(epireview)


orderly_strict_mode()

## pathogen should be set to one of our priority-pathogens
## use capital case
## orderly_parameters(pathogen = 'EBOLA')
orderly_parameters(pathogen = NULL)

## Outputs
orderly_artefact(
  description = "Merged single and double extracted data as csv",
  files = c(
    "articles.csv",
    "models.csv",
    "parameters.csv",
    "outbreaks.csv"
  )
)


if(pathogen != "ZIKA"){
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
      "params_matching.csv","outbreaks_matching.csv"
    )
  )
  
  # Manually fixed files and "cleaning" script - these need to be in the
  # src/db_compilation folder
  orderly_resource(
    c(
      "ebola_qa_fixing.csv",
      "ebola_params_fixing.csv",
      "ebola_models_fixing.csv",
      ## LASSA FIXING FILES
      "lassa_qa_fixing.csv",
      "lassa_params_fixing.csv",
      "lassa_models_fixing.csv",
      "lassa_outbreaks_fixing.csv",
      ## SARS FIXING FILES
      "sars_qa_fixing.csv",
      "sars_params_fixing.csv",
      "sars_models_fixing.csv",
      ## NIPAH FIXING FILES
      "cleaning.R",
      "sars_cleaning.R",
      "ebola_cleaning.R",
      "lassa_cleaning.R"
    )
  )
  
  orderly_shared_resource("ebola_functions.R" = "ebola_functions.R")
  source("ebola_functions.R")
  ## Here we map the fixing files to the
  ## pathogen.
  fixing_files <- list(
    EBOLA = list(
      params_fix = "ebola_params_fixing.csv",
      models_fix = "ebola_models_fixing.csv",
      qa_fix = "ebola_qa_fixing.csv"
    ),
    LASSA = list(
      params_fix = "lassa_params_fixing.csv",
      models_fix = "lassa_models_fixing.csv",
      qa_fix = "lassa_qa_fixing.csv",
      outbreaks_fix = "lassa_outbreaks_fixing.csv"
    ),
    SARS = list(
      params_fix = "sars_params_fixing.csv",
      models_fix = "sars_models_fixing.csv",
      qa_fix = "sars_qa_fixing.csv"
    )
  )
  
  source("cleaning.R")
  source("ebola_cleaning.R")
  source("lassa_cleaning.R")
  source("sars_cleaning.R")
  source("zika_cleaning.R")
  
  # Single extractions
  article_single <- read_csv("single_extraction_articles.csv")
  model_single <- read_csv("single_extraction_models.csv")
  parameter_single <- read_csv("single_extraction_params.csv")
  outbreak_single <- read_csv("single_extraction_outbreaks.csv")
  
  article_single <- article_single %>% clean_names()
  
  model_single <- model_single %>% clean_names()
  
  parameter_single <- parameter_single %>% clean_names()
  
  outbreak_single <- outbreak_single %>% clean_names()
  
  # Double extractions - matched between extractors
  article_double <- read_csv("double_extraction_articles.csv")
  model_double <- read_csv("double_extraction_models.csv")
  param_double <- read_csv("double_extraction_params.csv")
  outbreak_double <- read_csv("double_extraction_outbreaks.csv")
  
  article_double <- article_double %>%
    clean_names() %>%
    arrange(covidence_id)
  model_double <- model_double %>%
    clean_names() %>%
    arrange(covidence_id)
  param_double <- param_double %>%
    clean_names() %>%
    arrange(covidence_id)
  outbreak_double <- outbreak_double %>%
    clean_names() 
  
  if (pathogen == "LASSA") outbreak_double <- arrange(outbreak_double, covidence_id)
  
  qa_matching <- read_csv("qa_matching.csv")
  model_matching <- read_csv("models_matching.csv")
  parameter_matching <- read_csv("params_matching.csv")
  outbreak_matching <- read_csv("outbreaks_matching.csv")
  
  qa_matching <- qa_matching %>% clean_names()
  parameter_matching <- parameter_matching %>% clean_names()
  model_matching <- model_matching %>% clean_names()
  outbreak_matching <- outbreak_matching %>% clean_names()
  
  qa_matching <- qa_matching %>% select(-c("num_rows", "matching")) 
  parameter_matching <- parameter_matching %>% select(-c("num_rows", "matching")) %>%
    mutate(covidence_id=as.numeric(covidence_id),
           article_id=as.numeric(article_id),
           access_param_id=as.numeric(access_param_id))
  model_matching <- model_matching %>% select(-c("num_rows", "matching")) %>%
    mutate(covidence_id=as.numeric(covidence_id),
           article_id=as.numeric(article_id),
           access_model_id=as.numeric(access_model_id))
  if (pathogen == "LASSA") {
    outbreak_matching <- outbreak_matching %>% select(-c("num_rows", "matching")) %>%
      mutate(covidence_id=as.numeric(covidence_id),
             article_id=as.numeric(article_id),
             access_outbreak_id=as.numeric(access_outbreak_id))
  } else outbreak_matching <-  outbreak_single
  
  
  # Double extractions - needed to be resolved between extractors
  qa_fixed <- read_csv(fixing_files[[pathogen]][["qa_fix"]])
  model_fixed <- read_csv(fixing_files[[pathogen]][["models_fix"]])
  parameter_fixed <- read_csv(fixing_files[[pathogen]][["params_fix"]])
  fixing_file <- fixing_files[[pathogen]][["outbreaks_fix"]]
  if (! is.null(fixing_file)) {
    outbreak_fixed <- read_csv(fixing_file)
    outbreak_fixed  <- outbreak_fixed %>% clean_names()
    outbreak_fixed <- outbreak_fixed %>%
      mutate(covidence_id=as.numeric(covidence_id),
             article_id=as.numeric(article_id),
             access_outbreak_id=as.numeric(access_outbreak_id))
    outbreak_fixed <- outbreak_fixed %>%
      filter(fixed == 1) %>%
      select(-c("fixed", "num_rows", "matching"))
    
    outbreak_double_ids <- outbreak_double %>%
      select(c(
        "article_id", "name_data_entry", "access_outbreak_id", "covidence_id",
        "id", "outbreak_data_id"
      ))
    
    outbreak_fixed <- outbreak_fixed %>%
      left_join(outbreak_double_ids,
                by = c(
                  "article_id", "name_data_entry",
                  "access_outbreak_id", "covidence_id","id", "outbreak_data_id"
                )
      )
    
    
  } else {
    outbreak_fixed <- outbreak_single
  }
  
  
  qa_fixed        <- qa_fixed %>% clean_names()
  
  model_fixed     <- model_fixed %>% clean_names()
  parameter_fixed <- parameter_fixed %>% clean_names()
  
  parameter_fixed <- parameter_fixed %>%
    mutate(covidence_id=as.numeric(covidence_id),
           article_id=as.numeric(article_id),
           access_param_id=as.numeric(access_param_id))
  model_fixed <- model_fixed %>%
    mutate(covidence_id=as.numeric(covidence_id),
           article_id=as.numeric(article_id),
           access_model_id=as.numeric(access_model_id))
  
  ## create final datasets
  # Add outbreak_fixed for next pathogen
  qa_fixed <- qa_fixed %>%
    filter(fixed == 1) %>%
    select(-c("fixed", "num_rows", "matching"))
  
  parameter_fixed <- parameter_fixed %>%
    filter(fixed == 1) %>%
    select(-c("fixed", "num_rows", "matching"))
  
  model_fixed <- model_fixed %>%
    filter(fixed == 1) %>%
    select(-c("fixed", "num_rows", "matching"))
  
  if (pathogen %in% c("EBOLA", "SARS")) {
    # join article data to qa files
    article_double_details <- article_double %>% select(-c(starts_with("qa")))
    
    article_matching <- qa_matching %>%
      left_join(article_double_details,
                by = c("id", "covidence_id", "name_data_entry")
      ) %>%
      distinct(covidence_id, .keep_all = TRUE) %>%
      mutate(double_extracted = 1)
    
    article_fixed <- qa_fixed %>%
      left_join(article_double_details,
                by = c("covidence_id", "name_data_entry")
      ) %>%
      mutate(double_extracted = 1)
    
    article_single <- article_single %>%
      mutate(double_extracted = 0)
  }
  # join ids
  param_double_ids <- param_double %>%
    select(c(
      "article_id", "name_data_entry", "access_param_id", "covidence_id",
      "id", "parameter_data_id"
    ))
  
  parameter_fixed <- left_join(parameter_fixed, param_double_ids)
  
  
  model_double_ids <- model_double %>%
    select(c(
      "article_id", "name_data_entry", "access_model_id", "covidence_id",
      "id", "model_data_id"
    ))
  
  model_fixed <- left_join(model_fixed, model_double_ids)
  
  
  
  # bind single and double together
  if (pathogen == "LASSA") {
    article_all <- rbind(
      article_single,
      qa_fixed)
  } else {
    article_all <- rbind(
      article_single,
      article_matching,
      article_fixed)
  }
  
  parameter_all <- rbind(
    parameter_single,
    parameter_matching,
    parameter_fixed
  )
  
  model_all <- rbind(
    model_single,
    model_matching,
    model_fixed
  )
  
  outbreak_all <- rbind(
    outbreak_single,
    outbreak_matching,
    outbreak_fixed
  )
  
  # Cleaning
  article_all   <- clean_dfs(article_all, pathogen)
  
  if (pathogen == "LASSA") {
    ## SB 14.05.2024
    ## Temporary fix to deal with garbled characters 
    ## To be removed once we have identified the source of the issue
    ## and fixed it properly
    outbreak_all$outbreak_location <- iconv(
      outbreak_all$outbreak_location, to = "UTF-8", sub = "byte"
    )
    outbreak_all <- lassa_outbreaks_cleaning(outbreak_all)
    outbreak_all  <- clean_dfs(outbreak_all, pathogen)
  } 
  
  model_all     <- clean_dfs(model_all, pathogen)
  if (pathogen == "LASSA") model_all <- lassa_models_cleaning(model_all)
  parameter_all <- clean_dfs(parameter_all, pathogen)
  if (pathogen == 'EBOLA') {
    parameter_all <- assign_ebola_outbreak(parameter_all)
    parameter_all <- assign_ebola_species(parameter_all)
    
  }
  # # Add article QA scores to article data
  if (pathogen == 'EBOLA') {
    article_all <- add_qa_scores(article_all, parameter_all)
    
    # # Add article QA scores as a parameter variable
    parameter_all <- parameter_all %>%
      left_join(
        select(article_all, covidence_id, article_qa_score),
        by = "covidence_id"
      )
  }
  print(class(article_all))
  write_csv(article_all, "articles.csv")
  if (pathogen %in% c("EBOLA", "SARS")) {
    file.create("outbreaks.csv")
  } else {
    write_csv(outbreak_all, "outbreaks.csv")
  }
  write_csv(model_all, "models.csv")
  write_csv(parameter_all, "parameters.csv")
}

if(pathogen == 'ZIKA'){

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
      'cleaning.R' = 'cleaning.R',
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
      "zika_db2_outbreaks_fixing.csv" = "zika_db2_outbreaks_fixing.csv")
  )
  
  source('cleaning.R')
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
  
  #' cleaning script for zika
  articles_clean <- zika_clean_articles(articles_all, pathogen = 'ZIKA')
  models_clean <- zika_clean_models(models_all, pathogen = 'ZIKA')
  outbreaks_clean <- zika_clean_outbreaks(outbreaks_all, pathogen = 'ZIKA')
  params_clean <- zika_clean_params(params_all, pathogen = 'ZIKA')
  
  # Add qa scores to article df
  articles_qa <- assign_qa_score(articles_clean, ignore_errors = TRUE)#add_qa_scores(articles_clean, params_clean)
  articles_qa <- as.data.frame(articles_qa$articles)
  
  
  # Save genomic data 
  genomic <- params_clean %>%
    filter(parameter_class == 'Mutations') %>%
    left_join(articles_clean %>% select(-name_data_entry), by = c('covidence_id', 'pathogen'))  %>%
    select( -c(starts_with('riskfactor'), r_pathway, seroprevalence_adjusted, third_sample_param_yn,
               contains('delay'), method_2_from_supplement, starts_with('cfr'), 
               starts_with('distribution'), case_definition, exponent_2,
               inverse_param, inverse_param_2, name_data_entry, trimester_exposed, starts_with('parameter_2')))
  
  saveRDS(genomic, "zika_genomic.rds")
  write_csv(genomic, "zika_genomic.csv")
  
  # save cleaned dfs
  saveRDS(articles_qa, 'articles.rds')
  saveRDS(models_clean, 'models.rds')
  saveRDS(outbreaks_clean, 'outbreaks.rds')
  saveRDS(params_clean, 'parameters.rds')
  write_csv(articles_qa, 'articles.csv')
  write_csv(models_clean, 'models.csv')
  write_csv(outbreaks_clean, 'outbreaks.csv')
  write_csv(params_clean, 'parameters.csv')
}
