# Task to compile single and double extraction databases together
library(dplyr)
library(janitor)
library(orderly2)
library(readr)
library(epireview)

## Outputs
if(pathogen!="OROV"){
  orderly_artefact(
    "Merged single and double extracted data as csv",
    c(
      "articles.csv",
      "models.csv",
      "parameters.csv",
      "outbreaks.csv"
    ))
} else if(pathogen=="OROV"){
  orderly_artefact(
    "Merged single and double extracted data as csv",
    c(
      "articles.csv",
      "parameters.csv",
      "outbreaks.csv"
    ))

}


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

filepath_base <- "access"

# Manually fixed files and "cleaning" script - these need to be in the
# src/db_compilation folder
orderly_resource(
  c(
    file.path(filepath_base, "ebola_qa_fixing.csv"),
    file.path(filepath_base, "ebola_params_fixing.csv"),
    file.path(filepath_base, "ebola_models_fixing.csv"),
    ## LASSA FIXING FILES
    file.path(filepath_base, "lassa_qa_fixing.csv"),
    file.path(filepath_base, "lassa_params_fixing.csv"),
    file.path(filepath_base, "lassa_models_fixing.csv"),
    file.path(filepath_base, "lassa_outbreaks_fixing.csv"),
    ## SARS FIXING FILES
    file.path(filepath_base, "sars_qa_fixing.csv"),
    file.path(filepath_base, "sars_params_fixing.csv"),
    file.path(filepath_base, "sars_models_fixing.csv"),
    ## OROV FIXING FILES
    #"orov_qa_fixing.csv",
    #"orov_params_fixing.csv",
    #"orov_models_fixing.csv",
    #"orov_outbreaks_fixing.csv",
    file.path(filepath_base, "cleaning.R"),
    file.path(filepath_base, "sars_cleaning.R"),
    file.path(filepath_base,  "ebola_cleaning.R"),
    file.path(filepath_base, "lassa_cleaning.R")
  )
)

orderly_shared_resource("ebola_functions.R" = "ebola_functions.R")
source("ebola_functions.R")
## Here we map the fixing files to the
## pathogen.
fixing_files <- list(
  EBOLA = list(
    params_fix =  file.path(filepath_base, "ebola_params_fixing.csv"),
    models_fix =  file.path(filepath_base, "ebola_models_fixing.csv"),
    qa_fix =  file.path(filepath_base, "ebola_qa_fixing.csv")
  ),
  LASSA = list(
     params_fix =  file.path(filepath_base, "lassa_params_fixing.csv"),
     models_fix =  file.path(filepath_base, "lassa_models_fixing.csv"),
     qa_fix =  file.path(filepath_base, "lassa_qa_fixing.csv"),
     outbreaks_fix =  file.path(filepath_base, "lassa_outbreaks_fixing.csv")
  ),
  SARS = list(
    params_fix =  file.path(filepath_base, "sars_params_fixing.csv"),
    models_fix =  file.path(filepath_base, "sars_models_fixing.csv"),
    qa_fix =  file.path(filepath_base, "sars_qa_fixing.csv")
  ),
  OROV = list(
    params_fix =  file.path(filepath_base, "orov_params_fixing.csv"),
    #models_fix = "orov_models_fixing.csv",
    qa_fix =  file.path(filepath_base, "orov_qa_fixing.csv"),
    outbreaks_fix =  file.path(filepath_base, "orov_outbreaks_fixing.csv")
  )
)

source(file.path(filepath_base, "cleaning.R"))
source(file.path(filepath_base, "ebola_cleaning.R"))
source(file.path(filepath_base, "lassa_cleaning.R"))
source(file.path(filepath_base, "sars_cleaning.R"))

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
article_double <- read_csv("double_extraction_articles.csv",
                           col_types = cols(Covidence_ID = col_integer(),
                                            Article_ID = col_integer()))
model_double <- read_csv("double_extraction_models.csv",
                         col_types = cols(Covidence_ID = col_integer(),
                                          Article_ID = col_integer(),
                                          access_model_id = col_integer()))
param_double <- read_csv("double_extraction_params.csv",
                         col_types = cols(Covidence_ID = col_integer(),
                                          Article_ID = col_integer(),
                                          access_param_id = col_integer()))
outbreak_double <- read_csv("double_extraction_outbreaks.csv",
                            col_types = cols(Covidence_ID = col_integer(),
                                             Article_ID = col_integer(),
                                             access_outbreak_id = col_integer()))


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
if(pathogen!="OROV"){
  model_fixed <- read_csv(fixing_files[[pathogen]][["models_fix"]])
}
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

if(pathogen!="OROV"){
  model_fixed     <- model_fixed %>% clean_names()
  model_fixed <- model_fixed %>%
    mutate(covidence_id=as.numeric(covidence_id),
           article_id=as.numeric(article_id),
           access_model_id=as.numeric(access_model_id))
}


parameter_fixed <- parameter_fixed %>% clean_names()

parameter_fixed <- parameter_fixed %>%
               mutate(covidence_id=as.numeric(covidence_id),
                      article_id=as.numeric(article_id),
                      access_param_id=as.numeric(access_param_id))


## create final datasets
# Add outbreak_fixed for next pathogen
qa_fixed <- qa_fixed %>%
  filter(fixed == 1) %>%
  select(-c("fixed", "num_rows", "matching"))

parameter_fixed <- parameter_fixed %>%
  filter(fixed == 1) %>%
  select(-c("fixed", "num_rows", "matching"))

if(pathogen!="OROV"){
  model_fixed <- model_fixed %>%
    filter(fixed == 1) %>%
    select(-c("fixed", "num_rows", "matching"))
}


if (pathogen %in% c("EBOLA", "SARS", "OROV")) {
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

if(pathogen!="OROV"){
  model_fixed <- left_join(model_fixed, model_double_ids)
}

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


if(pathogen!="OROV"){
  model_all <- rbind(
  model_single,
  model_matching,
  model_fixed
)
}


outbreak_all <- rbind(
  outbreak_single,
  outbreak_matching,
  outbreak_fixed
)

# Cleaning
if (pathogen != "OROV"){
  article_all   <- clean_dfs(article_all, pathogen)
}

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

if (pathogen != "OROV"){
  model_all     <- clean_dfs(model_all, pathogen)
}

if (pathogen == "LASSA") model_all <- lassa_models_cleaning(model_all)

if (pathogen != "OROV"){
  parameter_all <- clean_dfs(parameter_all, pathogen)
}

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
if(pathogen!="OROV"){
  write_csv(model_all, "models.csv")
}

write_csv(parameter_all, "parameters.csv")
