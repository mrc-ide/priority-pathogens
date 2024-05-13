# Task to compile single and double extraction databases together
library(dplyr)
library(janitor)
library(orderly2)
library(readr)


orderly_strict_mode()

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
infiles1 <- orderly_dependency(
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
infiles2 <- orderly_dependency(
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
    ## NIPAH FIXING FILES
    "cleaning.R",
    "lassa_cleaning.R",
    "sars_cleaning.R",
    "ebola_cleaning.R"
  )
)
source("lassa_cleaning.R")
source("sars_cleaning.R")
source("ebola_cleaning.R")
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
  )
)

source("cleaning.R")

## The files do not have UTF-8 encoding,
## before we read anything, we convert them to UTF-8 and then read them
## save them back
infiles <- c(infiles1$files$here, infiles2$files$here)
lapply(infiles, function(infile) {
  x <- read_csv(infile)
  x <- mutate(across(everything(), ~ iconv(., to = "UTF-8")))
  write_csv(x, infile, )
})

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
  clean_names() %>%
  arrange(covidence_id)

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
outbreak_matching <- outbreak_matching %>% select(-c("num_rows", "matching")) %>%
                  mutate(covidence_id=as.numeric(covidence_id),
                         article_id=as.numeric(article_id),
                         access_outbreak_id=as.numeric(access_outbreak_id))

# Double extractions - needed to be resolved between extractors
qa_fixed <- read_csv(fixing_files[[pathogen]][["qa_fix"]])
model_fixed <- read_csv(fixing_files[[pathogen]][["models_fix"]])
parameter_fixed <- read_csv(fixing_files[[pathogen]][["params_fix"]])
outbreak_fixed <- read_csv(fixing_files[[pathogen]][["outbreaks_fix"]])

qa_fixed        <- qa_fixed %>% clean_names()
outbreak_fixed  <- outbreak_fixed %>% clean_names()
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
outbreak_fixed <- outbreak_fixed %>%
               mutate(covidence_id=as.numeric(covidence_id),
                      article_id=as.numeric(article_id),
                      access_outbreak_id=as.numeric(access_outbreak_id))

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

outbreak_fixed <- outbreak_fixed %>%
  filter(fixed == 1) %>%
  select(-c("fixed", "num_rows", "matching"))

# # join article data to qa files
# article_double_details <- article_double %>% select(-c(starts_with("qa")))
# 
# article_matching <- qa_matching %>%
#   select(-c("num_rows", "matching")) %>%
#   left_join(article_double_details,
#     by = c("id", "covidence_id", "name_data_entry")
#   ) %>%
#   distinct(covidence_id, .keep_all = TRUE) %>%
#   mutate(double_extracted = 1)
# 
# article_fixed <- qa_fixed %>%
#   left_join(article_double_details,
#     by = c("covidence_id", "name_data_entry")
#   ) %>%
#   mutate(double_extracted = 1)
# 
# article_single <- article_single %>%
#   mutate(double_extracted = 0)
#  
# join ids
param_double_ids <- param_double %>%
   select(c(
     "article_id", "name_data_entry", "access_param_id", "covidence_id",
     "id", "parameter_data_id"
   ))

parameter_fixed <- parameter_fixed %>%
   left_join(param_double_ids,
     by = c(
       "article_id", "name_data_entry",
       "access_param_id", "covidence_id","id", "parameter_data_id"
     )
   )

model_double_ids <- model_double %>%
  select(c(
    "article_id", "name_data_entry", "access_model_id", "covidence_id",
    "id", "model_data_id"
  ))

model_fixed <- model_fixed %>%
  left_join(model_double_ids,
    by = c(
      "article_id", "name_data_entry",
      "access_model_id", "covidence_id", "id", "model_data_id"
    )
  )

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
outbreak_all  <- lassa_outbreaks_cleaning(outbreak_all) %>% 
  clean_dfs(pathogen)
model_all     <- clean_dfs(model_all, pathogen)
parameter_all <- clean_dfs(parameter_all, pathogen)

# # Add article QA scores to article data
# article_all <- add_qa_scores(article_all, parameter_all)
# 
# # Add article QA scores as a parameter variable
# parameter_all <- parameter_all %>%
#   left_join(
#     select(article_all, covidence_id, article_qa_score),
#     by = "covidence_id"
#   )
print(class(article_all))
write_csv(article_all, "articles.csv")
if (pathogen == "EBOLA") {
  file.create("outbreaks.csv")
} else {
  write_csv(outbreak_all, "outbreaks.csv")
}
write_csv(model_all, "models.csv")
write_csv(parameter_all, "parameters.csv")