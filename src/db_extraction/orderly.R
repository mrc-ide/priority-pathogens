library(orderly2)
orderly_strict_mode()

orderly_artefact(
  "Merged data as csv and errors as RDS",
  c("single_extraction_articles.csv", "single_extraction_models.csv",
    "single_extraction_params.csv",
    "double_extraction_articles.csv", "double_extraction_models.csv",
    "double_extraction_params.csv",
    ## Empty for ebola but creating for other
    ## pathogens
    "outbreaks.csv",
    "errors.rds"))

orderly_resource("validation.R")
infiles <- orderly_resource(
  c("DIDE Priority Pathogens EBOLA - ANNE.accdb",
    "DIDE Priority Pathogens EBOLA - CHRISTIAN.accdb",
    "DIDE Priority Pathogens EBOLA - CYRIL.accdb",
    "DIDE Priority Pathogens EBOLA - DARIYA.accdb",
    "DIDE Priority Pathogens EBOLA - ETTIE.accdb",
    "DIDE Priority Pathogens EBOLA - GINA.accdb",
    "DIDE Priority Pathogens EBOLA - JACK.accdb",
    "DIDE Priority Pathogens EBOLA - JOSEPH.accdb",
    "DIDE Priority Pathogens EBOLA - KELLY---.accdb",
    "DIDE Priority Pathogens EBOLA - PABLO.accdb",
    "DIDE Priority Pathogens EBOLA - PATRICK.accdb",
    "DIDE Priority Pathogens EBOLA - REBECCA.accdb",
    "DIDE Priority Pathogens EBOLA - RICHARD.accdb",
    "DIDE Priority Pathogens EBOLA - RUTH.accdb",
    "DIDE Priority Pathogens EBOLA - SABINE.accdb",
    "DIDE Priority Pathogens EBOLA - SANGEETA.accdb",
    "DIDE Priority Pathogens EBOLA - SEQUOIA.accdb",
    "DIDE Priority Pathogens EBOLA - THOM.accdb",
    "DIDE Priority Pathogens EBOLA - TRISTAN.accdb",
    "double/DIDE Priority Pathogens EBOLA - ANNE.accdb", "double/DIDE Priority Pathogens EBOLA - CHRISTIAN.accdb",
    "double/DIDE Priority Pathogens EBOLA - CYRIL.accdb", "double/DIDE Priority Pathogens EBOLA - DARIYA.accdb",
    "double/DIDE Priority Pathogens EBOLA - ETTIE.accdb", "double/DIDE Priority Pathogens EBOLA - GINA.accdb",
    "double/DIDE Priority Pathogens EBOLA - JACK.accdb", "double/DIDE Priority Pathogens EBOLA - JOSEPH.accdb",
    "double/DIDE Priority Pathogens EBOLA - KELLY.accdb", "double/DIDE Priority Pathogens EBOLA - PATRICK.accdb",
    "double/DIDE Priority Pathogens EBOLA - REBECCA.accdb", "double/DIDE Priority Pathogens EBOLA - RUTH.accdb",
    "double/DIDE Priority Pathogens EBOLA - SABINE.accdb", "double/DIDE Priority Pathogens EBOLA - SEQUOIA.accdb",
    "double-round2/DIDE Priority Pathogens ETTIE.accdb", "double-round2/DIDE Priority Pathogens REBECCA.accdb",
    "double-round2/DIDE Priority Pathogens RICHARD.accdb", "double-round2/DIDE Priority Pathogens SANGEETA.accdb",
    "double-round2/DIDE Priority Pathogens TRISTAN.accdb"
  )
)
## pathogen should be set to one of our priority-pathogens
## use capital case; see code below where this pathogen
## is used
## Downstream tasks can query on this parameter to
## pull in the correct files as dependancies.

orderly_parameters(pathogen = NULL)

library(dplyr)
library(ids)
library(odbc)
library(purrr)
library(readr)

source("validation.R")

## Extract one access DB at a time
## Modify primary key.
## Do this for all DBs, create a master CSV
## model, parameter and outbreak
## are all tied together using
## Article_ID
primary_key_col <- "Article_ID"
outfiles <- list(
  articles = "articles.csv",
  models = "models.csv",
  params = "parameters.csv"
  ## TODO add the name of outbreaks table
  ## and add a file name here,
  ## xxx = "outbreaks.csv"
)

from <- map(
  infiles, function(infile) {
    message("Reading ", infile)
    con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=[./", infile, "];"))

    if (is.null(con)){
      message("Error in reading ", infile)
      return()
    }

    res <- dbSendQuery(con, "SELECT * FROM [Article data - Table]")
    articles <- dbFetch(res)
    narticles <- nrow(articles)
    
    if (narticles == 0) return()
    
    ## pathogen-specific Covidence ID fixing before joining
    if (pathogen == "EBOLA") {
      articles$Covidence_ID[articles$DOI == "10.1016/j.rinp.2020.103593" &
                              articles$Name_data_entry == "Christian"] <- 19880
      articles$Covidence_ID[articles$DOI == "10.1142/s1793524517500577" &
                              articles$Name_data_entry == "Thomas Rawson"] <- 11565
      articles$Covidence_ID[articles$DOI == "10.1038/nature14594" &
                              articles$Name_data_entry == "Ettie"] <- 5197
    }
    
    ## Convert Covidence_ID to numeric.
    ## Covidence_ID is entered as a text, so
    ## also save the original i.e., as entered in the DB
    ## to allow errors induced by conversion to be fixed.
    articles$Covidence_ID_text <- articles$Covidence_ID
    articles$Covidence_ID <- as.integer(articles$Covidence_ID)
    
    articles$ID <- random_id(
      n = narticles, use_openssl = FALSE
    )

    res <- dbSendQuery(con, "SELECT * FROM [Model data - Table]")
    models <- dbFetch(res)

    models <- left_join(
      articles[ , c("Article_ID", "ID", "Pathogen", "Covidence_ID", "Name_data_entry")],
      models,
      by = "Article_ID"
    )
    nmodels <- nrow(models)
    models <- models %>% mutate(access_model_id = Model_data_ID)
    models$Model_data_ID <- random_id(
      n = nmodels, use_openssl = FALSE
    )
    

    res <- dbSendQuery(con, "SELECT * FROM [Parameter data - Table]")
    params <- dbFetch(res)


    params <- left_join(
      articles[ , c("Article_ID", "ID", "Pathogen", "Covidence_ID", "Name_data_entry")],
      params,
      by = "Article_ID"
    )
    nparams <- nrow(params)
    params <- params %>% mutate(access_param_id = Parameter_data_ID)
    params$Parameter_data_ID <- random_id(
      n = nparams, use_openssl = FALSE
    )
    articles <- validate(articles)
    models <- validate(models)
    params <- validate(params)
    # TODO Extract outbreaks table where relevant
    list(
      articles = articles, models = models, params = params
    )
  }
)
# Filter out empty databases
from <- keep(from, function(x) !is.null(x))
# Merge databases and then split again
articles <- map_dfr(from, function(x) x[["articles"]])
# SANGEETA TO DO: Make Covidence_ID numeric
models <- map_dfr(from, function(x) x[["models"]])
params <- map_dfr(from, function(x) x[["params"]])


# Ebola-specific cleaning
if(pathogen == "EBOLA") {
  # articles
  articles$Covidence_ID <- as.numeric(articles$Covidence_ID)
  articles$FirstAauthor_Surname[
    articles$Article_ID == 54 &
      articles$Name_data_entry == "Christian"] <- "Atangana"
  articles$FirstAauthor_FirstName[
    articles$Article_ID == 54 &
      articles$Name_data_entry == "Christian"] <- "Abdon"
  articles <- articles %>%
    filter(Article_ID!=14 | Name_data_entry!="Christian") %>%
    filter(!(Covidence_ID %in% c(5349, 1850, 1860, 1863, 2205, 2202, 483))) %>%
    mutate_at(vars(QA_M1, QA_M2, QA_A3, QA_A4, QA_D5, QA_D6, QA_D7),
    ~ifelse(Name_data_entry == "Anne" & Covidence_ID == 6346, "Yes", .))
  # models
  models$Covidence_ID <- as.numeric(models$Covidence_ID)
  models <- models %>% 
    mutate_if(is.character, list(~na_if(.,""))) %>%
    mutate(pathogen = ifelse(pathogen == "Sheppard", "Ebola virus", pathogen))
  model_cols <- colnames(models)
  check_model_cols <- model_cols[! model_cols %in%
                                   c("Article_ID", "ID", "Pathogen",
                                     "Covidence_ID", "Name_data_entry",
                                     "Model_data_ID", "Theoretical_model",
                                     "Code_available", "access_model_id")]
  models <- models %>%
    filter_at(vars(all_of(check_model_cols)), any_vars(!is.na(.)))
  # parameters
  params$Covidence_ID <- as.numeric(params$Covidence_ID)
  params <- params %>% mutate_if(is.character, list(~na_if(.,"")))
    # Add parameter type for accidental extractor errors
  params$Parameter_type[
    params$Covidence_ID == "16757" &
      params$Name_data_entry == "Ruth"] <- "Seroprevalence - IgG"
  params$Parameter_type[
    params$Covidence_ID == "4764" &
      params$Name_data_entry == "Kelly M"] <- "Risk factors"
    # Remove remaining blank parameter type entries
  params <- params %>%
    filter_at(vars(Parameter_type), any_vars(!is.na(.)))
    # Remove entries where all variables that aren't prepopulated are empty
  param_cols <- colnames(params)
  check_param_cols <- param_cols[! param_cols %in%
                                   c("Article_ID", "ID", "Pathogen",
                                     "Covidence_ID", "Name_data_entry",
                                     "Parameter_data_ID", "Exponent",
                                     "Distribution_par1_uncertainty",
                                     "Distribution_par2_uncertainty",
                                     "Method_from_supplement",
                                     "Method_disaggregated",
                                     "Method_disaggregated_only",
                                     "Genomic_sequence_available",
                                     "Inverse_param", "Parameter_FromFigure")]
  params <- params %>%
    filter_at(vars(all_of(check_param_cols)), any_vars(!is.na(.)))
}

## Check data after pathogen-specific cleaning
a_err <- validate_articles(articles)
m_err <- validate_models(models)
p_err <- validate_params(params)
saveRDS(
  list(articles_errors = a_err,
       models_errors = m_err,
       params_errors = p_err),
  "errors.rds"
)

## Write each DB to the same file now.
double_articles <- count(articles, Covidence_ID) %>% filter(n >= 2)
double_a <- articles[articles$Covidence_ID %in% double_articles$Covidence_ID, ]
double_m <- models[models$Covidence_ID %in% double_articles$Covidence_ID, ]
double_p <- params[params$Covidence_ID %in% double_articles$Covidence_ID, ]

single_a <- articles[!articles$Covidence_ID %in% double_articles$Covidence_ID, ]
single_m <- models[!models$Covidence_ID %in% double_articles$Covidence_ID, ]
single_p <- params[!params$Covidence_ID %in% double_articles$Covidence_ID, ]

write_csv(
  double_a, "double_extraction_articles.csv"
)
write_csv(
  double_m, "double_extraction_models.csv"
)
write_csv(
  double_p, "double_extraction_params.csv"
)

write_csv(
  single_a, "single_extraction_articles.csv"
)
write_csv(
  single_m, "single_extraction_models.csv"
)
write_csv(
  single_p, "single_extraction_params.csv"
)
## Empty outbreaks.csv for Ebola
## Amend this for other pathogens
file.create("outbreaks.csv")
