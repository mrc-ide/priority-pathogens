library(dplyr)
library(ids)
library(odbc)
library(orderly2)
library(orderly.sharedfile)
library(purrr)
library(readr)

## pathogen should be set to one of our priority-pathogens
## use capital case; see code below where this pathogen
## is used
## Downstream tasks can query on this parameter to
## pull in the correct files as dependancies.
## When running interactively, do not run this line , 

orderly_parameters(pathogen = NULL)
## set the pathogen variable directly as in the commented line below
## pathogen <- "EBOLA"
orderly_artefact(
  "Merged data as csv and errors as RDS",
  c(
    "single_extraction_articles.csv", "single_extraction_models.csv",
    "single_extraction_params.csv", "single_extraction_outbreaks.csv",
    "double_extraction_articles.csv", "double_extraction_models.csv",
    "double_extraction_params.csv", "double_extraction_outbreaks.csv",
    "errors.rds"
  )
)

orderly_resource("validation.R")
orderly_shared_resource("utils.R" = "utils.R")

source("utils.R")
source("validation.R")
## First get the pathogen-specific nested list from the function
## database_files and then plonk them into the function
## sharedfile_path one by one. Careful as there may be one or more
## shared locations from which to read the files.
infiles <- database_files(pathogen)
infiles <- imap(
  infiles, function(filenames, dbname) {
    map(filenames, function(fname) {
      sharedfile_path(fname, from = dbname)
    })
  }
)
infiles <- unlist(infiles)

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
  params = "parameters.csv",
  outbreaks = "outbreaks.csv"
)

from <- map(
  infiles, function(infile) {
    message("Reading ", infile)
    con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=[", infile, "];"))

    if (is.null(con)) {
      message("Error in reading ", infile)
      return()
    }

    res <- dbSendQuery(con, "SELECT * FROM [Article data - Table]")
    articles <- dbFetch(res)
    narticles <- nrow(articles)

    if (narticles == 0) {
      return()
    }

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
    articles$Covidence_ID <- gsub(" ", "", articles$Covidence_ID)
    articles$Covidence_ID <- as.integer(articles$Covidence_ID)
    

    articles$ID <- random_id(
      n = narticles, use_openssl = FALSE
    )

    res <- dbSendQuery(con, "SELECT * FROM [Model data - Table]")
    models <- dbFetch(res)
    
    models <- left_join(
      models,
      articles[, c("Article_ID", "ID", "Pathogen", "Covidence_ID", "Name_data_entry")],
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
      params,
      articles[, c("Article_ID", "ID", "Pathogen", "Covidence_ID", "Name_data_entry")],
      by = "Article_ID"
    )
    nparams <- nrow(params)
    params <- params %>% mutate(access_param_id = Parameter_data_ID)
    params$Parameter_data_ID <- random_id(
      n = nparams, use_openssl = FALSE
    )

    if (pathogen == "LASSA") {
      res <- dbSendQuery(con, "SELECT * FROM [Outbreak data - Table]")
      outbreaks <- dbFetch(res)

      outbreaks <- left_join(
        outbreaks,
        articles[, c("Article_ID", "ID", "Pathogen", "Covidence_ID", "Name_data_entry")],
        by = "Article_ID"
      )
      noutbreaks <- nrow(outbreaks)
      outbreaks <- outbreaks %>% mutate(access_outbreak_id = Outbreak_ID)
      outbreaks$Outbreak_data_ID <- random_id(
        n = noutbreaks, use_openssl = FALSE
      )
    }

    articles <- validate(articles)
    models <- validate(models)
    params <- validate(params)

    if (pathogen == "EBOLA") {
      outbreaks <- NULL
    }

    if (pathogen == "LASSA") {
      outbreaks <- validate(outbreaks)
    }

    # TODO Extract outbreaks table where relevant
    list(
      articles = articles, models = models, params = params, outbreaks = outbreaks
    )
  }
)
# Filter out empty databases
from <- keep(from, function(x) !is.null(x))
# Merge databases and then split again
articles <- map_dfr(from, function(x) x[["articles"]])
models <- map_dfr(from, function(x) x[["models"]])
params <- map_dfr(from, function(x) x[["params"]])

if (pathogen == "LASSA") {
  outbreaks <- map_dfr(from, function(x) x[["outbreaks"]])
}

# Ebola-specific cleaning
if (pathogen == "EBOLA") {
  # articles
  articles$Covidence_ID <- as.numeric(articles$Covidence_ID)
  articles <- articles %>%
    filter(Article_ID != 14 | Name_data_entry != "Christian") %>%
    # For some reason surname and first name are the wrong way around
    rename(
      temp_col = FirstAuthor_FirstName,
      FirstAuthor_FirstName = FirstAauthor_Surname
    ) %>%
    rename(FirstAauthor_Surname = temp_col) %>%
    filter(!(Covidence_ID %in% c(5349, 1850, 1860, 1863, 2205, 2202, 483))) %>%
    mutate_at(
      vars(QA_M1, QA_M2, QA_A3, QA_A4, QA_D5, QA_D6, QA_D7),
      ~ ifelse(Name_data_entry == "Anne" & Covidence_ID == 6346, "Yes", .)
    ) %>%
    mutate(Pathogen = ifelse(Pathogen == "Sheppard", "Ebola virus",
      ifelse(Pathogen == "Unwin", "Ebola virus",
        Pathogen
      )
    ))
  # models
  models$Covidence_ID <- as.numeric(models$Covidence_ID)
  models <- models %>%
    mutate_if(is.character, list(~ na_if(., ""))) %>%
    mutate(Pathogen = ifelse(Pathogen == "Sheppard", "Ebola virus",
      ifelse(Pathogen == "Unwin", "Ebola virus",
        Pathogen
      )
    ))
  model_cols <- colnames(models)
  check_model_cols <- model_cols[!model_cols %in%
    c(
      "Article_ID", "ID", "Pathogen",
      "Covidence_ID", "Name_data_entry",
      "Model_data_ID", "Theoretical_model",
      "Code_available", "access_model_id"
    )]
  models <- models %>%
    filter_at(vars(all_of(check_model_cols)), any_vars(!is.na(.)))
  # parameters
  params$Covidence_ID <- as.numeric(params$Covidence_ID)
  params <- params %>%
    mutate_if(is.character, list(~ na_if(., ""))) %>%
    mutate(Pathogen = ifelse(Pathogen == "Sheppard", "Ebola virus",
      ifelse(Pathogen == "Unwin", "Ebola virus",
        Pathogen
      )
    ))
  # Add parameter type for accidental extractor errors
  params$Parameter_type[
    params$Covidence_ID == "16757" &
      params$Name_data_entry == "Ruth"
  ] <- "Seroprevalence - IgG"
  params$Parameter_type[
    params$Covidence_ID == "4764" &
      params$Name_data_entry == "Kelly M"
  ] <- "Risk factors"
  # Remove remaining blank parameter type entries
  params <- params %>%
    filter_at(vars(Parameter_type), any_vars(!is.na(.)))
  # Remove entries where all variables that aren't prepopulated are empty
  param_cols <- colnames(params)
  check_param_cols <- param_cols[!param_cols %in%
    c(
      "Article_ID", "ID", "Pathogen",
      "Covidence_ID", "Name_data_entry",
      "Parameter_data_ID", "Exponent",
      "Distribution_par1_uncertainty",
      "Distribution_par2_uncertainty",
      "Method_from_supplement",
      "Method_disaggregated",
      "Method_disaggregated_only",
      "Genomic_sequence_available",
      "Inverse_param", "Parameter_FromFigure"
    )]
  params <- params %>%
    filter_at(vars(all_of(check_param_cols)), any_vars(!is.na(.)))
}

## Check data after pathogen-specific cleaning
a_err <- validate_articles(articles)
m_err <- validate_models(models)
p_err <- validate_params(params)

if (pathogen == "EBOLA") {
  o_err <- NULL
}

if (pathogen == "LASSA") {
  o_err <- validate_outbreaks(outbreaks, pathogen)
}

saveRDS(
  list(
    articles_errors = a_err,
    models_errors = m_err,
    params_errors = p_err,
    outbreak_errors = o_err
  ),
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

if (pathogen == "LASSA") {
  single_o <- outbreaks
  write_csv(
    single_o, "single_extraction_outbreaks.csv"
  )

  file.create("double_extraction_outbreaks.csv")
}

## Empty outbreaks.csv for Ebola
## Amend this for other pathogens
if (pathogen == "EBOLA") {
  file.create("single_extraction_outbreaks.csv")
  file.create("double_extraction_outbreaks.csv")
}
