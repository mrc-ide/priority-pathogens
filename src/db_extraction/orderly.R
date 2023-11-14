library(dplyr)
library(ids)
library(odbc)
library(orderly2)
library(orderly.sharedfile)
library(purrr)
library(readr)

## pathogen should be set to one of our priority-pathogens
## use capital case; see code below where this pathogen
################### README ###################
## IMPORTANT WHEN RUNNING INTERACTIVELY, FIRST COMMENT OUT THIS LINE:
#orderly_parameters(pathogen = NULL)
## orderly will scan orderly.R in the interactive mode, so that
## even if the above line is not run, you WILL get an error
## It is therefore important that the line is commented out *BEFORE*
## you start executing the script line by line.
## In the interactive mode, uncomment the line below and set the pathogen variable directly
## like this
pathogen <- "EBOLA"
## then run as normal.
## ONCE DONE, PLEASE COMMENT OUT THE DIRECT SETTING OF THE VARIABLE pathogen
## and uncomment the call to orderly_parameters.
######################################
## Downstream tasks can query on this parameter to
## pull in the correct files as dependencies.
## When running as an orderly task leave line 13 as it is

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

orderly_resource(c("pathogen_cleaning.R", "validation.R"))
orderly_shared_resource("utils.R" = "utils.R")

source("utils.R")
source("pathogen_cleaning.R")
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

all_conns <- map(
  infiles, function(infile) {
    message("Reading ", infile)
    con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=[", infile, "];"))

    if (is.null(con)) {
      message("Error in reading ", infile)
      return()
    }
    con
  }
)

all_articles <- map(
  all_conns, function(con) {
    res <- dbSendQuery(con, "SELECT * FROM [Article data - Table]")
    articles <- dbFetch(res)
    narticles <- nrow(articles)

    if (narticles == 0) {
      return()
    }
    ## pathogen-specific Covidence ID fixing before joining
    ## Fixes for Ebola
    articles$Covidence_ID[articles$DOI == "10.1016/j.rinp.2020.103593" &
      articles$Name_data_entry == "Christian"] <- 19880
    articles$Covidence_ID[articles$DOI == "10.1142/s1793524517500577" &
      articles$Name_data_entry == "Thomas Rawson"] <- 11565
    articles$Covidence_ID[articles$DOI == "10.1038/nature14594" &
      articles$Name_data_entry == "Ettie"] <- 5197

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
    dbClearResult(res)
    articles
  }
)
# Save all objects directly as extracted from the database for ease of
# debugging.
saveRDS(all_articles, "all_articles_raw.rds")
orderly_artefact("All articles", "all_articles_raw.rds")

all_models <- map(
  all_conns, function(con) {
    res <- dbSendQuery(con, "SELECT * FROM [Model data - Table]")
    models <- dbFetch(res)
    nmodels <- nrow(models)
    models <- models %>% mutate(access_model_id = Model_data_ID)
    models$Model_data_ID <- random_id(
      n = nmodels, use_openssl = FALSE
    )
    dbClearResult(res)
    models
  }
)

saveRDS(all_models, "all_models_raw.rds")
orderly_artefact("All Models", "all_models_raw.rds")

all_params <- map(
  all_conns, function(con) {
    res <- dbSendQuery(con, "SELECT * FROM [Parameter data - Table]")
    params <- dbFetch(res)
    nparams <- nrow(params)
    params <- params %>% mutate(access_param_id = Parameter_data_ID)
    params$Parameter_data_ID <- random_id(
      n = nparams, use_openssl = FALSE
    )
    dbClearResult(res)
    params
  }
)

saveRDS(all_params, "all_params_raw.rds")
orderly_artefact("All parameters", "all_params_raw.rds")

# Check if we have extracted outbreaks for this pathogen
all_tables <- dbListTables(all_conns[[1]])
outbreaks_ex <- ("Outbreak data - Table" %in% all_tables)
if (!outbreaks_ex) {
  all_outbreaks <- data.frame()
} else {
  all_outbreaks <- map(
    all_conns, function(con) {
      res <- dbSendQuery(con, "SELECT * FROM [Outbreak data - Table]")
      outbreaks <- dbFetch(res)
      noutbreaks <- nrow(outbreaks)
      outbreaks <- outbreaks %>% mutate(access_outbreak_id = Outbreak_ID)
      outbreaks$Outbreak_data_ID <- random_id(
        n = noutbreaks, use_openssl = FALSE
      )
      dbClearResult(res)
      outbreaks
    }
  )
}

saveRDS(all_outbreaks, "all_outbreaks_raw.rds")
# Close all connections
walk(all_conns, function(con) dbDisconnect(con))

# Get rid of NULL or 0-length results
null_articles <- map_lgl(all_articles, function(x) is.null(x))
all_articles <- all_articles[!null_articles]
all_models <- all_models[!null_articles]
all_params <- all_params[!null_articles]

from <- pmap(
  list(
    articles = all_articles,
    models = all_models,
    params = all_params
  ), function(articles, models, params) {
    models <- left_join(
      models,
      articles[, c("Article_ID", "ID", "Pathogen", "Covidence_ID", "Name_data_entry")],
      by = "Article_ID"
    )


    params <- left_join(
      params,
      articles[, c("Article_ID", "ID", "Pathogen", "Covidence_ID", "Name_data_entry")],
      by = "Article_ID"
    )
    articles <- validate(articles)
    models <- validate(models)
    params <- validate(params)

    list(
      articles = articles, models = models, params = params
    )
  }
)

if (outbreaks_ex) {
  outbreaks <- pmap_dfr(
    list(
      articles = all_articles,
      outbreaks = all_outbreaks
    ), function(articles, outbreaks) {
      left_join(
        outbreaks,
        articles[, c("Article_ID", "ID", "Pathogen", "Covidence_ID", "Name_data_entry")],
        by = "Article_ID"
      )
    }
  )
} else {
  outbreaks <- data.frame()
}



# Merge databases and then split again
articles <- map_dfr(from, function(x) x[["articles"]])
models <- map_dfr(from, function(x) x[["models"]])
params <- map_dfr(from, function(x) x[["params"]])

# Pathogen-specific cleaning
articles <- clean_articles(articles)
models <- clean_models(models)
params <- clean_params(params)

## Check data after pathogen-specific cleaning
a_err <- validate_articles(articles)
m_err <- validate_models(models)
p_err <- validate_params(params)

if (outbreaks_ex) {
  o_err <- validate_outbreaks(outbreaks)
} else {
  o_err <- NULL
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
single_a <- articles[!articles$Covidence_ID %in% double_articles$Covidence_ID, ]

double_m <- models[models$Covidence_ID %in% double_articles$Covidence_ID, ]
single_m <- models[!models$Covidence_ID %in% double_articles$Covidence_ID, ]

double_p <- params[params$Covidence_ID %in% double_articles$Covidence_ID, ]
single_p <- params[!params$Covidence_ID %in% double_articles$Covidence_ID, ]

if (outbreaks_ex) {
  # this will be empty for Lassa
  double_o <- outbreaks[outbreaks$Covidence_ID %in% double_articles$Covidence_ID, ]
  single_o <- outbreaks[!outbreaks$Covidence_ID %in% double_articles$Covidence_ID, ]
} else {
  single_o <- data.frame()
  double_o <- data.frame()
}


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

write_csv(
  single_o, "single_extraction_outbreaks.csv"
)

write_csv(
  double_o, "double_extraction_outbreaks.csv"
)
