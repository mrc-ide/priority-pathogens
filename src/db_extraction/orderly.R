library(orderly2)
orderly_strict_mode()

orderly_artefact(
  "Merged data as csv",
  c("articles.csv", "models.csv",
    "parameters.csv",
    ## Empty for ebola but creating for other
    ## pathogens
    "outbreaks.csv"))

infiles <- orderly_resource(
  c("DIDE Priority Pathogens EBOLA - ANNE.accdb",
    "DIDE Priority Pathogens EBOLA - CHRISTIAN.accdb",
    "DIDE Priority Pathogens EBOLA - CYRIL.accdb",
    "DIDE Priority Pathogens EBOLA - DARIYA.accdb",
    "DIDE Priority Pathogens EBOLA - ETTIE.accdb",
    "DIDE Priority Pathogens EBOLA - GINA.accdb",
    "DIDE Priority Pathogens EBOLA - JACK.accdb",
    "DIDE Priority Pathogens EBOLA - JOSEPH.accdb",
    "DIDE Priority Pathogens EBOLA - KELLY.accdb",
    "DIDE Priority Pathogens EBOLA - PATRICK.accdb",
    "DIDE Priority Pathogens EBOLA - REBECCA.accdb",
    "DIDE Priority Pathogens EBOLA - RUTH.accdb",
    "DIDE Priority Pathogens EBOLA - SABINE.accdb",
    "DIDE Priority Pathogens EBOLA - SANGEETA.accdb",
    "DIDE Priority Pathogens EBOLA - SEQUOIA.accdb"
  )
)
## pathogen should be set to one of our priority-pathogens
## use capital case; see code below where this pathogen
## is used in the file name
## Downstream tasks can query on this parameter to
## pull in the correct files as dependancies.
## Extraction can be single or double.
## This is so that downstream tasks can pull in the right
## outputs and put them together.
orderly_parameters(pathogen = NULL, extraction = "single")

library(dplyr)
library(ids)
library(odbc)
library(purrr)
library(readr)


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
    params$Parameter_data_ID <- random_id(
      n = nparams, use_openssl = FALSE
    )
    
    # TODO Extract outbreaks table where relevant
    list(
      articles = articles, models = models, params = params
    )
  }
)

## Write each DB to the same file now.
walk(
  from, function(x) {
    iwalk(x, function(df, dfname) {
      ## TODO
      ## have to do something about this if we
      ## decide to commit these files to git
      file <- outfiles[[dfname]] 
      append <- FALSE
      if (file.exists(file)) append <- TRUE
      message("Writing to ", file)
      write_csv(df, file, append = append)
    }
    )
      }
    )
  

## Empty outbreaks.csv for Ebola
## Amend this for other pathogens
file.create("outbreaks.csv")
## Clean-up
unlink(outdir, recursive = TRUE)
