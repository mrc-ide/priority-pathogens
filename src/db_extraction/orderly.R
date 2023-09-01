library(orderly2)
orderly_strict_mode()

orderly_artefact(
  "Merged data as csv",
  c("single_extraction_articles.csv", "single_extraction_models.csv",
    "single_extraction_params.csv",
    "double_extraction_articles.csv", "double_extraction_models.csv",
    "double_extraction_params.csv",
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
    "DIDE Priority Pathogens EBOLA - KELLY---.accdb",
    "DIDE Priority Pathogens EBOLA - PATRICK.accdb",
    "DIDE Priority Pathogens EBOLA - REBECCA.accdb",
    "DIDE Priority Pathogens EBOLA - RUTH.accdb",
    "DIDE Priority Pathogens EBOLA - SABINE.accdb",
    "DIDE Priority Pathogens EBOLA - SANGEETA.accdb",
    "DIDE Priority Pathogens EBOLA - SEQUOIA.accdb",
    "double/DIDE Priority Pathogens EBOLA - ANNA_MARIA.accdb", 
    "double/DIDE Priority Pathogens EBOLA - ANNE.accdb", "double/DIDE Priority Pathogens EBOLA - CHRISTIAN.accdb", 
    "double/DIDE Priority Pathogens EBOLA - CYRIL.accdb", "double/DIDE Priority Pathogens EBOLA - DARIYA.accdb", 
    "double/DIDE Priority Pathogens EBOLA - ETTIE.accdb", "double/DIDE Priority Pathogens EBOLA - GINA.accdb", 
    "double/DIDE Priority Pathogens EBOLA - JACK.accdb", "double/DIDE Priority Pathogens EBOLA - JOSEPH.accdb", 
    "double/DIDE Priority Pathogens EBOLA - KEITH.accdb", "double/DIDE Priority Pathogens EBOLA - KELLY.accdb", 
    "double/DIDE Priority Pathogens EBOLA - PABLO.accdb", "double/DIDE Priority Pathogens EBOLA - PATRICK.accdb", 
    "double/DIDE Priority Pathogens EBOLA - PAULA.accdb", "double/DIDE Priority Pathogens EBOLA - REBECCA.accdb", 
    "double/DIDE Priority Pathogens EBOLA - RICHARD.accdb", "double/DIDE Priority Pathogens EBOLA - RUTH.accdb", 
    "double/DIDE Priority Pathogens EBOLA - SABINE.accdb", "double/DIDE Priority Pathogens EBOLA - SANGEETA.accdb", 
    "double/DIDE Priority Pathogens EBOLA - SEQUOIA.accdb", "double/DIDE Priority Pathogens EBOLA - TRISTAN.accdb",
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

# check that none of the fields contain a comma
# Implement other checks here.
validate <- function(df) {
  out <- apply(df, 2, function(x) {
    gsub(pattern = ",", replacement = ";", x = x)
  })
  as.data.frame(out)
}

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
    articles <- validate(articles)
    models <- validate(models)
    params <- validate(models)
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
models <- map_dfr(from, function(x) x[["models"]])
params <- map_dfr(from, function(x) x[["params"]])



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
