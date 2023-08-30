library(orderly2)
orderly_strict_mode()

orderly_artefact(
  "Merged data as csv",
  c("articles.csv", "models.csv",
    "parameters.csv",
    ## Empty for ebola but creating for other
    ## pathogens
    "outbreaks.csv"))

orderly_resource(
  c("DIDE Priority Pathogens EBOLA - SABINE.accdb",
    "DIDE Priority Pathogens EBOLA - SANGEETA.accdb"
    )
)
## pathogen should be set to one of our priority-pathogens
## use capital case; see code below where this pathogen
## is used in the file name
## Downstream tasks can query on this parameter to
## pull in the correct files as dependancies.
## Extraction can be single or double.
## This is so that downsteam tasks can pull in the right
## outputs and put them together.
orderly_parameters(pathogen = NULL, extraction = "single")

library(dplyr)
library(here)
library(Hmisc)
library(ids)
library(purrr)
library(readr)

## Extract one access DB at a time
## Modify primary key.
## Do this for all DBs, create a master CSV
indir <- "."
outdir <- "renamed"
if (! dir.exists(outdir)) dir.create(outdir)
infiles <- list.files(path = indir, pattern = "*.accdb")

## make a copy so that the task can be run again if needed
file.copy(
  from = infiles, to = outdir, overwrite = TRUE
)
## Rename as otherwise you get an error while
## reading them in
## This only needs to be done, so can be moved outside
## of this file.
outfiles <- sapply(
  infiles, function(infile) {
    gsub(
      pattern = paste0("DIDE Priority Pathogens ", pathogen, " - "),
      replacement = "",
      x = infile
    )
  }
)

file.rename(
  from = list.files(path = outdir, pattern = "*.accdb", full.names = TRUE),
  to = paste0(outdir, "/", outfiles)
)

infiles <- list.files(path = outdir, pattern = "*.accdb", full.names = TRUE)
names(infiles) <- infiles

## model, parameter and outbreak
## are all tied together using
## Article_ID
primary_key_col <- "Article_ID"
outfiles <- list(
  "Article data - Table" = "articles.csv",
  "Model data - Table" = "models.csv",
  "Parameter data - Table" = "parameters.csv"
  ## TODO add the name of outbreaks table
  ## and add a file name here,
  ## xxx = "outbreaks.csv"
)

from <- map(
  infiles, function(infile) {
    message("Reading ", infile)
    mdb.get(infile, allow = "_")
  }
)

## Remove databases with empty tables
from <- keep(from, function(y) nrow(y[[1]]) > 0)

## Update the ID of each table.
## First modify the article column, adding an
## extra column. Then get this new column tacked
## on to the other tables as well.
## Deal with blank extractor name first
from <- imap(
  from, function(x, y) {
    message("Now reading ", y)
    ## Append a new column
    narticles <- nrow(x[["Article data - Table"]])


    x[["Article data - Table"]]$ID <- random_id(
      n = narticles, use_openssl = FALSE
    )
    ## Join with other tables so that they get this
    ## new column.
    ## Convert to integer as this is read in as
    ## labelled data
    x[["Model data - Table"]][[primary_key_col]] <- as.integer(
      x[["Model data - Table"]][[primary_key_col]]
    )
    x[["Model data - Table"]] <- left_join(
      x[["Article data - Table"]][ , c(primary_key_col, "ID")],
      x[["Model data - Table"]],
      by = primary_key_col
    )
    ## update model_id column as it has no use now
    ## better than having multiple ID columns hanging around
    nmodels <- nrow(x[["Model data - Table"]])
    x[["Model data - Table"]][["Model_data_ID"]] <- random_id(
      n = nmodels, use_openssl = FALSE
    )
    ## Same for the third table
    x[["Parameter data - Table"]][[primary_key_col]] <- as.integer(
      x[["Parameter data - Table"]][[primary_key_col]]
    )
    x[["Parameter data - Table"]] <- left_join(
      x[["Article data - Table"]][ , c(primary_key_col, "ID")],
      x[["Parameter data - Table"]],
      by = primary_key_col
    )
    nparams <- nrow(x[["Parameter data - Table"]])
    x[["Parameter data - Table"]][["Parameter_data_ID"]] <-  random_id(
      n = nparams, use_openssl = FALSE
    )
    ## TODO
    ## Do the same for outbreaks table
    x
  }
)
## Write each DB to the same file now.
walk(
  from, function(x) {
    walk2(
      outfiles, x, function(file, df) {
        ## TODO
        ## have to do something about this if we
        ## decide to commit these files to git
        append <- FALSE
        if (file.exists(file)) append <- TRUE
        message(file)
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
