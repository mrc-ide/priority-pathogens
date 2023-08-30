library(orderly2)
orderly_strict_mode()

orderly_artefact(
  "Merged data as csv",
  c("ebola_article.csv", "ebola_model.csv", "ebola_parameters.csv"))

orderly_resource(
  c("DIDE Priority Pathogens EBOLA - SABINE.accdb",
    "DIDE Priority Pathogens EBOLA - SANGEETA.accdb"
    )
)

library(dplyr)
library(here)
library(Hmisc)
library(purrr)
library(readr)

## Plan
## Extract one access DB at a time
## Modify primary key.
## Write to csv
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
      pattern = "DIDE Priority Pathogens EBOLA - ",
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

primary_key_col <- "Article_ID"
extractor_col <- "Name_data_entry"
outfiles <- list(
  "Article data - Table" = "ebola_article.csv",
  "Model data - Table" = "ebola_model.csv",
  "Parameter data - Table" = "ebola_parameters.csv"
)

from <- map(
  infiles, function(infile) {
    message("Reading ", infile)
    mdb.get(infile, allow = "_")
  }
)


## Remove databases with empty tables
from <- keep(from, function(y) nrow(y[[1]]) > 0)

## First modify the article column, adding an
## extra column. Then get this new column tacked
## on to the other tables as well.
## Deal with blank extractor name first
from <- imap(
  from, function(x, y) {
    message("Now reading ", y)
    names <- x[["Article data - Table"]][[extractor_col]]
    ## TODO Check that a blank field does show up as 0 length
    ## string
    blank <- which(lapply(names, length) == 0)
    names[blank] <- "Blank"
    ## Append a new column
    x[["Article data - Table"]]$ID <- paste0(
      x[["Article data - Table"]][[primary_key_col]],
      "_", names
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
    ## Same for the third table
    x[["Parameter data - Table"]][[primary_key_col]] <- as.integer(
      x[["Parameter data - Table"]][[primary_key_col]]
    )
    x[["Parameter data - Table"]] <- left_join(
      x[["Article data - Table"]][ , c(primary_key_col, "ID")],
      x[["Parameter data - Table"]],
      by = primary_key_col
    )
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
## Clean-up
unlink(outdir, recursive = TRUE)
