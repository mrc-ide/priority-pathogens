## List all the database files here
## This simple function has a nested list with the
## following format:
## list(
##     pathogen1 = list(
##         db11 = character vector of file names,
##         db12 = character vector of file names,
##         ),
##     pathogen2 = list(
##         db21 = character vector of file names,
##         db21 = character vector of file names,
##         )
## )
## pathogen1 etc are pathogen names
## db11, db12 are the names given to the
## database location in orderly_config.yml
## See README for more information on this
database_files <- function(pathogen) {
  infiles <- list(
    EBOLA = list(
      singledb = c(
        "DIDE Priority Pathogens EBOLA - ANNE.accdb",
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
        "DIDE Priority Pathogens EBOLA - TRISTAN.accdb"
      ),
      doubledb = c(
        "DIDE Priority Pathogens EBOLA - ANNE.accdb", "DIDE Priority Pathogens EBOLA - CHRISTIAN.accdb",
        "DIDE Priority Pathogens EBOLA - CYRIL.accdb", "DIDE Priority Pathogens EBOLA - DARIYA.accdb",
        "DIDE Priority Pathogens EBOLA - ETTIE.accdb", "DIDE Priority Pathogens EBOLA - GINA.accdb",
        "DIDE Priority Pathogens EBOLA - JACK.accdb", "DIDE Priority Pathogens EBOLA - JOSEPH.accdb",
        "DIDE Priority Pathogens EBOLA - KELLY.accdb", "DIDE Priority Pathogens EBOLA - PATRICK.accdb",
        "DIDE Priority Pathogens EBOLA - REBECCA.accdb", "DIDE Priority Pathogens EBOLA - RUTH.accdb",
        "DIDE Priority Pathogens EBOLA - SABINE.accdb", "DIDE Priority Pathogens EBOLA - SEQUOIA.accdb"
      ),
      doubledb2 = c(
        "DIDE Priority Pathogens ETTIE.accdb", "DIDE Priority Pathogens REBECCA.accdb",
        "DIDE Priority Pathogens RICHARD.accdb", "DIDE Priority Pathogens SANGEETA.accdb",
        "DIDE Priority Pathogens TRISTAN.accdb"
      )
    ),
    LASSA = list(
      singledb = c(
        "DIDE Priority Pathogens LASSA - Christian.accdb",
        "DIDE Priority Pathogens LASSA - Dariya.accdb",
        "DIDE Priority Pathogens LASSA - Gina.accdb",
        "DIDE Priority Pathogens LASSA - Joseph.accdb",
        "DIDE Priority Pathogens LASSA - Kelly.accdb",
        "DIDE Priority Pathogens LASSA - Patrick.accdb",
        "DIDE Priority Pathogens LASSA - Richard.accdb",
        "DIDE Priority Pathogens LASSA - Ruth.accdb",
        "DIDE Priority Pathogens LASSA - Thom.accdb",
        "DIDE Priority Pathogens LASSA - Tristan.accdb"
      ),
      doubledb = c(
        "DIDE Priority Pathogens LASSA - Christian.accdb",
        "DIDE Priority Pathogens LASSA - Dariya.accdb",
        "DIDE Priority Pathogens LASSA - Gina.accdb",
        "DIDE Priority Pathogens LASSA - Joseph.accdb",
        "DIDE Priority Pathogens LASSA - Kelly.accdb",
        "DIDE Priority Pathogens LASSA - Patrick.accdb",
        "DIDE Priority Pathogens LASSA - Richard.accdb",
        "DIDE Priority Pathogens LASSA - Ruth.accdb",
        "DIDE Priority Pathogens LASSA - Thom.accdb",
        "DIDE Priority Pathogens LASSA - Tristan.accdb"
      ),
      doubledb2 = c()
    )
    ## Nipah Database files
  )

  ## Return files for pathogen
  infiles[[pathogen]]
}
