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
      ebolasingledb = c(
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
        "DIDE Priority Pathogens EBOLA - TRISTAN.accdb",
        "DIDE Priority Pathogens EBOLA - EXTRA CHRISTIAN.accdb",
        "DIDE Priority Pathogens EBOLA - EXTRA SANGEETA.accdb"
      ),
      eboladoubledb = c(
        "DIDE Priority Pathogens EBOLA - ANNE.accdb", "DIDE Priority Pathogens EBOLA - CHRISTIAN.accdb",
        "DIDE Priority Pathogens EBOLA - CYRIL.accdb", "DIDE Priority Pathogens EBOLA - DARIYA.accdb",
        "DIDE Priority Pathogens EBOLA - ETTIE.accdb", "DIDE Priority Pathogens EBOLA - GINA.accdb",
        "DIDE Priority Pathogens EBOLA - JACK.accdb", "DIDE Priority Pathogens EBOLA - JOSEPH.accdb",
        "DIDE Priority Pathogens EBOLA - KELLY.accdb", "DIDE Priority Pathogens EBOLA - PATRICK.accdb",
        "DIDE Priority Pathogens EBOLA - REBECCA.accdb", "DIDE Priority Pathogens EBOLA - RUTH.accdb",
        "DIDE Priority Pathogens EBOLA - SABINE.accdb", "DIDE Priority Pathogens EBOLA - SEQUOIA.accdb"
      ),
      eboladoubledb2 = c(
        "DIDE Priority Pathogens ETTIE.accdb", "DIDE Priority Pathogens REBECCA.accdb",
        "DIDE Priority Pathogens RICHARD.accdb", "DIDE Priority Pathogens SANGEETA.accdb",
        "DIDE Priority Pathogens TRISTAN.accdb"
      )
    ),
    LASSA = list(
      lassasingledb = c(
        "DIDE Priority Pathogens LASSA - Chaining.accdb",
        "DIDE Priority Pathogens LASSA - Christian.accdb",
        "DIDE Priority Pathogens LASSA - Dariya.accdb",
        "DIDE Priority Pathogens LASSA - Gina.accdb",
        "DIDE Priority Pathogens LASSA - Joseph.accdb",
        "DIDE Priority Pathogens LASSA - Kelly.accdb",
        "DIDE Priority Pathogens LASSA - Patrick.accdb",
        "DIDE Priority Pathogens LASSA - Revisions.accdb",
        "DIDE Priority Pathogens LASSA - Richard.accdb",
        "DIDE Priority Pathogens LASSA - Ruth.accdb",
        "DIDE Priority Pathogens LASSA - Thom.accdb",
        "DIDE Priority Pathogens LASSA - Tristan.accdb"
      ),
      lassadoubledb = c(
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
      )
    ),
    SARS = list(
      sarssingledb = c(
        "DIDE Priority Pathogens SARS - Anna Vicco.accdb",
        "DIDE Priority Pathogens SARS - Anna Vicco double.accdb",
        "DIDE Priority Pathogens SARS - Anne.accdb",
        "DIDE Priority Pathogens SARS - Bethan.accdb",
        "DIDE Priority Pathogens SARS - Christian.accdb",
        "DIDE Priority Pathogens SARS - Dominic.accdb",
        "DIDE Priority Pathogens SARS - Ettie.accdb",
        "DIDE Priority Pathogens SARS - Joseph.accdb",
        "DIDE Priority Pathogens SARS - Kanchan.accdb",
        "DIDE Priority Pathogens SARS - Kelly.accdb",
        "DIDE Priority Pathogens SARS - Kieran.accdb",
        "DIDE Priority Pathogens SARS - Patrick.accdb",
        "DIDE Priority Pathogens SARS - Paula_double.accdb",
        "DIDE Priority Pathogens SARS - Paula_single.accdb",
        "DIDE Priority Pathogens SARS - Rebecca.accdb",
        "DIDE Priority Pathogens SARS - Richard.accdb",
        "DIDE Priority Pathogens SARS - Rob.accdb",
        "DIDE Priority Pathogens SARS - Ruth.accdb",
        "DIDE Priority Pathogens SARS - Sangeeta.accdb",
        "DIDE Priority Pathogens SARS - Sequoia.accdb",
        "DIDE Priority Pathogens SARS - Thom.accdb",
        "DIDE Priority Pathogens SARS - Tristan.accdb"
      )
    ),    
    ZIKA = list(
      zikasingledb = c(
        "DIDE Priority Pathogens Zika 2 - Anna.accdb",
        "DIDE Priority Pathogens Zika 2 - Anna-Maria.accdb",
        "DIDE Priority Pathogens Zika 2 - Christian.accdb",
        # "DIDE Priority Pathogens Zika 2 - Gina.accdb",
        # "DIDE Priority Pathogens Zika 2 - Dom.accdb",
        # "DIDE Priority Pathogens Zika 2 - Ettie.accdb",
        # "DIDE Priority Pathogens Zika 2 - Ilaria.accdb",
        "DIDE Priority Pathogens Zika 2 - Joseph.accdb",
        "DIDE Priority Pathogens Zika 2 - Keith.accdb",
        "DIDE Priority Pathogens Zika 2 - Kelly.accdb",
        "DIDE Priority Pathogens Zika 2 - Patrick.accdb", 
        "DIDE Priority Pathogens Zika 2 - Richard.accdb",
        # "DIDE Priority Pathogens Zika 2 - Rob.accdb",
        "DIDE Priority Pathogens Zika 2 - Ruth.accdb",
        "DIDE Priority Pathogens Zika 2 - Sangeeta.accdb",
        # "DIDE Priority Pathogens Zika 2 - Sequoia.accdb",
        "DIDE Priority Pathogens Zika 2 - Shazia.accdb",
        "DIDE Priority Pathogens Zika 2 - Thom.accdb",
        "DIDE Priority Pathogens Zika 2 - Tristan.accdb" 
      ),
      # zikadoubledb = c(
      #   "DIDE Priority Pathogens Zika - Anna.accdb",
      #   "DIDE Priority Pathogens Zika - Anna-Maria.accdb",
      #   "DIDE Priority Pathogens Zika - Christian.accdb",
      #   "DIDE Priority Pathogens Zika - Dom.accdb",
      #   "DIDE Priority Pathogens Zika - Ettie.accdb",
      #   "DIDE Priority Pathogens Zika - Ilaria.accdb",
      #   "DIDE Priority Pathogens Zika - Keith.accdb",
      #   "DIDE Priority Pathogens Zika - Kelly.accdb",
      #   "DIDE Priority Pathogens Zika - Patrick.accdb",
      #   "DIDE Priority Pathogens Zika - Richard.accdb",
      #   "DIDE Priority Pathogens Zika - Rob.accdb",
      #   "DIDE Priority Pathogens Zika - Ruth.accdb",
      #   "DIDE Priority Pathogens Zika - Sangeeta.accdb",
      #   "DIDE Priority Pathogens Zika - Sequoia.accdb",
      #   "DIDE Priority Pathogens Zika - Shazia.accdb",
      #   "DIDE Priority Pathogens Zika - Thom.accdb",
      #   "DIDE Priority Pathogens Zika - Tristan.accdb"
      # ),
      zikadoubledb2 = c(
        "DIDE Priority Pathogens Zika 2 - Anna.accdb",
        "DIDE Priority Pathogens Zika 2 - Anna-Maria.accdb",
        "DIDE Priority Pathogens Zika 2 - Christian.accdb",
        "DIDE Priority Pathogens Zika 2 - Dom.accdb",
        # "DIDE Priority Pathogens Zika 2 - Ettie.accdb",
        # "DIDE Priority Pathogens Zika 2 - Ilaria.accdb",
        "DIDE Priority Pathogens Zika 2 - Keith.accdb",
        "DIDE Priority Pathogens Zika 2 - Kelly.accdb",
        "DIDE Priority Pathogens Zika 2 - Patrick.accdb",
        "DIDE Priority Pathogens Zika 2 - Richard.accdb",
        "DIDE Priority Pathogens Zika 2 - Rob.accdb",
        "DIDE Priority Pathogens Zika 2 - Ruth.accdb",
        "DIDE Priority Pathogens Zika 2 - Sangeeta.accdb",
        "DIDE Priority Pathogens Zika 2 - Sequoia.accdb",
        "DIDE Priority Pathogens Zika 2 - Shazia.accdb",
        "DIDE Priority Pathogens Zika 2 - Thom.accdb",
        "DIDE Priority Pathogens Zika 2 - Tristan.accdb"
      )
    )
    ## Nipah Database files
  )

  ## Return files for pathogen
  infiles[[pathogen]]
}



convert_to_utf <- function(vec, from = "windows-1252", to = "UTF-8") {
  if (is.character(vec)) {
    # If we can convert from and to the target then assume string is in target
    # encoding
    invalid <- is.na(iconv(vec, from = to, to = to))
    if (any(invalid)) {
      # Fix the problem by converting to the target :)
      vec[invalid] <- iconv(vec[invalid], from = from, to = to)
      # Not sure if this a good idea..., but letter following unrecognised
      # encoding is captial
      vec[invalid] <- str_to_title(vec[invalid])
    }
  }
  vec
}

