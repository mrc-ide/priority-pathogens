## EBOLA WORKFLOW
library(optparse)
library(orderly2)
library(ids)
library(zip)
option_list <- list(
  make_option(c("-l", "--location"), type="character", default = NULL,
              help = "Download location for the zipped outputs from the database 
              tasks. You must specify a fully specified location on your machine.")
)

parser <- OptionParser(option_list = option_list)

args <- parse_args(parser)

location <- args$location

## Retrieve the full path from the location
location <- normalizePath(location)

## Check if the file already exists in the location, and if it does, stop and
## ask the user to delete the file before running the script
if (file.exists(file.path(location, "ebola-outputs.zip"))) {
  stop("The file ebola-outputs.zip already exists in the location. 
  Please delete the file and run the script again.")
}

## Download the zipped outputs from the database tasks
url <- "https://github.com/mrc-ide/priority-pathogens/releases/download/test/ebola-outputs-test-location.zip"
## Download
download.file(url, destfile = file.path(location, "ebola-outputs.zip"), mode = "wb")
## unzip
## The outputs are zipped in a folder called ebola-outputs-test-location
## Check that the folder does not already exist in the location
if (dir.exists(file.path(location, "ebola-outputs-test-location"))) {
  stop("The folder ebola-outputs-test-location already exists in the location. 
  Please delete the folder and run the script again.")
}
unzip(zipfile = file.path(location, "ebola-outputs.zip"), exdir = location)
## Initialize orderly
orderly_init()

###############
## DATABASES ##
###############

# Extract databases from shared drive (ensure config file pathway matches your machine)
## orderly_run("db_extraction", parameters = list(pathogen = "EBOLA"))

# Sort into double extracted matching and fixing
## orderly_run("db_double", parameters = list(pathogen = "EBOLA"))

# Bring single, matching double, and fixed double together
## orderly_run("db_compilation", parameters = list(pathogen = "EBOLA"))

## The above tasks use the Access databases that have not been uploaded as part 
## of the repository. 
## Outputs of the above tasks are made available as a zipped file from the 
## associated github release.
## The script will download these files, and add the downloaded folder 
## as an orderly location so that the outputs can be used by the tasks below.
## Create a random name for location in case user already has ebola-outputs
## This will also help ensure that the script can be run multiple times without
## the need to remove location
loc_name <- adjective_animal()
orderly_location_add(
    loc_name, type = "path", 
    args = list(path = file.path(location, "ebola-outputs-test-location"))
)
orderly_location_pull_metadata(location = loc_name)
orderly_location_pull_packet()
##############
## ANALYSIS ##
##############

# Reproduction number
orderly_run("ebola_reproduction_number", parameters = list(pathogen = "EBOLA"))

# Severity (CFR/IFR)
orderly_run("ebola_severity", parameters = list(pathogen = "EBOLA"))

# Human Delays
orderly_run("ebola_delays", parameters = list(pathogen = "EBOLA"))

# Seroprevalence
orderly_run("ebola_seroprevalence", parameters = list(pathogen = "EBOLA"))

# Risk factors
orderly_run("ebola_risk_factors", parameters = list(pathogen = "EBOLA"))

# Mutations
orderly_run("ebola_mutations", parameters = list(pathogen = "EBOLA"))

# Attack rate
orderly_run("ebola_attack_rate", parameters = list(pathogen = "EBOLA"))

# Overdispersion
orderly_run("ebola_overdispersion", parameters = list(pathogen = "EBOLA"))

# Growth rate
orderly_run("ebola_growth_rate", parameters = list(pathogen = "EBOLA"))

# Doubling time
orderly_run("ebola_doubling_time", parameters = list(pathogen = "EBOLA"))

# Models
orderly_run("ebola_models", parameters = list(pathogen = "EBOLA"))

# Summary: list of parameters and QA scores
orderly_run("ebola_summary", parameters = list(pathogen = "EBOLA"))

###################################################
## FIGURES & TABLES FOR MANUSCRIPT & SUPPLEMENTS ##
###################################################

# Figures and tables for main manuscript
orderly_run("ebola_figures", parameters = list(pathogen = "EBOLA"))

# Figures and tables for supplementary material
orderly_run("ebola_supplementary_figures", parameters = list(pathogen = "EBOLA"))

