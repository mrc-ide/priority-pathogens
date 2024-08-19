## SARS WORKFLOW

#install.packages("orderly2", repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
#remotes::install_github("mrc-ide/orderly.sharedfile")
#orderly2::orderly_init(".")
library(orderly2)
library(optparse)
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
#if (file.exists(file.path(location, "sars-outputs.zip"))) {
#  stop("The file sars-outputs.zip already exists in the location. 
#  Please delete the file and run the script again.")
#}

## Download the zipped outputs from the database tasks
url <- "https://github.com/mrc-ide/priority-pathogens/releases/download/sars/sars-outputs.zip"
## Download
download.file(url, destfile = file.path(location, "sars-outputs.zip"), mode = "wb")
## unzip
## The outputs are zipped in a folder called sars-outputs
## Check that the folder does not already exist in the location
if (dir.exists(file.path(location, "sars-outputs"))) {
  stop("The folder sars-outputs already exists in the location. 
  Please delete the folder and run the script again.")
}
unzip(zipfile = file.path(location, "sars-outputs.zip"), exdir = location)
## Initialize orderly
orderly_init()

###############
## DATABASES ##
###############

## The first task db_extraction uses the Access databases that have not been 
## uploaded as part of the repository. 
## Outputs of the above tasks are made available as a zipped file from the 
## associated github release.
## The script will download these files, and add the downloaded folder 
## as an orderly location so that the outputs can be used by the tasks below.
## Create a random name for location in case user already has pathogen-outputs
## This will also help ensure that the script can be run multiple times without
## the need to remove location
loc_name <- adjective_animal()
 orderly_location_add(
   loc_name, type = "path", 
   args = list(path = file.path(location, "sars-outputs"))
 )
 orderly_location_pull_metadata(location = loc_name)
 orderly_location_pull_packet()
# 
# # Sort into double extracted matching and fixing
orderly_run("db_double", parameters = list(pathogen = "SARS"))
# 
# # Bring single, matching double, and fixed double together
orderly_run("db_compilation", parameters = list(pathogen = "SARS"))



##############
## ANALYSIS ##
##############

# map figure
orderly_run("sars_world_map", parameters = list(pathogen = "SARS"))

# Delay figures
orderly_run("sars_delays", parameters = list(pathogen = "SARS"))

# Transmission figures
orderly_run("sars_transmission", parameters = list(pathogen = "SARS"))

# Summary figures
orderly_run("sars_summary", parameters = list(pathogen = "SARS"))

# Latex tables
orderly_run("sars_latex_tables", parameters = list(pathogen = "SARS"))

# Risk factor plots
orderly_run("sars_risk_factors", parameters = list(pathogen = "SARS"))

######################
## FIGURES & TABLES ##
######################

# Collate figures and tables
orderly_run("sars_collate", parameters = list(pathogen = "SARS"))
