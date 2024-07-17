## LASSA WORKFLOW

#install.packages("orderly2", repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
#remotes::install_github("mrc-ide/orderly.sharedfile")
#orderly2::orderly_init()
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
if (file.exists(file.path(location, "lassa-outputs.zip"))) {
  stop("The file lassa-outputs.zip already exists in the location. 
  Please delete the file and run the script again.")
}

## Download the zipped outputs from the database tasks
url <- "https://github.com/mrc-ide/priority-pathogens/releases/download/test2/lassa-outputs.zip"
## Download
download.file(url, destfile = file.path(location, "lassa-outputs.zip"), mode = "wb")
## unzip
## The outputs are zipped in a folder called lassa-outputs
## Check that the folder does not already exist in the location
if (dir.exists(file.path(location, "lassa-outputs"))) {
  stop("The folder lassa-outputs-test-location already exists in the location. 
  Please delete the folder and run the script again.")
}
unzip(zipfile = file.path(location, "lassa-outputs.zip"), exdir = location)
## Initialize orderly
orderly_init()

###############
## DATABASES ##
###############

# # Extract databases from shared drive (ensure config file pathway matches your machine)
# orderly_run("db_extraction", parameters = list(pathogen = "LASSA"))
# 
# # Sort into double extracted matching and fixing
# orderly_run("db_double", parameters = list(pathogen = "LASSA"))
# 
# # Bring single, matching double, and fixed double together
# orderly_run("db_compilation", parameters = list(pathogen = "LASSA"))

## The above tasks use the Access databases that have not been uploaded as part 
## of the repository. 
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
  args = list(path = file.path(location, "lassa-outputs"))
)
orderly_location_pull_metadata(location = loc_name)
orderly_location_pull_packet()

##############
## ANALYSIS ##
##############

# Seroprevalence figures
orderly_run("lassa_serology", parameters = list(pathogen = "LASSA"))

# Severity figures
orderly_run("lassa_severity", parameters = list(pathogen = "LASSA"))

# Delay figures
orderly_run("lassa_delays", parameters = list(pathogen = "LASSA"))

# Transmission figures
orderly_run("lassa_transmission", parameters = list(pathogen = "LASSA"))

# Summary figures
orderly_run("lassa_summary", parameters = list(pathogen = "LASSA"))

# Latex tables
orderly_run("lassa_latex_tables", parameters = list(pathogen = "LASSA"))

######################
## FIGURES & TABLES ##
######################

# Collate figures and tables
orderly_run("lassa_collate", parameters = list(pathogen = "LASSA"))