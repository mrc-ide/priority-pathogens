## ZIKA WORKFLOW
# based on SARS workflow

# install.packages("orderly2", repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
# remotes::install_github("mrc-ide/orderly.sharedfile")
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
# 
# ## Retrieve the full path from the location
location <- normalizePath(location)

## Check if the file already exists in the location, and if it does, stop and
## ask the user to delete the file before running the script
#if (file.exists(file.path(location, "sars-outputs.zip"))) {
#  stop("The file sars-outputs.zip already exists in the location. 
#  Please delete the file and run the script again.")
#}

## Download the zipped outputs from the database tasks
url <- "https://github.com/mrc-ide/priority-pathogens/releases/download/zika/zika-outputs.zip"
## Download
download.file(url, destfile = file.path(location, "zika-outputs.zip"), mode = "wb")
## unzip
## The outputs are zipped in a folder called lassa-outputs
## Check that the folder does not already exist in the location
if (dir.exists(file.path(location, "zika-outputs"))) {
  stop("The folder lassa-outputs-test-location already exists in the location. 
  Please delete the folder and run the script again.")
}
unzip(zipfile = file.path(location, "zika-outputs.zip"), exdir = location)


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
  loc_name,
  type = "path",
  args = list(path = file.path(location, "zika-outputs"))
)
orderly_location_fetch_metadata(location = loc_name)
orderly_location_pull(NULL, location = loc_name)

## Initialize orderly
orderly_init()


###############
## DATABASES ##
###############

# Extraction prep - access versus redcap
# orderly_run("db_extraction_prep", parameter = list(pathogen = 'ZIKA'))

# # Extract databases from shared drive (ensure config file pathway matches your machine)
orderly_run("db_extraction", parameters = list(pathogen = "ZIKA"))

# Sort into double extracted matching and fixing
orderly_run("db_double", parameters = list(pathogen = "ZIKA"))

# Bring single, matching double, and fixed double together
orderly_run("zika_compilation", parameters = list(pathogen = "ZIKA"))
# orderly_run("db_compilation", parameters = list(pathogen = "ZIKA"))


##############
## ANALYSIS, FIGURES, AND TABLES ##
##############

# Data curation for analysis 
orderly2::orderly_run('zika_prep_data', parameters = list(pathogen = 'ZIKA',
                                                          plotting = TRUE))
orderly2::orderly_run('zika_prep_data', parameters = list(pathogen = 'ZIKA',
                                                          plotting = FALSE))

# Delay figures
orderly_run("zika_delays", parameters = list(pathogen = "ZIKA"))

# Risk factors 
orderly_run("zika_risk_factors", parameters = list(pathogen = 'ZIKA'))

# Reproduction numbers
orderly_run("zika_reproduction_numbers", parameters = list(pathogen = "ZIKA"))

# Genomic data 
orderly_run('zika_genomic', parameters = list(pathogen = 'ZIKA'))
 
# Miscarriage/microcephaly 
orderly_run('zika_zcs_microcephaly', parameters = list(pathogen = 'ZIKA'))

# Other pars: H-to_H contribution to transmission rate, CFR, prop of sympt, overdisp, growth rate 
orderly_run('zika_other_pars', parameters = list(pathogen = 'ZIKA'))

#Seroprevalence plots and maps
orderly_run('zika_serop', parameters = list(pathogen = 'ZIKA'))

# # Summary figures
# orderly_run("sars_summary", parameters = list(pathogen = "SARS"))
# 
# # Latex tables
orderly_run("zika_latex_tables", parameters = list(pathogen = "ZIKA"))

# Seroprevalence
# orderly_run('zika_serop', parameters = list(pathogen = 'ZIKA'))

# summary figures 
orderly_run('zika_summary', parameters = list(pathogen = 'ZIKA'))

# Outbreaks
orderly_run('zika_outbreaks', parameters = list(pathogen = 'ZIKA'))

# Collate figures and tables
# orderly_run("zika_collate_plots", parameters = list(pathogen = "LASSA"))

## Clean up user environment
orderly_location_remove(name = loc_name)
