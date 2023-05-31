source('R/data_cleaning.R')

# create folder for pathogens upfront so that we can save data to them
create_data_subfolders <- function(pathogen)
{
  ## Ensure that output directories exist
  dir.create(paste0("data/", pathogen,"/final/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0("data/", pathogen,"/fixed/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0("data/", pathogen,"/fixing/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0("data/", pathogen,"/output/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0("data/", pathogen,"/raw/"), showWarnings = FALSE, recursive = TRUE)
}

create_R_subfolders <- function(pathogen)
{
  ## Ensure that output directories exist
  dir.create(paste0("R/", pathogen), showWarnings = FALSE, recursive = TRUE)
}