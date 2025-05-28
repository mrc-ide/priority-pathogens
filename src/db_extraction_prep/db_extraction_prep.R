library(orderly2)

orderly_parameters(pathogen = NULL,
                   mode="",
                   orderly_download_dependency=FALSE)

pathogen <- toupper(pathogen)
mode <- toupper(mode)

orderly_artefact(description="rds file with the processed tables",
                 "extracted_tables.rds")

# Mode serves to overwrite the default pathogen method of extraction
# E.g. if new EBOLA articles are extracted with redcap, mode can be used to
# overwrite the default extraction method. For each pathogen, the necessary data
# files and config files are available for the default extraction method.
# If you use a different method, you need to provide the necessary files.
if (pathogen %in% c("MARBURG", "EBOLA", "SARS", "ZIKA") & mode!="REDCAP"|
    mode=="ACCESS"){
  source("prepare_access.R")
} else if (pathogen %in% c("OROV", "NIPAH", "ONBOARDING") & mode!="ACCESS"|
           mode=="REDCAP"){
  source("prepare_redcap.R")
}
