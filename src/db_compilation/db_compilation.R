library(orderly2)

orderly_strict_mode()

orderly_parameters(pathogen = NULL,
                   mode="")

pathogen <- toupper(pathogen)
mode <- toupper(mode)

# Mode serves to overwrite the default pathogen method of extraction
# E.g. if new EBOLA articles are extracted with redcap, mode can be used to
# overwrite the default extraction method. For each pathogen, the necessary data
# files and config files are available for the default extraction method.
# If you use a different method, you need to provide the necessary files.
if (pathogen %in% c("MARBURG", "EBOLA", "SARS") & mode!="REDCAP"|
    mode=="ACCESS"){
  orderly_resource("access_compilation.R")
  source(file.path("access_compilation.R"))
} else if (pathogen %in% c("OROV", "NIPAH") & mode!="ACCESS"|
           mode=="REDCAP"){
  orderly_resource("redcap_compilation.R")
  source("redcap_compilation.R")
}
