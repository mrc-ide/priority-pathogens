## LASSA WORKFLOW

#install.packages("orderly2", repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
#remotes::install_github("mrc-ide/orderly.sharedfile")
#orderly2::orderly_init(".")
library(orderly2)

###############
## DATABASES ##
###############

# Extract databases from shared drive (ensure config file pathway matches your machine)
orderly_run("db_extraction", parameters = list(pathogen = "LASSA"))

# Sort into double extracted matching and fixing
orderly_run("db_double", parameters = list(pathogen = "LASSA"))

# Bring single, matching double, and fixed double together
orderly_run("db_compilation", parameters = list(pathogen = "LASSA"))

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