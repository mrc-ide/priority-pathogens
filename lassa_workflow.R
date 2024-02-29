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

###################################################
## FIGURES & TABLES FOR MANUSCRIPT & SUPPLEMENTS ##
###################################################

# Figures for main manuscript and SI
orderly_run("lassa_figures", parameters = list(pathogen = "LASSA"))