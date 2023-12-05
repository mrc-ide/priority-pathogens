## EBOLA WORKFLOW

library(orderly2)

###############
## DATABASES ##
###############

# Extract databases from shared drive (ensure config file pathway matches your machine)
orderly_run("db_extraction", parameters = list(pathogen = "EBOLA"))

# Sort into double extracted matching and fixing
orderly_run("db_double", parameters = list(pathogen = "EBOLA"))

# Bring single, matching double, and fixed double together
orderly_run("db_compilation", parameters = list(pathogen = "EBOLA"))

##############
## ANALYSIS ##
##############

# Reproduction number
orderly_run("ebola_reproduction_number", parameters = list(pathogen = "EBOLA"))

# Severity (CFR/IFR)
orderly_run("ebola_severity", parameters = list(pathogen = "EBOLA"))

# Human Delays
# To do: look into dealing with inverse parameters

# Risk factors

# Seroprevalence

# Mutations

# Attack rate

# Overdispersion

# Growth rate

# Doubling time





