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
orderly_run("ebola_delays", parameters = list(pathogen = "EBOLA"))

# Seroprevalence (Note: not finished - this is rough and more cleaning needed)
orderly_run("ebola_seroprevalence", parameters = list(pathogen = "EBOLA"))

# Risk factors

# Mutations

# Attack rate

# Overdispersion

# Growth rate

# Doubling time

# Figures for main manuscript
orderly_run("ebola_figures", parameters = list(pathogen = "EBOLA"))

# Figures for supplementary material



