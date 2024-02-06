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
orderly_run("ebola_risk_factors", parameters = list(pathogen = "EBOLA"))
# TO DO FOR RISK FACTORS:
# combine some tables into multi-panel for supplements

# Mutations
orderly_run("ebola_mutations", parameters = list(pathogen = "EBOLA"))

# Attack rate
orderly_run("ebola_attack_rate", parameters = list(pathogen = "EBOLA"))
# TO DO FOR ATTACK RATE: Add attack_rate_type for last 2 papers

# Overdispersion
orderly_run("ebola_overdispersion", parameters = list(pathogen = "EBOLA"))

# Growth rate
orderly_run("ebola_growth_rate", parameters = list(pathogen = "EBOLA"))

# Doubling time
orderly_run("ebola_doubling_time", parameters = list(pathogen = "EBOLA"))

# Figures for main manuscript
orderly_run("ebola_figures", parameters = list(pathogen = "EBOLA"))

# Figures for supplementary material



