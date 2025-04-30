library(orderly2)

# tidy up the junk that is already in there

orderly_cleanup("db_redcap_download")
orderly_cleanup("db_extraction_prep")
orderly_cleanup("db_extraction")
orderly_cleanup("db_double")
orderly_cleanup("db_compilation")
## don't run the below because it gets rids of things you need 
# orderly_cleanup("orov_summary")
# #orderly_cleanup("orov_attack_rates")
# orderly_cleanup("orov_delays")
# orderly_cleanup("orov_evidence_of_infection")
# #orderly_cleanup("orov_risk_factors")

# workflow
# sorting the data

# call Redcap API
orderly_run("db_redcap_download",list(pathogen="OROV"))
# must have put API token from Redcap in environment for this to work:
# usethis::edit_r_environ() - run this and then restart R to make work
# 9/2: this seems to have all the data we want (rough eyeball/n_obs)

# run extraction prep
#setwd(src/db_extraction_prep)
orderly_run("db_extraction_prep",list(pathogen="OROV"))
# then you read the progress report and iterate until things are looking the way that we want them to
### had to comment a line out of this to make it work in generate_report file
## cannot get anything from changing mapping files in shared
## need to revisit this but move on for now
# 9/2 - Shazia and Patrick's extractions not coming through - but worked through interactively and looked fine (this was me being stupid)

# run db_extraction
orderly_run("db_extraction",list(pathogen="OROV"))
# oops didn't run this before and that is why it wasn't happy
# this runs fine 

# run db_double
orderly_run("db_double",list(pathogen="OROV"))
## then once you have run this you fix the fixing sheet

# run db_compilation
orderly_run("db_compilation",list(pathogen="OROV"))
### should be able to rerun multiple times with the same fixing files 




