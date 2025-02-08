library(orderly2)

# load data

# call Redcap API
#setwd("src/db_redcap_download")
orderly_run("db_redcap_download",list(pathogen="OROV"))
# must have put API token from Redcap in environment for this to work:
# usethis::edit_r_environ() - run this and then restart R to make work

# run extraction prep
#setwd(src/db_extraction_prep)
orderly_run("db_extraction_prep",list(pathogen="OROV"))
# then you read the progress report and iterate until things are looking the way that we want them to
### had to comment a line out of this to make it work in generate_report file
## cannot get anything from changing mapping files in shared
## need to revisit this but move on for now

# run db_double
orderly_run("db_double",list(pathogen="OROV"))

# run db_compilation
orderly_run("db_compilation",list(pathogen="OROV"))



