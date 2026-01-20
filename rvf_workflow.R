library(orderly)

#remotes::install_github("mrc-ide/orderly.sharedfile")

# *--------------------------- Download REDCap data ---------------------------*
# Downloads the data report from REDCap
# Requires:
#   - A REDCap api
#       --> usethis::edit_r_environ() to set this up,
#       --> Add "REDCAP_TOKEN=[your API token]" to the file that opens
#       --> Restart R for the environment variable to be available
#   - rvf_config.yaml.that specifies how to run the task
#     (relative path: src/db_redcap_download/download_config/rvf_config.yaml)
orderly_run("db_redcap_download",list(pathogen="RVF"))

# *----------------- Prepare data to generate extraction csvs -----------------*
# Prepares the REDCap data so that double and single extraction csvs can be
# created in db_extraction
# Requires:
#   - target_table.csv (relative path: shared/redcap_data/target_table.csv)
#   - mapping_table.csv
#     (relative path: shared/redcap_data/nipah/mapping_table.csv)
#   - config.yaml that specifies how to run the task
#     (relative path: src/db_extraction_prep/redcap_task/nipah/config.yaml)
orderly_run("db_extraction_prep",list(pathogen="RVF",
                                      orderly_download_dependency=TRUE))

# *------------------------- Generate extraction csvs -------------------------*
# Extracts double and single extraction csvs used the .rds file from
# db_extraction_prep
orderly_run("db_extraction",list(pathogen="RVF"))

# *-------------------------- Generate fixing files ---------------------------*
# Check if any double extractions need to be fixed and generate necessary files
orderly_run("db_double",list(pathogen="RVF"))

# *-------------------------- Generate final dataset --------------------------*
# Merge double_extraction, fixing, and single extraction files to create a final
# dataset
orderly_run("db_compilation", list(pathogen="RVF"))

# *-------------------------------- Clean data --------------------------------*
orderly_run("db_cleaning",list(pathogen="RVF", debug_mode=TRUE))
