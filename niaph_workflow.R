library(orderly2)

# *--------------------------- Download REDCap data ---------------------------*
# Downloads the data report from REDCap
# Requires:
#   - A REDCap api
#   - nipah_config.yaml.that specifies how to run the task
#     (relative path: src/db_redcap_download/download_config/nipah_config.yaml)
orderly_run("db_redcap_download",list(pathogen="NIPAH"))

# *----------------- Prepare data to generate extraction csvs -----------------*
# Prepares the REDCap data so that double and single extraction csvs can be
# created in db_extraction
# Requires:
#   - target_table.csv (relative path: shared/redcap_data/target_table.csv)
#   - mapping_table.csv
#     (relative path: shared/redcap_data/nipah/mapping_table.csv)
#   - config.yaml that specifies how to run the task
#     (relative path: src/db_extraction_prep/redcap_task/nipah/config.yaml)
orderly_run("db_extraction_prep",list(pathogen="NIPAH",
                                      orderly_download_dependency=TRUE))

# *------------------------- Generate extraction csvs -------------------------*
# Extracts double and single extraction csvs used the .rds file from
# db_extraction_prep
orderly_run("db_extraction",list(pathogen="NIPAH"))

# *-------------------------- Generate fixing files ---------------------------*
# Check if any double extractions need to be fixed and generate necessary files
orderly_run("db_double",list(pathogen="NIPAH"))

# *-------------------------- Generate final dataset --------------------------*
# Merge double_extraction, fixing, and single extraction files to create a final
# dataset
orderly_run("nipah_compilation",list(pathogen="NIPAH"))

# *-------------------------------- Clean data --------------------------------*
orderly_run("nipah_cleaning",list(pathogen="NIPAH", debug_mode=TRUE))
