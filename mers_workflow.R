library(orderly2)

# *--------------------------- Download REDCap data ---------------------------*
# Downloads the data report from REDCap
# Requires:
#   - A REDCap api
#   - mers_config.yaml.that specifies how to run the task
#     (relative path: src/db_redcap_download/download_config/mers_config.yaml)
orderly_run("db_redcap_download",list(pathogen="MERS"))

# *----------------- Prepare data to generate extraction csvs -----------------*
# Prepares the REDCap data so that double and single extraction csvs can be
# created in db_extraction
# Requires:
#   - target_table.csv (relative path: shared/redcap_data/target_table.csv)
#   - mapping_table.csv
#     (relative path: shared/redcap_data/mers/mapping_table.csv)
#   - config.yaml that specifies how to run the task
#     (relative path: src/db_extraction_prep/redcap_task/mers/config.yaml)
orderly_run("db_extraction_prep", list(pathogen="MERS",
                                       orderly_download_dependency=TRUE))

# *------------------------- Generate extraction csvs -------------------------*
# Extracts double and single extraction csvs used the .rds file from
# db_extraction_prep
orderly_run("db_extraction",list(pathogen="MERS"))

# *-------------------------- Generate fixing files ---------------------------*
# Check if any double extractions need to be fixed and generate necessary files
orderly_run("db_double",list(pathogen="MERS"))

# *-------------------------- Generate final dataset --------------------------*
# Merge double_extraction, fixing, and single extraction files to create a final
# dataset
orderly_run("db_compilation", list(pathogen="MERS"))

# *-------------------------------- Clean data --------------------------------*
# debug_mode = TRUE keeps "article_id" and "name_data_entry" columns included in
# the cleaned output. Useful for debugging but shouldn't be included in the
# final version
orderly_run("db_cleaning",list(pathogen="MERS", debug_mode=TRUE))

# *------------------------------- Latex tables -------------------------------*
# Add cleaning mode
orderly_run("mers_latex_tables", list(pathogen="MERS"))

# *---------------------------- Plots and analysis ----------------------------*
orderly_run("mers_transmission", list(pathogen="MERS"))
