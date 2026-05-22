library(orderly)
# 
# remotes::install_github("mrc-ide/orderly.sharedfile")
# orderly::orderly_init()

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
#     (relative path: shared/redcap_data/rvf/mapping_table.csv)
#   - config.yaml that specifies how to run the task
#     (relative path: src/db_extraction_prep/redcap_task/rvf/config.yaml)
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
# debug_mode = TRUE keeps "article_id" and "name_data_entry" columns included in
# the cleaned output. Useful for debugging but shouldn't be included in the
# final version
orderly_run("db_cleaning",list(pathogen="RVF", debug_mode=TRUE))

# *------------------------------- Latex tables -------------------------------*
# Add cleaning mode
orderly_run("rvf_latex_tables", list(pathogen="RVF"))


# *---------------------------- Plots and analysis ----------------------------*
# Summary plots 
orderly_run("rvf_summary", list(pathogen="RVF"))

# Serology plots
orderly_run("rvf_serology", list(pathogen="RVF"))

# Transmission (and severity) plots
orderly_run("rvf_transmission", list(pathogen="RVF"))

# Delay plots 
orderly_run("rvf_delays", list(pathogen="RVF"))

# Risk factor plots
orderly_run("rvf_risk_factors", list(pathogen="RVF"))





