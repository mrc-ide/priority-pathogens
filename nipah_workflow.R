library(orderly2)

# *--------------------------- Download REDCap data ---------------------------*
# Downloads the data report from REDCap
# Requires:
#   - A REDCap api
#   - nipah_config.yaml.that specifies how to run the task
#     (relative path: src/db_redcap_download/download_config/nipah_config.yaml)

# NOTE: IF YOU ARE A PERG MEMBER WITH A REDCAP API KEY PLEASE UNCOMMENT THE
#       LINE BELOW AND SET 'orderly_download_dependency=TRUE' IN LINE 24
# orderly_run("db_redcap_download",list(pathogen="NIPAH"))

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
                                      orderly_download_dependency=FALSE))

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
orderly_run("db_compilation", list(pathogen="NIPAH"))

# *-------------------------------- Clean data --------------------------------*
orderly_run("db_cleaning",list(pathogen="NIPAH", debug_mode=TRUE))

# *------------------------------- Latex tables -------------------------------*
# Add cleaning mode
orderly_run("nipah_latex_tables", list(pathogen="NIPAH"))

# *---------------------------- Plots and analysis ----------------------------*
# Serology
orderly_run("nipah_serology")

# Maps
orderly_run("nipah_deduplicate_outbreaks")
orderly_run("nipah_map_prep")
orderly_run("nipah_map_alternate")

orderly_run("nipah_iedcr_map_prep")
orderly_run("nipah_map_alternate_iedcr")

# Severity
orderly_run("nipah_severity_extracted_params", list(pathogen="NIPAH"))
orderly_run("nipah_severity_extracted_outbreaks", list(pathogen="NIPAH"))
orderly_run("nipah_severity_IEDCR", list(pathogen="NIPAH"))

# Transmission
orderly_run("nipah_transmission", list(pathogen="NIPAH"))

# Delays
orderly_run("nipah_inc_period_meta")
orderly_run("nipah_delays")

# Risk factors
orderly_run("nipah_risk_factors", list(pathogen="NIPAH"))

# SI summary plots
orderly_run("nipah_summary", list(pathogen="NIPAH"))

# SI summary tables
orderly_run("nipah_supp_tables", list(pathogen="NIPAH"))

