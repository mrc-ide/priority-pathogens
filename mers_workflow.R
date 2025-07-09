library(orderly2)

# *--------------------------- Download REDCap data ---------------------------*
# Downloads the data report from REDCap
# Requires:
#   - A REDCap api
#   - mers_config.yaml.that specifies how to run the task
#     (relative path: src/db_redcap_download/download_config/mers_config.yaml)
orderly_run("db_redcap_download",list(pathogen="MERS"))

