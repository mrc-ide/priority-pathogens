# *============================================================================*
library(httr)
library(orderly2)
library(yaml)

# *------------------------------ Orderly config ------------------------------*
orderly_parameters(pathogen = NULL)

config_filepath <- file.path("download_config",
                             paste0(tolower(pathogen), "_config.yaml"))

orderly_resource(config_filepath)

# *---------------------------- REDCap Parameters -----------------------------*
config_list <- yaml.load_file(config_filepath)
report_id_list <- config_list[["report_ids"]]
toked_name <- config_list[["api_token_env_var"]]

token <- Sys.getenv(toked_name)

endpoint_url <- "https://redcap.imperial.ac.uk/api/"

# *----------------------------- Helper functions -----------------------------*
create_record_list <- function(token, record_id){
  record_post_body_list <- list("token"=token,
                                content="report",
                                format="csv",
                                report_id=report_id,
                                csvDelimiter=",",
                                rawOrLabel="label",
                                rawOrLabelHeaders="raw",
                                exportCheckboxLabel="false",
                                returnFormat="csv")
}

# *--------------------------- Downloading the data ---------------------------*
for (report_id in names(report_id_list)){
  post_body_list <- create_record_list(token, report_id)

  response <- POST(endpoint_url, body = post_body_list, encode = "form")
  result_df <- content(response)

  write.csv(result_df, file=report_id_list[[report_id]], row.names=FALSE)
}

orderly_artefact(description="csv files downloaded from redcap reports",
                 unlist(report_id_list))
# *============================================================================*

