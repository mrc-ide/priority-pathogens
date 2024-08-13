library(orderly2)

orderly_init()
orderly_location_add(
    "packit.dide",
    type = "packit", 
    args = list(
        url = "https://packit.dide.ic.ac.uk/priority-pathogens",
        token = Sys.getenv("GITHUB_TOKEN"))
)

orderly_location_pull_metadata("packit.dide")
orderly_location_pull_packet("latest(name == 'db_extraction' && parameter:pathogen == 'LASSA')",
                             options = list(location="packit.dide"))

ids <- c(
    orderly_run("db_double", parameters = list(pathogen = "LASSA")),
    orderly_run("db_compilation", parameters = list(pathogen = "LASSA")),
    orderly_run("lassa_serology", parameters = list(pathogen = "LASSA")),
    orderly_run("lassa_severity", parameters = list(pathogen = "LASSA")),
    orderly_run("lassa_delays", parameters = list(pathogen = "LASSA")),
    orderly_run("lassa_transmission", parameters = list(pathogen = "LASSA")),
    orderly_run("lassa_summary", parameters = list(pathogen = "LASSA")),
    orderly_run("lassa_latex_tables", parameters = list(pathogen = "LASSA")),
    orderly_run("lassa_collate", parameters = list(pathogen = "LASSA")))

orderly_location_push(ids, "packit.dide")
