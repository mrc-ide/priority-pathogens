library(dplyr)
library(orderly2)
library(readr)
library(stringr)
library(tidyr)

pathogen <- "NIPAH"

orderly_dependency(
  "db_cleaning", "latest(parameter:pathogen == 'NIPAH')",
  c("articles.csv", "models.csv", "params.csv", "outbreaks.csv")
)

orderly_shared_resource("nipah_functions.R")
orderly_shared_resource("nipah_bangladesh_district_data.csv")
orderly_shared_resource("NIPAH_Bangladesh_IEDCR.csv")

source("nipah_functions.R")

## *=============== DEDUPLICATE OUTBREAKS =====================================*
articles <- read_csv("articles.csv")
outbreaks <- read_csv("outbreaks.csv")
models <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)
outbreaks <- dfs$outbreaks

outbreaks <- outbreaks %>% mutate(
  outbreak_location = coalesce(outbreak_location, outbreak_country),
  outbreak_end_year = coalesce(outbreak_end_year, outbreak_start_year),
  outbreak_duriation_years = outbreak_end_year - outbreak_start_year,
  type_cases_sex_disagg = replace_na(type_cases_sex_disagg, "Unspecified"),
  outbreak_notes = replace_na(outbreak_notes, ""),
  outbreak_location_type = str_to_title(replace_na(outbreak_location_type, "Unspecified"))
)
outbreaks$is_duplicate <- FALSE
outbreaks[outbreaks$outbreak_duriation_years > 5, ]$is_duplicate <- TRUE
outbreaks[outbreaks$outbreak_country == "Malaysia,Singapore", ]$is_duplicate <- TRUE
### we take the individual state outbreaks rather than the national aggregate one.
outbreaks[outbreaks$outbreak_country == "Malaysia" & outbreaks$outbreak_location_type != "State", ]$is_duplicate <- TRUE 
outbreaks[outbreaks$outbreak_country == "Singapore" & (is.na(outbreaks$deaths) | outbreaks$type_cases_sex_disagg == "Confirmed"), ]$is_duplicate <- TRUE # we take the report with confirmed cases, also not the sero study
outbreaks[str_detect(outbreaks$outbreak_notes, "cluster"), ]$is_duplicate <- TRUE # we take the individual outbreaks rather than the cluster

exclude_ids <- c(
  # India outbreaks to exclude
  "1150|India|Siliguri|2001|1",
  "292|India|Siliguri; West Bangal|2001|1",

  # Bangladesh outbreaks to exclude
  "1150|Bangladesh|Meherpur|2001|4",
  "851|Bangladesh|Meherpur|2001|4",
  "1150|Bangladesh|Naogaon|2003|1",
  "851|Bangladesh|Naogaon|2003|1",
  "192|Bangladesh|Rajbari|2004|1",
  "851|Bangladesh|Rajbari; 7 Other Northwestern Districts|2004|1",
  "851|Bangladesh|Faridpur|2004|2",
  "1150|Bangladesh|Faridpur|2004|4",
  "186|Bangladesh|Rajbari; Faridpur|2004|NA",
  "186|Bangladesh|Unspecified 7 Districts|2004|NA",
  "1150|Bangladesh|Tangail|2005|1",
  "960|Bangladesh|Haripur Upazila (Subdistrict) Of Thakurgaon District|2007|1",
  "976|Bangladesh|Sadar Upazila|2007|3",
  "1150|India|Nadia|2007|4",
  "1110|Bangladesh|Manikgonj|2008|2",
  "1110|Bangladesh|Rajbari|2008|2",
  "947|Bangladesh|Faridpur|2010|1",
  "956|Bangladesh|Lalmonirhat; Dinajpur; Rajbari; Rangpur|2010|12",
  "2709|Bangladesh|Chattogram Division|2011|NA",
  "2709|Bangladesh|Mymensingh Division|2013|NA",

  # Other outbreaks to exclude
  "2886|India|Kerala|2018|5",
  "2979|India|Kozhikode|2018|5",
  "4358|India|Kerela|2018|5"
)

outbreaks <- outbreaks %>%
  filter(!is_duplicate) %>%
  mutate(
    unique_id = paste(covidence_id, outbreak_country, outbreak_location,
      outbreak_start_year, str_remove(outbreak_start_month, "^0+"),
      sep = "|"
    ),
    EXCLUDE = ifelse(unique_id %in% exclude_ids, 1, 0)
  ) %>%
  filter(!EXCLUDE)

subcolumns_outbreak <- outbreaks %>%
  dplyr::select(outbreak_country, outbreak_location, outbreak_source,
                cases_confirmed, cases_suspected, cases_asymptomatic,
                cases_unspecified, outbreak_probable, deaths,
                outbreak_start_month, outbreak_start_year, refs) %>%
  mutate(cases_confirmed_raw = cases_confirmed) %>%
  mutate(cases_confirmed = coalesce(cases_confirmed, cases_suspected)) %>% # only use cases suspected if we don't have confirmed cases
  mutate(cases_confirmed = case_when(
    cases_confirmed < deaths ~ cases_confirmed + replace_na(cases_suspected, 0), # for Bangladesh we have some instances which require suspected cases to be added to confirmed to be feasible with deaths
    TRUE ~ cases_confirmed
  )) %>%
  mutate_if(is.numeric, list(~ replace_na(., 0))) %>%
  mutate(total_cases = cases_confirmed + cases_asymptomatic + cases_unspecified + outbreak_probable) %>%
  arrange(outbreak_start_year, outbreak_start_month) %>%
  mutate(num_loc = str_count(outbreak_location, ";") + 1) %>%
  separate_rows(outbreak_location, sep = ";") %>%
  mutate(
    outbreak_location = str_trim(outbreak_location),
    total_cases = total_cases / num_loc,
    deaths = deaths / num_loc
  ) # for location where we split report the AVERAGE number of cases

saveRDS(subcolumns_outbreak, "cleaned_outbreak_data.RDS")
orderly_artefact(files = "cleaned_outbreak_data.RDS")
