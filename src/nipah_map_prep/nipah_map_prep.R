library(orderly2)
library(readr)
library(tidyverse)
library(sf)
library(stringr)



pathogen <- "NIPAH"

orderly_dependency(
  "db_cleaning", "latest(parameter:pathogen == 'NIPAH')",
  c("articles.csv", "models.csv", "params.csv", "outbreaks.csv")
)

orderly_dependency(
  "nipah_deduplicate_outbreaks", "latest", "cleaned_outbreak_data.RDS"
)

orderly_shared_resource("nipah_functions.R")
orderly_shared_resource("nipah_bangladesh_district_data.csv")
orderly_shared_resource("NIPAH_Bangladesh_IEDCR.csv" )

source("nipah_functions.R")

###################
## DATA CURATION ##
###################

articles <- read_csv("articles.csv")
outbreaks <- read_csv("outbreaks.csv")
models <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles <- dfs$articles

articles <- epireview::assign_qa_score(articles = articles)$articles
qa_scores <- articles %>% dplyr::select(covidence_id, qa_score)

outbreaks <- dfs$outbreaks
models <- dfs$models
parameters <- dfs$parameters %>%
  left_join(qa_scores) %>%
  mutate(article_label = make.unique(refs)) %>%
  mutate(article_label = factor(article_label, levels = rev(unique(article_label)))) %>%
  mutate(
    in_CSF = case_when(
      str_detect(parameter_notes, "CSF") ~ TRUE,
      TRUE ~ FALSE
    ),
    population_location = coalesce(population_location, population_country)
  )

nipah_bangladesh <- read_csv("nipah_bangladesh_district_data.csv")


# Extract and clean parameter locations
parameter_locs <- parameters$population_location |>
  lapply(FUN = function(x) {
    x |>
      str_split(";") |>
      unlist()
  }) |>
  unlist() |>
  na.omit() |>
  str_trim() |>
  unique() |>
  str_to_title()

# Extract and clean outbreak locations
outbreak_locs <- outbreaks$outbreak_location |>
  lapply(FUN = function(x) {
    x |>
      str_split(";") |>
      unlist()
  }) |>
  unlist() |>
  na.omit() |>
  str_trim() |>
  unique() |>
  str_to_title()

# Combine all unique locations
locations <- unique(c(parameter_locs, outbreak_locs))




location_mapping <- tribble(
  ~location, ~iso3, ~district, ~division_or_state, ~notes,
  # ---- Bangladesh ----
  "Haripur Upazila", "BGD", "Thakurgaon", "Rangpur", "Subdistrict of Thakurgaon",
  "Haripur Upazila (Subdistrict) Of Thakurgaon District", "BGD", "Thakurgaon", "Rangpur", "Full location specification",
  "Sadar Upazila", "BGD", NA, NA, "Requires specific district context",
  "Northwest Bangladesh", "BGD", NA, "Rangpur; Rajshahi", "Regional designation",
  "Manikgonj", "BGD", "Manikganj", "Dhaka", NA,
  "Rangpur District", "BGD", "Rangpur", "Rangpur", NA,
  "Rajshahi District", "BGD", "Rajshahi", "Rajshahi", NA,
  "Lalmohirhat", "BGD", "Lalmonirhat", "Rangpur", "Common misspelling of Lalmonirhat",
  "Comilla", "BGD", "Cumilla", "Chittagong", "Alternate spelling",
  "Joypurhat", "BGD", "Joypurhat", "Rajshahi", NA,
  "Bogra", "BGD", "Bogra", "Rajshahi", NA,
  "Jessore", "BGD", "Jashore", "Khulna", "Official spelling update",
  "Goalando", "BGD", "Rajbari", "Dhaka", "Subdistrict of Rajbari",
  "7 Other Northwestern Districts", "BGD", NA, "Rangpur; Rajshahi", "Regional grouping",
  "Unspecified 7 Districts", "BGD", NA, NA, "Northwest region unspecified",
  "Barishal Division", "BGD", NA, "Barishal", "Division-level entry",
  "Rajshahi Division", "BGD", NA, "Rajshahi", "Division-level entry",
  "Chattogram Division", "BGD", NA, "Chittagong", "Division-level entry",
  "Rangpur Division", "BGD", NA, "Rangpur", "Division-level entry",
  "Dhaka Division", "BGD", NA, "Dhaka", "Division-level entry",
  "Khulna Division", "BGD", NA, "Khulna", "Division-level entry",
  "Mymensingh Division", "BGD", NA, "Mymensingh", "Division-level entry",

                                        # ---- India ----
  "Siliguri", "IND", "Siliguri", "West Bengal", NA,
  "Nearby Districts Of Kozhikode", "IND", "Kozhikode", "Kerala", "Primary reference district",
  "Kerela", "IND", NA, "Kerala", "Common misspelling",
  "West Bangal", "IND", NA, "West Bengal", "Common misspelling",

  # ---- Malaysia ----
  "Seremban Hospital", "MYS", "Seremban", "Negeri Sembilan", NA,
  "Kuala Lumpur Hospital", "MYS", "Kuala Lumpur", "Kuala Lumpur", NA,
  "Ipoh Hospital", "MYS", "Kinta", "Perak", "District of Kinta",
  "University Of Malaya Medical Center", "MYS", "Kuala Lumpur", "Kuala Lumpur", NA,
  "University Malaya Medical Centre", "MYS", "Kuala Lumpur", "Kuala Lumpur", "Alternate spelling",
  "Kelang Hospita", "MYS", "Klang", "Selangor", "Misspelling of Klang",
  "Tioman Island", "MYS", "Rompin", "Pahang", NA,
  "Kampung Sungai Nipah", "MYS", "Seremban", "Negeri Sembilan", NA,
  "Malaysia", "MYS", NA, NA, "Country-level entry",
  "Peninsular Malaysia", "MYS", NA, NA, "Regional designation",
  "Negri Sembilan State", "MYS", NA, "Negeri Sembilan", "Common alternate spelling",
  "University Hospital", "MYS", "Kuala Lumpur", "Kuala Lumpur", "Generic reference",
  "Bukit Pelandok", "MYS", "Port Dickson", "Negeri Sembilan", NA,
  "Negeri Sembalin", "MYS", NA, "Negeri Sembilan", "Misspelling",
  "Fatimah Hospital Ipoh", "MYS", "Kinta", "Perak", NA,
  "Sibu Hospital Sarawak", "MYS", "Sibu", "Sarawak", NA,

  # ---- Other Countries ----
  "Villages Across The South Of Cameroon", "CMR", "Océan", "South", "Approximate district",
  "Senator Ninoy Aquino", "PHL", "Sultan Kudarat", "Sultan Kudarat", NA,
  "Wat Luang", "THA", NA, NA, "Needs precise district info",

  # ---- Empty Entry ----
  "", NA, NA, NA, "Blank entry"
)

# Remove empty row if needed
location_mapping <- location_mapping %>% filter(location != "")

location_mapping <- location_mapping %>%
  mutate(across(where(is.list), ~ map_chr(., ~ paste(., collapse = "; ")))) %>%
  separate_rows(district, sep = ";") %>%
  mutate(district = str_trim(district))

subcolumns_outbreak <- readRDS("cleaned_outbreak_data.RDS") 

locations_with_cases_and_deaths <- subcolumns_outbreak %>%
  group_by(outbreak_location, outbreak_country) %>%
  summarise(
    tot_cases = sum(total_cases),
    tot_deaths = sum(deaths)
  ) %>%
  left_join(location_mapping, by = c("outbreak_location" = "location")) %>%
  mutate(map_location = coalesce(district, division_or_state, outbreak_location),
         outbreak_country) %>%
  ungroup() |>
  dplyr::select(outbreak_country, map_location, tot_cases, tot_deaths)

locations_with_cases_and_deaths$tot_cases_binned <-
  cut(locations_with_cases_and_deaths$tot_cases, breaks = c(1, 10, 30, 45, 235),
      right = FALSE, order_result = TRUE)


saveRDS(locations_with_cases_and_deaths, "locations_with_cases_and_deaths.rds")
orderly_artefact(files = "locations_with_cases_and_deaths.rds")






## prepare shapefiles for maps
## this is the shapefile with country boundaries
orderly_shared_resource("World_Bank_Official_Boundaries_adm0/")
orderly_shared_resource("World_Bank_Official_Boundaries_adm1/")
orderly_shared_resource("World_Bank_Official_Boundaries_adm2/")
orderly_shared_resource("World_Bank_Official_Boundaries_Ocean_Mask/")

l0_in <- read_sf("World_Bank_Official_Boundaries_adm0/WB_GAD_ADM0.shp") %>%                                rename(COUNTRY = NAM_0) 
#

### this is the shapefile with level 1 regions
l1_in <- read_sf("World_Bank_Official_Boundaries_adm1/WB_GAD_ADM1.shp") %>%
  rename(COUNTRY = NAM_0) %>%
  mutate(COUNTRY = case_when( # country names must be consistent between shapefiles
    COUNTRY == "Cabo Verde" ~ "Cape Verde",
    COUNTRY == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    COUNTRY == "Guinea Bissau" ~ "Guinea-Bissau",
    TRUE ~ COUNTRY
  )) %>%
  rename(REG_CODE = ADM1CD_c) # store region codes, e.g. SL01, in column REG_CODE

l2_in <- read_sf("World_Bank_Official_Boundaries_adm2/WB_GAD_ADM2.shp") %>%
  rename(COUNTRY = NAM_0)

om <- read_sf("World_Bank_Official_Boundaries_Ocean_Mask/WB_GAD_ocean_mask.shp") 



# what is the 'true' number of cases we assign, how do we demonstrate time dimension?

# outbreaks %>% filter(!is_duplicate) %>% group_by(outbreak_location, outbreak_start_year) %>% summarise(n=n()) %>% filter(n>1)

# world               <- ne_countries(scale = "medium", returnclass = "sf")
# worldmap            <- st_transform(world, crs = st_crs(l0))

# southeast_asia_cropped <- st_crop(worldmap, xmin = -20, xmax = 45,
#                          ymin = 30, ymax = 73)


l0 <- l0_in %>%
  left_join(
    rename(
      locations_with_cases_and_deaths,
      tc_l0 = tot_cases,
      td_l0 = tot_deaths,
      tc_binned = tot_cases_binned
    ),
    by = c("COUNTRY" = "map_location")) %>%
  mutate(total_cases = tc_l0, total_deaths = td_l0, tot_cases_binned = tc_binned)

l1 <- l1_in %>%
  left_join(
    rename(
      locations_with_cases_and_deaths, tc_l1 = tot_cases, td_l1 = tot_deaths,
      tc_binned = tot_cases_binned
    ),
    by = c("NAM_1" = "map_location")) %>%
  mutate(total_cases = tc_l1, total_deaths = td_l1, tot_cases_binned = tc_binned)

l2 <- left_join(
  l2_in,
    rename(
      locations_with_cases_and_deaths, tc_l2 = tot_cases, td_l2 = tot_deaths,
      tc_l2_binned = tot_cases_binned
    ),
    by = c("NAM_2" = "map_location", "COUNTRY" = "outbreak_country")) %>%
  left_join(
    rename(
      locations_with_cases_and_deaths, tc_l1 = tot_cases, td_l1 = tot_deaths,
      tc_l1_binned = tot_cases_binned
    ),
    by = c("NAM_1" = "map_location", "COUNTRY" = "outbreak_country")) %>%
  left_join(
    rename(
      locations_with_cases_and_deaths , tc_l0 = tot_cases, td_l0 = tot_deaths,
      tc_l0_binned = tot_cases_binned
    ),
    by = c("COUNTRY" = "map_location")) %>%
  mutate(
    total_cases = coalesce(tc_l2, tc_l1, tc_l0),
    total_deaths = coalesce(td_l2, td_l1, td_l0),
    total_cases_binned = coalesce(tc_l2_binned, tc_l1_binned, tc_l0_binned)
  ) %>%
  filter(!is.na(total_cases)) # remove visual noise

saveRDS(l0, "l0_shapefile_with_cases_and_deaths.rds")
saveRDS(l1, "l1_shapefile_with_cases_and_deaths.rds")
saveRDS(l2, "l2_shapefile_with_cases_and_deaths.rds")

orderly_artefact(files = c(
  "l0_shapefile_with_cases_and_deaths.rds",
  "l1_shapefile_with_cases_and_deaths.rds",
  "l2_shapefile_with_cases_and_deaths.rds"
))
