library(dplyr)
library(orderly)
library(readr)
library(sf)

orderly_shared_resource("World_Bank_Official_Boundaries_adm2/")
orderly_shared_resource(
  "nipah_bangladesh_district_data.csv" = "nipah_bangladesh_district_data.csv"
)

nipah_bangladesh <- read_csv('nipah_bangladesh_district_data.csv')
l2_in <- read_sf("World_Bank_Official_Boundaries_adm2/WB_GAD_ADM2.shp") %>%
  rename(COUNTRY = NAM_0)

nipah_bangladesh[
  nipah_bangladesh$District == "Sariatpur",
  "District"
] <- "Shariatpur"
nipah_bangladesh[
  nipah_bangladesh$District == "Norail",
  "District"
] <- "Narail"

location_mapping_bangladesh <- tibble::tribble(
  ~location         , ~adm2_code  ,
  "Jhalakati"       , "BGD001004" ,
  "Cumilla"         , "BGD002006" ,
  "Dhaka"           , "BGD003001" ,
  "Faridpur"        , "BGD003002" ,
  "Gopalganj"       , "BGD003004" ,
  "Madaripur"       , "BGD003006" ,
  "Manikganj"       , "BGD003007" ,
  "Rajbari"         , "BGD003011" ,
  "Shariatpur"      , "BGD003012" ,
  "Tangail"         , "BGD003013" ,
  "Chuadanga"       , "BGD004002" ,
  "Jhenaidah"       , "BGD004004" ,
  "Khulna"          , "BGD004005" ,
  "Kushtia"         , "BGD004006" ,
  "Magura"          , "BGD004007" ,
  "Meherpur"        , "BGD004008" ,
  "Narail"          , "BGD004009" ,
  "Mymensingh"      , "BGD005002" ,
  "Bogura"          , "BGD006001" ,
  "Chapainawabganj" , "BGD006002" ,
  "Naogaon"         , "BGD006004" ,
  "Natore"          , "BGD006005" ,
  "Pabna"           , "BGD006006" ,
  "Rajshahi"        , "BGD006007" ,
  "Dinajpur"        , "BGD007001" ,
  "Gaibandha"       , "BGD007002" ,
  "Jaipurhat"       , "BGD006003" ,
  "Kurigram"        , "BGD007003" ,
  "Lalmonirhat"     , "BGD007004" ,
  "Nilphamari"      , "BGD007005" ,
  "Panchagarh"      , "BGD007006" ,
  "Rangpur"         , "BGD007007" ,
  "Thakurgaon"      , "BGD007008"
)

bangladesh_hospitals <- tibble::tibble(
  name = c(
    "Rajshahi Medical College Hospital",
    "Rangpur Medical College Hospital",
    "Bangabandhu Sheikh Mujib Medical College Hospital",
    "Tangail General Hospital",
    "Rajbari General Hospital",
    "Chattogram Medical College Hospital",
    "Khulna Medical College Hospital",
    "Sher-E-Bangla Medical College Hospital",
    "Mymensingh Medical College Hospital",
    "Sythet IMG Osmani Medical College Hospital",
    "Naogaon Sadar Hospital",
    "Joypurhat Sadar Hospital",
    "Meherpur Sadar Hospital",
    "Manikganj Sadar Hospital",
    "Bogra Medical College Hospital"
  ),
  surveillance_period = c(
    "2006 - ongoing",
    "2006 - ongoing",
    "2006 - ongoing",
    "2006 - ongoing",
    "2006 - ongoing",
    "2018 - ongoing",
    "2018 - ongoing",
    "2020 - ongoing",
    "2020 - ongoing",
    "2021 - ongoing",
    "2006 - 2009",
    "2006 - 2009",
    "2006 - 2009",
    "2006 - 2009",
    "2006 - 2009"
  ),
  lat = c(
    24.3645,
    25.7439,
    23.6010,
    24.2513,
    23.7610,
    22.3569,
    22.8456,
    22.7010,
    24.7471,
    24.8918,
    24.8136,
    25.0947,
    23.7669,
    23.8603,
    24.8510
  ),
  long = c(
    88.6283,
    89.2752,
    89.8337,
    89.9167,
    89.6410,
    91.7832,
    89.5403,
    90.3535,
    90.4203,
    91.8800,
    88.9314,
    89.0944,
    88.6622,
    90.0058,
    89.3711
  )
)

iedcr_locations_with_cases <- nipah_bangladesh

iedcr_locations_with_cases$tot_cases_binned <-
  cut(
    nipah_bangladesh$`Grand Total`,
    breaks = c(1, 10, 20, 30, 75),
    right = FALSE,
    order_result = TRUE
  )


l2_iedcr_bangladesh <- l2_in %>%
  filter(COUNTRY == "Bangladesh") %>%
  left_join(
    iedcr_locations_with_cases %>%
      left_join(
        location_mapping_bangladesh,
        by = c("District" = "location")
      ) %>%
      rename(tc_l2 = `Grand Total`),
    by = c("ADM2CD_c" = "adm2_code")
  ) %>%
  mutate(total_cases = tc_l2)


saveRDS(
  l2_iedcr_bangladesh,
  "l2_bangladesh_iedcr_shapefile_with_cases.rds"
)
saveRDS(
  bangladesh_hospitals,
  "bangladesh_hospital_locs.rds"
)

orderly_artefact(
  files = c(
    "l2_bangladesh_iedcr_shapefile_with_cases.rds",
    "bangladesh_hospital_locs.rds"
  )
)
