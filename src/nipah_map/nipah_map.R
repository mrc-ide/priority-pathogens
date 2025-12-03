library(orderly2)
library(tidyverse)
library(stringr)
library(metafor)
library(meta)
library(estmeansd)
library(mixdist)
library(ggplot2)
library(ggsci)
library(sf)
library(ragg)
library(ggspatial)
library(ggforce)
library(png)
library(grid)
library(patchwork)
library(gridExtra)

# *============================================================================*
# *------------------------------ Orderly config ------------------------------*
orderly_strict_mode()

orderly_parameters(pathogen = "NIPAH")

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "params.csv","outbreaks.csv"))

orderly_shared_resource("nipah_functions.R"="nipah_functions.R")
orderly_shared_resource("nipah_bangladesh_district_data.csv"="nipah_bangladesh_district_data.csv")
orderly_shared_resource("NIPAH_Bangladesh_IEDCR.csv"="NIPAH_Bangladesh_IEDCR.csv")

source("nipah_functions.R")

orderly_artefact("nipah-specific figures",c("nipha_outbreaks_map.pngf"))

###################
## DATA CURATION ##
###################

articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles,outbreaks,models,parameters, plotting = TRUE)

articles   <- dfs$articles

articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles %>% dplyr::select(covidence_id,qa_score)

outbreaks  <- dfs$outbreaks
models     <- dfs$models
parameters <- dfs$parameters %>% left_join(qa_scores) %>%
  mutate(article_label = make.unique(refs)) %>%
  mutate(article_label = factor(article_label,levels=rev(unique(article_label)))) %>%
  mutate(in_CSF = case_when(str_detect(parameter_notes,'CSF')~TRUE,
                            TRUE~FALSE),
         population_location = coalesce(population_location, population_country))

nipah_bangladesh <-read_csv('nipah_bangladesh_district_data.csv')

# *============================= LOCATIONS ===============================================*
parameter_locs <- str_to_title(unique(str_trim(na.omit(unlist(lapply(parameters$population_location, FUN=function(x) unlist(str_split(x,';'))))))))
outbreak_locs  <- str_to_title(unique(str_trim(na.omit(unlist(lapply(outbreaks$outbreak_location, FUN=function(x) unlist(str_split(x,';'))))))))
locations <- unique(c(parameter_locs, outbreak_locs))

#prepare shapefiles for maps
l0_in <- read_sf('../../shared/World_Bank_Official_Boundaries_adm0/WB_GAD_ADM0.shp') %>% #this is the shapefile with country boundaries
  rename(COUNTRY = NAM_0) #store country names in column COUNTRY
#

#
l1_in <- read_sf('../../shared/World_Bank_Official_Boundaries_adm1/WB_GAD_ADM1.shp') %>% #this is the shapefile with level 1 regions
  rename(COUNTRY = NAM_0) %>%
  mutate(COUNTRY = case_when( #country names must be consistent between shapefiles
    COUNTRY == "Cabo Verde" ~ "Cape Verde",
    COUNTRY == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    COUNTRY == "Guinea Bissau" ~ "Guinea-Bissau",
    TRUE ~ COUNTRY)) %>%
  rename(REG_CODE = ADM1CD_c) #store region codes, e.g. SL01, in column REG_CODE

l2_in <- read_sf('../../shared/World_Bank_Official_Boundaries_adm2/WB_GAD_ADM2.shp') %>% #this is the shapefile with level 1 regions
  rename(COUNTRY = NAM_0)

om <- read_sf('../../shared/World_Bank_Official_Boundaries_Ocean_Mask/WB_GAD_ocean_mask.shp')

mapped_locations   <- locations[(locations %in% l2_in$NAM_2)|(locations %in% l2_in$NAM_1)|(locations %in% l2_in$COUNTRY)]
unmapped_locations <- locations[!(locations %in% l2_in$NAM_2) & !(locations %in% l2_in$NAM_1) & !(locations %in% l2_in$COUNTRY)]


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
  "Siliguri", "IND", "Darjeeling", "West Bengal", NA,
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
  mutate(across(where(is.list), ~map_chr(., ~paste(., collapse = "; ")))) %>%
  separate_rows(district, sep = ";") %>%
  mutate(district=str_trim(district))

location_mapping[!((location_mapping$district %in% l2_in$NAM_2)|(location_mapping$district %in% l2_in$NAM_1)|
                     location_mapping$division_or_state %in% l2_in$NAM_1),]

# *=============== DEDUPLICATE OUTBREAKS =============================================================*
outbreaks <- outbreaks %>% mutate(outbreak_location = coalesce(outbreak_location, outbreak_country),
                                  outbreak_end_year = coalesce(outbreak_end_year,outbreak_start_year),
                                  outbreak_duriation_years = outbreak_end_year - outbreak_start_year,
                                  type_cases_sex_disagg = replace_na(type_cases_sex_disagg,'Unspecified'),
                                  outbreak_notes = replace_na(outbreak_notes,''),
                                  outbreak_location_type = str_to_title(replace_na(outbreak_location_type,'Unspecified')))
outbreaks$is_duplicate <- FALSE
outbreaks[outbreaks$outbreak_duriation_years>5,]$is_duplicate <- TRUE
outbreaks[outbreaks$outbreak_country == 'Malaysia,Singapore',]$is_duplicate <- TRUE
outbreaks[outbreaks$outbreak_country == 'Malaysia' & outbreaks$outbreak_location_type != 'State',]$is_duplicate <- TRUE #we take the individual state outbreaks rather than the national aggregate one.
outbreaks[outbreaks$outbreak_country == 'Singapore' & (is.na(outbreaks$deaths) | outbreaks$type_cases_sex_disagg=='Confirmed'),]$is_duplicate <- TRUE #we take the report with confirmed cases, also not the sero study
outbreaks[str_detect(outbreaks$outbreak_notes,'cluster'),]$is_duplicate <- TRUE #we take the individual outbreaks rather than the cluster

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

outbreaks <- outbreaks %>% filter( !is_duplicate ) %>%
  mutate( unique_id = paste(covidence_id, outbreak_country, outbreak_location,
                            outbreak_start_year, str_remove(outbreak_start_month, "^0+"), sep = "|"),
          EXCLUDE   = ifelse(unique_id %in% exclude_ids, 1, 0)) %>%
  filter(!EXCLUDE)

subcolumns_outbreak <- outbreaks %>% dplyr::select(outbreak_country,outbreak_location,outbreak_source,cases_confirmed,cases_suspected,cases_asymptomatic,cases_unspecified,outbreak_probable,deaths,outbreak_start_month,outbreak_start_year,refs) %>%
  mutate( cases_confirmed_raw = cases_confirmed) %>%
  mutate( cases_confirmed = coalesce(cases_confirmed,cases_suspected)) %>% # only use cases suspected if we don't have confirmed cases
  mutate( cases_confirmed = case_when(cases_confirmed<deaths ~ cases_confirmed + replace_na(cases_suspected,0), # for Bangladesh we have some instances which require suspected cases to be added to confirmed to be feasible with deaths
                                      TRUE ~ cases_confirmed )) %>%
  mutate_if(is.numeric, list(~replace_na(., 0))) %>%
  mutate( total_cases = cases_confirmed + cases_asymptomatic + cases_unspecified + outbreak_probable) %>%
  arrange(outbreak_start_year,outbreak_start_month) %>%
  mutate(num_loc = str_count(outbreak_location, ";") + 1 ) %>%
  separate_rows(outbreak_location, sep = ";") %>%
  mutate(outbreak_location=str_trim(outbreak_location),
         total_cases = total_cases/num_loc,
         deaths = deaths / num_loc)    # for location where we split report the AVERAGE number of cases

location_agg <- subcolumns_outbreak %>% group_by(outbreak_location) %>%
  summarise(tot_cases = sum(total_cases),
            tot_deaths = sum(deaths)) %>%
  left_join(location_mapping, by = c("outbreak_location" = "location")) %>%
  mutate(map_location=coalesce(district, division_or_state, outbreak_location)) %>%
  dplyr::select(map_location,tot_cases,tot_deaths)


# what is the 'true' number of cases we assign, how do we demonstrate time dimension?

#outbreaks %>% filter(!is_duplicate) %>% group_by(outbreak_location, outbreak_start_year) %>% summarise(n=n()) %>% filter(n>1)

#world               <- ne_countries(scale = "medium", returnclass = "sf")
#worldmap            <- st_transform(world, crs = st_crs(l0))

#southeast_asia_cropped <- st_crop(worldmap, xmin = -20, xmax = 45,
#                          ymin = 30, ymax = 73)


l0 <- l0_in %>% left_join(location_agg %>% rename(tc_l0=tot_cases,td_l0=tot_deaths),by=c('COUNTRY'='map_location')) %>%
  mutate(total_cases = tc_l0,
         total_deaths = td_l0)

l1 <- l1_in %>% left_join(location_agg %>% rename(tc_l1=tot_cases,td_l1=tot_deaths),by=c('NAM_1'='map_location')) %>%
  mutate(total_cases = tc_l1,
         total_deaths = td_l1)

l2 <- l2_in %>% left_join(location_agg %>% rename(tc_l2=tot_cases,td_l2=tot_deaths),by=c('NAM_2'='map_location')) %>%
  left_join(location_agg %>% rename(tc_l1=tot_cases,td_l1=tot_deaths),by=c('NAM_1'='map_location')) %>%
  left_join(location_agg %>% rename(tc_l0=tot_cases,td_l0=tot_deaths),by=c('COUNTRY'='map_location')) %>%
  mutate(total_cases = coalesce(tc_l2, tc_l1, tc_l0),
         total_deaths = coalesce(td_l2, td_l1, td_l0)) %>%
  filter(!is.na(total_cases))     # remove visual noise



location_mapping_bangladesh <- tribble(
  ~location, ~iso3, ~adm2_code, ~district, ~division,
  # ---- Bangladesh Districts ----
  "Jhalakati", "BGD", "BGD008004", "Jhalokathi", "Barishal",
  "Cumilla", "BGD", "BGD002003", "Cumilla", "Chittagong",
  "Dhaka", "BGD", "BGD003009", "Dhaka", "Dhaka",
  "Faridpur", "BGD", "BGD003002", "Faridpur", "Dhaka",
  "Gopalganj", "BGD", "BGD008002", "Gopalganj", "Barishal",
  "Madaripur", "BGD", "BGD008003", "Madaripur", "Barishal",
  "Manikganj", "BGD", "BGD003006", "Manikganj", "Dhaka",
  "Rajbari", "BGD", "BGD003008", "Rajbari", "Dhaka",
  "Sariatpur", "BGD", "BGD003011", "Shariatpur", "Dhaka",
  "Tangail", "BGD", "BGD003010", "Tangail", "Dhaka",
  "Chuadanga", "BGD", "BGD004007", "Chuadanga", "Khulna",
  "Jhenaidah", "BGD", "BGD004006", "Jhenaidah", "Khulna",
  "Khulna", "BGD", "BGD004005", "Khulna", "Khulna",
  "Kushtia", "BGD", "BGD004008", "Kushtia", "Khulna",
  "Magura", "BGD", "BGD004009", "Magura", "Khulna",
  "Meherpur", "BGD", "BGD004004", "Meherpur", "Khulna",
  "Norail", "BGD", "BGD004010", "Narail", "Khulna",
  "Mymensingh", "BGD", "BGD009003", "Mymensingh", "Mymensingh",
  "Bogura", "BGD", "BGD006001", "Bogra", "Rajshahi",
  "Chapainawabganj", "BGD", "BGD006008", "Chapai Nawabganj", "Rajshahi",
  "Naogaon", "BGD", "BGD006004", "Naogaon", "Rajshahi",
  "Natore", "BGD", "BGD006003", "Natore", "Rajshahi",
  "Pabna", "BGD", "BGD006007", "Pabna", "Rajshahi",
  "Rajshahi", "BGD", "BGD006006", "Rajshahi", "Rajshahi",
  "Dinajpur", "BGD", "BGD005004", "Dinajpur", "Rangpur",
  "Gaibandha", "BGD", "BGD005002", "Gaibandha", "Rangpur",
  "Jaipurhat", "BGD", "BGD006002", "Joypurhat", "Rajshahi",
  "Kurigram", "BGD", "BGD005006", "Kurigram", "Rangpur",
  "Lalmonirhat", "BGD", "BGD005002", "Lalmonirhat", "Rangpur",
  "Nilphamari", "BGD", "BGD005003", "Nilphamari", "Rangpur",
  "Panchagarh", "BGD", "BGD005008", "Panchagarh", "Rangpur",
  "Rangpur", "BGD", "BGD005005", "Rangpur", "Rangpur",
  "Thakurgaon", "BGD", "BGD005007", "Thakurgaon", "Rangpur"
)


hospitals <- data.frame(
  name = c(
    "Rajshahi Medical College Hospital",
    "Rangpur Medical College Hospital",
    "Bangabandhu Sheikh Mujib Medical College Hospital",
    "Tangail General Hospital",
    "Rajbari General Hospital",
    "Chattogram Medical College Hospital",
    "Khulna Medical College Hospital",
    "Sher e Bangla Medical College Hospital",
    "Mymensingh Medical College Hospital",
    "Sythet IMG Osmani Medical College Hospital",
    "Naogaon Sadar Hospital",
    "Joypurhat Sadar Hospital",
    "Meherpur Sadar Hospital",
    "Manikganj Sadar Hospital",
    "Bogra Medical College Hospital"
  ),
  `Surveillance period` = c(
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
  )
)

manual_coords <- data.frame(
  name = hospitals$name,
  lat = c(24.3645, 25.7439, 23.6010, 24.2513, 23.7610, 22.3569, 22.8456, 22.7010, 24.7471, 24.8918, 24.8136, 25.0947, 23.7669, 23.8603, 24.8510),
  long = c(88.6283, 89.2752, 89.8337, 89.9167, 89.6410, 91.7832, 89.5403, 90.3535, 90.4203, 91.8800, 88.9314, 89.0944, 88.6622, 90.0058, 89.3711),
  `Surveillance period` = hospitals$Surveillance.period
)


nipah_bangladesh <- nipah_bangladesh %>% left_join(location_mapping_bangladesh, by = c("District" = "location"))

l2_bangladesh <- l2_in %>% filter(COUNTRY=='Bangladesh') %>% left_join( nipah_bangladesh %>% rename(tc_l2=`Grand Total`),by=c('ADM2CD_c'='adm2_code')) %>%
  left_join(nipah_bangladesh %>% dplyr::select(-Division) %>% rename(tc_l1=`Grand Total`),by=c('NAM_1'='District')) %>%
  mutate(total_cases = coalesce(tc_l2, tc_l1)) #%>%
#filter(!is.na(total_cases))     # remove visual noise

nipah_country <- l2$COUNTRY |> unique()

# Need to fix India, include Bangladesh as a panel so that it can be seen more easily.
# Maybe split this into a multi-panel figure??? Chat with Tristan
gg <- ggplot() +
  geom_sf(data = om, lwd = 0.001, col = "lightgrey", fill = 'lightblue',alpha=0.3) +
  geom_sf(data = l0_in, lwd = 0.5, col = "black",  fill = NA) +
  coord_sf(xlim = c(70, 130), ylim = c(-5, 30), expand = FALSE) +
  scale_fill_continuous(na.value="white") +
  theme_bw()


# Bangladesh inset
gg_bangladesh <- ggplot() +
  geom_sf(data = l2_bangladesh, lwd = 0.2, col = "darkgrey",  aes(fill = total_cases),na.rm = TRUE) +
  geom_sf(data = om, lwd = 0.001, col = "lightgrey", fill = 'lightblue',alpha=0.3) +
  geom_sf(data = l0_in, lwd = 0.5, col = "black",  fill = NA) +
  coord_sf(xlim = c(87.9, 93), ylim = c(21, 26.7), expand = FALSE) +
  geom_point(data = manual_coords, aes(x = long, y = lat, color = Surveillance.period), size = 2, shape = 18) +
  geom_text(data = manual_coords, aes(x = long, y = lat, label = name), hjust = 0, vjust = 0, nudge_y = 0.05, size = 2.5) +
  scale_fill_gradient(low='palegreen', high="darkblue",
                      trans = scales::trans_new("log2p",
                                                transform = function(x) log2(x + 1),
                                                inverse = function(x) 2^x - 1),
                      breaks = c(0, 1, 4, 8, 16, 64, 240),
                      name = 'Total cases',na.value="grey95",limits=c(0,240)) +
  ggsci::scale_color_lancet() + xlab('') + ylab('') +
  labs(title='Bangladesh IEDCR Surveillance Data',color = "Surveillance Period") +
  theme_bw()

# North India + Bangladesh from outbreak data
gg_northern_india_bangladesh <- ggplot()  +
  geom_sf(data = l1 %>% filter(COUNTRY %in% c('India','Bangladesh')), lwd = 0.4, col = "darkgrey",  aes(fill = total_cases),na.rm = TRUE) +
  geom_sf(data = l2 %>% filter(NAM_2 %in% location_agg$map_location), lwd = 0.2, col = "darkgrey",  aes(fill = total_cases),na.rm = TRUE) +
  geom_sf(data = l0_in %>% filter(COUNTRY %in% c('India','Bangladesh')), lwd = 0.5, col = "black",  fill = NA) +
  geom_sf(data = om, lwd = 0.001, col = "lightgrey", fill = 'lightblue',alpha=0.3) +
  coord_sf(xlim = c(83, 92), ylim = c(20, 28), expand = FALSE) +
  scale_fill_gradient(low='palegreen', high="darkblue",
                      trans = scales::trans_new("log2p",
                                                transform = function(x) log2(x + 1),
                                                inverse = function(x) 2^x - 1),
                      breaks = c(0, 1, 4, 8, 16, 64, 240),
                      name = 'Total cases',na.value="grey95",limits=c(0,240)) +
  labs(title='Northern India & Bangladesh\nreported outbreaks' ) +
  theme_bw()

# Kerala
gg_kerala <- ggplot()  +
  geom_sf(data = l1 %>% filter(COUNTRY %in% c('India')), lwd = 0.4, col = "darkgrey",  aes(fill = total_cases),na.rm = TRUE) +
  geom_sf(data = l2 %>% filter(NAM_2 %in% location_agg$map_location), lwd = 0.2, col = "darkgrey",  aes(fill = total_cases),na.rm = TRUE) +
  geom_sf(data = l0_in %>% filter(COUNTRY %in% c('India')), lwd = 0.5, col = "black",  fill = NA) +
  geom_sf(data = om, lwd = 0.001, col = "lightgrey", fill = 'lightblue',alpha=0.3) +
  coord_sf(xlim = c(74, 78), ylim = c(8, 13), expand = FALSE) +
  scale_fill_gradient(low='palegreen', high="darkblue",
                      trans = scales::trans_new("log2p",
                                                transform = function(x) log2(x + 1),
                                                inverse = function(x) 2^x - 1),
                      breaks = c(0, 1, 4, 8, 16, 64, 240),
                      name = 'Total cases',na.value="grey95",limits=c(0,240)) +
  labs(title='Kerala\nreported outbreaks' ) +
  theme_bw()



# Malaysia & Singapore
gg_malaysia_singapore <- ggplot()  +
  geom_sf(data = l1 %>% filter(COUNTRY %in% c('Malaysia','Singapore')), lwd = 0.4, col = "darkgrey",  aes(fill = total_cases),na.rm = TRUE) +
  geom_sf(data = l2 %>% filter(NAM_2 %in% location_agg$map_location), lwd = 0.2, col = "darkgrey",  aes(fill = total_cases),na.rm = TRUE) +
  geom_sf(data = l0_in %>% filter(COUNTRY %in% c('Malaysia','Singapore')), lwd = 0.5, col = "black",  fill = NA) +
  geom_sf(data = l0 %>% filter(COUNTRY %in% c('Singapore')), lwd = 0.7, col = "black",  aes(fill = total_cases),na.rm = TRUE) +
  geom_sf(data = om, lwd = 0.001, col = "lightgrey", fill = 'lightblue',alpha=0.3) +
  coord_sf(xlim = c(100, 105), ylim = c(1, 7), expand = FALSE) +
  scale_fill_gradient(low='palegreen', high="darkblue",
                      trans = scales::trans_new("log2p",
                                                transform = function(x) log2(x + 1),
                                                inverse = function(x) 2^x - 1),
                      breaks = c(0, 1, 4, 8, 16, 64, 240),
                      name = 'Total cases',na.value="grey95",limits=c(0,240)) +
  labs(title='Malaysian peninsular\nreported outbreaks' ) +
  theme_bw()


# Philippines
gg_philippines <- ggplot()  +
  geom_sf(data = l1 %>% filter(COUNTRY %in% c('Philippines')), lwd = 0.4, col = "darkgrey",  aes(fill = total_cases),na.rm = TRUE) +
  geom_sf(data = l2 %>% filter(NAM_2 %in% location_agg$map_location), lwd = 0.2, col = "darkgrey",  aes(fill = total_cases),na.rm = TRUE) +
  geom_sf(data = l0_in %>% filter(COUNTRY %in% c('Philippines')), lwd = 0.5, col = "black",  fill = NA) +
  geom_sf(data = om, lwd = 0.001, col = "lightgrey", fill = 'lightblue',alpha=0.3) +
  coord_sf(xlim = c(115, 130), ylim = c(5, 20), expand = FALSE) +
  scale_fill_gradient(low='palegreen', high="darkblue",
                      trans = scales::trans_new("log2p",
                                                transform = function(x) log2(x + 1),
                                                inverse = function(x) 2^x - 1),
                      breaks = c(0, 1, 4, 8, 16, 64, 240),
                      name = 'Total cases',na.value="grey95",limits=c(0,240)) +
  labs(title='Philippines\nreported outbreaks' ) +
  theme_bw()

layout_design <-
  "AAAEE
   AAAEE
   AAAEE
   AAAEE
   AAAEE
   #BCD#
   #BCD#"

map_plot <-  gg_northern_india_bangladesh + gg_kerala + gg_malaysia_singapore + gg_philippines + gg_bangladesh +
  plot_layout(guides = 'collect', design = layout_design) + plot_annotation(tag_levels = 'A')

ggsave("nipha_outbreaks_map.png", plot = map_plot, width = 17, height = 13)
