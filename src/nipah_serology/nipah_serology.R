####################
## SEROPREVALENCE ##
####################

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

orderly_parameters(pathogen = NULL)

orderly_dependency(
  "db_cleaning",
  "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "models.csv", "params.csv", "outbreaks.csv"))

orderly_shared_resource("nipah_functions.R"="nipah_functions.R")
orderly_shared_resource("nipah_bangladesh_district_data.csv"="nipah_bangladesh_district_data.csv")
orderly_shared_resource("NIPAH_Bangladesh_IEDCR.csv"="NIPAH_Bangladesh_IEDCR.csv")

source("nipah_functions.R")

orderly_artefact("nipah-specific figures",c("figure_2.png","figure_2.pdf"))

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

mapped_locations   <- locations[(locations %in% l2$NAM_2)|(locations %in% l2$NAM_1)|(locations %in% l2$COUNTRY)]
unmapped_locations <- locations[!(locations %in% l2$NAM_2) & !(locations %in% l2$NAM_1) & !(locations %in% l2$COUNTRY)]


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



# *============================================================================*
#extract data to be plotted
sero_studies <- parameters %>% filter(parameter_class == 'Seroprevalence') %>%
  filter(qa_score>0.5) %>%
  mutate(parameter_unit = 'Percentage (%)', #replace_na(parameter_unit, 'Percentage (%)'),
         population_group = replace_na(population_group, 'Other'),
         sero_CSF = case_when(str_detect(parameter_notes,'CSF') ~ 'CSF',
                             TRUE ~ 'Other'))

# adjusted CFR
# overlap of outbreak location with sero data
# time to seroconversion, anti-body duration... https://www.neurology-asia.org/articles/20001_023.pdf
# also IgG still positive 10yrs post illness https://www.researchgate.net/profile/Victor-Chong-3/publication/287575780_Ten_year_clinical_and_serological_outcomes_of_Nipah_virus_infection/links/56f11b8c08ae519284fbd500/Ten-year-clinical-and-serological-outcomes-of-Nipah-virus-infection.pdf

cfr_bangladesh <- read_csv('NIPAH_Bangladesh_IEDCR.csv')

cfr <- parameters %>% filter(parameter_class == 'Severity') %>%
  filter(qa_score>0.5) %>%
  mutate(parameter_unit = 'Percentage (%)', #replace_na(parameter_unit, 'Percentage (%)'),
         population_group = replace_na(population_group, 'Other'))

cfr <- cfr %>% mutate(parameter_context_location_type=replace_na(parameter_context_location_type,'Unspecified'),
                      parameter_notes=replace_na(parameter_notes,''))

# DONT DEDUPLICATE AS THIS HAS CONTEXT INFORMATION
# cfr$is_duplicate <- FALSE
# #cfr[cfr$outbreak_duriation_years>5,]$is_duplicate <- TRUE
# cfr[cfr$population_country == 'Malaysia,Singapore',]$is_duplicate <- TRUE
# cfr[cfr$population_country == 'Malaysia' & cfr$parameter_context_location_type != 'State',]$is_duplicate <- TRUE #we take the individual state outbreaks rather than the national aggregate one.
# cfr[str_detect(cfr$parameter_notes,'cluster'),]$is_duplicate <- TRUE #we take the individual outbreaks rather than the cluster
#
# cfr <- cfr %>% filter( !is_duplicate ) %>%
#   mutate( unique_id = paste(covidence_id, population_country, population_location,
#                             population_study_start_year, str_remove(population_study_start_month, "^0+"), sep = "|"),
#           EXCLUDE   = ifelse(unique_id %in% exclude_ids, 1, 0)) %>%
#   filter(!EXCLUDE)

cfr_from_outbreaks <- subcolumns_outbreak %>%
  mutate(outbreak_source = replace_na(outbreak_source, 'Unknown')) %>%
  mutate(cfr_ifr_denominator = round(total_cases),
         cfr_ifr_numerator   = round(deaths),
         CFR                 = cfr_ifr_numerator / cfr_ifr_denominator,
         refs                = paste(outbreak_country, outbreak_location, sep = " |> "),
         parameter_value     = CFR,
         parameter_unit      = 'Percentage') %>% arrange(desc(CFR)) %>%
  mutate(outbreak_source = case_when(str_detect(outbreak_source, 'Domestic animal' ) ~ 'Domestic animal',
                                      str_detect(outbreak_source, 'Wild animal' ) ~ 'Wild animal',
                                  TRUE ~ outbreak_source))

cfr_outbreak_ma <- metaprop_wrap(cfr_from_outbreaks, subgroup = 'outbreak_country',
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = 'red',
                    #plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = 'red',
                    width = 5000, height = 4750, resolution = 500)

cfr_cfr_studies_group <- metaprop_wrap(cfr, subgroup = 'population_group',
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = 'red',
                    width = 4000, height = 4750, resolution = 500)

cfr_cfr_studies_country <- metaprop_wrap(cfr, subgroup = 'population_country',
                                       plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = 'red',
                                       width = 4000, height = 4750, resolution = 500)

cfr_from_bangladesh_surveillance <- cfr_bangladesh %>%
  filter(Cases!=0) %>%
  mutate(cfr_ifr_denominator = Cases,
         cfr_ifr_numerator   = Death,
         CFR                 = cfr_ifr_numerator / cfr_ifr_denominator,
         refs                = Year,
         parameter_value     = CFR,
         parameter_unit      = 'Percentage') %>% arrange(desc(CFR))

cfr_from_bangladesh_surveillance_ma <- metaprop_wrap(cfr_from_bangladesh_surveillance, subgroup = NA,
                                 plot_pooled = TRUE, sort_by_subg = FALSE, plot_study = TRUE, digits = 3, colour = 'red',
                                 width = 5500, height = 3750, resolution = 500)

cfr_forest_country <- forest_plot(cfr |> mutate(parameter_value = coalesce(parameter_value,central)) |>
                                  arrange(population_country,desc(parameter_value)),
                                'Serology (%)','population_country',c(0,100),text_size = 13)

cfr_forest_pop_group <- forest_plot(cfr |> mutate(parameter_value = coalesce(parameter_value,central)) |>
                                    arrange(population_group,desc(parameter_value)),
                                  'Serology (%)','population_group',c(0,100),text_size = 13)

# Serological studies
sero_study_type <- metaprop_wrap(sero_studies, subgroup = 'population_study_type',
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = 'red',
                    width = 4000, height = 4750, resolution = 500,
                    at = seq(0,0.3,by=0.05), xlim = c(0,0.35))

sero_group <- metaprop_wrap(sero_studies, subgroup = 'population_group',
                                 plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = 'red',
                                 width = 4000, height = 4750, resolution = 500,
                                 at = seq(0,0.3,by=0.05), xlim = c(0,0.35))

sero_sample_type <- metaprop_wrap(sero_studies, subgroup = 'population_sample_type',
                            plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = 'red',
                            width = 4000, height = 4750, resolution = 500,
                            at = seq(0,0.3,by=0.05), xlim = c(0,0.35))

sero_country <- metaprop_wrap(sero_studies, subgroup = 'population_country',
                            plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = 'red',
                            width = 4000, height = 4750, resolution = 500,
                            at = seq(0,0.3,by=0.05), xlim = c(0,0.35))

sero_CSF <- metaprop_wrap(sero_studies, subgroup = 'sero_CSF',
                              plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = 'red',
                              width = 4000, height = 4750, resolution = 500,
                              at = seq(0,0.3,by=0.05), xlim = c(0,0.35))

sero_sero_type <- metaprop_wrap(sero_studies, subgroup = 'parameter_type',
                          plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = 'red',
                          width = 4000, height = 4750, resolution = 500,
                          at = seq(0,0.3,by=0.05), xlim = c(0,0.35))

sero_forest_type <- forest_plot(sero_studies |> mutate(parameter_value = coalesce(parameter_value,central)) |>
                                  arrange(parameter_type,desc(parameter_value)),
                            'Serology (%)','parameter_type',c(0,100),text_size = 13)

sero_forest_pop_type <- forest_plot(sero_studies |> mutate(parameter_value = coalesce(parameter_value,central)) |>
                                  arrange(population_sample_type,desc(parameter_value)),
                                'Serology (%)','population_sample_type',c(0,100),text_size = 13)

sero_forest_pop_group <- forest_plot(sero_studies |> mutate(parameter_value = coalesce(parameter_value,central)) |>
                                      arrange(population_group,desc(parameter_value)),
                                    'Serology (%)','population_group',c(0,100),text_size = 13)

sero_forest_sero_CSF <- forest_plot(sero_studies |> mutate(parameter_value = coalesce(parameter_value,central)) |>
                                       arrange(sero_CSF,desc(parameter_value)),
                                     'Serology (%)','sero_CSF',c(0,100),text_size = 13)

sero_forest_country <- forest_plot(sero_studies |> mutate(parameter_value = coalesce(parameter_value,central)) |>
                                      arrange(population_country,desc(parameter_value)),
                                    'Serology (%)','population_country',c(0,100),text_size = 13)

#why is seroprevalence so low in people under investigation??

d0 <- parameters %>% filter(parameter_class == 'Seroprevalence') %>%
  filter( (population_sample_type == 'Population-Based' | population_sample_type == 'Community-Based') &
           population_group == 'General population')

forest_plot(d0 |> mutate(parameter_value = coalesce(parameter_value,central)) |>
              arrange(population_country,desc(parameter_value)),
            'Serology (%)','population_country',c(0,25),text_size = 13)


d1 <- parameters %>% filter(parameter_class == 'Seroprevalence' &
                              parameter_type != 'IgG' &
                              parameter_type != 'IgM' &
                              (population_sample_type == 'Population-Based' |
                                 population_sample_type == 'Community-Based') &
                              population_group == 'General population')
d2 <- parameters %>% filter(parameter_type == 'IgG' &
                              (population_sample_type == 'Population-Based' |
                                 population_sample_type == 'Community-Based') &
                              population_group == 'General population')

