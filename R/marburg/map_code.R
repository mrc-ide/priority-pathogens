# Code for map of seroprevalence overlaid with outbreaks over time (different panels) - 
# only Africa
# shapes for different assays

#' input: cleaned outbreaks dataset, cleaned parameter dataset, admin boundaries 
REGENERATE_DATA <- FALSE

if(REGENERATE_DATA) source("R/marburg/script_marburg.R")              #only rerun this if we want to re-generate the marburg files (rather than rewriting this every time) 

library(sf)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(stringr)
library(ggplot2)

table(parameter_clean$population_country)
table(outbreak_clean$outbreak_country)

# Long dataset with all of the countries only for seroprevalence
param_clean <- parameter_clean %>%
  filter(parameter_class == 'Seroprevalence') %>%
  mutate(population_location = gsub(';',',', population_location),
         population_location = gsub(' &',',', population_location),
         population_location = gsub(' and',',', population_location),
         population_location = gsub('District', '', population_location),
         population_location = gsub('district', '', population_location),
         population_location = gsub('County', '', population_location)) 

param_split <- cSplit(param_clean, splitCols = c("population_country", 'population_location'), sep = ",", direction = "wide", drop = FALSE) %>%
  select(-population_country, -population_location) %>%
  pivot_longer(starts_with('population_country'),
               names_to = 'c',
               values_to = 'country') %>%
  filter(!is.na(country)) %>%
  pivot_longer(starts_with('population_location'),
               names_to = 'reg',
               values_to = 'region') %>%
  mutate(region = ifelse(!is.na(country) & is.na(region) & reg == 'population_location_1', 'Unknown', region)) %>%
  filter(!is.na(region)) %>%
  select(-c(c, reg)) %>%
  # remove duplicated regions that have sample sizes in them 
  filter(!grepl("[0-9]", region)) %>%
  mutate(region = ifelse(region == 'Pointe-Noire', 'Pointe Noire', region))

# write.csv(param_clean, 'parameter_clean.csv', row.names = FALSE)


# Pull in the shapefiles --------------------------------------------------
  # filter(COUNTRY %in% c('Germany', 'Switzerland', 'Austria', 'Belgium', 'Poland', 'Netherlands','Luxembourg', 'Liechtenstein','Czechia','Denmark', 'France'))
### get a europe shapefile!
# africa <- st_read('Z:/Shape files/GADM_2020/Africa_Boundaries.shp')
world <- st_read('data/gadm_410.gpkg')
africa_raw <- world %>%
  filter(CONTINENT=='Africa') %>%
  select(c(UID, GID_0, NAME_0, VARNAME_0, NAME_1, VARNAME_1, NAME_2, VARNAME_2, geom))
africa <- africa_raw %>%
  group_by(UID, GID_0, NAME_0, VARNAME_0, NAME_1, VARNAME_1) %>%
  summarise(geom = sf::st_union(geom)) %>%
  ungroup() %>% st_as_sf()
st_write(africa, 'data/Africa_adm1.shp')

# africa_adm0 <- st_read('Z:/Shape files/GADM_2020/SSA_shapefile_adm0.shp')
africa_adm1 <- st_read('Z:/Shape files/GADM_2020/SSA_shapefile_adm1.shp')

# plot(africa_adm0$geometry)
plot(africa$geom)
# plot(africa$geometry)
NAME_1 <- as.data.frame(unique(africa$NAME_1))
VARNAME_1 <- as.data.frame(unique(africa$VARNAME_1))
NAME_0 <- as.data.frame(unique(africa$NAME_0))

# Check if region is in list 
param_split <- param_split %>%
  rowwise() %>%
  mutate(in_region = ifelse(grepl(region, NAME_1), 1 , 0),
         in_varname = ifelse(grepl(region, VARNAME_1), 1, 0),
         in_country = ifelse(grepl(country, NAME_0), 1, 0),
         in_shp = ifelse(in_region == 1 | in_varname==1 | in_country ==1, 1, 0))
table(param_split$in_shp)
table(param_split$in_region)
table(param_split$in_varname)

# Join shp to parameter dataset -------------------------------------------
# need to join by country or region - some have country and don't have region 
# remove 'District' from names in param_split
param_sf <- africa %>%
  left_join(param_split, by = c('NAME_0' = 'country')) 
# check to make sure the regions are in the sf dataset and spelled teh same
regions <- unique(param_split$region)


# Map of Africa -----------------------------------------------------------
#could divide by parameter type? facet?
ggplot(param_sf ) +
  geom_sf(aes(fill = parameter_value)) +
  geom_sf(aes(group = NAME_0), color = 'black') +
  theme_void()

table(param_split$parameter_type)
