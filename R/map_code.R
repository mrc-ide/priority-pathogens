# Code for map of seroprevalence overlaid with outbreaks over time (different panels) - 
# only Africa
# shapes for different assays

#' input: cleaned outbreaks dataset, cleaned parameter dataset, admin boundaries 

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
  filter(!grepl("[0-9]", region)) %>%
  mutate(region = ifelse(region == 'Pointe-Noire','Pointe Noire', region))

# write.csv(param_clean, 'parameter_clean.csv', row.names = FALSE)


# Pull in the shapefiles --------------------------------------------------
  # filter(COUNTRY %in% c('Germany', 'Switzerland', 'Austria', 'Belgium', 'Poland', 'Netherlands','Luxembourg', 'Liechtenstein','Czechia','Denmark', 'France'))
### get a europe shapefile!
# africa <- st_read('Z:/Shape files/GADM_2020/Africa_Boundaries.shp')
# africa_adm0 <- st_read('Z:/Shape files/GADM_2020/SSA_shapefile_adm0.shp')
africa_adm1 <- st_read('Z:/Shape files/GADM_2020/SSA_shapefile_adm1.shp')

# plot(africa_adm0$geometry)
plot(africa_adm1$geometry)
# plot(africa$geometry)

# Check if region is in list 
param_split <- param_split %>%
  mutate(in_geom = ifelse(str_detect(region, regs) | str_detect(region, VARNAME_1), 1 , 0))


# Join shp to parameter dataset -------------------------------------------
# need to join by country or region - some have country and don't have region 
# remove 'District' from names in param_split
param_sf <- africa_adm1 %>%
  left_join(param_split, by = c('NAME_0' = 'country', 'NAME_1' = 'region')) 
# check to make sure the regions are in the sf dataset and spelled teh same
regions <- unique(param_split$region)
regs <- as.data.frame(unique(africa_adm1$NAME_1))

# Map of Africa -----------------------------------------------------------
#could divide by parameter type? facet?
ggplot(param_sf ) +
  geom_sf(aes(fill = parameter_value)) +
  theme_void()

table(param_split$parameter_type)
