# Code for map of seroprevalence overlaid with outbreaks over time (different panels) - 
# only Africa
# shapes for different assays

#' input: cleaned outbreaks dataset, cleaned parameter dataset, admin boundaries 

library(sf)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(stringr)

table(parameter_clean$population_country)
table(outbreak_clean$outbreak_country)

# Long dataset with all of the countries only for seroprevalence
param_clean <- parameter_clean %>%
  filter(parameter_class == 'Seroprevalence') 

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
  select(-c(c, reg))

# write.csv(param_clean, 'parameter_clean.csv', row.names = FALSE)



regions <- unlist(param_clean$population_location)
countries <- param_clean$population_country


