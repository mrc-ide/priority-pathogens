# Task to produce results for outbreaks 

library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(gpkg)
library(RSQLite)
library(ggrepel)
library(countrycode)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("zika_prep_data", "latest(parameter:pathogen == this:pathogen &&
                   parameter:plotting == FALSE)",
                   c("articles_curated.rds", "outbreaks_curated.rds", "models_curated.rds", "parameters_curated.rds"))

##################
## DATA CURATION ##
###################

articles   <- readRDS("articles_curated.rds")
models     <- readRDS("models_curated.rds")
parameters <- readRDS("parameters_curated.rds")
outbreaks <- readRDS('outbreaks_curated.rds')

dfs <- data_curation(articles,tibble(),models,parameters, plotting = FALSE )

articles   <- dfs$articles
models     <- dfs$models
parameters <- dfs$parameters


world <- ne_countries(scale = 'medium', returnclass = 'sf')
# 
# world <- geopackage('data/gadm_410-levels.gpkg')
# 

outbreaks <- merge(outbreaks, articles, by = "covidence_id") 
# outbreaks <-   filter(outbreaks, !qa_score < 0.5)

outbreaks_agg <- outbreaks %>%
  mutate(outbreak_country = case_when(
    outbreak_country == 'France (Martinique)' ~ "France",
    TRUE ~ outbreak_country
  )) %>%
  group_by(covidence_id, outbreak_country) %>%
  count() %>%
  group_by(outbreak_country) %>%
  count()

out_sf <- world %>%
  left_join(outbreaks_agg, by = c('admin' = 'outbreak_country')) %>%# by.y ="outbreak_country", by.x = 'sovereignt')
  mutate(admin = case_when(
    admin == 'Federated States of Micronesia' ~ "Micronesia",
    TRUE ~ admin
  ))
# colour = "gray50", fill = "gray90"
out_plt <- ggplot() +
  geom_sf(data = out_sf, lwd = 0.3, col = "gray50", aes(fill = as.factor(n))) +
  scale_fill_viridis_d( na.value = 'grey90') +
  theme_void() +
  theme(axis.text = element_blank(),
        text = element_text(size = 14)) +
  labs(fill = 'Number of papers\nreporting outbreaks',
       x = '',
       y = '') +
  geom_label_repel(data = out_sf %>% filter(!is.na(n)),
                   aes(x=label_x,y=label_y,label=paste0(admin,": ",n), 
                       fontface = "bold"), size = 3,
                   max.overlaps = 50,
                   label.padding = 0.2) +
  coord_sf(xlim=c(-180,180), ylim=c(-60,90))     # cut out antarctica

out_plt

# Sero plot 
sero_all <- parameters %>%
  filter(parameter_type =='Seroprevalence - IgG' | grepl("PRNT", parameter_type))%>%
  group_by(covidence_id, population_country) %>%
  count() %>%
  group_by(population_country) %>%
  count() %>%
  mutate(population_country = case_when(
    population_country == 'DRC' ~ "Democratic Republic of the Congo",
    TRUE ~ population_country
  ))

sero_all_sf <- world %>%
  left_join(sero_all, by = c('admin' = 'population_country')) %>%# by.y ="outbreak_country", by.x = 'sovereignt')
  mutate(admin = case_when(
    admin == 'Federated States of Micronesia' ~ "Micronesia",
    admin == 'Democratic Republic of the Congo' ~ "DRC",
    TRUE ~ admin
  ))

sero_all <- ggplot() +
  geom_sf(data = sero_all_sf, lwd = 0.3, col = "grey30", aes(fill = n)) +
  scale_fill_viridis_c(option = 'plasma', direction = -1, na.value = 'grey80') +
  theme_light() +
  labs(fill = 'Number of papers\nreporting IgG or PRNT',
       x = '',
       y = '') +
  theme(axis.text = element_blank()) +
  coord_sf(xlim=c(-180,180), ylim=c(-60,90))


# general pop only 
sero_gen <- parameters %>%
  filter(parameter_type =='Seroprevalence - IgG' | grepl("PRNT", parameter_type))%>%
  filter(population_group == 'General population' & population_sample_type == "Population based") %>%
  group_by(covidence_id, population_country) %>%
  count() %>%
  group_by(population_country) %>%
  count() %>%
  mutate(population_country = case_when(
    population_country == 'DRC' ~ "Democratic Republic of the Congo",
    TRUE ~ population_country
  ))

sero_gen_sf <- world %>%
  left_join(sero_gen, by = c('admin' = 'population_country')) %>%# by.y ="outbreak_country", by.x = 'sovereignt')
  mutate(admin = case_when(
    admin == 'Federated States of Micronesia' ~ "Micronesia",
    admin == 'Democratic Republic of the Congo' ~ "DRC",
    TRUE ~ admin
  ))

sero_gen <- ggplot() +
  geom_sf(data = sero_gen_sf, lwd = 0.3, col = "grey30", aes(fill = n)) +
  scale_fill_viridis_c(option = 'plasma', direction = -1, na.value = 'grey80') +
  theme_light() +
  labs(fill = 'Number of papers\nreporting IgG or PRNT',
       x = '',
       y = '') +
  theme(axis.text = element_blank()) +
  coord_sf(xlim=c(-180,180), ylim=c(-60,90))


ggsave("outbreaks.svg", out_plt, bg = 'white', height = 8, width = 14, dpi =300)
ggsave("outbreaks.png", out_plt, bg = 'white', height = 8, width = 14, dpi =300)
ggsave("sero_all.png", sero_all, bg = 'white')
ggsave("sero_general.png", sero_gen, bg = 'white')


# Get information about outbreaks
nrow(outbreaks)
length(unique(outbreaks$covidence_id))
length(unique(outbreaks$outbreak_country))

table(outbreaks$outbreak_country)
outsumm <- as.data.frame(table(outbreaks$outbreak_country, outbreaks$covidence_id)) %>%
  filter(Freq > 0) %>%
  group_by(Var1) %>%
  count()

outbreaks <- outbreaks %>%
  mutate(continent = countrycode(sourcevar = outbreak_country,
                                 origin = "country.name",
                                 destination = "continent"))
table(outbreaks$continent)
