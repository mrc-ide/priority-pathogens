library(ggplot2)
library(sf)
library(rnaturalearth)
library(gpkg)
library(RSQLite)
library(ggrepel)

parameters <- readRDS("P:/Zika/priority-pathogens/archive/db_compilation/20250205-144757-57dcf554/parameters.rds")
outbreaks <- readRDS("P:/Zika/priority-pathogens/archive/db_compilation/20250205-144757-57dcf554/outbreaks.rds")

world <- ne_countries(scale = 'medium', returnclass = 'sf')
# 
# world <- geopackage('data/gadm_410-levels.gpkg')
# 
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

out_plt <- ggplot() +
  geom_sf(data = out_sf, lwd = 0.3, col = "grey30", aes(fill = n)) +
  scale_fill_viridis_c(option = 'plasma', direction = -1, na.value = 'grey80') +
  theme_light() +
  theme(axis.text = element_blank()) +
  labs(fill = 'Number of papers\nreporting outbreaks',
       x = '',
       y = '') +
  geom_label_repel(data = out_sf %>% filter(!is.na(n)),
                   aes(x=label_x,y=label_y,label=paste0(admin,": ",n), 
                   fontface = "bold"), size = 2,
                   max.overlaps = 50,
                   label.padding = 0.2) +
  coord_sf(xlim=c(-180,180), ylim=c(-60,90))     # cut out antarctica
  
  

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


ggsave("data/zika_plots/outbreaks.png", out_plt)
ggsave("data/zika_plots/sero_all.png", sero_all)
ggsave("data/zika_plots/sero_general.png", sero_gen)
