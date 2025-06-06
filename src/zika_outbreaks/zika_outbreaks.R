# Task to produce results for outbreaks 

library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(gpkg)
library(RSQLite)
library(ggrepel)
library(countrycode)
library(ggpattern)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("zika_prep_data", "latest(parameter:pathogen == this:pathogen &&
                   parameter:plotting == FALSE)",
                   c("articles_curated.rds", "outbreaks_curated.rds", "models_curated.rds", "parameters_curated.rds"))

##################
## DATA CURATION ##
###################
outbreaks <- readRDS('outbreaks_curated.rds')
articles <- readRDS('articles_curated.rds')
world <- ne_countries(scale = 'medium', returnclass = 'sf')

outbreaks <- merge(outbreaks, articles, by = "covidence_id") 
outbreaks <-   filter(outbreaks, !qa_score < 0.5)

outbreaks.WHO <- c(
  "Angola", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic",
  "Côte d’Ivoire", "Ethiopia", "Gabon", "Guinea-Bissau", "Kenya", "Nigeria", "Senegal", "Uganda",
  "Anguilla", "Antigua and Barbuda" , "Argentina", "Aruba", "Bahamas", "Barbados", "Belize", "Bolivia",
  "Bonaire", "Sint Eustatius", "Saba", "Brazil", "British Virgin Islands",
  "Cayman Islands", "Colombia", "Costa Rica", "Cuba", "Curaçao", "Dominica", "Dominican Republic", 
  "Ecuador", "El Salvador", "French Guiana", "Grenada", "Guadeloupe", "Guatemala",
  "Guyana", "Haiti", "Honduras", "Easter Island– Chile", "Jamaica", "Martinique", "Mexico",
  "Montserrat", "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto Rico", "Saint Barthélemy",
  "Saint Kitts", "Nevis", "Saint Lucia", "Saint Martin", "Saint Vincent", "Grenadines",
  "Saint Maarten", "Suriname", "Trinidad", "Tobago", "Turks", "Caicos", 
  "United States of America", "United States Virgin Islands", "Venezuela", 
  "Bangladesh", "India", "Indonesia", "Maldives", "Myanmar", "Thailand",
  "American Samoa", "Cambodia", "Cook Islands", "Fiji", "French Polynesia", 
  "Lao People’s Democratic Republic","Marshall Islands", "Malaysia", "Micronesia" ,
   "New Caledonia", "Palau", "Papua New Guinea", "Philippines", "Samoa", "Singapore",
  "Solomon Islands", "Tonga", "Vanuatu", "Vietnam", "France", "The Bahamas" 
)

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

out_sf <- out_sf %>%
  mutate(is_outbreak_who = admin %in% outbreaks.WHO)


out_plt <- ggplot() +
  # Layer based on n outbreaks
  geom_sf(data = out_sf, aes(fill = as.factor(n)), color = "gray50", lwd = 0.3) +
  
  # Layer for outbreak.WHO
  geom_sf_pattern(
    data = filter(out_sf, is_outbreak_who),
    aes(geometry = geometry),
    inherit.aes = FALSE,
    pattern = "stripe",
    fill = NA,
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    pattern_size = 0.01,
    color = NA
  ) +
  
  # Viridis scale
  scale_fill_viridis_d(na.value = 'grey90', guide = guide_legend(na.translate = FALSE)) +
  theme_void() +
  theme(axis.text = element_blank(),
        text = element_text(size = 14)) +
  labs(fill = 'Number of papers\nreporting outbreaks',
       x = '',
       y = '') +
  
  # labels
  geom_label_repel(data = out_sf %>% filter(!is.na(n)),
                   aes(x = label_x, y = label_y, label = paste0(admin, ": ", n)), 
                   fontface = "bold", size = 3,
                   max.overlaps = 50,
                   label.padding = 0.2) +
  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90))

ggsave("outbreaks_QA.png", out_plt, bg = 'white', height = 8, width = 14, dpi =300)
# ggsave("outbreaks.png", out_plt, bg = 'white', height = 8, width = 14, dpi =300)
# ggsave("sero_all.png", sero_all, bg = 'white')
# ggsave("sero_general.png", sero_gen, bg = 'white')


# Get information about outbreaks
# nrow(outbreaks)
# length(unique(outbreaks$covidence_id))
# length(unique(outbreaks$outbreak_country))
# 
# table(outbreaks$outbreak_country)
# outsumm <- as.data.frame(table(outbreaks$outbreak_country, outbreaks$covidence_id)) %>%
#   filter(Freq > 0) %>%
#   group_by(Var1) %>%
#   count()
# 
# outbreaks <- outbreaks %>%
#   mutate(continent = countrycode(sourcevar = outbreak_country,
#                                  origin = "country.name",
#                                  destination = "continent"))
# table(outbreaks$continent)
