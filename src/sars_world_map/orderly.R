#task to create sars maps

library(tidyverse)
library(readxl)
library(harrypotter)
library(meta)
library(ggplot2)
library(grid)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggrepel)
library(patchwork)
library(gt)
library(png)
library(scales)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("db_compilation", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("world_cases_table.xlsx" = "world_cases_table.xlsx")
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")

orderly_artefact("sars-specific tables",c("figure_2_world_map.png","figure_2_world_map.pdf","sgp_info.png",
                                          "hkg_info.png","twn_info.png","can_info.png","chn_info.png","vnm_info.png",
                                          "sars_articles.csv", "sars_models.csv", "sars_parameters.csv"))

###################
## DATA CURATION ##
###################

articles   <- read_csv("articles.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("parameters.csv")

# to save down for epireview use plotting = FALSE
dfs <- data_curation(articles,tibble(),models,parameters, plotting =  FALSE )

articles   <- dfs$articles
models     <- dfs$models
parameters <- dfs$parameters

write_csv(articles,'sars_articles.csv')
write_csv(models,'sars_models.csv')
write_csv(parameters,'sars_parameters.csv')

dfs <- data_curation(articles,tibble(),models,parameters, plotting =  TRUE )

articles   <- dfs$articles
models     <- dfs$models
parameters <- dfs$parameters

create_inset_png <- function(data=non_imported_cnts_short,location='SGP')
{
  location_label_map <- list(SGP='Singapore',
                             HKG='Hong Kong',
                             CAN='Canada',
                             CHN='China',
                             TWN='Taiwan',
                             VNM='Vietnam')
  
  loc_in <- location_label_map[[location]]
  
  LOC <- data %>% filter(ISO3==location) %>% 
    dplyr::select(-ISO3) %>%
    mutate(onset_first_probable_case = format(as.Date(onset_first_probable_case),format="%d-%b-%y"),
           onset_last_probable_case  = format(as.Date(onset_last_probable_case),format="%d-%b-%y")) %>%
    rename('Onset 1st case'=onset_first_probable_case,
           'Onset last case'=onset_last_probable_case,
           'Imported %'=pct_imported,'CFR %'=CFR) 
  
  LOC <- as_tibble(cbind(loc_name = names(LOC), t(LOC))) %>% 
    rename(' '=V2,!!loc_in:=loc_name) %>%
    gt() %>%
    cols_label(!!loc_in:= md(paste0('_**',loc_in ,'**_'))) %>%
    tab_options(data_row.padding = px(1)) %>%
    tab_style(
      style = list(cell_borders(sides = "all", color = "white", style = "solid", weight = px(1))),
      location = cells_body()             
    ) %>%
    tab_style(
      style = list( cell_text(weight = "bold")),
      locations = cells_body( columns = !!loc_in ) ) %>%
    gtsave(paste0(tolower(location),'_info.png'))
}

world               <- ne_countries(scale = "medium", returnclass = "sf")
sars_data           <- read_excel("world_cases_table.xlsx") %>% filter(!is.na(ISO3)) %>% rename(`Total Cases`=Total)
non_imported_cnts   <- sars_data %>% filter(pct_imported < .5 & ISO3 != 'RUS') 
non_imported_cnts$Median_age[is.na(non_imported_cnts$Median_age)] <- '-'

f <- data.frame(map_point = c("Beijing",
                              "Guangzhou",
                              "Singapore",
                              "Toronto"),
                latitude  = c(39.9042,23.1291,1.290270,43.6532),
                longitude = c(116.4074,113.2644,103.851959,-79.3832),
                type      = c("Outbreak Location","Outbreak Location","Outbreak Location","Outbreak Location"))

world      <- world %>% left_join(sars_data,by=c('iso_a3'='ISO3'))
world_local_transmission <- world %>% filter(iso_a3 %in% non_imported_cnts$ISO3)

world_map <- ggplot() + 
  geom_sf(data = world, lwd = 0.3, col = "grey70", aes(fill = `Total Cases`)) +
  geom_sf(data = world_local_transmission, lwd = 1, col = "darkred",  fill = NA) +
  geom_point(data = f, aes(x = longitude, y = latitude, color = "Outbreak Location"), shape = 8, size = 1.5, stroke = 1.5) +
  theme_light() +
  coord_sf(xlim=c(-180,180), ylim=c(-60,90)) +    # cut out antarctica
  scale_color_manual(values = c("Outbreak Location" = "green2")) + 
  geom_label_repel(data = world_local_transmission,aes(x=label_x,y=label_y,label=name),size = 2, fontface = "bold") +
  scale_fill_hp(option = "NewtScamander",na.value='grey90',trans = 'log',labels = scales::label_comma(accuracy = 1)) +
  scale_alpha_continuous(trans='log1p', limits=c(1,6000)) +
  theme(panel.grid.minor = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.3), panel.background = element_rect(fill = "aliceblue"),
        legend.position = 'right') +
  ylab('Latitude') + xlab('Longitude') + 
  labs(colour="")

# use inset_elements for location specific data we want to show for 6 outbreak regions with local transmission
non_imported_cnts_short <- non_imported_cnts %>% 
  dplyr::select(ISO3,`Total Cases`,Median_age,CFR,pct_imported,onset_first_probable_case,onset_last_probable_case) %>%
  mutate(CFR=round(CFR*100,2),
         pct_imported = round(pct_imported*100,2))

for(iso3 in non_imported_cnts_short$ISO3)
  create_inset_png(non_imported_cnts_short,iso3)

non_imported_cnts <- non_imported_cnts %>% 
  mutate(cfr_ifr_denominator = (n_recovered+n_currently_hospitalised+n_deaths),
         cfr_ifr_numerator   = n_deaths,
         refs                = Country,
         parameter_value     = CFR,
         parameter_unit      = 'Percentage') %>% arrange(desc(CFR))

m1 <- metaprop_wrap(non_imported_cnts, subgroup = NA,  
                    plot_pooled = TRUE, sort_by_subg = FALSE, plot_study = TRUE, digits = 3, colour = 'red', 
                    width = 4000, height = 1650, resolution = 500,
                    at = seq(0,0.3,by=0.05), xlim = c(0,0.35))

inset_width <- 0.05*2.2
inset_hight <- 0.125*1.75
test <- world_map + patchwork::inset_element(grid::rasterGrob(png::readPNG("sgp_info.png")), 0.725,0.15,0.725+inset_width,.15+inset_hight) +
  patchwork::inset_element(grid::rasterGrob(png::readPNG("hkg_info.png")), 0.85,0.275,0.85+inset_width,.275+inset_hight) +
  patchwork::inset_element(grid::rasterGrob(png::readPNG("twn_info.png")), 0.85,0.5,0.85+inset_width,.5+inset_hight) + 
  patchwork::inset_element(grid::rasterGrob(png::readPNG("chn_info.png")), 0.75,0.725,0.75+inset_width,0.725+inset_hight) + 
  patchwork::inset_element(grid::rasterGrob(png::readPNG("can_info.png")), 0.075,0.5,0.075+inset_width,.5+inset_hight) + 
  patchwork::inset_element(grid::rasterGrob(png::readPNG("vnm_info.png")), 0.6,0.225,0.6+inset_width,.225+inset_hight) + 
  patchwork::inset_element(m1$plot, 0.01,0.01,0.425,0.375) 

ggsave("figure_2_world_map.png", plot = test, width = 18, height = 12)
ggsave("figure_2_world_map.pdf", plot = test, width = 18, height = 12)