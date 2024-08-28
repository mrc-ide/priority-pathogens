#task to plot lassa seroprevalence figures

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

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("db_compilation", "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "outbreaks.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("Africa_Boundaries-shp" = "Africa_Boundaries-shp",
                        "wca_admbnda_adm0_ocha" = "wca_admbnda_adm0_ocha",
                        "wca_admbnda_adm1_ocha" = "wca_admbnda_adm1_ocha")
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
orderly_artefact("lassa-specific figures",c("figure_2.png","figure_2.pdf"))

###################
## DATA CURATION ##
###################

articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("parameters.csv")

dfs <- curation(articles,outbreaks,models,parameters, plotting = TRUE)

articles   <- dfs$articles
outbreaks  <- dfs$outbreaks
models     <- dfs$models
parameters <- dfs$parameters

####################
## SEROPREVALENCE ##
####################

#prepare shapefiles for maps
l0 <- read_sf('Africa_Boundaries-shp/Africa_Boundaries.shp') %>% #this is the shapefile with country boundaries
      rename(COUNTRY = NAME_0) #store country names in column COUNTRY
#
l0 <- l0 %>% filter(!(COUNTRY %in% c("Mali","Burkina Faso","Nigeria","Cameroon")))
lt <- read_sf('wca_admbnda_adm0_ocha/wca_admbnda_adm0_ocha.shp') %>%
      rename(COUNTRY = admin0Name) %>% 
      filter(COUNTRY %in% c("Mali","Burkina Faso","Nigeria","Cameroon"))
l0 <- bind_rows(l0,lt) %>%
      mutate(geometry = st_make_valid(geometry)) #seems to be broken
#
l1 <- read_sf('wca_admbnda_adm1_ocha/wca_admbnda_adm1_ocha.shp') %>% #this is the shapefile with level 1 regions
      rename(COUNTRY = admin0Name) %>% 
      mutate(COUNTRY = case_when( #country names must be consistent between shapefiles
             COUNTRY == "Cabo Verde" ~ "Cape Verde",
             COUNTRY == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
             COUNTRY == "Guinea Bissau" ~ "Guinea-Bissau",
             TRUE ~ COUNTRY)) %>%
      rename(REG_CODE = admin1Pcod) #store region codes, e.g. SL01, in column REG_CODE 

#extract data to be plotted
d1 <- parameters %>% filter(parameter_class == 'Seroprevalence' & 
                            parameter_type != 'Seroprevalence - IgG' &
                            parameter_type != 'Seroprevalence - IgM' &
                           (population_sample_type == 'Population based' |
                            population_sample_type == 'Community based') &
                            population_group == 'General population')
d2 <- parameters %>% filter(parameter_type == 'Seroprevalence - IgG' &
                           (population_sample_type == 'Population based' |
                            population_sample_type == 'Community based') &
                            population_group == 'General population')

#specify outbreak positions
f1 <- data.frame(map_point = c("Zorzor Liberia 1972, 1980-1982",
                               "Onitsha Nigeria 1974","Jos Nigeria 1969-1970",
                               "Panguma-Tongo Sierra Leone 1972"),
                 latitude  = c(7.775914,6.134231,9.889000,8.658131),
                 longitude = c(-9.432665,6.803424,8.860781,-11.063004),
                 type      = c("Outbreak","Outbreak","Outbreak","Outbreak"))
f2 <- data.frame(map_point = c("Tchaourou Benin 2015-2016","Tanguieta Benin 2014",
                               "Suakoko Liberia 2016",
                               "Edo-Ondo Nigeria 2015-2016, 2018, 2019","Ebonyi Nigeria 2012, 2018, 2019","Bauchi-Plateau-Taraba Nigeria 2015-2016, 2018",
                               "Tonkolili Sierra Leone 2019","Kenema Sierra Leone 1997, 2010-2012","Kailahun Sierra Leone 1997"),
                 latitude  = c(8.888718,10.628637,6.985884,6.703927,6.054975,9.484048,8.748332,7.873899,8.277113),
                 longitude = c(2.584898,1.260810,-9.579024,5.572952,7.874588,10.530532,-11.836672,-11.184657,-10.566351),
                 type      = c("Outbreak","Outbreak","Outbreak","Outbreak","Outbreak","Outbreak","Outbreak","Outbreak","Outbreak"))

#specify country names and positions
n1 <- data.frame(latitude  = c(19.1,18.1,17.1,15.1,15.0,12.5,
                               1.0,-3.4,0.3,5.0,7.9,7.9,
                               0,10.3,4.7,2.9,10.9,13.4,
                               11,6.5,4.2), 
                 longitude = c(-10.9,-1.5,9.1,18.7,-14.8,-1.6,
                               24.5,6.1,6,12.0,-5.6,-1,
                               16,2.3,1.6,3.5,-11.7,-19,
                               -18.9,-15.5,-11.9), 
                 c_name    = c("Mauritania","Mali","Niger","Chad","Senegal","Burkina Faso",
                               "Central\nAfrican\nRepublic","Gabon","Equatorial\nGuinea","Cameroon","Côte d'Ivoire","Ghana",
                               "Republic\nof Congo","Benin","Togo","Nigeria","Guinea","Gambia",
                               "Guinea\n-Bissau","Sierra\nLeone","Liberia"))
n2 <- data.frame(latitude  = c(16.4,
                               3.8,4.25,
                               5.2,3.8,9.0,
                               7.0,5), 
                 longitude = c(-11.5,
                               -4.5,-0.5,
                               2.3,4.5,-15.5,
                               -14.5,-11.5), 
                 c_name    = c("Mali",
                               "Côte d'Ivoire","Ghana",
                               "Benin","Nigeria","Guinea",
                               "Sierra\nLeone","Liberia"))

#specify position of lines pointing to countries (added to map after function call)
ll1 <- data.frame(lat1 = c(2.50,-3.2,1.0,5.1,3.3,13.4,11.500,7.200,4.5),
                  lon1 = c(23.5,7.30,7.6,1.6,3.7,-17.8,-17.7,-14.5,-11),
                  lat2 = c(3.80,-2.5,1.7,6.0,4.5,13.4,11.700,7.900,5.5),
                  lon2 = c(22.5,9.20,9.4,1.4,5.3,-17,-16.500,-13.2,-10))
ll2 <- data.frame(lat1 = c( 16.1,  9.1,   7.45, 5.1, 4.1, 4.5, 5.4, 4),
                  lon1 = c(-11.4,-14.75,-13.9,-10.8,-4.5,-0.5, 2.3, 4.5),
                  lat2 = c( 15.3,  9.7,   7.9,  5.7, 5,   5.1, 6.2, 4.8),
                  lon2 = c(-10.9,-14.0, -13.2,-10.2,-4.9,-0.8, 2.2, 5.3))

#call mapping function
p1 <- map_generic(l0,l1,d1,f1,n1,range_mp=TRUE,summ_dups="mean",c(-23.2,25.1),c(-3.9,22.25),c(0,60),'magma','')
p1 <- p1 + geom_rect(aes(xmin = -18.8, xmax = 14.75, ymin = 2, ymax = 16.75), linetype = 2, color = "grey40", fill = NA)
p1 <- p1 + geom_segment(data = ll1, aes(x = lon1, xend = lon2, y = lat1, yend = lat2), size = 0.5, color = "black")
p2 <- map_generic(l0,l1,d2,f2,n2,range_mp=TRUE,summ_dups="mean",c(-18.5,13.13),c(2.75,16.1),c(0,60),'magma','')
p2 <- p2 + geom_segment(data = ll2, aes(x = lon1, xend = lon2, y = lat1, yend = lat2), size = 0.5, color = "black")

patchwork <- p1 / p2
patchwork <- patchwork + plot_layout(heights = c(1,0.75)) + plot_annotation(tag_levels = 'A')
ggsave("figure_2.png", plot = patchwork, width = 12, height = 14)
ggsave("figure_2.pdf", plot = patchwork, width = 12, height = 14)