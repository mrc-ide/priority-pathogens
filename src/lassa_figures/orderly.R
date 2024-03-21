#task to plot lassa-specific figures

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
orderly_parameters(pathogen = "LASSA")
orderly_dependency("db_compilation", "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "outbreaks.csv", "models.csv", "parameters.csv"))
orderly_shared_resource("Africa_Boundaries-shp" = "Africa_Boundaries-shp",
                        "wca_admbnda_adm0_ocha" = "wca_admbnda_adm0_ocha",
                        "wca_admbnda_adm1_ocha" = "wca_admbnda_adm1_ocha",
                        "rivers_africa_37333"   = "rivers_africa_37333")
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
orderly_artefact("lassa-specific figures",
  c("figure_2.png","figure_3.png","figure_4.png","figure_5.png","figure_s1.png",
    "figure_s2.png","figure_s3.png","figure_s4.png","figure_s5.png","figure_s6.png",
    "figure_s7.png","figure_s8.png","figure_s9.png","figure_s10.png","figure_s11.png"))

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
n2 <- data.frame(latitude = numeric(0), longitude = numeric(0), c_name = character(0))

#specify position of lines pointing to countries (added to map after function call)
ll <- data.frame(lat1 = c(2.50,-3.2,1.0,5.1,3.3,13.4,11.500,7.200,4.5),
                 lon1 = c(23.5,7.30,7.6,1.6,3.7,-17.8,-17.7,-14.5,-11),
                 lat2 = c(3.80,-2.5,1.7,6.0,4.5,13.4,11.700,7.900,5.5),
                 lon2 = c(22.5,9.20,9.4,1.4,5.3,-17,-16.500,-13.2,-10))

#call mapping function
p1 <- map_generic(l0,l1,d1,f1,n1,range_mp=TRUE,summ_dups="mean",c(-23.2,25.1),c(-3.9,22.25),c(0,60),'magma','')
p1 <- p1 + geom_rect(aes(xmin = -18.8, xmax = 14.75, ymin = 2, ymax = 16.75), linetype = 2, color = "grey40", fill = NA)
p1 <- p1 + geom_segment(data = ll, aes(x = lon1, xend = lon2, y = lat1, yend = lat2), size = 0.5, color = "black")
p2 <- map_generic(l0,l1,d2,f2,n2,range_mp=TRUE,summ_dups="mean",c(-18.5,13.13),c(2.75,16.1),c(0,60),'magma','')

patchwork <- p1 / p2
patchwork <- patchwork + plot_layout(heights = c(1,0.75)) + plot_annotation(tag_levels = 'A')
ggsave("figure_2.png", plot = patchwork, width = 12, height = 14)

##############
## SEVERITY ##
##############

#extract data and define columns for meta-analysis
d1 <- parameters %>% filter(parameter_type == 'Severity - case fatality rate (CFR)') %>%
                     mutate(case_def = case_when(
                            covidence_id %in% c(18,88,167,174,307,461,645,845,870,873,874,920,921,1080,1173,1181,1254,1272,1368,1413,1426,1444,2567,2579,2585,2589,2636,2651,2662,2684,2818,3147,3215,3530,3635,3716,3841,3991,4314,4727) ~ "Lab-Confirmed",
                            covidence_id %in% c(252,1433,2714,4745) ~ "Clinically-Diagnosed",
                            covidence_id %in% c(1328) ~ "Probable",
                            covidence_id %in% c(832,2617) ~ "Suspected",
                            covidence_id %in% c(416,670) ~ "Lab-Confirmed;Clinically-Diagnosed",
                            covidence_id %in% c(1183,2611,2656,2760,3634) ~ "Lab-Confirmed;Probable",
                            covidence_id %in% c(854,871,1033,1447) ~ "Lab-Confirmed;Suspected")) %>%
                     mutate(lineage = case_when(
                            population_country == "Nigeria" & population_location %in% c("Aboh Mbaise; Aba; Owerri","Ebonyi","Ebonyi State","Edo North; Central Senatorial District Of Edo State","Edo State","Ondo State","Irrua","Irrua; Edo State")~ "Lineage II Region - Nigeria",
                            population_country == "Nigeria" & population_location %in% c("Bauchi State","Jos","Plateau State") ~ "Lineage III Region - Nigeria",  
                            population_country %in% c("Guinea","Liberia","Sierra Leone","Guinea;Liberia;Sierra Leone") ~ "Lineage IV Region - Guinea, Liberia, Sierra Leone",
                            population_country %in% c("Benin","Togo") ~ "Lineage VII Region - Benin,Togo",
                            TRUE ~ "Unspecified - Nigeria")) %>%
                     mutate(study_midyear = ifelse(!is.na(population_study_start_year) & !is.na(population_study_end_year),
                                                   round((population_study_start_year + population_study_end_year) / 2),
                                                   population_study_start_year)) %>%
                     mutate(study_midyear_cat = case_when(
                            study_midyear %in% 1970:1979 ~ "1970-1979",
                            study_midyear %in% 1980:1989 ~ "1980-1989",
                            study_midyear %in% 1990:1999 ~ "1990-1999",
                            study_midyear %in% 2000:2009 ~ "2000-2009",
                            study_midyear %in% 2010:2019 ~ "2010-2019",
                            study_midyear %in% 2020:2029 ~ "2020-Present",
                            TRUE ~ "Unspecified")) %>%
                     mutate(cfr_denom_cat = case_when(
                            cfr_ifr_denominator %in% 1:29      ~ "Reported Cases < 30",
                            cfr_ifr_denominator %in% 30:99     ~ "Reported Cases = 30-99",
                            cfr_ifr_denominator %in% 100:299   ~ "Reported Cases = 100-299",
                            cfr_ifr_denominator %in% 300:999   ~ "Reported Cases = 300-999",
                            cfr_ifr_denominator %in% 1000:9999 ~ "Reported Cases >= 1000",
                            TRUE ~ "Unspecified")) %>%
                     mutate(population_group = case_when(
                            population_group == "Persons under investigation" ~ "Persons Under Investigation",
                            population_group == "Pregnant women" ~ "Pregnant Women",
                            population_group == "Children" ~ population_group,
                            TRUE ~ "Mixed Groups")) %>%
                     mutate(duplicate_cfr = case_when(
                            covidence_id %in% c(832,845,870) & population_group != "Persons Under Investigation" ~ "Known",
                            covidence_id == 1413 & cfr_ifr_method == "Naive" ~ "Known",
                            covidence_id %in% c(645,4745,870,871,1426,1413,1444,3147,2714,461,2818,1272,167,2567, 
                                                2760,2656,4314,2589,3215,3991,2662,3635,874,920,2636,252,3530) ~ "Assumed",
                            TRUE ~ "False"))

#meta-analysis with strict de-duplication
da <- d1 %>% filter(duplicate_cfr == "False")

m1 <- metaprop_wrap(dataframe = da, subgroup = "lineage", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = "red", 
                    width = 4000, height = 4000, resolution = 500)
m2 <- metaprop_wrap(dataframe = da, subgroup = "population_country", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = "red", 
                    width = 4000, height = 4000, resolution = 500)
m3 <- metaprop_wrap(dataframe = da %>% arrange(cfr_ifr_denominator), subgroup = "cfr_denom_cat", 
                    plot_pooled = TRUE, sort_by_subg = FALSE, plot_study = FALSE, digits = 3, colour = "red", 
                    width = 4000, height = 4000, resolution = 500)
m4 <- metaprop_wrap(dataframe = da, subgroup = "population_group", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = "red", 
                    width = 4000, height = 4000, resolution = 500)

p1 <- m1$plot
p2 <- m2$plot
p3 <- m3$plot
p4 <- m4$plot

#lineage region map
l0 <- read_sf('Africa_Boundaries-shp/Africa_Boundaries.shp') %>% 
      rename(COUNTRY = NAME_0) 
l0 <- l0 %>% filter(!(COUNTRY %in% c("Mali","Burkina Faso","Nigeria","Cameroon")))
lt <- read_sf('wca_admbnda_adm0_ocha/wca_admbnda_adm0_ocha.shp') %>%
      rename(COUNTRY = admin0Name) %>% 
      filter(COUNTRY %in% c("Mali","Burkina Faso","Nigeria","Cameroon"))
l0 <- bind_rows(l0,lt) %>%
      mutate(geometry = st_make_valid(geometry))
lr <- read_sf('rivers_africa_37333/rivers_africa_37333.shp') %>%
      filter((MAJ_NAME == "Niger" & Strahler >= 5)|
             (MAJ_NAME == "Lake Chad" & Strahler >= 5)|
             (MAJ_NAME == "Senegal" & Strahler >= 5)|
             (MAJ_NAME == "Volta" & Strahler >= 5)|
             (MAJ_NAME == "Gulf of Guinea" & Strahler >= 5))

lz <- data.frame(label     = c("I","II","III","IV","V","VI","VII"),
                 latitude  = c(10.682579,6.221933,10.142249,8.868797,10.872315,7.689613,9.005704),  
                 longitude = c(13.266454,6.658358,9.162117,-10.338539,-6.819674,4.618568,1.666451),
                 major_rad = c(0.45,2.0,2.2,3.75,1.8,0.45,2.25),
                 minor_rad = c(0.45,1.5,1.6,2.25,1.1,0.45,0.90),
                 rot       = c(0,-pi/5,0,-pi/4,-pi/2.5,0,-pi/2))

n6 <- data.frame(latitude  = c(14,7.9,5.2,4.7,2.9,9,6.5,4.2), 
                 longitude = c(-8.5,-5.3,3.5,1,3.5,-17,-15.5,-11.9), 
                 c_name    = c("Mali","Côte d'Ivoire","Benin","Togo","Nigeria","Guinea","Sierra\nLeone","Liberia"))
r6 <- data.frame(latitude  = c(11,7.8,7.2), 
                 longitude = c(5.8,11,-1.2), 
                 c_name    = c("Niger\nRiver","Benue\nRiver","Volta\nRiver"))
ll <- data.frame(lat1 = c(5,   3.3, 7.20, 4.5, 9.1, 5.5),
                 lon1 = c(1,   3.7,-14.5,-11, -15.9,2.9),
                 lat2 = c(6.1, 4.5, 7.90, 5.5, 9.9, 6.2),
                 lon2 = c(1.45,5.3,-13.2,-10, -14.3,2.3))

long_lim <- c(-18.5,13.13)
lat_lim  <- c(2.75,16.1)
#title    <- 'LASV Lineage Zones, West Africa'

p5 <- ggplot() +
      geom_sf(data = l0, lwd = 0.3, col = "grey40",  fill = "grey90") +
      geom_sf(data = lr, lwd = 0.7, col = "blue3") +
      geom_ellipse(data = lz, aes(x0 = longitude, y0 = latitude, a = major_rad, b = minor_rad, angle = rot),
                   col = NA, fill = "red", alpha = 0.6) + 
      geom_text(data = lz, aes(x = longitude, y = latitude, label = label), size = 5, fontface='bold', color = "white") +
      geom_text(data = n6, aes(x = longitude, y = latitude, label = c_name), size = 3.4, fontface = 'italic', color = "black") +
      geom_text(data = r6, aes(x = longitude, y = latitude, label = c_name), size = 3.4, fontface = 'italic', color = "blue3") +
      geom_segment(data = ll, aes(x = lon1, xend = lon2, y = lat1, yend = lat2), size = 0.5, color = "black") +
      coord_sf(xlim = long_lim, ylim = lat_lim) +
      theme_void() + 
      annotation_scale(location = "bl", width_hint = 0.2) +
      annotation_north_arrow(location = "bl", which_north = "true", pad_y = unit(0.4, "in"), style = north_arrow_fancy_orienteering) +
      annotation_custom(grob = rectGrob(gp = gpar(col = "black", fill = NA, lwd = 3)), 
                        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) #+
    #ggtitle(title)

patchwork <- ((p1 | plot_spacer() | p5 | plot_spacer()) + plot_layout(ncol = 4, widths = c(1,0.5,1.3,0.2))) /
             ((p2 | p3 | p4) + plot_layout(ncol = 3))
patchwork <- patchwork + plot_annotation(tag_levels = 'A') 
ggsave("figure_3.png", plot = patchwork, width = 18, height = 12)

#figure_S5-S9: meta-analysis with all estimates plotted
m1 <- metaprop_wrap(dataframe = da %>% arrange(-central), subgroup = "lineage", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = TRUE, digits = 3, colour = "red", 
                    width = 1200, height = 1600, resolution = 115)
m2 <- metaprop_wrap(dataframe = da %>% arrange(-central), subgroup = "population_country", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = TRUE, digits = 3, colour = "red", 
                    width = 1200, height = 1600, resolution = 115)
m3 <- metaprop_wrap(dataframe = da %>% arrange(-central), subgroup = "study_midyear_cat", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = TRUE, digits = 3, colour = "red", 
                    width = 1200, height = 1600, resolution = 115)
m4 <- metaprop_wrap(dataframe = da %>% arrange(factor(cfr_denom_cat,levels=c("Reported Cases < 30","Reported Cases = 30-99","Reported Cases = 100-299","Reported Cases = 300-999","Reported Cases >= 1000","Unspecified")),-central), subgroup = "cfr_denom_cat", 
                    plot_pooled = TRUE, sort_by_subg = FALSE, plot_study = TRUE, digits = 3, colour = "red", 
                    width = 1200, height = 1600, resolution = 115)
m5 <- metaprop_wrap(dataframe = da %>% arrange(-central), subgroup = "population_group", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = TRUE, digits = 3, colour = "red", 
                    width = 1200, height = 1600, resolution = 115)

p1 <- m1$plot
p2 <- m2$plot
p3 <- m3$plot
p4 <- m4$plot
p5 <- m5$plot

ggsave("figure_S5.png", plot = p1, width = 12, height = 16)
ggsave("figure_S6.png", plot = p2, width = 12, height = 16)
ggsave("figure_S7.png", plot = p3, width = 12, height = 16)
ggsave("figure_S8.png", plot = p4, width = 12, height = 16)
ggsave("figure_S9.png", plot = p5, width = 12, height = 16)

#figure_S10: meta-analysis with only known duplicates excluded
db <- d1 %>% filter(duplicate_cfr %in% c("False","Assumed"))

m1 <- metaprop_wrap(dataframe = db, subgroup = "lineage", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = "red", 
                    width = 4000, height = 4000, resolution = 500)
m2 <- metaprop_wrap(dataframe = db, subgroup = "population_country", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = "red", 
                    width = 4000, height = 4000, resolution = 500)
m3 <- metaprop_wrap(dataframe = db %>% arrange(cfr_ifr_denominator), subgroup = "cfr_denom_cat", 
                    plot_pooled = TRUE, sort_by_subg = FALSE, plot_study = FALSE, digits = 3, colour = "red", 
                    width = 4000, height = 4000, resolution = 500)
m4 <- metaprop_wrap(dataframe = db, subgroup = "population_group", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = "red", 
                    width = 4000, height = 4000, resolution = 500)

p1 <- m1$plot
p2 <- m2$plot
p3 <- m3$plot
p4 <- m4$plot

patchwork <- (p1 + p2 + p3 + p4) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A') 
ggsave("figure_S10.png", plot = patchwork, width = 12, height = 12)

#figure_S11: meta-analysis without de-duplication
dc <- d1

m1 <- metaprop_wrap(dataframe = dc, subgroup = "lineage", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = "red", 
                    width = 4000, height = 4000, resolution = 500)
m2 <- metaprop_wrap(dataframe = dc, subgroup = "population_country", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = "red", 
                    width = 4000, height = 4000, resolution = 500)
m3 <- metaprop_wrap(dataframe = dc %>% arrange(cfr_ifr_denominator), subgroup = "cfr_denom_cat", 
                    plot_pooled = TRUE, sort_by_subg = FALSE, plot_study = FALSE, digits = 3, colour = "red", 
                    width = 4000, height = 4000, resolution = 500)
m4 <- metaprop_wrap(dataframe = dc, subgroup = "population_group", 
                    plot_pooled = TRUE, sort_by_subg = TRUE, plot_study = FALSE, digits = 3, colour = "red", 
                    width = 4000, height = 4000, resolution = 500)

p1 <- m1$plot
p2 <- m2$plot
p3 <- m3$plot
p4 <- m4$plot

patchwork <- (p1 + p2 + p3 + p4) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A') 
ggsave("figure_S11.png", plot = patchwork, width = 12, height = 12)

##################
## HUMAN DELAYS ##
##################

#extract data to be plotted
d1 <- parameters %>% filter(parameter_type == 'Human delay - incubation period')
d2 <- parameters %>% filter(parameter_type == 'Human delay - symptom onset>admission to care')
d3 <- parameters %>% filter(parameter_type == 'Human delay - time in care (length of stay)' |
                            parameter_type == 'Human delay - admission to care>discharge/recovery' |
                            parameter_type == 'Human delay - admission to care>death')
d4 <- parameters %>% filter(parameter_type == 'Human delay - symptom onset>discharge/recovery' |
                            parameter_type == 'Human delay - symptom onset>death')

#arrange data and format for plotting
d1 <- d1 %>% arrange(-central)
d2 <- d2 %>% arrange(-central)
co <- c('Human delay - admission to care>discharge/recovery',
        'Human delay - admission to care>death',
        'Human delay - time in care (length of stay)')
d3 <- d3 %>% mutate(parameter_type = factor(parameter_type, levels = co)) %>%
             arrange(parameter_type,-central)
co <- c('Human delay - symptom onset>discharge/recovery',
        'Human delay - symptom onset>death')
d4 <- d4 %>% mutate(parameter_type = factor(parameter_type, levels = co)) %>%
             arrange(parameter_type,-central)

d3 <- d3 %>% mutate(parameter_type = factor(parameter_type,
                                            levels = unique(parameter_type),
                                            labels = c("Recovery","Death","Unspecified")))
d4 <- d4 %>% mutate(parameter_type = factor(parameter_type,
                                            levels = unique(parameter_type),
                                            labels = c("Recovery","Death")))

#call forest plot function
p1 <- forest_plot(d1,'Incubation Period (days)',"parameter_type",c(0,40))
p2 <- forest_plot(d2,'Onset-Admission Delay (days)',"parameter_type",c(-0.5,40))
p3 <- forest_plot(d3,'Admission-Outcome Delay (days)',"parameter_type",c(-1,40))
p4 <- forest_plot(d4,'Onset-Outcome Delay (days)',"parameter_type",c(0,40))

#meta-analysis of onset-admission delay
set.seed(42)
m5 <- metamean_wrap(dataframe = d2, estmeansd_method = "Cai",
                    plot_study = TRUE, digits = 2, lims = c(0,14), colour = "dodgerblue3", label = "Mean Onset-Admission Delay (days)",
                    width = 9500, height = 4500, resolution = 1000)

p5 <- m5$plot

patchwork <- ((p1 | p2) / (p3 | p4) / p5)
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_4.png", plot = patchwork, width = 12, height = 16)

#figure_S4: hypothetical probability distributions for onset-admission delay
p1 <- pdf_generic(m5$result,"common",c("Gamma","Lognormal","Weibull"),c(-0.2,30),'Onset-Admission Delay (days)')
p2 <- pdf_generic(m5$result,"random",c("Gamma","Lognormal","Weibull"),c(-0.2,30),'Onset-Admission Delay (days)')

patchwork <- (p1 + p2) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_S4.png", plot = patchwork, width = 12, height = 6)

##################
## TRANSMISSION ##
##################

#extract data to be plotted
d1 <- parameters %>% filter(parameter_type == 'Mutations - evolutionary rate')
d2 <- parameters %>% filter(parameter_type == 'Mutations – substitution rate')
d3 <- parameters %>% filter(parameter_class == 'Relative contribution')
d4 <- parameters %>% filter(parameter_class == 'Attack rate')
d5 <- parameters %>% filter(parameter_class == 'Growth rate')
d6 <- parameters %>% filter(parameter_class == 'Reproduction number')

#arrange data and format for plotting
d1 <- d1 %>% mutate(across(c(parameter_value,parameter_lower_bound, parameter_upper_bound,
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value),
                           ~. * 10^4)) #multiply by 10^4
d2 <- d2 %>% mutate(across(c(parameter_value,parameter_lower_bound, parameter_upper_bound,
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value),
                           ~. * 10^4)) #multiply by 10^4
d4 <- d4 %>% mutate_at(vars(c("parameter_value","parameter_lower_bound","parameter_upper_bound",
                              "parameter_uncertainty_lower_value","parameter_uncertainty_upper_value")), 
                       list(~ ifelse(parameter_unit == "No units", . * 100, .))) %>% mutate(parameter_unit = ifelse(parameter_unit == "No units", "Percentage (%)", parameter_unit))

d1 <- d1 %>% arrange(genome_site,-central) 
d2 <- d2 %>% arrange(genome_site,-central)
d3 <- d3 %>% arrange(-central)
d4 <- d4 %>% mutate(arate=c("Primary","Primary","Secondary")) %>%
             arrange(arate,-central)
d5 <- d5 %>% arrange(-central)
d6 <- d6 %>% arrange(parameter_type,-central)

d2 <- d2 %>% mutate(genome_site = factor(genome_site,
                                         levels = unique(genome_site),
                                         labels = c("L","S")))
d6 <- d6 %>% mutate(parameter_type = factor(parameter_type,
                                            levels = unique(parameter_type),
                                            labels = c("Basic (R0)","Effective (Re)")))

#call forest plot function
p1 <- forest_plot(d1,expression(Evolutionary~Rate~(s/s/y ~10^{-4})),"genome_site",c(5,30)) + guides(color = guide_legend(title = "Gene", order = 1))
p2 <- forest_plot(d2,expression(Substitution~Rate~(s/s/y ~10^{-4})),"genome_site",c(5,30)) + guides(color = guide_legend(title = "Segment", order = 1))
p3 <- forest_plot(d3,'Human-Human Transmission Contribution (%)',"pathogen",c(10,35))
p4 <- forest_plot(d4,'Attack Rate (%)',"arate",c(-0.01,1))
p5 <- forest_plot(d5,'Growth Rate (per day)',"pathogen",c(0,1.25))
p6 <- forest_plot(d6,'Reproduction Number',"parameter_type",c(0.5,2))

patchwork <- (p6 + p5 + p4 + p3 + p1 + p2) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A') #+ plot_layout(guides = 'collect')
ggsave("figure_5.png", plot = patchwork, width = 12, height = 10)

##############
## ARTICLES ##
##############

quality <- articles %>%
           mutate_at(vars(starts_with("qa")), funs(replace(., . == "Yes", 1))) %>%
           mutate_at(vars(starts_with("qa")), funs(replace(., . == "No", 0))) %>%
           mutate_at(vars(starts_with("qa")), as.numeric)
quality <- quality %>% rowwise() %>% mutate(score = 100*mean(c(qa_m1,qa_m2,qa_a3,qa_a4,qa_d5,qa_d6,qa_d7),na.rm=TRUE))
quality <- quality %>% mutate(category = ifelse(covidence_id %in% models$covidence_id, "Modelling Studies", "Non-Modelling Studies"))
quality$category <- factor(quality$category, levels = c("Non-Modelling Studies", "Modelling Studies"))

answers <- quality %>% 
           filter(!is.na(year_publication) & !is.na(pathogen)) %>% 
           dplyr::select(covidence_id,qa_m1,qa_m2,qa_a3,qa_a4,qa_d5,qa_d6,qa_d7) %>%
           pivot_longer(-covidence_id,names_to = "Question",values_to= "Assessment") %>% 
           mutate(Assessment=as.factor(as.character(Assessment)),
                  Assessment=case_when(Assessment == '1' ~ 'Yes',
                  Assessment == '0' ~ 'No')) %>%
           mutate(Question=case_when(Question=="qa_m1"~"Q1 Method: \nClear & \nReproducible",
                                     Question=="qa_m2"~"Q2 Method: \nRobust & \nAppropriate",
                                     Question=="qa_a3"~"Q3 Assumptions: \nClear & \nReproducible",
                                     Question=="qa_a4"~"Q4 Assumptions: \nRobust & \nAppropriate",
                                     Question=="qa_d5"~"Q5 Data: \nClear & \nReproducible",
                                     Question=="qa_d6"~"Q6 Data: \nIssues \nAcknowledged",
                                     Question=="qa_d7"~"Q7 Data: \nIssues \nAccounted For")) 

answers$Question <- factor(answers$Question, levels=rev(c("Q1 Method: \nClear & \nReproducible",
                                                          "Q2 Method: \nRobust & \nAppropriate",
                                                          "Q3 Assumptions: \nClear & \nReproducible",
                                                          "Q4 Assumptions: \nRobust & \nAppropriate",
                                                          "Q5 Data: \nClear & \nReproducible",
                                                          "Q6 Data: \nIssues \nAcknowledged",
                                                          "Q7 Data: \nIssues \nAccounted For")))

answers$Assessment[is.na(answers$Assessment)] <- "NA"
answers$Assessment <- factor(answers$Assessment,levels=c("NA","No","Yes"))

p1 <- ggplot(data=articles, aes(x = year_publication)) +
      geom_histogram(binwidth = 1, fill = "steelblue4", color = "black", alpha = 0.7) +
      scale_x_continuous(limits = c(1965,2025), breaks = seq(1970, 2020, by = 10), expand = c(0, 0)) + 
      scale_y_continuous(limits = c(0,25), expand = c(0, 0)) +
      theme_minimal() + 
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA)) + 
      labs(x = "Year of Publication", y = "Article Count")

p2 <- ggplot() +
      geom_histogram(data=quality, aes(x = score), binwidth = 20, boundary = 0, fill = "steelblue4", color = "black", alpha = 0.7) +
      scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20)) + 
      scale_y_continuous(limits = c(0,50), expand = c(0, 0)) +
      theme_minimal() + 
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA)) + 
      labs(x = "Quality Assessment Score (%)", y = "Article Count")

p3 <- ggplot() +
      geom_point(data = quality, aes(x=year_publication,y=score,color=category)) +
      geom_smooth(data = subset(quality, category == "Non-Modelling Studies"), aes(x=year_publication,y=score), span = 2, color = "springgreen3", fill = "springgreen3") +
      geom_smooth(data = subset(quality, category == "Modelling Studies"), aes(x=year_publication,y=score), span = 2, color = "red", fill = "red") +
      scale_x_continuous(limits = c(1965,2025), breaks = seq(1970, 2020, by=10), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20)) +
      xlab("Year of Publication") + ylab("Quality Assessment Score (%)") +  
      scale_color_manual(values = c("Non-Modelling Studies" = "springgreen3","Modelling Studies" = "red"), name = NULL) +
      theme_minimal() +      
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
            legend.position = 'bottom')

p4 <- answers %>%
      group_by(Question,Assessment) %>% summarize(count=n()) %>% ungroup() %>%
      ggplot(aes(fill=Assessment, y=count, x=Question)) + 
      geom_bar(position="stack", stat="identity") + theme_bw() +
      scale_y_continuous(limits = c(0,160), breaks = seq(0,160,by=40), expand = c(0, 0)) +
      scale_fill_manual(values = c("darkolivegreen2","coral1","grey70"),aesthetics = "fill",name="",breaks=c('Yes', 'No','NA')) +
      xlab("") + ylab("Article Count") + 
      coord_flip() +
      theme_minimal() + 
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA)) +
      theme(legend.position = 'bottom')

patchwork <- (p1 + p2 + p3 + p4) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_S1.png", plot = patchwork, width = 12, height = 12)

############
## MODELS ##
############

models <- models %>% mutate(model_type = str_replace_all(model_type,"Branching process","Branching Process"),
                            assumptions = str_replace_all(assumptions,";Latent period is same as incubation period",""),
                            compartmental_type = str_replace_all(compartmental_type,";SIR",""),
                            interventions_type = str_replace_all(interventions_type,"Unspecified","Other"))

p1 <- ggplot() + 
      geom_bar(data = models, aes(x = model_type, fill = stoch_deter), color = "black") + 
      scale_y_continuous(limits = c(0,30), breaks = seq(0,30,by = 5), expand = c(0,0)) +
      xlab("Model Type") + ylab("Model Count") +
      scale_fill_manual(values = c("Deterministic" = "steelblue4","Stochastic" = "red"), name = NULL) +
      theme_minimal() +      
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
            legend.position = c(0,1), legend.justification = c(0,1), legend.box.just = "left")

p2 <- ggplot() + 
      geom_bar(data = models, aes(x = factor(transmission_route, levels = c("Vector/Animal to human",
                                                                            "Human to human (direct contact)",
                                                                            "Human to human (direct contact);Vector/Animal to human",
                                                                            "Airborne or close contact;Human to human (direct contact);Vector/Animal to human")), fill = model_type), color = "black") + 
      scale_x_discrete(labels = c("Vector/Animal to human" = "Rodent-Human Only",
                                  "Human to human (direct contact);Vector/Animal to human" = "Rodent-Human\n& Human-Human",
                                  "Human to human (direct contact)" = "Human-Human Only",
                                  "Airborne or close contact;Human to human (direct contact);Vector/Animal to human" = "Rodent-Human,\nHuman-Human\n& Airborne")) +
      scale_y_continuous(limits = c(0,30), breaks = seq(0,30,by = 5), expand = c(0,0)) +
      xlab("Transmission Route(s)") + ylab("Model Count") +
      scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3"), name = NULL) +
      theme_minimal() +      
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
            legend.position = c(0,1), legend.justification = c(0,1), legend.box.just = "left")

p3 <- ggplot() + 
      geom_bar(data = models, aes(x = factor(assumptions, levels = c("Homogeneous mixing",
                                                                     "Heterogenity in transmission rates - over time",
                                                                     "Heterogenity in transmission rates - between groups",
                                                                     "Age dependent susceptibility",
                                                                     "Heterogenity in transmission rates - between groups;Heterogenity in transmission rates - over time")), fill = model_type), color = "black") +
      scale_x_discrete(labels = c("Homogeneous mixing" = "Homogeneous",
                                  "Heterogenity in transmission rates - over time" = "Time-\nHeterogeneous",
                                  "Heterogenity in transmission rates - between groups" = "Subgroup-\nHeterogeneous",
                                  "Age dependent susceptibility" = "Age-\nHeterogeneous",
                                  "Heterogenity in transmission rates - between groups;Heterogenity in transmission rates - over time" = "Time- & Subgroup-\nHeterogeneous")) +
      scale_y_continuous(limits = c(0,30), breaks = seq(0,30,by = 5), expand = c(0,0)) +
      xlab("Human Transmission Heterogeneity") + ylab("Model Count") +
      scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3"), name = NULL) +
      theme_minimal() +      
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
            legend.position = "none")

p4 <- ggplot() + 
      geom_bar(data = models, aes(x = factor(compartmental_type, levels = c("SIR",
                                                                            "SEIR",
                                                                            "Other compartmental",
                                                                            "Not compartmental")), fill = model_type), color = "black") + 
      scale_x_discrete(labels = c("Other compartmental" = "Other",
                                  "Not compartmental" = "N/A")) +
      scale_y_continuous(limits = c(0,30), breaks = seq(0,30,by = 5), expand = c(0,0)) +
      xlab("Human Compartments") + ylab("Model Count") +
      scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3"), name = NULL) +
      theme_minimal() +      
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
            legend.position = "none")

p5 <- ggplot() + 
      geom_bar(data = models, aes(x = factor(theoretical_model, levels = c("FALSE",
                                                                           "TRUE")), fill = model_type), color = "black") +
      scale_x_discrete(labels = c("FALSE" = "Fitted to Data",
                                  "TRUE" = "Theoretical")) +
      scale_y_continuous(limits = c(0,30), breaks = seq(0,30,by = 5), expand = c(0,0)) +
      xlab("Model Calibration") + ylab("Model Count") +
      scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3"), name = NULL) +
      theme_minimal() +      
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
            legend.position = "none")

p6 <- ggplot() + 
      geom_bar(data = models %>% separate_rows(interventions_type, sep = ";"), 
               aes(x = factor(interventions_type, levels = c("Vector/Animal control","Behaviour changes","Quarantine",
                                                             "Contact tracing","Treatment","Hospitals",
                                                             "Vaccination","Other")), fill = model_type), color = "black") +
      scale_x_discrete(labels = c("Vector/Animal control" = "Rodent\nControl",
                                  "Behaviour changes" = "Behaviour\nChanges",
                                  "Contact tracing" = "Contact\nTracing",
                                  "Other" = "Other &\nUnspecified")) +
      scale_y_continuous(limits = c(0,30), breaks = seq(0,30,by = 5), expand = c(0,0)) +
      xlab("Interventions") + ylab("Model Count") +
      scale_fill_manual(values = c("Branching Process" = "purple","Compartmental" = "orange", "Other" = "springgreen3"), name = NULL) +
      theme_minimal() +      
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
            legend.position = "none")

patchwork <- (p1 + p2 + p3 + p4 + p5 + p6) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_S2.png", plot = patchwork, width = 12, height = 16)

################
## PARAMETERS ##
################

parameters <- parameters %>% mutate(parameter_class = case_when(
                                      parameter_class == "Reproduction number" ~ "Reproduction Numbers",
                                      parameter_class %in% c("Relative contribution","Attack rate","Growth rate") ~ "Other Transmission Parameters",
                                      parameter_class == "Human delay" ~ "Delays",
                                      parameter_class == "Risk factors" ~ "Risk Factors",
                                      TRUE ~ parameter_class),
                                    parameter_type = case_when(
                                      parameter_type == "Attack rate" ~ "Attack Rate",  
                                      parameter_type == "Growth rate (r)" ~ "Growth Rate",  
                                      parameter_type == "Human delay - admission to care>death" ~ "Time Admission to Outcome",  
                                      parameter_type == "Human delay - admission to care>discharge/recovery" ~ "Time Admission to Outcome",  
                                      parameter_type == "Human delay - incubation period" ~ "Incubation Period",  
                                      parameter_type == "Human delay - other human delay (go to section)" ~ "Other Delay",  
                                      parameter_type == "Human delay - symptom onset>admission to care" ~ "Time Onset to Admission",  
                                      parameter_type == "Human delay - symptom onset>death" ~ "Time Onset to Outcome",  
                                      parameter_type == "Human delay - symptom onset>discharge/recovery" ~ "Time Onset to Outcome",  
                                      parameter_type == "Human delay - time in care (length of stay)" ~ "Time Onset to Outcome",  
                                      parameter_type == "Mutations - evolutionary rate" ~ "Evolutionary Rate",  
                                      parameter_type == "Mutations – substitution rate" ~ "Substitution Rate",  
                                      parameter_type == "Relative contribution - human to human" ~ "Relative Transmission Contribution",  
                                      parameter_type == "Reproduction number (Basic R0)" ~ "Basic (R0)",  
                                      parameter_type == "Reproduction number (Effective, Re)" ~ "Effective (Re)",  
                                      parameter_type == "Risk factors" ~ "Risk Factors",  
                                      parameter_type == "Seroprevalence - IFA" ~ "IFA",  
                                      parameter_type == "Seroprevalence - IgG" ~ "IgG", 
                                      parameter_type == "Seroprevalence - IgM" ~ "IgM",  
                                      parameter_type == "Seroprevalence - PRNT" ~ "PRNT",  
                                      parameter_type == "Seroprevalence - Unspecified" ~ "Unspecified",  
                                      parameter_type == "Severity - case fatality rate (CFR)" ~ "Case Fatality Ratio (CFR)",  
                                      TRUE ~ parameter_type),
                                    population_country = ifelse(is.na(population_country),"Unspecified",population_country), 
                                    study_midyear = ifelse(!is.na(population_study_start_year) & !is.na(population_study_end_year),
                                                           round((population_study_start_year + population_study_end_year) / 2),
                                                           population_study_start_year),
                                    study_midyear_cat = case_when(
                                      study_midyear %in% 1960:1969 ~ "1960-1969",
                                      study_midyear %in% 1970:1979 ~ "1970-1979",
                                      study_midyear %in% 1980:1989 ~ "1980-1989",
                                      study_midyear %in% 1990:1999 ~ "1990-1999",
                                      study_midyear %in% 2000:2009 ~ "2000-2009",
                                      study_midyear %in% 2010:2019 ~ "2010-2019",
                                      study_midyear %in% 2020:2029 ~ "2020-Present",
                                      TRUE ~ "Unspecified"),
                                    population_sample_type = case_when(
                                      population_sample_type == "Mixed settings" ~ "Mixed Settings",   
                                      population_sample_type == "Trade / business based" ~ "Trade/Business-Based",
                                      is.na(population_sample_type) ~ "Unspecified",
                                      TRUE ~ str_replace_all(population_sample_type, " based", "-Based"))) %>%
                                    mutate(parameter_type = factor(parameter_type, levels = unique(parameter_type[order(parameter_class,parameter_type)])))

p1 <- ggplot() + 
      geom_bar(data = parameters,
               aes(x = reorder(parameter_type, parameter_class), fill = parameter_class), color = "black") + 
      scale_x_discrete(limits = rev) + 
      scale_y_continuous(limits = c(0,180), breaks = seq(0,180,by = 30), expand = c(0,0)) +
      xlab("Parameter Type") + ylab("Parameter Count") +
      scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
      theme_minimal() +      
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
            legend.position = c(1,1), legend.justification = c(1,1), legend.box.just = "left") +
      coord_flip()

p2 <- ggplot() + 
      geom_bar(data = parameters %>% separate_rows(population_country, sep = ";"),           
               aes(x = population_country, fill = parameter_class), color = "black") + 
      scale_x_discrete(limits = rev) + 
      scale_y_continuous(limits = c(0,180), breaks = seq(0,180,by = 30), expand = c(0,0)) +
      xlab("Study Country") + ylab("Parameter Count") +
      scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
      theme_minimal() +      
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
            legend.position = "none") +
      coord_flip()

p3 <- ggplot() + 
      geom_bar(data = parameters, aes(x = factor(study_midyear_cat, levels = c("1960-1969","1970-1979",
                                                                               "1980-1989","1990-1999",
                                                                               "2000-2009","2010-2019",
                                                                               "2020-Present","Unspecified")), fill = parameter_class), color = "black") + 
      scale_x_discrete(limits = rev) + 
      scale_y_continuous(limits = c(0,180), breaks = seq(0,180,by = 30), expand = c(0,0)) +
      xlab("Study Year") + ylab("Parameter Count") +
      scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
      theme_minimal() +      
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
            legend.position = "none") +
      coord_flip()

p4 <- ggplot() + 
      geom_bar(data = parameters, aes(x = population_sample_type, fill = parameter_class), color = "black") + 
      scale_x_discrete(limits = rev) + 
      scale_y_continuous(limits = c(0,180), breaks = seq(0,180,by = 30), expand = c(0,0)) +
      xlab("Study Setting") + ylab("Parameter Count") +
      scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95, name = NULL) +
      theme_minimal() +      
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
            legend.position = "none") +
      coord_flip()

patchwork <- (p1 + p2 + p3 + p4) + plot_layout(ncol = 1, heights = c(1.1,1.7,0.5,0.7))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_S3.png", plot = patchwork, width = 12, height = 16)