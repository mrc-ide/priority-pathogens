#task to plot lassa severity figures

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
                        "rivers_africa_37333"   = "rivers_africa_37333")
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")
orderly_artefact("lassa-specific figures",c("figure_3.png","figure_S6.png","figure_S7.png",
                                            "figure_S8.png","figure_S9.png","figure_S10.png",
                                            "figure_S11.png","figure_S12.png",
                                            "figure_3.pdf","figure_S6.pdf","figure_S7.pdf",
                                            "figure_S8.pdf","figure_S9.pdf","figure_S10.pdf",
                                            "figure_S11.pdf","figure_S12.pdf"))

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

##############
## SEVERITY ##
##############

#extract data and define columns for meta-analysis
d1 <- parameters %>% filter(parameter_type == 'Severity - case fatality rate (CFR)') %>%
                     mutate(case_def = case_when(
                            covidence_id %in% c(18,88,167,174,307,461,645,845,870,873,874,920,921,1080,1173,1181,1254,1272,1368,1413,1426,1444,2567,2579,2585,2589,2636,2651,2662,2684,2818,3147,3215,3530,3635,3716,3841,3991,4314,4727,5419,5439,5450,5622,5623,2791,3210,1366,2229,1083,1192) ~ "Lab-Confirmed",
                            covidence_id %in% c(252,1433,2714,4745) ~ "Clinically-Diagnosed",
                            covidence_id %in% c(1328) ~ "Probable",
                            covidence_id %in% c(832,2617) ~ "Suspected",
                            covidence_id %in% c(416,670) ~ "Lab-Confirmed;Clinically-Diagnosed",
                            covidence_id %in% c(1183,2611,2656,2760,3634) ~ "Lab-Confirmed;Probable",
                            covidence_id %in% c(854,871,1033,1447) ~ "Lab-Confirmed;Suspected")) %>%
                     mutate(lineage = case_when(
                            population_country == "Nigeria" & population_location %in% c("Aboh Mbaise, Aba, Owerri","Ebonyi","Ebonyi State","Edo North, Central Senatorial District Of Edo State","Edo State","Ondo State","Irrua","Irrua, Edo State","Isth","Irrua Specialist Teaching Hospital","Edo, Ebonyi, Ondo, Kebbi")~ "Lineage II Region - Nigeria",
                            population_country == "Nigeria" & population_location %in% c("Bauchi State","Atbuth, Bauchi","Jos","Plateau State") ~ "Lineage III Region - Nigeria",  
                            population_country %in% c("Guinea","Liberia","Sierra Leone","Guinea, Sierra Leone","Guinea, Liberia, Sierra Leone") ~ "Lineage IV Region - Guinea, Liberia, Sierra Leone",
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
                                                2760,2656,4314,2589,3215,3991,2662,3635,874,920,2636,252,3530,1254,2684,5439,5419,1366,1083,5622,1192,5623,3210) ~ "Assumed",
                            TRUE ~ "False"))#only identified for estimates passed to meta-analysis (i.e. denominator not NA)

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
ggsave("figure_3.pdf", plot = patchwork, width = 18, height = 12)

#figure_S6-S10: meta-analysis with all estimates plotted
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

ggsave("figure_S6.png", plot = p1, width = 12, height = 16)
ggsave("figure_S6.pdf", plot = p1, width = 12, height = 16)
ggsave("figure_S7.png", plot = p2, width = 12, height = 16)
ggsave("figure_S7.pdf", plot = p2, width = 12, height = 16)
ggsave("figure_S8.png", plot = p3, width = 12, height = 16)
ggsave("figure_S8.pdf", plot = p3, width = 12, height = 16)
ggsave("figure_S9.png", plot = p4, width = 12, height = 16)
ggsave("figure_S9.pdf", plot = p4, width = 12, height = 16)
ggsave("figure_S10.png", plot = p5, width = 12, height = 16)
ggsave("figure_S10.pdf", plot = p5, width = 12, height = 16)

#figure_S11: meta-analysis with only known duplicates excluded
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
ggsave("figure_S11.png", plot = patchwork, width = 12, height = 12)
ggsave("figure_S11.pdf", plot = patchwork, width = 12, height = 12)

#figure_S12: meta-analysis without de-duplication
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
ggsave("figure_S12.png", plot = patchwork, width = 12, height = 12)
ggsave("figure_S12.pdf", plot = patchwork, width = 12, height = 12)