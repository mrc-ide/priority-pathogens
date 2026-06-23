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
library(orderly2)

#orderly preparation
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
# orderly_dependency("zika_compilation", "latest(parameter:pathogen == this:pathogen)",
#                    c("articles.rds", "outbreaks.rds", "models.rds", "parameters.rds"))
orderly_dependency("zika_prep_data", "latest(parameter:pathogen == this:pathogen &&
                   parameter:plotting == TRUE)",
                   c("articles_curated.rds", "outbreaks_curated.rds", "models_curated.rds", "parameters_curated.rds"))


orderly_shared_resource("zika_functions.R" = "zika_functions.R",
                        "gadm41_BOL_1.shp" = "shapefile/gadm41_BOL_1.shp",
                        "gadm41_BOL_1.dbf" = "shapefile/gadm41_BOL_1.dbf",
                        "gadm41_BOL_1.shx" = "shapefile/gadm41_BOL_1.shx",

                        "gadm41_MEX_1.shp" = "shapefile/gadm41_MEX_1.shp",
                        "gadm41_MEX_1.dbf" = "shapefile/gadm41_MEX_1.dbf",
                        "gadm41_MEX_1.shx" = "shapefile/gadm41_MEX_1.shx",

                        "gadm41_KEN_1.shp" = "shapefile/gadm41_KEN_1.shp",
                        "gadm41_KEN_1.dbf" = "shapefile/gadm41_KEN_1.dbf",
                        "gadm41_KEN_1.shx" = "shapefile/gadm41_KEN_1.shx",

                        "gadm41_THA_1.shp" = "shapefile/gadm41_THA_1.shp",
                        "gadm41_THA_1.dbf" = "shapefile/gadm41_THA_1.dbf",
                        "gadm41_THA_1.shx" = "shapefile/gadm41_THA_1.shx",

                        "gadm41_COL_0.shp" = "shapefile/gadm41_COL_0.shp",
                        "gadm41_COL_0.dbf" = "shapefile/gadm41_COL_0.dbf",
                        "gadm41_COL_0.shx" = "shapefile/gadm41_COL_0.shx",

                        "gadm41_GUF_0.shp" = "shapefile/gadm41_GUF_0.shp",
                        "gadm41_GUF_0.dbf" = "shapefile/gadm41_GUF_0.dbf",
                        "gadm41_GUF_0.shx" = "shapefile/gadm41_GUF_0.shx",

                        "gadm41_HND_0.shp" = "shapefile/gadm41_HND_0.shp",
                        "gadm41_HND_0.dbf" = "shapefile/gadm41_HND_0.dbf",
                        "gadm41_HND_0.shx" = "shapefile/gadm41_HND_0.shx",

                        "gadm41_BGD_0.shp" = "shapefile/gadm41_BGD_0.shp",
                        "gadm41_BGD_0.dbf" = "shapefile/gadm41_BGD_0.dbf",
                        "gadm41_BGD_0.shx" = "shapefile/gadm41_BGD_0.shx",

                        "gadm41_THA_0.shp" = "shapefile/gadm41_THA_0.shp",
                        "gadm41_THA_0.dbf" = "shapefile/gadm41_THA_0.dbf",
                        "gadm41_THA_0.shx" = "shapefile/gadm41_THA_0.shx")


source("zika_functions.R")

# Function to geocode locations (run once and save results)
geocode_locations <- function(data, save_path = "geocoded_locations_zika.csv") {
  # if (file.exists(save_path)) {
  #   return(read.csv(save_path))
  # }
  data <- tidygeocoder::geocode(data, population_location, method = "osm")
  write.csv(data, save_path, row.names = FALSE)
  return(data)
}


# Function to load and merge shapefiles
load_and_merge_shapefile <- function(data, country, shapefile_path, shapefile_col) {
  result <- filter(data, population_country == country)
  shapefile <- st_read((shapefile_path))
  result <- merge(result, shapefile, by.x = "population_location", by.y = shapefile_col)
  return(st_as_sf(result))
}

params_clean <- readRDS("parameters_curated.rds")

serop_data <- params_clean %>%
  filter(str_starts(parameter_type, "Seroprevalence"))

serop_data <- filter(serop_data, !qa_score < 0.5)

# View(serop_data[, c("covidence_id", "central", "parameter_value", "parameter_uncertainty_type","parameter_uncertainty_lower_value", "parameter_uncertainty_upper_value",
#                         "parameter_lower_bound", "parameter_upper_bound", "population_sample_size", "population_country", "population_location",
#                         "population_group",
#                         "population_sample_type", "population_study_start_year",
#                         "population_study_end_year")]

# write.csv(serop_data, "zika_serop_data.csv")
serop_data_map <- serop_data %>%
  filter(population_group == "General population" & population_sample_type == "Population based"&
           parameter_type %in% c( "Seroprevalence - IgG", "Seroprevalence - HAI/HI" , "Seroprevalence - MIA" , "Seroprevalence - MIA",  "Seroprevalence - Neutralisation/PRNT"      ))


serop_data_map <- filter(serop_data_map, !qa_score < 0.5) #malaysia excluded

#geolocate locations
locations <- geocode_locations(serop_data_map)

#Mexico has 4 entries but only one to use, fix name removing State. Honduras and Puerto rico are at national level
locations <- locations %>%
  mutate(population_location = ifelse(population_location == "Chiapas State", "Chiapas", population_location),
         population_location = ifelse(population_country == "Honduras", "Honduras", population_location),
         population_location = ifelse(population_country == "Puerto Rico", "Puerto Rico", population_location),
         population_country = ifelse(population_country ==  "France" & population_location ==  "French Guiana", "French Guiana", population_country) )


#Bangladesh has some that are only national, same Thailand
locations <- locations %>%
  mutate(population_location = ifelse(is.na(population_location) & population_country == "Bangladesh", "Bangladesh", population_location),
         population_location = ifelse(is.na(population_location) & population_country == "Thailand", "Thailand", population_location))


#keep most recent estimate in case of multiple entry for same location
locations <- locations %>%
  group_by(population_location) %>%
  slice_max(population_study_end_year, n = 1, with_ties = FALSE) %>% #will laways keep just one even if same
  ungroup()

#Colombia has both admin/national and cities, change location to be Colombia when national
result.COL <- filter(locations, population_country == "Colombia") %>% #national
  mutate(population_location = ifelse(is.na(population_location), "Colombia", population_location)) %>%
  filter(population_location != "Barranquilla")  # town


# Load country-level shapefiles
result.BOL <- load_and_merge_shapefile(locations, "Bolivia", "gadm41_BOL_1.shp", "NAME_1")
# result.COL <- load_and_merge_shapefile(result.COL, "Colombia", "gadm41_COL_0.shp", "COUNTRY")
result.FG  <- load_and_merge_shapefile(locations, "French Guiana", "gadm41_GUF_0.shp", "COUNTRY")
result.MEX <- load_and_merge_shapefile(locations, "Mexico", "gadm41_MEX_1.shp", "NAME_1")
# result.HON <- load_and_merge_shapefile(locations, "Honduras", "gadm41_HND_0.shp", "COUNTRY")

# Check if CRS is missing
st_crs(result.BOL)       # should be NA as the others
st_crs(result.MEX)
st_crs(result.FG)

result.BOL <- st_set_crs(result.BOL, NA)


# Manually add coordinates for specific locations
locations <- locations %>%
  mutate(long = ifelse(population_location == "Ribeirão Preto, São Paulo State", -47.820838, long),
         lat = ifelse(population_location == "Ribeirão Preto, São Paulo State", -21.176631, lat),

         long = ifelse(population_location == "Pueblo Nuevo", -79.5136, long),
         lat = ifelse(population_location == "Pueblo Nuevo", -7.189056, lat),

         long = ifelse(population_location == "Barranquilla", -74.82318, long),
         lat = ifelse(population_location == "Barranquilla", 11.01019, lat),

         long = ifelse(population_location ==  "Puerto Rico", -66.1, long),
         lat = ifelse(population_location ==  "Puerto Rico", 18.45, lat))


# Combine geolocated data
result.geoloc.americas <- filter(locations, population_country %in% c("Brazil", "Honduras", "Puerto Rico", "Peru")) %>% #city estimates
  rbind(filter(locations, population_location == "Barranquilla")) #city estimate Colombia

# Plot South America
south_america <- ggplot() +
  borders("world", colour = "gray50", fill = "gray90") +
  geom_sf(data = result.BOL, aes(fill = central), color = "white") +
  # geom_sf(data = result.COL, aes(fill = central), color = "white") +
  geom_sf(data = result.FG, aes(fill = central), color = "white") +
  # geom_sf(data = result.HON, aes(fill = central), color = "white") +
  geom_sf(data = result.MEX, aes(fill = central), color = "white") +
  coord_sf(xlim = c(-100, -30), ylim = c(-30, 25)) +  # Adjusted limits
  geom_point(data = result.geoloc.americas, aes(x = long, y = lat, color = central), size = 3) +
  scale_color_viridis_c(limits = c(0, 70),guide = "none") +
  scale_fill_viridis_c(limits = c(0, 70),guide = "none") +
  theme_bw()

# print(south_america)

result.THA <- filter(locations, population_country == "Thailand")

# Load country-level shapefiles
result.BGD <- load_and_merge_shapefile(locations, "Bangladesh", "gadm41_BGD_0.shp", "COUNTRY")
result.THA1 <- load_and_merge_shapefile(locations, "Thailand", "gadm41_THA_0.shp", "COUNTRY")
result.THA2 <- load_and_merge_shapefile(locations, "Thailand", "gadm41_THA_1.shp", "NAME_1")

# Load Malaysia shapefile separately (with exclusion of Sabah & Sarawak)
# shapefile.MYS <- st_read(paste0(getwd(), "/shapefile/gadm41_MYS_shp/gadm41_MYS_1.shp")) %>%
#   filter(!NAME_1 %in% c("Sabah", "Sarawak"))  # Selecting only Peninsular Malaysia

# Assign central values from result.MYS to the filtered shapefile
# result.MYS <- filter(locations, population_country == "Malaysia")
# shapefile.MYS$central <- result.MYS$central

# Manually add coordinates for Nanning City, China
locations <- locations %>%
  mutate(long = ifelse(population_location == "Nanning City, Guanxi", 108.316666, long),
         lat = ifelse(population_location == "Nanning City, Guanxi", 22.816668, lat))

# Filter other Asian locations
result.geoloc.asia <- filter(locations, population_country %in% c("China", "Indonesia",  "Myanmar",  "Pakistan"))

# Plot Asia Map
central_asia <- ggplot() +
  borders("world", colour = "gray50", fill = "gray90") +
  geom_sf(data = result.BGD, aes(fill = central), color = "white") +
  # geom_sf(data = shapefile.MYS, aes(fill = central), color = "white") +
  geom_sf(data = result.THA1, aes(fill = central), color = "white") +
  geom_sf(data = result.THA2, aes(fill = central), color = "white") +
  coord_sf(xlim = c(70, 120), ylim = c(-5, 35)) +  # Adjusted limits
  geom_point(data = result.geoloc.asia, aes(x = long, y = lat, color = central), alpha = 0.7, size = 3) +
  # scale_color_viridis_c(limits = c(0, 70),guide = "none") +
  scale_fill_viridis_c(limits = c(0, 70), name = "none") +
  scale_color_viridis_c(limits = c(0, 70), guide = "none") +
  theme_bw()

# print(central_asia)


# Update population_location for Kenya
result.KEN <- locations %>%
  filter(population_country == "Kenya" & population_location %in% c("Turkana", "Kitui District", "West Pokot" )) %>%
  mutate(population_location = ifelse(population_location == "Kitui District", "Kutui", population_location))

# Read Kenya shapefile and merge
shapefile.KEN2 <- st_read("gadm41_KEN_1.shp")

result.KEN <- merge(result.KEN, shapefile.KEN2, by.x = "population_location", by.y = "NAME_1") %>%
  st_as_sf()

# Manually update coordinates for specific locations
result.KEN1 <- locations %>%
  filter(population_country == "Kenya" & population_location %in% c("Malindi District", "Central Nyanza")) %>%
  mutate(
    long = case_when(
      population_location == "Malindi District" ~ 40.117,
      population_location == "Central Nyanza" ~ 34.477884280634,
      TRUE ~ long
    ),
    lat = case_when(
      population_location == "Malindi District" ~ -3.219186,
      population_location == "Central Nyanza" ~ -0.489257579667,
      TRUE ~ lat
    )
  )

# Update Mali locations
result.MAL <- locations %>%
  filter(population_country == "Mali") %>%
  mutate(
    lat = case_when(
      population_location == "Bamba, Sibirila, Bougouni, Sikasso" ~ 10.3833,
      population_location == "Banzana, Sibirila, Bougouni, Sikasso" ~ 10.533,
      population_location == "Soromba, Sibirila, Bougouni, Sikasso" ~ 10.583,
      TRUE ~ lat
    ),
    long = case_when(
      population_location == "Bamba, Sibirila, Bougouni, Sikasso" ~ -7.1500,
      population_location == "Banzana, Sibirila, Bougouni, Sikasso" ~ -7.250,
      population_location == "Soromba, Sibirila, Bougouni, Sikasso" ~ -7.150,
      TRUE ~ long
    )
  )

# Filter other African countries
result.CABO <- locations %>%
  filter(population_country == "Cabo Verde") %>%
  mutate(long = -23.6167, lat = 15.1067)

result.GAB <- filter(locations, population_country == "Gabon")

result.RUW <- locations %>%
  filter(population_country == "Rwanda") %>%
  mutate(long = 29.75, lat = 2.080)

# Combine African locations
result.geoloc.africa <- rbind(result.CABO, result.GAB, result.KEN1, result.MAL, result.RUW)

# Plot the map
central_africa <- ggplot() +
  borders("world", colour="gray50", fill="gray90") +
  geom_sf(data = result.KEN, aes(fill = central), color = "white") +
  coord_sf(xlim = c(-25, 40), ylim = c(-15, 20)) + # Focused map
  geom_point(data = result.geoloc.africa, aes(x = long, y = lat, color = central), alpha = 0.7, size = 3) +
  scale_size_continuous(range = c(3, 8)) +
  scale_color_viridis_c(limits = c(0, 70), name = "% Seroprevalence") +
  scale_fill_viridis_c(limits = c(0, 70), guide = "none") +
  # scale_fill_viridis_c(limits = c(0, 70), name = "% Seroprevalence") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    # legend.position = c(0.3,0.4)
  )

# central_africa

central_africa <- central_africa+
  labs(fill = NULL, col = NULL) +
  theme(legend.position = c(0.2,0.3),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))+
  # theme(legend.position = "none")+
        # legend.background = element_rect(fill = "transparent"))+
  #theme(legend.position = c(0.1,0.2))+
  theme(
    axis.text = element_blank(),      # Remove text labels
    axis.ticks = element_blank(),     # Remove tick marks
    axis.title = element_blank()      # Remove axis titles
  )
south_america <- south_america +
  theme(legend.position = "none")+
  labs(fill = NULL,  col = NULL) +
  theme(
    axis.text = element_blank(),      # Remove text labels
    axis.ticks = element_blank(),     # Remove tick marks
    axis.title = element_blank()      # Remove axis titles
  )
central_asia <- central_asia +
  theme(legend.position = "none")+
  labs(fill = NULL,  col = NULL) +
  theme(
    axis.text = element_blank(),      # Remove text labels
    axis.ticks = element_blank(),     # Remove tick marks
    axis.title = element_blank()      # Remove axis titles
  )

# coord <- coord_sf(expand = FALSE)

# south_america <- south_america + coord
# central_africa <- central_africa + coord
# central_asia <- central_asia + coord

final_plot <- ggarrange(
  south_america,
  central_africa,
  central_asia,
  ncol = 3,
  align = "hv"
)

# Final multipanel plot
final_plot <- ggarrange(south_america, central_africa, central_asia, ncol = 3)
# +
#   plot_layout(guides = "none")
# final_plot
# +
#   plot_layout(guides = "collect")

# final_plot
ggsave(
  paste0("ZIKA_serop_map_QA.png"),
  plot = final_plot,
  device = png,

  scale = 1,
  height = 100,
  width = 200,
  units = "mm",
  dpi = 300)

ggsave(
  paste0("ZIKA_serop_map_QA_AF.pdf"),
  plot = central_africa,
  # device = pdf,
  device = cairo_pdf,
  scale = 1,
  height = 200,
  width = 200,
  units = "mm",
  dpi = 300)

ggsave(
  paste0("ZIKA_serop_map_QA_SA.pdf"),
  plot = south_america,
  # device = pdf,
  device = cairo_pdf,
  scale = 1,
  height = 200,
  width = 200,
  units = "mm",
  dpi = 300)

ggsave(
  paste0("ZIKA_serop_map_QA_AS.pdf"),
  plot = central_asia,
  device = cairo_pdf,
  # device = pdf,
  scale = 1,
  height = 200,
  width = 200,
  units = "mm",
  dpi = 300)
#
ggsave(
  paste0("ZIKA_serop_map_QA.svg"),
  plot = final_plot,
  device = svg,
  scale = 1,
  height = 8,
  width = 14,
  dpi = 300)


######## seroprevalence plot

# Fix missing study years and countries
year_fixes <- data.frame(
  covidence_id = c(5863, 18107, 1993, 7654, 22284, 10010, 174, 6423, 10168, 6689, 6731, 10094, 10094),
  parameter_type = c("Seroprevalence - IgG", "Seroprevalence - IgG", "Seroprevalence - Neutralisation/PRNT",
                     "Seroprevalence - IgG", "Seroprevalence - IgG", "Seroprevalence - IgM",
                     "Seroprevalence - Western blot", "Seroprevalence - IgM", "Seroprevalence - IgM",
                     "Seroprevalence - IgG", "Seroprevalence - Neutralisation/PRNT",
                     "Seroprevalence - IgM", "Seroprevalence - Neutralisation/PRNT"),
  population_study_end_year = c(2020, 2022, 1978, 2018, 2019, 2022, 2015, 2021, 2016, 2017, 2016, 2016, 2016)
)

seroprevalence_data <- serop_data %>%
  left_join(year_fixes, by = c("covidence_id", "parameter_type")) %>%
  mutate(
    population_study_end_year = coalesce(population_study_end_year.y, population_study_end_year.x),
    population_group = replace_na(population_group, "Other"),
    population_country = ifelse(covidence_id == 6197, "Burkina Faso", population_country)
  ) %>%
  select(-population_study_end_year.x, -population_study_end_year.y)

# Standardize population_group values
seroprevalence_data <- seroprevalence_data %>%
  mutate(population_group = case_when(
    population_group %in% c("Household contacts of survivors", "Sex workers", "Outdoor workers") ~ "Other",
    TRUE ~ population_group
  ))

# Assign continent and year groups
seroprevalence_data <- seroprevalence_data %>%
  mutate(
    continent = countrycode(population_country, origin = "country.name", destination = "continent"),
    year_group = case_when(
      population_study_end_year < 2016 ~ "Pre-2016",
      population_study_end_year > 2019 ~ "Post-2019",
      TRUE ~ as.character(population_study_end_year)
    ),
    year_group = factor(year_group, levels = c("Pre-2016", as.character(2016:2019), "Post-2019")),
    population_group = factor(population_group,
                              levels = c("General population", "Blood donors", "Persons under investigation",
                                         "Pregnant women", "Mixed groups", "Children", "Other"))
  )

seroprevalence_data$continent <- ifelse(seroprevalence_data$population_country == "France (French Guiana)", "Americas", seroprevalence_data$continent)

# Aggregate data for bar plot
bar_data <- seroprevalence_data %>%
  group_by(population_group, continent, year_group) %>%
  summarise(unique_count = n_distinct(covidence_id), .groups = "drop")

bar_data2 <- seroprevalence_data %>%
  group_by(population_group, continent, year_group, parameter_type) %>%
  summarise(unique_count = n_distinct(covidence_id), .groups = "drop")

bar_data2$parameter_type <- gsub("Seroprevalence - ", "", bar_data2$parameter_type)
bar_data2$parameter_type <- gsub("Biotinylated-EDIII antigen capture ELISA", "capture ELISA", bar_data2$parameter_type)
# Plot population group (stacked bar)
plot_population_group <- ggplot(bar_data, aes(x = population_group, y = unique_count, fill = continent)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~year_group) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1),
        axis.text.y = element_text(size = 10)) +
  ylim(0, 14) +
  xlab(NULL) +
  ylab("Number of Unique Covidence IDs") +
  scale_fill_viridis_d(option = "plasma") +
  guides(fill = guide_legend(title = NULL))

# Plot population group (boxplot)
plot_population_group2 <- ggplot(seroprevalence_data, aes(x = population_group, y = central, fill = continent)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(aes(color = continent), position = position_dodge(width = 0.7),
             size = 1, alpha = 0.7) +
  facet_wrap(~year_group) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1),
        axis.text.y = element_text(size = 10)) +
  xlab(NULL) +
  ylab("% Seroprevalence") +
  scale_fill_viridis_d(option = "plasma") +
  scale_colour_viridis_d(option = "plasma") +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL))

plot_population_group3 <- ggplot(bar_data2, aes(x = parameter_type, y = unique_count, fill = continent)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year_group) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1),
        axis.text.y = element_text(size = 10)) +
  ylim(0, 14) +
  xlab(NULL) +
  ylab("Number of Unique Covidence IDs") +
  scale_fill_viridis_d(option = "plasma") +
  guides(fill = guide_legend(title = NULL))

# plot_population_group4 <- ggplot(bar_data3, aes(x = parameter_type, y = unique_count, fill = population_country)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~year_group) +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1),
#         axis.text.y = element_text(size = 10)) +
#   ylim(0, 14) +
#   xlab(NULL) +
#   ylab("Number of Unique Covidence IDs") +
#   scale_fill_viridis_d(option = "plasma") +
#   guides(fill = guide_legend(title = NULL))
# Print plots
# print(plot_population_group)
# print(plot_population_group2)

ggsave(
  filename = paste0("ZIKA_serop_group_QA.png"),
  plot = plot_population_group,
  device = png,
  # path = file_path,
  scale = 1,
  height = 200,
  width = 400,
  units = "mm",
  dpi = 300)

ggsave(
  filename = paste0("ZIKA_serop_group2_QA.png"),
  plot = plot_population_group2,
  device = png,
  # path = file_path,
  scale = 1,
  height = 200,
  width = 400,
  units = "mm",
  dpi = 300)

ggsave(
  filename = paste0("ZIKA_serop_group3_QA.png"),
  plot = plot_population_group3,
  device = png,
  # path = file_path,
  scale = 1,
  height = 200,
  width = 400,
  units = "mm",
  dpi = 300)

#filter for QA score
seroprevalence_data <- filter(seroprevalence_data, !qa_score < 0.5)

# Create a mean year from start and end
df.gen <- seroprevalence_data %>%
  mutate(
    survey_start_year = as.numeric(population_study_start_year),
    survey_end_year = as.numeric(population_study_end_year),
    mean_year = as.integer(rowMeans(cbind(survey_start_year, survey_end_year), na.rm = TRUE)) #no middle year
  )

# Filter for countries with >1 study and >1 unique year
# country_study_counts <- df.gen %>%
#   filter(!is.na(mean_year)) %>%
#   group_by(population_country) %>%
#   summarise(
#     study_count = n(),
#     unique_years = n_distinct(mean_year)
#   ) %>%
#   filter(study_count > 1, unique_years > 1)

# df_filtered <- df.gen %>%
#   filter(population_country %in% country_study_counts$population_country)

# plot_3 <- ggplot(df.gen, aes(x = population_location, y = central)) +
#   geom_point(aes(colour = as.factor(mean_year), shape = parameter_type), alpha = 0.7) +
#   # geom_line(aes(group = population_country, colour = as.factor(mean_year)), linewidth = 0.8) +
#   theme_bw() +
#   facet_wrap(~population_country, scales = "free_x" )+
#   labs(
#     # title = "Zika Seroprevalence Over Time by Country",
#     x = NULL,
#     y = "Seroprevalence %",
#     colour = NULL
#   )+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave(
#   paste0("ZIKA_serop_type3.png"),
#   plot = plot_3,
#   device = png,
#   # path = file_path,
#   scale = 1,
#   height = 200,
#   width = 400,
#   units = "mm",
#   dpi = 300)
