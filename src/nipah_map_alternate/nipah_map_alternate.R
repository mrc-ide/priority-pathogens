library(cowplot)
library(dplyr)
library(ggforce)
library(ggplot2)
library(ggsci)
library(ggspatial)
library(grid)
library(gridExtra)
library(orderly2)
library(patchwork)
library(png)
library(ragg)
library(sf)
library(stringr)

orderly_dependency(
  "nipah_serology", "latest",
  c("sero_forest_pop_group_cols.rds", "sero_forest_pop_group_cols.png")
)

orderly_dependency(
  "nipah_map_prep", "latest",
  c(
    "locations_with_cases_and_deaths.rds",
    "l0_shapefile_with_cases_and_deaths.rds",
    "l1_shapefile_with_cases_and_deaths.rds",
    "l2_shapefile_with_cases_and_deaths.rds"
  )
)

orderly_dependency("nipah_map_prep", "latest", "locations_with_cases_and_deaths.rds")

locations_with_cases_and_deaths <- readRDS("locations_with_cases_and_deaths.rds")

orderly_shared_resource("World_Bank_Official_Boundaries_adm0/")
orderly_shared_resource("World_Bank_Official_Boundaries_adm1/")
orderly_shared_resource("World_Bank_Official_Boundaries_adm2/")
orderly_shared_resource("World_Bank_Official_Boundaries_Ocean_Mask/")

# prepare shapefiles for maps
l0_in <- readRDS("l0_shapefile_with_cases_and_deaths.rds")
l1_in <- readRDS("l1_shapefile_with_cases_and_deaths.rds")
l2_in <- readRDS("l2_shapefile_with_cases_and_deaths.rds")
om <- read_sf("World_Bank_Official_Boundaries_Ocean_Mask/WB_GAD_ocean_mask.shp")

# Calculate centroids 
sf_use_s2(FALSE)
l1_centroids <- filter(
  l1_in, COUNTRY %in% c("India", "Bangladesh", "Malaysia", "Singapore", "Philippines")
) %>% st_centroid()

l2_centroids <- filter(l2_in, NAM_2 %in% locations_with_cases_and_deaths$map_location) %>%
  st_centroid()


                                        # Explicitly transform bounds - clearer and more reliable
crs_latlong <- 4326
crs_scale <- 3857

pall <- ggplot() +
  # Circles at centroids - Layer 1
  geom_sf(
    data = l1_centroids,
    aes(size = total_cases),
    fill = "red", 
    col = "red",    alpha = 0.5,
    na.rm = TRUE
  ) +
  # Circles at centroids - Layer 2
  geom_sf(
    data = l2_centroids,
    aes(size = total_cases),
    fill = "red", col = "red",
    alpha = 0.5,
    na.rm = TRUE
  ) +
    geom_sf(
    data = filter(l0_in, COUNTRY %in% c("India", "Bangladesh")),
    lwd = 0.5, col = "black", fill = NA
  ) +
    geom_sf(
      data = om, lwd = 0.001, col = "lightgrey", fill = "lightblue", alpha = 0.3
    ) +
  scale_size_continuous(
      name = "Total Cases",
    limits = c(1, 235),
    range = c(2, 15),
    breaks = c(1, 10, 50, 100, 200),
    ##guide = "none"
    ) +
  labs(size = "Total cases") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_bw() +
  theme(legend.position = "top", legend.key.height = unit(0.3, "cm"))

legend <- get_legend(pall)

## Now remove legend from the individual plots
pall <- pall + theme(legend.position = "none")

bbox_ll <- st_bbox(c(xmin = 83, xmax = 92, ymin = 20, ymax = 28), crs = crs_latlong)
bbox_utm <- st_bbox(st_transform(st_as_sfc(bbox_ll), crs_scale))

pindia_bangladesh <- pall +
  coord_sf(
  crs = crs_scale, # Only need to specify output CRS
  xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
  ylim = c(bbox_utm["ymin"], bbox_utm["ymax"]),
  expand = FALSE
) + ggtitle("India & Bangladesh") 


## Kerala
bbox_ll <- st_bbox(c(xmin = 74, xmax = 78, ymin = 8, ymax = 13), crs = crs_latlong)
bbox_utm <- st_bbox(st_transform(st_as_sfc(bbox_ll), crs_scale))

pkerala <- pall +
  coord_sf(
    crs = crs_scale, # Only need to specify output CRS
    xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
    ylim = c(bbox_utm["ymin"], bbox_utm["ymax"]),
    expand = FALSE
  ) + ggtitle("Kerala")

## Malaysia & Singapore
bbox_ll <- st_bbox(c(xmin = 100, xmax = 105, ymin = 1, ymax = 7), crs = crs_latlong)
bbox_utm <- st_bbox(st_transform(st_as_sfc(bbox_ll), crs_scale))
pmalaysia_singapore <- pall +
  coord_sf(
    crs = crs_scale, # Only need to specify output CRS
    xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
    ylim = c(bbox_utm["ymin"], bbox_utm["ymax"]),
    expand = FALSE
  ) + ggtitle("Malaysia & Singapore")

## Philippines
bbox_ll <- st_bbox(c(xmin = 116, xmax = 130, ymin = 5, ymax = 20), crs = crs_latlong)
bbox_utm <- st_bbox(st_transform(st_as_sfc(bbox_ll), crs_scale))
pphilippines <- pall +
  coord_sf(
    crs = crs_scale, # Only need to specify output CRS
    xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
    ylim = c(bbox_utm["ymin"], bbox_utm["ymax"]),
    expand = FALSE
  ) + ggtitle("Philippines")

## Put the plots together
pfinal <- pindia_bangladesh + pkerala +  pmalaysia_singapore + pphilippines +
  plot_layout(ncol = 2) 


width <- 12
height <- 10
ggsave("pfinal.png", pfinal, width = width, height = height, dpi = 300, bg = "white")
ggsave("pindia_bangladesh.png", pindia_bangladesh, width = width, height = height, bg = "white")
ggsave("pkerala.png", pkerala, width = width, height = height, bg = "white")
ggsave("pmalaysia_singapore.png", pmalaysia_singapore, width = width, height = height, bg = "white")
ggsave("pphilippines.png", pphilippines, width = width, height = height, bg = "white")
ggsave("map_legend.png", legend)

orderly_artefact(
  files = c("pfinal.png", "pindia_bangladesh.png", "pkerala.png",
            "pmalaysia_singapore.png", "pphilippines.png",
            "map_legend.png")
)

orderly_resource("nipah_map.tex")
system("pdflatex nipah_map.tex")
