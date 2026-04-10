library(cowplot)
library(dplyr)
library(ggforce)
library(ggplot2)
library(ggsci)
library(ggspatial)
library(grid)
library(gridExtra)
library(magick)
library(orderly2)
library(patchwork)
library(png)
library(ragg)
library(sf)
library(stringr)

orderly_dependency(
  "nipah_serology",
  "latest",
  c("sero_forest_pop_group_cols.rds")
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
l0_centroids <- filter(
  l0_in, COUNTRY %in% c( "Singapore")
) %>% st_centroid()

l1_centroids <- filter(
  l1_in, COUNTRY %in% c("India", "Bangladesh", "Malaysia", "Singapore", "Philippines")
) %>% st_centroid()

l2_centroids <- filter(l2_in, NAM_2 %in% locations_with_cases_and_deaths$map_location) %>%
  st_centroid()


                                        # Explicitly transform bounds - clearer and more reliable
title_size <- 17
text_size <- 15

crs_latlong <- 4326
crs_scale <- 3857

pall <- ggplot() +
    geom_sf(
    data = l0_centroids,
    aes(size = total_cases),
    fill = "red",
    col = "red", alpha = 0.5,
    na.rm = TRUE
  ) +
  # Circles at centroids - Layer 1
  geom_sf(
    data = l1_centroids,
    aes(size = total_cases),
    fill = "red",
    col = "red", alpha = 0.5,
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
  geom_sf(data = l2_in |> filter(!is.na(tc_l2)),
          lwd = 0.2, col = "gray50", fill = NA, na.rm = TRUE) +
  geom_sf(data = l1_in |>
            filter(COUNTRY %in% c("Singapore", "India", "Bangladesh",
                                  "Malaysia", "Philippines")),
          lwd = 0.3, col = "#654321CC", fill = NA, na.rm = TRUE) +
  geom_sf(data = l0_in, lwd = 0.55, col = "black", fill = NA, na.rm = TRUE) +
  geom_sf(
    data = om, lwd = 0.001, col = "lightgrey", fill = "lightblue", alpha = 0.3
  ) +
  scale_size_continuous(
    name = "Total Cases",
    limits = c(1, 235),
    range = c(2, 15),
    breaks = c(1, 10, 50, 100, 200),
    ## guide = "none"
  ) +
  labs(size = "Total cases") +
  annotation_scale(location = "bl", width_hint = 0.3,
                   text_cex = 1.2,
                   line_width = 1,
                   height = unit(0.2, "cm")) +
  theme_bw() +
  theme(
    plot.title=element_text(size=title_size, hjust = 0.5),
    legend.background = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.5),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    text=element_text(size=text_size),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


bbox_ll <- st_bbox(c(xmin = 85.5, xmax = 92, ymin = 20, ymax = 28), crs = crs_latlong)
bbox_utm <- st_bbox(st_transform(st_as_sfc(bbox_ll), crs_scale))

pindia_bangladesh <- pall +
  coord_sf(
  crs = crs_scale, # Only need to specify output CRS
  xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
  ylim = c(bbox_utm["ymin"], bbox_utm["ymax"]),
  expand = FALSE
  ) + ggtitle("West Bengal & Bangladesh") +
  theme(legend.position = "none")


## Kerala
bbox_ll <- st_bbox(c(xmin = 74, xmax = 78, ymin = 8, ymax = 13), crs = crs_latlong)
bbox_utm <- st_bbox(st_transform(st_as_sfc(bbox_ll), crs_scale))

pkerala <- pall +
  coord_sf(
    crs = crs_scale, # Only need to specify output CRS
    xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
    ylim = c(bbox_utm["ymin"], bbox_utm["ymax"]),
    expand = FALSE
  ) +
  scale_x_continuous(breaks = seq(74, 78, by = 1)) +
  scale_y_continuous(breaks = seq(8, 13, by = 1)) +
  ggtitle("Kerala")

## Malaysia & Singapore
bbox_ll <- st_bbox(c(xmin = 100, xmax = 105.05, ymin = 1, ymax = 7), crs = crs_latlong)
bbox_utm <- st_bbox(st_transform(st_as_sfc(bbox_ll), crs_scale))
pmalaysia_singapore <- pall +
  coord_sf(
    crs = crs_scale, # Only need to specify output CRS
    xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
    ylim = c(bbox_utm["ymin"],
             bbox_utm["ymax"]),
    expand = FALSE
  ) + ggtitle("Peninsular Malaysia & Singapore")  +
  theme(legend.position = "none")

## Philippines
bbox_ll <- st_bbox(c(xmin = 116, xmax = 129.4, ymin = 5, ymax = 20), crs = crs_latlong)
bbox_utm <- st_bbox(st_transform(st_as_sfc(bbox_ll), crs_scale))
pphilippines <- pall +
  coord_sf(
    crs = crs_scale, # Only need to specify output CRS
    xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
    ylim = c(bbox_utm["ymin"], bbox_utm["ymax"]),
    expand = FALSE
  ) + ggtitle("Philippines") +
  theme(legend.position = "none")



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


files = c("pfinal.png", "pindia_bangladesh.png", "pkerala.png",
            "pmalaysia_singapore.png", "pphilippines.png"
          )
for (f in files) {
  img <- magick::image_read(f)
  img_trimmed <- magick::image_trim(img)
  magick::image_write(img_trimmed, f)
}
orderly_artefact(files = files)

layout_design <-
  "GGE
   ABE
   CDE
   FFE"

blank_tag <- ggplot() +
  theme_void()

sero_forest_pop_group <- readRDS("sero_forest_pop_group_cols.rds")
map_plot <- pindia_bangladesh +
  pkerala +
  pmalaysia_singapore +
  pphilippines +
  (sero_forest_pop_group +
    theme(legend.position = c(0.8, 0.425))) +
  plot_spacer() +
  # plot_spacer() +
  blank_tag +
  plot_layout(design = layout_design) +
  plot_annotation(tag_levels = list(c("", "", "", "", "B", "", "A"))) +
  plot_layout(
    byrow = FALSE,
    heights = c(0.0, 0.95, 0.86, 0.075),
    widths = c(1, 0.7, 1.25)
  ) &
  theme(plot.tag.position = "topleft", plot.tag = element_text(size = 25))

ggsave(
  "nipah_outbreaks_map.png",
  plot = map_plot,
  width = 25,
  height = 16.2,
  dpi = 300
)

ggsave("nipah_outbreaks_map.pdf",
       plot = map_plot,
       width = 25,
       height = 16.2,
       device = cairo_pdf)

ggsave("pindia_bangladesh.pdf",
       plot = pindia_bangladesh,
       width = 6.5,
       height = 8,
       device = cairo_pdf)

ggsave("pkerala.pdf",
       plot = pkerala,
       width = 7,
       height = 8,
       device = cairo_pdf)

ggsave("pmalaysia_singapore.pdf",
       plot = pmalaysia_singapore,
       width = 6,
       height = 7,
       device = cairo_pdf)

ggsave("pphilippines.pdf",
       plot = pphilippines,
       width = 6,
       height = 7,
       device = cairo_pdf)
