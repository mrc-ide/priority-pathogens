library(dplyr)
library(ggplot2)
library(ggspatial)
library(orderly)
library(sf)

orderly_dependency(
  "nipah_iedcr_map_prep",
  "latest",
  c(
    "l2_bangladesh_iedcr_shapefile_with_cases.rds",
    "bangladesh_hospital_locs.rds"
  )
)

orderly_shared_resource("World_Bank_Official_Boundaries_adm0/")
orderly_shared_resource("World_Bank_Official_Boundaries_adm1/")
orderly_shared_resource("World_Bank_Official_Boundaries_Ocean_Mask/")

# prepare shapefiles for maps
om <- read_sf("World_Bank_Official_Boundaries_Ocean_Mask/WB_GAD_ocean_mask.shp")

l0_in <- read_sf("World_Bank_Official_Boundaries_adm0/WB_GAD_ADM0.shp") |>
  rename(COUNTRY = NAM_0)

l1_in <- read_sf("World_Bank_Official_Boundaries_adm1/WB_GAD_ADM1.shp") |>
  rename(COUNTRY = NAM_0)

l2_iedcr_bangladesh_centroids <- l2_iedcr_bangladesh |>
  filter(NAM_2 %in% iedcr_locations_with_cases$District) |>
  st_centroid()

bangladesh_hospitals <-  readRDS("bangladesh_hospital_locs.rds")

bangladesh_hospitals_sf <- bangladesh_hospitals %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)


# Explicitly transform bounds - clearer and more reliable
title_size <- 17
text_size <- 15

crs_latlong <- 4326
crs_scale <- 3857

hospital_label_offsets <- tibble::tribble(
  ~name , ~nudge_x , ~nudge_y , ~hjust ,
  "Bangabandhu Sheikh Mujib Medical College Hospital", 5000, 10000, 0,
  "Bogra Medical College Hospital", 5000, 10000, 0 ,
  "Manikganj Sadar Hospital", 5000 ,10000, 0,
  "Sher-E-Bangla Medical College Hospital", 5000, 10000, 0,
  "Joypurhat Sadar Hospital",5000, 10000, 0,
  "Rajbari General Hospital",-12000, NA, 0.5
)

hospital_labels <- bangladesh_hospitals_sf |>
  st_transform(crs_scale) |>
  left_join(hospital_label_offsets, by = "name") |>
  mutate(
    nudge_x = coalesce(nudge_x, 0),
    nudge_y = coalesce(nudge_y, 14000),
    hjust = coalesce(hjust, 0.5)
  )

hospital_coords <- st_coordinates(hospital_labels)
hospital_labels$X <- hospital_coords[, "X"] + hospital_labels$nudge_x
hospital_labels$Y <- hospital_coords[, "Y"] + hospital_labels$nudge_y

l2_iedcr_bangladesh_centroids_plot <- l2_iedcr_bangladesh_centroids |>
  st_transform(crs_scale)

district_dot_offsets <- tibble::tribble(
  ~NAM_2, ~nudge_x, ~nudge_y
)

district_dot_coords <- st_coordinates(l2_iedcr_bangladesh_centroids_plot)
l2_iedcr_bangladesh_centroids_plot <- l2_iedcr_bangladesh_centroids_plot |>
  left_join(district_dot_offsets, by = "NAM_2") |>
  mutate(
    nudge_x = coalesce(nudge_x, 0),
    nudge_y = coalesce(nudge_y, 0),
    X = district_dot_coords[, "X"] + nudge_x,
    Y = district_dot_coords[, "Y"] + nudge_y
  )

## Bangladesh IEDCR surveillance
bbox_ll <- st_bbox(
  c(xmin = 88, xmax = 93, ymin = 21.5, ymax = 26.7),
  crs = crs_latlong
)
bbox_utm <- st_bbox(st_transform(st_as_sfc(bbox_ll), crs_scale))
gg_idecr_bangladesh <- ggplot() +
  geom_sf(
    data = l2_iedcr_bangladesh |> filter(!is.na(tc_l2)),
    lwd = 0.2,
    col = "gray50",
    fill = NA,
    na.rm = TRUE
  ) +
  geom_sf(
    data = l1_in |>
      filter(
        COUNTRY %in% c("Bangladesh")
      ),
    lwd = 0.3,
    col = "#654321CC",
    fill = NA,
    na.rm = TRUE
  ) +
  geom_sf(data = l0_in, lwd = 0.55, col = "black", fill = NA, na.rm = TRUE) +
  geom_sf(
    data = om,
    lwd = 0.001,
    col = "lightgrey",
    fill = "lightblue",
    alpha = 0.3
  ) +
  scale_size_continuous(
    name = "Total Cases",
    limits = c(1, 75),
    range = c(2, 15),
    breaks = c(1, 10, 20, 30, 75),
    ## guide = "none"
  ) +
  labs(size = "Total cases", x = "", y = "") +
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    text_cex = 1.2,
    line_width = 1,
    height = unit(0.2, "cm")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 23, hjust = 0.5),
    legend.background = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.70, 0.95),
    legend.direction = "horizontal",
    text = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_sf_text(
    data = bangladesh_hospitals_sf,
    aes(label = "\u2695", color = surveillance_period), #plus "\u271A"
    size = 13,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_text(
    data = hospital_labels,
    aes(
      x = X,
      y = Y,
      label = name,
      color = surveillance_period,
      hjust = hjust
    ),
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  coord_sf(
    crs = crs_scale,
    xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
    ylim = c(bbox_utm["ymin"], bbox_utm["ymax"]),
    expand = FALSE
  ) +
  # For a better colour legend instead of text
  geom_point(
    data = hospital_labels,
    aes(x = X, y = Y, color = surveillance_period),
    shape = 15,
    size = 0.0,
    show.legend = TRUE
  ) +
  scale_color_manual(
    values = c(
      "2006 - ongoing" = "#0A2A43",
      "2018 - ongoing" = "#0099B4",
      "2020 - ongoing" = "#925E9F",
      "2021 - ongoing" = "#42B540",
      "2006 - 2009" = "gray50"
    ),
    name = "Surveillance period"
  ) +
  ggtitle("") +
  guides(
    size = guide_legend(order = 1),
    color = guide_legend(
      nrow = 2,
      byrow = TRUE,
      override.aes = list(
        label = NULL,
        shape = 15,
        size = 5
      )
    )
  ) +
  # Circles at centroids - Layer 2
  geom_point(
    data = l2_iedcr_bangladesh_centroids_plot,
    aes(x = X, y = Y, size = total_cases),
    fill = "red",
    color = "red",
    alpha = 0.5,
    shape = 21,
    na.rm = TRUE
  )

ggsave(
  "gg_idecr_bangladesh.png",
  plot = gg_idecr_bangladesh,
  width = 14.25,
  height = 16,
  dpi = 400
)

