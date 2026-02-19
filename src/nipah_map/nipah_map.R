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

# *============================================================================*
# *------------------------------ Orderly config ------------------------------*
orderly_dependency(
  "nipah_serology", "latest", "sero_forest_pop_group_cols.rds"
)

orderly_dependency(
  "nipah_map_prep", "latest",
  c("locations_with_cases_and_deaths.rds",
    "l0_shapefile_with_cases_and_deaths.rds",
    "l1_shapefile_with_cases_and_deaths.rds",
    "l2_shapefile_with_cases_and_deaths.rds"
    )
)

locations_with_cases_and_deaths <- readRDS("locations_with_cases_and_deaths.rds")

orderly_artefact(
  description = "nipah-specific figures",
  files = c("nipah_outbreaks_map.png")
)


orderly_shared_resource("World_Bank_Official_Boundaries_adm0/")
orderly_shared_resource("World_Bank_Official_Boundaries_adm1/")
orderly_shared_resource("World_Bank_Official_Boundaries_adm2/")
orderly_shared_resource("World_Bank_Official_Boundaries_Ocean_Mask/")

# prepare shapefiles for maps
l0_in <- readRDS("l0_shapefile_with_cases_and_deaths.rds")
l1_in <- readRDS("l1_shapefile_with_cases_and_deaths.rds")
l2_in <- readRDS("l2_shapefile_with_cases_and_deaths.rds")
om <- read_sf("World_Bank_Official_Boundaries_Ocean_Mask/WB_GAD_ocean_mask.shp")




# North India + Bangladesh from outbreak data
gg_northern_india_bangladesh <- ggplot() +
  geom_sf(
    data = filter(l1_in, COUNTRY %in% c("India", "Bangladesh")),
    aes(fill = total_cases),
    lwd = 0.4, col = "darkgrey", 
    na.rm = TRUE
  ) +
  geom_sf(
    data = filter(l2_in, NAM_2 %in% locations_with_cases_and_deaths$map_location),
    aes(fill = total_cases),
    lwd = 0.2, col = "darkgrey", 
    na.rm = TRUE
  ) +
  geom_sf(
    data = filter(l0_in, COUNTRY %in% c("India", "Bangladesh")),
    lwd = 0.5, col = "black", fill = NA
  ) +
  geom_sf(
    data = om, lwd = 0.001, col = "lightgrey", fill = "lightblue", alpha = 0.3
  ) +
  coord_sf(xlim = c(83, 92), ylim = c(20, 28), expand = FALSE) +
  scale_fill_gradient(
    low = "palegreen", high = "darkblue",
    trans = scales::trans_new("log2p",
      transform = function(x) log2(x + 1),
      inverse = function(x) 2^x - 1
    ),
    breaks = c(0, 1, 4, 8, 16, 64, 240),
    name = "Total cases", na.value = "grey95", limits = c(0, 240)
  ) +
  labs(title = "Northern India & Bangladesh\nreported outbreaks") +
  theme_bw() +
  guides(
    fill = guide_none(),
    color = guide_none()
  )




# Kerala
gg_kerala <- ggplot() +
  geom_sf(data = l1 %>% filter(COUNTRY %in% c("India")), lwd = 0.4, col = "darkgrey", aes(fill = total_cases), na.rm = TRUE) +
  geom_sf(data = l2 %>% filter(NAM_2 %in% locations_with_cases_and_deaths$map_location), lwd = 0.2, col = "darkgrey", aes(fill = total_cases), na.rm = TRUE) +
  geom_sf(data = l0_in %>% filter(COUNTRY %in% c("India")), lwd = 0.5, col = "black", fill = NA) +
  geom_sf(data = om, lwd = 0.001, col = "lightgrey", fill = "lightblue", alpha = 0.3) +
  coord_sf(xlim = c(74, 78), ylim = c(8, 13), expand = FALSE) +
  scale_fill_gradient(
    low = "palegreen", high = "darkblue",
    trans = scales::trans_new("log2p",
      transform = function(x) log2(x + 1),
      inverse = function(x) 2^x - 1
    ),
    breaks = c(0, 1, 4, 8, 16, 64, 240),
    name = "Total cases", na.value = "grey95", limits = c(0, 240)
  ) +
  labs(title = "Kerala\nreported outbreaks") +
  theme_bw() +
  guides(
    fill = guide_none(),
    color = guide_none()
  )



# Malaysia & Singapore
gg_malaysia_singapore <- ggplot() +
  geom_sf(data = l1 %>% filter(COUNTRY %in% c("Malaysia", "Singapore")), lwd = 0.4, col = "darkgrey", aes(fill = total_cases), na.rm = TRUE) +
  geom_sf(data = l2 %>% filter(NAM_2 %in% locations_with_cases_and_deaths$map_location), lwd = 0.2, col = "darkgrey", aes(fill = total_cases), na.rm = TRUE) +
  geom_sf(data = l0_in %>% filter(COUNTRY %in% c("Malaysia", "Singapore")), lwd = 0.5, col = "black", fill = NA) +
  geom_sf(data = l0 %>% filter(COUNTRY %in% c("Singapore")), lwd = 0.7, col = "black", aes(fill = total_cases), na.rm = TRUE) +
  geom_sf(data = om, lwd = 0.001, col = "lightgrey", fill = "lightblue", alpha = 0.3) +
  coord_sf(xlim = c(100, 105), ylim = c(1, 7), expand = FALSE) +
  scale_fill_gradient(
    low = "palegreen", high = "darkblue",
    trans = scales::trans_new("log2p",
      transform = function(x) log2(x + 1),
      inverse = function(x) 2^x - 1
    ),
    breaks = c(0, 1, 4, 8, 16, 64, 240),
    name = "Total cases", na.value = "grey95", limits = c(0, 240)
  ) +
  labs(title = "Malaysian peninsular\nreported outbreaks") +
  theme_bw() +
  guides(
    fill = guide_none(),
    color = guide_none()
  )


# Philippines
gg_philippines <- ggplot() +
  geom_sf(data = l1 %>% filter(COUNTRY %in% c("Philippines")), lwd = 0.4, col = "darkgrey", aes(fill = total_cases), na.rm = TRUE) +
  geom_sf(data = l2 %>% filter(NAM_2 %in% locations_with_cases_and_deaths$map_location), lwd = 0.2, col = "darkgrey", aes(fill = total_cases), na.rm = TRUE) +
  geom_sf(data = l0_in %>% filter(COUNTRY %in% c("Philippines")), lwd = 0.5, col = "black", fill = NA) +
  geom_sf(data = om, lwd = 0.001, col = "lightgrey", fill = "lightblue", alpha = 0.3) +
  coord_sf(xlim = c(115, 130), ylim = c(5, 20), expand = FALSE) +
  scale_fill_gradient(
    low = "palegreen", high = "darkblue",
    trans = scales::trans_new("log2p",
      transform = function(x) log2(x + 1),
      inverse = function(x) 2^x - 1
    ),
    breaks = c(0, 1, 4, 8, 16, 64, 240),
    name = "Total cases", na.value = "grey95", limits = c(0, 240)
  ) +
  labs(title = "Philippines\nreported outbreaks") +
  theme_bw() +
  guides(
    fill = guide_none(),
    color = guide_none()
  )






map_theme <- theme(
  plot.margin = margin(2, 2, 2, 2),
  legend.position = "none"
)

layout_design <-
   "AAABBBEEEE
    AAABBBEEEE
    CCCDDDEEEE
   CCCDDDEEEE"

text_size <- 13

sero_forest_pop_group <- readRDS("sero_forest_pop_group_cols.rds")
map_plot <- gg_northern_india_bangladesh + gg_kerala +
  gg_malaysia_singapore + gg_philippines + (sero_forest_pop_group +
    theme(legend.position = c(0.85, 0.51))) +
  plot_layout(design = layout_design) +
  plot_annotation(tag_levels = "A") +
  plot_layout(byrow = FALSE)

ggsave("nipah_outbreaks_map.png", plot = map_plot, width = 19, height = 22)

ggsave("northern_india_bangladesh.png",
  plot = gg_northern_india_bangladesh,
  width = 10.4, height = 10
)

ggsave("kerala.png", plot = gg_kerala, width = 6.4, height = 8)
ggsave("malaysia_singapore.png", plot = gg_malaysia_singapore, width = 8.4, height = 10)
ggsave("philippines.png", plot = gg_philippines, width = 9.9, height = 10)
