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

orderly_dependency("nipah_map_prep", "latest", "locations_with_cases_and_deaths.rds")

locations_with_cases_and_deaths <- readRDS("locations_with_cases_and_deaths.rds")

orderly_artefact(
  description = "nipah-specific figures",
  files = c("nipha_outbreaks_map.png")
)



# prepare shapefiles for maps
l0_in <- read_sf("../../shared/World_Bank_Official_Boundaries_adm0/WB_GAD_ADM0.shp") %>% # this is the shapefile with country boundaries
  rename(COUNTRY = NAM_0) # store country names in column COUNTRY
#

#
l1_in <- read_sf("../../shared/World_Bank_Official_Boundaries_adm1/WB_GAD_ADM1.shp") %>% # this is the shapefile with level 1 regions
  rename(COUNTRY = NAM_0) %>%
  mutate(COUNTRY = case_when( # country names must be consistent between shapefiles
    COUNTRY == "Cabo Verde" ~ "Cape Verde",
    COUNTRY == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    COUNTRY == "Guinea Bissau" ~ "Guinea-Bissau",
    TRUE ~ COUNTRY
  )) %>%
  rename(REG_CODE = ADM1CD_c) # store region codes, e.g. SL01, in column REG_CODE

l2_in <- read_sf("../../shared/World_Bank_Official_Boundaries_adm2/WB_GAD_ADM2.shp") %>% # this is the shapefile with level 1 regions
  rename(COUNTRY = NAM_0)

om <- read_sf("../../shared/World_Bank_Official_Boundaries_Ocean_Mask/WB_GAD_ocean_mask.shp")



# what is the 'true' number of cases we assign, how do we demonstrate time dimension?

# outbreaks %>% filter(!is_duplicate) %>% group_by(outbreak_location, outbreak_start_year) %>% summarise(n=n()) %>% filter(n>1)

# world               <- ne_countries(scale = "medium", returnclass = "sf")
# worldmap            <- st_transform(world, crs = st_crs(l0))

# southeast_asia_cropped <- st_crop(worldmap, xmin = -20, xmax = 45,
#                          ymin = 30, ymax = 73)


l0 <- l0_in %>%
  left_join(
    rename(
      locations_with_cases_and_deaths, tc_l0 = tot_cases, td_l0 = tot_deaths),
    by = c("COUNTRY" = "map_location")) %>%
  mutate(total_cases = tc_l0, total_deaths = td_l0)

l1 <- l1_in %>%
  left_join(
    rename(
      locations_with_cases_and_deaths, tc_l1 = tot_cases, td_l1 = tot_deaths),
    by = c("NAM_1" = "map_location")) %>%
  mutate(total_cases = tc_l1, total_deaths = td_l1)

l2 <- left_join(
  l2_in, 
    rename(
      locations_with_cases_and_deaths, tc_l2 = tot_cases, td_l2 = tot_deaths),
    by = c("NAM_2" = "map_location")) %>%
  left_join(
    rename(locations_with_cases_and_deaths, tc_l1 = tot_cases, td_l1 = tot_deaths),
    by = c("NAM_1" = "map_location")) %>%
  left_join(
    rename(locations_with_cases_and_deaths, tc_l0 = tot_cases, td_l0 = tot_deaths),
    by = c("COUNTRY" = "map_location")) %>%
  mutate(
    total_cases = coalesce(tc_l2, tc_l1, tc_l0),
    total_deaths = coalesce(td_l2, td_l1, td_l0)
  ) %>%
  filter(!is.na(total_cases)) # remove visual noise



location_mapping_bangladesh <- tribble(
  ~location, ~iso3, ~adm2_code, ~district, ~division,
  # ---- Bangladesh Districts ----
  "Jhalakati", "BGD", "BGD008004", "Jhalokathi", "Barishal",
  "Cumilla", "BGD", "BGD002003", "Cumilla", "Chittagong",
  "Dhaka", "BGD", "BGD003009", "Dhaka", "Dhaka",
  "Faridpur", "BGD", "BGD003002", "Faridpur", "Dhaka",
  "Gopalganj", "BGD", "BGD008002", "Gopalganj", "Barishal",
  "Madaripur", "BGD", "BGD008003", "Madaripur", "Barishal",
  "Manikganj", "BGD", "BGD003006", "Manikganj", "Dhaka",
  "Rajbari", "BGD", "BGD003008", "Rajbari", "Dhaka",
  "Sariatpur", "BGD", "BGD003011", "Shariatpur", "Dhaka",
  "Tangail", "BGD", "BGD003010", "Tangail", "Dhaka",
  "Chuadanga", "BGD", "BGD004007", "Chuadanga", "Khulna",
  "Jhenaidah", "BGD", "BGD004006", "Jhenaidah", "Khulna",
  "Khulna", "BGD", "BGD004005", "Khulna", "Khulna",
  "Kushtia", "BGD", "BGD004008", "Kushtia", "Khulna",
  "Magura", "BGD", "BGD004009", "Magura", "Khulna",
  "Meherpur", "BGD", "BGD004004", "Meherpur", "Khulna",
  "Norail", "BGD", "BGD004010", "Narail", "Khulna",
  "Mymensingh", "BGD", "BGD009003", "Mymensingh", "Mymensingh",
  "Bogura", "BGD", "BGD006001", "Bogra", "Rajshahi",
  "Chapainawabganj", "BGD", "BGD006008", "Chapai Nawabganj", "Rajshahi",
  "Naogaon", "BGD", "BGD006004", "Naogaon", "Rajshahi",
  "Natore", "BGD", "BGD006003", "Natore", "Rajshahi",
  "Pabna", "BGD", "BGD006007", "Pabna", "Rajshahi",
  "Rajshahi", "BGD", "BGD006006", "Rajshahi", "Rajshahi",
  "Dinajpur", "BGD", "BGD005004", "Dinajpur", "Rangpur",
  "Gaibandha", "BGD", "BGD005002", "Gaibandha", "Rangpur",
  "Jaipurhat", "BGD", "BGD006002", "Joypurhat", "Rajshahi",
  "Kurigram", "BGD", "BGD005006", "Kurigram", "Rangpur",
  "Lalmonirhat", "BGD", "BGD005002", "Lalmonirhat", "Rangpur",
  "Nilphamari", "BGD", "BGD005003", "Nilphamari", "Rangpur",
  "Panchagarh", "BGD", "BGD005008", "Panchagarh", "Rangpur",
  "Rangpur", "BGD", "BGD005005", "Rangpur", "Rangpur",
  "Thakurgaon", "BGD", "BGD005007", "Thakurgaon", "Rangpur"
)


hospitals <- data.frame(
  name = c(
    "Rajshahi Medical College Hospital",
    "Rangpur Medical College Hospital",
    "Bangabandhu Sheikh Mujib Medical College Hospital",
    "Tangail General Hospital",
    "Rajbari General Hospital",
    "Chattogram Medical College Hospital",
    "Khulna Medical College Hospital",
    "Sher e Bangla Medical College Hospital",
    "Mymensingh Medical College Hospital",
    "Sythet IMG Osmani Medical College Hospital",
    "Naogaon Sadar Hospital",
    "Joypurhat Sadar Hospital",
    "Meherpur Sadar Hospital",
    "Manikganj Sadar Hospital",
    "Bogra Medical College Hospital"
  ),
  `Surveillance period` = c(
    "2006 - ongoing",
    "2006 - ongoing",
    "2006 - ongoing",
    "2006 - ongoing",
    "2006 - ongoing",
    "2018 - ongoing",
    "2018 - ongoing",
    "2020 - ongoing",
    "2020 - ongoing",
    "2021 - ongoing",
    "2006 - 2009",
    "2006 - 2009",
    "2006 - 2009",
    "2006 - 2009",
    "2006 - 2009"
  )
)

manual_coords <- data.frame(
  name = hospitals$name,
  lat = c(24.3645, 25.7439, 23.6010, 24.2513, 23.7610, 22.3569, 22.8456, 22.7010, 24.7471, 24.8918, 24.8136, 25.0947, 23.7669, 23.8603, 24.8510),
  long = c(88.6283, 89.2752, 89.8337, 89.9167, 89.6410, 91.7832, 89.5403, 90.3535, 90.4203, 91.8800, 88.9314, 89.0944, 88.6622, 90.0058, 89.3711),
  `Surveillance period` = hospitals$Surveillance.period
)


nipah_bangladesh <- nipah_bangladesh %>%
  left_join(location_mapping_bangladesh, by = c("District" = "location"))

## Bin total cases and use categorical color scale.
nipah_bangladesh$total_cases_binned <-
  cut(nipah_bangladesh$`Grand Total`,
      breaks = c(1, 10, 30, 75),
      right = FALSE,
      order_result = TRUE
      )



l2_bangladesh <- l2_in %>%
  filter(COUNTRY == "Bangladesh") %>%
  left_join(nipah_bangladesh %>% rename(tc_l2 = `Grand Total`), by = c("ADM2CD_c" = "adm2_code")) %>%
  left_join(nipah_bangladesh %>% dplyr::select(-Division) %>% rename(tc_l1 = `Grand Total`), by = c("NAM_1" = "District")) %>%
  mutate(total_cases = coalesce(tc_l2, tc_l1)) # %>%
# filter(!is.na(total_cases))     # remove visual noise

nipah_country <- l2$COUNTRY |> unique()

# Need to fix India, include Bangladesh as a panel so that it can be seen more easily.
# Maybe split this into a multi-panel figure??? Chat with Tristan
gg <- ggplot() +
  geom_sf(data = om, lwd = 0.001, col = "lightgrey", fill = "lightblue", alpha = 0.3) +
  geom_sf(data = l0_in, lwd = 0.5, col = "black", fill = NA) +
  coord_sf(xlim = c(70, 130), ylim = c(-5, 30), expand = FALSE) +
  scale_fill_continuous(na.value = "white") +
  theme_bw()


# Bangladesh inset
gg_bangladesh <- ggplot() +
  geom_sf(data = l2_bangladesh, lwd = 0.2, col = "darkgrey", aes(fill = total_cases), na.rm = TRUE) +
  geom_sf(data = om, lwd = 0.001, col = "lightgrey", fill = "lightblue", alpha = 0.3) +
  geom_sf(data = l0_in, lwd = 0.5, col = "black", fill = NA) +
  coord_sf(xlim = c(87.9, 93), ylim = c(21, 26.7), expand = FALSE) +
  geom_point(data = manual_coords, aes(x = long, y = lat, color = Surveillance.period), size = 3.5, shape = 18) +
  geom_text(data = manual_coords, aes(x = long, y = lat, label = name), hjust = 0, vjust = 0, nudge_y = 0.05, size = 2.5) +
  scale_fill_gradient(
    low = "palegreen", high = "darkblue",
    trans = scales::trans_new("log2p",
      transform = function(x) log2(x + 1),
      inverse = function(x) 2^x - 1
    ),
    breaks = c(0, 1, 4, 8, 16, 64, 240),
    name = "Total cases", na.value = "grey95", limits = c(0, 240)
  ) +
  ggsci::scale_color_lancet() +
  xlab("") +
  ylab("") +
  labs(title = "Bangladesh IEDCR Surveillance Data", color = "Surveillance Period") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 5)))



## Bangladesh inset with categorical scale
gg_bangladesh <- ggplot() +
  geom_sf(
    data = l2_bangladesh, lwd = 0.2, col = "darkgrey", aes(fill = total_cases_binned.x), na.rm = TRUE
  ) +
  geom_sf(data = om, lwd = 0.001, col = "lightgrey", fill = "lightblue", alpha = 0.3) +
  geom_sf(data = l0_in, lwd = 0.5, col = "black", fill = NA) +
  coord_sf(xlim = c(87.9, 93), ylim = c(21, 26.7), expand = FALSE) +
  geom_point(data = manual_coords, aes(x = long, y = lat, color = Surveillance.period), size = 3.5, shape = 18) +
  geom_text(data = manual_coords, aes(x = long, y = lat, label = name), hjust = 0, vjust = 0, nudge_y = 0.05, size = 2.5) +
  ggsci::scale_color_lancet() +
  xlab("") +
  ylab("") +
  labs(title = "Bangladesh IEDCR Surveillance Data", color = "Surveillance Period") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 5)))




# North India + Bangladesh from outbreak data
gg_northern_india_bangladesh <- ggplot() +
  geom_sf(
    data = filter(l1, COUNTRY %in% c("India", "Bangladesh")),
    aes(fill = total_cases),
    lwd = 0.4, col = "darkgrey", 
    na.rm = TRUE
  ) +
  geom_sf(
    data = filter(l2, NAM_2 %in% locations_with_cases_and_deaths$map_location),
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


## Categorical
gg_northern_india_bangladesh2 <- ggplot() +
  geom_sf(
    data = filter(l1, COUNTRY %in% c("India", "Bangladesh")),
    aes(fill = tot_cases_binned),
    lwd = 0.4, col = "darkgrey", 
    na.rm = TRUE
  ) +
  geom_sf(
    data = filter(l2, NAM_2 %in% locations_with_cases_and_deaths$map_location),
    aes(fill = tot_cases_binned),
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
  labs(title = "Northern India & Bangladesh\nreported outbreaks") +
  theme_bw() +
  guides(
    ##fill = guide_none(),
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
  "AAABBBFFFF
   AAABBBFFFF
   AAABBBFFFF
   AAABBBFFFF
   AAABBBFFFF
   CCDDEEFFFF
   CCDDEEFFFF"

text_size <- 13
gg_bangladesh <- gg_bangladesh +
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

map_plot <- gg_northern_india_bangladesh + gg_bangladesh + gg_kerala +
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
ggsave("bangladesh.png", plot = gg_bangladesh, width = 10, height = 10)
ggsave("kerala.png", plot = gg_kerala, width = 6.4, height = 8)
ggsave("malaysia_singapore.png", plot = gg_malaysia_singapore, width = 8.4, height = 10)
ggsave("philippines.png", plot = gg_philippines, width = 9.9, height = 10)
