# *=================== Nipah severity meta-analysis & plots ===================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(grid)
library(lubridate)
library(metafor)
library(meta)
library(orderly2)
library(patchwork)
library(readr)
library(stringr)
library(tidyr)

# *--------------------------------- Orderly ----------------------------------*
pathogen <- orderly_parameters(pathogen = "NIPAH")

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "outbreaks.csv", "models.csv", "params.csv"))

orderly_shared_resource("nipah_functions.R" = "nipah_functions.R")
orderly_shared_resource("NIPAH_Bangladesh_IEDCR.csv"="NIPAH_Bangladesh_IEDCR.csv")

source("nipah_functions.R")

# *----------------------------- Data preparation -----------------------------*
# IEDCR
cfr_bangladesh <- read_csv('NIPAH_Bangladesh_IEDCR.csv')

cfr_from_bangladesh_surveillance <- cfr_bangladesh |>
  mutate(year_cat = case_when(
    Year %in% 2001:2005 ~ "2001-2005",
    Year %in% 2006:2010 ~ "2006-2010",
    Year %in% 2011:2015 ~ "2011-2015",
    Year %in% 2016:2020 ~ "2016-2020",
    Year %in% 2021:2029 ~ "2021-Present",
    TRUE ~ "Unspecified")) |>
  mutate(cfr_ifr_denominator = Cases,
         cfr_ifr_numerator   = Death,
         CFR                 = ifelse(Cases==0, NA,
                                      cfr_ifr_numerator/cfr_ifr_denominator),
         refs                = Year,
         parameter_value     = CFR,
         parameter_unit      = 'Percentage') |>
  arrange(desc(CFR))

# *------------------------------ Meta-analysis -------------------------------*
# Plot file structure - many plots created in this task so better to create a
# folder structure
dir.create("figures")

# Plot colour
imperial_khaki <- "#EFE58B"
imperial_blue <- rgb(0, 62 / 256, 116 / 256, 0.7)
imperial_light_blue <- "#B9EEFF"
tangerine <- "#EC7300"
crimson <- "#DC143C"

diamond_colour <-"dodgerblue3"
square_colour <- imperial_khaki

text_size <- 13

meta_digits <- 3

# *--------------------- CFR from Bangladesh surveillance ---------------------*
# Overall
cfr_from_bangladesh_meta_data <- cfr_from_bangladesh_surveillance |>
  filter(Cases!=0)
cfr_from_bangladesh_surveillance_yc <- metaprop_wrap(
  cfr_from_bangladesh_meta_data, subgroup = "year_cat", plot_pooled = TRUE,
  sort_by_subg = TRUE, plot_study = FALSE, digits = meta_digits,
  colour = diamond_colour, colour_square = square_colour,
  width = 9500, height = 7000, resolution = 1000)

ggsave(file.path("figures", "figure_3_meta_year_cat_IEDCR.pdf"),
       cfr_from_bangladesh_surveillance_yc$plot, width = 10, height = 8)

# With study breakdown
cfr_from_bangladesh_surveillance_yc_study <- metaprop_wrap(
  cfr_from_bangladesh_meta_data, subgroup = 'year_cat', plot_pooled = TRUE,
  sort_by_subg = TRUE, plot_study = TRUE, digits = meta_digits,
  colour = diamond_colour, colour_square = square_colour,
  width = 10400, height = 11000, resolution = 1000)

ggsave(file.path("figures", "CFR_meta_year_cat_IEDCR_with_year.pdf"),
       cfr_from_bangladesh_surveillance_yc_study$plot,
       width = 7.5, height = 8.25)

# Alternate view:
all_years <- (min(cfr_from_bangladesh_surveillance$Year):
                max(cfr_from_bangladesh_surveillance$Year))
missing_years <- setdiff(all_years,
                         unique(cfr_from_bangladesh_surveillance$Year))

ymin <- 0
ymax <- max(cfr_from_bangladesh_surveillance$CFR * 100, na.rm = TRUE)

missing_df <- tibble(
  xmin = missing_years - 1,
  xmax = missing_years + 0,
  ymin = -Inf,
  ymax = Inf
)

overall_mean_iedcr_cfr <- (
  sum(cfr_from_bangladesh_surveillance$cfr_ifr_numerator)/
    sum(cfr_from_bangladesh_surveillance$cfr_ifr_denominator))

IEDCR_over_time_overall <- cfr_from_bangladesh_surveillance |>
  ggplot(aes(x=Year, y=CFR*100)) +
  geom_hline(yintercept=overall_mean_iedcr_cfr*100,
             linetype="dashed", color="#363737") +
  geom_point(colour="#00468BFF") +
  geom_line(colour="#00468BFF") +
  geom_rect(data = missing_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "green3", alpha = 0.15) +
  geom_text(x = min(cfr_from_bangladesh_surveillance$Year)+2.8, y = 71.8 + 3.5,
            label =sprintf("Overall~CFR==%.0f*'%%'", overall_mean_iedcr_cfr * 100),
            color = "black", hjust = 1, size=3,
            parse = TRUE) +
  labs(x=" Outbreak year", y="CFR (%)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
        text = element_text(size = 18))
IEDCR_over_time_overall

ggsave(file.path("figures", "IEDCR_over_time_overall.pdf"),
       IEDCR_over_time_overall, width = 8, height = 5)
ggsave(file.path("figures", "IEDCR_over_time_overall.png"),
       IEDCR_over_time_overall, width = 8, height = 5)

# -----------------------------
# V2
# -----------------------------
all_years <- (min(cfr_from_bangladesh_surveillance$Year, na.rm = TRUE):
                max(cfr_from_bangladesh_surveillance$Year, na.rm = TRUE))

overall_mean_iedcr_cfr <- (
  sum(cfr_from_bangladesh_surveillance$cfr_ifr_numerator, na.rm = TRUE) /
    sum(cfr_from_bangladesh_surveillance$cfr_ifr_denominator, na.rm = TRUE)
)

# totals
overall_deaths <- sum(cfr_from_bangladesh_surveillance$cfr_ifr_numerator, na.rm = TRUE)
overall_cases  <- sum(cfr_from_bangladesh_surveillance$cfr_ifr_denominator, na.rm = TRUE)

# overall CFR
overall_mean_iedcr_cfr <- overall_deaths / overall_cases

# exact binomial 95% CI
overall_ci <- binom.test(overall_deaths, overall_cases)$conf.int

# (optional) percent versions
overall_mean_iedcr_cfr_pct <- 100 * overall_mean_iedcr_cfr
overall_ci_pct <- 100 * overall_ci

# label (replace years with whatever span you want)
start_year <- min(cfr_from_bangladesh_surveillance$Year, na.rm = TRUE)
end_year   <- max(cfr_from_bangladesh_surveillance$Year, na.rm = TRUE)

overall_label <- sprintf(
  # "%d-%d = %.0f%% (95%% CI: %.0f%%–%.0f%%)",
  "%.1f%% (95%% CI: %.1f%%-%.1f%%)",
  # start_year, end_year,
  overall_mean_iedcr_cfr_pct,
  overall_ci_pct[1],
  overall_ci_pct[2])


# period_stats <- period_df |>
#   rowwise() |>
#   mutate(year_cat = sprintf("%d-%d", start, end),
#          cfr = {
#            d <- cfr_from_bangladesh_surveillance |>
#              filter(Year >= start, Year <= end)
#            if (nrow(d) == 0) NA_real_ else
#              sum(d$cfr_ifr_numerator, na.rm = TRUE) /
#              sum(d$cfr_ifr_denominator, na.rm = TRUE)
#            },
#          y = cfr * 100,
#          label = sprintf("%d-%d = %.0f%%", start, end, y)
#          ) |>
#   ungroup()

period_stats <- tibble(
  start = c(2001, 2006, 2011, 2016, 2021),
  end   = c(2005, 2010, 2015, 2020, 2025)
) |>
  mutate(
    seg_start = start - 1,
    seg_end   = end,
    xmid      = (seg_start + seg_end) / 2-0.175,

    # pre-create columns the loop will fill
    year_cat = NA_character_,
    deaths   = NA_real_,
    cases    = NA_real_,
    cfr      = NA_real_,
    cfr_lwr  = NA_real_,
    cfr_upr  = NA_real_,
    y        = NA_real_,
    y_lwr    = NA_real_,
    y_upr    = NA_real_,
    label    = NA_character_
  )

for (i in seq_len(nrow(period_stats))) {

  start_year <- period_stats$start[i]
  end_year   <- period_stats$end[i]

  # Filter once per period
  d <- cfr_from_bangladesh_surveillance[
    cfr_from_bangladesh_surveillance$Year >= start_year &
      cfr_from_bangladesh_surveillance$Year <= end_year,
  ]

  period_stats$year_cat[i] <- sprintf("%d-%d", start_year, end_year)

  if (nrow(d) == 0) next

  deaths <- sum(d$cfr_ifr_numerator, na.rm = TRUE)
  cases  <- sum(d$cfr_ifr_denominator, na.rm = TRUE)

  if (cases == 0) next

  cfr <- deaths / cases
  ci  <- binom.test(deaths, cases)$conf.int

  period_stats$deaths[i]   <- deaths
  period_stats$cases[i]    <- cases
  period_stats$cfr[i]      <- cfr
  period_stats$cfr_lwr[i]  <- ci[1]
  period_stats$cfr_upr[i]  <- ci[2]

  period_stats$y[i]      <- 100 * cfr
  period_stats$y_lwr[i]  <- 100 * ci[1]
  period_stats$y_upr[i]  <- 100 * ci[2]

  period_stats$label[i] <- sprintf(
    # "%d-%d = %.0f%% (95%% CI: %.0f%%-%.0f%%)",
    "%.1f%% (95%% CI: %.1f%%-%.1f%%)",
    # start_year,
    # end_year,
    100 * cfr,
    100 * ci[1],
    100 * ci[2]
  )
}

missing_years <- setdiff(all_years,
                         unique(cfr_from_bangladesh_surveillance[cfr_from_bangladesh_surveillance$Cases!=0, ]$Year))

missing_df <- tibble(
  xmin = missing_years - 1,
  xmax = missing_years + 0,
  ymin = -Inf,
  ymax = Inf
)

missing_df <- tibble(year = missing_years) |>
  mutate(xmin = year-1, xmax = year) |>
  rowwise() |>
  mutate(
    period_row = list(period_stats |> filter(xmax > seg_start, xmax <= seg_end) |> slice(1)),
    cfr_lwr = period_row$cfr_lwr,
    cfr_upr = period_row$cfr_upr
  ) |>
  ungroup() |>
  select(xmin, xmax, cfr_lwr, cfr_upr) |>
  mutate(
    y_lwr = 100 * cfr_lwr,
    y_upr = 100 * cfr_upr
  )

missing_df <- bind_rows(
  missing_df |> transmute(xmin, xmax, ymin = 0, ymax = y_lwr),
  missing_df |> transmute(xmin, xmax, ymin = y_upr, ymax = Inf)
)

# Plot
IEDCR_over_time_overall <- cfr_from_bangladesh_surveillance |>
  mutate(CFR_plot = if_else(Year %in% missing_years, NA_real_, CFR * 100)) |>
  ggplot(aes(x = Year, y = CFR * 100)) +
  geom_point(colour = "#00468BFF") +
  geom_line(colour = "#00468BFF") +
  geom_segment(data = period_stats,
               aes(x = seg_start, xend = seg_end,
                   y = y, yend = y, color=year_cat),
               linetype="dashed") +
  geom_rect(data = period_stats,
              aes(xmin = seg_start, xmax = seg_end,
                  ymin = y_lwr, ymax = y_upr, fill = year_cat),
              inherit.aes = FALSE,
              alpha = 0.15) +
  geom_rect(data = missing_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "green3", alpha = 0.15) +
  geom_text(data = period_stats,
            aes(x = xmid, y = 101, label = label, color=year_cat),
            inherit.aes = FALSE, vjust = -0.6, size = 3.5, na.rm = TRUE) +
  annotate("text", x = 2000, y = 107, label = "5-year CFRs",
           hjust = 0, vjust = -0.2, size = 3.8, fontface="bold") +
  annotate("text", x = 2000, y = 119, label = "Overall CFR",
           hjust = 0, vjust = -0.2, size = 3.8, fontface="bold") +
  annotate("text", x = 2000, y = 114,
           label = overall_label,
           hjust = 0, vjust = -0.2, size = 3.5, color=imperial_blue) +
  labs(x = "Outbreak year end", y = "CFR (%)") +
  scale_color_nejm() +
  scale_fill_nejm() +
  guides(color=guide_none(), fill=guide_none()) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 100, by = 25)) +
  coord_cartesian(c(2000.5,2025)) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black",
                                    linewidth = 1.25, fill = NA),
        text = element_text(size = 18))

ggsave(file.path("figures", "IEDCR_over_time_overall_v2.pdf"),
       IEDCR_over_time_overall, width = 12, height = 6)
ggsave(file.path("figures", "IEDCR_over_time_overall_v2.png"),
       IEDCR_over_time_overall, width = 12, height = 6)

