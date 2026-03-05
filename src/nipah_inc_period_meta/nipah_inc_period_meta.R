library(dplyr)
library(grid)
library(meta)
library(orderly2)
library(patchwork)
library(readr)
library(stringr)
library(tidyr)

orderly_dependency(
  "db_cleaning", "latest(parameter:pathogen == 'NIPAH')",
  c("articles.csv", "outbreaks.csv", "models.csv", "params.csv")
)

orderly_shared_resource("nipah_functions.R")

source("nipah_functions.R")
# *------------------------------ Data curation -------------------------------*
articles <- read_csv("articles.csv")
outbreaks <- read_csv("outbreaks.csv")
models <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

parameters <- dfs$parameters

datasets_with_refs <- list(
    d1 = list(
      median = 10.0, min = 9.0, max = 12.0, n = 4,
      access_param_id = "113_003"
    ),
    d2 = list(
      median = 4.0, min = 2.0, max = 7.0, n = 6,
      access_param_id = "113_004"
    ),
    d3 = list(
      median = 9.0, min = 6.0, max = 14.0, n = 11,
      access_param_id = "138_020"
    ),
    d4 = list(
      median = 9.5, min = 4.0, max = 14.0, n = 22,
      access_param_id = "121_002"
    ),
    d5 = list(
      median = 8.0, min = 3.0, max = 20.0, n = 15,
      access_param_id = "081_001"
    ),
    d6 = list(
      median = 9.0, min = 6.0, max = 11.0, n = 11,
      access_param_id = "030_002"
    ),
    d7 = list(
      median = 10.0, min = 8.0, max = 15.0, n = 14,
      access_param_id = "151_001"
    ),
    d8 = list(
      median = 9.0, min = 6.0, max = 11.0, n = 11,
      access_param_id = "033_006"
    )
  )

datasets <- lapply(datasets_with_refs, function(x) x[!names(x) %in% "access_param_id"])


# params_in <- as_tibble(parameters) %>%
#   filter(parameter_type == 'Human delay - incubation period' &
#          parameter_value_type == 'Median' &
#          !is.na(population_sample_size) &
#          population_sample_size < 23 )

# No sample size 139_004 (although median (range))
params_in <- parameters |>
  filter(parameter_type == 'Human delay - incubation period',
         !is.na(population_sample_size),
         parameter_value_type=="Mean" &
           grepl(x = tolower(parameter_2_value_type),
                 pattern = "standard deviation") |
           parameter_value_type=="Median" &
           grepl(x = tolower(parameter_2_value_type),
                 pattern = "iqr") |
           parameter_value_type == "Median" &
           grepl(x = tolower(parameter_2_value_type),
             pattern = "range")) |>
  mutate(duplicate_incp = case_when(
    access_param_id %in% c("138_020", "138_3141") ~ "Known",
    TRUE~"False"))

params_in <- params_in |>
  filter(duplicate_incp!="Known")

# params_in$parameter_uncertainty_type <- 'range'
params_in$parameter_uncertainty_type <- params_in$parameter_2_value_type
params_in$parameter_uncertainty_lower_value <- params_in$parameter_2_lower_bound
params_in$parameter_uncertainty_upper_value <- params_in$parameter_2_upper_bound


ip_same_data <-
  metamean_wrap(dataframe = params_in, estmeansd_method = "Cai",
                plot_study = TRUE, digits = 2, lims = c(2,15),
                colour = "dodgerblue3", label = "Median Incubation Period (days)",
                width = 11000, height = 4200, resolution = 1000)


params_ip_all <- as_tibble(parameters) %>%
  filter(parameter_type == 'Human delay - incubation period' &
         !is.na(population_sample_size) & !is.na(parameter_value) ) %>%
  mutate(
    parameter_uncertainty_type = replace_na(parameter_uncertainty_type, 'range'),
    parameter_uncertainty_lower_value =
      coalesce(parameter_uncertainty_lower_value, parameter_2_lower_bound),
    parameter_uncertainty_upper_value =
      coalesce(parameter_uncertainty_upper_value, parameter_2_upper_bound)
  )

ip_all_data <- metamean_wrap(
  dataframe = params_ip_all, estmeansd_method = "Cai",
  plot_study = TRUE, digits = 2, lims = c(2, 15), colour = "darkorange2",
  label = "Median Incubation Period (days)",
  width = 9500, height = 4200, resolution = 1000
)

imperial_khaki <- "#EFE58B"

saveRDS(ip_same_data, file = "ip_same_data.rds")
png("nipah_ip_meta_analysis_same_data.png", width = 12400, height = 4600,
    res = 1000)

# can't get size by square size right but the random effects look similar and
# this is what we focus on
forest(ip_same_data$result, layout = "Revman5",
       colgap.forest.left = "3mm",
       leftcols = c("studlab", "mean", "sd",
                    "n", "w.common",
                    "w.random", "effect.ci"),
       leftlabs = c("Study", "Mean", "SD", "Total", "Weight  \n(common)",
                    "Weight  \n(random)", ""),
       colgap.left = "7mm",
       smlab.pos = 4,
       overall = TRUE, pooled.events = TRUE,
       print.subgroup.name = FALSE, sort.subgroup = TRUE,
       study.results = TRUE,
       digits = 2,
       digits.sd = 2,
       showweights = TRUE,
       col.diamond.lines = "black",col.diamond.common = "dodgerblue3",
       col.diamond.random = "dodgerblue3",
       col.square = imperial_khaki, col.square.lines = "black",
       col.study = "black", col.subgroup = "black",
       col.inside = "black", weight.study = "same",
       xlim = c(2,12),
       xlab="Incubation period (days)",
       fs.predict.labels = 11.5,
       fs.hetstat=11,
       fs.test.subgroup = 11,
       fs.axis = 11,
       fontsize = 14,
       plotwidth = "72.5mm",
       spacing = 1.15)

dev.off()

orderly_artefact(files = c("ip_same_data.rds", "nipah_ip_meta_analysis_same_data.png"))
