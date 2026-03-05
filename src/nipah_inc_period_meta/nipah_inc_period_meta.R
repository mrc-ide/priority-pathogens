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


params_in <- as_tibble(parameters) %>%
  filter(parameter_type == 'Human delay - incubation period' &
         parameter_value_type == 'Median' &
         !is.na(population_sample_size) &
         population_sample_size < 23 )

params_in$parameter_uncertainty_type <- 'range'
params_in$parameter_uncertainty_lower_value <- params_in$parameter_2_lower_bound
params_in$parameter_uncertainty_upper_value <- params_in$parameter_2_upper_bound


ip_same_data <-
  metamean_wrap(dataframe = params_in, estmeansd_method = "Cai",
                plot_study = TRUE, digits = 2, lims = c(2,15),
                colour = "dodgerblue3", label = "Median Incubation Period (days)",
                width = 9500, height = 4200, resolution = 1000)


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

saveRDS(ip_same_data, file = "ip_same_data.rds")
png("nipah_ip_meta_analysis_same_data.png", width = 2800, height = 1000, res = 300)
plot(ip_same_data$result)
dev.off()

orderly_artefact(files = c("ip_same_data.rds", "nipah_ip_meta_analysis_same_data.png"))
