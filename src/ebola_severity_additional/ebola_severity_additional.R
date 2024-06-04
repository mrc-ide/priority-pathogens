library(dplyr)
library(orderly2)
library(epireview)
orderly_dependency("ebola_severity", "latest", "ebola_severity.rds")
orderly_shared_resource("ebola_functions.R" = "ebola_functions.R")
orderly_shared_resource("ebola_visualisation.R" = "ebola_visualisation.R")

source("ebola_functions.R")
source("ebola_visualisation.R")

cfr <- readRDS("ebola_severity.rds")
## Removing this as it is a CFR across multiple outbreaks from 1976 to 2014
idx <- cfr$covidence_id %in% 2594 & cfr$cfr_ifr_numerator %in% 526
cfr <- cfr[!idx, ]
## Text says 96; table says 88. I can't see where the 96 comes from
## Going with 88. No CI provided for this estimate
idx <- cfr$covidence_id %in% 2594 & cfr$cfr_ifr_numerator %in% 252
cfr$parameter_value[idx] <- 88
cfr$parameter_uncertainty_lower_value[idx] <- NA
cfr$parameter_uncertainty_upper_value[idx] <- NA

## Study in pregnant women; naive estimate
cfr$cfr_ifr_method[cfr$covidence_id %in% 1889] <- "Naive"

## https://doi.org/10.1093/cid/ciz678 estimates CFR with and without
## imputation of missing data. Only number of cases without impuation
## has been extracted, even where the CFR itself is based on imputed
## data. Fixing this.
cfr$population_sample_size[cfr$covidence_id %in% 16500 & 
  cfr$cfr_ifr_method %in% "Adjusted"] <- 33338
cfr$population_sample_size[cfr$covidence_id %in% 16500 & 
  cfr$cfr_ifr_method %in% "Naive" & cfr$parameter_value == 71.9] <- 33338  

## Adjusted CFRs, high-quality studies
adjusted_high <- cfr[cfr$cfr_ifr_method %in% "Adjusted" & 
  cfr$article_qa_score > 50, ]
adjusted_high <- param_pm_uncertainty(adjusted_high) |> 
  mark_multiple_estimates(label_type = "letters") |>
  reorder_studies()
p1 <- forest_plot(
  adjusted_high, col_by = "population_country", facet_by = "ebola_species") +
labs(x = "Case fatality rate (%)", 
  title = "Adjusted case fatality rates in high-quality studies") +
  theme(axis.text.y = element_text(size = 8))

ggsave("adjusted_high.png", p1, width = 10, height = 6, units = "in")

adjusted_all <- cfr[cfr$cfr_ifr_method %in% "Adjusted", ]
adjusted_all <- param_pm_uncertainty(adjusted_all) |> 
  mark_multiple_estimates(label_type = "letters") |>
  reorder_studies()
p2 <- forest_plot(
  adjusted_all, col_by = "population_country", facet_by = "ebola_species") +
labs(x = "Case fatality rate (%)", 
  title = "Adjusted CFR estimates") +
  theme(axis.text.y = element_text(size = 8))

ggsave("adjusted_all.png", p2, width = 10, height = 6, units = "in")

naive_all <- cfr[cfr$cfr_ifr_method %in% "Naive", ]
naive_all <- param_pm_uncertainty(naive_all) |> 
  mark_multiple_estimates(label_type = "letters") |>
  reorder_studies()

p3 <- forest_plot(
  naive_all, col_by = "population_country", facet_by = "ebola_species") +
labs(x = "Case fatality rate (%)",
  title = "Naive CFR estimates") +
  theme(axis.text.y = element_text(size = 6))

ggsave("naive_all.png", p3, width = 10, height = 6, units = "in")

## Retain only general population estimates
pb <- cfr[cfr$population_sample_type %in% "Population based" & 
  cfr$article_qa_score > 50, ]
pb <- param_pm_uncertainty(pb)  
pb <- mark_multiple_estimates(pb, label_type = "letters")
pb <- reorder_studies(pb)
p4 <- forest_plot(
  pb, col_by = "population_country", facet_by = "ebola_species"
) +
  labs(x = "Case fatality rate (%)", 
    title = "Population-based CFR estimates in high-quality studies") +
  theme(axis.text.y = element_text(size = 8))

ggsave("population_based.png", p4, width = 10, height = 6, units = "in")

pb <- cfr[cfr$population_sample_type %in% "Population based", ]
pb <- param_pm_uncertainty(pb)  
pb <- mark_multiple_estimates(pb, label_type = "letters")
pb <- reorder_studies(pb)
p5 <- forest_plot(
  pb, col_by = "population_country", facet_by = "ebola_species"
) +
  labs(x = "Case fatality rate (%)", 
    title = "Population-based CFR estimates") +
  theme(axis.text.y = element_text(size = 8))

ggsave("population_based_all.png", p5, width = 10, height = 6, units = "in")

## Estimates where the population_sample_type is not population-based; could be 
## missing or hospital-based etc
gb <- cfr[!cfr$population_sample_type %in% "Population based", ]
gb <- param_pm_uncertainty(gb)  
gb <- mark_multiple_estimates(gb, label_type = "letters")
gb <- reorder_studies(gb)
p5 <- forest_plot(
  gb, col_by = "population_country", facet_by = "ebola_species"
) +
  labs(x = "Case fatality rate (%)", 
    title = "Population-based CFR estimates") +
  theme(axis.text.y = element_text(size = 8))

ggsave("not_population_based_all.png", p5, width = 10, height = 6, units = "in")

## Distribution of central values by denominator
wa <- cfr[cfr$outbreak %in% "West Africa 2013-2016", ]
wa$sample_size <- case_when(
  wa$cfr_ifr_denominator <= 100 ~ "0-100",
  wa$cfr_ifr_denominator > 100 & wa$cfr_ifr_denominator <= 1000 ~ 
    "101-1000",
  wa$cfr_ifr_denominator > 1000 ~ " > 1000",
  TRUE ~ NA_character_
)

ggplot(wa, aes(y = parameter_value, x = cfr_ifr_denominator)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~sample_size, scales = "free_x") + 
  labs(x = "Case fatality rate (%)", 
    title = "Distribution of central values by denominator") +
  theme(legend.position = "bottom") +
  ylim(0, 100)
  

weighted_cfr <- function(x) {
  sum(
    (x$parameter_value * x$cfr_ifr_denominator) / 
    sum(x$cfr_ifr_denominator, na.rm = TRUE),
    na.rm = TRUE
  )
}
w_overall <- weighted_cfr(cfr)
uw_overall <- mean(cfr$parameter_value, na.rm = TRUE)
w_by_method <- split(cfr, cfr$cfr_ifr_method) |>
  map_dfr(weighted_cfr, .id = "cfr_ifr_method") 
uw_by_method <- cfr |>
  group_by(cfr_ifr_method) |>
  summarise(uw = mean(parameter_value, na.rm = TRUE))

ggplot(cfr, aes(parameter_value)) + 
  geom_histogram(binwidth = 1, position = position_dodge()) +
  facet_wrap(~cfr_ifr_method, ncol = 1) +
  geom_vline(xintercept = weighted_cfr, color = "red") +
  labs(x = "Case fatality rate (%)", 
    title = "Distribution of central values") +
    theme_minimal()
