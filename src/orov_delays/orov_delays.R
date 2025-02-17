## OROV delays

library(tidyverse)
library(forcats)
library(orderly2)

orderly_strict_mode()
orderly2::orderly_parameters(pathogen = "OROV")

orderly2::orderly_artefact(description = "inputs folder",
                           files = "inputs/")

# did not like latest(parameter:pathogen == this:pathogen) - fix in future - seems to have resolved itself for now...
orderly_dependency(
  name = "db_compilation",
  query = "latest(parameter:pathogen == this:pathogen)",
  files = c("inputs/articles.csv"="articles.csv",
            "inputs/parameters.csv"="parameters.csv",
            "inputs/outbreaks.csv"="outbreaks.csv"))

# forest plot code
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")

# read in data
articles <- read.csv("inputs/articles.csv")
outbreaks <- read.csv("inputs/outbreaks.csv")
parameters <- read.csv("inputs/parameters.csv")

## human delays
delays <- parameters %>% filter(parameter_type_broad=="Human delay") %>%
  select(covidence_id,parameter_type,parameter_value,
         parameter_lower_bound,parameter_upper_bound,
         parameter_unit,parameter_value_type,parameter_statistical,
         parameter_uncertainty_single_value,
         parameter_uncertainty_singe_type,
         parameter_hd_from,parameter_hd_to,
         method_disaggregated,method_disaggregated_by,
         parameter_context_human,parameter_context_genotype,population_sex,
         population_sample_size,	parameter_context_sample_type,
         population_sample_type,	population_group,	population_age_min,
         population_age_max,	population_country,	population_location,
         parameter_context_timing,	parameter_context_zika,
         parameter_context_urban,	parameter_context_data,	exponent,
         population_study_start_day,	population_study_start_month,
         population_study_start_year,	population_study_end_day,
         population_study_end_month,	population_study_end_year
  )
#write.csv(delays,
#          "delay_clean.csv",row.names=FALSE)


delays <- delays %>% mutate(
  delay_label = case_when(
    parameter_type=="Human delay - other human delay (go to section)" ~ paste0("Human delay - ",parameter_hd_from,">",parameter_hd_to),
    parameter_type!="Human delay - other human delay (go to section)" ~ parameter_type
  )
)

delays %>% group_by(delay_label) %>% summarise(length(delay_label))

parameters$population_location


