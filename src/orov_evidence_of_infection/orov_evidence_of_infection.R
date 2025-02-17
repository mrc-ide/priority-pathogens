## evidence of infection

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


# range of infection prevalence
parameters %>% filter(parameter_type_broad=="Infection prevalence") %>% select(parameter_value) %>% summary()

parameters %>% filter(parameter_type_broad=="Seroprevalence") %>% select(parameter_value) %>% summary()



