# *=============================== Nipah delays ===============================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(orderly2)
library(patchwork)
library(readr)
library(stringr)

# *--------------------------------- Orderly ----------------------------------*
orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "outbreaks.csv", "models.csv", "params.csv"))

orderly_shared_resource("nipah_functions.R" = "nipah_functions.R")
source("nipah_functions.R")

orderly_artefact(description="Nipah delay figures",
                 c("figure_5SI_incubation.pdf"))

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")


# 2 Gamma uncertainties:
# 6289bd0b84d80771220a16da295c90a2 : Nikolay (2019) Human delay - serial interval
# 7eb2ec3c3268d44a1edfab0f7c95f9ff : Nikolay (2019) Human delay - incubation period
dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

d1 <- parameters %>% filter(parameter_type == 'Human delay - incubation period')

# Deduplication
# # Nikolay (2019) is duplicated (reference below is a subset of the first)
# d1 <- d1 |>
#   filter(parameter_data_id!="bb5bdf26abeda50067007b8db5d4bb15")
d1 <- d1 |>
  arrange(population_group, desc(central))

d2 <- parameters %>% filter(parameter_type == 'Human delay - symptom onset>admission to care')
d3 <- parameters %>% filter(parameter_type == 'Human delay - time in care (length of stay)' |
                              parameter_type == 'Human delay - admission to care>discharge/recovery' |
                              parameter_type == 'Human delay - admission to care>death') %>%
  mutate(parameter_value_type = case_when(is.na(parameter_value_type)~'Unspecified',
                                          TRUE ~ parameter_value_type))
d4 <- parameters %>% filter(parameter_type == 'Human delay - symptom onset>discharge/recovery' |
                              parameter_type == 'Human delay - symptom onset>death')

d5 <- parameters %>% filter(parameter_type == 'Human delay - serial interval')

# *---------------------------------- Plots -----------------------------------*
# Plot properties
text_size <- 28

# Incubation period
p1 <- forest_plot(d1,
                  "Incubation period (days)",
                  "population_group", c(0,35),
                  text_size=text_size,
                  show.legend = NA)
p1

ggsave("figure_5SI_incubation.pdf", plot = p1, width = 15, height = 15)


