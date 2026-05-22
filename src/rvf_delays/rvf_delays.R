# *=============================== RVF delays ===============================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(orderly)
library(patchwork)
library(readr)
library(stringr)
library(ggbreak)


# *--------------------------------- Orderly ----------------------------------*
pars <- orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "params.csv", "outbreaks.csv"))

orderly_shared_resource("rvf_functions.R" = "rvf_functions.R")
source("rvf_functions.R")

orderly_artefact(description="RVF delay figures",
                 c("figure_delays.pdf",
                   "figure_delays.png"))


# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

#Remove the words "human delay" throughout
parameters <- parameters |>
  mutate(parameter_type = str_replace(parameter_type, "Human delay - ", ""),
         parameter_type = str_to_sentence(parameter_type))

all_delays <- filter(parameters, parameter_class == "Human delay")

#Filter out all low-QA
all_delays_qa <- filter(all_delays, qa_score >= 0.5)

table(all_delays$parameter_type)

# Filter out everything but the delays
# (There was one "Generation Time" which has been removed in QA filtering)
all_delay_types <- c("Incubation period", #0
                     "Time in care (length of stay)", #0
                     "Serial interval", #0
                     "Other human delay (go to section)", #2
                     "Symptom onset>death", #0
                     "Symptom onset>admission to care", #4
                     "Admission to care>death", #1
                     "Symptom onset>discharge/recovery", #1
                     "Admission to care>discharge/recovery", #0
                     "Symptom onset/fever>discharge or death", #2
                     "Infectious period" #None
)
parameters <- parameters |>
  filter(parameter_type %in% all_delay_types)
parameters_qa <- filter(parameters, qa_score >= 0.5)


# *--------------------------------- Summary ----------------------------------*
num_delays <- NROW(parameters)

cat("\nIn total", num_delays, "delays were extracted.")

cat("\nThe composition is as follows:\n")
parameters |>
  group_by(parameter_type) |>
  count() |>
  arrange(desc(n)) |>
  print()

# Rows with variability only
varb_only_rows <- parameters |>
  filter(is.na(parameter_value) &
           is.na(parameter_lower_bound) &
           is.na(parameter_upper_bound)) |>
  NROW()

varb_only_data <- filter(parameters, is.na(parameter_value) &
                           is.na(parameter_lower_bound) &
                           is.na(parameter_upper_bound))

cat("Number of variability only rows:", varb_only_rows)


# *------------------------------ Plot datasets -------------------------------*

# Incubation period
d1 <- parameters %>% filter(tolower(parameter_type) == 'incubation period')  

# Onset to admission
d2 <- parameters |>
  filter(tolower(parameter_type) == 'symptom onset>admission to care')  #25

# Hospital admission to outcome
d3 <- parameters |>
  filter(tolower(parameter_type) %in% c('admission to care>death')   #26 + 1 + 1
  )

# Symptom-onset to outcome
d4 <- parameters |>
  filter(tolower(parameter_type) %in% c(
    'symptom onset>discharge/recovery')
  )

# Symptom-onset to death
d5 <- parameters |>
  filter(tolower(parameter_type) %in% c(
    'symptom onset>death')
  )

# Symptom Onset/Fever to Discharge or death
d6 <- parameters |>
  filter(tolower(parameter_type) %in% c(
    'symptom onset/fever>discharge or death')
  ) 


# *---------------------------------- Plots -----------------------------------*
lanonc_colours <- ggsci::pal_lancet("lanonc")(9)

# Plot properties
text_size <- 14

# We'll produce multiple plots to filter in/out low QA studies
qa_thresh_vec <- c("qa"=0.5)
qa_alpha_vec <- c(1)

# Incubation period:
d1$parameter_unit # NA needs fixing?
p1 <- forest_plot(d1,
                  "Incubation period",
                  "population_group", c(0,35),
                  text_size=text_size,
                  segment_show.legend = NA,
                  sort=TRUE,
                  qa_alpha =0.3) 
# Onset to care:
d2$parameter_unit
p2 <- forest_plot(d2,
                  "Symptom onset > admission to care",
                  "population_group", c(0,35),
                  text_size=text_size,
                  segment_show.legend = NA,
                  sort=TRUE,
                  qa_alpha =0.3)

# Admission to care>death:
d3$parameter_unit <- "Needs checking" # needs updating
p3 <- forest_plot(d3,
                  "Admission to care > death",
                  "population_group", c(0,35),
                  text_size=text_size,
                  segment_show.legend = NA,
                  sort=TRUE,
                  qa_alpha =0.3) 

# Symptom onset>discharge/recovery:
d4$parameter_unit
p4 <- forest_plot(d4,
                  "Symptom onset > discharge/recovery",
                  "population_group", c(-5,130),
                  text_size=text_size,
                  segment_show.legend = NA,
                  sort=TRUE,
                  qa_alpha =0.3) 

# Symptom onset>death:
d5$parameter_unit
p5 <- forest_plot(d5,
                  "Symptom onset > death",
                  "population_group", c(0,35),
                  text_size=text_size,
                  segment_show.legend = NA,
                  sort=TRUE,
                  qa_alpha =0.3) 

# Symptom onset/fever > discharge or death:
d6$parameter_unit
p6 <- forest_plot(df=d6,
                  label="Symptom onset > discharge or death",
                  color_column = "population_group", 
                  lims = c(-5,130),
                  text_size=text_size,
                  segment_show.legend = NA,
                  sort=TRUE,
                  qa_alpha =0.3) 



# Save transmission plots
patchwork_delays <- p1+p2+p3+p5+p6+p4+plot_layout(ncol=1)
patchwork_delays <- patchwork_delays + plot_annotation(tag_levels = 'A')
ggsave("figure_delays.png", plot = patchwork_delays, width = 10, height = 15)
ggsave("figure_delays.pdf", plot = patchwork_delays, width = 10, height = 15)
