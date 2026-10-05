## OROV delays

library(tidyverse)
library(forcats)
library(orderly2)
library(ggsci)
library(patchwork)
library(stringr)


orderly_strict_mode()
orderly2::orderly_parameters(pathogen = "OROV")

orderly2::orderly_artefact(description = "inputs folder",
                           files = "inputs/")

orderly_dependency(
  name = "db_compilation_orov",
  query = "latest()",
  files = c("inputs/articles.csv"="articles.csv",
            "inputs/parameters.csv"="parameters.csv",
            "inputs/outbreaks.csv"="outbreaks.csv"))

# forest plot code
orderly_shared_resource("orov_functions.R" = "orov_functions.R")
source("orov_functions.R")

# read in data
articles <- read.csv("inputs/articles.csv")
outbreaks <- read.csv("inputs/outbreaks.csv")
parameters <- read.csv("inputs/parameters.csv")

dfs <- curation(articles, outbreaks, tibble(), parameters, plotting = TRUE)

parameters <- dfs$parameters
parameters <- parameters |> mutate(qa_score = article_qa_score / 100)
parameters$parameter_class <- parameters$parameter_type_broad

parameters <- parameters |>
  mutate(
    population_group = factor(
      population_group,
      levels = c(
        sort(setdiff(unique(population_group), c("Other", "Unspecified"))),
        "Other",
        "Unspecified"
      )
    )
  )

# manual fix
# check covidence ID 17 Moreira 2024. 5 days (SD 3.04) listed in db as onset to discharge but is onset to recovery
parameters |>
  mutate(parameter_type = case_when(covidence_id == 17 & parameter_type == "Human delay - symptom onset>discharge/recovery" 
                                     ~ "Human delay - Symptom Onset/Fever>Symptom Resolution",
                                    .default = parameter_type)) -> parameters

d1 <- parameters |> 
  filter(parameter_class == "Delays") %>% 
  mutate(location_label = case_when(
    population_country %in% c(
      "Brazil","Trinidad and Tobago","Cuba") ~ "Local transmission",
    population_country %in% c(
      "Italy","Germany","Switzerland") ~ "Importation",
    population_country=="France" & population_location== "Saul, French Guiana"  ~ "Local transmission",
    population_country=="France" & is.na(population_location) ~ "Importation",
    .default = population_country
  ),
  location_label = factor(location_label,
                          levels=c("Local transmission","Importation")),
  delay_human_mosquito = case_when(
    grepl("Human delay",parameter_type) ~ "Human only",
    grepl("Mosquito delay",parameter_type) ~ "Mosquito only",
    .default = "Human and Mosquito"
  ),
  delay_label = case_when(
    parameter_type=="Human delay - other human delay (go to section)" ~ paste0("Human delay - ",parameter_hd_from,">",parameter_hd_to),
    parameter_type!="Human delay - other human delay (go to section)" ~ parameter_type
  ),
  delay_label_short = case_when(
    delay_label =="Human delay - symptom onset>discharge/recovery" ~ "Symptom onset -> Discharge from care",
    delay_label=="Human delay - Symptom Onset/Fever>Seeking Care" ~ "Symptom onset -> Seeking care",
    delay_label=="Human delay - time in care (length of stay)" ~ "Time in care",
    delay_label=="Human delay - incubation period" ~ "Incubation period",
    delay_label=="Human delay - symptom onset>admission to care" ~ "Symptom onset -> Admission to care",
    delay_label=="Human delay - Symptom Onset/Fever>Discharge from Critical Care/ICU" ~ "Symptom onset -> Discharge from critical care",
    delay_label=="Human delay - Admission to Care/Hospitalisation>Discharge from Critical Care/ICU" ~ "Admission to care -> Discharge from critical care",
    delay_label =="Human delay - Symptom Onset/Fever>Symptom Resolution" ~ "Duration of symptoms",
    delay_label=="Delay - human to mosquito generation time" ~ "Generation time (human to vector)",
    delay_label=="Mosquito delay - extrinsic incubation period" ~ "Extrinsic incubation period",
    delay_label=="Delay - mosquito to human generation time" ~ "Generation time (vector to human)",
    delay_label=="Human delay - generation time" ~ "Generation time",
    delay_label=="Human delay - admission to care>death" ~ "Admission to care -> Death",
    delay_label=="Human delay - Symptom Onset/Fever>Symptom Recurrence" ~ "Symptom onset -> Symptom recurrence",
    delay_label=="Human delay - Symptom Onset/Fever>Symptom Worsening" ~ "Symptom onset -> Symptom worsening",
    delay_label=="Human delay - incubation period" ~ "Incubation period",
    delay_label=="Human delay - Symptom Resolution>Symptom Recurrence" ~ "Symptom resolution -> Symptom recurrence",
    delay_label=="Human delay - Symptom Recurrence>Seeking Care" ~ "Symptom recurrence -> Seeking care",
    delay_label=="Human delay - Symptom Recurrence>Symptom Resolution" ~ "Symptom recurrence -> Symptom resolution",
    delay_label=="Human delay - admission to care>discharge/recovery" ~ "Admission to care -> Recovery",
    delay_label=="Human delay - Symptom Onset/Fever>Viral clearance" ~ "Symptom onset -> Viral clearance",
    delay_label=="Human delay - Symptom Onset/Fever>Admission to Care/Hospitalisation" ~ "Symptom onset -> Admission to care",
    .default = "You missed one"
  ),
  delay_label_short = factor(
    delay_label_short,
    levels = c("Incubation period",
               "Generation time",
               "Symptom onset -> Symptom worsening",
               "Symptom onset -> Seeking care",
               "Symptom onset -> Admission to care",
               "Admission to care -> Discharge from critical care",
               "Admission to care -> Recovery",
               "Symptom onset -> Discharge from critical care",
               "Admission to care -> Death",
               "Duration of symptoms",
               "Time in care",
               "Symptom onset -> Discharge from care",
               "Symptom onset -> Viral clearance",
               #"Symptom onset -> Death",
               "Symptom onset -> Symptom recurrence",
               "Symptom resolution -> Symptom recurrence",
               "Symptom recurrence -> Seeking care",
               "Symptom recurrence -> Symptom resolution",
               "Extrinsic incubation period",
               "Generation time (human to vector)",
               "Generation time (vector to human)")
  ),
  delay_facet_label = case_when(
    delay_label_short %in% c("Generation time (human to vector)",
                             "Generation time (vector to human)",
                             "Generation time",
                             "Extrinsic incubation period") ~ "Transmission",        delay_label_short %in% c("Incubation period",
                                                                                                              "Symptom onset -> Symptom worsening",
                                                                                                              "Symptom onset -> Viral clearance",
                                                                                                              "Duration of symptoms" ) ~ "Symptoms",
    delay_label_short %in% c("Symptom onset -> Seeking care",
                             "Symptom onset -> Admission to care",
                             "Symptom onset -> Discharge from critical care",
                             "Symptom onset -> Discharge from care") ~ "Symptoms to care milestones",
    delay_label_short %in% c("Symptom onset -> Symptom recurrence",
                             "Symptom resolution -> Symptom recurrence",
                             "Symptom recurrence -> Seeking care",
                             "Symptom recurrence -> Symptom resolution") ~ "Recurrence",
    delay_label_short %in% c("Admission to care -> Discharge from critical care",
                             "Admission to care -> Recovery",
                             "Admission to care -> Death",
                             "Time in care") ~ "Care to outcomes",
    .default = "you missed one"
  ))

d1 <- d1 %>% mutate(
  parameter_value_type = case_when(
    parameter_value_type %in% c("Unspecified","Other","Central - unspecified") ~ "Unspecified",
    .default = parameter_value_type
  )
)


## for the analysis ------------------------------------------------------------
d1$parameter_type

d1 |> filter(parameter_hd_to %in% c("Discharge from Critical Care/ICU", "Discharge from Care/Hospital")| 
               parameter_type == "Human delay - symptom onset>discharge/recovery")

# delay between symptom onset and seeking care
d1 |> filter(parameter_hd_from == "Symptom Onset/Fever" &
               parameter_hd_to == "Seeking Care")

# symptom onset and admission to healthcare facility
d1 |> filter(delay_label_short == "Symptom onset -> Admission to care") |> select(covidence_id)

# symptom onset to discharge from care
d1 |> filter(delay_label_short == "Symptom onset -> Discharge from care") 

## for the plot ----------------------------------------------------------------
qa_threshold <- -1
qa_alpha <- 1
point_size <- 4.5
text_size <- 20

custom_colour_pop_groups <- get_colour_pop_groups(
  parameters,
  d1
)


ref_labels <- data.frame(refs=unique(d1$refs), 
                         labels = paste0(substring(
                           str_replace_all(unique(d1$refs), fixed(" "), ""), 1, 3),
                           ".",
                           str_extract(unique(d1$refs), "\\((.*?)\\)")))
d1e <- d1 |>
  left_join(ref_labels)|>
  select(!refs)|>
  rename(refs=labels)


test_legend <- forest_plot(d1e,
                           "Days",
                           "location_label",
                           c(-1,31),
                           custom_colours = NA,
                           segment_show.legend = c(color = TRUE, shape = FALSE),
                           text_size = text_size,
                           qa_alpha = qa_alpha,
                           sort = TRUE,
                           point_size = point_size)+
  theme(
    legend.position = "bottom",                # Move legend to bottom
    legend.direction = "horizontal",           # Make it horizontal
    legend.box.just = "center"  ,
    text = element_text(size=30)
  )

get_legend_35 <- function(plot) {
  # return all legend candidates
  legends <- cowplot::get_plot_component(plot, "guide-box", return_all = TRUE)
  # find non-zero legends
  nonzero <- vapply(legends, \(x) !inherits(x, "zeroGrob"), TRUE)
  idx <- which(nonzero)
  # return first non-zero legend if exists, and otherwise first element (which will be a zeroGrob) 
  if (length(idx) > 0) {
    return(legends[[idx[1]]])
  } else {
    return(legends[[1]])
  }
}
leg <- get_legend_35(test_legend)


forest_plot(
  d1e |>
    filter(qa_score > qa_threshold,
           delay_facet_label == "Symptoms") |>
    mutate(delay_label_short = case_when(delay_label_short == "Incubation period" ~ "B1. Incubation\nperiod",
                                         delay_label_short == "Symptom onset -> Symptom worsening" ~ "B2. Symptom onset ->\nSymptom worsening",
                                         delay_label_short == "Duration of symptoms" ~ "B3. Duration of\nsymptoms",
                                         delay_label_short == "Symptom onset -> Viral clearance" ~ "B4. Symptom onset ->\nViral clearance")),
  "Days",
  "location_label",
  c(-1,31),
  custom_colours = NA,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size) +
  facet_wrap(~delay_label_short,
             scales="free_y",ncol=1,
             strip.position = "top") + 
  ggtitle("B. Symptoms") +
  theme(plot.title = element_text(hjust = 0.5, size=25),
        legend.position="none") -> p1a


forest_plot(
  d1e |>
    filter(qa_score > qa_threshold,
           delay_facet_label == "Symptoms to care milestones")|>
    mutate(delay_label_short = case_when(delay_label_short == "Symptom onset -> Seeking care" ~ "C1. Symptom onset ->\nSeeking care",
                                         delay_label_short == "Symptom onset -> Admission to care" ~ "C2. Symptom onset ->\nAdmission to care",
                                         delay_label_short == "Symptom onset -> Discharge from critical care" ~ "C3. Symptom onset ->\nDischarge from critical care",
                                         delay_label_short == "Symptom onset -> Discharge from care" ~ "C4. Symptom onset ->\nDischarge from care")),
  "Days",
  "location_label",
  c(-1,31),
  custom_colours = NA,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size) +
  facet_wrap(~delay_label_short,scales="free_y",ncol=1,
             strip.position = "top") + 
  ggtitle("C. Symptoms to care milestones") +
  theme(plot.title = element_text(hjust = 0.5, size=25),
        legend.position="none") -> p2a


forest_plot(
  d1e |>
    filter(qa_score > qa_threshold,
           delay_facet_label == "Care to outcomes")|>
    mutate(delay_label_short = case_when(delay_label_short == "Admission to care -> Discharge from critical care" ~ "D1. Admission to care ->\nDischarge from critical care",
                                         delay_label_short == "Admission to care -> Recovery" ~ "D2. Admission to care ->\nRecovery",
                                         delay_label_short == "Admission to care -> Death" ~ "D3. Admission to care ->\nDeath",
                                         delay_label_short == "Time in care" ~ "D4. Time in\ncare")),
  "Days",
  "location_label",
  c(-1,31),
  custom_colours = NA,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size) +
  facet_wrap(~delay_label_short,scales="free_y",ncol=1,
             strip.position = "top") + 
  ggtitle("D. Care to outcome") +
  theme(plot.title = element_text(hjust = 0.5, size=25),
        legend.position="none") -> p3a

forest_plot(
  d1e |>
    filter(qa_score > qa_threshold,
           delay_facet_label == "Recurrence")|>
    mutate(delay_label_short = case_when(delay_label_short == "Symptom onset -> Symptom recurrence" ~ "E1. Symptom onset ->\nSymptom recurrence",
                                         delay_label_short == "Symptom resolution -> Symptom recurrence" ~ "E2. Symptom resolution ->\nSymptom recurrence",
                                         delay_label_short == "Symptom recurrence -> Seeking care" ~ "E3. Symptom recurrence ->\nSeeking care",
                                         delay_label_short == "Symptom recurrence -> Symptom resolution" ~ "E4. Symptom recurrence ->\nSymptom resolution")),
  "Days",
  "location_label",
  c(-1,31),
  custom_colours = NA,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size) +
  facet_wrap(~delay_label_short,scales="free_y",ncol=1,
             strip.position = "top") + 
  ggtitle("E. Recurrence") +
  theme(plot.title = element_text(hjust = 0.5, size=25),
        legend.position="none") -> p4a


forest_plot(
  d1e |>
    filter(qa_score > qa_threshold,
           delay_facet_label == "Transmission")|>
    mutate(delay_label_short = case_when(delay_label_short == "Generation time" ~ "A1. Generation\ntime",
                                         delay_label_short == "Extrinsic incubation period" ~ "A2. Extrinsic\nincubation period",
                                         delay_label_short == "Generation time (human to vector)" ~ "A3. Generation time\n(human to vector)",
                                         delay_label_short == "Generation time (vector to human)" ~ "A4. Generation time\n(vector to human)")),
  "Days",
  "location_label",
  c(-1,31),
  custom_colours = NA,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size) +
  facet_wrap(~delay_label_short,scales="free_y",ncol=1,
             strip.position = "top") + 
  ggtitle("A. Transmission") +
  theme(plot.title = element_text(hjust = 0.5, size=25),
        legend.position="none") -> p5a

d <- ggpubr::as_ggplot(leg) 
panels<-cowplot::plot_grid(
  p5a+theme(legend.position = "none"),
  p1a+theme(legend.position = "none"),
  p2a+theme(legend.position = "none"),
  p3a+theme(legend.position = "none"),
  p4a+theme(legend.position = "none"), ncol=5)#, labels = "AUTO")
leg_panels <- cowplot::plot_grid(panels, 
                                 d, ncol=1, rel_heights = c(1,0.15))


## read in the image of the timeline
library("magick")
img_path <- "OROV timeline labels.png"
if (!file.exists(img_path)) {
  stop("Image file not found. Please check the path.")
}
img <- image_read(img_path)
img_plot <- cowplot::ggdraw() + cowplot::draw_image(img)

cowplot::plot_grid( img_plot, leg_panels,
                    
                    ncol=1,
                    rel_heights = c(0.4,1), 
                    align = "hv", axis = "tblr")
ggsave("delays_timeline.png",width=30,height=29)




