## OROV delays

library(tidyverse)
library(forcats)
library(orderly2)
library(ggsci)

orderly_strict_mode()
orderly2::orderly_parameters(pathogen = "OROV")
#pathogen <- "OROV"

orderly2::orderly_artefact(description = "inputs folder",
                           files = "inputs/")

# did not like latest(parameter:pathogen == this:pathogen) - fix in future - seems to have resolved itself for now...
orderly_dependency(
  name = "db_compilation_orov",
  query = "latest(parameter:pathogen == this:pathogen)",
  files = c("inputs/articles.csv"="articles.csv",
            "inputs/parameters.csv"="parameters.csv",
            "inputs/outbreaks.csv"="outbreaks.csv"))

# forest plot code
#orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
#source("lassa_functions.R")
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

d1 <- parameters |> filter(parameter_class == "Delays") %>% 
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
                                  "Symptom onset -> Discharge from care") ~ "Symptoms to care",
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

## maybe this is cheating but do a little tweak to parameter_value_type here so only this way for delays and not elsewhere
d1 <- d1 %>% mutate(
  parameter_value_type = case_when(
    parameter_value_type %in% c("Unspecified","Other","Central - unspecified") ~ "Unspecified",
    .default = parameter_value_type
  )
)


#forest_plot(d1)
qa_threshold <- -1
qa_alpha <- 1
point_size <- 4.5
text_size <- 20

custom_colour_pop_groups <- get_colour_pop_groups(
  parameters,
  d1
)

# testing it works overall
forest_plot(
  d1 |>
    filter(qa_score > qa_threshold),
  "Delays (days)",
  "location_label",
  c(0,40),
  custom_colours = NA,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size) + 
  facet_wrap(~delay_label_short,scales="free_y",nrow=10)

p1 <- forest_plot(
  d1 |>
    filter(delay_facet_label == "Symptoms"),
  "Days",
  "location_label",
  c(-1,31),
  custom_colours = NA,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size) + 
  facet_wrap(~delay_label_short,scales="free_y",ncol=1,strip.position = "right") + ggtitle("Symptoms")#+ 
#theme(legend.position = "bottom") +  guides(colour = guide_legend(ncol = 3) )

p2 <- forest_plot(
  d1 |>
    filter(delay_facet_label == "Symptoms to care"),
  "Days",
  "location_label",
  c(-1,31),
  custom_colours = NA,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size) + 
  facet_wrap(~delay_label_short,scales="free_y",ncol=1,strip.position = "right") + ggtitle("Symptoms to care")


p3 <- forest_plot(
  d1 |>
    filter(delay_facet_label == "Care to outcomes"),
  "Days",
  "location_label",
  c(-1,31),
  custom_colours = NA,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size) + 
  facet_wrap(~delay_label_short,scales="free_y",ncol=1,strip.position = "right")+ ggtitle("Care to outcome")


p4 <- forest_plot(
  d1 |>
    filter(delay_facet_label == "Recurrence"),
  "Days",
  "location_label",
  c(-1,31),
  custom_colours = NA,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size) + 
  facet_wrap(~delay_label_short,scales="free_y",ncol=1,strip.position = "right") + ggtitle("Recurrence")


p5 <- forest_plot(
  d1 |>
    filter(qa_score > qa_threshold,
           delay_facet_label == "Transmission"),
  "Days",
  "location_label",
  c(-1,31),
  custom_colours = NA,
  segment_show.legend = c(color = TRUE, shape = FALSE),
  text_size = text_size,
  qa_alpha = qa_alpha,
  sort = TRUE,
  point_size = point_size) + 
  facet_wrap(~delay_label_short,scales="free_y",ncol=1,strip.position = "right") + ggtitle("Transmission")

cowplot::plot_grid(p1+theme(legend.position = "none"),
                   p2+theme(legend.position = "none"),
                   p3+theme(legend.position = "none"),
                   p4+theme(legend.position = "none"),
                   p5+theme(legend.position = "none"),ncol=5,
                   rel_widths = c(1,1,1,1.2,0.8))
ggsave("test.png",width=30,height=20)


## for the analysis 
d1$parameter_type



# delays <- parameters %>% filter(parameter_type_broad=="Delays") %>% 
#   mutate(
#   delay_label = case_when(
#     parameter_type=="Human delay - other human delay (go to section)" ~ paste0("Human delay - ",parameter_hd_from,">",parameter_hd_to),
#     parameter_type!="Human delay - other human delay (go to section)" ~ parameter_type
#   )
# )
# 
# delays %>% filter(population_sample_size==1) %>% nrow()

# delays %>% select(parameter_type,parameter_hd_from,parameter_hd_to,delay_label) %>% View()
# 
# delays %>% filter(parameter_type=="Human delay - other human delay (go to section)"&is.na(parameter_hd_from))
# 
# delays_cleaned <- data_curation(articles,delays, TRUE, FALSE)
# 
# # forest_plot(df = delays_cleaned$parameters,
# #             label = "delays",
# #             color_column = "parameter_type",
# #             lims = c(0,60))
# 
# delays_cleaned$parameters$central
# 
# 
# df <- delays_cleaned$parameters %>% mutate(urefs = make.unique(refs)) %>%
#   mutate(urefs = factor(urefs, levels = rev(unique(urefs))),
#          location_label = case_when(
#            population_country %in% c(
#              "Brazil","Trinidad and Tobago","Cuba") ~ "Local transmission",
#            population_country %in% c(
#              "France","Italy","Switzerland") ~ "Imported cases",
#            .default = population_country
#          ),
#          delay_human_mosquito = case_when(
#            grepl("Human delay",delay_label) ~ "Human only",
#            grepl("Mosquito delay",delay_label) ~ "Mosquito only",
#            .default = "Human and Mosquito"
#          ),
#          delay_label_short = case_when(
#            delay_label=="Human delay - symptom onset>discharge/recovery" ~ "Symptom onset -> Discharge/recovery",
#            delay_label=="Human delay - Symptom Onset/Fever>Seeking Care" ~ "Symptom onset -> Seeking care",
#            delay_label=="Human delay - time in care (length of stay)" ~ "Time in care",
#            delay_label=="Human delay - incubation period" ~ "Incubation period",
#            delay_label=="Human delay - symptom onset>admission to care" ~ "Symptom onset -> Admission to care",
#            delay_label=="Human delay - Symptom Onset/Fever>Discharge from Critical Care/ICU" ~ "Symptom onset -> Discharge from critical care",
#            delay_label=="Human delay - Admission to Care/Hospitalisation>Discharge from Critical Care/ICU" ~ "Admission to care -> Discharge from critical care",
#            delay_label =="Human delay - Symptom Onset/Fever>Symptom Resolution" ~ "Duration of symptoms",
#            delay_label=="Delay - human to mosquito generation time" ~ "Generation time (human to mosquito)",
#            delay_label=="Mosquito delay - extrinsic incubation period" ~ "Extrinsic incubation period",
#            delay_label=="Delay - mosquito to human generation time" ~ "Generation time (mosquito to human)",
#            delay_label=="Human delay - generation time" ~ "Generation time",
#            delay_label=="Human delay - admission to care>death" ~ "Admission to care -> Death",
#            .default = "You missed one"
#          ),
#          delay_label_short = factor(
#            delay_label_short,
#            levels = c("Incubation period",
#                       "Generation time",
#                       "Symptom onset -> Seeking care",
#                       "Symptom onset -> Admission to care",
#                       "Admission to care -> Discharge from critical care",
#                       "Symptom onset -> Discharge from critical care",
#                       "Admission to care -> Death",
#                       "Time in care",
#                       "Symptom onset -> Discharge/recovery",
#                       "Symptom onset -> Death",
#                       "Duration of symptoms",
#                       "Extrinsic incubation period",
#                       "Generation time (human to mosquito)",
#                       "Generation time (mosquito to human)")
#          )
#          )
# 


# #cats <- length(unique(df[[color_column]]))
# color_column <- "location_label"
# lims <- c(0,30)
# label <- "Delays (Days)"
# #custom_colours <- NA
# text_size <- 11
# 
# ggplot(df) +
#   geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
#                    y = urefs, yend = urefs, color = .data[[color_column]]),
#                size = 3, alpha = 0.65) +
#   geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
#                     y = urefs),
#                 width = 0.15, lwd=0.5, color = "black", alpha = 1) +
#   geom_point(aes(x = parameter_value, y = urefs, 
#                  shape = parameter_value_type, fill = .data[[color_column]]),
#              size = 3, stroke = 1,
#              color = "black", alpha = 1)+ 
#   scale_fill_lancet(palette = "lanonc") + 
#   scale_color_lancet(palette = "lanonc") +
#   scale_shape_manual(name = "Parameter Type",values = c(Mean = 21, Median = 22, Other = 23, `Case Study` = 25),
#                      breaks = c("Mean", "Median",
#                                 "Other","Case Study")) +
#   facet_wrap(~delay_label_short,scales="free_y",nrow=5,
#              labeller = label_wrap_gen())+
#   scale_x_continuous(limits = lims, expand = c(0, 0)) +
#   scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
#   labs(x = label, y = NULL, 
#        color = "Population country",fill="Population country") +
#   theme_minimal() + 
#   theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
#         text = element_text(size = text_size))  
# 
# ## break down into the different types 
# 
# a <- ggplot(df %>% filter(delay_human_mosquito=="Mosquito only")) +
#   geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
#                    y = urefs, yend = urefs, color = .data[[color_column]]),
#                size = 3, alpha = 0.65) +
#   geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
#                     y = urefs),
#                 width = 0.15, lwd=0.5, color = "black", alpha = 1) +
#   geom_point(aes(x = parameter_value, y = urefs, 
#                  shape = parameter_value_type, fill = .data[[color_column]]),
#              size = 3, stroke = 1,
#              color = "black", alpha = 1)+ 
#   #scale_fill_lancet(palette = "lanonc") + 
#   scale_fill_manual(name = "location_label",
#                     values = c(`Local transmission` = "#00468BFF",
#                                `Imported cases`= "#ED0000FF"))+
#   scale_color_manual(name = "location_label",
#                     values = c(`Local transmission` = "#00468BFF",
#                                `Imported cases`= "#ED0000FF"))+
#   scale_shape_manual(name = "Parameter Type",values = c(Mean = 21, Median = 22, Other = 23, `Case Study` = 25),
#                      breaks = c("Mean", "Median",
#                                 "Other","Case Study")) +
#   facet_wrap(~delay_label_short,scales="free_y",nrow=5,
#              labeller = label_wrap_gen(width=30))+
#   scale_x_continuous(limits = c(0,22), expand = c(0, 0)) +
#   scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
#   labs(x = label, y = NULL, 
#        color = "",fill="", tag="A") +
#   theme_minimal() + 
#   theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
#         text = element_text(size = text_size),legend.position = "None")  
# 
# b <- ggplot(df %>% filter(delay_human_mosquito=="Human and Mosquito")) +
#   geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
#                    y = urefs, yend = urefs, color = .data[[color_column]]),
#                size = 3, alpha = 0.65) +
#   geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
#                     y = urefs),
#                 width = 0.15, lwd=0.5, color = "black", alpha = 1) +
#   geom_point(aes(x = parameter_value, y = urefs, 
#                  shape = parameter_value_type, fill = .data[[color_column]]),
#              size = 3, stroke = 1,
#              color = "black", alpha = 1)+ 
#   scale_fill_manual(name = "location_label",
#                     values = c(`Local transmission` = "#00468BFF",
#                                `Imported cases`= "#ED0000FF"))+
#   scale_color_manual(name = "location_label",
#                      values = c(`Local transmission` = "#00468BFF",
#                                 `Imported cases`= "#ED0000FF"))+
#   scale_shape_manual(name = "Parameter Type",values = c(Mean = 21, Median = 22, Other = 23, `Case Study` = 25),
#                      breaks = c("Mean", "Median",
#                                 "Other","Case Study")) +
#   facet_wrap(~delay_label_short,scales="free_y",nrow=5,
#              labeller = label_wrap_gen(width=30))+
#   scale_x_continuous(limits = c(0,22), expand = c(0, 0)) +
#   scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
#   labs(x = label, y = NULL, 
#        color = "Population country",fill="Population country", tag="B") +
#   theme_minimal() + 
#   theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
#         text = element_text(size = text_size),legend.position = "None") 
# 
# 
# c <- ggplot(df %>% filter(delay_human_mosquito=="Human only")) +
#   geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
#                    y = urefs, yend = urefs, color = .data[[color_column]]),
#                size = 3, alpha = 0.65) +
#   geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
#                     y = urefs),
#                 width = 0.15, lwd=0.5, color = "black", alpha = 1) +
#   geom_point(aes(x = parameter_value, y = urefs, 
#                  shape = parameter_value_type, fill = .data[[color_column]]),
#              size = 3, stroke = 1,
#              color = "black", alpha = 1)+ 
#   scale_fill_manual(name = "Transmission setting",
#                     values = c(`Local transmission` = "#00468BFF",
#                                `Imported cases`= "#ED0000FF"))+
#   scale_color_manual(name = "Transmission setting",
#                      values = c(`Local transmission` = "#00468BFF",
#                                 `Imported cases`= "#ED0000FF"))+
#   scale_shape_manual(name = "Parameter Type",values = c(Mean = 21, Median = 22, Other = 23, `Case Study` = 25),
#                      breaks = c("Mean", "Median",
#                                 "Other","Case Study")) +
#   facet_wrap(~delay_label_short,scales="free_y",nrow=10,
#              labeller = label_wrap_gen(width=30))+
#   scale_x_continuous(limits = c(0,30), expand = c(0, 0)) +
#   scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
#   labs(x = label, y = NULL, 
#        color = "Transmission setting",fill="Transmission setting", tag="C") +
#   theme_minimal() + 
#   theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
#         text = element_text(size = text_size)) 
# 
# cowplot::plot_grid(cowplot::plot_grid(a,b,ncol=1,rel_heights = c(1,2)),
#                    c,ncol=2,rel_widths = c(1,3))


