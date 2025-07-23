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
  name = "db_compilation",
  query = "latest(parameter:pathogen == this:pathogen)",
  files = c("inputs/articles.csv"="articles.csv",
            "inputs/parameters.csv"="parameters.csv",
            "inputs/outbreaks.csv"="outbreaks.csv"))

# forest plot code
#orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
#source("lassa_functions.R")
source("orov_functions.R")

# read in data
articles <- read.csv("inputs/articles.csv")
outbreaks <- read.csv("inputs/outbreaks.csv")
parameters <- read.csv("inputs/parameters.csv")


# ## for the purposes of plotting sort the variability/uncertainty thing 
# Barbiero 2025
# Gourjalt 2025
# Castilleti 2024b
# 
# delays <- parameters %>% filter(parameter_type_broad=="Delays")
# write.csv(delays,"clean_delays.csv",row.names=FALSE)
# delays$article_label
# delays %>% filter(article_label=="Barbiero 2025")
# 
# delays$parameter_value_type[which(delays$article_label=="Barbiero 2025")] <- "Other"
# 
# delays_cleaned$parameters %>% filter(article_label=="Barbiero 2025")


# ## human delays
delays <- parameters %>% filter(parameter_type_broad=="Delays") %>%
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
write.csv(delays,
          "delay_clean.csv",row.names=FALSE)


delays <- parameters %>% filter(parameter_type_broad=="Delays") %>% 
  mutate(
  delay_label = case_when(
    parameter_type=="Human delay - other human delay (go to section)" ~ paste0("Human delay - ",parameter_hd_from,">",parameter_hd_to),
    parameter_type!="Human delay - other human delay (go to section)" ~ parameter_type
  )
)

# delays %>% select(parameter_type,parameter_hd_from,parameter_hd_to,delay_label) %>% View()
# 
# delays %>% filter(parameter_type=="Human delay - other human delay (go to section)"&is.na(parameter_hd_from))

delays_cleaned <- data_curation(articles,delays, TRUE, FALSE)

forest_plot(df = delays_cleaned$parameters,
            label = "delays",
            color_column = "parameter_type",
            lims = c(0,60))

delays_cleaned$parameters$central


df <- delays_cleaned$parameters %>% mutate(urefs = make.unique(refs)) %>%
  mutate(urefs = factor(urefs, levels = rev(unique(urefs))),
         location_label = case_when(
           population_country %in% c(
             "Brazil","Trinidad and Tobago","Cuba") ~ "Local transmission",
           population_country %in% c(
             "France","Italy","Switzerland") ~ "Imported cases",
           .default = population_country
         ),
         delay_human_mosquito = case_when(
           grepl("Human delay",delay_label) ~ "Human only",
           grepl("Mosquito delay",delay_label) ~ "Mosquito only",
           .default = "Human and Mosquito"
         ),
         delay_label_short = case_when(
           delay_label=="Human delay - symptom onset>discharge/recovery" ~ "Symptom onset -> Discharge/recovery",
           delay_label=="Human delay - Symptom Onset/Fever>Seeking Care" ~ "Symptom onset -> Seeking care",
           delay_label=="Human delay - time in care (length of stay)" ~ "Time in care",
           delay_label=="Human delay - incubation period" ~ "Incubation period",
           delay_label=="Human delay - symptom onset>admission to care" ~ "Symptom onset -> Admission to care",
           delay_label=="Human delay - Symptom Onset/Fever>Discharge from Critical Care/ICU" ~ "Symptom onset -> Discharge from critical care",
           delay_label=="Human delay - Admission to Care/Hospitalisation>Discharge from Critical Care/ICU" ~ "Admission to care -> Discharge from critical care",
           delay_label =="Human delay - Symptom Onset/Fever>Symptom Resolution" ~ "Duration of symptoms",
           delay_label=="Delay - human to mosquito generation time" ~ "Generation time (human to mosquito)",
           delay_label=="Mosquito delay - extrinsic incubation period" ~ "Extrinsic incubation period",
           delay_label=="Delay - mosquito to human generation time" ~ "Generation time (mosquito to human)",
           delay_label=="Human delay - generation time" ~ "Generation time",
           delay_label=="Human delay - admission to care>death" ~ "Admission to care -> Death",
           .default = "You missed one"
         ),
         delay_label_short = factor(
           delay_label_short,
           levels = c("Incubation period",
                      "Generation time",
                      "Symptom onset -> Seeking care",
                      "Symptom onset -> Admission to care",
                      "Admission to care -> Discharge from critical care",
                      "Symptom onset -> Discharge from critical care",
                      "Admission to care -> Death",
                      "Time in care",
                      "Symptom onset -> Discharge/recovery",
                      "Symptom onset -> Death",
                      "Duration of symptoms",
                      "Extrinsic incubation period",
                      "Generation time (human to mosquito)",
                      "Generation time (mosquito to human)")
         )
         )



#cats <- length(unique(df[[color_column]]))
color_column <- "location_label"
lims <- c(0,30)
label <- "Delays (Days)"
#custom_colours <- NA
text_size <- 11

ggplot(df) +
  geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                   y = urefs, yend = urefs, color = .data[[color_column]]),
               size = 3, alpha = 0.65) +
  geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                    y = urefs),
                width = 0.15, lwd=0.5, color = "black", alpha = 1) +
  geom_point(aes(x = parameter_value, y = urefs, 
                 shape = parameter_value_type, fill = .data[[color_column]]),
             size = 3, stroke = 1,
             color = "black", alpha = 1)+ 
  scale_fill_lancet(palette = "lanonc") + 
  scale_color_lancet(palette = "lanonc") +
  scale_shape_manual(name = "Parameter Type",values = c(Mean = 21, Median = 22, Other = 23, `Case Study` = 25),
                     breaks = c("Mean", "Median",
                                "Other","Case Study")) +
  facet_wrap(~delay_label_short,scales="free_y",nrow=5,
             labeller = label_wrap_gen())+
  scale_x_continuous(limits = lims, expand = c(0, 0)) +
  scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
  labs(x = label, y = NULL, 
       color = "Population country",fill="Population country") +
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        text = element_text(size = text_size))  

## break down into the different types 

a <- ggplot(df %>% filter(delay_human_mosquito=="Mosquito only")) +
  geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                   y = urefs, yend = urefs, color = .data[[color_column]]),
               size = 3, alpha = 0.65) +
  geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                    y = urefs),
                width = 0.15, lwd=0.5, color = "black", alpha = 1) +
  geom_point(aes(x = parameter_value, y = urefs, 
                 shape = parameter_value_type, fill = .data[[color_column]]),
             size = 3, stroke = 1,
             color = "black", alpha = 1)+ 
  #scale_fill_lancet(palette = "lanonc") + 
  scale_fill_manual(name = "location_label",
                    values = c(`Local transmission` = "#00468BFF",
                               `Imported cases`= "#ED0000FF"))+
  scale_color_manual(name = "location_label",
                    values = c(`Local transmission` = "#00468BFF",
                               `Imported cases`= "#ED0000FF"))+
  scale_shape_manual(name = "Parameter Type",values = c(Mean = 21, Median = 22, Other = 23, `Case Study` = 25),
                     breaks = c("Mean", "Median",
                                "Other","Case Study")) +
  facet_wrap(~delay_label_short,scales="free_y",nrow=5,
             labeller = label_wrap_gen(width=30))+
  scale_x_continuous(limits = c(0,22), expand = c(0, 0)) +
  scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
  labs(x = label, y = NULL, 
       color = "",fill="", tag="A") +
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        text = element_text(size = text_size),legend.position = "None")  

b <- ggplot(df %>% filter(delay_human_mosquito=="Human and Mosquito")) +
  geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                   y = urefs, yend = urefs, color = .data[[color_column]]),
               size = 3, alpha = 0.65) +
  geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                    y = urefs),
                width = 0.15, lwd=0.5, color = "black", alpha = 1) +
  geom_point(aes(x = parameter_value, y = urefs, 
                 shape = parameter_value_type, fill = .data[[color_column]]),
             size = 3, stroke = 1,
             color = "black", alpha = 1)+ 
  scale_fill_manual(name = "location_label",
                    values = c(`Local transmission` = "#00468BFF",
                               `Imported cases`= "#ED0000FF"))+
  scale_color_manual(name = "location_label",
                     values = c(`Local transmission` = "#00468BFF",
                                `Imported cases`= "#ED0000FF"))+
  scale_shape_manual(name = "Parameter Type",values = c(Mean = 21, Median = 22, Other = 23, `Case Study` = 25),
                     breaks = c("Mean", "Median",
                                "Other","Case Study")) +
  facet_wrap(~delay_label_short,scales="free_y",nrow=5,
             labeller = label_wrap_gen(width=30))+
  scale_x_continuous(limits = c(0,22), expand = c(0, 0)) +
  scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
  labs(x = label, y = NULL, 
       color = "Population country",fill="Population country", tag="B") +
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        text = element_text(size = text_size),legend.position = "None") 


c <- ggplot(df %>% filter(delay_human_mosquito=="Human only")) +
  geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                   y = urefs, yend = urefs, color = .data[[color_column]]),
               size = 3, alpha = 0.65) +
  geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                    y = urefs),
                width = 0.15, lwd=0.5, color = "black", alpha = 1) +
  geom_point(aes(x = parameter_value, y = urefs, 
                 shape = parameter_value_type, fill = .data[[color_column]]),
             size = 3, stroke = 1,
             color = "black", alpha = 1)+ 
  scale_fill_manual(name = "Transmission setting",
                    values = c(`Local transmission` = "#00468BFF",
                               `Imported cases`= "#ED0000FF"))+
  scale_color_manual(name = "Transmission setting",
                     values = c(`Local transmission` = "#00468BFF",
                                `Imported cases`= "#ED0000FF"))+
  scale_shape_manual(name = "Parameter Type",values = c(Mean = 21, Median = 22, Other = 23, `Case Study` = 25),
                     breaks = c("Mean", "Median",
                                "Other","Case Study")) +
  facet_wrap(~delay_label_short,scales="free_y",nrow=10,
             labeller = label_wrap_gen(width=30))+
  scale_x_continuous(limits = c(0,30), expand = c(0, 0)) +
  scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
  labs(x = label, y = NULL, 
       color = "Transmission setting",fill="Transmission setting", tag="C") +
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        text = element_text(size = text_size)) 

cowplot::plot_grid(cowplot::plot_grid(a,b,ncol=1,rel_heights = c(1,2)),
                   c,ncol=2,rel_widths = c(1,3))


