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
# orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
# source("lassa_functions.R")
source("C:/Users/rom116/Documents/github/priority-pathogens/src/orov_delays/orov_functions.R")

# read in data
articles <- read.csv("inputs/articles.csv")
outbreaks <- read.csv("inputs/outbreaks.csv")
parameters <- read.csv("inputs/parameters.csv")


# range of infection prevalence
parameters %>% filter(parameter_type_broad=="Infection prevalence") %>% select(parameter_value) %>% summary()

parameters %>% filter(parameter_type_broad=="Seroprevalence") %>% select(parameter_value) %>% summary()

# create spreadsheet to be able to
write.csv(parameters %>% filter(
  parameter_type_broad %in% c("Infection prevalence","Seroprevalence"))
            ,"inf_sero_to_clean.csv",row.names=FALSE)


## infection prevalence 
inf_prev <- parameters %>% filter(parameter_type_broad=="Infection prevalence")

inf_prev_cleaned <- data_curation(articles,inf_prev, TRUE, FALSE)

df   <- inf_prev_cleaned$parameters %>% mutate(urefs = make.unique(refs)) %>%
  mutate(urefs = factor(urefs, levels = rev(unique(urefs))))
#cats <- length(unique(df[[color_column]]))
color_column <- "population_country"
lims <- c(0,100)
label <- "Infection prevalence (%)"
#custom_colours <- NA
text_size <- 11

ggplot(df %>% filter(!is.na(central))) +
  geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                   y = urefs, yend = urefs, color = .data[[color_column]]),
               size = 3, alpha = 0.65) +
  geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                    y = urefs),
                width = 0.15, lwd=0.5, color = "black", alpha = 1) +
  geom_point(aes(x = central, y = urefs, 
                 shape = parameter_value_type, fill = .data[[color_column]]),
             size = 3, stroke = 1,
             color = "black", alpha = 1)+ 
  #scale_fill_lancet(palette = "lanonc") + 
  #scale_color_lancet(palette = "lanonc") +
  scale_shape_manual(name = "Parameter Type",values = c(Mean = 21, `Central - unspecified` = 22, Unspecified = 24, Other = 23, `Case Study` = 25),
                     breaks = c("Mean", "Central - unspecified",
                                "Other","Unspecified")) +
  facet_wrap(~population_sample_type,scales="free_y",
             labeller = label_wrap_gen())+
  scale_x_continuous(limits = lims, expand = c(0, 0)) +
  scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
  labs(x = label, y = NULL, 
       color = "Population country",fill="Population country") +
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        text = element_text(size = text_size))  



## infection prevalence 
sero_prev <- parameters %>% filter(parameter_type_broad=="Seroprevalence")

seroprev_cleaned <- data_curation(articles,sero_prev, TRUE, FALSE)

df2   <- seroprev_cleaned$parameters %>% mutate(urefs = make.unique(refs)) %>%
  mutate(urefs = factor(urefs, levels = rev(unique(urefs))))
#cats <- length(unique(df[[color_column]]))
color_column <- "population_country"
lims <- c(0,100)
label <- "Infection prevalence (%)"
#custom_colours <- NA
text_size <- 11

ggplot(df2 %>% filter(!is.na(central))) +
  geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                   y = urefs, yend = urefs, color = .data[[color_column]]),
               size = 3, alpha = 0.65) +
  geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                    y = urefs),
                width = 0.15, lwd=0.5, color = "black", alpha = 1) +
  geom_point(aes(x = central, y = urefs, 
                 shape = parameter_value_type, fill = .data[[color_column]]),
             size = 3, stroke = 1,
             color = "black", alpha = 1)+ 
  #scale_fill_lancet(palette = "lanonc") + 
  #scale_color_lancet(palette = "lanonc") +
  scale_shape_manual(name = "Parameter Type",values = c(Mean = 21, `Central - unspecified` = 22, Unspecified = 24, Other = 23, `Case Study` = 25),
                     breaks = c("Mean", "Central - unspecified",
                                "Other","Unspecified")) +
  facet_wrap(~population_sample_type,scales="free_y",nrow=4,
             labeller = label_wrap_gen())+
  scale_x_continuous(limits = lims, expand = c(0, 0)) +
  scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
  labs(x = label, y = NULL, 
       color = "Population country",fill="Population country") +
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        text = element_text(size = text_size))  



## create plot of non-human estimates
# change multi-country country label




