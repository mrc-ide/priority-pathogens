## script to compare an overview of the parameter values, types of each 
# parameters and what we do and do not currently know
library(ggplot2)
library(tidyverse)

parameter_df <- read.csv("data/marburg/final/parameter_final.csv")
parameter_df$parameter_class[parameter_df$parameter_class == "Other"] <- "Other transmission parameters" ## Anne's suggestion to rename this

# there is a problem that one seroprevalence IgG is not labelled with a parameter class so sorting this here as its messing up the plot (RM)
# doing a hacky fix because its only one obs
parameter_df <- parameter_df %>% mutate(parameter_class = ifelse(parameter_data_id==11,
                                                                 "Seroprevalence",parameter_class))


parameter_df <- parameter_df %>% dplyr::filter(!is.na(parameter_type)) %>%
  dplyr::arrange(parameter_class, parameter_value)

parameter_df$parameter_class <- factor(parameter_df$parameter_class, levels = unique(parameter_df$parameter_class))
parameter_df$parameter_type <- factor(parameter_df$parameter_type, levels = unique(parameter_df$parameter_type))

ggplot(parameter_df, aes(x = parameter_type, col = parameter_class, fill = parameter_class)) + 
  geom_bar() + theme_bw() +
  theme(axis.text.x = element_text(angle = 315, vjust = 0, hjust=0)) +
  labs(x = "Parameter type", y = "Number extracted") + 
  guides(fill = guide_legend(title="Parameter Classification"), 
         col = guide_legend(title="Parameter Classification")) + 
  scale_y_continuous(breaks = seq(0, 10, by = 2))+
  scale_fill_viridis_d(option = "magma",begin=0.15,end=0.95)+
  scale_colour_viridis_d(option="magma",begin=0.15,end=0.95)

ggsave(filename="data/marburg/output/parameter_overview.png",bg = "white",
       width = 15, height = 10, units = "cm")
