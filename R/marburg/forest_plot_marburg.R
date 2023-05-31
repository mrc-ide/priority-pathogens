## function to produce forest plot
## to go into data_analysis.R when up and running

# doing on 'parameter_single' until get finalised data

REGENERATE_DATA <- FALSE

if(REGENERATE_DATA) source("R/marburg/script_marburg.R")              #only rerun this if we want to re-generate the marburg files (rather than rewriting this every time) 

library(ggplot2)
library(tidyverse)
library(patchwork)
library(cowplot)

df <- read.csv("data/marburg/final/parameter_final.csv")
article_df <- read.csv("data/marburg/final/article_final.csv")

# merge with article ID to get the y-axis labels
df <- merge(df,article_df %>% dplyr::select(article_id,first_author_first_name,year_publication),
            all.x=TRUE,by="article_id") %>%
  mutate(article_label = as.character(paste0(first_author_first_name," ",year_publication))) %>%
  dplyr::arrange(article_label, -year_publication) %>%
  dplyr::filter(article_id %in% c(17,15) == FALSE) 

human_delay <- forest_plot_delay(df)
#ggsave(plot = human_delay,filename="data/marburg/output/FP_human_delay.png",bg = "white",width = 25, height = 15, units = "cm")

mutations <- forest_plot_mutations(df)
#ggsave(plot = mutations,filename="data/marburg/output/FP_mutations.png",bg = "white",width = 15, height = 10, units = "cm")


reproduction_number <- forest_plot_R(df)
#ggsave(plot = reproduction_number,filename="data/marburg/output/FP_reproduction_number.png",bg = "white",width = 15, height = 10, units = "cm")

severity <- forest_plot_fr(df)
#ggsave(plot = severity,filename="data/marburg/output/FP_severity.png",bg = "white",width = 15, height = 10, units = "cm")


plot_grid(reproduction_number+labs(tag="A"),
          severity+labs(tag="B"),
          human_delay+labs(tag="C"),
          mutations+labs(tag="D"),
          nrow=2,align="hv",rel_heights = c(0.7,1))
ggsave(filename="data/marburg/output/panel_plot.png",bg = "white",width = 15, height=10)


#panel_plot <- (reproduction_number + ggtitle("A") + severity + ggtitle("B")) / (human_delay + ggtitle("C") + mutations + ggtitle("D"))
#ggsave(plot = panel_plot,filename="data/marburg/output/panel_plot.png",bg = "white",width = 15, height=10)




