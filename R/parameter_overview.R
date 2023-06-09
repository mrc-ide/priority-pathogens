## script to compare an overview of the parameter values, types of each 
# parameters and what we do and do not currently know

plot_param_overview <- function(parameter_df, pathogen){
  #' input: final parameter dataset; e.g. parameter_df <- read.csv("data/marburg/final/parameter_final.csv")
  #'        pathogen name 
  #' output: plot with pathogen class and number of papers      
  library(ggplot2)
  library(tidyverse)
  
  parameter_df <- parameter_df %>% 
    dplyr::filter(!is.na(parameter_type)) %>%
    dplyr::arrange(parameter_class, parameter_value) %>%
    # convert to factor
    mutate(parameter_class = factor(parameter_class, levels = unique(parameter_class)),
           parameter_type = factor(parameter_type, levels = unique(parameter_type))) %>%
    # change label of parameter type 
    mutate(parameter_type_short = case_when(parameter_type=="Human delay - time symptom to careseeking"~"Time symptom to careseeking",
                                            parameter_type=="Human delay - generation time"~"Generation time",
                                            parameter_type=="Human delay - time symptom to outcome"~"Time symptom to outcome",
                                            parameter_type=="Human delay - time in care"~"Time in care",
                                            parameter_type=="Human delay - incubation period"~"Incubation period",
                                            parameter_type=="Mutations - mutation rate"~"Mutation rate",
                                            parameter_type=="Mutations - substitution rate"~"Substitution rate",
                                            parameter_type=="Mutations – substitution rate"~"Substitution rate",
                                            parameter_type=="Mutations - evolutionary rate"~"Evolutionary rate",
                                            parameter_type=="Seroprevalence - IFA"~"IFA",
                                            parameter_type=="Seroprevalence - Unspecified"~"Unspecified",
                                            parameter_type=="Seroprevalence - IgG"~"IgG",
                                            parameter_type=="Seroprevalence - IgM"~"IgM",
                                            parameter_type=="Seroprevalence - HAI/HI"~"HAI/HI",
                                            parameter_type=="Severity - case fatality rate (CFR)"~"Case fatality ratio (CFR)",
                                            parameter_type=="Reproduction number (Basic R0)"~"Basic (R0)",
                                            parameter_type=="Reproduction number (Effective, Re)"~"Effective (Re)",
                                            .default = parameter_type),
           parameter_type_short = factor(parameter_type_short,
                                         levels=c("Time symptom to careseeking","Generation time",
                                                  "Time symptom to outcome","Time in care","Incubation period",
                                                  "Mutation rate","Substitution rate","Evolutionary rate",
                                                  "Growth rate (r)","Doubling time","Attack rate",
                                                  "Effective (Re)","Basic (R0)","Risk factors",
                                                  "IFA","Unspecified","IgG","IgM","HAI/HI",
                                                  "Case fatality ratio (CFR)")))
  
  ggplot(parameter_df) + 
    geom_bar(aes(x = parameter_type_short, fill = parameter_class), color = 'black', linewidth = 0.3) + 
    labs(x = "Parameter", 
         y = "Number extracted") + 
    guides(fill = guide_legend(title="Parameter Classification")) + 
    scale_y_continuous(breaks = seq(0, 10, by = 2))+
    scale_fill_viridis_d(option = "magma", begin=0.15, end=0.95)+
    coord_flip() +
    theme_bw() +
    theme(panel.grid.minor = element_line(linewidth = 0.2), 
          panel.grid.major = element_line(linewidth = 0.2),
          legend.position = c(0.75, 0.35),
          legend.title = element_text(size = 8), 
          legend.text  = element_text(size = 8),
          legend.key.size = unit(0.7, "lines"),
          legend.background = element_rect(fill = "transparent", colour = "transparent"))
  
  
  ggsave(filename=paste0("data/", pathogen,"/output/parameter_overview_shortnames.png"), bg = "white",
         width = 15, height = 10, units = "cm")
}

