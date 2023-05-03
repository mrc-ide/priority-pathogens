## function to produce forest plot
## to go into data_analysis.R when up and running

# doing on 'parameter_single' until get finalised data

REGENERATE_DATA <- FALSE

if(REGENERATE_DATA) source("R/script.R")              #only rerun this if we want to re-generate the marburg files (rather than rewriting this every time) 

library(ggplot2)
library(tidyverse)

df <- read.csv("data/marburg/final/parameter_final.csv")
article_df <- read.csv("data/marburg/final/article_final.csv")

# merge with article ID to get the y-axis labels
df <- merge(df,article_df %>% dplyr::select(article_id,first_author_first_name,year_publication),
            all.x=TRUE,by="article_id") %>%
  mutate(article_label = as.character(paste0(first_author_first_name," ",year_publication))) %>%
  dplyr::arrange(article_label, -year_publication) %>%
  dplyr::filter(article_id %in% c(17,15) == FALSE) 


# fatality rates 
forest_plot_fr <- function(df) {
  
  parameter <- "Severity"
  
  df_cfr <- df %>% filter(parameter_class == parameter) %>%
    mutate(parameter_value = as.numeric(parameter_value)) %>%
    group_by(parameter_type) %>%
    dplyr::arrange(first_author_first_name)
  
  df_cfr$pooled <- (sum(df_cfr$cfr_ifr_numerator, na.rm = TRUE)/sum(df_cfr$cfr_ifr_denominator, na.rm = TRUE))*100
  p <- df_cfr$pooled[1]/100
  n <- sum(df_cfr$cfr_ifr_denominator, na.rm = TRUE)
  df_cfr$pooled_low <- (p - 1.96*(sqrt((p * (1-p))/n)))*100
  df_cfr$pooled_upp <- (p + 1.96*(sqrt((p * (1-p))/n)))*100
  
  df_plot <- df_cfr %>% 
    dplyr::filter(is.na(parameter_value) == FALSE) %>%
    dplyr::select(c(article_id:distribution_par2_uncertainty, covidence_id:pooled_upp)) %>%
    dplyr::arrange((cfr_ifr_method))
  
  ## ensuring that each entry is on it's own line
  df_plot$article_label_unique <- make.unique(df_plot$article_label)
  
  plot <- ggplot(df_plot, aes(x=parameter_value, y=article_label_unique, 
                              col = cfr_ifr_method)) + 
    theme_bw() + geom_point() +
    scale_y_discrete(labels=setNames(df_plot$article_label, df_plot$article_label_unique)) +
    facet_grid(cfr_ifr_method ~ ., scales = "free", space = "free") +
    geom_errorbar(aes(y=article_label_unique,xmin=parameter_lower_bound,xmax=parameter_upper_bound,
                      group=parameter_data_id,
                      linetype="Parameter range"),
                  position=position_dodge(width=0.5),
                  width=0.25) +
    geom_errorbar(aes(y=article_label_unique,
                      xmin=parameter_uncertainty_lower_value,xmax=parameter_uncertainty_upper_value,
                      group=parameter_data_id,
                      linetype="Uncertainty interval"),
                  position=position_dodge(width=0.5),
                  width=0.25) +
    labs(x="Parameter value",y="Study (First author surname and publication year)",
         linetype="",colour="") + 
    scale_linetype_manual(values = c("dotted","solid"))+
    theme(legend.position="bottom",
          strip.text = element_text(size=12)) +
    xlim(c(0, 100)) 
  
  return(plot)
  
}

## reproduction numbers
forest_plot_R <- function(df){
  
  parameter <- "Reproduction number"
  
  df_R <- df %>% filter(parameter_class == parameter) %>%
    mutate(parameter_value = as.numeric(parameter_value)) %>%
    group_by(parameter_type) %>%
    dplyr::arrange(first_author_first_name)
  
  df_plot <- df_R %>% filter(parameter_class == parameter) %>%
    mutate(parameter_value = as.numeric(parameter_value)) %>%
    group_by(parameter_type) %>% ### median function not behaving in ggplot so going with this even with grouping
    mutate(median = median(parameter_value,na.rm=TRUE))
  
  df_plot$article_label_unique <- make.unique(df_plot$article_label)
  
  plot <-
    ggplot(df_plot, aes(x = parameter_value, y = article_label_unique, col = parameter_type))+
    theme_minimal()+
    # scale_x_continuous(breaks = seq(0, 2, by = 0.2))+
    geom_errorbar(aes(y=article_label,xmin=parameter_lower_bound,xmax=parameter_upper_bound,
                      group=parameter_data_id,
                      linetype="Parameter range"),
                  position=position_dodge(width=0.5),
                  width=0.25)+
    geom_errorbar(aes(y=article_label,
                      xmin=parameter_uncertainty_lower_value,xmax=parameter_uncertainty_upper_value,
                      group=parameter_data_id,
                      linetype="Uncertainty interval"),
                  position=position_dodge(width=0.5),
                  width=0.25)+
    geom_point(aes(x=parameter_value,y=article_label,group=parameter_data_id),position=position_dodge(width=0.5))+
    # geom_vline(aes(xintercept=median,col="Sample median"),linetype="dashed", colour = "grey") +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "black") +
    labs(x="Parameter value",y="Study (First author surname and publication year)",linetype="",colour="")+
    scale_linetype_manual(values = c("dotted","solid"))+
    scale_colour_discrete(labels = c("Basic R0", "Effective Re")) +
    theme(legend.position="bottom",
          legend.text = element_text(size=10),
          strip.text = element_text(size=12)) + xlim(c(0,2)) 
  
  return(plot)
  
}

## human delays
forest_plot_delay <- function(df){
  
  parameter <- "Human delay"
  
  df_delay <- df %>% 
    dplyr::filter(parameter_class == parameter) %>%
    dplyr::mutate(parameter_value = as.numeric(parameter_value)) %>%
    dplyr::group_by(parameter_type) %>%
    dplyr::arrange(first_author_first_name) %>%
    dplyr::filter(parameter_type != "Human delay - generation time")
  
  df_plot <- df_delay %>% 
    dplyr::filter(parameter_class == parameter) %>%
    dplyr::mutate(parameter_value = as.numeric(parameter_value)) %>%
    dplyr::group_by(parameter_type) %>% ### median function not behaving in ggplot so going with this even with grouping
    mutate(median = median(parameter_value,na.rm=TRUE))
  
  df_plot$article_label_unique <- make.unique(df_plot$article_label)
  
  plot <- ggplot(df_plot, aes(x=parameter_value, y=article_label_unique, 
                              col = parameter_type)) + 
    theme_bw() + geom_point() +
    scale_y_discrete(labels=setNames(df_plot$article_label, df_plot$article_label_unique)) +
    facet_grid(parameter_type ~ ., scales = "free", space = "free") +
    geom_errorbar(aes(y=article_label_unique,xmin=parameter_lower_bound,xmax=parameter_upper_bound,
                      group=parameter_data_id,
                      linetype="Parameter range"),
                  # position=position_dodge(width=0.5),
                  width=0.4) +
    geom_errorbar(aes(y=article_label_unique,
                      xmin=parameter_uncertainty_lower_value,xmax=parameter_uncertainty_upper_value,
                      group=parameter_data_id,
                      linetype="Uncertainty interval"),
                  # position=position_dodge(width=0.5),
                  width = 0.4) +
    labs(x="Parameter value",y="Study (First author surname and publication year)",
         linetype="",colour="") + 
    scale_linetype_manual(values = c("dotted","solid")) +
    scale_colour_discrete(labels = c("Incubation period", 
                                     "Time in care",
                                     "Symptom to careseeking",
                                     "Symptom to outcome"))

  
  
  return(plot)
  
}

## mutations -- NOTE: come back and check the index notation on these values 
forest_plot_mutations <- function(df){
  
  parameter <- "Mutations"
  
  df_mutations <- df %>% 
    dplyr::filter(parameter_class == parameter) %>%
    dplyr::mutate(parameter_value = as.numeric(parameter_value)) %>%
    dplyr::group_by(parameter_type) %>%
    dplyr::arrange(first_author_first_name)
  
  df_plot <- df_mutations %>% 
    dplyr::filter(parameter_class == parameter) %>%
    dplyr::mutate(parameter_value = as.numeric(parameter_value)) %>%
    dplyr::group_by(parameter_type) #%>% ### median function not behaving in ggplot so going with this even with grouping
    # mutate(median = median(parameter_value,na.rm=TRUE))
  
  df_plot$article_label_unique <- make.unique(df_plot$article_label)
  
  plot <- ggplot(df_plot, aes(x=parameter_value, y=article_label_unique, 
                              col = parameter_type)) + 
    theme_bw() + geom_point() +
    scale_y_discrete(labels=setNames(df_plot$article_label, df_plot$article_label_unique)) +
    facet_grid(parameter_type ~ ., scales = "free", space = "free") +
    geom_errorbar(aes(y=article_label_unique,xmin=parameter_lower_bound,xmax=parameter_upper_bound,
                      group=parameter_data_id,
                      linetype="Parameter range"),
                  # position=position_dodge(width=0.5),
                  width=0.4) +
    geom_errorbar(aes(y=article_label_unique,
                      xmin=parameter_uncertainty_lower_value,xmax=parameter_uncertainty_upper_value,
                      group=parameter_data_id,
                      linetype="Uncertainty interval"),
                  # position=position_dodge(width=0.5),
                  width = 0.4) +
    labs(x="Parameter value",y="Study (First author surname and publication year)",
         linetype="",colour="") + 
    scale_linetype_manual(values = c("dotted","solid"))
  
  
  return(plot)
  
}


human_delay <- forest_plot_delay(df)
human_delay
ggsave(plot = human_delay,filename="data/marburg/output/FP_human_delay.png",bg = "white")

mutations <- forest_plot_mutations(df)
mutations
ggsave(plot = mutations,filename="data/marburg/output/FP_mutations.png",bg = "white",
       width = 25, height = 15, units = "cm")


reproduction_number <- forest_plot_R(df)
reproduction_number
ggsave(plot = reproduction_number,filename="data/marburg/output/FP_reproduction_number.png",bg = "white",
       width = 25, height = 15, units = "cm")


severity <- forest_plot_fr(df)
severity
ggsave(plot = severity,filename="data/marburg/output/FP_severity.png",bg = "white",
       width = 25, height = 15, units = "cm")

## the only figure that doesn't yet have a custom figure -- I think this is 
# better as a table regardless?
seroprevalence <- forest_plot(df,"Seroprevalence")
seroprevalence
ggsave(plot = seroprevalence,filename="data/marburg/output/FP_seroprevalence.png",bg = "white")


# for testing purposes
#parameter <- "Human delay"

# # original code from Ruth
# forest_plot <- function(df, parameter){
# 
#   if(!(parameter %in% c("Human delay","Seroprevalence","Mutations","Risk factors","Reproduction number",
#                      "Severity","Mosquito","Relative","Other"))){
#     stop("Parameter class not valid.")
#   }
# 
# 
#   df_plot <- df %>% filter(parameter_class == parameter) %>%
#     mutate(parameter_value = as.numeric(parameter_value)) %>%
#     group_by(parameter_type) %>% ### median function not behaving in ggplot so going with this even with grouping
#     mutate(median = median(parameter_value,na.rm=TRUE))
# 
#   ## need some logic for the different parameter classes
#   ## can't do for risk factors as not extracting values
#   ## don't think we can plot seroprevalence this way - needs to include time as well
#   ## how to distinguish between uncertainty reporting and range reporting
# 
#   ## need to see "Relative contribution" to be able to plot
#   ## 'Mosquito' won't be an issue for Marburg
#   ## decide what to do with "Other"
# 
#   if(parameter %in% c("Human delay","Mutations","Reproduction number","Severity")){
# 
#     plot <- ggplot(df_plot)+
#       theme_minimal()+
#       facet_wrap(~parameter_type,scales="free_x",
#                  labeller = labeller(parameter_type = label_wrap_gen(width = 25)))+
#       geom_errorbar(aes(y=article_label,xmin=parameter_lower_bound,xmax=parameter_upper_bound,
#                         group=parameter_data_id,
#                         linetype="Parameter range"),
#                     position=position_dodge(width=0.5),
#                     width=0.25)+
#       geom_errorbar(aes(y=article_label,
#                         xmin=parameter_uncertainty_lower_value,xmax=parameter_uncertainty_upper_value,
#                         group=parameter_data_id,
#                         linetype="Uncertainty interval"),
#                     position=position_dodge(width=0.5),
#                     width=0.25)+
#       geom_point(aes(x=parameter_value,y=article_label,group=parameter_data_id),position=position_dodge(width=0.5))+
#       geom_vline(aes(xintercept=median,col="Sample median"),linetype="dashed")+
#       labs(x="Parameter value",y="Study (First author surname and publication year)",linetype="",colour="")+
#       scale_linetype_manual(values = c("dotted","solid"))+
#       scale_colour_manual(values=c("blue"))+
#       theme(legend.position="bottom",
#             strip.text = element_text(size=12))
# 
#   }
# 
#   if(parameter=="Seroprevalence"){
# 
#     plot <- ggplot(df_plot,
#            aes(x=population_study_end_year,y=parameter_value,group=factor(parameter_data_id)))+
#       theme_minimal()+
#       facet_wrap(~parameter_type,scales="free_x",
#                  labeller = labeller(parameter_type = label_wrap_gen(width = 25)))+
#       geom_errorbar(position=position_dodge2(width=0.5),
#                     aes(ymin=parameter_lower_bound,ymax=parameter_upper_bound,
#                         group=parameter_data_id,col=article_label,linetype="Parameter range"))+
#       geom_errorbar(position=position_dodge2(width=0.5),
#                     aes(ymin=parameter_uncertainty_lower_value,ymax=parameter_uncertainty_upper_value,
#                         group=parameter_data_id,col=article_label,linetype="Uncertainty interval"))+
#       geom_point(position=position_dodge2(width=0.5),
#                  aes(x=population_study_end_year,y=parameter_value,
#                      col=article_label,group=parameter_data_id))+
#       labs(x="Reported end year of study",y="Parameter value",linetype="",colour="Study")+
#       scale_linetype_manual(values = c("dotted","solid"))+
#       theme(legend.position="bottom",
#              legend.box="vertical",
#             strip.text = element_text(size=12))+
#       scale_colour_viridis_d(end=0.9)+
#       guides(colour = guide_legend(order = 2),
#              linetype = guide_legend(order = 1,byrow=TRUE))
# 
#   }
# 
#   if(parameter %in% c("Risk factors","Mosquito","Relative","Other")){
#     stop("Parameter class not yet coded.")
#   }
# 
#   return(plot)
# 
# }

