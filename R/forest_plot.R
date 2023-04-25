## function to produce forest plot
## to go into data_analysis.R when up and running

# doing on 'parameter_single' until get finalised data

source("R/script.R")
library(ggplot2)
df <- read.csv("data/marburg/final/parameter_final.csv")
article_df <- read.csv("data/marburg/final/article_final.csv")

# merge with article ID to get the y-axis labels
df <- merge(df,article_df %>% dplyr::select(article_id,first_author_first_name,year_publication),
            all.x=TRUE,by="article_id") %>%
  mutate(article_label = as.character(paste0(first_author_first_name," ",year_publication))) %>%
  dplyr::arrange(article_label)

# for testing purposes
#parameter <- "Human delay"

## original code from Ruth
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

forest_plot_fr <- function(df) {
  
  parameter <- "Severity"
  
  df_cfr <- df %>% filter(parameter_class == parameter) %>%
    mutate(parameter_value = as.numeric(parameter_value)) %>%
    group_by(parameter_type) %>%
    dplyr::arrange(first_author_first_name) #%>%
    # dplyr::filter(article_label != "Nyakarahuka 2016")
  # df_cfr$article_label <- factor(df_cfr$article_label)
    
  # estimate pooled CFR based on num and denom
  df_cfr$pooled <- (sum(df_cfr$cfr_ifr_numerator, na.rm = TRUE)/sum(df_cfr$cfr_ifr_denominator, na.rm = TRUE))*100
  p <- df_cfr$pooled[1]/100
  n <- sum(df_cfr$cfr_ifr_denominator, na.rm = TRUE)
  df_cfr$pooled_low <- (p - 1.96*(sqrt((p * (1-p))/n)))*100
  df_cfr$pooled_upp <- (p + 1.96*(sqrt((p * (1-p))/n)))*100
  
  df_plot <- df_cfr %>% 
    dplyr::filter(is.na(parameter_value) == FALSE) %>%
    dplyr::select(c(article_id:distribution_par2_uncertainty, covidence_id:pooled_upp)) %>%
    dplyr::arrange((article_label))

  plot <- ggplot(df_plot, aes(x=parameter_value, y=article_label, group=parameter_data_id))+
    theme_minimal()+
    facet_grid(cfr_ifr_method ~ .) +
    geom_point(aes())+
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
    geom_vline(aes(xintercept=pooled,col="Pooled unadjusted CFR"),linetype="dashed") + 
    geom_rect(aes(xmin = pooled_low,xmax = pooled_upp, ymin = -Inf, ymax = Inf),fill = "blue", alpha = 0.05) +
    labs(x="Parameter value",y="Study (First author surname and publication year)",linetype="",colour="")+
    scale_linetype_manual(values = c("dotted","solid","dashed"))+
    scale_colour_manual(values=c("blue"))+
    theme(legend.position="bottom",
          strip.text = element_text(size=12)) +
    xlim(c(0, 100))

  return(plot)
  
}

human_delay <- forest_plot(df,"Human delay")
human_delay
ggsave(plot = human_delay,filename="data/marburg/output/FP_human_delay.png",bg = "white")

mutations <- forest_plot(df,"Mutations")
mutations
ggsave(plot = mutations,filename="data/marburg/output/FP_mutations.png",bg = "white")


reproduction_number <- forest_plot(df,"Reproduction number")
reproduction_number
ggsave(plot = reproduction_number,filename="data/marburg/output/FP_reproduction_number.png",bg = "white")


severity <- forest_plot_fr(df)
severity
ggsave(plot = severity,filename="data/marburg/output/FP_severity.png",bg = "white")

seroprevalence <- forest_plot(df,"Seroprevalence")
seroprevalence
ggsave(plot = seroprevalence,filename="data/marburg/output/FP_seroprevalence.png",bg = "white")

cfr_table_df <- df %>%
  dplyr::filter(parameter_class == "Severity")

