## function to produce forest plot
## to go into data_analysis.R when up and running

# doing on 'parameter_single' until get finalised data

source("R/script.R")
library(ggplot2)

#df <- parameter_single
# merge with article ID to get the y-axis labels
df <- merge(parameter_single,article_single %>% dplyr::select(article_id,first_author_first_name,year_publication),
            all.x=TRUE,by="article_id") %>%
  mutate(article_label = factor(paste0(first_author_first_name," ",year_publication)),
         article_label = reorder(article_label,-year_publication))

# for testing purposes
#parameter <- "Human delay"

forest_plot <- function(df, parameter){

  if(!(parameter %in% c("Human delay","Seroprevalence","Mutations","Risk factors","Reproduction number",
                     "Severity","Mosquito","Relative","Other"))){
    stop("Parameter class not valid.")
  }


  df_plot <- df %>% filter(parameter_class == parameter) %>%
    mutate(parameter_value = as.numeric(parameter_value)) %>%
    group_by(parameter_type) %>% ### median function not behaving in ggplot so going with this even with grouping
    mutate(median = median(parameter_value,na.rm=TRUE))

  ## need some logic for the different parameter classes
  ## can't do for risk factors as not extracting values
  ## don't think we can plot seroprevalence this way - needs to include time as well
  ## how to distinguish between uncertainty reporting and range reporting

  ## need to see "Relative contribution" to be able to plot
  ## 'Mosquito' won't be an issue for Marburg
  ## decide what to do with "Other"

  if(parameter %in% c("Human delay","Mutations","Reproduction number","Severity")){

    plot <- ggplot(df_plot)+
      theme_minimal()+
      facet_wrap(~parameter_type,scales="free_x",
                 labeller = labeller(parameter_type = label_wrap_gen(width = 25)))+
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
      geom_vline(aes(xintercept=median,col="Sample median"),linetype="dashed")+
      labs(x="Parameter value",y="Study (First author surname and publication year)",linetype="",colour="")+
      scale_linetype_manual(values = c("dotted","solid"))+
      scale_colour_manual(values=c("blue"))+
      theme(legend.position="bottom",
            strip.text = element_text(size=12))

  }

  if(parameter=="Seroprevalence"){

    plot <- ggplot(df_plot,
           aes(x=population_study_end_year,y=parameter_value,group=factor(parameter_data_id)))+
      theme_minimal()+
      facet_wrap(~parameter_type,scales="free_x",
                 labeller = labeller(parameter_type = label_wrap_gen(width = 25)))+
      geom_errorbar(position=position_dodge2(width=0.5),
                    aes(ymin=parameter_lower_bound,ymax=parameter_upper_bound,
                        group=parameter_data_id,col=article_label,linetype="Parameter range"))+
      geom_errorbar(position=position_dodge2(width=0.5),
                    aes(ymin=parameter_uncertainty_lower_value,ymax=parameter_uncertainty_upper_value,
                        group=parameter_data_id,col=article_label,linetype="Uncertainty interval"))+
      geom_point(position=position_dodge2(width=0.5),
                 aes(x=population_study_end_year,y=parameter_value,
                     col=article_label,group=parameter_data_id))+
      labs(x="Reported end year of study",y="Parameter value",linetype="",colour="Study")+
      scale_linetype_manual(values = c("dotted","solid"))+
      theme(legend.position="bottom",
             legend.box="vertical",
            strip.text = element_text(size=12))+
      scale_colour_viridis_d(end=0.9)+
      guides(colour = guide_legend(order = 2),
             linetype = guide_legend(order = 1,byrow=TRUE))

  }

  if(parameter %in% c("Risk factors","Mosquito","Relative","Other")){
    stop("Parameter class not yet coded.")
  }

  return(plot)

}


human_delay <- forest_plot(df,"Human delay")
human_delay
#### ggsave currently not working for me (turns plot black and blurry...)
#ggsave(plot = human_delay,filename="data/marburg/output/FP_human_delay.png")

mutations <- forest_plot(df,"Mutations")
mutations
#### ggsave currently not working for me (turns plot black and blurry...)
#ggsave(plot = mutations,filename="data/marburg/output/FP_mutations.png")


reproduction_number <- forest_plot(df,"Reproduction number")
reproduction_number
#### ggsave currently not working for me (turns plot black and blurry...)
#ggsave(plot = reproduction_number,filename="data/marburg/output/FP_reproduction_number.png")


severity <- forest_plot(df,"Severity")
severity
#### ggsave currently not working for me (turns plot black and blurry...)
#ggsave(plot = severity,filename="data/marburg/output/FP_severity.png")

seroprevalence <- forest_plot(df,"Seroprevalence")
seroprevalence
#### ggsave currently not working for me (turns plot black and blurry...)
#ggsave(plot = seroprevalence,filename="data/marburg/output/FP_seroprevalence.png")



