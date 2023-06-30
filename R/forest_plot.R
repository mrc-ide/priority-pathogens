## function to produce forest plot
library(ggplot2)
library(tidyverse)
library(patchwork)
library(cowplot)

# fatality rates 
forest_plot_fr <- function(df,outbreak_naive=FALSE) {
  
  parameter <- "Severity"
  
  if(outbreak_naive) {
    df <- df %>% dplyr::filter(keep_record == 1)
  }
  
  df_cfr <- df %>% filter(parameter_class == parameter) %>%
    mutate(parameter_value = as.numeric(parameter_value)) %>%
    group_by(parameter_type) %>%
    dplyr::arrange(article_label)
  
  df_cfr$pooled <- (sum(df_cfr$cfr_ifr_numerator, na.rm = TRUE)/sum(df_cfr$cfr_ifr_denominator, na.rm = TRUE))*100
  p <- df_cfr$pooled[1]/100
  n <- sum(df_cfr$cfr_ifr_denominator, na.rm = TRUE)
  df_cfr$pooled_low <- (p - 1.96*(sqrt((p * (1-p))/n)))*100
  df_cfr$pooled_upp <- (p + 1.96*(sqrt((p * (1-p))/n)))*100
  
  df_plot <- df_cfr %>% 
    dplyr::filter(is.na(parameter_value) == FALSE) %>%
    #dplyr::select(c(article_id:distribution_par2_uncertainty, covidence_id:pooled_upp)) %>%
    dplyr::arrange((cfr_ifr_method))
  
  ## ensuring that each entry is on it's own line
  df_plot$article_label_unique <- make.unique(df_plot$article_label)
  
  ## sorry team this is also hacky but I'm gonna scream if these points don't go where we want them 
  df_plot <- df_plot %>% mutate(order_num = seq(1,dim(df_plot)[1],1))
  
  if(outbreak_naive) {
    df_plot2 <- df_plot %>% 
      dplyr::arrange(outbreak_start_year) %>%
      dplyr::mutate(order_num = row_number(),
                    p = parameter_value/100,
                    lower_ci = (p  - 1.96*(sqrt((p * (1-p))/cfr_ifr_denominator)))*100,
                    upper_ci = (p  + 1.96*(sqrt((p * (1-p))/cfr_ifr_denominator)))*100,
                    outbreak_year_cnt = as.character(paste0(outbreak_year_cnt, " [n =", cfr_ifr_denominator,"]")))
    
    plot <- ggplot(df_plot2, aes(x=parameter_value, 
                                y=reorder(article_label_unique,-order_num), 
                                col = article_label)) + 
      theme_bw() + geom_point(size = 3) +
      scale_y_discrete(labels=setNames(df_plot2$outbreak_year_cnt, 
                                       df_plot2$article_label_unique)) +
      #geom_rect(xmin=unique(df_cfr$pooled_low),xmax=unique(df_cfr$pooled_upp),
      #          ymin=-Inf,ymax=Inf,alpha=0.02,col=NA,fill="grey") +
      geom_segment(aes(y = article_label_unique, yend = article_label_unique,
                        x = lower_ci, xend = upper_ci,
                        group=parameter_data_id), 
                   lwd=3,
                   alpha = 0.4) + 
      labs(x="Case fatality ratio (%)",y="",#y="Study",
           linetype="",colour="",fill="") + 
      theme(legend.position="right",
            legend.text = element_text(size=12),
            strip.text = element_text(size=20)) +
      # xlim(c(0, 100)) +
      scale_color_brewer(palette = 'Dark2')+
      guides(colour = guide_legend(order=1,ncol=1),
             linetype = guide_legend(order=2,ncol=1))+
      #geom_vline(xintercept = unique(df_cfr$pooled),linetype="dashed") +
      #scale_linetype_manual(values = c("solid"),labels = function(x) str_wrap(x, width = 5))+
      #scale_fill_manual(values="grey") + 
      geom_vline(xintercept = c(0, 100), linetype = "dotted") +
      scale_x_continuous(breaks = seq(-20, 120, by = 20))
  } else {
    df_plot <- df_plot %>% 
      dplyr::arrange(article_label_unique) %>%
      dplyr::mutate(order_num = row_number())
    
    plot <- ggplot(df_plot, aes(x=parameter_value, y=reorder(article_label_unique,-order_num), 
                                col = cfr_ifr_method)) + 
      theme_bw() + geom_point(size = 3) +
      scale_y_discrete(labels=setNames(df_plot$article_label, df_plot$article_label_unique)) +
      geom_errorbar(aes(y=article_label_unique,
                        xmin=parameter_uncertainty_lower_value,
                        xmax=parameter_uncertainty_upper_value,
                        group=parameter_data_id,
                        linetype="Uncertainty interval"),
                    position=position_dodge(width=0.5),
                    width=0.25,
                    lwd=1) + 
      geom_segment(aes(y=article_label_unique, 
                       yend = article_label_unique,
                       x=parameter_lower_bound, 
                       xend=parameter_upper_bound,
                       group=parameter_data_id),
                    lwd=5,
                    alpha = 0.4) +
      labs(x="Case fatality ratio (%)",y="",#y="Study",
           linetype="",colour="",fill="") + 
      theme(#legend.position="right",
            legend.text = element_text(size=12),
            strip.text = element_text(size=20)) +
      # xlim(c(0, 100)) +
      scale_colour_manual(values=c("#D95F02","#7570B3"))+
      # scale_color_brewer(palette = 'Dark2') +
      # scale_colour_viridis_d(option="viridis",begin=0.2,end=0.8)+
      guides(colour = guide_legend(order=1,ncol=1),
             linetype = guide_legend(order=2,ncol=1))+
      #geom_vline(xintercept = unique(df_cfr$pooled),linetype="dashed")+
      #geom_rect(xmin=unique(df_cfr$pooled_low),xmax=unique(df_cfr$pooled_upp),
      #          ymin=-Inf,ymax=Inf,alpha=0.05,col=NA,fill="grey"#,aes(fill="CFR - pooled 95% CI")
      #)+
      scale_linetype_manual(values = c("solid"),labels = function(x) str_wrap(x, width = 5))+
      scale_fill_manual(values="grey") + 
      geom_vline(xintercept = c(0, 100), linetype = "dotted") + 
      scale_x_continuous(breaks = seq(-20, 120, by = 20)) + 
      xlim(c(-20, 120))
  }
  
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
  
  df_plot <- df_plot %>% mutate(parameter_type_short = 
                                  ifelse(parameter_type=="Reproduction number (Basic R0)",
                                                             "Basic (R0)",
                                                             ifelse(parameter_type=="Reproduction number (Effective, Re)","Effective (Re)",NA)))
  
  plot <-
    ggplot(df_plot, aes(x = parameter_value, y = article_label_unique, col = parameter_type_short))+
    theme_bw()+
    # scale_x_continuous(breaks = seq(0, 2, by = 0.2))+
    # geom_errorbar(aes(y=article_label,xmin=parameter_lower_bound,xmax=parameter_upper_bound,
    #                   group=parameter_data_id,
    #                   linetype="Parameter range"),
    #               position=position_dodge(width=0.5),
    #               width=0.25)+
    geom_errorbar(aes(y=article_label,
                      xmin=parameter_uncertainty_lower_value,xmax=parameter_uncertainty_upper_value,
                      group=parameter_data_id,
                      linetype="Uncertainty interval"),
                  position=position_dodge(width=0.5),
                  width = 0.2,
                  lwd=1)+
    geom_point(aes(x=parameter_value,y=article_label,group=parameter_data_id),size = 3)+
    # geom_vline(aes(xintercept=median,col="Sample median"),linetype="dashed", colour = "grey") +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "dark grey") +
    labs(x="Reproduction number",y="",#y="Study",
         linetype="",colour="")+
    scale_linetype_manual(values = c("solid"),labels = function(x) str_wrap(x, width = 5))+
    #scale_colour_discrete(labels = c("Basic R0", "Effective Re")) +
    scale_colour_manual(values=c("#D95F02","#7570B3"))+
    theme(#legend.position="bottom",
          legend.text = element_text(size=12),
          strip.text = element_text(size=20)) + xlim(c(0,2)) +
    guides(colour = guide_legend(order=1,ncol=1),
           linetype = guide_legend(order=2,ncol=1))
  
  return(plot)
  
}

## human delays
forest_plot_delay <- function(df){
  
  parameter <- "Human delay"
  
  df_delay <- df %>% 
    dplyr::filter(parameter_class == parameter) %>%
    dplyr::mutate(parameter_value = as.numeric(parameter_value)) %>%
    mutate(parameter_type_short = ifelse(parameter_type=="Human delay - generation time","Generation time",
                                         ifelse(parameter_type=="Human delay - incubation period",
                                                "Incubation period",
                                                ifelse(parameter_type=="Human delay - time in care","Time in care",
                                                       ifelse(parameter_type=="Human delay - time symptom to careseeking","Time symptom to careseeking",
                                                              ifelse(parameter_type=="Human delay - time symptom to outcome" & riskfactor_outcome=="Death","Time symptom to outcome (Death)",
                                                                     ifelse(parameter_type=="Human delay - time symptom to outcome" & riskfactor_outcome=="Other","Time symptom to outcome (Other)",NA))))))) %>%
    dplyr::group_by(parameter_type_short) %>%
    dplyr::arrange(first_author_first_name) 
     
    #dplyr::filter(parameter_type != "Human delay - generation time")
  
  df_plot <- df_delay %>% 
    dplyr::filter(parameter_class == parameter) %>%
    dplyr::mutate(parameter_value = as.numeric(parameter_value)) %>%
    dplyr::group_by(parameter_type_short) %>% ### median function not behaving in ggplot so going with this even with grouping
    mutate(median = median(parameter_value,na.rm=TRUE)) %>%
    dplyr::arrange(desc(parameter_type_short), desc(parameter_value), desc(article_label))
  
  df_plot$article_label_unique <- make.unique(df_plot$article_label)
  df_plot$article_label_unique <- factor(df_plot$article_label_unique, levels = df_plot$article_label_unique)
    
  plot <-
    ggplot(df_plot, aes(col = parameter_type_short)) + 
    theme_bw() + 
    geom_point(aes(x=parameter_value, y=article_label_unique, shape = parameter_value_type,
                   ),
               size = 3.5) +
    scale_y_discrete(labels = setNames(df_plot$article_label, df_plot$article_label_unique)) +
    scale_x_continuous(breaks = c(seq(0, 60, by = 10))) +
    # facet_wrap(parameter_type ~ ., scales = "free",  strip.position = "top", ncol = 1) +
    geom_segment(aes( y=article_label_unique, yend = article_label_unique,
                      x=parameter_lower_bound, xend=parameter_upper_bound,
                      group=parameter_data_id, 
                      # linetype="Parameter range"
                      ),
                  # position=position_dodge(width=0.5),
                  # width=0.4,
                  lwd=5,
                  alpha = 0.4) +
    geom_errorbar(aes(y=article_label_unique,
                      xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                      group=parameter_data_id,
                      # linetype="Uncertainty interval"
                      ),
                  # position=position_dodge(width=0.5),
                  width = 0.4,
                  lwd=1) +
    labs(x="Delay (days)", 
         y="",#y="Study (First author surname and publication year)",
         linetype="", 
         colour="",
         shape = '',
         caption = '*Solid transparent rectangles refer to parameter ranges while the error bars are uncertainty intervals.') + 
    # scale_linetype_manual(values = c("blank",'solid'),
    #                       labels = function(x) str_wrap(x, width = 5)) +
    scale_color_brewer(palette = 'Dark2',#end=0.9,
                           labels = function(x) str_wrap(x, width = 18))+
    scale_shape_manual(values = c(16,15,17,18), 
                       labels = c('Mean','Median','Std Dev', 'Other'),
                       na.translate = F) +
    # scale_colour_discrete(labels = c("Incubation period",
    #                                  "Time in care",
    #                                  "Symptom to careseeking",
    #                                  "Symptom to outcome")) +
    theme(#legend.position="bottom",
          legend.text = element_text(size=12),
          strip.text = element_text(size=20)) + #xlim(c(0,56)) +
    guides(colour = guide_legend(order=1,ncol =1),
           linetype = guide_legend(order=2,ncol=1)) 


  return(plot)
  
}

## mutations -- NOTE: come back and check the index notation on these values 
forest_plot_mutations <- function(df){
  
  parameter <- "Mutations"
  
  df_mutations <- df %>% 
    dplyr::filter(parameter_class == parameter) %>%
    dplyr::mutate(parameter_value = as.numeric(parameter_value)) %>%
    dplyr::group_by(parameter_type) 
  
  df_plot <- df_mutations %>% 
    dplyr::filter(parameter_class == parameter) %>%
    dplyr::mutate(parameter_value = as.numeric(parameter_value)) %>%
    dplyr::group_by(parameter_type) #%>% ### median function not behaving in ggplot so going with this even with grouping
    # mutate(median = median(parameter_value,na.rm=TRUE))
  
  df_plot$article_label_unique <- make.unique(df_plot$article_label)
  df_plot <- df_plot %>%
    dplyr::mutate(gene = ifelse(is.na(genome_site)==TRUE, "Whole genome", genome_site))  %>%
    dplyr::arrange(gene, desc(parameter_value))
  df_plot$article_label_unique <- factor(df_plot$article_label_unique, levels = df_plot$article_label_unique)
  df_plot <- df_plot %>%
    dplyr::mutate(parameter_value = if_else(parameter_value > 1e-02, parameter_value * 1e-04, parameter_value),
                  parameter_uncertainty_single_value = if_else(parameter_uncertainty_single_value > 1e-02, parameter_uncertainty_single_value * 1e-04, parameter_uncertainty_single_value))

  
  # df_plot <- df_plot %>%
  #   dplyr::mutate(parameter_type = ifelse(article_label == "Suzuki 1997", "Mutations - substitution rate",
  #                                         parameter_type))
  
  plot <-
    ggplot(df_plot, aes(x=(parameter_value * 1e04), y=article_label_unique, 
                               col = gene)) + 
    theme_bw() + geom_point(size = 3) +
    geom_vline(xintercept = 0, color = 'black', linetype = 'dashed') +
    # facet_grid(gene ~ ., scales = "free_y", space = "free") +
    scale_y_discrete(labels=setNames(df_plot$article_label, df_plot$article_label_unique)) +
    geom_segment(aes(y=article_label_unique, yend = article_label_unique,
                      x=if_else((parameter_value - parameter_uncertainty_single_value)* 1e04 < 0, 0,
                                   (parameter_value - parameter_uncertainty_single_value)* 1e04),
                      xend=(parameter_value + parameter_uncertainty_single_value)* 1e04,
                      group=parameter_data_id),
                      # linetype="Value \u00B1 standard error *"), 
                 lwd=5,
                 alpha = 0.4) +
    geom_errorbar(aes(y=article_label_unique,
                      xmin=parameter_uncertainty_lower_value* 1e04,
                      xmax=parameter_uncertainty_upper_value* 1e04,
                      group=parameter_data_id),
                      # linetype="Uncertainty interval"),
                  width = 0.3,
                  lwd=1) +
    labs(x=expression(Molecular~evolutionary~rate~(substitution/site/year ~10^{-4})),
         y="",#y="Study",
         linetype="",colour="",
         caption = '*Solid transparent lines are calculated as the parameter value \u00B1 standard error. Error bars refer to uncertainty intervals.') + 
    # scale_linetype_manual(values = c("dotted","solid"),labels = function(x) str_wrap(x, width = 18)) + #+ scale_x_log10()
    #geom_vline(xintercept = 0, linetype = "dotted", colour = "dark grey") +
    theme(legend.text = element_text(size=12),
          strip.text = element_text(size=20)) + xlim(c(0,10)) +
    scale_color_brewer(palette = 'Dark2',labels = function(x) str_wrap(x, width = 10)) +
    guides(colour = guide_legend(order=1,ncol=1)   )
  
  return(plot)

}
