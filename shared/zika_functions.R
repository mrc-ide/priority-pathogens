# function to produce forest plot for given dataframe - modified from lassa_functions.R

forest_plot <- function(df, ycol = "urefs", label, shape_column = 'parameter_value_type', 
                        color_column, lims, text_size = 11, show_label = FALSE, custom_colours = NA) {
  
  stopifnot(length(unique(df$parameter_unit[!is.na(df$parameter_unit)])) == 1)#values must have same units
  
  df   <- df %>% mutate(urefs = make.unique(refs)) %>%
    mutate(urefs = factor(urefs, levels = rev(unique(urefs))),
           label_group = case_when(
             is.na(population_location)~ population_country,
             !is.na(population_location) & population_country != 'Unspecified' ~ paste0(population_location,'\n' ,' (',population_country,')'),
             !is.na(population_location) & population_country == 'Unspecified' ~ population_location,
             TRUE ~ NA))
  cats <- length(unique(df[[color_column]]))
  
  
  gg <- ggplot(df) +
    geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                     y = .data[[ycol]], yend = .data[[ycol]], color = .data[[color_column]]),
                 linewidth = 3, alpha = 0.65) +
    geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                      color = .data[[color_column]],
                      y = .data[[ycol]]),
                  width = 0.15, lwd=0.5, color = "black", alpha = 1) +
    geom_point(aes(x = central, y = .data[[ycol]], 
                   shape = .data[[shape_column]], fill = .data[[color_column]]),
               size = 3, stroke = 1,
               color = "black", alpha = 1)
  
  
  
  if (all(df$parameter_class=="Reproduction number")) {gg <- gg + geom_vline(xintercept = 1, linetype = "dashed", colour = "dark grey")}
  
  if (shape_column == 'parameter_value_type'){
    gg <- gg + 
      scale_shape_manual(name = "Parameter Type",
                         values = c(Mean = 21, Median = 22, Central = 25, Unspecified = 24, Other = 23),
                         breaks = c("Mean", "Median", "Unspecified", "Central", "Other")) 
  } else if (shape_column == 'case_definition'){
    gg <- gg + 
      scale_shape_manual(name = 'Case definition',
                         values = c("Confirmed" = 21, "Suspected" = 22, "Confirmed;Suspected" = 25,
                                    "Unspecified" = 24),
                         breaks = c("Confirmed", "Suspected", "Confirmed;Suspected", "Unspecified"))
  } else if (shape_column == 'population_sample_type'){
    gg <- gg + 
      scale_shape_manual(name = 'Sample type',
                         values = c("Hospital based" = 21, "Mixed settings" = 22, "Population based" = 25,
                                    "Travel based" = 23,
                                    "Other" = 24),
                         breaks = c("Hospital based", "Mixed settings", "Population based",
                                    "Travel based", "Other"))
  }
  
  if(sum(!is.na(custom_colours)))
  {
    gg <- gg + 
      scale_x_continuous(limits = lims, expand = c(0.05, 0)) +
      scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
      labs(x = label, y = NULL) +
      scale_color_manual(values = custom_colours) +
      scale_fill_manual(values = custom_colours) +
      theme_minimal() + 
      theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
            text = element_text(size = text_size))
  } else {
    gg <- gg + scale_fill_lancet(palette = "lanonc") + scale_color_lancet(palette = "lanonc") +
      scale_x_continuous(limits = lims, expand = c(0.05, 0)) +
      scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
      labs(x = label, y = NULL) +
      theme_minimal() + 
      theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
            text = element_text(size = text_size))  
  }
  
  if (cats == 1) {
    gg <- gg + guides(fill = "none", color = FALSE, shape = guide_legend(title = NULL,order = 1))
  } else {
    gg <- gg + guides(fill = "none", color = guide_legend(title = NULL,order = 1), shape = guide_legend(title = NULL,order = 2))}
  
  if(show_label)
    gg <- gg + geom_text_repel(aes(x = coalesce(parameter_value), y = urefs, label = population_country_ISO), nudge_y = 0.5, segment.color = "grey50" ) 
  #gg <- gg + geom_text_repel(aes(x = coalesce(parameter_uncertainty_upper_value,parameter_upper_bound,parameter_value), y = urefs, label = population_country_ISO), nudge_x = 1.5, segment.color = "grey90" ) 
  
  return(gg)
}
