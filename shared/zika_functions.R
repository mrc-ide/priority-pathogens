# function to produce forest plot for given dataframe - modified from lassa_functions.R

forest_plot <- function(df, ycol = "urefs", label, shape_column = 'parameter_value_type', 
                        color_column, lims, text_size = 12, show_label = FALSE, 
                        custom_colours = NA) {
  
  stopifnot(length(unique(df$parameter_unit[!is.na(df$parameter_unit)])) == 1)#values must have same units
  
  df   <- df %>% mutate(urefs = make.unique(refs)) %>%
    mutate(urefs = factor(urefs, levels = rev(unique(urefs))),
           label_group = case_when(
             is.na(population_location) | population_location == '' ~ population_country,
             (population_location !="" & !is.na(population_location)) & population_country != 'Unspecified' ~ paste0(population_location,'\n' ,' (',population_country,')'),
             !is.na(population_location) & population_country == 'Unspecified' ~ population_location,
             TRUE ~ NA)) %>%
    arrange(desc(parameter_value)) %>% 
    mutate(label_group = factor(label_group, levels=unique(label_group)),
           urefs = factor(urefs, levels = unique(urefs)))
  cats <- length(unique(df[[color_column]]))
  
  jitterer <- position_jitter(seed = 123, width = 0.03)
  
  df <- df %>%
    group_by(.data[[ycol]]) %>%
    mutate(n = n()) 
  
  gg <- ggplot(df) +
    geom_segment(data = df %>% filter(n == 1),
                 aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                     y = .data[[ycol]], yend = .data[[ycol]], color = .data[[color_column]]),
                 linewidth = 4, alpha = 0.65) +
    geom_errorbar(data = df %>% filter(n == 1),
                  aes(x = central, xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                      color = .data[[color_column]], group = .data[[ycol]],
                      y = .data[[ycol]]),
                  width = 0.2, lwd=0.7,  alpha = 1) +#color = "black",
    geom_point(data = df %>% filter(n == 1),
               aes(x = central, y = .data[[ycol]],
                   shape = .data[[shape_column]], fill = .data[[color_column]],
                   color = .data[[color_column]]),
               size = 4, stroke = 1,
               alpha = 1) +
    
    geom_segment(data = df %>% filter(n > 1),
                 aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                     y = .data[[ycol]], yend = .data[[ycol]], color = .data[[color_column]]),
                 position = jitterer,
                 linewidth = 4, alpha = 0.65) +
    geom_errorbar(data = df %>% filter(n > 1),
                  aes(x = central, xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                      color = .data[[color_column]], group = .data[[ycol]],
                      y = .data[[ycol]]),
                  position = jitterer,
                  width = 0.2, lwd=0.5,  alpha = 1) +#color = "black",
    geom_point(data = df %>% filter(n > 1),
               aes(x = central, y = .data[[ycol]], 
                   shape = .data[[shape_column]], fill = .data[[color_column]], 
                   color = .data[[color_column]]),
               position = jitterer,
               size = 4, stroke = 1,
               alpha = 1) 
  
  
  
  if (all(df$parameter_class=="Reproduction number")) {gg <- gg + geom_vline(xintercept = 1, linetype = "dashed", colour = "dark grey")}
  
  if (shape_column == 'parameter_value_type'){
    gg <- gg + 
      scale_shape_manual(name = "Parameter Type",
                         values = c(Mean = 21, Median = 22, Central = 25, "Maximum likelihood" = 23, Unspecified = 24, Other = 8),
                         breaks = c("Mean", "Median", "Unspecified", "Central", "Maximum likelihood", "Other")) 
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
                                    "Travel based" = 23, "Community based" = 8, 'Unspecified' = 4,
                                    "Other" = 24),
                         breaks = c("Hospital based", "Mixed settings", "Population based",
                                    "Travel based","Community based", "Other", 'Unspecified'))
  } else if (shape_column == 'population_group'){
    gg <- gg + 
      scale_shape_manual(name = 'Sample group',
                         values = c("General population" = 21, "Persons under investigation" = 22, "Blood donors" = 25,
                                    "Mixed groups" = 13, 'Pregnant women' = 3, 'Children' = 8,'Household contacts of survivors'=4,
                                    "Other" = 23, 'Unspecified' = 24),
                         breaks = c("General population", "Persons under investigation", "Blood donors",
                                    "Mixed groups", 'Pregnant women', 'Children', 'Household contacts of survivors',
                                    "Other", 'Unspecified'))
  }
  
  if(sum(!is.na(custom_colours)))
  {
    gg <- gg + 
      scale_x_continuous(limits = lims, expand = c(0.05, 0)) +
      scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
      labs(x = label, y = NULL) +
      scale_color_manual(values = custom_colours, labels = function(x) str_wrap(x, width = 18)) +
      scale_fill_manual(values = custom_colours, labels = function(x) str_wrap(x, width = 18)) +
      theme_minimal() + 
      theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
            text = element_text(size = text_size))
  } else {
    gg <- gg + scale_fill_lancet(palette = "lanonc", labels = function(x) str_wrap(x, width = 18)) +
      scale_color_lancet(palette = "lanonc", labels = function(x) str_wrap(x, width = 18)) +
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
