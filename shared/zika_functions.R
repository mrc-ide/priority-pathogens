# function to produce forest plot for given dataframe - modified from lassa_functions.R

forest_plot <- function(df, ycol = "urefs", label, shape_column = 'parameter_value_type', 
                        color_column, lims, text_size = 12, show_label = FALSE, 
                        custom_colours = NA, 
                        facet_by_country = FALSE, facet_by_continent = FALSE, facet_by_poptype = FALSE, facet_by_delay = FALSE,
                        point_size = 3) {
  
  stopifnot(length(unique(df$parameter_unit[!is.na(df$parameter_unit)])) == 1)#values must have same units
  
  df   <- df %>% mutate(urefs = make.unique(refs)) %>%
    mutate(urefs = factor(urefs, levels = rev(unique(urefs))),
           country_brazil = ifelse(population_country == 'Brazil', 'Brazil','Rest of World'),
           population_location = ifelse(population_location == 'Wynwood Neighborhood, Miami-Dade County, Florida','Miami-Dade County, Florida', population_location),
           continent = countrycode(sourcevar = population_country,
                                               origin = "country.name",
                                               destination = "continent"),
           continent = case_when(
             grepl(population_country, 'Multi') ~ "Multi-country",
             population_country == 'France' & population_location == 'French Guiana' ~ "Americas",
             population_location %in% c('Martinique','Guadeloupe','Saint Martin') ~ "Americas",
             population_country == "France (Martinique, Guadeloupe, French Guiana)" ~ "Americas",
             population_country == "Multi-country: France and Netherlands" ~ "Europe",
             population_country == "Micronesia" ~ "Oceania",
             population_country == 'Multi-country: Brazil and Colombia'  ~ "Americas",
             population_country == "Multi-country: Brazil, Colombia, El Salvador" ~ "Americas",
             population_country == "Multi-country: Americas (n = 32)" ~ "Americas",
             population_country == "Singapore" | population_location == 'Singapore' ~ "Asia",
             population_country == 'Brazil' & population_location != 'Brazil' ~ "Brazil (Americas)",
             # population_country == 'French Polynesia' & population_location != 'French Polynesia' ~ "French Polynesia (Oceania)",
             population_country == 'France' & grepl('French Polynesia', population_location) & population_location != 'French Polynesia' ~ "French Polynesia, France (Oceania)",
             population_location == 'French Polynesia' ~ "Oceania",
             population_location == 'Sous-le-vent Islands' ~ "French Polynesia, France (Oceania)",
             population_country == 'Colombia' & population_location != "Colombia" ~ "Colombia (Americas)",
             population_country == 'France' & population_location == 'New Caledonia' ~ "Oceania",
             population_country == 'France' & population_location == 'French Guiana' ~ "Americas",
             population_country == 'Unspecified' ~ "Unspecified",
             TRUE ~ continent
             ),
           continent = factor(continent, levels = c('Brazil (Americas)','French Polynesia, France (Oceania)','Colombia (Americas)', 'Americas','Asia','Europe','Oceania','Africa',"Unspecified")),
           population_country = str_replace(population_country, 'Multi-country: ', ""),
           label_group = case_when(
             is.na(population_location) | population_location == '' ~ population_country,
             is.na(population_country) | population_country=='' ~ population_location,
             (!is.na(population_location) & population_country == 'Unspecified') | 
               ((population_country == 'Brazil' | population_country == "Colombia" | continent == 'French Polynesia, France (Oceania)') & 
                  (facet_by_country == TRUE | facet_by_continent == TRUE)) ~ population_location,
             (population_location != "" & !is.na(population_location)) & 
               (population_country != 'Unspecified') ~ paste0(population_location,'\n' ,' (',population_country,')'),
             TRUE ~ NA),
           label_group = case_when(
             label_group == 'continental United States and Hawaii\n (United States)' ~ 'United States',
             label_group == 'Singapore\n (Singapore)' ~ 'Singapore',
             # label_group == 'Brazil' ~ "Brazil (national)",
             TRUE ~ label_group),
           label_group = str_replace(label_group, ", French Polynesia$", "")
           ) %>%
    arrange(desc(parameter_value)) %>% 
    mutate(label_group = factor(label_group, levels = unique(label_group)),
           urefs = factor(urefs, levels = unique(urefs)))
  cats <- length(unique(df[[color_column]]))
  
  jitterer <- position_jitter(seed = 123, width = 0.025)
  
  df <- df %>%
      group_by(.data[[ycol]]) %>%
      mutate(n = n()) 
  
  sortby <- if(ycol == 'label_group') 'population_country' else 'parameter_value'
  
  gg <- ggplot(df) +
    
    geom_segment(data = df %>% filter(n > 1) %>% arrange(sortby) %>%
                   mutate(label_group = factor(label_group, levels=unique(label_group)),
                          urefs = factor(urefs, levels = unique(urefs))),
                 aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                     y = .data[[ycol]], yend = .data[[ycol]], color = .data[[color_column]]),
                 position = jitterer,
                 linewidth = 4, alpha = 0.6) +
    geom_errorbar(data = df %>% filter(n > 1) %>% arrange(sortby) %>%
                    mutate(label_group = factor(label_group, levels=unique(label_group)),
                           urefs = factor(urefs, levels = unique(urefs))),
                  aes(x = central, xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                      color = .data[[color_column]], group = .data[[ycol]],
                      y = .data[[ycol]]),
                  position = jitterer,
                  width = 0.2, lwd=0.6,  alpha = 1) +#color = "black",
    geom_point(data = df %>% filter(n > 1) %>% arrange(sortby) %>%
                 mutate(label_group = factor(label_group, levels=unique(label_group)),
                        urefs = factor(urefs, levels = unique(urefs))),
               aes(x = central, y = .data[[ycol]],
                   shape = .data[[shape_column]], fill = .data[[color_column]],
                   color = .data[[color_column]]),
               position = jitterer,
               size = point_size, stroke = 1,
               alpha = 1) +

    geom_segment(data = df %>% filter(n == 1) %>% arrange(sortby) %>%
                   mutate(label_group = factor(label_group, levels=unique(label_group)),
                          urefs = factor(urefs, levels = unique(urefs))),
                 aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                     y = .data[[ycol]], yend = .data[[ycol]], color = .data[[color_column]]),
                 linewidth = 4, alpha = 0.6) +
    geom_errorbar(data = df %>% filter(n == 1) %>% arrange(sortby) %>%
                    mutate(label_group = factor(label_group, levels=unique(label_group)),
                           urefs = factor(urefs, levels = unique(urefs))),
                  aes(x = central, xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                      color = .data[[color_column]], group = .data[[ycol]],
                      y = .data[[ycol]]),
                  width = 0.2, lwd=0.6,  alpha = 1) +#color = "black",
    geom_point(data = df %>% filter(n == 1) %>% arrange(sortby) %>%
                 mutate(label_group = factor(label_group, levels=unique(label_group)),
                        urefs = factor(urefs, levels = unique(urefs))),
               aes(x = central, y = .data[[ycol]],
                   shape = .data[[shape_column]], fill = .data[[color_column]],
                   color = .data[[color_column]]),
               size = point_size, stroke = 1,
               alpha = 1) 
  
  if(facet_by_country){
    gg <- gg +
      ggforce::facet_col(vars(country_brazil), scales = 'free_y', space = 'free')  #.~country_brazil ncol = 1, 
  } else if(facet_by_continent){
    gg <- gg + 
      ggforce::facet_col(vars(continent), scales = 'free_y', space = 'free')
  } else if(facet_by_poptype){
    gg <- gg + 
      ggforce::facet_col(vars(population_sample_type), scales = 'free_y', space = 'free')
  } else if (facet_by_delay){
    gg <- gg +
      ggforce::facet_col(vars(parameter_type), scales = 'free_y', space = 'free')
  }
  
  
  
  if (all(df$parameter_class=="Reproduction number")) {gg <- gg + geom_vline(xintercept = 1, linetype = "dashed", colour = "dark grey")}
  
  if (shape_column == 'parameter_value_type'){
    gg <- gg + 
      scale_shape_manual(name = "Parameter Type",
                         values = c(Mean = 21, Median = 22, Central = 25, "Maximum likelihood" = 23, Unspecified = 24, Other = 8),
                         breaks = c("Mean", "Median", "Unspecified", "Central", "Maximum likelihood", "Other"), 
                         labels = function(x) str_wrap(x, width = 18)) 
  } else if (shape_column == 'case_definition'){
    gg <- gg + 
      scale_shape_manual(name = 'Case definition',
                         values = c("Confirmed" = 21, "Suspected" = 22, "Confirmed or suspected" = 25,
                                    'Other' = 13,
                                    "Unspecified" = 24),
                         breaks = c("Confirmed", "Suspected", "Confirmed or suspected", "Other","Unspecified"), 
                         labels = function(x) str_wrap(x, width = 18))
  } else if (shape_column == 'population_sample_type'){
    gg <- gg + 
      scale_shape_manual(name = 'Sample type',
                         values = c("Hospital based" = 21, "Mixed settings" = 22, "Population based" = 25,
                                    "Travel based" = 23, "Community based" = 7, 'Unspecified' = 4,
                                    "Other" = 24),
                         breaks = c("Hospital based", "Mixed settings", "Population based",
                                    "Travel based","Community based", "Other", 'Unspecified'), 
                         labels = function(x) str_wrap(x, width = 18))
  } else if (shape_column == 'population_group'){
    gg <- gg + 
      scale_shape_manual(name = 'Sample group',
                         values = c("General population" = 21, "Persons under investigation" = 22, "Blood donors" = 25,
                                    "Mixed groups" = 13, 'Pregnant women' = 7, 'Children' = 8,'Household contacts of survivors'=4,
                                    "Other" = 23, 'Unspecified' = 24),
                         breaks = c("General population", "Persons under investigation", "Blood donors",
                                    "Mixed groups", 'Pregnant women', 'Children', 'Household contacts of survivors',
                                    "Other", 'Unspecified'), 
                         labels = function(x) str_wrap(x, width = 18))
  }
  
  lancetcols <- c('#00468BFF', '#ED0000FF','#42B540FF','#0099B4FF', '#925E9FFF','#AD002AFF','#FDAF91FF','#ADB6B6FF','#1B1919FF')
  
  if(sum(!is.na(custom_colours))) {
      gg <- gg + 
        scale_color_manual(values = custom_colours, labels = function(x) str_wrap(x, width = 18)) +
        scale_fill_manual(values = custom_colours, labels = function(x) str_wrap(x, width = 18)) 
  } else {
    if(color_column == 'population_sample_type'){
      gg <- gg + 
        scale_color_manual(values = c("Population based" = lancetcols[1], 'Hospital based' =  lancetcols[2] ,
                                      "Community based"  = lancetcols[3], 'Travel based' =    lancetcols[4] ,
                                      'Unspecified'      = lancetcols[8], 'Contact based' =   lancetcols[6] ,
                                      'Mixed settings'   = lancetcols[7], 'Other' =           lancetcols[5] ,
                                      'School based'     = lancetcols[9], 'Household based' = "#DF8F44"), 
                           labels = function(x) str_wrap(x, width = 18)) +
        scale_fill_manual(values = c("Population based" = lancetcols[1], 'Hospital based' =   lancetcols[2] ,
                                     "Community based"  = lancetcols[3], 'Travel based' =     lancetcols[4] ,
                                     'Unspecified'      = lancetcols[8], 'Contact based' =    lancetcols[6] ,
                                     'Mixed settings'   = lancetcols[7], 'Other' =            lancetcols[5] ,
                                     'School based'     = lancetcols[9], 'Household based' =  "#DF8F44"), 
                          labels = function(x) str_wrap(x, width = 18)) 
    } else if(color_column == 'parameter_type'){
      gg <- gg + 
        scale_color_manual(values = c("Serial interval" = lancetcols[5], 'Symptom onset to admission' =  lancetcols[6] ,
                                      "Symptom onset to recovery"  = lancetcols[7], "Latent period" = "#DF8F44",
                                      "Admission to care to discharge from care" = lancetcols[4]), 
                           labels = function(x) str_wrap(x, width = 18)) +
        scale_fill_manual(values = c("Serial interval" = lancetcols[5], 'Symptom onset to admission' =  lancetcols[6] ,
                                     "Symptom onset to recovery"  = lancetcols[7], "Latent period" = "#DF8F44",
                                     "Admission to care to discharge from care" = lancetcols[4]), 
                          labels = function(x) str_wrap(x, width = 18)) 
    } else if(color_column == 'population_group'){
      gg <- gg + 
        scale_color_manual(values = c("General population" = lancetcols[1], 'Persons under investigation' =  lancetcols[2] ,
                                      "Other"  = lancetcols[3], 'Children' =    lancetcols[4] ,
                                      'Pregnant women'      = lancetcols[5], 'Mixed groups' =   lancetcols[6] ,
                                      'Blood donors'   = lancetcols[7], 'Unspecified' =  lancetcols[8] ,
                                      'Household contacts of survivors' = "#DF8F44"), 
                           labels = function(x) str_wrap(x, width = 18)) +
        scale_fill_manual(values = c("General population" = lancetcols[1], 'Persons under investigation' =  lancetcols[2] ,
                                     "Other"  = lancetcols[3], 'Children' =    lancetcols[4] ,
                                     'Pregnant women'      = lancetcols[5], 'Mixed groups' =   lancetcols[6] ,
                                     'Blood donors'   = lancetcols[7], 'Unspecified' =  lancetcols[8] ,
                                     'Household contacts of survivors' = "#DF8F44"), 
                          labels = function(x) str_wrap(x, width = 18)) 
    } else {gg <- gg + 
      scale_fill_lancet(palette = "lanonc", labels = function(x) str_wrap(x, width = 18)) +
      scale_color_lancet(palette = "lanonc", labels = function(x) str_wrap(x, width = 18)) 
    }
  }
  
  gg <- gg +
    scale_x_continuous(limits = lims, expand = c(0.01, 0)) +
    labs(x = label, y = NULL) +
    theme_minimal() + 
    theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
          text = element_text(size = text_size))#,
          # legend.key.size = unit(5, 'cm')) 
    
  if(ycol == 'urefs'){
    gg <- gg + 
      scale_y_discrete(labels = setNames(df$refs, df$urefs))
  } else {
    gg <- gg + 
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) 
  }
  
  # if (cats == 1) {
  #   gg <- gg + guides(fill = "none", color = FALSE, shape = guide_legend(title = NULL,order = 1))
  # } else {
    gg <- gg + guides(fill = "none", color = guide_legend(title = NULL,order = 1), shape = guide_legend(title = NULL,order = 2))#}
  
  if(show_label)
    gg <- gg + geom_text_repel(aes(x = coalesce(parameter_value), y = urefs, label = population_country_ISO), nudge_y = 0.5, segment.color = "grey50" ) 
  #gg <- gg + geom_text_repel(aes(x = coalesce(parameter_uncertainty_upper_value,parameter_upper_bound,parameter_value), y = urefs, label = population_country_ISO), nudge_x = 1.5, segment.color = "grey90" ) 
  
  return(gg)
}
