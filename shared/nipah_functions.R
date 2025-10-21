#function to tidy-up all dataframes

data_curation <- function(articles, outbreaks, models, parameters, plotting,switch_first_surname=FALSE) {

  articles   <- articles %>%
    mutate(refs = paste(first_author_surname," (",year_publication,")",sep="")) %>% #define references use what we have from epireview function to be consistent
    group_by(refs) %>% mutate(counter = row_number()) %>% ungroup() %>% #distinguish same-author-same-year references
    mutate(new_refs = ifelse(refs %in% refs[duplicated(refs)], paste0(sub("\\)$", "", refs),letters[counter],")"), refs)) %>%
    select(-counter,-refs) %>%
    rename(refs = new_refs) |>
    mutate(refs = str_to_title(refs))

  if(dim(outbreaks)[1]>0)
  {
    outbreaks  <- outbreaks %>%
      mutate(refs = articles$refs[match(covidence_id, articles$covidence_id)])
  }

  models     <- models %>%
    mutate(refs = articles$refs[match(covidence_id, articles$covidence_id)])

  parameters <- parameters %>%
    mutate(refs = articles$refs[match(covidence_id, articles$covidence_id)]) %>%
    filter(!parameter_from_figure)

  param4plot <- parameters %>%
    mutate(across(
      c(parameter_value, parameter_lower_bound, parameter_upper_bound,
        parameter_uncertainty_lower_value, parameter_uncertainty_upper_value,
        parameter_uncertainty_single_value),
      ~ ifelse(inverse_param, 1 / ., .) * 10^exponent * ifelse(parameter_unit %in% "Weeks", 7, 1))
    ) |>
    mutate(across(
      c(parameter_2_value, parameter_2_lower_bound, parameter_2_upper_bound,
        parameter_2_sample_paired_lower, parameter_2_sample_paired_upper,
        parameter_2_uncertainty_upper_value, parameter_2_uncertainty_lower_value,
        parameter_2_uncertainty_single_value),
      ~ ifelse(inverse_param, 1 / ., .) * 10^exponent * ifelse(parameter_unit %in% "Weeks", 7, 1))
    ) %>%
    mutate(parameter_unit = ifelse(parameter_unit %in% "Weeks", "Days", parameter_unit)) %>%
    mutate(no_unc = is.na(parameter_uncertainty_lower_value) & is.na(parameter_uncertainty_upper_value), #store uncertainty in pu_lower and pu_upper
           custom_se = case_when(str_detect(str_to_lower(parameter_2_value_type),"standard deviation") & no_unc & !is.na(population_sample_size) ~ parameter_2_value/sqrt(population_sample_size),
                                 TRUE ~ NA),
           parameter_uncertainty_lower_value = case_when(
             str_detect(str_to_lower(parameter_uncertainty_single_type),"standard error") & no_unc ~ parameter_value-parameter_uncertainty_single_value,
             !is.na(custom_se) & no_unc ~ parameter_value-custom_se,
             str_detect(str_to_lower(distribution_type),"gamma") & no_unc ~ qgamma(0.05, shape = (distribution_par1_value/distribution_par2_value)^2, rate = distribution_par1_value/distribution_par2_value^2),
             TRUE ~ parameter_uncertainty_lower_value),
           parameter_uncertainty_upper_value = case_when(
             str_detect(str_to_lower(parameter_uncertainty_single_type),"standard error") & no_unc ~ parameter_value+parameter_uncertainty_single_value,
             !is.na(custom_se) & no_unc ~ parameter_value+custom_se,
             str_detect(str_to_lower(distribution_type),"gamma") & no_unc ~ qgamma(0.95, shape = (distribution_par1_value/distribution_par2_value)^2, rate = distribution_par1_value/distribution_par2_value^2),
             TRUE ~ parameter_uncertainty_upper_value)) %>%
    mutate(central = coalesce(parameter_value,
                              100*cfr_ifr_numerator/cfr_ifr_denominator,
                              0.5*(parameter_lower_bound+parameter_upper_bound))) |>
    select(-c(no_unc))

  if (plotting) {
    parameters <- param4plot
  } else {
    check_param_id <- (parameters$parameter_data_id == param4plot$parameter_data_id )    # check that parameter data ids didn't get scrambled
    if(sum(check_param_id)==dim(parameters)[1])
    {
      parameters$central <- param4plot$central
    } else {
      errorCondition('parameters not in right order to match')
    }
  }

  if(dim(outbreaks)[1]>0)
  {
    outbreaks  <- outbreaks  %>% mutate(outbreak_location  = str_replace_all(outbreak_location, "\xe9" , "é"))
  }

  parameters <- parameters %>% mutate(parameter_type     = str_replace_all(parameter_type, "\x96" , "–"),
                                      population_country = str_replace_all(population_country, c("昼㸴" = "ô", "�" = "ô")))

  if(switch_first_surname)   # this is due to legacy access database issue
  {
    articles <- articles %>% rename(first_author_first_name=first_author_surname,first_author_surname=first_author_first_name)
  }

  return(list(articles = articles, outbreaks = outbreaks,
              models = models, parameters = parameters))
}


curation <- function(articles, outbreaks, models, parameters, plotting) {
  #call data_curation function (which at some stage will move to epireview) but keep curation to be backward competible
  df <- data_curation(articles,outbreaks,models,parameters,plotting)

  return(list(articles = df$articles, outbreaks = df$outbreaks,
              models = df$models, parameters = df$parameters))
}

# function to produce forest plot for given dataframe

forest_plot <- function(df, label, color_column, lims, text_size = 11,
                        show_label = FALSE, custom_colours = NA,
                        show.legend=NA) {
  stopifnot(length(unique(df$parameter_unit[!is.na(df$parameter_unit)])) == 1)#values must have same units

  df   <- df %>% mutate(urefs = make.unique(refs)) %>%
    mutate(urefs = factor(urefs, levels = rev(unique(urefs))))
  cats <- length(unique(df[[color_column]]))

  gg <- ggplot(df) +
    geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                     y = urefs, yend = urefs, color = .data[[color_column]]),
                 size = 3, alpha = 0.65, show.legend = show.legend) +
    geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                      y = urefs),
                  width = 0.15, lwd=0.5, color = "black", alpha = 1) +
    geom_errorbar(aes(xmin=parameter_2_lower_bound, xmax=parameter_2_upper_bound,
                      y = urefs),
                  width = 0.15, lwd=0.5, color = "black", alpha = 1, linetype="dashed") +
    geom_point(aes(x = parameter_value, y = urefs,
                   shape = parameter_value_type, fill = .data[[color_column]]),
               size = 3, stroke = 1,
               color = "black", alpha = 1)

  if (all(df$parameter_class=="Reproduction number")) {gg <- gg + geom_vline(xintercept = 1, linetype = "dashed", colour = "dark grey")}

  if(sum(!is.na(custom_colours)))
  {
    gg <- gg +
      scale_shape_manual(name = "Parameter Type",
                         values = c(Mean = 21, Median = 22, Unspecified = 24,
                                    Other = 23, `Central - unspecified`=25),
                         breaks = c("Mean", "Median", "Unspecified", "Other",
                                    "Central - unspecified")) +
      scale_x_continuous(limits = lims, expand = c(0, 0)) +
      scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
      labs(x = label, y = NULL) +
      scale_color_manual(values = custom_colours) +
      scale_fill_manual(values = custom_colours) +
      theme_minimal() +
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
            text = element_text(size = text_size))
  } else {
    gg <- gg + scale_fill_lancet(palette = "lanonc") + scale_color_lancet(palette = "lanonc") +
      scale_shape_manual(name = "Parameter Type",
                         values = c(Mean = 21, Median = 22, Unspecified = 24,
                                    Other = 23, `Central - unspecified`=25),
                         breaks = c("Mean", "Median", "Unspecified", "Other",
                                    "Central - unspecified")) +
      scale_x_continuous(limits = lims, expand = c(0, 0)) +
      scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
      labs(x = label, y = NULL) +
      theme_minimal() +
      theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
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

# function to produce map for given shapefiles and dataframes

map_generic <- function(l0, l1, df, f, n, range_mp, summ_dups,
                        long_lim, lat_lim, col_lim, color_opt, title) {

  stopifnot(length(unique(df$parameter_unit[!is.na(df$parameter_unit)])) == 1)#values must have same units

  country_list <- unique(l0$COUNTRY)

  df           <- df %>% separate_longer_delim(population_country, delim = ",") %>% mutate(population_country = str_trim(population_country, side = "left"))#dataframe with expanded list of countries
  regional_dat <- unique(df$population_country[!is.na(df$population_location)])#countries with regional data
  regional_dat <- intersect(country_list,regional_dat)#only countries in country_list are plotted
  country_dat  <- country_list[!(country_list %in% regional_dat)]#countries with national data only

  shp_regional <- l1 %>% filter(COUNTRY %in% regional_dat)
  shp_country  <- l0 %>% filter(COUNTRY %in% country_dat) %>% mutate(REG_CODE = str_replace_all(COUNTRY," ",""))
  shapes       <- bind_rows(shp_country,shp_regional)

  regional_NA  <- shp_regional %>% group_by(COUNTRY) %>% summarise(all_codes = str_c(REG_CODE, collapse = ","))#plot all regions for non-location-specific values for countries with OTHER location-specific values

  df <- df %>% mutate(parameter_value = coalesce(parameter_value, 100*cfr_ifr_numerator/cfr_ifr_denominator)) %>%
    mutate(parameter_value = if(range_mp) {
      coalesce(parameter_value, (parameter_lower_bound + parameter_upper_bound)/2)
    } else {parameter_value}) %>%
    mutate(REG_CODE =
             ifelse(
               population_country %in% country_dat,
               shp_country$REG_CODE[match(population_country,shp_country$COUNTRY)],
               ifelse(
                 is.na(population_location),
                 regional_NA$all_codes[match(population_country,regional_NA$COUNTRY)],
                 case_when(
                   population_country == "Benin" ~ str_replace_all(population_location, c("Central Region" = "BJ04,BJ05,BJ07,BJ12")),
                   population_country == "Côte d'Ivoire" ~ str_replace_all(population_location, c("Abidjan" = "CI01", "Northeastern Region" = "CI08,CI14")),
                   population_country == "Ghana" ~ str_replace_all(population_location, c("Ankaakur" = "GH02", "Ehiawenwu" = "GH06", "Amomaso" = "GH03", "Menkwo" = "GH04", "Mangoase" = "GH04", "Jirandogo" = "GH11", "Bowena" = "GH11", "Natorduori" = "GH13", "Teanoba" = "GH09", "Doniga Naveransa" = "GH12", "Tamale" = "GH08", "Agogo" = "GH02", "Kumasi" = "GH02")),#map in article
                   population_country == "Guinea" ~ str_replace_all(population_location, c("Gueckedou" = "GN08","Macenta" = "GN08","Pita" = "GN07","Lola" = "GN08","Yomou" = "GN08")),
                   population_country == "Mali" ~ str_replace_all(population_location, c("Bougouni District" = "ML03")),
                   population_country == "Nigeria" ~ str_replace_all(population_location, c("Edo State" = "NG12", "Southeast" = "NG01,NG04,NG11,NG14,NG17","Benue River" = "NG07","Ibadan" = "NG31")),
                   population_country == "Sierra Leone" ~ str_replace_all(population_location, c("Kenema" = "SL01", "Port Loko" = "SL05", "Tonkolili" = "SL02","Niahun" = "SL01", "Konia" = "SL01", "Palima" = "SL01", "Semewabu" = "SL01", "Tongola" = "SL01", "Njakundoma" = "SL01", "Kpandebu" = "SL01", "Neama" = "SL01", "Lowoma" = "SL01", "Landoma" = "SL01", "Bomie" = "SL01", "Yengema" = "SL03", "Kamethe" = "SL05", "Kamabunyele" = "SL02", "Kathumpe" = "SL02")),
                   population_country == "Central African Republic" ~ str_replace_all(population_location, c("Nola And Ikaumba" = "CF23,CF12", "The Pre\nForest\nGrassland Of Bozo And Bangassou" = "CF11,CF62", "The Moist\nWooded Grassland Of Bouar And Obo" = "CF22,CF63", "The Dry Wooded\nRassland Near Mbre" = "CF51", "And The Dry Grassland-Of Birao." = "CF53")),#map in article
                   population_country == "Gabon" ~ str_replace_all(population_location, c("Haut-Ogooue" = "GA02")),
                   population_country == "Liberia" ~ str_replace_all(population_location, c("Zigida" = "LR08","Lofa County" = "LR08", "Montserrado County" = "LR11")),
                   TRUE ~ population_location)))) %>%
    mutate(REG_CODE = str_replace_all(REG_CODE, " ", "")) %>%
    mutate(REG_CODE = sapply(str_split(REG_CODE, ","), function(x) paste(unique(x), collapse = ","))) %>% #remove duplicate regions for each value
    separate_longer_delim(REG_CODE, delim = ",")#broadcast multi-region values

  if (summ_dups=="mean") {
    df <- df %>% group_by(REG_CODE) %>%
      mutate(value = mean(parameter_value)) %>% distinct(REG_CODE,value)
  } else if (summ_dups=="most_recent") {
    df <- df %>% group_by(REG_CODE) %>%
      mutate(value = ifelse(all(is.na(population_study_start_year)),
                            first(parameter_value),
                            parameter_value[which.max(!is.na(population_study_start_year))])) %>% distinct(REG_CODE, value)
  } else if (summ_dups=="max_sample") {
    df <- df %>% group_by(REG_CODE) %>%
      mutate(value = ifelse(all(is.na(population_sample_size)),
                            first(parameter_value),
                            parameter_value[which.max(!is.na(population_sample_size))])) %>% distinct(REG_CODE, value)
  } else {stop("Error: choose summary option for duplicate REG_CODEs")}

  shapes <- shapes %>% left_join(df,by=c('REG_CODE'))

  gg <- ggplot() +
    geom_sf(data = shapes, lwd = 0.3, col = "grey40", aes(fill = value)) +
    geom_sf(data = l0, lwd = 0.7, col = "black",  fill = NA) +
    scale_fill_viridis_c(option = color_opt, direction = -1, na.value = "grey80",
                         limits = col_lim) +
    geom_point(data = f, aes(x = longitude, y = latitude, color = "Outbreak"), shape = 8, size = 2.5, stroke = 1.5) +
    scale_color_manual(values = c("Outbreak" = "green1")) +
    geom_text(data = n, aes(x = longitude, y = latitude, label = c_name), size = 3.4, fontface = 'italic', color = "black") +
    coord_sf(xlim = long_lim, ylim = lat_lim) +
    theme_void() +
    guides(fill = guide_colorbar(title = NULL,order = 1), color = guide_legend(title = NULL)) +
    theme(legend.key.size = unit(2, "lines"), legend.position = c(0.07, 0.5), legend.text = element_text(size = 12)) +
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "bl", which_north = "true", pad_y = unit(0.4, "in"), style = north_arrow_fancy_orienteering) +
    annotation_custom(grob = rectGrob(gp = gpar(col = "black", fill = NA, lwd = 3)),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    ggtitle(title)
  #geom_text(data = f %>% filter(type == "Treatment Centre"), aes(x = longitude, y = latitude, color = type), label = "T", size = 5, fontface='bold') +
  #scale_color_manual(values = c(Outbreak = "green1", "Treatment Centre" = "blue")) +
  #guides(fill = guide_colorbar(title = NULL), color = guide_legend(title = NULL, override.aes = list(shape = c(8,NA)))) +
  #scale_shape_manual(values = c(circle = 21, square = 22, triangle = 24)) +
  #scale_color_manual(values = c(blue = "blue", green = "green")) +
  #geom_emoji(data=f, aes(longitude,latitude), emoji = "", size=0.03) +

  return(gg)
}

#wrapper function for metamean

metamean_wrap <- function(dataframe, estmeansd_method,
                          plot_study, digits, lims, colour, label,
                          width, height, resolution, subgroup = NA, sort_by_subg = FALSE, colgap_shift = 0){

  dataframe <- filter_df_for_metamean(dataframe)
  dataframe <- dataframe[!is.na(dataframe$id),]
  if(!is.na(subgroup))
  {
    mtan <- metamean(data = dataframe,
                     studlab = refs,
                     mean = xbar,
                     sd = parameter_uncertainty_single_value,
                     median = median,
                     q1 = q1,
                     q3 = q3,
                     min = min,
                     max = max,
                     n = population_sample_size,
                     method.mean = estmeansd_method,
                     method.sd = estmeansd_method,
                     subgroup = dataframe[[subgroup]],
                     sm = "MRAW",
                     method.tau = "ML")

    png(file = "temp.png", width = width, height = height, res = resolution)
    forest(mtan, layout = "RevMan5",
           overall = TRUE, pooled.events = TRUE,
           study.results = plot_study,
           print.subgroup.name = FALSE, sort.subgroup = sort_by_subg,
           digits = digits, digits.sd = digits, digits.weight = digits,
           col.diamond.lines = "black",col.diamond.common = colour, col.diamond.random = colour,
           weight.study = "same", col.square.lines = "black", col.square = colour, col.study = "black", col.inside = "black",
           at = seq(lims[1],lims[2],by=2), xlim = lims, xlab = label, fontsize = 10, colgap.forest.left = paste0( colgap_shift,"cm"))
    dev.off()
  } else {
    mtan <- metamean(data = dataframe,
                     studlab = refs,
                     mean = xbar,
                     sd = parameter_uncertainty_single_value,
                     median = median,
                     q1 = q1,
                     q3 = q3,
                     min = min,
                     max = max,
                     n = population_sample_size,
                     method.mean = estmeansd_method,
                     method.sd = estmeansd_method,
                     sm = "MRAW",
                     method.tau = "ML")

    png(file = "temp.png", width = width, height = height, res = resolution)
    forest(mtan, layout = "RevMan5",
           overall = TRUE, pooled.events = TRUE,
           study.results = plot_study,
           digits = digits, digits.sd = digits, digits.weight = digits,
           col.diamond.lines = "black",col.diamond.common = colour, col.diamond.random = colour,
           weight.study = "same", col.square.lines = "black", col.square = colour, col.study = "black", col.inside = "black",
           at = seq(lims[1],lims[2],by=2), xlim = lims, xlab = label, fontsize = 10)
    dev.off()
  }

  gg <- png::readPNG("temp.png", native = TRUE)
  file.remove("temp.png")
  gg <- wrap_elements(plot = rasterGrob(gg, interpolate = TRUE))
  return(list(result = mtan, plot = gg))
}

## THIS IS INITIAL VERSION ONLY -- WORK IN PROGRESS
metagen_wrap <- function(dataframe, estmeansd_method,
                         plot_study, digits, lims, colour, label,
                         width, height, resolution, subgroup = NA, sort_by_subg = FALSE){

  #dataframe <- epireview::filter_df_for_metamean(dataframe)
  # must have the correct columns
  df <- dataframe
  cols_needed <- c("parameter_value", "parameter_unit", "population_sample_size",
                   "parameter_value_type", "parameter_uncertainty_single_type",
                   "parameter_uncertainty_type", "parameter_uncertainty_lower_value",
                   "parameter_uncertainty_upper_value")

  if (!all(cols_needed %in% colnames(df))) {
    cols_missing <- cols_needed[!cols_needed %in% colnames(df)]
    stop(
      "df must have columns named: ", paste(cols_needed, collapse = ", "),
      ". Columns missing: ", paste(cols_missing, collapse = ", "),
      call. = FALSE
    )
  }

  ## Ensure that there is a single parameter type present
  if(length(unique(df$parameter_type)) != 1) {
    stop("parameter_type must be the same across all values.", call. = FALSE)
  }

  ## First check that there are no rows where a value is present but unit is
  ## missing, or vice versa
  if(any(is.na(df$parameter_value) & !is.na(df$parameter_unit))) {
    message("parameter_value must be present if parameter_unit is present.
              Rows with non-NA parameter_value and NA parameter_unit will be
              removed.")
    df <- filter(df,!( is.na(.data[["parameter_value"]]) & !is.na(.data[["parameter_unit"]]) ))
  }

  if(any(!is.na(df$parameter_value) & is.na(df$parameter_unit))) {
    message("parameter_unit is missing but parameter_value is present.
              Rows with non-NA parameter_value and NA parameter_unit will be
              removed."
    )
    df <- filter(df,!( is.na(.data[["parameter_value"]]) &
                         !is.na(.data[["parameter_unit"]]) ))
  }

  # values of the parameter must all have the same units
  if(length(unique(df$parameter_unit[!is.na(df$parameter_unit)])) != 1) {
    msg1 <- "parameter_unit must be the same across all values."
    msg2 <- "Consider calling delays_to_days() if you are working with delays."
    stop(paste(msg1, msg2), call. = FALSE)
  }

  # For this meta analysis don't enforce that we need to have sample sizes
  # df <- df %>% filter(!is.na(.data[["population_sample_size"]])) %>%
  #   filter(!is.na(.data[["parameter_value"]])) %>%
  #   filter(
  #     (.data[["parameter_value_type"]] == 'Mean' &
  #        grepl(x = tolower(.data[["parameter_uncertainty_single_type"]]),
  #              pattern = 'standard deviation')) |
  #       (.data[["parameter_value_type"]] == 'Median' &
  #          grepl(x = tolower(.data[["parameter_uncertainty_type"]]),
  #                pattern = 'iqr')) |
  #       (.data[["parameter_value_type"]] == 'Median' &
  #          grepl(x = tolower(.data[["parameter_uncertainty_type"]]),
  #                pattern = 'range'))
  #   )

  df <- mutate(
    df,
    xbar = ifelse(
      .data[["parameter_value_type"]] == "Mean", .data[["parameter_value"]], NA),
    median = ifelse(
      .data[["parameter_value_type"]] == "Median", .data[["parameter_value"]], NA
    ),
    q1 = ifelse(
      grepl(x = tolower(.data[["parameter_uncertainty_type"]]), "iqr"),
      .data[["parameter_uncertainty_lower_value"]], NA),
    q3 = ifelse(grepl(x = tolower(.data[["parameter_uncertainty_type"]]), "iqr"),
                .data[["parameter_uncertainty_upper_value"]], NA),
    min = ifelse(
      grepl(
        x = tolower(.data[["parameter_uncertainty_type"]]), pattern = "range"
      ) &
        !grepl(x = tolower(.data[["parameter_uncertainty_type"]]), "iqr"),
      .data[["parameter_uncertainty_lower_value"]], NA
    ),
    max = ifelse(
      grepl(x = tolower(.data[["parameter_uncertainty_type"]]), pattern = "range"
      ) &
        !grepl(x = tolower(.data[["parameter_uncertainty_type"]]), pattern = "iqr"),
      .data[["parameter_uncertainty_upper_value"]], NA
    )
  )

  CI_level     <- as.double(substr(df$parameter_uncertainty_type,1,2))/100
  CI_level_adj <- CI_level + (1-CI_level)/2
  SE           <- (df$parameter_uncertainty_upper_value-df$parameter_uncertainty_lower_value)/(2* qnorm(CI_level_adj))
  ln_case      <- !is.na(str_detect(str_to_lower(df$distribution_type),'log'))
  SE[ln_case]  <- (df$parameter_uncertainty_upper_value[ln_case]-df$parameter_uncertainty_lower_value[ln_case])/(2* qlnorm(CI_level_adj[ln_case]))
  df$SE        <- SE

  dataframe <- df %>% filter(!is.na(SE))

  if(!is.na(subgroup))
  {
    mtan <- metagen(data = dataframe,
                    TE = parameter_value,
                    seTE = SE,
                    studlab = refs,
                    #mean = xbar,
                    #sd = parameter_uncertainty_single_value,
                    median = median,
                    q1 = q1,
                    q3 = q3,
                    min = min,
                    max = max,
                    method.mean = estmeansd_method,
                    method.sd = estmeansd_method,
                    subgroup = dataframe[[subgroup]],
                    sm = "R0",
                    method.tau = "REML")

    png(file = "temp.png", width = width, height = height, res = resolution)
    forest(mtan, layout = "RevMan5",
           overall = TRUE, pooled.events = TRUE,
           study.results = plot_study,
           print.subgroup.name = FALSE, sort.subgroup = sort_by_subg,
           digits = digits, digits.sd = digits, digits.weight = digits,
           col.diamond.lines = "black",col.diamond.common = colour, col.diamond.random = colour,
           weight.study = "same", col.square.lines = "black", col.square = colour, col.study = "black", col.inside = "black",
           at = seq(lims[1],lims[2],by=2), xlim = lims, xlab = label, fontsize = 10)
    dev.off()
  } else {
    mtan <- metagen(data = dataframe,
                    TE = parameter_value,
                    seTE = SE,
                    studlab = refs,
                    #mean = xbar,
                    #sd = parameter_uncertainty_single_value,
                    median = median,
                    q1 = q1,
                    q3 = q3,
                    min = min,
                    max = max,
                    #n = population_sample_size,
                    method.mean = estmeansd_method,
                    method.sd = estmeansd_method,
                    sm = "R0",
                    method.tau = "REML"
    )

    png(file = "temp.png", width = width, height = height, res = resolution)
    forest(mtan, layout = "RevMan5",
           overall = TRUE, pooled.events = TRUE,
           study.results = plot_study,
           digits = digits, digits.sd = digits, digits.weight = digits,
           col.diamond.lines = "black",col.diamond.common = colour, col.diamond.random = colour,
           weight.study = "same", col.square.lines = "black", col.square = colour, col.study = "black", col.inside = "black",
           at = seq(lims[1],lims[2],by=2), xlim = lims, xlab = label, fontsize = 10)
    dev.off()
  }

  gg <- png::readPNG("temp.png", native = TRUE)
  file.remove("temp.png")
  gg <- wrap_elements(plot = rasterGrob(gg, interpolate = TRUE))
  return(list(result = mtan, plot = gg))
}

#wrapper function for metaprop

metaprop_wrap <- function(dataframe, subgroup,
                          plot_pooled, sort_by_subg, plot_study, digits, colour,
                          width, height, resolution,
                          at = seq(0,1,by=0.2), xlim = c(0,1)){

  stopifnot(length(unique(dataframe$parameter_unit[!is.na(dataframe$parameter_unit)])) == 1)#values must have same units

  dataframe <- dataframe %>% filter(!is.na(cfr_ifr_denominator)) %>%
    filter(!(is.na(cfr_ifr_numerator)&is.na(parameter_value))) %>%
    mutate(cfr_ifr_numerator = case_when(
      is.na(cfr_ifr_numerator) & !is.na(parameter_value) ~ round((parameter_value/100)*cfr_ifr_denominator),
      TRUE ~ cfr_ifr_numerator))

  if(!is.na(subgroup))
  {
    mtan <- metaprop(data = dataframe,
                     studlab = refs,
                     event = cfr_ifr_numerator,
                     n = cfr_ifr_denominator,
                     subgroup = dataframe[[subgroup]],
                     sm = "PLOGIT",
                     method="GLMM",
                     method.tau = "ML")

    png(file = "temp.png", width = width, height = height, res = resolution)
    forest(mtan, layout = "RevMan5",
           overall = plot_pooled, pooled.events = TRUE,
           print.subgroup.name = FALSE, sort.subgroup = sort_by_subg,
           study.results = plot_study,
           digits = digits,
           col.diamond.lines = "black",col.diamond.common = colour, col.diamond.random = colour,
           col.subgroup = "black", col.inside = "black",
           weight.study = "same", #col.square.lines = "green", col.square = "blue", #not working
           at = at, xlim = xlim, xlab="Case Fatality Ratio", fontsize=11)
    dev.off()
  } else {
    mtan <- metaprop(data = dataframe,
                     studlab = refs,
                     event = cfr_ifr_numerator,
                     n = cfr_ifr_denominator,
                     sm = "PLOGIT",
                     method="GLMM",
                     method.tau = "ML")

    png(file = "temp.png", width = width, height = height, res = resolution)
    forest(mtan, layout = "RevMan5",
           overall = plot_pooled, pooled.events = TRUE,
           study.results = plot_study,
           digits = digits,
           col.diamond.lines = "black",col.diamond.common = colour, col.diamond.random = colour,
           col.subgroup = "black", col.inside = "black",
           weight.study = "same", #col.square.lines = "green", col.square = "blue", #not working
           at = at, xlim = xlim, xlab="Case Fatality Ratio", fontsize=11)
    dev.off()
  }


  pg <- png::readPNG("temp.png", native = TRUE)
  file.remove("temp.png")
  gg <- wrap_elements(plot = rasterGrob(pg, interpolate = TRUE))
  return(list(result = mtan, plot = gg))
}

#function to plot pdfs for given dataframe and inputted distribution types

pdf_generic <- function(meta, model, dists, lims, label) {

  x_values <- seq(lims[1],lims[2], by = 0.01)

  mean   <- meta[[paste("TE.", model, sep="")]]
  se_m   <- meta[[paste("seTE.", model, sep="")]]
  sd_p   <- sqrt(sum((meta$n-1)*(meta$sd^2))/(sum(meta$n)-meta$k))
  r_mean <- rnorm(10000,mean,se_m)

  dist_data <- data.frame()
  for (j in 1:length(dists)){
    pdfs <- matrix(0, ncol = length(x_values), nrow = length(r_mean))
    if (dists[j]=="Exponential"){
      for (i in 1:length(r_mean)){
        pdfs[i,] <- dexp(x_values, rate = 1/r_mean[i])}
      y_central <- dexp(x_values, rate = 1/mean)
    } else if (dists[j]=="Weibull"){
      for (i in 1:length(r_mean)){
        w_params <- weibullpar(r_mean[i], sd_p)
        pdfs[i,] <- dweibull(x_values, shape = w_params$shape, scale = w_params$scale)}
      w_params  <- weibullpar(mean, sd_p)
      y_central <- dweibull(x_values, shape = w_params$shape, scale = w_params$scale)
    } else if (dists[j]=="Gamma"){
      for (i in 1:length(r_mean)){
        pdfs[i,] <- dgamma(x_values, shape = (r_mean[i]/sd_p)^2, rate = r_mean[i]/(sd_p^2))}
      y_central <- dgamma(x_values, shape = (mean/sd_p)^2, rate = mean/(sd_p^2))
    } else if (dists[j]=="Lognormal"){
      for (i in 1:length(r_mean)){
        mean_log <- log(r_mean[i]^2 / sqrt(r_mean[i]^2 + sd_p^2))
        sd_log   <- sqrt(log(1 + sd_p^2 / r_mean[i]^2))
        pdfs[i,] <- dlnorm(x_values, meanlog = mean_log, sdlog = sd_log)}
      mean_log  <- log(mean^2 / sqrt(mean^2 + sd_p^2))
      sd_log    <- sqrt(log(1 + sd_p^2 / mean^2))
      y_central <- dlnorm(x_values, meanlog = mean_log, sdlog = sd_log)
    } else {
      y_central <- rep(0, times = length(x_values))
    }
    y_bounds <- apply(pdfs, 2, function(column)
    {quantile(column, c(0.025, 0.975))})
    df        <- data.frame(x_values = x_values, dist = rep(dists[j], length(x_values)),
                            y_l = y_bounds[1,], y_u = y_bounds[2,], y_central = y_central)
    dist_data <- rbind(dist_data,df)
  }

  gg <- ggplot() +
    geom_ribbon(data = dist_data, aes(x = x_values, ymin = y_l, ymax = y_u, fill = dist), alpha = 0.2) +
    geom_line(data = dist_data, aes(x = x_values, y = y_central, color = dist), size = 0.25) +
    scale_fill_lancet(palette = "lanonc") + scale_color_lancet(palette = "lanonc") +
    scale_x_continuous(limits = lims, expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 0.151), expand = c(0, 0)) +
    labs(x = label, y = "Probability Density") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
          legend.position = c(1,1), legend.justification = c(1,1), legend.box.just = "left") +
    guides(color = "none", fill = guide_legend(title = NULL,order = 1))

  return(gg)
}

#function to create latex-readable csv files for tables

insert_blank_rows <- function(dataframe, column) {
  tlabels <- as.character(unique(dataframe[[column]]))

  df_split <- split(dataframe, dataframe[[column]])

  dataframe <- do.call(rbind, lapply(df_split, function(group) {
    rbind(group, rep(NA, ncol(group)))
  }))

  dataframe <- rbind(NA,dataframe)#add NAs for top row header
  dataframe <- dataframe[-nrow(dataframe),]#remove NAs at bottom

  inds                 <- which(is.na(dataframe[[column]]))
  dataframe[[column]] <- NULL #Set column to NA

  dataframe[inds,1]    <- tlabels
  dataframe[[1]][inds] <- paste0("\\bfseries{", dataframe[[1]][inds], "}")

  dataframe            <- dataframe %>% mutate_all(~ ifelse(is.na(.), "", .))

  dataframe
}

# --------- Copied from epireview:
# Changes: parameter_uncertainty_singe_type -> parameter_uncertainty_single_type
filter_df_for_metamean <- function (df)
{
  cols_needed <- c("parameter_value", "parameter_unit", "population_sample_size",
                   "parameter_value_type", "parameter_uncertainty_single_type",
                   "parameter_uncertainty_type", "parameter_uncertainty_lower_value",
                   "parameter_uncertainty_upper_value")
  df <- epireview::check_df_for_meta(df, cols_needed)
  df <- df[!is.na(df[["population_sample_size"]]), ]
  df <- df[!is.na(df[["parameter_value"]]), ]
  df <- df[df[["parameter_value_type"]] == "Mean" & grepl(x = tolower(df[["parameter_uncertainty_single_type"]]),
                                                          pattern = "standard deviation") | df[["parameter_value_type"]] ==
             "Median" & grepl(x = tolower(df[["parameter_uncertainty_type"]]),
                              pattern = "iqr") | df[["parameter_value_type"]] == "Median" &
             grepl(x = tolower(df[["parameter_uncertainty_type"]]),
                   pattern = "range"), ]
  df$xbar <- ifelse(df[["parameter_value_type"]] == "Mean",
                    df[["parameter_value"]], NA)
  df$median <- ifelse(df[["parameter_value_type"]] == "Median",
                      df[["parameter_value"]], NA)
  df$q1 <- ifelse(grepl(x = tolower(df[["parameter_uncertainty_type"]]),
                        "iqr"), df[["parameter_uncertainty_lower_value"]], NA)
  df$q3 <- ifelse(grepl(x = tolower(df[["parameter_uncertainty_type"]]),
                        "iqr"), df[["parameter_uncertainty_upper_value"]], NA)
  df$min <- ifelse(grepl(x = tolower(df[["parameter_uncertainty_type"]]),
                         pattern = "range") & !grepl(x = tolower(df[["parameter_uncertainty_type"]]),
                                                     "iqr"), df[["parameter_uncertainty_lower_value"]], NA)
  df$max <- ifelse(grepl(x = tolower(df[["parameter_uncertainty_type"]]),
                         pattern = "range") & !grepl(x = tolower(df[["parameter_uncertainty_type"]]),
                                                     pattern = "iqr"), df[["parameter_uncertainty_upper_value"]],
                   NA)
  df
}
