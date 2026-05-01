#function to tidy-up all dataframes

data_curation <- function(
  articles,
  outbreaks,
  models,
  parameters,
  plotting,
  switch_first_surname = FALSE
) {
  articles <- articles |>
    mutate(
      refs = paste(first_author_surname, " (", year_publication, ")", sep = "")
    ) |> #define references use what we have from epireview function to be consistent
    group_by(refs) |>
    mutate(counter = row_number()) |>
    ungroup() |> #distinguish same-author-same-year references
    mutate(
      new_refs = ifelse(
        refs %in% refs[duplicated(refs)],
        paste0(sub("\\)$", "", refs), letters[counter], ")"),
        refs
      )
    ) |>
    dplyr::select(-counter, -refs) |>
    rename(refs = new_refs) |>
    mutate(refs = str_to_title(refs))

  if (dim(outbreaks)[1] > 0) {
    outbreaks <- outbreaks |>
      mutate(refs = articles$refs[match(covidence_id, articles$covidence_id)])
  }

  if (dim(models)[1] > 0) {
    models <- models |>
      mutate(refs = articles$refs[match(covidence_id, articles$covidence_id)])
  }

  parameters <- parameters |>
    mutate(refs = articles$refs[match(covidence_id, articles$covidence_id)]) |>
    filter(!parameter_from_figure)

  param4plot <- parameters |>
    mutate(across(
      c(
        parameter_value,
        parameter_lower_bound,
        parameter_upper_bound,
        parameter_uncertainty_lower_value,
        parameter_uncertainty_upper_value,
        parameter_uncertainty_single_value
      ),
      ~ ifelse(inverse_param, 1 / ., .) *
        10^exponent *
        ifelse(parameter_unit %in% "Weeks", 7, 1)
    )) |>
    mutate(across(
      c(
        parameter_2_value,
        parameter_2_lower_bound,
        parameter_2_upper_bound,
        parameter_2_uncertainty_upper_value,
        parameter_2_uncertainty_lower_value,
        parameter_2_uncertainty_single_value
      ),
      ~ ifelse(inverse_param, 1 / ., .) *
        10^exponent *
        ifelse(parameter_unit %in% "Weeks", 7, 1)
    )) |>
    mutate(
      parameter_unit = ifelse(
        parameter_unit %in% "Weeks",
        "Days",
        parameter_unit
      )
    ) |>
    mutate(
      no_unc = (is.na(parameter_uncertainty_lower_value) &
        is.na(parameter_uncertainty_upper_value)),
      custom_se = ifelse(
        str_detect(str_to_lower(parameter_2_value_type), "standard deviation") &
          no_unc &
          !is.na(population_sample_size),
        parameter_2_value / sqrt(population_sample_size),
        NA
      ),
      # needs to be in order for case when - assumes that if there is an SE
      # that will be used first else use the custom SE.
      unc_inferred_from_se = str_detect(
        str_to_lower(parameter_uncertainty_single_type),
        "standard error"
      ) &
        no_unc,
      unc_inferred_from_custom = !is.na(custom_se) & no_unc,
      parameter_uncertainty_lower_value = case_when(
        unc_inferred_from_se ~ parameter_value -
          parameter_uncertainty_single_value,
        # unc_inferred_from_custom ~ parameter_value - custom_se,
        TRUE ~ parameter_uncertainty_lower_value
      ),
      parameter_uncertainty_upper_value = case_when(
        unc_inferred_from_se ~ parameter_value +
          parameter_uncertainty_single_value,
        # unc_inferred_from_custom ~ parameter_value + custom_se,
        TRUE ~ parameter_uncertainty_upper_value
      ),
      uncertainty_inferred_flag = case_when(
        unc_inferred_from_se ~ "inferred from SE",
        # unc_inferred_from_custom ~ "inferred from custom SE",
        TRUE ~ NA
      )
    ) |>
    mutate(
      central = coalesce(
        parameter_value,
        100 * cfr_ifr_numerator / cfr_ifr_denominator
      ),
      central_range_midpoint = 0.5 *
        (parameter_lower_bound + parameter_upper_bound)
    ) |>
    dplyr::select(
      -c(no_unc, unc_inferred_from_se, unc_inferred_from_custom, custom_se)
    )

  if (plotting) {
    parameters <- param4plot
  } else {
    check_param_id <- (parameters$parameter_data_id ==
      param4plot$parameter_data_id) # check that parameter data ids didn't get scrambled
    if (sum(check_param_id) == dim(parameters)[1]) {
      parameters$central <- param4plot$central
      parameters$central_range_midpoint <- param4plot$central_range_midpoint
    } else {
      errorCondition('parameters not in right order to match')
    }
  }

  if (dim(outbreaks)[1] > 0) {
    outbreaks <- outbreaks |>
      mutate(
        outbreak_location = str_replace_all(outbreak_location, "\xe9", "é")
      )
  }

  if (switch_first_surname) {
    # this is due to legacy access database issue
    articles <- articles |>
      rename(
        first_author_first_name = first_author_surname,
        first_author_surname = first_author_first_name
      )
  }

  return(list(
    articles = articles,
    outbreaks = outbreaks,
    models = models,
    parameters = parameters
  ))
}


curation <- function(articles, outbreaks, models, parameters, plotting) {
  #call data_curation function (which at some stage will move to epireview) but keep curation to be backward competible
  df <- data_curation(articles, outbreaks, models, parameters, plotting)

  return(list(
    articles = df$articles,
    outbreaks = df$outbreaks,
    models = df$models,
    parameters = df$parameters
  ))
}

# function to produce forest plot for given dataframe
forest_plot <- function(
  df,
  label,
  color_column,
  lims,
  text_size = 11,
  show_label = FALSE,
  custom_colours = NA,
  segment_show.legend = NA,
  point_show.legend = NA,
  sort = FALSE,
  qa_alpha = 1,
  point_size = 3
) {
  stopifnot(length(unique(df$parameter_unit[!is.na(df$parameter_unit)])) == 1) #values must have same units

  if (sort) {
    df <- df |> arrange(.data[[color_column]], central)
  }

  # Assume that if a single type was displayed it would be converted to a
  # lower + upper value
  df <- df |>
    mutate(urefs = make.unique(refs)) |>
    mutate(
      urefs = factor(urefs, levels = rev(unique(urefs))),
      uncertainty_present = (!(is.na(parameter_uncertainty_lower_value) &
        is.na(parameter_uncertainty_upper_value)))
    )
  df$plot_alpha <- 1
  df$segment_alpha <- 1

  if (qa_alpha != 1) {
    if(any(df$qa_score<=0.5)){
    df[df$qa_score <= 0.5, ]$plot_alpha <- qa_alpha
    df[df$qa_score <= 0.5, ]$segment_alpha <- 0.65 * qa_alpha
      }
    }

  cats <- length(unique(df[[color_column]]))
  gg <- ggplot(df) +
    geom_segment(
      aes(
        x = parameter_lower_bound,
        xend = parameter_upper_bound,
        y = urefs,
        yend = urefs,
        color = .data[[color_column]],
      ),
      linewidth = 3,
      alpha = df$segment_alpha,
      show.legend = segment_show.legend
    ) +
    geom_errorbar(
      aes(
        xmin = parameter_uncertainty_lower_value,
        xmax = parameter_uncertainty_upper_value,
        y = urefs,
        linetype = "Uncertainty",
        colour = .data[[color_column]]
      ),
      width = 0.25,
      lwd = 0.5,
      #color = "black",
      alpha = df$plot_alpha
    ) +
    geom_errorbar(
      data = df[!df$uncertainty_present, ],
      aes(
        xmin = parameter_2_lower_bound,
        xmax = parameter_2_upper_bound,
        y = urefs,
        linetype = "Variability",
        colour = .data[[color_column]]
      ),
      width = 0.25,
      lwd = 0.5,
      #color = "black",
      lineend = "square",
      alpha = df[!(df$uncertainty_present), ]$plot_alpha
    ) +
    geom_errorbar(
      data = df[df$uncertainty_present, ],
      aes(
        xmin = parameter_2_lower_bound,
        xmax = parameter_2_upper_bound,
        y = urefs,
        linetype = "Variability",
        colour = .data[[color_column]]
      ),
      width = 0.25,
      lwd = 0.5,
      #color = "black",
      lineend = "square",
      position = position_nudge(y = -0.25),
      alpha = df[df$uncertainty_present, ]$plot_alpha
    ) +
    geom_point(
      aes(
        x = parameter_value,
        y = urefs,
        shape = parameter_value_type,
        fill = .data[[color_column]]
      ),
      alpha = df$plot_alpha,
      size = point_size,
      stroke = 1,
      color = "black",
      show.legend = point_show.legend
    )

  if (all(df$parameter_class == "Reproduction number")) {
    gg <- gg +
      geom_vline(xintercept = 1, linetype = "dashed", colour = "dark grey")
  }

  if (sum(!is.na(custom_colours))) {
    gg <- gg +
      scale_shape_manual(
        name = "Parameter Type",
        values = c(
          Mean = 21,
          Median = 22,
          Other = 24,
          `Case Study` = 25,
          Unspecified = 23
        ),
        breaks = c(
          "Mean",
          "Median",
          "Other",
          "Case Study",
          "Unspecified"
        )
      ) +
      scale_linetype_manual(
        name = "Variation Type",
        values = c("Uncertainty" = "solid", "Variability" = "dashed"),
        breaks = c("Uncertainty", "Variability")
      ) +
      scale_x_continuous(limits = lims, expand = c(0, 0)) +
      scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
      labs(x = label, y = NULL) +
      scale_color_manual(values = custom_colours) +
      scale_fill_manual(values = custom_colours) +
      theme_minimal() +
      theme(
        panel.border = element_rect(
          color = "black",
          linewidth = 1.25,
          fill = NA
        ),
        text = element_text(size = text_size)
      )
  } else {
    gg <- gg +
      scale_fill_lancet(palette = "lanonc") +
      scale_color_lancet(palette = "lanonc") +
      scale_shape_manual(
        name = "Parameter Type",
        values = c(
          Mean = 21,
          Median = 22,
          Unspecified = 23,
          Other = 24,
          `Case Study` = 25
        ),
        breaks = c(
          "Mean",
          "Median",
          "Unspecified",
          "Other",
          "Case Study"
        )
      ) +
      scale_linetype_manual(
        name = "Variation Type",
        values = c("Uncertainty" = "solid", "Variability" = "dashed"),
        breaks = c("Uncertainty", "Variability")
      ) +
      scale_x_continuous(limits = lims, expand = c(0, 0)) +
      scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
      labs(x = label, y = NULL) +
      theme_minimal() +
      theme(
        panel.border = element_rect(
          color = "black",
          linewidth = 1.25,
          fill = NA
        ),
        text = element_text(size = text_size)
      )
  }

  if (cats == 1) {
    gg <- gg +
      guides(
        fill = "none",
        color = "none",
        shape = guide_legend(title = NULL, order = 1),
        linetype = guide_legend(title = NULL, order = 2)
      )
  } else {
    gg <- gg +
      guides(
        fill = "none",
        color = guide_legend(title = NULL, order = 1),
        shape = guide_legend(title = NULL, order = 2),
        linetype = guide_legend(title = NULL, order = 3)
      )
  }

  if (show_label) {
    gg <- gg +
      geom_text_repel(
        aes(
          x = coalesce(parameter_value),
          y = urefs,
          label = population_country_ISO
        ),
        nudge_y = 0.5,
        segment.color = "grey50"
      )
  }
  #gg <- gg + geom_text_repel(aes(x = coalesce(parameter_uncertainty_upper_value,parameter_upper_bound,parameter_value), y = urefs, label = population_country_ISO), nudge_x = 1.5, segment.color = "grey90" )

  return(gg)
}

#function to create latex-readable csv files for tables

insert_blank_rows <- function(dataframe, column) {
  tlabels <- as.character(unique(dataframe[[column]]))

  df_split <- split(dataframe, dataframe[[column]])

  dataframe <- do.call(
    rbind,
    lapply(df_split, function(group) {
      rbind(group, rep(NA, ncol(group)))
    })
  )

  dataframe <- rbind(NA, dataframe) #add NAs for top row header
  dataframe <- dataframe[-nrow(dataframe), ] #remove NAs at bottom

  inds <- which(is.na(dataframe[[column]]))
  dataframe[[column]] <- NULL #Set column to NA

  dataframe[inds, 1] <- tlabels
  dataframe[[1]][inds] <- paste0("\\bfseries{", dataframe[[1]][inds], "}")

  dataframe <- dataframe |> mutate_all(~ ifelse(is.na(.), "", .))

  dataframe
}

# *---------------------------- Colour functions ------------------------------*
get_colour_pop_groups <- function(df_all, df_subset) {
  bmj_colours <- ggsci::pal_bmj("default")(9)
  temp <- bmj_colours[4]
  bmj_colours[4] <- bmj_colours[6]
  bmj_colours[6] <- temp
  temp <- bmj_colours[1]
  bmj_colours[1] <- bmj_colours[2]
  bmj_colours[2] <- temp

  all_pop_groups <- df_all |>
    dplyr::distinct(population_group) |>
    dplyr::arrange(
      population_group == "Unspecified",
      population_group == "Other",
      population_group
    ) |>
    dplyr::pull()

  colours <- bmj_colours[seq_along(all_pop_groups)]
  names(colours) <- all_pop_groups

  if ("Unspecified" %in% names(colours)) {
    colours["Unspecified"] <- "grey50"
  }

  groups_in_subset <- unique(df_subset$population_group)
  return(colours[names(colours) %in% groups_in_subset])
}

get_colour_genome_groups <- function(df_all, df_subset) {
  nejm_colours <- ggsci::pal_nejm("default")(8)

  all_genomes <- df_all |>
    dplyr::distinct(genome_site) |>
    dplyr::pull()

  colours <- nejm_colours[seq_along(all_genomes)]
  names(colours) <- all_genomes

  if ("Unspecified" %in% names(colours)) {
    colours["Unspecified"] <- "grey50"
  }

  genomes_in_subset <- unique(df_subset$genome_site)
  return(colours[names(colours) %in% genomes_in_subset])
}
