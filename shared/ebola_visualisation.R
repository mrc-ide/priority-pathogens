## Function to create plot

create_plot <- function(df, param = NA, r_type = NA, qa_filter = TRUE,
                        facet_by = NA, symbol_shape_by = NA,
                        symbol_col_by = "population_country", axis_label = NA) {
  # Sort out the uncertainty for plotting 
  df$plot_uncertainty = NA
  df$plot_uncertainty[which(df$parameter_uncertainty_type %in% c("95% CrI", "HPDI 95%", "95% CI",  "HDPI 95%"))] = "About estimate"
  df$plot_uncertainty[which(df$parameter_uncertainty_type %in% c("Range", "IQR"))] = "About estimate"
  df$plot_uncertainty[which(df$parameter_uncertainty_singe_type %in% c("SD of the inverse mean", "SD of the mean"))] = "About estimate"
  df$plot_uncertainty[which(df$parameter_uncertainty_singe_type %in% c("SD of the sample"))] = "About sample"
  
  tmp = df[which(is.na(df$plot_uncertainty)), c(81, 16, 13)]
  
  mypalette <- hue_pal()(length(levels(df[[symbol_col_by]])))
  names(mypalette) <- sort(levels(df[[symbol_col_by]]))

  # Helping to distinguish between similar countries
  if (symbol_col_by == "population_country") {
    mypalette["Guinea, Sierra Leone"] <- "#006F19" # done
      mypalette["Guinea, Liberia, Sierra Leone"] <- "#2571b2"
        mypalette["Guinea, Liberia, Nigeria, Sierra Leone"] <- "#a25505"
          mypalette["Nigeria"] <- "#8b56b2"
            mypalette["Liberia, Sierra Leone"] <- "#6f0056"
              mypalette["Liberia"] <- "#00C091"
                mypalette["Sudan"] <- "#f818ca"
                  mypalette["Uganda"] <- "#e06666"
                    mypalette["Gabon"] <- "#d5d800"
  }
  
  if (param == "Reproduction number") {
    df <- df %>%
      filter(parameter_type_short == r_type)
  }

  if (qa_filter) {
    df <- df %>%
      filter(article_qa_score >= 50)
  }

  base_plot <- df %>%
    ggplot(aes(
      x = parameter_value,
      y = article_label_unique,
      col = !!sym(symbol_col_by)
    )) +
    theme_minimal()

  if (!is.na(symbol_shape_by)) {
    base_plot <- base_plot +
      geom_point(aes(
        x = parameter_value, y = article_label_unique,
        shape = !!sym(symbol_shape_by),
      ), size = 2.5)
  } else {
    base_plot <- base_plot +
      geom_point(aes(x = parameter_value, y = article_label_unique),
        size = 2.5
      )
  }

  plot <- base_plot +
    scale_y_discrete(labels = setNames(
      df$article_label,
      df$article_label_unique
    )) +
    geom_segment(aes(
      y = article_label_unique, yend = article_label_unique,
      x = parameter_lower_bound, xend = parameter_upper_bound,
      group = parameter_data_id
    ), lwd = 2.5, alpha = 0.4) +
    geom_errorbar(
      aes(
        y = article_label_unique,
        xmin = parameter_uncertainty_lower_value,
        xmax = parameter_uncertainty_upper_value,
        group = parameter_data_id,
        linetype = plot_uncertainty
      ),
      width = 0.4, lwd = 1
    ) +
    labs(x = param, y = "", linetype = "", colour = "") +
    theme(
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 10, face = "bold"),
      legend.title = element_blank(),
      axis.title.x = element_text(vjust = -1),
      legend.spacing.x = unit(6, 'mm')
    ) +
    scale_color_manual(values = mypalette,
                       labels = function(x) str_wrap(x, width = 43)) +
    guides(
      colour = guide_legend(order = 1, ncol = 1),
      linetype = guide_legend(order = 2, ncol = 1)
    ) +
    scale_linetype_manual(breaks = c("About estimate", "About sample"),
                          values = c("solid", "dashed"))
  
  if (!is.na(facet_by)) {
    plot <- plot +
      facet_col(facets = vars(!!sym(facet_by)), scales = "free_y", space = "free")
  }
  
  if (param == "Reproduction number") {
    plot <- plot +
      scale_x_continuous(limits = c(0, 10), expand = c(0, 0), oob = scales::squish) +
      geom_vline(xintercept = 1, linetype = "dashed", colour = "dark grey") + 
      guides(linetype="none")
  }

  if (param == "Severity") {
    plot <- plot +
      coord_cartesian(xlim = c(0, 100), expand = FALSE, clip = "off") +
      xlab("CFR (%)")
  }
  
  if (param == "Seroprevalence") {
    plot <- plot + xlab("Seroprevalence (%)") +
      coord_cartesian(xlim = c(0, 100), expand = FALSE, clip = "off")
  }
  
  if (param == "Human delay") {
    plot <- plot + xlab("Delay (Days)")
  }
  
  if (param == "Doubling time") {
    plot <- plot +
      coord_cartesian(xlim = c(10, 70), expand = FALSE, clip = "off") +
      xlab("Doubling time (Days)")
  }
  
  if (param == "Mutations") {
    plot <- plot +
      #scale_x_continuous(limits = c(0, 10), expand = c(0, 0), oob = scales::squish) +
      xlab(axis_label)
  }

  plot
}

## Function for rounding values within ranges of character variables
round_range <- function(range_string, digits = 0) {
  parts <- strsplit(range_string, " - ")[[1]]
  rounded_parts <- lapply(parts, function(x) round(as.numeric(x), digits = digits))
  pasted_range <- paste(rounded_parts, collapse = " - ")
  returned_range <- na_if(pasted_range, "NA")

  returned_range
}

## Function to create table of parameter values - this is NOT generic

# df = clean data frame
# param = the parameter_class you're interested in (e.g. "Reproduction number,
# "Human delay", "Severity", etc)
# r_type = if param = reproduction number r_type is either "Basic (R0)" or "Effective (Re)"
# rounding = optional rounding, either "integer" (rounds to whole number), "1d" (1 digit) or "2d" (2 digits)
# delay_type = if param = "Human delay", delay_type is the delay grouping e.g.
# "Admission to care", "Symptom onset", "Infection process", "Death to burial"
# group = the variable used for grouping in the table

create_table <- function(df, param = NA, r_type = NA, delay_type = NA,
                         group = "outbreak", qa_filter = TRUE, rounding = "none") {
  
  df <- df %>%
    filter(parameter_class == param) %>%
    mutate(article_qa_score = round(article_qa_score, digits = 1))

  if (rounding == "integer") {
    df <- df %>%
      mutate(
        parameter_value = round(parameter_value),
        parameter_bounds = sapply(parameter_bounds, round_range),
        comb_uncertainty = sapply(comb_uncertainty, round_range)
      )
  }

  if (rounding == "1d") {
    df <- df %>%
      mutate(
        parameter_value = round(parameter_value, digits = 1),
        parameter_bounds = sapply(parameter_bounds, round_range, digits = 1),
        comb_uncertainty = sapply(comb_uncertainty, round_range, digits = 1)
      )
  }
  
  if (rounding == "2d") {
    df <- df %>%
      mutate(
        parameter_value = round(parameter_value, digits = 2),
        parameter_bounds = sapply(parameter_bounds, round_range, digits = 2),
        comb_uncertainty = sapply(comb_uncertainty, round_range, digits = 2)
      )
  }

  if (group == "outbreak") {
    group = "Outbreak"
  }
  
  if (param == "Reproduction number") {
    df <- df %>%
      filter(parameter_type_short == r_type)
  }
  
  if (!is.na(delay_type)) {
    df <- df %>%
      filter(delay_start == delay_type)
  }

  if (qa_filter) {
    df <- df %>%
      filter(article_qa_score >= 50)
  }

  border_style <- fp_border(color = "black", width = 1)
  set_flextable_defaults(background.color = "white", na.string = "")

  r_tbl <- df %>%
    select(c(
      parameter_type,
      Article = article_label,
      `QA score (%)` = article_qa_score,
      `Outbreak` = outbreak,
      Country = population_country,
      `Location` = population_location,
      `Central estimate` = parameter_value,
      `Unit` = parameter_unit,
      `Central range` = parameter_bounds,
      `Central type` = parameter_value_type,
      `Disaggregated by` = method_disaggregated_by,
      `Uncertainty type` = comb_uncertainty_type,
      `Uncertainty` = comb_uncertainty,
      `Method` = method_r,
      `Timing` = method_moment_value,
      delay_short,
      `Adjustment` = cfr_ifr_method,
      `Numerator` = cfr_ifr_numerator,
      `Denominator` = cfr_ifr_denominator,
      `Population Group` = population_group,
      `Population Sample` = population_sample_type,
      `Sample size` = population_sample_size,
      `Species` = ebola_species,
      `Survey date` = survey_date,
      `Survey start day` = population_study_start_day,
      `Survey start month` = population_study_start_month,
      `Survey start year` = population_study_start_year,
      `Timing of survey` = method_moment_value,
      `Inverse` = inverse_param,
      `Attack rate type` = attack_rate_type,
      `Genome site` = genome_site
    ))
  
  if (param != "Seroprevalence") {
    if (group != "Outbreak") {
    r_tbl <- r_tbl %>%
      arrange(
        !!sym(group),
        `Outbreak`,
        Country,
        `Survey start year`,
        `Survey start month`,
        `Survey start day`
        )
  } else {
    r_tbl <- r_tbl %>%
      arrange(
        `Outbreak`,
        Country,
        `Survey start year`,
        `Survey start month`,
        `Survey start day`
      )
    }
  }
  
  if (param == "Seroprevalence") {
    r_tbl <- r_tbl %>%
      arrange(
        !!sym(group),
        Country,
        `Survey start year`,
        `Survey start month`,
        `Survey start day`
      )
  }
  
    r_tbl <- r_tbl %>%
      group_by(!!sym(group)) %>%
    mutate(
      index_of_change = row_number(),
      index_of_change = ifelse(
        index_of_change == max(index_of_change), 1, 0
        )
      ) %>%
    as_grouped_data(groups = {{group}} )
    
    if (param %in% c("Reproduction number", "Overdispersion")) {
      r_tbl <- r_tbl %>% as_flextable(
        col_keys = c(
          "Outbreak", "Country", "Survey date",
          "Central estimate", "Central range", "Central type",
          "Uncertainty", "Uncertainty type", "Method", "Disaggregated by",
          "Article", "QA score (%)"
        ),
        hide_grouplabel = TRUE
      )
    }
    
    if (param == "Severity") {
      sev_labels <- data.frame(
        key = c(
          "Outbreak", "Country", "Survey date",
          "Central estimate", "Numerator", "Denominator",
          "Central range", "Uncertainty", "Uncertainty type", "Adjustment",
          "Population Sample", "Disaggregated by",
          "Article", "QA score (%)"
        ),
        label = c(
          "Outbreak", "Country", "Survey date",
          "Central estimate", "Deaths", "Cases",
          "Central range", "Uncertainty", "Uncertainty type", "Adjustment",
          "Population Sample", "Disaggregated by",
          "Article", "QA score (%)"
        )
      )
      r_tbl <- r_tbl %>% as_flextable(
        col_keys = c(
          "Outbreak", "Country", "Survey date",
          "Central estimate", "Numerator", "Denominator",
          "Central range", "Uncertainty", "Uncertainty type", "Adjustment",
          "Population Sample", "Disaggregated by",
          "Article", "QA score (%)"
        ),
        hide_grouplabel = TRUE
      ) %>%
        set_header_df(
          mapping = sev_labels,
          key = "key"
        )
    }
    
    if (param == "Human delay") {
      r_tbl <- r_tbl %>% as_flextable(
        col_keys = c(
          "Outbreak", "Country", "Survey date", "Central estimate",
          "Central range", "Central type", "Uncertainty", "Uncertainty type",
          "Population Sample", "Sample size", "Disaggregated by",
          "Article", "QA score (%)"
        ),
        hide_grouplabel = TRUE
      )
    }
    
    if (param == "Seroprevalence") {
      sero_labels <- data.frame(
        key = c(
          "Country", "Location", "Survey date", "Central estimate", "Central range",
          "Central type", "Uncertainty", "Population Sample", "Sample size",
          "Disaggregated by", "Article", "QA score (%)"
        ),
        label = c(
          "Country", "Location", "Survey date", "Central estimate", "Central range",
          "Central type", "Uncertainty (95% CI)", "Population Sample", "Sample size",
          "Disaggregated by", "Article", "QA score (%)"
        )
      )
      r_tbl <- r_tbl %>% as_flextable(
        col_keys = c(
          "Country", "Location", "Survey date", "Central estimate", "Central range",
          "Central type", "Uncertainty", "Population Sample", "Sample size",
          "Disaggregated by", "Article", "QA score (%)"
        ),
        hide_grouplabel = TRUE
      ) %>%
        set_header_df(
          mapping = sero_labels,
          key = "key"
        )
    }
    
    if (param == "Mutations") {
      r_tbl <- r_tbl %>% as_flextable(
        col_keys = c(
          "Country", "Survey date",
          "Central estimate", "Central range", "Central type", "Uncertainty",
          "Uncertainty type", "Genome site", "Population Sample",
          "Sample size", "Disaggregated by", "Species", "Article", "QA score (%)"
        ),
        hide_grouplabel = TRUE
      )
    }
    
    if (param %in% "Growth rate") {
      r_tbl <- r_tbl %>% as_flextable(
        col_keys = c(
          "Outbreak", "Country", "Survey date",
          "Central estimate", "Unit", "Central range", "Central type",
          "Uncertainty", "Uncertainty type", "Disaggregated by", "Article", "QA score (%)"
        ),
        hide_grouplabel = TRUE
      )
    }
    
    if (param %in% "Doubling time") {
      dt_labels <- data.frame(
        key = c(
          "Outbreak", "Country", "Survey date", "Central estimate",
          "Central range", "Uncertainty", "Disaggregated by", "Article", "QA score (%)"
        ),
        label = c(
          "Outbreak", "Country", "Survey date", "Central estimate (Days)",
          "Central range", "Uncertainty (95% CI)", "Disaggregated by", "Article", "QA score (%)"
        ))
      r_tbl <- r_tbl %>% as_flextable(
        col_keys = c(
          "Outbreak", "Country", "Survey date", "Central estimate",
          "Central range", "Uncertainty", "Disaggregated by", "Article", "QA score (%)"
        ),
        hide_grouplabel = TRUE
      ) %>%
        set_header_df(
          mapping = dt_labels,
          key = "key"
        )
    }
    
    
    if (param %in% "Attack rate") {
      ar_labels <- data.frame(
        key = c(
          "Outbreak", "Country", "Survey date",
          "Central estimate", "Central range", "Uncertainty",
          "Population Sample", "Sample size",
          "Population Group", "Disaggregated by", "Article", "QA score (%)"
        ),
        label = c(
          "Outbreak", "Country", "Survey date",
          "Central estimate (%)", "Central range", "Uncertainty (95% CI)",
          "Population Sample", "Sample size",
          "Population Group", "Disaggregated by", "Article", "QA score (%)"
        )
      )
      r_tbl <- r_tbl %>% as_flextable(
        col_keys = c(
          "Outbreak", "Country", "Survey date",
          "Central estimate", "Central range", "Uncertainty",
          "Population Sample", "Sample size",
          "Population Group", "Disaggregated by", "Article", "QA score (%)"
        ),
        hide_grouplabel = TRUE
      ) %>%
        set_header_df(
          mapping = ar_labels,
          key = "key"
        )
    }
    
    r_tbl <- r_tbl %>%
      fontsize(i = 1, size = 12, part = "header") %>%
    autofit() %>%
    theme_booktabs() %>%
    hline(i = ~ index_of_change == 1) %>%
    bold(i = 1, bold = TRUE, part = "header") %>%
      hline(i = ~ is.na(`Survey date`)) %>%
      bold(j = 1, i = ~ is.na(`Survey date`), bold = TRUE, part = "body" ) %>%
    add_footer_lines("") %>%
    align(align = "left", part = "all")

  r_tbl
}


# Function to generate min and max columns to get a range of the min and max
# parameter values for different groups

generate_min_max_columns <- function(data) {
  data %>%
    mutate(
      min_central = pmin(parameter_value, parameter_upper_bound,
        parameter_lower_bound,
        na.rm = TRUE
      ),
      max_central = pmax(parameter_value, parameter_upper_bound,
        parameter_lower_bound,
        na.rm = TRUE
      ),
      min_uncertainty = pmin(#parameter_uncertainty_single_value,
                             parameter_uncertainty_upper_value,
                             parameter_uncertainty_lower_value, na.rm = TRUE
                             ),
      max_uncertainty = pmax(#parameter_uncertainty_single_value,
                             parameter_uncertainty_upper_value,
                             parameter_uncertainty_lower_value, na.rm = TRUE)
    )
}



## Function to create table with ranges of parameter values by specified groups

# df = clean data frame
# main_group = variable name for grouping of the ranges e.g. "outbreak", "population_country", "ebola_species"
# main_group_label = how the variable name should appear in the flextable e.g. "Outbreak", "Country", "Species"
# sub_group = variable name for grouping within the main groups

create_range_table <- function(df, main_group = NA, main_group_label = NA,
                               sub_group = NA, sub_group_label = NA,
                               qa_filter = TRUE, rounding = "none") {
  border_style <- fp_border(color = "black", width = 1)
  set_flextable_defaults(background.color = "white")

  dat <- generate_min_max_columns(data = df)

  if (qa_filter) {
    dat <- dat %>% filter(article_qa_score >= 50)
  }

  if (is.na(sub_group)) {
    grouped_dat <- dat %>%
      group_by(!!sym(main_group))
  } else {
    grouped_dat <- dat %>%
      group_by(!!sym(main_group), !!sym(sub_group))
  }
  
  with_ranges <- grouped_dat %>%
    summarise(
      min = min(min_central, na.rm = TRUE),
      max = max(max_central, na.rm = TRUE),
      n_estimates = n(),
      min_unc = suppressWarnings(min(min_uncertainty, na.rm = TRUE)),
      max_unc = suppressWarnings(max(max_uncertainty, na.rm = TRUE))
    ) %>%
    mutate(
      across(c(min, min_unc, max, max_unc), ~ ifelse(is.infinite(.), NA, .)),
      est_range = ifelse(min == max, as.character(min),
                         paste(min, max, sep = " - ")
      ),
      est_unc = ifelse(min_unc == max_unc, as.character(min_unc),
                       paste(min_unc, max_unc, sep = " - ")
    ))

  if (is.na(sub_group)) {
  labels <- setNames(
    as.list(
      c(paste0(main_group_label))
    ),
    names(with_ranges[1])
  )
  } else {
    labels <- setNames(
      as.list(
        c(paste0(main_group_label), paste0(sub_group_label))
      ),
      names(with_ranges[1:2])
    )
  }

  if (rounding == "integer") {
    with_ranges <- with_ranges %>%
      mutate(
        est_range = sapply(est_range, round_range),
        est_unc = sapply(est_unc, round_range)
      )
  }

  if (rounding == "1d") {
    with_ranges <- with_ranges %>%
      mutate(
        est_range = sapply(est_range, round_range, digits = 1),
        est_unc = sapply(est_unc, round_range, digits = 1)
      )
  }

  if (is.na(sub_group)) {
    grouped_tab <- with_ranges %>%
      select(c(
        {{ main_group }},
        `Central Estimate Range` = est_range,
        `Uncertainty Range` = est_unc,
        `Total` = n_estimates
      )) %>%
      group_by(!!sym(main_group)) %>%
      mutate(
        index_of_change = row_number(),
        index_of_change = ifelse(
          index_of_change == max(index_of_change), 1, 0
        )
      ) %>%
      as_grouped_data(groups = {{ main_group }}) %>%
      mutate(
        index_of_change = ifelse(is.na(index_of_change), 0, index_of_change)
      ) %>%
      flextable(
        col_keys = c(
          {{ main_group }},
          "Central Estimate Range", "Uncertainty Range", "Total"
        )
      )
    
  } else {
    
    grouped_tab <- with_ranges %>%
      select(c(
        {{ main_group }},
        {{ sub_group }},
        `Central Estimate Range` = est_range,
        `Uncertainty Range` = est_unc,
        `Total` = n_estimates
      )) %>%
      group_by(!!sym(main_group)) %>%
      mutate(
        index_of_change = row_number(),
        index_of_change = ifelse(
          index_of_change == max(index_of_change), 1, 0
        )
      ) %>%
      as_grouped_data(groups = {{ main_group }}) %>%
      mutate(
        index_of_change = ifelse(is.na(index_of_change), 0, index_of_change)
      ) %>%
      flextable(
        col_keys = c(
          {{ main_group }}, {{ sub_group }},
          "Central Estimate Range", "Uncertainty Range", "Total"
        )
      )
  }

  tab_ranges <- grouped_tab %>%
    fontsize(i = 1, size = 12, part = "header") %>%
    border_remove() %>%
    autofit() %>%
    theme_booktabs() %>%
    hline(i = ~ index_of_change == 1) %>%
    bold(i = 1, bold = TRUE, part = "header") %>%
    add_footer_lines("") %>%
    align(align = "left", part = "all") %>%
    set_header_labels(values = labels)

  tab_ranges
}
