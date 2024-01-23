## Function to create plot

create_plot <- function(df, param = NA, r_type = NA, qa_filter = TRUE,
                        facet_by = NA, symbol_shape_by = NA,
                        symbol_col_by = "population_country") {
  
  mypalette <- hue_pal()(length(levels(df[[symbol_col_by]])))
  names(mypalette) <- sort(levels(df[[symbol_col_by]]))

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
        group = parameter_data_id
      ),
      width = 0.4, lwd = 1
    ) +
    labs(x = param, y = "", linetype = "", colour = "") +
    theme(
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 10),
      legend.title = element_blank(),
      axis.title.x = element_text(vjust = -1),
      legend.spacing.x = unit(6, 'mm')
    ) +
    scale_color_manual(values = mypalette,
                       labels = function(x) str_wrap(x, width = 43)) +
    guides(
      colour = guide_legend(order = 1, ncol = 1),
      linetype = guide_legend(order = 2, ncol = 1)
    )
  
  if (!is.na(facet_by)) {
    plot <- plot +
      facet_col(facets = vars(!!sym(facet_by)), scales = "free_y", space = "free")
  }
  
  if (param == "Reproduction number") {
    plot <- plot +
      scale_x_continuous(limits = c(0, 10), expand = c(0, 0), oob = scales::squish) +
      geom_vline(xintercept = 1, linetype = "dashed", colour = "dark grey")
  }

  if (param == "Severity") {
    plot <- plot + coord_cartesian(xlim = c(0, 100), expand = FALSE, clip = "off")
  }
  
  if (param == "Human delay") {
    plot <- plot + xlab("Delay (Days)")
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
# rounding = optional rounding, either "integer" (rounds to whole number) or "1d" (1 digit)
# delay_type = if param = "Human delay", delay_type is the delay grouping e.g.
# "Admission to care", "Symptom onset", "Infection process", "Death to burial"
# group = the variable used for grouping in the table

create_table <- function(df, param = NA, r_type = NA, delay_type = NA,
                         group = "outbreak", qa_filter = TRUE, rounding = "none") {
  
  df <- df %>%
    filter(parameter_class == param)

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

  if (group == "outbreak") {
    group = "Outbreak"
  }
  
  if (param == "Reproduction number") {
    df <- df %>%
      filter(parameter_type_short == r_type)
  }
  
  if (param == "Human delay") {
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
      `Survey date` = survey_date,
      `Survey start day` = population_study_start_day,
      `Survey start month` = population_study_start_month,
      `Survey start year` = population_study_start_year,
      `Timing of survey` = method_moment_value,
      `Inverse` = inverse_param
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
    
    if (param == "Reproduction number") {
      r_tbl <- r_tbl %>% as_flextable(
        col_keys = c(
          "Outbreak", "Article", "Country", "Survey date",
          "Central estimate", "Central range", "Central type",
          "Uncertainty", "Uncertainty type", "Method", "Disaggregated by"
        ),
        hide_grouplabel = TRUE
      )
    }
    
    if (param == "Severity") {
      r_tbl <- r_tbl %>% as_flextable(
        col_keys = c(
          "Outbreak", "Article", "Country", "Survey date",
          "Central estimate", "Numerator", "Denominator",
          "Central range", "Uncertainty", "Uncertainty type", "Adjustment",
          "Population Sample", "Disaggregated by"
        ),
        hide_grouplabel = TRUE
      )
    }
    
    if (param == "Human delay") {
      r_tbl <- r_tbl %>% as_flextable(
        col_keys = c(
          "Outbreak", "Article", "Country", "Survey date", "Central estimate",
          "Central range", "Central type", "Uncertainty", "Uncertainty type",
          "Population Sample", "Sample size", "Disaggregated by"
        ),
        hide_grouplabel = TRUE
      )
    }
    
    if (param == "Seroprevalence") {
      r_tbl <- r_tbl %>% as_flextable(
        col_keys = c(
          "Country", "Location", "Article", "Timing", "Survey date",
          "Central estimate", "Central range", "Central type", "Uncertainty",
          "Uncertainty type", "Population Sample", "Sample size", "Disaggregated by"
        ),
        hide_grouplabel = TRUE
      )
    }
    
    r_tbl <- r_tbl %>%
      fontsize(i = 1, size = 12, part = "header") %>%
    autofit() %>%
    theme_booktabs() %>%
    vline(j = "Survey date", border = border_style) %>%
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
        `Number of Papers` = n_estimates
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
          "Central Estimate Range", "Uncertainty Range", "Number of Papers"
        )
      )
    
  } else {
    
    grouped_tab <- with_ranges %>%
      select(c(
        {{ main_group }},
        {{ sub_group }},
        `Central Estimate Range` = est_range,
        `Uncertainty Range` = est_unc,
        `Number of Papers` = n_estimates
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
          "Central Estimate Range", "Uncertainty Range", "Number of Papers"
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
