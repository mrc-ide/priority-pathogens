## Function to create plot

create_plot <- function(df, param = NA, r_type = NA, qa_filter = TRUE) {
  
  df %>%
  filter(
    if(param == "Reproduction number") {
    parameter_type_short == r_type
    },
    if(qa_filter) {
    article_qa_score >= 50
    }
  ) %>%
  
  ggplot(aes(x = parameter_value,
             y = article_label_unique,
             col = population_country)) +
  theme_minimal() +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "dark grey") +
  geom_point(aes(x = parameter_value, y = article_label_unique,
                 shape = parameter_value_type), size = 2.5) +
  scale_y_discrete(labels = setNames(df_plot$article_label,
                                     df_plot$article_label_unique)) +
  geom_segment(aes(y = article_label_unique, yend = article_label_unique,
                   x = parameter_lower_bound, xend = parameter_upper_bound,
                   group = parameter_data_id), lwd = 1.5, alpha = 0.4) +
  geom_errorbar(aes(y = article_label_unique,
                    xmin = parameter_uncertainty_lower_value,
                    xmax = parameter_uncertainty_upper_value,
                    group = parameter_data_id),
                #linetype = comb_uncertainty_type),
                width = 0.4, lwd = 1) +
  labs(x = param, y = "", linetype = "", colour = "") +
  facet_col(facets = vars(outbreak), scales = "free", space  = "free") +
  theme(legend.text = element_text(size = 10),
        strip.text = element_text(size = 10),
        legend.title = element_blank()) +
  coord_cartesian(xlim = c(0, 10)) +
  guides(colour = guide_legend(order = 1, ncol = 1),
         linetype = guide_legend(order = 2, ncol = 1))

}

## Function to create table of parameter values

# df = clean data frame
# if param = reproduction number r_type is either "Basic (R0)" or "Effective (Re)"

create_table <- function(df, param = NA, r_type = NA, qa_filter = TRUE) {
  
  df <- df %>%
    filter(parameter_class == param)
  
  if(param == "Reproduction number") {
    df <- df %>%
      filter(parameter_type_short == r_type)
  }
  
  if (qa_filter) {
    df <- df %>%
      filter(article_qa_score >= 50)
  }
  
  border_style <- fp_border(color = "black", width = 1)
  set_flextable_defaults(background.color = "white")
  
  r_tbl <- df %>%
    select(c(Article = article_label,
             `Outbreak` = outbreak,
             Country = population_country,
             `Central estimate` = parameter_value,
             `Central range` = parameter_bounds,
             `Central type` = parameter_value_type,
             `Uncertainty type` = comb_uncertainty_type,
             `Uncertainty` = comb_uncertainty,
             `Method` = method_r,
             `Adjustment` = cfr_ifr_method,
             `Denominator` = cfr_ifr_denominator,
             `Population Group` = population_group,
             `Survey date` = survey_date,
             `Survey start day` = population_study_start_day,
             `Survey start month` = population_study_start_month,
             `Survey start year` = population_study_start_year,
             `Timing of survey` = method_moment_value)) %>%
    arrange(
      `Outbreak`,
      Country,
      `Survey start year`,
      `Survey start month`,
      `Survey start day`
    ) %>%
    group_by(`Outbreak`) %>%
    mutate(index_of_change = row_number(),
           index_of_change = ifelse(
             index_of_change == max(index_of_change), 1, 0)) %>%
    as_grouped_data(groups = "Outbreak") %>%
    flextable(
      col_keys = c("Outbreak", "Article", "Country", "Survey date",
                   "Central estimate", "Central range", "Central type",
                   "Uncertainty", "Uncertainty type", "Method", "Adjustment",
                   "Denominator"
      )) %>%
    fontsize(i = 1, size = 12, part = "header") %>%
    border_remove() %>%
    autofit() %>%
    theme_booktabs() %>%
    vline(j = c(4), border = border_style) %>%
    hline(i = ~ index_of_change == 1) %>%
    bold(i = 1, bold = TRUE, part = "header") %>%
    add_footer_lines("") %>%
    align(align = "left", part = "all")

  if(param != "Reproduction number") {
    tbl <- delete_columns(r_tbl, "Method")
  }
  
  if(param != "Severity") {
    tbl <- delete_columns(r_tbl, c("Adjustment", "Denominator"))
  }
  
tbl
  
}


# Function to generate min and max columns for a given parameter value type.
# These can be used to more easily get a range of the central mean, median and
# other/unspecified estimates
generate_min_max_columns <- function(data, value_type) {
  data %>%
    mutate(
      !!paste0("min_value_for_", value_type) := pmin(
        ifelse(parameter_value_type == value_type, parameter_value, NA),
        ifelse(parameter_value_type == value_type, parameter_upper_bound, NA),
        ifelse(parameter_value_type == value_type, parameter_lower_bound, NA),
        na.rm = TRUE
      ),
      !!paste0("max_value_for_", value_type) := pmax(
        ifelse(parameter_value_type == value_type, parameter_value, NA),
        ifelse(parameter_value_type == value_type, parameter_upper_bound, NA),
        ifelse(parameter_value_type == value_type, parameter_lower_bound, NA),
        na.rm = TRUE
      ))
}



## Function to create table of parameter values

# df = clean data frame
# group_tab = grouping of the ranges e.g. "Outbreak", "Country", "Species"
# value_type = which value types to create ranges for e.g. c("Mean", "Median",
# "Other_Unspecified", etc)

create_range_table <- function(df, group_tab, value_type, qa_filter = TRUE) {
  
border_style <- fp_border(color = "black", width = 1)
set_flextable_defaults(background.color = "white")

dat <- map_dfr(
  value_type, ~ generate_min_max_columns(value_type = ., data = df)
)

if (qa_filter) {
  dat <- dat %>% filter(article_qa_score >= 50)
}

if (group_tab == "Outbreak") {
  grouped_dat <- dat %>%
    group_by(outbreak, parameter_value_type)
}

if (group_tab == "Country") {
  grouped_dat <- dat %>%
    group_by(population_country, parameter_value_type)
}

if (group_tab == "Species") {
  grouped_dat <- dat %>%
    group_by(ebola_species, parameter_value_type)
}

suppressWarnings(
with_ranges <- grouped_dat %>%
  # to find the min and max of each value type
  summarize(
    across(starts_with("min_"), list(
      min = ~ min(., na.rm = TRUE)
    ), .names = "{.fn}_{str_extract(.col, '([^_]+)$')}"),
    across(starts_with("max_"), list(
      max = ~ max(., na.rm = TRUE)
    ), .names = "{.fn}_{str_extract(.col, '([^_]+)$')}"),
    # to fix incorrect n divide by number of value type groups
    n_estimates = n() / length(value_type)
  ) %>%
  clean_names() %>%
  mutate(
    across(starts_with("min_") | starts_with("max_"), ~ ifelse(is.infinite(.), NA, .)),
    est_range = case_when(
      parameter_value_type == "Mean" ~
        ifelse(min_mean == max_mean, as.character(max_mean),
               paste(min_mean, max_mean, sep = " - ")
        ),
      parameter_value_type == "Median" ~
        ifelse(min_median == max_median, as.character(max_median),
               paste(min_median, max_median, sep = " - ")
        ),
      parameter_value_type == "Other_Unspecified" ~
        ifelse(min_unspecified == max_unspecified, as.character(max_unspecified),
               paste(min_unspecified, max_unspecified, sep = " - ")
        ),
      TRUE ~ NA
    ),
    parameter_value_type = ifelse(
      parameter_value_type == "Other_Unspecified", "Other/Unspecified",
      parameter_value_type
    )
  )
)

if (group_tab == "Outbreak") {
  grouped_tab <- with_ranges %>%
    select(c(
      `Outbreak` = outbreak,
      `Estimate Type` = parameter_value_type,
      `Central Estimate Range` = est_range,
      `Number of Estimates` = n_estimates
    )) %>%
    group_by(`Outbreak`) %>%
    mutate(
      index_of_change = row_number(),
      index_of_change = ifelse(
        index_of_change == max(index_of_change), 1, 0
      )
    ) %>%
    as_grouped_data(groups = "Outbreak") %>%
    flextable(
      col_keys = c("Outbreak", "Estimate Type",
                   "Central Estimate Range", "Number of Estimates")
    )
}

if (group_tab == "Country") {
  grouped_tab <- with_ranges %>%
    select(c(
      `Country` = population_country,
      `Estimate Type` = parameter_value_type,
      `Central Estimate Range` = est_range,
      `Number of Estimates` = n_estimates
    )) %>%
    group_by(`Country`) %>%
    mutate(
      index_of_change = row_number(),
      index_of_change = ifelse(
        index_of_change == max(index_of_change), 1, 0
      )
    ) %>%
    as_grouped_data(groups = "Country") %>%
    flextable(
      col_keys = c("Country", "Estimate Type",
                   "Central Estimate Range", "Number of Estimates")
    )
}

if (group_tab == "Species") {
  grouped_tab <- with_ranges %>%
    select(c(
      `Species` = ebola_species,
      `Estimate Type` = parameter_value_type,
      `Central Estimate Range` = est_range,
      `Number of Estimates` = n_estimates
    )) %>%
    group_by(`Species`) %>%
    mutate(
      index_of_change = row_number(),
      index_of_change = ifelse(
        index_of_change == max(index_of_change), 1, 0
      )
    ) %>%
    as_grouped_data(groups = "Species") %>%
    flextable(
      col_keys = c("Species", "Estimate Type",
                   "Central Estimate Range", "Number of Estimates")
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
  align(align = "left", part = "all")

tab_ranges
}
