library(dplyr)
library(orderly2)
library(readr)
library(stringr)
library(tibble)
library(tidyr)


# *============================================================================*
# *------------------------------ Orderly config ------------------------------*
orderly_strict_mode()

orderly_parameters(pathogen = NULL)

orderly_dependency(
  "db_cleaning",
  "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "models.csv", "params.csv", "outbreaks.csv"))

orderly_shared_resource("nipah_functions.R"="nipah_functions.R")

source("nipah_functions.R")

orderly_artefact(description = "Nipah supplementary tables and statistics",
                 c("tab_param_count_class.tex", "tab_param_count_type.tex",
                   "tab_model_count.tex"))


# *------------------------------- Read in data -------------------------------*
articles <- read_csv("articles.csv", show_col_types = FALSE)
models <- read_csv("models.csv", show_col_types = FALSE)
outbreaks <- read_csv("outbreaks.csv", show_col_types = FALSE)
parameters <- read_csv("params.csv", show_col_types = FALSE)

# Should plotting be TRUE?
dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

outbreaks  <- dfs$outbreaks
models     <- dfs$models

parameters <- dfs$parameters |>
  left_join(qa_scores) |>
  mutate(parameter_value = coalesce(parameter_value,central)) |>
  arrange(desc(parameter_value))

# *--------------------------- Latex tables for SI ----------------------------*
# *----------- All | qa counts class table (tab:param_count_class) ------------*
neat_orderly_output <- function(){
  cli_h1("Creating SI latex summary tables")
  param_count_noqa <- parameters |>
    group_by(parameter_class) |>
    summarise(n_param   = n(),
              n_article = length(unique(refs)),
              .groups="drop")

  param_count_qa <- parameters |>
    group_by(parameter_class) |>
    filter(qa_score>0.5) |>
    summarise(n_param   = n(),
              n_article = length(unique(refs)),
              .groups="drop")

  param_count_class <- param_count_noqa |>
    left_join(param_count_qa,
              by=c('parameter_class'), suffix = c('_noqa','_qa')) |>
    mutate(
      param_post_qa_filter = paste0(
        round(replace_na(n_param_qa,0) / n_param_noqa * 100,0),'%'),
      article_post_qa_filter = paste0(
        round(replace_na(n_article_qa,0) / n_article_noqa * 100,0),'%')
    )

  cli_h2("tab:param_count_class")
  param_count_class_tex <- param_count_class |>
    select(-c(param_post_qa_filter,
              article_post_qa_filter)) |>
    xtable::xtable(type = "latex") |>
    print(include.rownames=FALSE)

  write(param_count_class_tex, file="tab_param_count_class.tex")

# *------------ All | qa counts type table (tab:param_count_type) -------------*
  param_count_noqa_type <- parameters |> group_by(parameter_type) |>
    summarise(n_param   = n(),
              n_article = length(unique(refs)))

  param_count_qa_type <- parameters |>
    group_by(parameter_type) |>
    filter(qa_score>0.5) |>
    summarise(n_param   = n(),
              n_article = length(unique(refs)),
              .groups="drop")

  param_count_type <- param_count_noqa_type |>
    left_join(param_count_qa_type,
              by=c('parameter_type'), suffix = c('_noqa','_qa')) |>
    mutate(
      param_post_qa_filter = paste0(
        round(replace_na(n_param_qa,0) / n_param_noqa * 100,0),'%'),
      article_post_qa_filter = paste0(
        round(replace_na(n_article_qa,0) / n_article_noqa * 100,0),'%')
    )

  parameter_type_replacement <- c("symptom onset"="onset",
                                  "admission to care"="admission",
                                  "Human delay"="Delay",
                                  "human delay \\(go to section\\)"="")
  param_count_type$parameter_type <- str_replace_all(param_count_type$parameter_type,
                                                     parameter_type_replacement)

  cli_h2("tab:param_count_type")
  param_count_type_tex <- param_count_type |>
    select(-c(param_post_qa_filter,
              article_post_qa_filter)) |>
    xtable::xtable(type = "latex") |>
    print(include.rownames=FALSE)

  write(param_count_type_tex,
        file="tab_param_count_type.tex")

# *------------------- Model count table (tab:model_count) --------------------*
  model_count <- models |>
    group_by(model_type, stoch_deter) |>
    summarise(n_param   = n()) |>
    arrange(stoch_deter) |>
    pivot_wider(names_from = stoch_deter,
                values_from = n_param,
                values_fill = 0) |>
    arrange(model_type)

  cli_h2("tab:model_count\n")
  model_count_tex <- model_count |>
    xtable::xtable(type = "latex") |>
    print(include.rownames=FALSE)

  write(model_count_tex,
        file="tab_model_count.tex")

# *--------------------------- Parameter statistics ---------------------------*
  cli_h1("Generating summary parameter statistics")

  cli_h2("Overall param count")
  param_count_overall <- parameters |>
    mutate(param_grouping= ifelse(parameter_type=='Risk factors',
                                  "Risk factors", "Non-risk factor")) |>
    group_by(param_grouping) |>
    summarise(n_param   = n(),
              n_article = length(unique(refs)),
              .groups = "drop") |>
    print()

  # All not QA filtered
  cli_h2("Severity: \n")
  # Returns inf when no values to min or max
  parameters |>
    filter(parameter_class == 'Severity') |>
    mutate(cfr_ifr_method = replace_na(cfr_ifr_method,'Unspecified')) |>
    group_by(parameter_type,population_country) |>
    mutate(
      parameter_uncertainty_lower_value = case_when(
        is.infinite(parameter_uncertainty_lower_value) ~ NA,
        TRUE ~ parameter_uncertainty_lower_value ),
      parameter_uncertainty_upper_value = case_when(
        is.infinite(parameter_uncertainty_upper_value) ~ NA,
        TRUE ~ parameter_uncertainty_upper_value )
    ) |>
    summarise(n_param = n(),
              n_article = length(unique(refs)),
              mean = mean(parameter_value, na.rm = TRUE),
              #weighted.mean = weighted.mean(parameter_value, population_sample_size, na.rm = TRUE),
              central_min = ifelse(all(is.na(parameter_value)),
                                   NA, min(parameter_value, na.rm = TRUE)),
              central_max = ifelse(all(is.na(parameter_value)),
                                   NA, max(parameter_value, na.rm = TRUE)),
              uncertainty_min = ifelse(all(is.na(parameter_uncertainty_lower_value)),
                                       NA, min(parameter_uncertainty_lower_value, na.rm = TRUE)),
              uncertainty_max = ifelse(all(is.na(parameter_uncertainty_lower_value)),
                                       NA, max(parameter_uncertainty_lower_value, na.rm = TRUE)),
              .groups = "drop")

  parameters |>
    filter(parameter_class == 'Severity') |>
    mutate(cfr_ifr_method = replace_na(cfr_ifr_method,'Unspecified'),
           cfr_ifr_method = case_when(cfr_ifr_method=="Naïve" ~ 'Naive',
                                      TRUE ~ cfr_ifr_method)) |>
    group_by(cfr_ifr_method) |>
    summarise(n_param   = n(),
              n_article = length(unique(refs)),
              .groups = "drop") |>
    print()

  cli_h2("Delays: \n")
  parameters |>
    filter(parameter_class == 'Human delay') |>
    group_by(parameter_type) |>
    mutate(
      parameter_uncertainty_lower_value = case_when(
        is.infinite(parameter_uncertainty_lower_value) ~ NA,
        TRUE ~ parameter_uncertainty_lower_value ),
      parameter_uncertainty_upper_value = case_when(
        is.infinite(parameter_uncertainty_upper_value) ~ NA,
        TRUE ~ parameter_uncertainty_upper_value )
    ) |>
    summarise(n_param = n(),
              n_article = length(unique(refs)),
              central_min = ifelse(all(is.na(parameter_value)),
                                       NA, min(parameter_value, na.rm = TRUE)),
              central_max = ifelse(all(is.na(parameter_value)),
                                       NA, max(parameter_value, na.rm = TRUE)),
              uncertainty_min = ifelse(all(is.na(parameter_uncertainty_lower_value)),
                                       NA, min(parameter_uncertainty_lower_value, na.rm = TRUE)),
              uncertainty_max = ifelse(all(is.na(parameter_uncertainty_lower_value)),
                                       NA, max(parameter_uncertainty_lower_value, na.rm = TRUE)),
              .groups = "drop") |>
    print()

  cli_h2("R0: \n")
  parameters |>
    filter(parameter_class == 'Reproduction number')  |>
    group_by(parameter_type) |>
    summarise(min=min(parameter_value),
              max=max(parameter_value)) |>
    print()


  cli_h2("Risk factors")
  cat("All risk factors: \n")
  parameters |>
    filter(parameter_type=='Risk factors') |>
    group_by(riskfactor_outcome) |>
    summarise(n_param   = n(),
              n_article = length(unique(refs)),
              .groups = "drop") |>
    print()

  cat("\nSignificant risk factors: \n")
  # No na risk factor outcomes
  parameters |>
    filter(parameter_type=='Risk factors',
           !is.na(riskfactor_outcome),
           riskfactor_significant == 'Significant') |>
    group_by(riskfactor_outcome) |>
    summarise(n_param   = n(),
              n_article = length(unique(refs)),
              .groups = "drop") |>
    print()

  cat("\nRisk factors by significance and QA score (>0.5 = high qa): \n")
  parameters |>
    filter(parameter_type=='Risk factors') |>
    mutate(riskfactor_significant = replace_na(riskfactor_significant,
                                               "Unspecified"),
           qa_label = case_when(qa_score>0.5~'high_qa',
                                TRUE ~ 'low_qa')) |>
    group_by(riskfactor_significant, qa_label) |>
    summarise(n_param   = n(),
              n_article = length(unique(refs)),
              .groups = "drop") |>
    print()

  cli_h2("Serology high QA score (>0.5) by population group: \n")
  parameters |> filter(str_starts(parameter_type,'Seroprevalence ') ) |>
    filter(qa_score>0.5) |>
    group_by(population_group) |>
    summarise(n_param   = n(),
              n_article = length(unique(refs)),
              .groups = "drop") |>
    print()

  cli_h2("Incubation period high QA score: \n")
  parameters |>
    filter(parameter_type == 'Human delay - incubation period' ) |>
    filter(qa_score>0.5) |>
    summarise(n_param   = n(),
              n_article = length(unique(refs))) |>
    print()
}

neat_orderly_output()
