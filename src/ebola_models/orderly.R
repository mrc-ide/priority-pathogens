# Ebola models

library(dplyr)
library(tidyr)
library(orderly2)
library(readr)
library(ggplot2)
library(ggforce)
library(flextable)
library(ftExtra)
library(officer)
library(purrr)
library(janitor)
library(stringr)
library(scales)
library(epitrix)
library(doconv)

orderly_strict_mode()

# Outputs
orderly_artefact(
  "Tables for ebola models",
  c(
    "Model_results/overview_table.docx",
    "Model_results/overview_table.pdf",
    "Model_results/model_type_table.png",
    "Model_results/assumptions_table.png"
  )
)

orderly_parameters(pathogen = "EBOLA")

# Get data from db_compilation
orderly_dependency(
  "db_compilation",
  "latest(parameter:pathogen == this:pathogen)",
  c(
    "articles.csv",
    "models.csv"
  )
)

# Script with functions for plots/tables
orderly_shared_resource("ebola_functions.R" = "ebola_functions.R")
orderly_shared_resource("ebola_visualisation.R" = "ebola_visualisation.R")

# Load data
articles <- read_csv("articles.csv")
models <- read_csv("models.csv")
source("ebola_functions.R")
source("ebola_visualisation.R")

df <- left_join(
  models,
  articles[, c("covidence_id", "first_author_surname", "year_publication",
               "article_label", "doi", "notes", "article_qa_score")],
  by = "covidence_id"
) %>%
  arrange(article_label, -year_publication)

model_dat <- df %>%
  # remove unspecified model
  filter(!model_type %in% "Unspecified") %>%
  # remove compartmental type because it's inconsistent (move this to cleaning.R?)
  select(-compartmental_type) %>%
  mutate(
  # group other and combinations of models together
    model_type = gsub(";", " & ", model_type),
    model_type = gsub(" / ", "/", model_type),
    model_type =
      case_when(
        model_type %in% c("Agent/Individual based & Compartmental",
                          "Compartmental & Other",
                          "Branching process & Compartmental",
                          "Other") ~ "Other or combination",
        TRUE ~ model_type
      ),
    code_available = as.character(code_available),
    code_available =
      case_when(code_available %in% "TRUE" ~ "Yes",
                code_available %in% "FALSE" ~ NA,
                TRUE ~ code_available),
    theoretical_model = as.character(theoretical_model),
    theoretical_model =
      case_when(theoretical_model %in% "TRUE" ~ "Yes",
                theoretical_model %in% "FALSE" ~ NA,
                TRUE ~ theoretical_model),
    interventions_type = gsub(";", ", ", interventions_type),
    assumptions = gsub("Heterogenity", "Heterogeneity", assumptions),
    assumptions = gsub("period is same", "period the same", assumptions),
    assumptions = gsub("- ", "", assumptions),
    stoch_deter =
      case_when(
        is.na(stoch_deter) & model_type %in% "Branching process" ~ "Stochastic",
        stoch_deter %in% "Deterministic;Stochastic" ~ "Deterministic & Stochastic",
        TRUE ~ stoch_deter),
    article_qa_score = round(article_qa_score, digits = 2),
    ebola_variant =
      case_when(
        ebola_variant %in%
          "Bundibugyo virus (BDBV);Sudan virus (SUDV);Taï Forest virus (TAFV);Zaire Ebola virus (EBOV)" ~
          "All species",
        ebola_variant %in% "Unspecified" ~ NA,
        TRUE ~ ebola_variant),
    article_label =
      case_when(
        ebola_variant %in% "Zaire Ebola virus (EBOV)" ~ paste0(article_label, "*"),
        ebola_variant %in% "Bundibugyo virus (BDBV)" ~ paste0(article_label, "**"),
        ebola_variant %in% "All species" ~ paste0(article_label, "***"),
        TRUE ~ article_label),
    interventions_type =
      case_when(
        interventions_type %in% "Unspecified" ~ NA, TRUE ~ interventions_type)
  )

unique(model_dat$model_type)
unique(model_dat$assumptions)

dir.create("Model_results")

# Create overview table for models
border_style <- fp_border(color = "black", width = 1)
set_flextable_defaults(background.color = "white", na.string = "")

model_tbl <- model_dat %>%
  mutate(
    assumptions = gsub(";", ", ", assumptions),
    assumptions = gsub("Heterogeneity in transmission rates between groups, Heterogeneity in transmission rates over time",
                       "Heterogeneity in transmission rates between groups and over time", assumptions),
    assumptions =
      case_when(assumptions %in%
                  "Heterogeneity in transmission rates between groups, Latent period the same as incubation period, Heterogeneity in transmission rates over time" ~
                  "Heterogeneity in transmission rates between groups and over time, Latent period the same as incubation period",
                TRUE ~ assumptions)
  ) %>%
  select(c(
    Article = article_label,
    `QA score (%)` = article_qa_score,
    `Model type` = model_type,
    `Stochastic or Deterministic` = stoch_deter,
    `Theoretical model` = theoretical_model,
    `Interventions` = interventions_type,
    `Code available` = code_available,
    `Assumptions` = assumptions
  )) %>%
  arrange(
    `Model type`, desc(`QA score (%)`)
  ) %>%
  group_by(`Model type`) %>%
  mutate(
    index_of_change = row_number(),
    index_of_change = ifelse(
      index_of_change == max(index_of_change), 1, 0
    )
  ) %>%
  as_flextable(
    col_keys = c(
      "Model type", "Stochastic or Deterministic",
      "Theoretical model", "Interventions", "Code available", "Assumptions",
      "Article", "QA score (%)"
    ),
    hide_grouplabel = TRUE
  ) %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  hline(i = ~ index_of_change == 1) %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  hline(i = ~ !is.na(`Model type`)) %>%
  bold(j = 1, i = ~ !is.na(`Model type`), bold = TRUE, part = "body" ) %>%
  add_footer_lines("") %>%
  align(align = "left", part = "all") %>%
  line_spacing(i = 1, space = 1.5, part = "header")

# Paginate the model table
p_model_tbl <- autofit(model_tbl) |> paginate()

# Make sure to remove white space by adjusting width and height
# Can only save paginated version to docx or rtf
save_as_docx(p_model_tbl, path = "Model_results/overview_table.docx",
             pr_section = prop_section(
               page_size = page_size(orient = "landscape", width = 22, height = 16),
               type = "continuous",
               page_margins = page_mar(bottom = 0, top = 0, right = 0, left = 0, gutter = 0)
             ))

# Then convert to pdf
docx2pdf("Model_results/overview_table.docx",
         output = "Model_results/overview_table.pdf")

# Model type summary
set_flextable_defaults(background.color = "white", na.string = "")
type_tab <- model_dat %>%
  group_by(model_type) %>%
  summarise(count = n()) %>%
  arrange(ifelse(model_type %in% "Other", Inf, desc(count))) %>%
  select(`Model type` = model_type, `Count` = count) %>%
  flextable() %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  border_inner_h(part = "header") %>%
  border_outer() %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  line_spacing(i = 1, space = 1.5, part = "header")

save_as_image(type_tab, path = "Model_results/model_type_table.png")

# Assumption summary
set_flextable_defaults(background.color = "white", na.string = "")
assump_tab <- model_dat %>%
  separate_rows(assumptions, sep = ";") %>%
  # remove "unspecified" assumptions
  filter(!assumptions %in% "Unspecified") %>%
  group_by(model_type, assumptions) %>%
  summarise(count = n()) %>%
  arrange(model_type, desc(count)) %>%
  as_grouped_data(groups = "model_type") %>%
  select(
    `Model type` = model_type,
    `Assumptions` = assumptions,
    `Count` = count
  ) %>%
  flextable() %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  hline(i = ~ !is.na(`Model type`)) %>%
  bold(j = 1, i = ~ !is.na(`Model type`), bold = TRUE, part = "body" ) %>%
  border_inner_h(part = "header") %>%
  border_outer() %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  add_footer_lines("") %>%
  line_spacing(i = 1, space = 1.5, part = "header")

save_as_image(assump_tab, path = "Model_results/assumptions_table.png")
