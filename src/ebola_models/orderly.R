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

orderly_resource("models_with_code.csv")

# Load data
articles <- read_csv("articles.csv")
models <- read_csv("models.csv")
code_models <- read_csv("models_with_code.csv")

source("ebola_functions.R")
source("ebola_visualisation.R")

df <- left_join(
  models,
  articles[, c("covidence_id", "first_author_surname", "year_publication",
               "article_label", "doi", "notes", "article_qa_score")],
  by = "covidence_id"
) %>%
  arrange(article_label, -year_publication)

print(sprintf("n model studies overall: %d", length(unique(df$article_label))))
print(sprintf("m models: %d", length(df$article_label)))

model_dat <- df %>%
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
    assumptions = case_when(is.na(assumptions) ~ "Unspecified", TRUE ~ assumptions),
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

# How many have code associated?
model_dat %>% filter(code_available %in% "Yes") %>% nrow() # n=37, 13%

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
      case_when(
        assumptions %in%
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
  select(`Model type` = model_type, `Total` = count) %>%
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
  mutate(
    model_type = factor(
      model_type, levels = c("Compartmental", "Other or combination",
                             "Branching process", "Agent/Individual based")
    )
  ) %>%
  separate_rows(assumptions, sep = ";") %>%
  # remove "unspecified" assumptions
  filter(!assumptions %in% "Unspecified") %>%
  group_by(model_type, assumptions) %>%
  summarise(count = n()) %>%
  arrange(model_type, desc(count)) %>%
  select(
    `Model type` = model_type,
    `Assumptions` = assumptions,
    `Total` = count
  ) %>%
  group_by(`Model type`) %>%
  mutate(
    index_of_change = row_number(),
    index_of_change = ifelse(
      index_of_change == max(index_of_change), 1, 0
    )
  ) %>%
  as_grouped_data(groups = "Model type") %>%
  as_flextable(col_keys = c("Assumptions", "Total"), hide_grouplabel = TRUE
  ) %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  hline(i = ~ !is.na(`Model type`)) %>%
  hline(i = ~ index_of_change == 1) %>%
  bold(j = 1, i = ~ !is.na(`Model type`), bold = TRUE, part = "body" ) %>%
  border_inner_h(part = "header") %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  add_footer_lines("") %>%
  line_spacing(i = 1, space = 1.5, part = "header")

save_as_image(assump_tab, path = "Model_results/assumptions_table.png")

## Models with code
ids_to_remove = c("6334", "20368", "15958", "4301", "17203", "3342", "17333", 
                  "22053", "2941", "8800", "8734", "7283", "865")
# Remove incomplete ones and remove stochastic 4778 - stochastic
code_models_data = code_models %>% filter(! covidence_id %in% ids_to_remove) %>%
  filter(!(covidence_id == "4778" & stoch_deter == "Stochastic")) %>%
  select(c(Article = article_label,
           `QA score (%)` = article_qa_score,
            Purpose = "Purpose of model/application", 
           `Model type` = "model_type",
           `Heath states` = "Heath states considered (up to 10 states)",
           `Uncertainty considered` = "Uncertainty considered", 
           `Fitted?` = "Is the model fitted?", 
           `Fitting Method` = "Fitting Method", 
           `Data` = "Data used", 
           `Outbreak period` = "Period of outbreak modelled", 
           `Spatial model` = "Is the model spatial?", 
           `Spillover included` = "Include spillover?", 
           `Language` = "Language of model", 
           `Link` = "DOI/link for code", 
           `README?` = "Is there a README?"
           )) 
  code_models_complete = code_models_data %>%
  arrange(`Model type`, desc(`Article`)
           ) %>%
  as_flextable() %>%
  fontsize(i = 1, size = 10, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  #hline(i = ~ index_of_change == 1) %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  #hline(i = ~ !is.na(`Model type`)) %>%
  #bold(j = 1, i = ~ !is.na(`Model type`), bold = TRUE, part = "body" ) %>%
  add_footer_lines("") %>%
  align(align = "left", part = "all") %>%
  line_spacing(i = 1, space = 1.5, part = "header")

p_code_model_tbl <- autofit(code_models_complete) |> paginate()

# Make sure to remove white space by adjusting width and height
# Can only save paginated version to docx or rtf
save_as_docx(p_code_model_tbl, path = "Model_results/code_table.docx",
             pr_section = prop_section(
               page_size = page_size(orient = "landscape", width = 23, height = 20),
               type = "continuous",
               page_margins = page_mar(bottom = 0, top = 0, right = 0, left = 0, gutter = 0)
             ))

# Then convert to pdf
docx2pdf("Model_results/code_table.docx",
         output = "Model_results/code_table.pdf")

print(sprintf("n model studies: %d", length(unique(code_models_data$Article))))
print(sprintf("m models: %d", length(code_models_data$Article)))

print(sprintf("m model compartmental: %d", sum(code_models_data$`Model type` == "Compartmental")))
print(sprintf("m model branching: %d", sum(code_models_data$`Model type` == "Branching process")))
print(sprintf("m model agent: %d", sum(code_models_data$`Model type` == "Agent/Individual based")))

print(sprintf("m language R: %d %f", sum(code_models_data$`Language` == "R"), 
              sum(code_models_data$`Language` == "R")/length(code_models_data$`Language`)))
print(sprintf("m language python: %d %f", sum(code_models_data$`Language` == "Python"), 
              sum(code_models_data$`Language` == "Python")/length(code_models_data$`Language`)))
print(sprintf("m language MATLAB: %d %f", sum(code_models_data$`Language` == "MATLAB"), 
              sum(code_models_data$`Language` == "MATLAB")/length(code_models_data$`Language`)))

print(sprintf("m Readme: %d %f", sum(code_models_data$`README?` == "Yes"), 
              sum(code_models_data$`README?` == "Yes")/length(code_models_data$`Language`)))

print(sprintf("m Spatial: %d %f", sum(code_models_data$`Spatial model` == "Yes"), 
              sum(code_models_data$`Spatial model` == "Yes")/length(code_models_data$`Language`)))

print(sprintf("m Spillover: %d %f", sum(code_models_data$`Spillover included` == "Yes"), 
              sum(code_models_data$`Spillover included` == "Yes")/length(code_models_data$`Language`)))

print(sprintf("m purpose Transmission: %d %f", sum(code_models_data$Purpose == "Transmission"), 
              sum(code_models_data$`Purpose` == "Transmission")/length(code_models_data$`Language`)))
print(sprintf("m purpose Forecast: %d %f", sum(code_models_data$`Purpose` == "Forecast"), 
              sum(code_models_data$`Purpose` == "Forecast")/length(code_models_data$`Language`)))
print(sprintf("m purpose method development: %d %f", sum(code_models_data$`Purpose` == "Methodological development"), 
              sum(code_models_data$`Purpose` == "Methodological development")/length(code_models_data$`Language`)))



print(sprintf("m Fitted: %d %f", sum(code_models_data$`Fitted?` == "Yes"), 
              sum(code_models_data$`Fitted?` == "Yes")/length(code_models_data$`Language`)))

print(sprintf("m Fitting method MCMC: %d %f", sum(code_models_data$`Fitting Method` == "MCMC", na.rm = T), 
              sum(code_models_data$`Fitting Method` == "MCMC", na.rm = T)/length(code_models_data$`Language`)))
print(sprintf("m Fitting method MLE: %d %f", sum(code_models_data$`Fitting Method` == "MLE", na.rm = T), 
              sum(code_models_data$`Fitting Method` == "MLE", na.rm = T)/length(code_models_data$`Language`)))
