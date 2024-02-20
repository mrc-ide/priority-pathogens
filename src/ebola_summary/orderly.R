# Ebola summary

library(dplyr)
library(orderly2)
library(readr)
library(ggplot2)
library(ggforce)
library(flextable)
library(officer)
library(purrr)
library(scales)
library(epitrix)
library(stringr)

orderly_strict_mode()

# Outputs
orderly_artefact(
  "Overall summary plots and tables",
  c(
    "Summary_results/parameter_type_table.png",
    "Summary_results/parameter_group_table.png",
    "Summary_results/parameter_qa_scores.png"
  )
)

orderly_parameters(pathogen = "EBOLA")

# Get data from db_compilation
orderly_dependency(
  "db_compilation",
  "latest(parameter:pathogen == this:pathogen)",
  c(
    "articles.csv",
    "parameters.csv"
  )
)

# Script with functions for plots/tables
orderly_shared_resource("ebola_functions.R" = "ebola_functions.R")
orderly_shared_resource("ebola_visualisation.R" = "ebola_visualisation.R")
source("ebola_functions.R")
source("ebola_visualisation.R")

# Load data
articles <- read_csv("articles.csv")
params <- read_csv("parameters.csv")

df <- left_join(
  params,
  articles[, c(
    "covidence_id", "first_author_surname", "year_publication",
    "article_label", "doi", "notes"
  )],
  by = "covidence_id"
)

dir.create("Summary_results")

###################################
# Summary table of all parameters #
###################################

nrow(df)

summary_dat <- df %>%
  mutate(
    parameter_type =
      case_when(
        parameter_type %in% "Reproduction number (Basic R0)" ~
          "Basic reproduction number",
        parameter_type %in% "Reproduction number (Effective, Re)" ~
          "Effective reproduction number",
        parameter_type %in% "Severity - case fatality rate (CFR)" ~
          "Case Fatality Rate (CFR)",
        parameter_class %in% "Human delay" ~ delay_short,
        TRUE ~ parameter_type
      ),
    parameter_class =
      case_when(parameter_class %in% "Human delay" ~ "Delay",
                TRUE ~ parameter_class)
  )

# Parameter group table
set_flextable_defaults(background.color = "white", na.string = "")
group_tab <- summary_dat %>%
  group_by(parameter_class) %>%
  summarise(count = n()) %>%
  select(
    `Parameter Group` = parameter_class,
    `Total Parameters` = count,
  ) %>%
  arrange(desc(`Total Parameters`)) %>%
  flextable() %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  border_inner_h(part = "header") %>%
  border_outer() %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  add_footer_lines("") %>%
  line_spacing(i = 1, space = 1.5, part = "header")
group_tab

save_as_image(group_tab, path = "Summary_results/parameter_group_table.png")

# Parameter type table
set_flextable_defaults(background.color = "white", na.string = "")
param_tab <- summary_dat %>%
  group_by(parameter_class, parameter_type) %>%
  summarise(count = n()) %>%
  select(
    `Parameter Group` = parameter_class,
    `Parameter Type` = parameter_type,
    `Total Parameters` = count,
  ) %>%
  arrange(desc(`Total Parameters`), `Parameter Group`) %>%
  flextable() %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  border_inner_h(part = "header") %>%
  border_outer() %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  add_footer_lines("") %>%
  line_spacing(i = 1, space = 1.5, part = "header")
param_tab

save_as_image(param_tab, path = "Summary_results/parameter_type_table.png")

# Summary of QA scores
hist(df$article_qa_score)

hist_dat <- summary_dat %>%
  mutate(parameter_class =
           factor(parameter_class,
                     levels = c("Delay", "Risk factors", "Reproduction number",
                                "Severity", "Seroprevalence", "Mutations",
                                "Attack rate", "Overdispersion", "Growth rate",
                                "Doubling time"
                                     )))

qa_hist <- ggplot(hist_dat, aes(x = article_qa_score)) +
  geom_histogram(binwidth = 10, fill = "#A7C7E7", color = "#21618C") +
  facet_wrap(~ parameter_class, scales = "free_y", ncol = 2) +
  labs(x = "Article QA Score (%)", y = "Frequency") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 11),
        axis.title.x = element_text(vjust = -1.5))

ggsave("Summary_results/parameter_qa_scores.png", qa_hist,
       width = 7, height = 9, units = "in", bg = "white"
)
