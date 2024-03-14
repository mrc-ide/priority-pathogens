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
library(splitstackshape)
library(writexl)

orderly_strict_mode()

# Outputs
orderly_artefact(
  "Overall summary plots and tables",
  c(
    "Summary_results/parameter_type_table.png",
    "Summary_results/parameter_group_table.png",
    "Summary_results/parameter_qa_scores.png",
    "Summary_results/all_articles.xlsx"
  )
)

orderly_parameters(pathogen = "EBOLA")

# Get data from db_compilation
orderly_dependency(
  "db_compilation",
  "latest(parameter:pathogen == this:pathogen)",
  c(
    "articles.csv",
    "models.csv",
    "parameters.csv"
  )
)

# Manually put the included articles exported from covidence in src/ebola_summary:
orderly_resource("covidence.csv")

# Script with functions for plots/tables
orderly_shared_resource("ebola_functions.R" = "ebola_functions.R")
orderly_shared_resource("ebola_visualisation.R" = "ebola_visualisation.R")
source("ebola_functions.R")
source("ebola_visualisation.R")

# Load data
articles <- read_csv("articles.csv")
models <- read_csv("models.csv")
params <- read_csv("parameters.csv")

df <- left_join(
  params,
  articles[, c(
    "covidence_id", "first_author_surname", "year_publication",
    "article_label", "doi", "article_title", "notes"
  )],
  by = "covidence_id"
)

df_models <- left_join(
  models,
  articles[, c(
    "covidence_id", "first_author_surname", "year_publication",
    "article_label", "article_title", "doi", "notes"
  )],
  by = "covidence_id"
)

dir.create("Summary_results")

# Check all numbers add up and then pull out numbers for text
nrow(df) # number of parameters 1280
nrow(df_models) # number of models 295
table(df$outbreak) # number reporting on each outbreak (WA = 858)
table(df$ebola_species) # number assigned to each species (Zaire = 1123 + 3 + 5 + 5 = 1136)
param_ids <- unique(df$covidence_id)
model_ids <- unique(df_models$covidence_id)
all_ids <- c(unique(df$covidence_id), unique(df_models$covidence_id))
length(unique(all_ids)) # total papers 522

articles <- articles %>% mutate(
  parameter_reported = case_when(covidence_id %in% param_ids ~ 1, TRUE ~ NA),
  model_reported = case_when(covidence_id %in% model_ids ~ 1, TRUE ~ NA),
  both_reported = case_when(parameter_reported %in% 1 & model_reported %in% 1 ~ 1, TRUE ~ NA),
  parameter_only = case_when(parameter_reported %in% 1 & is.na(model_reported) ~ 1, TRUE ~ NA),
  model_only = case_when(model_reported %in% 1 & is.na(parameter_reported) ~ 1, TRUE ~ NA)
)

articles %>% filter(parameter_reported %in% 1) %>% nrow() # 354 articles
articles %>% filter(model_reported %in% 1) %>% nrow() # 280 articles

# Should all sum to 522
articles %>% filter(both_reported %in% 1) %>% nrow() # 112 articles
articles %>% filter(parameter_only %in% 1) %>% nrow() # 242 articles
articles %>% filter(model_only %in% 1) %>% nrow() # 168 articles

###################################
# Summary table of all parameters #
###################################

summary_dat <- df %>%
  mutate(
    parameter_type =
      case_when(
        parameter_type %in% "Reproduction number (Basic R0)" ~
          "Basic reproduction number (R0)",
        parameter_type %in% "Reproduction number (Effective, Re)" ~
          "Effective reproduction number (Re)",
        parameter_type %in% "Severity - case fatality rate (CFR)" ~
          "Case Fatality Ratio (CFR)",
        parameter_class %in% "Human delay" ~ delay_short,
        TRUE ~ parameter_type
      ),
    parameter_class =
      case_when(parameter_class %in% "Human delay" ~ "Delay",
                TRUE ~ parameter_class)
  ) %>%
  filter(!parameter_from_figure %in% TRUE)

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

save_as_image(group_tab, path = "Summary_results/parameter_group_table.png")

# Parameter type table
set_flextable_defaults(background.color = "white", na.string = "")
param_tab <- summary_dat %>%
  mutate(
    parameter_type =
      case_when(
        parameter_class %in% "Attack rate" & attack_rate_type %in% "Secondary" ~
          "Secondary attack rate",
        TRUE ~ parameter_type)
      ) %>%
  group_by(parameter_class, parameter_type) %>%
  summarise(count = n()) %>%
  expandRows(., "count", drop = FALSE) %>%
  mutate(
    # Remove parameter class names from parameter type names for consistency
    parameter_type = gsub("Seroprevalence - ", "", parameter_type),
    parameter_type = gsub("Mutations - ", "", parameter_type),
    parameter_type = gsub("Mutations – ", "", parameter_type),
    parameter_type = gsub("^([a-z])", "\\U\\1", parameter_type, perl = TRUE),
    parameter_type =
      case_when(
        parameter_class %in% "Mutations" ~ gsub("^([a-z])", "\\U\\1", parameter_type, perl = TRUE),
        parameter_class %in% "Delay" & count == 1 ~ "Other delay*",
      TRUE ~ parameter_type),
    # order by total in parameter class
    parameter_class = factor(parameter_class,
      levels = c("Delay", "Risk factors", "Reproduction number", "Severity",
                 "Seroprevalence", "Mutations", "Attack rate", "Overdispersion",
                 "Growth rate", "Doubling time")
      )
    ) %>%
  select(-count) %>%
  group_by(parameter_class, parameter_type) %>%
  summarise(count = n()) %>%
  select(
    `Parameter Group` = parameter_class,
    `Parameter Type` = parameter_type,
    `Total Parameters` = count,
  ) %>%
  ungroup() %>%
  group_by(`Parameter Group`) %>%
  arrange(`Parameter Group`, desc(`Total Parameters`)) %>%
  mutate(
    index_of_change = row_number(),
    index_of_change = ifelse(
      index_of_change == max(index_of_change), 1, 0
    )
  ) %>%
  as_grouped_data(groups = "Parameter Group") %>%
  as_flextable(col_keys = c("Parameter Type", "Total Parameters"), hide_grouplabel = TRUE
  ) %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  hline(i = ~ !is.na(`Parameter Group`)) %>%
  hline(i = ~ index_of_change == 1) %>%
  bold(j = 1, i = ~ !is.na(`Parameter Group`), bold = TRUE, part = "body" ) %>%
  border_inner_h(part = "header") %>%
  align(align = "left", part = "all") %>%
  align_nottext_col(align = "center") %>%
  add_footer_lines("") %>%
  line_spacing(i = 1, space = 1.5, part = "header")

save_as_image(param_tab, path = "Summary_results/parameter_type_table.png")

# Summary of QA scores
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

# Article summary
summary_table <- tibble(
  "Year < 1991" = articles %>% filter(year_publication < 1991) %>% nrow(),
  "Year 1991-2013" = articles %>% filter(year_publication >= 1991 & year_publication <= 2013) %>% nrow(),
  "Year >= 2014" = articles %>% filter(year_publication >= 2014) %>% nrow(),
  "QA Score < 50" = articles %>% filter(article_qa_score < 50) %>% nrow(),
  "QA Score >= 50" = articles %>% filter(article_qa_score >= 50) %>% nrow()
)

ggplot(articles, aes(x = year_publication, y = article_qa_score)) +
  geom_point() +
  geom_hline(yintercept = 50) +
  geom_vline(xintercept = 1991) +
  geom_vline(xintercept = 2014)

post2014 <- articles %>% filter(year_publication >=2014)

ggplot(post2014, aes(x = article_qa_score)) +
  geom_histogram(binwidth = 10)

post2014 %>% filter(article_qa_score <50) %>% nrow() # 123
post2014 %>% filter(article_qa_score >=50) %>% nrow() # 345


###############################
# Summary csv of all articles #
###############################

# Add Journal and Year of publication to the article label
# so that people can find it even if there is no DOI
articles$article_title <- paste0(
  articles$article_title, 
  " (", articles$journal, ", ", articles$year_publication, ")"
)
# And make case consistent - sentence case
articles$article_title <- str_to_title(articles$article_title)
# remove \r\n
articles$article_title <- gsub("\r\n", " ", articles$article_title)

## Format the doi as a URL
## Some have the word "doi" in them, some don't
articles$doi <- gsub("doi:", "", articles$doi, ignore.case = TRUE)
## Remove leading and trailing whitespace
articles$doi <- trimws(articles$doi)
articles$doi[!is.na(articles$doi)] <- paste0("https://doi.org/", articles$doi[!is.na(articles$doi)])
## If the doi is NA, leave it blank
articles$doi[is.na(articles$doi)] <- ""
## From params, we want: id, parameter_type, ebola_species, delay_short, parameter_class
cols <- c("id", "covidence_id", "parameter_type", "delay_short", "ebola_species", "parameter_class")
cols <- intersect(cols, colnames(params)) ## for other pathogens
params <- select(params, all_of(cols))

# Clean up some parameter names
params <- params %>%
  mutate(
    parameter_type =
      str_to_sentence(str_replace(parameter_type, "Mutations - |Mutations – ", "")),
    parameter_type =
      str_replace_all(parameter_type, c("igg" = "IgG", "igm" = "IgM", "ifa" = "IFA")),
    parameter_type =
      case_when(parameter_class %in% "Human delay" ~ paste0("Delay - ", delay_short),
                parameter_class %in% "Severity" ~ "Case Fatality Ratio",
                parameter_type %in% "Reproduction number (effective, re)" ~ "Effective reproduction number",
                parameter_type %in% "Reproduction number (basic r0)" ~ "Basic reproduction number",
                parameter_type %in% "Growth rate (r)" ~ "Growth rate",
    TRUE ~ parameter_type)
  )

## We will now go the other way, and find out what has been
## extracted from each article. That will make it easier to keep track
## of the number of articles.

out <- map_dfr(
  articles$covidence_id, function(id) {
    p <- params[params$covidence_id %in% id, cols]
    params_extrctd <- paste(unique(p$parameter_type), collapse = ", ")
    m <- models[models$covidence_id %in% id, ]
    model_extrctd <- ifelse(nrow(m) > 0, "Yes", "No")
    a <- articles[articles$covidence_id %in% id, ]
    data.frame(
      `Article` = a$article_label,
      `Title` = a$article_title,
      DOI = a$doi,
      `Parameters Extracted` = params_extrctd,
      `Model Extracted` = model_extrctd
    )
  }
)

## Alphabetically sort the articles
out <- arrange(out, `Article`)

## Add a tab with the included articles exported from Covidence
cov_dat <- read_csv("covidence.csv")
# Removing study as this doesn't match the article labels we created
# Removing Ref as this is blank
cov_dat <- cov_dat %>% select(-c("Notes", "Tags", "Study", "Ref"))

write_xlsx(list(Sheet1 = out, Sheet2 = cov_dat), "Summary_results/all_articles.xlsx")
