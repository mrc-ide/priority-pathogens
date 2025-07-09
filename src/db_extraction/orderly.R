library(dplyr)
library(orderly2)

orderly_parameters(pathogen = NULL)

orderly_artefact(
  "Merged data as csv and errors as RDS",
  c(
    "single_extraction_articles.csv", "single_extraction_models.csv",
    "single_extraction_params.csv", "single_extraction_outbreaks.csv",
    "double_extraction_articles.csv", "double_extraction_models.csv",
    "double_extraction_params.csv", "double_extraction_outbreaks.csv",
    "errors.rds"
  )
)

orderly_dependency("db_extraction_prep",
                   "latest(parameter:pathogen == this:pathogen)",
                   "extracted_tables.rds"
)

orderly_shared_resource("validation.R" = "validation.R")
source("validation.R")

extracted_tables_list <- readRDS("extracted_tables.rds")

articles <- extracted_tables_list[["articles"]]
models <- extracted_tables_list[["models"]]
params <- extracted_tables_list[["parameters"]]

outbreaks_ex <- ("outbreaks" %in% names(extracted_tables_list))

if (outbreaks_ex) {
  outbreaks <- extracted_tables_list[["outbreaks"]]
  
}

## Check data after pathogen-specific cleaning
a_err <- validate_articles(articles)
m_err <- validate_models(models)
p_err <- validate_params(params)

if (outbreaks_ex) {
  o_err <- validate_outbreaks(outbreaks)
} else {
  o_err <- NULL
}

saveRDS(
  list(
    articles_errors = a_err,
    models_errors = m_err,
    params_errors = p_err,
    outbreak_errors = o_err
  ),
  "errors.rds"
)

## Write each DB to the same file now.
double_articles <- count(articles, Covidence_ID) %>% filter(n >= 2)
double_a <- articles[articles$Covidence_ID %in% double_articles$Covidence_ID, ]
single_a <- articles[!articles$Covidence_ID %in% double_articles$Covidence_ID, ]

double_m <- models[models$Covidence_ID %in% double_articles$Covidence_ID, ]
single_m <- models[!models$Covidence_ID %in% double_articles$Covidence_ID, ]

double_p <- params[params$Covidence_ID %in% double_articles$Covidence_ID, ]
single_p <- params[!params$Covidence_ID %in% double_articles$Covidence_ID, ]

if (outbreaks_ex) {
  # this will be empty for Lassa
  double_o <- outbreaks[outbreaks$Covidence_ID %in% double_articles$Covidence_ID, ]
  single_o <- outbreaks[!outbreaks$Covidence_ID %in% double_articles$Covidence_ID, ]
} else {
  single_o <- data.frame()
  double_o <- data.frame()
}


write_csv(
  double_a, "double_extraction_articles.csv"
)
write_csv(
  double_m, "double_extraction_models.csv"
)
write_csv(
  double_p, "double_extraction_params.csv"
)

write_csv(
  single_a, "single_extraction_articles.csv"
)
write_csv(
  single_m, "single_extraction_models.csv"
)
write_csv(
  single_p, "single_extraction_params.csv"
)

write_csv(
  single_o, "single_extraction_outbreaks.csv"
)

write_csv(
  double_o, "double_extraction_outbreaks.csv"
)