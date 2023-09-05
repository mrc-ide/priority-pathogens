# Task to compile single and double extraction databases together
library(orderly2)
library(dplyr)
orderly_strict_mode()

## Outputs
orderly_artefact(
  "Merged single and double extracted data as csv",
  c("articles.csv", "models.csv", "parameters.csv"))

# # Once working as an orderly task, we can get inputs from previous tasks
# orderly_dependency("db_extraction", "TASKID",
#   c("single_extraction_articles.csv" = "single_extraction_articles.csv",
#     "single_extraction_parameters.csv" = "single_extraction_parameters.csv",
#     "single_extraction_models.csv" = "single_extraction_models.csv")
# )
# 
# orderly_dependency("db_double", "TASKID",
#                    c("qa_match.csv" = "qa_match.csv")
# )

## Inputs (when set up properly, this will just list the manually fixed files)
orderly_resource(
  c("single_extraction_articles.csv",
    "single_extraction_parameters.csv",
    "single_extraction_models.csv",
    "qa_fixing.csv",
    "parameter_fixing.csv",
    "model_fixing.csv",
    "qa_match.csv"
  )
)

# Single extractions
article_single <- import("articles.csv")
model_single <- import("models.csv")
parameter_single <- import("parameters.csv")
outbreak_single <- NULL

# Double extractions - matched between extractors
qa_matching <- import("qa_match.csv")
model_matching <- NULL
parameter_matching <- NULL
outbreak_matching <- NULL

# Double extractions - needed to be resolved between extractors
qa_fixed <- import("qa_fixing.csv")
model_fixed <- import("model_fixing.csv")
parameter_fixed <- import("parameter_fixing.csv")
outbreak_fixed <- NULL

## create final datasets
# TO DO: Add outbreak_fixed for next pathogen
qa_fixed <- qa_fixed %>%
  dplyr::filter(fixed == TRUE) %>%
  dplyr::select(names(article_single))

parameter_fixed <- parameter_fixed %>%
  dplyr::filter(fixed == TRUE) %>%
  dplyr::select(names(parameter_single))

model_fixed <- model_fixed %>%
  dplyr::filter(fixed == TRUE) %>%
  dplyr::select(names(model_single))

#article_final <- 

parameter_all <- rbind(parameter_single,
                       parameter_matching,
                       parameter_fixed)

model_all <- rbind(model_single,
                   model_matching,
                   model_fixed)

if (! dir.exists(outdir)) dir.create(outdir)
write_csv(parameter_all, "outdir/parameters.csv")
write_csv(model_all, "outdir/models.csv")
write_csv(article_all, "outdir/articles.csv")
