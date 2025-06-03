# This script is to be run after zika_compilation which provides basic cleaning 
# In this script, we will do general preparation of data for tables and figures 

# It will take the place of the data_curation() function currently housed in lassa_function.R
# as well as the ad-hoc data cleaning in each of the analysis orderly tasks

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL,
                   plotting = NULL) # can be TRUE or FALSE; TRUE outputs plotting-ready dfs
orderly_dependency("zika_compilation", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.rds", "outbreaks.rds", "models.rds", "parameters.rds"))

## Outputs
orderly_artefact(
  description = "Curated and ready to plot or tabulate data as rds and csv",
  files = c(
    "articles_curated.rds",
    "models_curated.rds",
    "parameters_curated.rds",
    "outbreaks_curated.rds",
    "zika_genomic.rds"
  )
)

orderly_resource(c("data_curation.R" = "data_curation.R"))
source("data_curation.R")

###################
## DATA CURATION ##
###################

articles   <- readRDS("articles.rds")
outbreaks  <- readRDS("outbreaks.rds")
models     <- readRDS("models.rds")
parameters <- readRDS("parameters.rds") %>%
  ########################### to remove once this is sorted
  filter(!is.na(parameter_data_id))

dfs <- curation(articles,outbreaks,models,parameters, plotting = plotting)

articles   <- dfs$articles
qa_scores  <- articles %>% dplyr::select(covidence_id,qa_score)

outbreaks  <- dfs$outbreaks

models     <- dfs$models

parameters <- dfs$parameters %>% left_join(qa_scores) %>%
  mutate(article_label = make.unique(refs)) %>%
  mutate(article_label = factor(article_label,levels=rev(unique(article_label))))

# once i add in the extra cleaning in each task, then can remove that from the analsysi tasks as well 
# (Especially the latex tables one)

# Save genomic data 
genomic <- parameters %>%
  filter(parameter_class == 'Mutations') %>%
  left_join(articles_clean %>% select(-name_data_entry), by = c('covidence_id', 'pathogen'))  %>%
  select( -c(starts_with('riskfactor'), r_pathway, seroprevalence_adjusted, third_sample_param_yn,
             contains('delay'), method_2_from_supplement, #starts_with('cfr'), 
             starts_with('distribution'), case_definition, exponent_2, 
             inverse_param, inverse_param_2, name_data_entry, trimester_exposed, starts_with('parameter_2')))

saveRDS(genomic, "zika_genomic.rds")
write_csv(genomic, "zika_genomic.csv")


# save cleaned dfs
saveRDS(articles, 'articles_curated.rds')
saveRDS(models, 'models_curated.rds')
saveRDS(outbreaks, 'outbreaks_curated.rds')
saveRDS(parameters, 'parameters_curated.rds')
write_csv(articles, 'articles_curated.csv')
write_csv(models, 'models_curated.csv')
write_csv(outbreaks, 'outbreaks_curated.csv')
write_csv(parameters, 'parameters_curated.csv')