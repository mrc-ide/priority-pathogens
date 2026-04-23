## OROPOUCHE ONLY TASK
# Task to read in databases (replacing redcap workflow)
library(tidyverse)
library(orderly2)
library(readxl)

orderly_strict_mode()

## pathogen should be set to one of our priority-pathogens
## use capital case
## orderly_parameters(pathogen = 'EBOLA')
orderly_parameters(pathogen = "OROV")

orderly_resource(
  c(## data files
    "inputs/articles_orov.csv",
    "inputs/outbreaks_orov.xlsx",
    "inputs/parameters_orov.xlsx"
  )
)

orderly_artefact(
  description = "Extracted data ready for analysis",
  files = c(
    "articles.csv",
    "parameters.csv",
    "outbreaks.csv"
  ))

## read in the data 
articles <- read.csv("inputs/articles_orov.csv")
outbreaks <- readxl::read_excel("inputs/outbreaks_orov.xlsx")
parameters <- readxl::read_excel("inputs/parameters_orov.xlsx")
## placeholder for genomics 

## note that the cleaning function was already applied before so do not need to update again now

# # quick check that the articles match
# po_ids <- c(parameters$covidence_id,outbreaks$covidence_id) %>% unique()
# articles$covidence_id[which(!(articles$covidence_id %in% po_ids))]

# recheck the qa score
# check that have labels and qa scores integrated

# redo QA score
articles <- articles %>% 
  mutate(# clean
  qa_m1 = case_when(qa_m1=="Not Applicable" ~ NA,
                    .default = qa_m1),
  qa_m2 = case_when(qa_m2=="Not Applicable" ~ NA,
                    .default = qa_m2),
  qa_a3 = case_when(qa_a3=="Not Applicable" ~ NA,
                    .default = qa_a3),
  qa_a4 = case_when(qa_a4=="Not Applicable" ~ NA,
                    .default = qa_a4),
  qa_d5 = case_when(qa_d5=="Not Applicable" ~ NA,
                    .default = qa_d5),
  qa_d6 = case_when(qa_d6=="Not Applicable" ~ NA,
                    .default = qa_d6),
  qa_d7 = case_when(qa_d7=="Not Applicable" ~ NA,
                    .default = qa_d7)) %>%
  mutate(# calculate QA
    total_qa =
      rowSums(!is.na(
        select(., qa_m1, qa_m2, qa_a3, qa_a4, qa_d5, qa_d6, qa_d7))),
    yes_score = rowSums(
      select(., qa_m1, qa_m2, qa_a3, qa_a4, qa_d5, qa_d6, qa_d7) == "Yes",
      na.rm = TRUE
    ),
    qa_score = ifelse(total_qa > 0, yes_score / total_qa * 100, NA)
  ) %>%
  select(-c(total_qa, yes_score))

# fix article labels
length(which(is.na(articles$article_label)))

# add this into parameters and outbreaks 
qa_scores_and_labels <- articles %>% select(covidence_id,article_label,article_qa_score)

outbreaks <- left_join(outbreaks,qa_scores_and_labels)
parameters <- left_join(parameters,qa_scores_and_labels)

# outbreak ID 
outbreaks$outbreak_id[is.na(outbreaks$outbreak_id)] <- seq(
  from = max(outbreaks$outbreak_id,na.rm=TRUE) + 1,
  length.out = length(which(is.na(outbreaks$outbreak_id))),
  by=1)

# parameter ID and other admin columns 
parameters$access_param_id[is.na(parameters$access_param_id)] <- seq(
  from = max(parameters$access_param_id,na.rm=TRUE) + 1,
  length.out = length(which(is.na(parameters$access_param_id))),
  by=1)

# add parameter type
parameters$parameter_type %>% unique() 

parameters <- parameters %>% mutate(
  parameter_type_broad = factor(case_when(
    grepl("Infection prevalence",parameter_type) ~ "Infection prevalence",
    parameter_type=="Attack rate" ~ "Attack rate",
    grepl("delay",parameter_type) ~ "Delays",
    grepl("Mutations",parameter_type) ~ "Genomic parameters",
    parameter_type=="Risk factors" ~ "Risk factors",
    grepl("Seroprevalence",parameter_type) ~ "Seroprevalence",
    grepl("Severity",parameter_type) ~ "Severity",
    grepl("Reproduction",parameter_type) ~ "Transmissibility",
    parameter_type=="Delay - human to mosquito generation time" ~ "Delays",
    parameter_type=="Delay - mosquito to human generation time" ~ "Delays"
    )
  )
)

# save outputs 
# write.csv(x = articles,file = "outputs/articles.csv",row.names=FALSE)
# write.csv(x = outbreaks,file = "outputs/outbreaks.csv",row.names=FALSE)
# write.csv(parameters,"outputs/parameters.csv",row.names=FALSE)

write_csv(articles, "articles.csv")
write_csv(outbreaks, "outbreaks.csv")
write_csv(parameters, "parameters.csv")
