orov_cleaning_articles <- function(df) {
  df <- df %>% 
    # dummy entry but marked as OROV 
    filter(covidence_id!="999999",
           # duplicate entries for 30 and 109 - the ID gets regenerated each run so not applicable for filtering
           #id!="6e85a97a-c427-4eef-b0e8-f55142802ef1",
           #id!="23d7b6a2-bbbe-4f94-984f-da9fd0bab2d5"
           !(article_id %in% c(104,59))
           )
}


orov_cleaning_parameters <- function(df){
  
  df <- df %>% mutate(
    parameter_type_broad = factor(case_when(
      grepl("Infection prevalence",parameter_type) ~ "Infection prevalence",
      parameter_type=="Attack rate" ~ "Attack rate",
      grepl("Human delay",parameter_type) ~ "Human delay",
      grepl("Mutations",parameter_type) ~ "Genomic parameters",
      parameter_type=="Risk factors" ~ "Risk factors",
      grepl("Seroprevalence",parameter_type) ~ "Seroprevalence",
      grepl("Severity",parameter_type) ~ "Severity"
    )),
    parameter_context_human = case_when(
      covidence_id==65&is.na(parameter_context_human) ~ "Human",
      covidence_id==29&is.na(parameter_context_human) ~ "Both",
      !is.na(parameter_context_human) ~ parameter_context_human
    ),
    riskfactor_outcome = case_when(
      riskfactor_outcome == "Presence of Oropouch [Oropouche ONLY]" ~ "Presence of Oropouche",
      riskfactor_outcome != "Presence of Oropouch [Oropouche ONLY]" ~ riskfactor_outcome
      ),
    parameter_statistical = case_when(
      access_param_id %in% c(330,365,368,379) ~ "Observed sample statistic",
      access_param_id==321 ~ "Case study [Oropouche ONLY]",
      !(access_param_id %in% c(321,330,365,368,379)) ~ parameter_statistical
    ),
    population_sample_type = case_when(
      parameter_context_human=="Not human" ~ "Non-human",
      parameter_context_human!="Not human" ~ population_sample_type
    ),
    population_group = case_when(
      parameter_context_human=="Not human" ~ "Non-human",
      parameter_context_human!="Not human" ~ population_group
    )
    ) %>% filter(
      !(access_param_id %in% c(147,149))
    )

}


## sort anything that says OROV only


# # get rid of human/non-human NAs
# parameters$parameter_context_human[which(is.na(parameters$parameter_context_human)&parameters$covidence_id=="65")] <- "Human"
# parameters$parameter_context_human[which(is.na(parameters$parameter_context_human)&parameters$covidence_id=="29")] <- "Both"

# risk factors 
# cov id 136
# adjusted and significant can stay 
# not adjusted & sig - age, occupation, sex, other
# not adjusted & not sig - occupation, other
## remove: other, not sig, not adj - 147
## remove: age, sex, sig, not adj - 149

