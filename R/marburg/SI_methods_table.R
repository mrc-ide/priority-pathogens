# Creation of a table for SI Methods section 
source('R/data_cleaning.R')
library(ggplot2)
library(dplyr)
library(flextable)
library(scales)
library(stringr)
library(gt)
library(tm)

# Pull in data for dropdowns
p <- read.csv('data/marburg/access_db_dropdown_parameters.csv')
m <- read.csv('data/marburg/access_db_dropdown_models.csv')
o <- read.csv('data/marburg/access_db_dropdown_outbreaks.csv') 

parameters <- import('data/marburg/final/parameter_final.csv') %>% 
  select(-name_data_entry, -double_extracted, -covidence_id, -Uncertainty, -c(`Survey year`))
outbreaks <- import('data/marburg/final/outbreak_final.csv')%>%
  select(-outbreak_size, -cases_severe_hospitalised, -name_data_entry, -double_extracted, -covidence_id)
models <- import('data/marburg/final/model_final.csv') %>%
  select(-name_data_entry, -double_extracted, -covidence_id) %>%
  

# Outbreaks table ----
o_latex <- tibble(`Data field` = c('Outbreak ID',
                                     'Article ID',
                                     'Outbreak start day',
                                     'Outbreak start month',
                                     'Outbreak start year',
                                     'Outbreak end day',
                                     'Outbreak end month',
                                     'Outbreak end year',
                                     'Duration (months)',
                                     'Asymptomatic transmission described',
                                     'Outbreak country',
                                     'Outbreak location',
                                     'Cases confirmed',
                                     'Mode of detection of cases',
                                     'Cases suspected',
                                     'Asymptomatic cases',
                                     'Deaths'),
                    `Expected data type` = sapply(outbreaks, class),
                    `Variable name` = names(outbreaks),
                    `Notes` = c('ID assigned by database',
                                'ID to connect to article form',
                                'Day of outbreak start if reported',
                                'Month of outbreak start if reported',
                                'Year of outbreak start if reported',
                                'Day of outbreak end if reported',
                                'Month of outbreak end if reported',
                                'Year of outbreak end if reported',
                                'Duration of outbreak in months, if reported. No calculation of duration is done.',
                                'Tick box whether asymptomatic transmission is described in the paper or not.',
                                'Country or countries where the outbreak took place - see dropdown list',
                                'Region/district/province/city where the outbreak took place',
                                'Number of confirmed cases as reported',
                                'Method for case detection - see dropdown list',
                                'Number of suspected cases as reported',
                                'Number of asymptomatic cases as reported',
                                'Number of deaths as reported')) %>%
  gt() %>% 
  as_latex() %>% as.character()

writeLines(o_latex, paste0("data/marburg/output/outbreaks_latex_db.txt"))



# Model table -------------------------------------------------------------
m_latex <- tibble(`Data field` = c('Model data ID',
                                   'Article ID',
                                   'Model type',
                                   'Compartmental type',
                                   'Stochastic or deterministic',
                                   'Theoretical model',
                                   'Intervention type',
                                   'Code available',
                                   'Transmission route',
                                   'Assumptions'),
                  `Expected data type` = sapply(models, class),
                  `Variable name` = names(models),
                  `Notes` = c('ID assigned by database',
                              'ID to connect to article form',
                              'General type of model - see dropdown list',
                              'Specific type of compartmental model - see dropdown list',
                              'Stochastic or deterministic model as reported',
                              'Tick box whether the model was fitted to data (NA) or just theoretical (TRUE)',
                              'Type of intervention(s) modelled - see dropdown list',
                              'Tick box whether code for model was publicly available and reported in the paper',
                              'Transmission route(s) modelled - see dropdown list',
                              'General assumptions for the model - see dropdown list')) %>%
  gt() %>% 
  as_latex() %>% as.character()

writeLines(m_latex, paste0("data/marburg/output/models_latex_db.txt"))


# Parameter table ---------------------------------------------------------

p_latex <- tibble(`Data field` = c('Parameter data ID',
                                   'Article ID',
                                   'Parameter type',
                                   'Parameter value',
                                   'Parameter unit',
                                   'Parameter lower bound',
                                   'Parameter upper bound',
                                   'Parameter value type',
                                   'Parameter uncertainty - single value',
                                   'Parameter uncertainty - single type',
                                   'Parameter uncertainty - lower value',
                                   'Parameter uncertainty - upper value',
                                   'Parameter uncertainty paired type',
                                   'Numerator',
                                   'Denominator',
                                   'Distribution type',
                                   'First distribution parameter value',
                                   'First distribution parameter type',
                                   'First distribution parameter uncertainty',
                                   'Second distribution parameter value',
                                   'Second distribution parameter type',
                                   'Second distribution parameter uncertainty',
                                   'Is parameter from supplement?',
                                   'Survey timing related to outbreak',
                                   'Is the CFR/IFR estimate adjusted?',
                                   'Method to estimate R',
                                   'Parameter estimates disaggregated by',
                                   'Disaggregated data available',
                                   'Only disaggregated data available',
                                   'Outcome for risk factor(s)',
                                   'Risk factor name',
                                   'Risk factor occupation',
                                   'Risk factor significant',
                                   'Risk factor adjusted',
                                   'Sex of study population',
                                   'Population sample setting',
                                   'Population group',
                                   'Study population minimum age (years)',
                                   'Study population maximum age (years)',
                                   'Study population sample size',
                                   'Study population country',
                                   'Study population location',
                                   'Start day of study',
                                   'Start month of study',
                                   'Start year of study',
                                   'End day of study',
                                   'End month of study',
                                   'End year of study',
                                   'Genome site',
                                   'Genomic sequence available?',
                                   'Parameter class'),
                  `Expected data type` = sapply(parameters, class),
                  `Variable name` = names(parameters),
                  `Notes` = c('ID assigned by database',
                              'ID to connect to article form',
                              'Category of parameter - see dropdown list',
                              'Central parameter value',
                              'Units for parameter value, applies to central estimate and ranges/uncertainty intervals - see dropdown list',
                              'Lower bound of the parameter range if a range was reported or if data are disaggregated',
                              'Upper bound of the parameter range if a range was reported or if data are disaggregated',
                              'Type of central parameter value - see dropdown list',
                              'Value for uncertainty for central parameter value if a single value was reported (e.g. value of std. dev.)',
                              'Type of uncertainty fpr central parameter value if single value was reported - see dropdown list',
                              'Lower bound for uncertainty for central parameter value if paired values were reported',
                              'Upper bound for uncertainty for central parameter value if paired values were reported',
                              'Type of uncertainty for central parameter value if paired values were reported - see dropdown list',
                              'Numerator of either CFR/IFR (deaths) or seroprevalence (number seropositive) estimates',
                              'Denominator of either CFR/IFR (cases) or seroprevalence (number tested) estimates',
                              'Type of distribution for estimated parameter - see dropdown list',
                              'Value for first distribution parameter (e.g. shape or scale parameter for a gamma distribution)',
                              'Type of value for first distribution parameter - see dropdown list',
                              'Tick box for whether uncertainty is estimated for the first distribution parameter (TRUE) or not (FALSE)',
                              'Value for second distribution parameter (e.g. shape or scale parameter for a gamma distribution)',
                              'Type of value for second distribution parameter - see dropdown list',
                              'Tick box for whether uncertainty is estimated for the second distribution parameter (TRUE) or not (FALSE)',
                              'Tick box for whether parameter was extracted from supplement (TRUE) or not (FALSE)',
                              'Timing of the survey in relation to the outbreak, if specified in paper - see dropdown list',
                              'Is the CFR/IFR estimate adjusted, unadjusted, or unspecified - see dropdown list',
                              'Method used for estimation of the reproduction number - see dropdown list',
                              'Categories for disaggregation of parameter estimates',
                              'Tick box if disaggregated estimates are available (TRUE) or not (FALSE)',
                              'Tick box if ONLY disaggregated estimates are available (TRUE) or if a central estimate is also available (FALSE)',
                              'Outcome for risk factor(s) - see dropdown list',
                              'Risk factor name - see dropdown list',
                              'If risk factor is an occupation, then specified occupation as risk factor - see dropdown list',
                              'Statistical significance of risk factor(s) - see dropdown list',
                              'Adjustment status of risk factor(s)- see dropdown list',
                              'Sex of survey population - see dropdown list',
                              'General setting of the survey - see dropdown list',
                              'Specific group of the survey population - see dropdown list',
                              'Minimum age of the survey population in years',
                              'Maximum age of the survey population in years',
                              'Sample size of the population used for parameter estimation',
                              'Country of the survey population - see dropdown list',
                              'Region/district/province/city of the survey population - see dropdown list',
                              'Study start day',
                              'Study start month - see dropdown list',
                              'Study start year - see dropdown list',
                              'Study end day',
                              'Study end month - see dropdown list',
                              'Study end year - see dropdown list',
                              'Site of genome or gene studied',
                              'Tick box whether genomic sequence data are available (TRUE) or not (FALSE)',
                              'General parameter class (delays, seroprevalence, reproduction numbers, mutations, severity, risk factors, relative contribution)')) %>%
  gt() %>% 
  as_latex() %>% as.character()

writeLines(p_latex, paste0("data/marburg/output/parameters_latex_db.txt"))
