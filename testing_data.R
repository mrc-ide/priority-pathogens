orderly2::orderly_run(name = 'zika_compilation', parameters = list(pathogen ='ZIKA'))
library(ggplot2)
library(dplyr)

parameters <- readRDS("P:/Zika/priority-pathogens/archive/zika_compilation/20240801-092947-7c5e9a82/parameters.rds")
outbreaks <- readRDS("P:/Zika/priority-pathogens/archive/zika_compilation/20240801-092947-7c5e9a82/outbreaks.rds")
table(parameters$parameter_type)

# sero, attack rate, incubation period, R, risk factors 

sero <- parameters %>% 
  filter(parameter_type %in% c('Seroprevalence - HAI/HI','Seroprevalence - IgG','Seroprevalence - Unspecified',
                             'Seroprevalence - IgM','Seroprevalence - Neutralisation/PRNT') & population_group == 'General population' & 
                       population_sample_type =='Population based') %>%
  select(parameter_type, population_group, covidence_id, population_country, parameter_value)
table(sero$population_group)
table(sero$population_sample_type)
table(as.numeric(sero$parameter_value))
table(sero$parameter_value_type)
table(sero$population_country)

ar <- parameters %>%
  filter(parameter_type == 'Attack rate')


table(outbreak_double$outbreak_country)
table(outbreak_double$cases_confirmed)


# Check that central estimate is within ranges / CrI / CI
names(parameters)

pars <- parameters %>%
  mutate(across(.cols = where(is.character), .fns = ~dplyr::na_if(.x, "NA"))) %>%
  mutate(parameter_value = as.numeric(parameter_value),
         parameter_lower_bound = as.numeric(parameter_lower_bound),
         parameter_upper_bound = as.numeric(parameter_upper_bound),
         parameter_uncertainty_lower_value = as.numeric(parameter_uncertainty_lower_value),
         parameter_uncertainty_upper_value = as.numeric(parameter_uncertainty_upper_value)) %>%
  # filter to parameters we are interested in for now (for IDM abstract)
  filter(parameter_type %in% c('Seroprevalence - HAI/HI','Seroprevalence - IgG',#'Seroprevalence - Unspecified',
                               'Seroprevalence - IgM','Seroprevalence - Neutralisation/PRNT', 
                               'Reproduction number (Basic R0)','Reproduction number (Effective; Re)',
                               'Reproduction number (Basic R0) - Human', 'Reproduction number (Effective; Re) - Human',
                               'Reproduction number (Basic R0) - Mosquito',
                               'Attack rate')) %>%
  mutate(central_ok = case_when(!is.na(parameter_value) & (!is.na(parameter_lower_bound) & !is.na(parameter_upper_bound)) & # range over disaggregation
                                parameter_value >= parameter_lower_bound & parameter_value <= parameter_upper_bound ~ 1,
                                !is.na(parameter_value) & (!is.na(parameter_uncertainty_lower_value) & !is.na(parameter_uncertainty_upper_value)) & # CrI, CIs
                                parameter_value >= parameter_uncertainty_lower_value & parameter_value <= parameter_uncertainty_upper_value ~ 1,
                                is.na(parameter_value) ~ NA)) %>%
  group_by(parameter_type) %>%
  mutate(max_value = max(parameter_value, na.rm = TRUE))

table(pars$central_ok)

pars %>% select(parameter_value, parameter_lower_bound, parameter_upper_bound, parameter_uncertainty_lower_value, 
                parameter_uncertainty_upper_value, central_ok, max_value) %>% View()

# Quick plot of attack rate 
table(pars[which(pars$parameter_type == 'Attack rate'),]$parameter_unit)
table(pars[which(pars$parameter_type == 'Attack rate'),]$population_group)


pars <- parameters

ggplot(pars[which(pars$parameter_type == 'Attack rate' & pars$parameter_unit == 'Percentage (%)'),]) + 
  geom_point(aes(y = as.factor(covidence_id), x = as.numeric(parameter_value)) )
ars <- ar %>%
  filter(parameter_unit == 'Percentage (%)') %>%
  select(parameter_value, population_country, population_group, population_location)
# ggplot(pars[which(pars$parameter_type == 'Attack rate' & 
#                     pars$parameter_unit == 'Percentage (%)' &
#                     pars$population_group == 'General population'),]) + 
#   geom_point(aes(y = as.factor(covidence_id), x = parameter_value)) 
# 
# 
# ggplot(pars[which(pars$parameter_type == 'Attack rate' & 
#                     pars$parameter_unit == 'Percentage (%)' &
#                     pars$population_group == 'Persons under investigation'),]) + 
#   geom_point(aes(y = as.factor(covidence_id), x = parameter_value)) 

ggplot(pars) + 
  geom_point(aes(x = as.factor(covidence_id), y = parameter_value, color = population_country)) + 
  geom_errorbar(aes(x = as.factor(covidence_id), ymin = parameter_lower_bound, ymax = parameter_upper_bound, color = population_country)) +
  geom_errorbar(aes(x = as.factor(covidence_id), ymin = parameter_uncertainty_lower_value, ymax = parameter_uncertainty_upper_value, color = population_country), 
                linetype = 2) +
  facet_wrap(~parameter_type, scales = 'free_y') + 
  theme(legend.position = 'none')

ggplot(pars %>% filter(parameter_type == 'Seroprevalence - IgG' & population_group == 'General population' & 
                         population_sample_type =='Population based')) + 
  geom_point(aes(x = as.factor(covidence_id), y = parameter_value, color = population_country)) + 
  geom_errorbar(aes(x = as.factor(covidence_id), ymin = parameter_lower_bound, ymax = parameter_upper_bound, color = population_country)) +
  geom_errorbar(aes(x = as.factor(covidence_id), ymin = parameter_uncertainty_lower_value, ymax = parameter_uncertainty_upper_value, color = population_country), 
                linetype = 2) +
  facet_wrap(~parameter_type, scales = 'free_y') + 
  theme(legend.position = 'none')




ggplot(pars %>% filter(parameter_type == 'Reproduction number (Basic R0)' & covidence_id!=1496 )) + 
  geom_point(aes(x = as.factor(covidence_id), y = parameter_value, color = population_country)) + 
  geom_errorbar(aes(x = as.factor(covidence_id), ymin = parameter_lower_bound, ymax = parameter_upper_bound, color = population_country)) +
  geom_errorbar(aes(x = as.factor(covidence_id), ymin = parameter_uncertainty_lower_value, ymax = parameter_uncertainty_upper_value, color = population_country), 
                linetype = 2) +
  facet_wrap(~parameter_type, scales = 'free_y') + 
  theme(legend.position = 'none')
  
outbreaks2 <- outbreaks %>%
  mutate_at(vars(cases_confirmed,cases_suspected, cases_unspecified, cases_severe,cases_asymptomatic),
            ~ na_if(.,'NA')) %>%
  mutate_at(vars(cases_confirmed,cases_suspected, cases_unspecified, cases_severe,cases_asymptomatic),
            ~ as.numeric(.))

ggplot(outbreaks2 %>% filter(cases_suspected < 90000)) + 
  geom_point(aes(x = as.factor(covidence_id), y = cases_confirmed, color = outbreak_country), shape = 1) +
  geom_point(aes(x = as.factor(covidence_id), y = cases_suspected, color = outbreak_country), shape = 2) +
  geom_point(aes(x = as.factor(covidence_id), y = cases_unspecified, color = outbreak_country), shape = 3) +
  geom_point(aes(x = as.factor(covidence_id), y = cases_severe, color = outbreak_country), shape = 4) +
  geom_point(aes(x = as.factor(covidence_id), y = cases_asymptomatic, color = outbreak_country), shape = 5) 


## Outbreaks
ggplot(outbreaks) + 
  geom_point(aes(x = outbreak_date_year, y = as.numeric(cases_confirmed), color = outbreak_country))
