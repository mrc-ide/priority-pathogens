orderly2::orderly_run(name = 'zika_compilation', parameters = list(pathogen ='ZIKA'))

parameters <- readRDS("P:/Zika/priority-pathogens/archive/zika_compilation/20240711-150144-015934c9/parameters.rds")
table(parameters$parameter_type)

# sero, attack rate, incubation period, R, risk factors 

sero <- parameters %>% 
  filter(parameter_type %in% c('Seroprevalence - HAI/HI','Seroprevalence - IgG','Seroprevalence - Unspecified',
                             'Seroprevalence - IgM','Seroprevalence - Neutralisation/PRNT'))
table(sero$population_group)
table(sero$population_sample_type)
table(as.numeric(sero$parameter_value))
table(sero$parameter_value_type)
table(sero$population_country)

ar <- param_double %>%
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



ggplot(pars[which(pars$parameter_type == 'Attack rate' & pars$parameter_unit == 'Percentage (%)'),]) + 
  geom_point(aes(y = as.factor(covidence_id), x = parameter_value)) 


ggplot(pars[which(pars$parameter_type == 'Attack rate' & 
                    pars$parameter_unit == 'Percentage (%)' &
                    pars$population_group == 'General population'),]) + 
  geom_point(aes(y = as.factor(covidence_id), x = parameter_value)) 


ggplot(pars[which(pars$parameter_type == 'Attack rate' & 
                    pars$parameter_unit == 'Percentage (%)' &
                    pars$population_group == 'Persons under investigation'),]) + 
  geom_point(aes(y = as.factor(covidence_id), x = parameter_value)) 
