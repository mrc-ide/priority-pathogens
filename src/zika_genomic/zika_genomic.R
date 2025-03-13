# Task to make genomic plot

library(orderly2)
library(ggplot2)
library(dplyr)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("zika_prep_data", "latest(parameter:pathogen == this:pathogen &&
                   parameter:plotting == TRUE)",
                   c("zika_genomic.rds","articles.rds", "outbreaks.rds", "models.rds", "parameters.rds"))
orderly_artefact(files = 'zika_genomic.png')
genomic <- readRDS('zika_genomic.rds')


# Data preparation 
genomic <- genomic %>%
  mutate(parameter_value_type = case_when(
    parameter_value_type == 'Other' ~'Unspecified',
    is.na(parameter_value_type) ~ 'Unspecified',
    TRUE ~ parameter_value_type)) %>%
  filter(parameter_unit !='Mutations/year') %>%
  mutate(parameter_type = stringr::str_to_title(stringr::str_replace(parameter_type, 'Mutations - ','')),
         parameter_type = ifelse(parameter_type == "Mutation Rate", "Mutation Rate\n(mutations/site/generation)", parameter_type),
         parameter_type = factor(parameter_type, levels = c('Evolutionary Rate', "Substitution Rate", "Mutation Rate\n(mutations/site/generation)")))

###
# plot values of genomic data by sampling interval 

# calculate sampling interval 
genomic <- genomic %>%
  mutate(sampling_interval = population_study_end_year - population_study_start_year,
         sampling_interval = ifelse(sampling_interval == 0, 1, sampling_interval))

# make plot for only sub/site/year

ggplot(genomic) +
  geom_segment(aes(y = parameter_lower_bound, yend = parameter_upper_bound,
                   x = sampling_interval, yend = sampling_interval),
               linewidth = 3, alpha = 0.65, color = 'lightblue') +
  geom_errorbar(data =genomic %>% drop_na(parameter_uncertainty_type), aes(ymin=parameter_uncertainty_lower_value, ymax=parameter_uncertainty_upper_value,
                    x = sampling_interval, color = parameter_uncertainty_type),
                width = 1.3, lwd=0.75, alpha = 1) +
  geom_point(aes(x = sampling_interval, y = central, shape = parameter_value_type), size = 2) + 
  scale_y_log10()+
  theme_bw(base_size = 14) + 
  facet_wrap(~parameter_type, scales = 'free_y') +
  labs(x = 'Sampling interval (years)',
       y = 'Substitutions/site/year',
       color = '',
       shape = '')

ggsave('zika_genomic.png', height = 4, width = 12)

