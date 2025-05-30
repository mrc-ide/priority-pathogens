# Task to make genomic plot

library(orderly2)
library(ggplot2)
library(dplyr)
library(patchwork)
library(gridExtra)

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
         sampling_interval = ifelse(sampling_interval == 0, 1, sampling_interval)) %>%
  mutate(lineage = ifelse(is.na(genomic_lineage), 'Unspecified', genomic_lineage))

# make plot for only sub/site/year
jitterer <- position_jitter(seed = 123, width = 5)

p1 <- ggplot(genomic %>% filter(qa_score >= 0.5)) +
  geom_segment(aes(y = parameter_lower_bound, yend = parameter_upper_bound,
                   x = sampling_interval),#, yend = sampling_interval),
               position = jitterer,
               linewidth = 3, alpha = 0.65, color = 'lightblue') +
  geom_errorbar(aes(ymin=parameter_uncertainty_lower_value, ymax=parameter_uncertainty_upper_value,
                    y = central,x = sampling_interval, color = parameter_uncertainty_type),
                position = jitterer,
                width = 1.3, lwd=0.75, alpha = 1) +
  geom_point(aes(x = sampling_interval, y = central, shape = parameter_value_type), 
             position = jitterer,
             size = 2) + 
  scale_y_log10()+
  scale_color_discrete(na.translate = F) +
  theme_bw(base_size = 14) + 
  facet_wrap(~parameter_type, scales = 'free_y') +
  labs(x = 'Sampling interval (years)',
       y = 'Substitutions/site/year',
       color = '',
       shape = '')

ggsave(filename = 'zika_genomic.png', plot = p1, height = 4, width = 12)




noqa_nomut <- ggplot(genomic %>% filter(parameter_type != 'Mutation Rate\n(mutations/site/generation)')) +
  geom_segment(aes(y = parameter_lower_bound, yend = parameter_upper_bound,
                   x = sampling_interval),
               position = jitterer,
               linewidth = 3, alpha = 0.65, color = 'lightblue') +
  geom_errorbar(aes(ymin=parameter_uncertainty_lower_value, ymax=parameter_uncertainty_upper_value,
                    y = central,x = sampling_interval, color = parameter_uncertainty_type),
                position = jitterer,
                width = 4, lwd=0.75, alpha = 1) +
  geom_point(aes(x = sampling_interval, y = central, shape = parameter_value_type),
             position = jitterer,
             size = 2) +
  scale_y_log10()+
  scale_color_discrete(na.translate = F) +
  theme_bw(base_size = 14) +
  facet_wrap(~parameter_type, scales = 'free_y') +
  labs(x = 'Sampling interval (years)',
       y = 'Substitutions/site/year',
       color = '',
       shape = '')

# mutation rate plot
mutationplot <- ggplot(genomic %>% filter(parameter_type == 'Mutation Rate\n(mutations/site/generation)') %>%
                         mutate(parameter_type = 'Mutation rate')) +
  geom_errorbar(aes(ymin=parameter_uncertainty_lower_value, ymax=parameter_uncertainty_upper_value,
                    y = central,x = sampling_interval, color = genome_site),
                position = jitterer,
                width = 0.3, lwd=0.75, alpha = 1) +
  geom_point(aes(x = sampling_interval, y = central, color = genome_site), 
             position = jitterer,
             size = 2) + 
  scale_y_log10()+
  scale_color_discrete(na.translate = F) +
  theme_bw(base_size = 14) + 
  facet_wrap(~parameter_type, scales = 'free_y') +
  labs(x = 'Sampling interval (years)',
       y = 'Mutations/site/generation',
       color = '',
       shape = '')
ggsave(filename ='zika_genomic_mut_rate.png', plot = mutationplot, height = 4, width = 4)

# Combine the one without mutation rate and the one with to get all estimates but with mutation rates labeled by genome site
layout <- "AAAABB"
zika_genomic_noqa <- noqa_nomut + mutationplot + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')
ggsave('zika_genomic_noqa.png', zika_genomic_noqa, height = 4, width = 12)

# length(unique(genomic$covidence_id))
# nrow(genomic)
# length(unique(genomic[genomic$qa_score >= 0.5,]$covidence_id))
# nrow(genomic[genomic$qa_score >= 0.5,])
# 
# table(genomic$parameter_type)
# table(genomic[genomic$qa_score >= 0.5,]$parameter_type)
# table(genomic$population_country)
