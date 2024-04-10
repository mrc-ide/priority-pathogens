library(meta)
library(metafor)

param_df    <- read.csv("data/marburg/final/parameter_final.csv")
outbreak_df <- read.csv("data/marburg/final/outbreak_final.csv")
article_df  <- read.csv("data/marburg/final/article_clean.csv")
article <- read.csv("data/marburg/final/article_final.csv")

# merge with article ID article labels
df <- merge(param_df, article_df %>% dplyr::select(article_id, first_author_first_name, year_publication),
            all.x = TRUE, by = 'article_id') %>%
  mutate(article_label = as.character(paste0(first_author_first_name, " ", year_publication)),
         population_country = str_replace_all(population_country, ";", ", ")) %>%
  dplyr::arrange(article_label, -year_publication) %>%
  dplyr::filter(article_id %in% c(17,15) == FALSE) %>%
  rename('Survey year' = Survey.year) %>%
  mutate(parameter_uncertainty_lower_value = replace(parameter_uncertainty_lower_value, (parameter_uncertainty_type == "Range" & !is.na(parameter_lower_bound) & parameter_class == "Human delay"), NA),
         parameter_uncertainty_upper_value = replace(parameter_uncertainty_upper_value, (parameter_uncertainty_type=="Range" & !is.na(parameter_upper_bound) & parameter_class == "Human delay"), NA)) %>%
  rowwise() %>% 
  mutate(parameter_uncertainty_lower_value = replace(parameter_uncertainty_lower_value, parameter_data_id == 43, parameter_uncertainty_lower_value * 1e-4),   # need to adjust for scaling
         parameter_uncertainty_upper_value = replace(parameter_uncertainty_upper_value, parameter_data_id == 43, parameter_uncertainty_upper_value * 1e-4)) %>%             # need to adjust for scaling
  mutate(parameter_value = replace(parameter_value, parameter_data_id == 34, 0.93),
         cfr_ifr_method = replace(cfr_ifr_method, str_starts(parameter_type,"Severity") & is.na(cfr_ifr_method), "Unknown")) %>%
  mutate(parameter_value_type = ifelse(parameter_data_id == 16, 'Other', parameter_value_type),
         parameter_value_type = ordered(parameter_value_type, levels = c('Mean','Median','Standard Deviation','Other','Unspecified') )) 

df_out <- merge(outbreak_df, article %>% dplyr::select(covidence_id, first_author_first_name, year_publication),
                all.x = TRUE, by = "covidence_id") %>%
  mutate(article_label = as.character(paste0(first_author_first_name, " ", year_publication)),
         outbreak_country = str_replace_all(outbreak_country, ";", ", ")) %>%
  dplyr::arrange(article_label, -year_publication) %>%
  dplyr::filter(article_id %in% c(17,15) == FALSE) %>%
  filter(!is.na(deaths) & !is.na(cases_confirmed)) %>% 
  mutate(cases_suspected = replace_na(cases_suspected, 0),
         parameter_value = deaths/(cases_confirmed + cases_suspected)*100,
         cfr_ifr_numerator = deaths,
         cfr_ifr_denominator = cases_confirmed + cases_suspected,
         cfr_ifr_method = "Naive",
         parameter_class = "Severity",
         parameter_type = "Severity - case fatality rate (CFR)",
         parameter_uncertainty_lower_value = NA,
         parameter_uncertainty_upper_value = NA,
         outbreak_year_cnt = str_replace(paste0(outbreak_country, " (",outbreak_start_year, ")"), "NA", "unknown") ) %>%
  dplyr::arrange(outbreak_year_cnt) %>% distinct()
df_out$parameter_data_id <- seq(1, dim(df_out)[1], 1)
df_out$keep_record <- c(1,0,0,0,0,1,0,0,1,1,1,1,1,1,1,0,1,0,1,0,1,1)

cfr <- df %>% filter(cfr_ifr_denominator>0 & parameter_class=="Severity")

meta_cfr <- metaprop(cfr_ifr_numerator, cfr_ifr_denominator, studlab=article_label, sm="PLOGIT", data=cfr, method="GLMM", method.tau="ML")
summary(meta_cfr)

forest.meta(meta_cfr, layout="RevMan5", xlab="Proportion", comb.r=T, comb.f=F, xlim = c(0,1), fontsize=10, digits=3)

cfr_outbreak <- df_out %>% filter(keep_record==1)

meta_cfr_outbreak <- metaprop(cfr_ifr_numerator, cfr_ifr_denominator, studlab=article_label, sm="PLOGIT", data=cfr_outbreak, method="GLMM", method.tau="ML")
summary(meta_cfr_outbreak)

forest.meta(meta_cfr_outbreak, layout="RevMan5", xlab="Proportion", comb.r=T, comb.f=F, xlim = c(0,1), fontsize=10, digits=3)

# Assessment for small study effects
funnel.meta(meta_cfr)
