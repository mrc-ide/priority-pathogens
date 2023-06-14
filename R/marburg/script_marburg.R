source('R/data_cleaning.R')

article_raw <- import('data/marburg/raw/marburg_article_new.xlsx')
model_raw <- import('data/marburg/raw/marburg_model_new.xlsx')
outbreak_raw <- import('data/marburg/raw/marburg_outbreak_new.xlsx')
parameter_raw <- import('data/marburg/raw/marburg_parameter_new.xlsx')

real_duplicate <- article_raw %>% filter(duplicated(Covidence_ID)) %>% pull(var = Covidence_ID)

article_clean <- article_raw %>% clean_dfs(column_name = 'article_title') 
model_clean <- model_raw %>% clean_dfs(column_name = 'model_type')
outbreak_clean <- outbreak_raw %>% clean_dfs(column_name = 'outbreak_country')
parameter_clean <- parameter_raw %>% clean_dfs(column_name = 'parameter_type')

details <- article_clean %>% get_details(double_vec = real_duplicate)

article_clean$double_extracted <- details$double_extracted #%>% add_details(detail = details)
model_clean <- model_clean %>% add_details(detail = details)
outbreak_clean <- outbreak_clean %>% add_details(detail = details) %>%
  mutate(outbreak_country = str_replace(outbreak_country, 'Congo, Rep.', 'Republic of the Congo'),
         outbreak_country = str_replace(outbreak_country, 'Congo, Dem. Rep.', 'Democratic Republic of the Congo'),
         outbreak_country = str_replace(outbreak_country, 'Yuogslavia', 'Yugoslavia'))
parameter_clean <- parameter_clean %>% add_details(detail = details)

article_single <- filter_extracted(df = article_clean, double = FALSE, matching = FALSE)
article_double <- article_clean %>% dplyr::filter(double_extracted == 1) %>%
  filter(article_id!=34) %>%
  distinct(covidence_id, .keep_all = TRUE)

## I know there isn't a double for models
model_single <- filter_extracted(df = model_clean, double = FALSE, matching = FALSE)

outbreak_single <- filter_extracted(df = outbreak_clean, double = FALSE, matching = FALSE)
outbreak_double_matching <- filter_extracted(df = outbreak_clean, double = TRUE, matching = TRUE,
                                             id_name1 = "article_id",
                                             id_name2 = "outbreak_id")
outbreak_double_discordant <- filter_extracted(df = outbreak_clean, double = TRUE, matching = FALSE,
                                               id_name1 = "article_id",
                                               id_name2 = "outbreak_id")


parameter_single <- filter_extracted(df = parameter_clean, double = FALSE, matching = FALSE)
parameter_double_matching <- filter_extracted(df = parameter_clean, 
                                             double = TRUE,
                                             matching = TRUE,
                                             id_name1 = "article_id",
                                             id_name2 = "parameter_data_id")
parameter_double_discordant <- filter_extracted(df = parameter_clean, 
                                                double = TRUE,
                                                matching = FALSE,
                                                id_name1 = "article_id",
                                                id_name2 = "parameter_data_id")

outbreak_fixing <- needs_fixing(outbreak_double_discordant, details)
parameter_fixing <- needs_fixing(parameter_double_discordant, details)

# write.csv(outbreak_fixing, "data/marburg/fixing/outbreak_fixing.csv", row.names = FALSE)
# write.csv(parameter_fixing, "data/marburg/fixing/parameter_fixing.csv", row.names = FALSE)

# ## when data is fixed
outbreak_fixed <- rio::import('data/marburg/fixed/marburg_outbreak_fixed.csv')
parameter_fixed <- rio::import('data/marburg/fixed/marburg_parameter_fixed.csv')

## create final datasets
outbreak_fixed <- outbreak_fixed %>%
  dplyr::filter(fixed == TRUE) %>%
  dplyr::select(names(outbreak_single))

parameter_fixed <- parameter_fixed %>%
  dplyr::filter(fixed == TRUE) %>%
  dplyr::select(names(parameter_single))

# final data sets
outbreak_final <- rbind(outbreak_single,
                        outbreak_double_matching,
                        outbreak_fixed)
parameter_final <- rbind(parameter_single,
                         parameter_double_matching,
                         parameter_fixed) %>%
  mutate(Uncertainty = ifelse(parameter_uncertainty_type == 'CI95%', paste0(parameter_uncertainty_lower_value, ", ", parameter_uncertainty_upper_value),
                              ifelse(parameter_uncertainty_type %in% c('Range', 'Highest Posterior Density Interval 95%'), paste0(parameter_uncertainty_lower_value, ' - ', parameter_uncertainty_upper_value),
                                     NA)),
         population_study_start_month = case_when(covidence_id == '3795' ~ 'Feb',
                                                  covidence_id == '2600' ~ 'March',
                                                  TRUE ~ population_study_start_month),
         population_study_start_year = case_when(covidence_id == '3795' ~ '1975', 
                                                 covidence_id == '2600' ~ '1980',
                                                 TRUE ~ population_study_start_year),
         population_study_end_month = case_when(covidence_id == '2600' ~ 'Dec',
                                                TRUE ~ population_study_end_month),
         population_study_end_year = case_when(covidence_id == '2600' ~ '1981',
                                               TRUE ~ population_study_end_year),
         # Format survey year
         `Survey year` = ifelse(population_study_start_year == population_study_end_year & !is.na(population_study_start_month) & !is.na(population_study_end_month),
                                paste0(population_study_start_month, "-", population_study_end_month, " ", population_study_start_year),
                                ifelse(population_study_start_year != population_study_end_year, 
                                       paste0(population_study_start_year,"-", population_study_end_year), population_study_start_year)) ,
         riskfactor_outcome = ifelse(parameter_data_id == '42' | parameter_data_id == '45', 'Death', 
                                     ifelse(parameter_data_id == '16', 'Other', riskfactor_outcome)))

  
model_final <- rbind(model_single)
article_final <- rbind(article_single,
                       article_double) %>%
  filter(!is.na(covidence_id))

write.csv(article_final, "data/marburg/final/article_final.csv", row.names = FALSE)
write.csv(model_final, "data/marburg/final/model_final.csv", row.names = FALSE)
write.csv(outbreak_final, "data/marburg/final/outbreak_final.csv", row.names = FALSE)
write.csv(parameter_final, "data/marburg/final/parameter_final.csv", row.names = FALSE) # 24 apr missing Cyril and Gina's DE
