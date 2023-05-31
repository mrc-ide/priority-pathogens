source('R/data_cleaning.R')

article_raw <- import('data/marburg/raw/marburg_article.xlsx')
model_raw <- import('data/marburg/raw/marburg_model.xlsx')
outbreak_raw <- import('data/marburg/raw/marburg_outbreak.xlsx')
parameter_raw <- readxl::read_xlsx('data/marburg/raw/marburg_parameter.xlsx')

# Editing specific entries
parameter_raw <- parameter_raw %>%
  janitor::clean_names() %>%
  mutate(parameter_type = ifelse(parameter_data_id == 54, 'Risk factor', parameter_type),
         # Format the start/stop months
         population_study_start_month = substring(population_study_start_month, 1, 3),
         population_study_end_month = substring(population_study_end_month, 1, 3),
         # fixing missing month/year 
         population_study_start_month = ifelse((article_id == 57), 'Oct', population_study_start_month),
         population_study_end_month = ifelse((article_id == 57), 'Sep', population_study_end_month),
         population_study_start_year = ifelse((article_id == 57), '1998', population_study_start_year),
         population_study_end_year = ifelse((article_id == 57), '2000', population_study_end_year),
         # Format survey year
         `Survey year` = ifelse(population_study_start_year == population_study_end_year & !is.na(population_study_start_month) & !is.na(population_study_end_month),
                                paste0(population_study_start_month, "-", population_study_end_month, " ", population_study_start_year),
                                ifelse(population_study_start_year != population_study_end_year, 
                                       paste0(population_study_start_year,"-", population_study_end_year), population_study_start_year)),
         # fixing mislabeling of range
         parameter_uncertainty_type = ifelse((article_id == 57 & parameter_data_id == 89) | 
                                               (article_id == 7 & parameter_data_id == 78), 'Range', parameter_uncertainty_type),
         # fixing missing country 
         population_country = ifelse(article_id == 58 | article_id == 57, 'Democratic Republic of the Congo', 
                                     ifelse(article_id == 6, 'South Africa', population_country)),
         # combining range and uncertainty range
         parameter_uncertainty_type = ifelse(!is.na(parameter_upper_bound) & is.na(parameter_uncertainty_upper_value), 'Range', parameter_uncertainty_type),
         parameter_uncertainty_upper_value = ifelse(!is.na(parameter_upper_bound) & is.na(parameter_uncertainty_upper_value), parameter_upper_bound, parameter_uncertainty_upper_value),
         parameter_uncertainty_lower_value = ifelse(!is.na(parameter_lower_bound) & is.na(parameter_uncertainty_lower_value), parameter_lower_bound, parameter_uncertainty_lower_value),
         Uncertainty = ifelse(parameter_uncertainty_type == 'CI95%', paste0(parameter_uncertainty_lower_value, ", ", parameter_uncertainty_upper_value),
                              ifelse(parameter_uncertainty_type %in% c('Range', 'Highest Posterior Density Interval 95%'), paste0(parameter_uncertainty_lower_value, ' - ', parameter_uncertainty_upper_value),
                                     NA))) 


real_duplicate <- c(unique(article_raw$Covidence_ID))

article_clean <- article_raw %>% clean_dfs(column_name = 'article_title') 
model_clean <- model_raw %>% clean_dfs(column_name = 'model_type')
outbreak_clean <- outbreak_raw %>% clean_dfs(column_name = 'outbreak_country')
parameter_clean <- parameter_raw %>% clean_dfs(column_name = 'parameter_type')

details <- article_clean %>% get_details(double_vec = real_duplicate)

article_clean <- article_clean %>% add_details(detail = details)
model_clean <- model_clean %>% add_details(detail = details)
outbreak_clean <- outbreak_clean %>% add_details(detail = details) %>%
  mutate(outbreak_country = str_replace(outbreak_country, 'Congo, Rep.', 'Republic of the Congo'),
         outbreak_country = str_replace(outbreak_country, 'Congo, Dem. Rep.', 'Democratic Republic of the Congo'))
parameter_clean <- parameter_clean %>% add_details(detail = details)

article_single <- filter_extracted(df = article_clean, double = FALSE, matching = FALSE)
article_double <- article_clean %>% dplyr::filter(double_extracted == 1) # filter_extracted doesn't work here -- need specific quality assessment

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

write.csv(outbreak_fixing, "data/marburg/fixing/outbreak_fixing.csv", row.names = FALSE)
write.csv(parameter_fixing, "data/marburg/fixing/parameter_fixing.csv", row.names = FALSE)

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
                         parameter_fixed)
model_final <- rbind(model_single)
article_final <- rbind(article_single,
                       article_double) # think if this is correct

write.csv(article_final, "data/marburg/final/article_final.csv")
write.csv(model_final, "data/marburg/final/model_final.csv")
write.csv(outbreak_final, "data/marburg/final/outbreak_final.csv")
write.csv(parameter_final, "data/marburg/final/parameter_final.csv") # 24 apr missing Cyril and Gina's DE
