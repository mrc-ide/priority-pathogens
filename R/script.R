source('R/data_cleaning.R')

## automate so you can read from the assignment
real_duplicate <- c(1483,
                    1594,
                    1595,
                    1613,
                    1615,
                    1649,
                    1692,
                    1693,
                    1871,
                    1927,
                    1930,
                    1931,
                    1983,
                    2032,
                    2042)

article_raw <- import('data/marburg/raw/marburg_article.xlsx')
model_raw <- import('data/marburg/raw/marburg_model.xlsx')
outbreak_raw <- import('data/marburg/raw/marburg_outbreak.xlsx')
parameter_raw <- readxl::read_xlsx('data/marburg/raw/marburg_parameter.xlsx')

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
