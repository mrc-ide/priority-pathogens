article_raw <- import('data/marburg/marburg_article.xlsx')
model_raw <- import('data/marburg/marburg_model.xlsx')
outbreak_raw <- import('data/marburg/marburg_outbreak.xlsx')
parameter_raw <- readxl::read_xlsx('data/marburg/marburg_parameter.xlsx')

article_clean <- article_raw %>% clean_dfs(column_name = 'article_title') 
model_clean <- model_raw %>% clean_dfs(column_name = 'model_type')
outbreak_clean <- outbreak_raw %>% clean_dfs(column_name = 'outbreak_country')
parameter_clean <- parameter_raw %>% clean_dfs(column_name = 'parameter_type')

details <- article_clean %>% get_details()

article_clean <- article_clean %>% add_details(detail = details)
model_clean <- model_clean %>% add_details(detail = details)
outbreak_clean <- outbreak_clean %>% add_details(detail = details)
parameter_clean <- parameter_clean %>% add_details(detail = details)

article_single <- get_single_extracted(df_with_detail = article_clean)
article_double <- get_double_extracted(df_with_detail = article_clean) 

model_single <- get_single_extracted(df_with_detail = model_clean)
model_double <- get_double_extracted(df_with_detail = model_clean)

outbreak_single <- get_single_extracted(df_with_detail = outbreak_clean)
outbreak_double <- get_double_extracted(df_with_detail = outbreak_clean)

parameter_single <- get_single_extracted(df_with_detail = parameter_clean)
parameter_double <- get_double_extracted(df_with_detail = parameter_clean)
parameter_double_matching <- double_matching(df = df, 
                                             column_name1 = "parameter_type", 
                                             column_name2 = "population_location", 
                                             id_name2 = "parameter_data_id")
parameter_double_discordant <- double_disconcordant(df = df, 
                                                    column_name1 = "parameter_type", 
                                                    column_name2 = "population_location", 
                                                    id_name2 = "parameter_data_id")

## sanity check
(nrow(parameter_double_matching)*2) + nrow(parameter_double_discordant) == nrow(parameter_double)
