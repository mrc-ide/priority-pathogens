source('R/data_cleaning.R')

article_raw <- import('data/marburg/raw/marburg_article.xlsx')
model_raw <- import('data/marburg/raw/marburg_model.xlsx')
outbreak_raw <- import('data/marburg/raw/marburg_outbreak.xlsx')
parameter_raw <- readxl::read_xlsx('data/marburg/raw/marburg_parameter.xlsx')

article_clean <- article_raw %>% clean_dfs(column_name = 'article_title') 
model_clean <- model_raw %>% clean_dfs(column_name = 'model_type')
outbreak_clean <- outbreak_raw %>% clean_dfs(column_name = 'outbreak_country')
parameter_clean <- parameter_raw %>% clean_dfs(column_name = 'parameter_type')

details <- article_clean %>% get_details()

article_clean <- article_clean %>% add_details(detail = details)
model_clean <- model_clean %>% add_details(detail = details)
outbreak_clean <- outbreak_clean %>% add_details(detail = details)
parameter_clean <- parameter_clean %>% add_details(detail = details)

article_single <- filter_extracted(df = article_clean, double == FALSE, matching == FALSE)
article_double <- article_clean %>% dplyr::filter(double_extracted == 1) 

model_single <- get_single_extracted(df_with_detail = model_clean)
model_double <- get_double_extracted(df_with_detail = model_clean)

outbreak_single <- get_single_extracted(df_with_detail = outbreak_clean)
outbreak_double <- get_double_extracted(df_with_detail = outbreak_clean)

parameter_single <- get_single_extracted(df_with_detail = parameter_clean)
parameter_double <- get_double_extracted(df_with_detail = parameter_clean)
parameter_double_matching <- filter_extracted(df = parameter_double, 
                                             double = TRUE,
                                             matching = TRUE,
                                             column_name1 = "parameter_type", 
                                             column_name2 = "population_location",
                                             id_name1 = "article_id",
                                             id_name2 = "parameter_data_id")
parameter_double_discordant <- filter_extracted(df = parameter_double, 
                                                double = TRUE,
                                                matching = FALSE,
                                                column_name1 = "parameter_type", 
                                                column_name2 = "population_location",
                                                id_name1 = "article_id",
                                                id_name2 = "parameter_data_id")

