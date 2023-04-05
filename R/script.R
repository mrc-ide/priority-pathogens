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
