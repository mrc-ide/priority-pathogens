article_raw <- import('data/marburg/marburg_article.xlsx')
outbreak_raw <- import('data/marburg/marburg_outbreak.xlsx')
model_raw <- import('data/marburg/marburg_model.xlsx')
parameters_raw <- readxl::read_xlsx('data/marburg/marburg_parameter.xlsx')


article_clean <- article_raw %>% clean_dfs(column_name = 'article_name') 

details <- article_clean %>% get_details()



parameter_clean <- parameters_raw %>% clean_dfs(column_name = 'parameter_type')
