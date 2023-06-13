# Function to produce table

REGENERATE_DATA <- FALSE

if(REGENERATE_DATA) source("R/marburg/script_marburg.R")              #only rerun this if we want to re-generate the marburg files (rather than rewriting this every time) 

source("R/table_code.R")

par_final <- read.csv("data/marburg/final/parameter_final.csv")
article_df <- read.csv("data/marburg/final/article_final.csv")

# merge with article ID article labels
df <- merge(par_final, article_df %>% dplyr::select(article_id, first_author_first_name,year_publication),
            all.x=TRUE, by="article_id") %>%
  mutate(article_label = as.character(paste0(first_author_first_name," ",year_publication)),
         population_country = str_replace_all(population_country,";",", ")) %>%
  dplyr::arrange(article_label, -year_publication) %>%
  dplyr::filter(article_id %in% c(17,15) == FALSE) %>%
  rename('Survey year'=Survey.year)
  

sero_table(df,pathogen="marburg")
delay_table(df,pathogen="marburg")
