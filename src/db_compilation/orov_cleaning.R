orov_cleaning_articles <- function(df) {
  df <- df %>% 
    # dummy entry but marked as OROV 
    filter(covidence_id!="999999",
           # duplicate entries for 30 and 109
           id!="6e85a97a-c427-4eef-b0e8-f55142802ef1",
           id!="23d7b6a2-bbbe-4f94-984f-da9fd0bab2d5")
}


orov_cleaning_parameters <- function(df){
  df <- df %>% mutate()
}