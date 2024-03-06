library(dplyr)
library(orderly2)
library(purrr)
library(readr)
library(stringr)
## orderly_parameters(pathogen = NULL)
orderly_parameters(pathogen = 'EBOLA')
orderly_dependency(
  "db_compilation",
  "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "parameters.csv", "models.csv", "outbreaks.csv")
)

params <- read_csv("parameters.csv")
articles <- read_csv("articles.csv")
models <- read_csv("models.csv")


## From the articles we want "first_author_first_name", "first_author_surname",  
## "article_title", "journal", "year_publication", "doi", "article_label"
articles <- select(
    articles,
    covidence_id,
    id,
    first_author_first_name,
    first_author_surname,
    article_title,
    journal,
    year_publication,
    doi,
    article_label
)
## Add Journal and Year of publication to the article label
## so that people can find it even if there is no DOI
articles$article_title <- paste0(
  articles$article_title, 
  " (", articles$journal, ", ", articles$year_publication, ")"
)
## And make case consistent - sentence case
articles$article_title <- str_to_title(articles$article_title)
## Format the doi as a URL
## Some have the word "doi" in them, some don't
articles$doi <- gsub("doi:", "", articles$doi, ignore.case = TRUE)
## Remove leading and trailing whitespace
articles$doi <- trimws(articles$doi)
articles$doi[!is.na(articles$doi)] <- paste0("https://doi.org/", articles$doi[!is.na(articles$doi)])
## If the doi is NA, leave it blank
articles$doi[is.na(articles$doi)] <- ""
## From params, we want: id, parameter_type, ebola_species, 
cols <- c("id", "covidence_id", "parameter_type", "ebola_species")
cols <- intersect(cols, colnames(params)) ## for other pathogens
params <- select(params, all_of(cols))

## We will now go the other way, and find out what has been
## extracted from each article. That will make it easier to keep track
## of the number of articles.

out <- map_dfr(
    articles$covidence_id, function(id) {
      p <- params[params$covidence_id %in% id, cols]
      params_extrctd <- paste(unique(p$parameter_type), collapse = "\n")
      m <- models[models$covidence_id %in% id, ]
      model_extrctd <- ifelse(nrow(m) > 0, "YES", "NO")
      a <- articles[articles$covidence_id %in% id, ]
      data.frame(
        `Article` = a$article_label,
        `Title` = a$article_title,
        ##journal = a$journal,
        ##year_publication = a$year_publication,
        ##first_author_first_name = a$first_author_first_name,
        ##first_author_surname = a$first_author_surname,
        Doi = a$doi,
        `Parameters Extracted` = params_extrctd,
        `Model Extarcted (YES/NO)` = model_extrctd
      )
    }
  )
## Alphabetically sort the articles
out <- arrange(out, `Article`)

write_csv(out, "all_studies.csv")
