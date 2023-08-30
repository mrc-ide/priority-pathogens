orderly2::orderly_strict_mode()

orderly2::orderly_artefact("Cleaned data", "data_cleaning.html")

rmd <- dir(pattern = ".Rmd")
if (length(rmd) != 1L) {
  stop("More than one Rmd is present, please edit script.R")
}

rmarkdown::render(rmd)
