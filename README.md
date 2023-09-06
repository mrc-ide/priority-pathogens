# priority-pathogens
Welcome to the GitHub repository for the priority pathogen project.

This repository has been set up as an orderly project. Please follow the 
installation instructions for orderly2 available here: 
https://github.com/mrc-ide/orderly2.

Note: When you run orderly for the first time you may get an error saying that
you need to run orderly2::orderly_init("pathway"), please run the line of code 
it tells you to.

### Task 1: db_extraction

*IMPORTANT* This task can only run on a Windows machine becuase as far
as we know, Microsoft does not provide a (free) Mac driver for their
Access DB. Mac users can use their Windows VM to run this task by
connecting the priority-pathogens repo to Rstudio on their VM. If you
are unsure about the steps, please feel free to message Rebecca or
Sangeeta.

Once extractions are complete and you want to compile the databases together:
* Clone the latest priority-pathogens repo
* Open the priority-pathogens R project on your machine
* Copy all extraction databases (single and double extraction databases) into 
the "src/db_extraction" folder (Note: this may change and be set up to get the
databases directly from the shared drive)
* Ensure that in the "orderly.R" script the orderly_resource() function has been
updated with all of the database file names you have added to the folder
* Then run the following (specifying the pathogen):

```
orderly2::orderly_run("db_extraction",
                        parameters = list(pathogen = "EBOLA"))
```
Replace EBOLA with the pathogen of interest.

This orderly task will have combined all the individual extraction databases into 
article, model, parameter and outbreak .csv files, and will have created new ID 
variables. It will automatically detect which papers have been double extracted 
and create separate article, model, parameter and outbreak files for single and
double extracted papers.

Combined data for single extracted papers will be in:
articles_single.csv, parameters_single.csv, models_single.csv, outbreaks_single.csv

Combined data for double extracted papers will be in:
articles_double.csv, parameters_double.csv, models_double.csv, outbreaks_double.csv

These files can be found within the “archive” folder in the priority-pathogens
directory.

### Task 2: db_double

This task takes the article, parameter and model csv files for the double
extracted papers and identifies the entries that match and those that need to
be given back to the extractors to be fixed.

* Copy across the "articles_double.csv", "parameters_double.csv", "models_double.csv"
and "outbreaks_double.csv" files from the "archive/db_extraction" folder to the
"src/db_double" folder (Note: update this to use orderly_dependency() when running properly)
* Ensure that the "orderly.R" script lists "articles_double.csv",
"parameters_double.csv", "models_double.csv" and "outbreaks_double.csv" in the
orderly_resource() function
* Then run the following:

```
orderly2::orderly_run("db_double")
```

Once this has run, the outputs will be in the "db_double" folder, within the
"archive" folder of the priority-pathogens main directory. Data that matches
between extractors will be in:

qa_matching.csv, model_matching.csv, parameter_matching.csv, outbreak_matching.csv

Whereas, data that does not match between extractors will be in:

qa_fixing.csv, model_fixing.csv, parameter_fixing.csv, outbreak_fixing.csv

These fixing files will need to go back to the extractors to be resolved.

### Task 3: db_compilation

Once the double extraction fixes are complete, all of the single extracted data
and double extracted data can then be compiled together. This task produces
separate article, parameter and model csv files that will be used for the analysis.

* Copy across the "articles_single.csv", "parameters_single.csv", "models_single.csv",
"outbreak_single.csv" files from the "archive/db_extraction" folder to the
"src/db_compilation" folder (Note: update this to use orderly_dependency() when running properly)
* Copy across the "qa_matching.csv", "model_matching.csv", "parameter_matching.csv",
"outbreak_matching.csv" files from the "archive/db_double" folder to the
"src/db_compilation" folder (Note: update this to use orderly_dependency() when running properly)
* Add the corrected "qa_fixing.csv", "model_fixing.csv", "parameter_fixing.csv",
"outbreak_fixing.csv" files to the "archive/db_double" folder
* Then run the following:

```
orderly2::orderly_run("db_compilation")
```

The outputs will be in the "db_compilation" folder, within the "archive" folder
of the priority-pathogens main directory.
