# priority-pathogens
Welcome to the GitHub repository for the priority pathogen project.

This repository has been set up as an orderly project. Please follow the 
installation instructions for orderly2 available here: 
https://github.com/mrc-ide/orderly2.

Once single or double extractions are complete and you want to compile the 
databases together:
* Clone the latest priority-pathogens repo
* Open the priority-pathogens R project on your machine
* Copy all extraction databases into the "src/db_extraction" folder
* Ensure that in the "orderly.R" script the orderly_resource() function has been
updated with all of the database filenames you have added to the folder
* Then run the following:

```
orderly2::orderly_run("db_extraction")
```

This orderly task will have combined all the individual extraction databases into 
article, model and parameter .csv files, and created new ID variables. These 
files can be found within the “archive” folder in the priority-pathogens directory.
