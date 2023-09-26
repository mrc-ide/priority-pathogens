# priority-pathogens
Welcome to the GitHub repository for the priority pathogen project.

This repository has been set up as an orderly project. Please follow the 
installation instructions for orderly2 available here: 
https://github.com/mrc-ide/orderly2.

Also install orderly.sharedfile plugin.
``` remotes::install_github("mrc-ide/orderly.sharedfile") ```

Note: When you run any of the orderly tasks for the first time, you may get an
error saying that you need to run orderly2::orderly_init("pathway"), please run
the line of code it tells you to.

You will find each of the tasks below in the src folder.

### Task 1: db_extraction

*IMPORTANT* This task can only run on a Windows machine becuase as far
as we know, Microsoft does not provide a (free) Mac driver for their
Access DB. Mac users can use their Windows VM to run this task by
connecting the priority-pathogens repo to Rstudio on their VM. If you
are unsure about the steps, please feel free to message Rebecca or
Sangeeta. 

*IMPORTANT* If you do not want to run any of the tasks yourself, but
still want to use the outputs, please see instructions below. 

Once extractions are complete and you want to compile the databases together:
* Clone the latest priority-pathogens repo
* Open the priority-pathogens R project on your machine
* Edit orderly_config.yml file to replace the "singledb", "doublesb", and 
"doubledb2" fields to appropriate values. These fields should contain the fully 
qualified name of the folder where the database files are located, 
*as seen from your machine*. For instance, I have mapped the PriorityPathogens 
shared drive to Y: locally. Hence for me, the entries are 
"Y:/Ebola/databases/Single extraction databases" etc.
* Ensure that in the "orderly.R" script has been
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

These files can be found within the “archive/db_extraction” folder in the 
priority-pathogens directory.

### Task 2: db_double

This task takes the article, parameter, model and outbreak csv files for the 
double extracted papers and identifies the entries that match and those that 
need to be given back to the extractors to be fixed.

* There is an orderly_dependency() function to pull across the files created in
the db_extraction task. Update the task id in this function to the latest
db_extraction task id, or replace this with "latest" to simply take the files
from the latest task run. When the db_double task runs, this will automatically
copy across the "double_extraction_articles.csv", "double_extraction_params.csv",
"double_extraction_models.csv" and "double_extraction_outbreaks.csv" files from
the "archive/db_extraction" folder to the "src/db_double" folder.
* Ensure that the orderly_dependency() function also lists the file names you
want from the db_extraction task, e.g. "double_extraction_articles.csv",
"double_extraction_params.csv", "double_extraction_models.csv" and 
"double_extraction_outbreaks.csv", depending on what was extracted for your
pathogen.
* Then run the following:

```
orderly2::orderly_run("db_double")
```

Once this has run, the outputs will be in the "archive/db_double" folder. You 
can find the archive folder within the priority-pathogens main directory. Data 
that matches between extractors will be in:

qa_matching.csv, model_matching.csv, parameter_matching.csv, outbreak_matching.csv

Whereas, data that does not match between extractors will be in:

qa_fixing.csv, model_fixing.csv, parameter_fixing.csv, outbreak_fixing.csv

These fixing files will need to go back to the extractors to be resolved.

### Task 3: db_compilation

Once the double extraction fixes are complete, all of the single extracted data
and double extracted data can then be compiled together. This task produces
separate article, parameter and model csv files that will be used for the analysis.

For this task there are two orderly_dependency() functions to update:
* In the orderly_dependency() function for "db_extraction", ensure that this has
the latest db_extraction task id. This will copy across the "articles_single.csv",
"parameters_single.csv", "models_single.csv", "outbreak_single.csv" and 
"double_extraction_articles.csv" files from the "archive/db_extraction" folder
to the "src/db_compilation" folder. Make sure the relevant files for your
pathogen are listed in this function.
* In the orderly_dependency() function for "db_double", ensure that this has
the latest db_double task id. This will copy across the "qa_matching.csv", 
"model_matching.csv", "parameter_matching.csv", and "outbreak_matching.csv" files
from the "archive/db_double" folder to the "src/db_compilation" folder. Again, 
make sure the relevant files for your pathogen are listed in this function.
* You will need to manually add the corrected "qa_fixing.csv", "model_fixing.csv",
"parameter_fixing.csv", and "outbreak_fixing.csv" files to the "archive/db_double"
folder.
* Then run the following:

```
orderly2::orderly_run("db_compilation")
```

The outputs will be in the "db_compilation" folder, within the "archive" folder
of the priority-pathogens main directory.

### FAQ

#### I don't want to or can't run one of the orderly tasks. Can I still somehow get the outputs?
Yes, you can. Say you are a Mac user and don't want to run the
db_compilation task. Or say if you have run it on your Windows VM and
want to copy the outputs over to your local machine. This is very easy
with orderly2.
1. First, run or have someone run for you the desired orderly task on
   a shared drive. This is so that the outputs are visible to you. 
2. Map the shared drive on your machine.
3. For instance, Rebecca has run the tasks db_compilation on a shared
   drive. I have mapped the drive on my Mac. Now, we add the orderly
   folder on the shared drive as an orderly "location" as follows:
   ``` 
   orderly2::orderly_location_add(name = "pp-network-drive", args
   = list(path =
   "/Volumes/outbreak_analysis/sbhatia/priority-pathogens/"), type =
   "path")
   ```
Here, "name" can be anything, and "args" should be the fully
   qualified name of the path where orderly project is available.
   
4. Then do
```
orderly2::orderly_location_pull_metadata()
```

This allows orderly2 to retrieve the necessary metadata from all
locations.

5. Finally, you can pull the outputs ("packets" in orderly2 terminology):
   
```
orderly2::orderly_location_pull_packet(<ids>)
```
where "ids" is the set of ids that you want to pull to your local
   machine. You can get this value by looking into the archive
   directory of the orderly project mounted on network drive.
   
6. If instead you want to push a task output to the shared drive for someone
else to pull:
   
```
orderly2::orderly_location_push("id", "pp-network-drive")
```
   
you supply the task id and the name of the location you mapped in step 3.
