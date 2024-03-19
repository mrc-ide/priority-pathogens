# Priority Pathogens

The priority-pathogens repository is an [orderly](https://mrc-ide.github.io/orderly2/) project. The repository
consists of a set of ``orderly tasks'' in the src folder that make up our analytical pipeline. The first three tasks in 
the pipeline (`db_extraction`, `db_double`, and `db_compilation`) read in the Access databases, clean, and compile a 
clean dataset for downstream analysis. These tasks need access to the database files stored on Imperial's internal 
network. Since an external user cannot run these tasks, we provide the outputs of these tasks with the associated 
Github release. These outputs must be downloaded and `made visible' to the orderly project for the rest of the code
to work. The R script `ebola_workflow.R` will download the outputs and do the necessary configuration for this to
happen. To run the script, 

1. First clone this git repository somewhere on your machine
```
git clone https://github.com/mrc-ide/priority-pathogens.git
```

2. Make sure you have the following packages installed
```
install.packages(c("cowplot", "doconv", "dplyr", "epitrix", "estmeansd", "flextable", "ftExtra", "ggforce", "ggplot2", "grid", "gridExtra", "janitor", "meta", "metafor", "officer", "optparse", "orderly2", "png", "purrr", "readr", "scales", "splitstackshape", "stringr", "tidyr", "writexl", "zip"))
```
Please note you need version 7.0.0 of ```meta``` due to changes in function names.

3. To run the Ebola analysis and recreate the findings in [Nash et al.](), navigate to the downloaded repository 
and run the script `ebola_workflow.R` specifying the location where the outputs from the first three tasks will be 
downloaded using the `-l` flag. On the command line, type the following:

```
Rscript ebola_workflow.R -l ~/Downloads
```
``~/Downloads'' should be replaced by the full path of the folder where you wish to download the outputs. Note that this
*** location should not be a sub-folder of the priority-pathogens repo ***.
The script will then download the outputs, configure the orderly project and run ebola analysis tasks sequentially. 
* Note that the tasks create and save a number of images and word documents, and will take a long time to finish. *
