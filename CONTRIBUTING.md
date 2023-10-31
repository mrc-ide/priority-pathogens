# Contributing to the priority-pathogens repository

To contribute to the project codebase, 

* clone the repository

```
git clone https://github.com/reconhub/projections.git
```

* Create a new local branch. It is generally a good idea to give your
  branch a sensible name e.g. lassa-cleaning, qa-analysis etc.

* If you want to create a new orderly task, then create a folder under
  "src". For pathogen-specific tasks, use "pathogen_<xyz>" convention
  where xyz describes the taks e.g., "ebola_cfr_metanalysis". 

* Develop the task locally and if you are happy that it works as
  expected, raise a PR. Before raising a PR, make sure that the task
  runs without any errors in a fresh R session. 

* Give sufficient details in the PR on how to run the task and
  expected outputs.

* Request review on the PR from someone *not* in your pathogen group.
  
* Incorporate any feedback given, and iterate until PR is approved for
  merge.

* In general, do not work on a branch for a long time as it then
  becomes a pain to merge it and isolate changes. 


