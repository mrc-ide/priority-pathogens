## analyse oropouche

library(tidyverse)
library(forcats)
library(orderly2)

orderly_strict_mode()
orderly2::orderly_parameters(pathogen = "OROV")

orderly2::orderly_artefact(description = "inputs folder",
                           files = "inputs/")

# did not like latest(parameter:pathogen == this:pathogen) - fix in future - seems to have resolved itself for now...
orderly_dependency(
  name = "db_compilation",
  query = "latest(parameter:pathogen == this:pathogen)",
  files = c("inputs/articles.csv"="articles.csv",
            "inputs/parameters.csv"="parameters.csv",
            "inputs/outbreaks.csv"="outbreaks.csv"))

# read in data
articles <- read.csv("inputs/articles.csv")
outbreaks <- read.csv("inputs/outbreaks.csv")
parameters <- read.csv("inputs/parameters.csv")

# covidence 
covidence <- read.csv("covidence/orov_covidence.csv") %>%
  rename(CovID = Covidence..)
covidence$CovID %>% unique() %>% length()

# do we have all the articles we are expecting?
testthat::expect_true(
  nrow(articles) == length(unique(covidence$CovID)))
testthat::expect_length(
  which(!(paste0("#",articles$covidence_id) %in% covidence$CovID)),
  0)

# which are missing 
if(nrow(articles) != length(unique(covidence$CovID))){
  all_ids <- covidence$CovID %>% sub('.', '', .) %>% as.numeric()
  all_ids[which(!(unique(all_ids) %in% articles$covidence_id))]
}

# what are we dealing with in terms of parameters 
parameters %>% select(parameter_type) %>% group_by(parameter_type) %>%
  summarise("total" = length(parameter_type))

parameters %>% filter(is.na(parameter_context_human))

ggplot(parameters,
       aes(x=fct_infreq(parameter_type_broad),
           fill=parameter_context_human))+
  geom_bar()+
  theme_bw()+
  scale_fill_manual(values= c("navyblue","royalblue","skyblue"))+
  labs(x="Type of parameter",y="Number extracted",fill="")+
  theme(axis.text.x = element_text(angle=90))


parameters %>% group_by(parameter_type_broad) %>%
  summarise(length(parameter_type_broad))

parameters %>% group_by(parameter_type_broad,parameter_context_human) %>%
  summarise(length(parameter_type_broad))


## how many preprints
articles %>% group_by(article_preprint) %>% summarise(length(article_preprint))
#articles %>% filter(is.na(article_preprint))

## look at QA scores
ggplot(articles,
       aes(x=year_publication,y=article_qa_score))+
  geom_point()+
  theme_bw()+
  labs(x="Article publication year",y="Quality assessment score (%)")

articles %>% filter(article_qa_score>50) %>% nrow()
articles %>% filter(article_qa_score>50) %>% nrow()/nrow(articles)

lm(articles$article_qa_score~articles$year_publication) %>% summary()

articles %>% filter(year_publication>=2020) %>% nrow()

## test for difference in average QA scores between the two time periods
articles %>% summarise(mean = mean(article_qa_score))

articles %>% mutate(post_2020 = ifelse(year_publication>=2020,1,0)) %>%
  group_by(post_2020) %>% 
  summarise(mean = mean(article_qa_score))

wilcox.test(
  x = articles %>% filter(year_publication < 2020) %>% 
    select(article_qa_score) %>% unlist() %>% as.numeric(),
  y= articles %>% filter(year_publication >= 2020) %>% 
    select(article_qa_score) %>% unlist() %>% as.numeric())


ggplot(articles %>% 
         mutate(post_2020 = factor(ifelse(
           year_publication>=2020,"Post-2020","Pre-2020"),
           levels=c("Pre-2020","Post-2020"))),
       aes(x=article_qa_score,fill=post_2020))+
  geom_histogram(position="dodge",bins=10)+
  theme_bw()+
  labs(x="Quality assessment score (%)",y="Number of papers",
       fill="Time period")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values=c("forestgreen","palegreen2"))



## need to see wtf is going on here
parameters %>% filter(parameter_context_human=="Not human") %>% 
  select(population_sample_type) %>% unique()

parameters %>% filter(parameter_context_human=="Not human") %>% 
  select(population_group) %>% unique()



## how many people from people with symptoms/who had Dengue 
parameters$population_group %>% as.factor() %>% summary()




###### outbreaks

outbreaks$outbreak_case_report

