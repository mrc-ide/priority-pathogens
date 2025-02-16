# OROV risk factors

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
#which(paste0("#",articles$covidence_id) %in% covidence$CovID)
#articles[which(duplicated(articles$covidence_id)),]
testthat::expect_length(
  which(!(paste0("#",articles$covidence_id) %in% covidence$CovID)),
  0)



## risk factors
parameters %>% filter(parameter_type_broad=="Risk factors") %>% View()


risk_params <- parameters %>%
  filter(grepl("Risk factors", parameter_type, ignore.case = TRUE)) %>%
  select(covidence_id,riskfactor_outcome, 
         riskfactor_name,	riskfactor_significant, 
         riskfactor_adjusted, population_sample_size,
         population_country, #dates,
         population_sample_type, population_group,parameter_context_human)


# what is happening in terms of number of obs per paper
risk_params %>% group_by(covidence_id) %>% 
  summarise(n = length(covidence_id)) %>% 
  arrange(-n)


risk_table <- risk_params %>%
  separate_longer_delim(riskfactor_name, delim = ";") %>% 
  mutate(population_sample_size = replace_na(as.double(population_sample_size),0),
         riskfactor_adjusted    = case_when(riskfactor_adjusted=='' ~ 'Unspecified',
                                            TRUE ~ riskfactor_adjusted)) %>%
  group_by(riskfactor_outcome,riskfactor_name,riskfactor_significant,riskfactor_adjusted,
           parameter_context_human) %>%
  summarise(n=n(),
            pop_size = sum(population_sample_size)) %>%
  unite(`Significant / Adjusted`,riskfactor_significant:riskfactor_adjusted, remove = FALSE, sep = " / ") %>% arrange(-n)

text_size <- 20
custom_colours <- c('Significant / Adjusted'='blue4', 'Significant / Not adjusted' = 'lightblue', 'Significant / Unspecified'='blue',
                    'Not significant / Adjusted'='darkred', 'Not significant / Not adjusted' = 'pink', 'Not significant / Unspecified'='red',
                    'Unspecified / Adjusted'='grey30', 'Unspecified / Not adjusted' = 'grey50', 'Unspecified / Unspecified'='grey70')



# Infection
risk_table_plt_infection <- risk_table %>% filter(riskfactor_outcome=='Infection') %>%
  ggplot(aes(x=riskfactor_name,y=n,col=`Significant / Adjusted`, fill=`Significant / Adjusted`)) + 
  geom_bar( stat='identity',position = position_dodge(preserve = "single")) +
  scale_color_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) + xlab('') + ylab('') + theme_light() + 
  theme( axis.text.x = element_text( angle = 25, hjust = 1, size = text_size ),
         strip.text = element_text( color = "black"),          
         strip.background =element_rect(fill="grey90"),
         text = element_text(size = text_size),
         legend.position = 'none')


# Serology
risk_table_plt_serology <- risk_table %>% filter(riskfactor_outcome=='Serology') %>%
  ggplot(aes(x=riskfactor_name,y=n,col=`Significant / Adjusted`, fill=`Significant / Adjusted`)) + 
  geom_bar( stat='identity',position = position_dodge(preserve = "single")) + 
  theme_light()+
  scale_color_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) + xlab('') + ylab('')  + 
  theme( axis.text.x = element_text( angle = 25, hjust = 1, size = text_size ),
         strip.text = element_text( color = "black"),          
         strip.background =element_rect(fill="grey90"),
         text = element_text(size = text_size),
         legend.position = 'none') 

cowplot::plot_grid(risk_table_plt_infection,
                   risk_table_plt_serology,
                   nrow=2)


## how many of each of the different outcomes
risk_params %>% select(riskfactor_outcome) %>% group_by(riskfactor_outcome) %>%
  summarise(length(riskfactor_outcome))


# whats happening with infection
risk_params %>% filter(riskfactor_outcome=="Infection")

# whats happening with serology
risk_params %>% filter(riskfactor_outcome=="Serology")

# what is happening with occupation
risk_params %>% filter(grepl("Occupation",riskfactor_name))

# what is the deal with animals
risk_params %>% filter(parameter_context_human=="Not human")


