# Task to make summary figures and tables 

library(dplyr) 
library(stringr)
library(tibble)
library(purrr)

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("zika_prep_data", "latest(parameter:pathogen == this:pathogen &&
                   parameter:plotting == TRUE)",
                   c("articles_curated.rds", "outbreaks_curated.rds", "models_curated.rds", "parameters_curated.rds"))

##################
## DATA CURATION ##
###################

articles   <- readRDS("articles_curated.rds")
models     <- readRDS("models_curated.rds")
parameters <- readRDS("parameters_curated.rds")

dfs <- data_curation(articles,tibble(),models,parameters, plotting = FALSE )

articles   <- dfs$articles
models     <- dfs$models
parameters <- dfs$parameters



## Articles 
quality <- articles %>%
  mutate(across(starts_with("qa"), ~as.numeric(replace(replace(., . == "Yes", 1), . == "No", 0))))

quality <- quality %>% rowwise() %>% 
  mutate(score = 100*qa_score)
quality <- quality %>% mutate(category = ifelse(covidence_id %in% models$covidence_id, "Modelling Studies", "Non-Modelling Studies"))
quality$category <- factor(quality$category, levels = c("Non-Modelling Studies", "Modelling Studies"))

answers <- quality %>% 
  filter(!is.na(year_publication) & !is.na(pathogen)) %>% 
  dplyr::select(covidence_id,qa_m1,qa_m2,qa_a3,qa_a4,qa_d5,qa_d6,qa_d7) %>%
  pivot_longer(-covidence_id,names_to = "Question",values_to= "Assessment") %>% 
  mutate(Assessment=as.factor(as.character(Assessment)),
         Assessment=case_when(Assessment == '1' ~ 'Yes',
                              Assessment == '0' ~ 'No')) %>%
  mutate(Question=case_when(Question=="qa_m1"~"Q1 Method: \nClear & \nReproducible",
                            Question=="qa_m2"~"Q2 Method: \nRobust & \nAppropriate",
                            Question=="qa_a3"~"Q3 Assumptions: \nClear & \nReproducible",
                            Question=="qa_a4"~"Q4 Assumptions: \nRobust & \nAppropriate",
                            Question=="qa_d5"~"Q5 Data: \nClear & \nReproducible",
                            Question=="qa_d6"~"Q6 Data: \nIssues \nAcknowledged",
                            Question=="qa_d7"~"Q7 Data: \nIssues \nAccounted For")) 

answers$Question <- factor(answers$Question, levels=rev(c("Q1 Method: \nClear & \nReproducible",
                                                          "Q2 Method: \nRobust & \nAppropriate",
                                                          "Q3 Assumptions: \nClear & \nReproducible",
                                                          "Q4 Assumptions: \nRobust & \nAppropriate",
                                                          "Q5 Data: \nClear & \nReproducible",
                                                          "Q6 Data: \nIssues \nAcknowledged",
                                                          "Q7 Data: \nIssues \nAccounted For")))

answers$Assessment[is.na(answers$Assessment)] <- "NA"
answers$Assessment <- factor(answers$Assessment,levels=c("NA","No","Yes"))

p1 <- ggplot(data=articles, aes(x = year_publication)) +
  geom_histogram(binwidth = 1, fill = "steelblue4", color = "black", alpha = 0.7) +
  scale_x_continuous(limits = c(1970,2025), breaks = seq(1970, 2023, by = 5), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0,95), expand = c(0, 0)) +
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA)) + 
  labs(x = "Year of Publication", y = "Article Count")

p2 <- ggplot() +
  geom_histogram(data=quality, aes(x = score), binwidth = 20, boundary = 0, fill = "steelblue4", color = "black", alpha = 0.7) +
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20)) + 
  scale_y_continuous(limits = c(0,210), expand = c(0, 0)) +
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA)) + 
  labs(x = "Quality Assessment Score (%)", y = "Article Count")

p3 <- ggplot() +
  geom_point(data = quality, aes(x=year_publication, y=score,color=category)) +
  geom_smooth(data = subset(quality, category == "Non-Modelling Studies"), aes(x=year_publication,y=score), span = 2, color = "springgreen3", fill = "springgreen3") +
  geom_smooth(data = subset(quality, category == "Modelling Studies"), aes(x=year_publication,y=score), span = 2, color = "red", fill = "red") +
  scale_x_continuous(limits = c(1969,2025), breaks = seq(1970, 2024, by=5), expand = c(0, 0.5)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20)) +
  xlab("Year of Publication") + ylab("Quality Assessment Score (%)") +  
  scale_color_manual(values = c("Non-Modelling Studies" = "springgreen3","Modelling Studies" = "red"), name = NULL) +
  theme_minimal() +      
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = 'bottom')

p4 <- answers %>%
  group_by(Question,Assessment) %>% summarize(count=n()) %>% ungroup() %>%
  ggplot(aes(fill=Assessment, y=count, x=Question)) + 
  geom_bar(position="stack", stat="identity") + theme_bw() +
  scale_y_continuous(limits = c(0,580), breaks = seq(0,580,by=40), expand = c(0, 0)) +
  scale_fill_manual(values = c("darkolivegreen2","coral1","grey70"),aesthetics = "fill",name="",breaks=c('Yes', 'No','NA')) +
  xlab("") + ylab("Article Count") + 
  coord_flip() +
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA)) +
  theme(legend.position = 'bottom')

patchwork <- (p1 + p2 + p3 + p4) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("figure_S1.png", plot = patchwork, width = 18, height = 12)
