#task to create sars latex tables

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
orderly_shared_resource("lassa_functions.R" = "lassa_functions.R")
source("lassa_functions.R")

orderly_artefact(files = c("figure_SI_risk_other.pdf", 
                           "figure_SI_risk_panel.pdf"))

###################
## DATA CURATION ##
###################

articles   <- read_csv("articles.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("parameters.csv")

dfs <- data_curation(articles,tibble(),models,parameters, plotting = FALSE )

articles   <- dfs$articles
models     <- dfs$models
parameters <- dfs$parameters

#parameters
parameters <- parameters %>% mutate(method_disaggregated_by = gsub(", ", ";", method_disaggregated_by),
                                    population_country = gsub( ",", ";", population_country))

parameters <- parameters %>% mutate(parameter_unit = replace_na(parameter_unit,""))
parameters <- mutate_at(parameters, 
                        vars(parameter_value, parameter_lower_bound, parameter_upper_bound, 
                             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value, 
                             parameter_uncertainty_single_value, distribution_par1_value, distribution_par2_value),
                        ~sub("\\.?0+$", "", sprintf("%.10f", round(., 10))))
##
parameters <- parameters %>%
  mutate(parameter_value=
           ifelse(parameter_value!="NA", 
                  parameter_value,
                  ifelse(parameter_lower_bound!="NA" & parameter_upper_bound!="NA",
                         paste(parameter_lower_bound, parameter_upper_bound, sep = " - "), "")))
#
parameters$parameter_unit <- gsub("Substitutions/site/year", "s/s/y", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Mutations/genome/generation \\(U\\)", "m/g/g", parameters$parameter_unit)
parameters$parameter_unit <- gsub("No units", "", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Per day", "per day", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Per hour", "per hour", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Percentage \\(%\\)", "\\\\%", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Days", "days", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Weeks", "weeks", parameters$parameter_unit)
parameters$parameter_unit <- gsub("Max. nr. of cases superspreading \\(related to case\\)", "mnc", parameters$parameter_unit)

parameters <- parameters %>% mutate(parameter_unit = case_when(
  exponent == 0 ~ parameter_unit,
  exponent == -2 & parameter_unit == "" ~ "\\\\%",
  exponent == -3 & parameter_unit == "" ~ "per 1000",
  exponent == -4 & parameter_unit == "" ~ "per 10k",
  TRUE ~ paste(parameter_unit, sprintf("$10^{%d}$",exponent), sep=" ")))
# new bit
parameters <- parameters %>% mutate(parameter_value = case_when(
  (parameter_class %in% c("Severity","Seroprevalence") | parameter_unit == "") ~ parameter_value,
  TRUE ~ paste(parameter_value, parameter_unit, sep = " ")))
# parameters <- parameters %>%
#               mutate(parameter_unit = ifelse(parameter_unit == "Percentage (%)", "%", parameter_unit),
#                      values = paste0(values, parameter_unit, sep = " "))
parameters <- parameters %>%
  mutate(unc_type=
           ifelse(!is.na(distribution_par2_type), 
                  paste(distribution_type, distribution_par2_type, sep = " "), 
                  ifelse(!is.na(parameter_uncertainty_type),
                         paste(parameter_uncertainty_type),    
                         ifelse(!is.na(parameter_uncertainty_singe_type),
                                paste(parameter_uncertainty_singe_type),""))))
parameters$unc_type <- gsub("Inter Quartile Range \\(IQR\\)", "IQR", parameters$unc_type)
parameters$unc_type <- gsub("IQR \\[Estimator\\]", "IQR_e", parameters$unc_type)
parameters$unc_type <- gsub("IQR \\[Sample\\]", "IQR_s", parameters$unc_type)
parameters$unc_type <- gsub("Range \\[Estimator\\]", "Range_e", parameters$unc_type)
parameters$unc_type <- gsub("Range \\[Sample\\]", "Range_s", parameters$unc_type)
parameters$unc_type <- gsub("Standard Error \\(SE\\)", "SE", parameters$unc_type)
parameters$unc_type <- gsub("Gamma Standard deviation", "Gamma SD", parameters$unc_type)
parameters$unc_type <- gsub("Normal Standard deviation", "Normal SD", parameters$unc_type)
parameters$unc_type <- gsub("Normal-Log Variance", "Normal-Log var", parameters$unc_type)
parameters$unc_type <- gsub("Standard deviation \\(Sd\\)", "SD", parameters$unc_type)
parameters$unc_type <- gsub("SD \\[Estimator\\]", "SD_e", parameters$unc_type)
parameters$unc_type <- gsub("SD \\[Sample\\]", "SD_s", parameters$unc_type)
parameters$unc_type <- gsub("Highest Posterior Density Interval 95%", "HPDI95%", parameters$unc_type)
parameters$unc_type <- gsub("CRI95%", "CrI95%", parameters$unc_type)
parameters$unc_type <- gsub("%", "\\\\%", parameters$unc_type)
parameters$unc_type <- gsub("_", "\\\\_", parameters$unc_type)
#
parameters <- parameters %>%
  mutate(uncertainty=
           ifelse(distribution_par2_value!="NA",
                  paste(distribution_par2_value),
                  ifelse(parameter_uncertainty_lower_value!="NA" & parameter_uncertainty_upper_value!="NA",
                         paste(parameter_uncertainty_lower_value, parameter_uncertainty_upper_value, sep = "-"),
                         ifelse(parameter_uncertainty_single_value!="NA",
                                paste(parameter_uncertainty_single_value),
                                ""))))
# new bit
parameters <- parameters %>% mutate(unc_type = case_when(
  unc_type %in% c("Unspecified","") ~ "",
  TRUE ~  paste(unc_type, ": ", uncertainty, sep = "")))
##                     
#parameters$cfr_ifr_denominator[is.na(parameters$cfr_ifr_denominator)] <- 
#                                     parameters$population_sample_size[is.na(parameters$cfr_ifr_denominator)]        
parameters$population_country <- gsub(",", "", parameters$population_country)
#new as well
parameters <- parameters %>%
  mutate(population_study_start_month = substr(population_study_start_month, 1, 3),
         population_study_end_month   = substr(population_study_end_month, 1, 3)) %>%
  mutate(dates = paste(population_study_start_day,population_study_start_month,population_study_start_year,"-",
                       population_study_end_day,population_study_end_month,population_study_end_year),
         dates = gsub("NA","",dates),
         dates = trimws(gsub("\\s{2,}"," ",dates)),
         dates = gsub("\\b(\\d{4})\\s*-\\s*\\1\\b","\\1",dates,perl = TRUE),
         dates = ifelse(dates == "-","",dates)) %>%
  mutate(population_study_start_day   = coalesce(population_study_start_day,1),
         population_study_start_month = ifelse(is.na(population_study_start_month),"Jan",population_study_start_month),
         population_study_start_year  = coalesce(population_study_start_year,0),
         sdate = as.Date(paste(population_study_start_day,population_study_start_month,
                               population_study_start_year, sep = "-"), format = "%d-%b-%Y")) %>%
  mutate(population_study_end_day   = coalesce(population_study_end_day,28),
         population_study_end_month = ifelse(is.na(population_study_end_month),"Dec",population_study_end_month),
         population_study_end_year = coalesce(population_study_end_year,2024),
         edate = as.Date(paste(population_study_end_day,population_study_end_month,
                               population_study_end_year, sep = "-"), format = "%d-%b-%Y"))
#
parameters$population_sample_type <- gsub("\\s.*", "", parameters$population_sample_type)
parameters$population_group <- str_to_title(parameters$population_group)
parameters$method_disaggregated_by <- gsub("Disease generation","Disease Generation",parameters$method_disaggregated_by)
parameters$method_disaggregated_by <- gsub("Level of exposure","Level of Exposure",parameters$method_disaggregated_by)
parameters <- parameters %>% mutate_all(~ ifelse(is.na(.), "", .))


#parameters - risk factors
risk_params <- parameters %>%
  filter(grepl("Risk factors", parameter_type, ignore.case = TRUE)) %>%
  select(riskfactor_outcome, 
         riskfactor_name,	riskfactor_significant, 
         riskfactor_adjusted, population_sample_size,
         population_country, dates,
         population_sample_type, population_group, refs)

risk_table <- risk_params %>%
  separate_longer_delim(riskfactor_name, delim = ";") %>% 
  mutate(population_sample_size = replace_na(as.double(population_sample_size),0),
         riskfactor_adjusted    = case_when(riskfactor_adjusted=='' ~ 'Unspecified',
                                            TRUE ~ riskfactor_adjusted)) %>%
  group_by(riskfactor_outcome,riskfactor_name,riskfactor_significant,riskfactor_adjusted) %>%
  summarise(n=n(),
            pop_size = sum(population_sample_size)) %>%
  unite(`Significant / Adjusted`,riskfactor_significant:riskfactor_adjusted, remove = FALSE, sep = " / ")

text_size <- 14
custom_colours <- c('Significant / Adjusted'='blue4', 'Significant / Not adjusted' = 'lightblue', 'Significant / Unspecified'='blue',
                    'Not significant / Adjusted'='darkred', 'Not significant / Not adjusted' = 'pink', 'Not significant / Unspecified'='red',
                    'Unspecified / Adjusted'='grey30', 'Unspecified / Not adjusted' = 'grey50', 'Unspecified / Unspecified'='grey70')

# Zika congenital syndrome/other birth defects
risk_table_plt_zcs <- risk_table %>% filter(riskfactor_outcome=='Zika congenital syndrome/other birth defects') %>%
  ggplot(aes(x=riskfactor_name,y=n,col=`Significant / Adjusted`, fill=`Significant / Adjusted`)) + 
  geom_bar( stat='identity',position = position_dodge(preserve = "single")) +
  scale_color_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) + xlab('') + ylab('') + theme_light() + 
  theme( axis.text.x = element_text( angle = 25, hjust = 1, size = text_size ),
         strip.text = element_text( color = "black"), 
         text = element_text(size = text_size),
         strip.background =element_rect(fill="grey90"))


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


#Microcephaly                            
risk_table_plt_microcephaly <- risk_table %>% filter(riskfactor_outcome=='Microcephaly') %>%
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

#Serology                             
risk_table_plt_serology  <- risk_table %>% filter(riskfactor_outcome=='Serology') %>%
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

# Everything else
risk_table_plt_other <- risk_table %>% 
  filter(!(riskfactor_outcome %in% c('Infection', 'Microcephaly','Serology','Zika congenital syndrome/other birth defects'))) %>%
  ggplot(aes(x = riskfactor_name, y = n,
             col = `Significant / Adjusted`,
             fill = `Significant / Adjusted`)) + 
  geom_bar(stat='identity',
           position = position_dodge(preserve = "single")) +
  scale_color_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) + xlab('') + ylab('') + theme_light() + 
  theme( axis.text.x = element_text( angle = 65, hjust = 1, size = text_size ),
         strip.text = element_text( color = "black"),          
         strip.background =element_rect(fill="grey90"),
         text = element_text(size = text_size)) +
  facet_wrap(~riskfactor_outcome)

risk_table_plt <- risk_table_plt_infection / risk_table_plt_zcs / risk_table_plt_microcephaly / risk_table_plt_serology +
  plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect")

ggsave("figure_SI_risk_panel.pdf", plot = risk_table_plt, width = 17, height = 20)
ggsave("figure_SI_risk_other.pdf", plot = risk_table_plt_other, width = 20, height = 16)

ggsave("figure_SI_risk_panel.png", plot = risk_table_plt, width = 17, height = 20)
ggsave("figure_SI_risk_other.png", plot = risk_table_plt_other, width = 20, height = 16)
