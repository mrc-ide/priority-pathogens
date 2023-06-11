## function to produce forest plot
## to go into data_analysis.R when up and running

# doing on 'parameter_single' until get finalised data

REGENERATE_DATA <- FALSE

if(REGENERATE_DATA) source("R/marburg/script_marburg.R")              #only rerun this if we want to re-generate the marburg files (rather than rewriting this every time) 

source("R/forest_plot.R")

param_df    <- read.csv("data/marburg/final/parameter_final.csv")
outbreak_df <- read.csv("data/marburg/final/outbreak_final.csv")
article_df  <- read.csv("data/marburg/final/article_clean.csv")

# merge with article ID article labels
df <- merge(param_df, article_df %>% dplyr::select(article_id, first_author_first_name,year_publication),
            all.x=TRUE, by="article_id") %>%
  mutate(article_label = as.character(paste0(first_author_first_name," ",year_publication)),
         population_country = str_replace_all(population_country,";",", ")) %>%
  dplyr::arrange(article_label, -year_publication) %>%
  dplyr::filter(article_id %in% c(17,15) == FALSE) %>%
  rename('Survey year'=Survey.year) %>%
  mutate(parameter_uncertainty_lower_value=replace(parameter_uncertainty_lower_value, (parameter_uncertainty_type=="Range"&!is.na(parameter_lower_bound)&parameter_class=="Human delay"), NA),
         parameter_uncertainty_upper_value=replace(parameter_uncertainty_upper_value, (parameter_uncertainty_type=="Range"&!is.na(parameter_upper_bound)&parameter_class=="Human delay"), NA)) %>%
  rowwise() %>% mutate(parameter_uncertainty_lower_value=replace(parameter_uncertainty_lower_value, parameter_data_id==43,parameter_uncertainty_lower_value* 1e-4),   # need to adjust for scaling
         parameter_uncertainty_upper_value=replace(parameter_uncertainty_upper_value, parameter_data_id==43,parameter_uncertainty_upper_value* 1e-4)) %>%             # need to adjust for scaling
  mutate(parameter_value=replace(parameter_value,parameter_data_id==34,0.93),
         cfr_ifr_method=replace(cfr_ifr_method,str_starts(parameter_type,"Severity")&is.na(cfr_ifr_method),"Unknown"))

df_out <- merge(outbreak_df, article_df %>% dplyr::select(article_id, first_author_first_name,year_publication),
                all.x=TRUE, by="article_id") %>%
  mutate(article_label = as.character(paste0(first_author_first_name," ",year_publication)),
         outbreak_country = str_replace_all(outbreak_country,";",", ")) %>%
  dplyr::arrange(article_label, -year_publication) %>%
  dplyr::filter(article_id %in% c(17,15) == FALSE) %>%
  filter(!is.na(deaths)&!is.na(cases_confirmed)&cases_confirmed>=10) %>% 
  mutate(cases_suspected=replace_na(cases_suspected,0),
         parameter_value= deaths/(cases_confirmed+cases_suspected)*100,
         cfr_ifr_numerator=deaths,
         cfr_ifr_denominator=cases_confirmed+cases_suspected,
         cfr_ifr_method="Naive",
         parameter_class="Severity",
         parameter_type="Severity - case fatality rate (CFR)",
         parameter_uncertainty_lower_value = NA,
         parameter_uncertainty_upper_value = NA,
         outbreak_year_cnt = paste(outbreak_country,outbreak_date_year))
df_out$parameter_data_id <- seq(1,dim(df_out)[1],1)


human_delay <- forest_plot_delay(df)
#ggsave(plot = human_delay,filename="data/marburg/output/FP_human_delay.png",bg = "white",width = 25, height = 15, units = "cm")

mutations <- forest_plot_mutations(df)
#ggsave(plot = mutations,filename="data/marburg/output/FP_mutations.png",bg = "white",width = 15, height = 10, units = "cm")


reproduction_number <- forest_plot_R(df)
#ggsave(plot = reproduction_number,filename="data/marburg/output/FP_reproduction_number.png",bg = "white",width = 15, height = 10, units = "cm")

severity_params <- forest_plot_fr(df) 
#ggsave(plot = severity,filename="data/marburg/output/FP_severity.png",bg = "white",width = 15, height = 10, units = "cm")
severity_outbreaks <- forest_plot_fr(df_out)

plot_grid(reproduction_number+labs(tag="A"),
          #severity+labs(tag="B"),
          human_delay+labs(tag="B"),
          mutations+labs(tag="C"),
          nrow=3,align="hv",rel_heights = c(0.7,1))
ggsave(filename="data/marburg/output/panel_plot.png",bg = "white",width = 12.5, height=15)


#panel_plot <- (reproduction_number + ggtitle("A") + severity + ggtitle("B")) / (human_delay + ggtitle("C") + mutations + ggtitle("D"))
#ggsave(plot = panel_plot,filename="data/marburg/output/panel_plot.png",bg = "white",width = 15, height=10)




