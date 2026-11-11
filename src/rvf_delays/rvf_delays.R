# *=============================== RVF delays ===============================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(orderly)
library(patchwork)
library(readr)
library(stringr)
library(ggbreak)
library(epireview)


# *--------------------------------- Orderly ----------------------------------*
pars <- orderly_parameters(pathogen = "RVF")

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "params.csv", "outbreaks.csv"))

orderly_shared_resource("rvf_functions.R" = "rvf_functions.R")
source("rvf_functions.R")

orderly_artefact(description="RVF delay figures",
                 c("figure_delays.pdf",
                   "figure_delays.png",
                   "figure_delays.RData"))


# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

#Remove the words "human delay" throughout
parameters <- parameters |>
  mutate(parameter_type = str_replace(parameter_type, "Human delay - ", ""),
         parameter_type = str_to_sentence(parameter_type))

all_delays <- filter(parameters, parameter_class == "Human delay")

#Filter out all low-QA
all_delays_qa <- filter(all_delays, qa_score >= 0.5)

table(all_delays$parameter_type)

# Filter out everything but the delays
# (There was one "Generation Time" which has been removed in QA filtering)
all_delay_types <- c("Incubation period", #2
                     "Time in care (length of stay)", #0
                     "Serial interval", #0
                     "Other human delay (go to section)", #2
                     "Symptom onset>death", #0
                     "Symptom onset>admission to care", #4
                     "Admission to care>death", #1
                     "Symptom onset>discharge/recovery", #1
                     "Admission to care>discharge/recovery", #0
                     "Symptom onset/fever>discharge or death", #2
                     "Infectious period" #None
)
parameters <- parameters |>
  filter(parameter_type %in% all_delay_types)
parameters_qa <- filter(parameters, qa_score >= 0.5)

# covidence #5139 - only keep range variability extraction no SD
parameters <- parameters |> filter(access_param_id!="618_004")


write.csv(all_delays,row.names = FALSE,"delays.csv")

# LOAD country colour palette
load("~/priority-pathogens/shared/country_palette.RData")

# *--------------------------------- Summary ----------------------------------*
num_delays <- NROW(parameters)

cat("\nIn total", num_delays, "delays were extracted.")

cat("\nThe composition is as follows:\n")
parameters |>
  group_by(parameter_type) |>
  count() |>
  arrange(desc(n)) |>
  print()

# Rows with variability only
varb_only_rows <- parameters |>
  filter(is.na(parameter_value) &
           is.na(parameter_lower_bound) &
           is.na(parameter_upper_bound)) |>
  NROW()

varb_only_data <- filter(parameters, is.na(parameter_value) &
                           is.na(parameter_lower_bound) &
                           is.na(parameter_upper_bound))

cat("Number of variability only rows:", varb_only_rows)


# *------------------------------ Plot datasets -------------------------------*

# Incubation period
d1 <- parameters %>% filter(tolower(parameter_type) == 'incubation period')  

# Onset to admission
d2 <- parameters |>
  filter(tolower(parameter_type) == 'symptom onset>admission to care')  #25

# Hospital admission to outcome
d3 <- parameters |>
  filter(tolower(parameter_type) %in% c('admission to care>death')   #26 + 1 + 1
  )

# Symptom-onset to outcome
d4 <- parameters |>
  filter(tolower(parameter_type) %in% c(
    'symptom onset>discharge/recovery')
  )

# # Symptom-onset to death
# d5 <- parameters |>
#   filter(tolower(parameter_type) %in% c(
#     'symptom onset>death')
#   )

# Symptom Onset/Fever to Discharge or death
d6 <- parameters |>
  filter(tolower(parameter_type) %in% c(
    'symptom onset/fever>discharge or death')
  ) 


# *---------------------------------- Plots -----------------------------------*
lanonc_colours <- ggsci::pal_lancet("lanonc")(9)

# Plot properties
text_size <- 14

# We'll produce multiple plots to filter in/out low QA studies
qa_thresh_vec <- c("qa"=0.5)
qa_alpha_vec <- c(1)

# Incubation period:
d1$parameter_unit # NA needs fixing?
p1 <- forest_plot(d1,
                  "Incubation period",
                  "population_group", c(0,35),
                  text_size=text_size,
                  segment_show.legend = NA,
                  sort=TRUE,
                  qa_alpha =0.3) 
# Onset to care:
d2$parameter_unit
p2 <- forest_plot(d2,
                  "Symptom onset > admission to care",
                  "population_group", c(0,8),
                  text_size=text_size,
                  segment_show.legend = NA,
                  sort=TRUE,
                  qa_alpha =0.3)

# Admission to care>death:
d3$parameter_unit <- "Needs checking" # needs updating
p3 <- forest_plot(d3,
                  "Admission to care > death",
                  "population_group", c(0,35),
                  text_size=text_size,
                  segment_show.legend = NA,
                  sort=TRUE,
                  qa_alpha =0.3) 

# Symptom onset>discharge/recovery:
d4$parameter_unit
p4 <- forest_plot(d4,
                  "Symptom onset > discharge/recovery",
                  "population_group", c(-5,130),
                  text_size=text_size,
                  segment_show.legend = NA,
                  sort=TRUE,
                  qa_alpha =0.3) 

# # Symptom onset>death:
# d5$parameter_unit
# p5 <- forest_plot(d5,
#                   "Symptom onset > death",
#                   "population_group", c(0,35),
#                   text_size=text_size,
#                   segment_show.legend = NA,
#                   sort=TRUE,
#                   qa_alpha =0.3) 

# Symptom onset/fever > discharge or death:
d6$parameter_unit
p6 <- forest_plot(df=d6,
                  label="Symptom onset > discharge or death",
                  color_column = "population_group", 
                  lims = c(-5,130),
                  text_size=text_size,
                  segment_show.legend = NA,
                  sort=TRUE,
                  qa_alpha =0.3) 


# Save transmission plots
patchwork_delays <- p1+p2+p3+p6+p4+plot_layout(ncol=1)
patchwork_delays <- patchwork_delays + plot_annotation(tag_levels = 'A')

########################################################################################
########## Combine all delays 





delays_data <- parameters |> filter(tolower(parameter_type) %in% c('incubation period',
                                                                   'symptom onset>admission to care',
                                                                   'symptom onset>discharge/recovery',
                                                                  # 'symptom onset>death',
                                                                   'symptom onset/fever>discharge or death',
                                                                   'admission to care>death')) |> mutate(
                                                                     parameter_type_general = case_when(
                                                                       tolower(parameter_type)=='incubation period' ~ "Incubation period",
                                                                       .default = "Human delay"
                                                                     )
                                                                   )|>
      mutate(parameter_type_general=factor(parameter_type_general, levels=c("Incubation period", "Human delay")), 
             parameter_type = ifelse(parameter_type=='Symptom onset/fever>discharge or death','Symptom onset/fever>discharge/death',parameter_type ))


# add astrix to Mohammed (2010) for HIV positive group 
delays_data$refs[which(delays_data$access_param_id%in%c("0659_0003","0659_0005"))] <- paste(delays_data$refs[which(delays_data$access_param_id%in%c("0659_0003","0659_0005"))], "*", sep="")


#"#F8766D" "#CD9600" "#7CAE00" "#00BE67" "#00BFC4" "#00A9FF" "#C77CFF" "#FF61CC" 
country_colours <- c("Egypt" = "#00BE67" ,
                     "Saudi Arabia"=  "#00BFC4" ,
                     "South Africa" =  "#00A9FF" ,
                     "Uganda" = "#C77CFF" ,
                     "United Republic of Tanzania"  = "#FF61CC" 
)


human_delays <- delays_data|>#filter(parameter_type_general=="Human delay")|>
  mutate(start_delay = sub(">.*", "", parameter_type), end_delay = str_extract(parameter_type, "[^>]+$"), 
         parameter_type = factor(parameter_type, levels =c("Incubation period","Symptom onset>admission to care",#"Symptom onset>death",
                                                           "Symptom onset>discharge/recovery", "Symptom onset/fever>discharge/death",
                                                           "Admission to care>death") ))

## plot with x-axis break
types <- levels(human_delays$parameter_type)
resolved_strip <- calc_element("strip.text", theme_minimal(base_size = text_size))

library(patchwork)


base_delay_plot <- function(human_delays) {
  
  ## data 
  sub_df <- human_delays |>
    mutate(urefs = make.unique(refs)) |>
    mutate(urefs = factor(urefs, levels = rev(unique(urefs))),
           uncertainty_present = (
             !(is.na(parameter_uncertainty_lower_value) &
                 is.na(parameter_uncertainty_upper_value))))|> 
    arrange((population_country)) |>
    mutate(urefs = (fct_inorder(urefs)))
  
  ## Compute separator positions ---------------------------------------------
  
  plot_df <- sub_df |>
    mutate(y_pos = as.numeric(urefs))
  
  y_positions_location <- plot_df |>
    ## this reflects the within facet order 
    mutate(y_key = as.character(interaction(population_country, sep = "--"))) |>
    #distinct(population_sample_type, y_key, population_country) |>
    group_by(parameter_type) |>
    
    # factor level order is preserved because country_location_label is already a factor
    arrange(
      (population_country), parameter_type,
      interaction(factor(sub("--.*", "", y_key)), population_country),
      .by_group = TRUE
    ) |>
    mutate(y_pos = row_number()) |>
    ungroup()
  
  
  country_seps <- y_positions_location |>
    group_by(parameter_type, population_country) |>
    summarise(y_min = min(y_pos), y_max = max(y_pos), .groups = "drop") |>
    group_by(parameter_type) |>
    arrange(y_min,desc(population_country), .by_group = TRUE) |>
    mutate(y_sep = y_min - 0.5) |>
    ungroup()
  
  
  # country_seps <- plot_df |>
  #   group_by(population_country,parameter_type) |>
  #   summarise(
  #     y_min = min(y_pos),
  #     y_max = max(y_pos),
  #     .groups = "drop"
  #   ) |>#
  #   group_by(parameter_type) |>
  #   arrange(desc(y_max)) |>
  #   mutate(y_sep = y_min - 0.5)
  
  ## Build plot --------------------------------------------------------------
  
  ggplot(sub_df) +
    
    geom_segment(
      aes(x = parameter_lower_bound,
          xend = parameter_upper_bound,
          y = urefs,
          yend = urefs,
          colour = population_country),
      linewidth = 3, alpha = 0.75
    ) +
    
    geom_errorbar(
      aes(xmin = parameter_uncertainty_lower_value,
          xmax = parameter_uncertainty_upper_value,
          y = urefs,
          linetype = "Uncertainty"),
      width = .25,
      colour = "black"
    ) +
    
    geom_errorbar(
      data = subset(sub_df, !uncertainty_present),
      aes(xmin = parameter_2_lower_bound,
          xmax = parameter_2_upper_bound,
          y = urefs,
          linetype = "Variability"),
      width = .25,
      colour = "black"
    ) +
    
    geom_errorbar(
      data = subset(sub_df, uncertainty_present),
      aes(xmin = parameter_2_lower_bound,
          xmax = parameter_2_upper_bound,
          y = urefs,
          linetype = "Variability"),
      position = position_nudge(y = -.25),
      width = .25,
      colour = "black"
    ) +
    
    geom_point(
      aes(x = parameter_value,
          y = urefs,
          shape = parameter_statistical_approach,
          fill = population_country),
      size = 3,
      colour = "black"
    ) +
    
    ## facet by delay
    facet_wrap(~parameter_type, ncol=1, scales= "free_y", space="free_y") + 
    
    # LEGENDS
    scale_color_manual(values = country_palette) +
    scale_fill_manual(values = country_palette) +
    scale_shape_manual(name = "Statistical approach",
                       values = c(`Observed sample statistic` = 21,
                                  `Estimated parameter` = 22, 
                                  `Unknown` = 23, 
                                  `Unspecified` = 24, `Other`=25),
                       breaks = c("Observed sample statistic", "Estimated parameter", "Unspecified", "Other",
                                  "Unknown")) +
    scale_linetype_manual(name   = "Variation Type",
                          values = c("Uncertainty" = "solid","Variability" = "dashed"),
                          breaks = c("Uncertainty", "Variability")) +
    # ## Country separators
    geom_segment(
      data = country_seps |> filter(y_min > min(y_min)),
      aes(y = y_sep, yend = y_sep,
          x = -Inf, xend = Inf),
      inherit.aes = FALSE,
      colour = "grey50",
      linewidth = .2
    ) +
    
    ## Country labels
    geom_text(
      data = country_seps,
      aes(y = y_max + .35,
          x = 0,
          label = population_country),
      inherit.aes = FALSE,
      hjust = 0,
      fontface = "bold",
      size = 2.5
    ) +
    
    # theme 
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA),
          text = element_text(size = text_size),
          legend.position = "none", plot.title = element_text(
            size   = resolved_strip$size,
            face   = resolved_strip$face,
            colour = resolved_strip$colour,
            hjust  = 0.5
          ), plot.margin = margin(5, 10, 5, 10)) +
    
    labs (x=NULL, y=NULL) +
    
    scale_x_continuous( breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,100,110,115,120,125,130)) +
    
    scale_y_discrete(labels = setNames(sub_df$refs, sub_df$urefs)) 
}


make_delay_plot <- function(human_delays){
  
  base <- base_delay_plot(human_delays)
  
  left <-
    base +
    coord_cartesian(xlim = c(0,40), clip = "on") +
    labs(x="Delay (days)")
  
  right <-
    base +
    coord_cartesian(xlim = c(110,125), clip = "on") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      strip.text = element_text(colour = "white"),
      plot.margin = margin(5, r=10, 5, l=1)
    ) + ggtitle(NULL) 
  
  left + right +
    plot_layout(widths = c(4,1.5))
}

stacked_delay <- make_delay_plot(human_delays) #lapply(types, make_delay_plot)

stacked_delay


# add legend space
text_box <- textGrob("place holder", gp = gpar(fontsize = 16))
delays_final <- plot_grid(
  stacked_delay, text_box,
  ncol = 1, 
  rel_heights = c(1, 0.05)   # tune this ratio to give the legend enough room
)


# Figure with x-axis breaks
ggsave("figure_delays.png", plot = stacked_delay, width = 10, height = 10)
ggsave("figure_delays.pdf", plot = stacked_delay, width = 10, height = 10)


save(file="figure_delays.RData",delays_final)

