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
pars <- orderly_parameters(pathogen = NULL)

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
all_delay_types <- c("Incubation period", #0
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

write.csv(all_delays,row.names = FALSE,"delays.csv")


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

# Symptom-onset to death
d5 <- parameters |>
  filter(tolower(parameter_type) %in% c(
    'symptom onset>death')
  )

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
                  "population_group", c(0,35),
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

# Symptom onset>death:
d5$parameter_unit
p5 <- forest_plot(d5,
                  "Symptom onset > death",
                  "population_group", c(0,35),
                  text_size=text_size,
                  segment_show.legend = NA,
                  sort=TRUE,
                  qa_alpha =0.3) 

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
patchwork_delays <- p1+p2+p3+p5+p6+p4+plot_layout(ncol=1)
patchwork_delays <- patchwork_delays + plot_annotation(tag_levels = 'A')

########################################################################################
########## Combine all delays 





delays_data <- parameters |> filter(tolower(parameter_type) %in% c('incubation period',
                                                                   'symptom onset>admission to care',
                                                                   'symptom onset>discharge/recovery',
                                                                   'symptom onset>death',
                                                                   'symptom onset/fever>discharge or death',
                                                                   'admission to care>death')) |> mutate(
                                                                     parameter_type_general = case_when(
                                                                       tolower(parameter_type)=='incubation period' ~ "Incubation period",
                                                                       .default = "Human delay"
                                                                     )
                                                                   )|>
      mutate(parameter_type_general=factor(parameter_type_general, levels=c("Incubation period", "Human delay")), 
             parameter_type = ifelse(parameter_type=='Symptom onset/fever>discharge or death','Symptom onset/fever>discharge/death',parameter_type ))


# add atrix to Mohammed (2010) for HIV positive group 
delays_data$refs[which(delays_data$access_param_id%in%c("659_003","659_005"))] <- paste(delays_data$refs[which(delays_data$access_param_id%in%c("659_003","659_005"))], "*", sep="")


#"#F8766D" "#CD9600" "#7CAE00" "#00BE67" "#00BFC4" "#00A9FF" "#C77CFF" "#FF61CC" 
country_colours <- c("Egypt" = "#00BE67" ,
                     "Saudi Arabia"=  "#00BFC4" ,
                     "South Africa" =  "#00A9FF" ,
                     "Uganda" = "#C77CFF" ,
                     "United Republic of Tanzania"  = "#FF61CC" 
)


human_delays <- delays_data|>#filter(parameter_type_general=="Human delay")|>
  mutate(start_delay = sub(">.*", "", parameter_type), end_delay = str_extract(parameter_type, "[^>]+$"), 
         parameter_type = factor(parameter_type, levels =c("Incubation period","Symptom onset>admission to care","Symptom onset>death",
                                                           "Symptom onset>discharge/recovery", "Symptom onset/fever>discharge/death",
                                                           "Admission to care>death") ))

## plot with x-axis break
types <- unique(human_delays$parameter_type)
resolved_strip <- calc_element("strip.text", theme_minimal(base_size = text_size))

plots <- lapply(types, function(pt) {
  sub_df <- human_delays[human_delays$parameter_type == pt, ]
  if(pt==types[6]){
    sub_df$parameter_unit <- sub_df$parameter_2_unit
    x_lab <- "Delays (days)"
  }
  p <- forest_plot(df = sub_df, label = "Delay (days)",
                   color_column = "population_country",
                   custom_colours = country_colours,
                   lims = c(0, 125), text_size = text_size,
                   sort = TRUE, qa_alpha = 0.3) +
    scale_x_break(c(40, 110), scales = 0.25) +
    scale_x_continuous(limits = c(0, 125),
                       breaks = c(0,10,20,30,40,50,100,110,120,130)) +
    ggtitle(pt) +               # replaces the facet strip
    theme(legend.position = "none", plot.title = element_text(
      size   = resolved_strip$size,
      face   = resolved_strip$face,
      colour = resolved_strip$colour,
      hjust  = 0.5
    ),
    plot.margin = margin(5, 10, 5, 10)) + 
    labs(x=NULL)
  p
})

# add x-axis label 
plots[[6]] <- plots[[6]] +
  labs(x="Delay (days)")
library(cowplot)


library(grid)

# find the longest y-axis label across ALL panels combined
max_label_width <- max(sapply(plots, function(p) {
  # extract the labels used in that plot's data
  labs <- unique(p$data$refs)  # adjust to whatever column holds your y-axis labels
  max(nchar(labs))
}))

# apply an identical left margin to every plot, sized generously for the longest label
plots <- lapply(plots, function(p) {
  p + theme(plot.margin = margin(5, 10, 5, 5 + max_label_width * 2.2)) 
  # the *2.2 multiplier is a rough points-per-character estimate at your text_size;
  # tune this by eye rather than trusting it exactly
})

# NOW grab each one as a grob, using the SAME width/height for every capture
target_width  <- 10   # inches, matching your final save width
target_height <- 2    # inches per panel, matching final save height / n_panels
# convert each ggbreak plot into a fully-rendered grob
plots_as_grobs <- lapply(plots, function(p) {
  grid.grabExpr(print(p), width = target_width, height = target_height)
})


# stack plots
stacked_delay <- plot_grid(
  plotlist = plots_as_grobs,     # your list of legend-less broken-axis plots
  ncol = 1, 
 # rel_heights = c(1.3, 1.3, 1.2, 1.2, 1.1, 1.3),
  align = "v"           # align panels vertically so x-axes line up
)

# add legend space
text_box <- textGrob("place holder", gp = gpar(fontsize = 16))
delays_final <- plot_grid(
  stacked_delay, text_box,
  ncol = 1, 
  rel_heights = c(1, 0.1)   # tune this ratio to give the legend enough room
)


# Fugure with x-axis breaks
ggsave("figure_breaks.png", plot = stacked_delay, width = 10, height = 10)
ggsave("figure_breaks.pdf", plot = stacked_delay, width = 10, height = 10)
# # Figure with all delays facet
# ggsave("figure_facet.png", plot = delays_all_qa_facet, width = 10, height = 10)
# ggsave("figure_facet.pdf", plot = delays_all_qa_facet, width = 10, height = 10)

save(file="figure_delays.RData",delays_final,stacked_delay)

