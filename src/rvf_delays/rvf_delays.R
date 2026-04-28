# *=============================== RVF delays ===============================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(orderly)
library(patchwork)
library(readr)
library(stringr)
library(ggbreak)

# *--------------------------------- Orderly ----------------------------------*
orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "models.csv", "params.csv"))

orderly_shared_resource("mers_functions.R" = "mers_functions.R")
source("mers_functions.R")

# orderly_artefact(description="Nipah delay figures",
#                  c("figure_5_delays.pdf",
#                    "figure_5_delays.png",
#                    "qa/figure_5_admis_outcome_pt.pdf",
#                    "qa/figure_5_incubation_pc.pdf",
#                    "qa/figure_5_incubation_pg.pdf",
#                    "qa/figure_5_incubation_pst.pdf",
#                    "qa/figure_5_onset_admis_outcome_pt.pdf",
#                    "qa/figure_5_onset_admis_pc.pdf",
#                    "qa/figure_5_onset_admis_pg.pdf",
#                    "qa/figure_5_onset_admis_pst.pdf",
#                    "qa/figure_5_onset_death_pc.pdf",
#                    "qa/figure_5_onset_death_pg.pdf",
#                    "qa/figure_5_onset_death_pst.pdf",
#                    "qa/figure_5_onset_outcome_pt.pdf"))

browser()

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- tibble()
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

## 320_002 has population_group as NA
## This breaks forest_plot, so we change it to "Unspecified"
# parameters[parameters$access_param_id=="320_002",
#           "population_group"] <- "Unspecified"

#390_002 and 390_003 are "per days", we have to change their parameter_unit to "Days" to not break forest_plot
# parameters[parameters$access_param_id=="390_002",
#            "parameter_unit"] <- "Days"
# parameters[parameters$access_param_id=="390_003",
#            "parameter_unit"] <- "Days"

#271-001 is a gamma distribution but with reported mean 6.99 (unspecified units)
#It's also low-QA. For now I'm going to manually change it to days though
# parameters[parameters$access_param_id=="271_001",
#            "parameter_unit"] <- "Days"

#032-001 is also "Unspecified"
#It's a decent study, but never explicity SAYS "Days"
# parameters[parameters$access_param_id=="032_001",
#            "parameter_unit"] <- "Days"

#261-001 extracts it's units as "Months", but it is also an "Other" delay, so look at this later
#(It's high QA)
#TODO: Look at this

#Remove the words "human delay" throughout
parameters <- parameters |>
  mutate(parameter_type = str_replace(parameter_type, "Human delay - ", ""),
         parameter_type = str_to_sentence(parameter_type))

all_delays <- filter(parameters, parameter_class == "Human delay")

#Filter out all low-QA
all_delays <- filter(all_delays, qa_score >= 0.5)

table(all_delays$parameter_type)

# Filter out everything but the delays
# (There was one "Generation Time" which has been removed in QA filtering)
all_delay_types <- c("Incubation period", #27
                     "Time in care (length of stay)", #27
                     "Serial interval", #8
                     "Other human delay (go to section)", #140!
                     "Symptom onset>death", #14
                     "Symptom onset>admission to care", #25
                     "Admission to care>death", #1
                     "Symptom onset>discharge/recovery", #6
                     "Admission to care>discharge/recovery", #1
                     "Infectious period" #None
)
parameters <- parameters |>
  filter(parameter_type %in% all_delay_types)
parameters <- filter(parameters, qa_score >= 0.5)
# *--------------------------------- Summary ----------------------------------*
num_delays <- NROW(parameters)

cat("\nIn total", num_delays, "delays were extracted.")

cat("\nThe composition is as follows:\n")
parameters |>
  group_by(parameter_type) |>
  count() |>
  arrange(desc(n)) |>
  print()

#Extract start and end from "Other delays"
parameters |>
  filter(parameter_type == "Other human delay (go to section)") |>
  select(parameter_type, other_delay_start, other_delay_end) |>
  print()

# rows with variability only
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
# Filter out the 4 variability only rows:
#TODO: Return to this and think about maybe keeping
parameters <- parameters |>
  filter(!is.na(parameter_value) |
           !is.na(parameter_lower_bound) |
           !is.na(parameter_upper_bound))

#Let's re-assign all the country tags
# parameters <- parameters |>
#   mutate(population_country=ifelse(population_country=="Algeria; Austria; Bahrain; China; Egypt; France; Germany; Greece; Iran (Islamic Republic of); Italy; Jordan; Kuwait; Lebanon; Malaysia; Netherlands; Oman; Philippines; Qatar; Republic of Korea; Saudi Arabia; Thailand; Tunisia; Türkiye; United Arab Emirates; United Kingdom of Great Britain and Northern Ireland; United States of America; Yemen",
#                                    "Global", population_country)) |>
#   mutate(population_country=ifelse(population_country=="Algeria; Egypt; Germany; Greece; Italy; Netherlands; Philippines; Thailand; United States of America",
#                                    "Global", population_country)) |>
#   mutate(population_country=ifelse(population_country=="Bahrain; Kuwait; Qatar; Saudi Arabia; United Arab Emirates; Yemen",
#                                    "Other (Middle East)", population_country)) |>
#   mutate(population_country=ifelse(population_country=="Democratic People's Republic of Korea; Republic of Korea; Saudi Arabia",
#                                    "Other", population_country)) |>
#   mutate(population_country=ifelse(population_country=="Oman",
#                                    "Other (Middle East)", population_country)) |>
#   mutate(population_country=ifelse(population_country=="Oman; Saudi Arabia",
#                                    "Other (Middle East)", population_country)) |>
#   mutate(population_country=ifelse(population_country=="Qatar",
#                                    "Other (Middle East)", population_country)) |>
#   mutate(population_country=ifelse(population_country=="United Arab Emirates",
#                                    "Other (Middle East)", population_country)) |>
#   mutate(population_country=ifelse(population_country=="Republic of Korea; Saudi Arabia",
#                                    "Other", population_country))

# Incubation period
d1 <- parameters %>% filter(tolower(parameter_type) == 'incubation period')  #26

# Onset to admission
d2 <- parameters |>
  filter(tolower(parameter_type) == 'symptom onset>admission to care')  #25

# Hospital admission to outcome
d3 <- parameters |>
  filter(tolower(parameter_type) %in% c('time in care (length of stay)',
                                        'admission to care>discharge/recovery',
                                        'admission to care>death')   #26 + 1 + 1
  )

# Symptom-onset to outcome
d4 <- parameters |>
  filter(tolower(parameter_type) %in% c(
    "symptom onset>admission to care",  #25
    'symptom onset>discharge/recovery',  #6
    'symptom onset>death',  #12
    'symptom onset>recovery/death',  #0
    "symptom onset>severe illness")
  )

d5 <- parameters %>% filter(tolower(parameter_type) == 'serial interval')  #8

# Combine before updating variable names
d6 <- d3 |>
  bind_rows(d4 |>
              filter(parameter_type!="Symptom onset>death"))

# Convert to factors
d3 <- d3 |>
  mutate(parameter_type=factor(parameter_type,
                               levels=c("Time in care (length of stay)",
                                        "Admission to care>discharge/recovery",
                                        "Admission to care>death"),
                               labels=c("Time in care",
                                        "Discharge/recovery",
                                        "Death")))

d4 <- d4 |>
  mutate(parameter_type=factor(parameter_type,
                               levels=c("Symptom onset>admission to care",
                                        "Symptom onset>severe illness",
                                        'Symptom onset>death',
                                        "Symptom onset>recovery/death",
                                        "Symptom onset>discharge/recovery"),
                               labels=c("Admission",
                                        "Severe illness",
                                        "Death",
                                        "Recovery/death",
                                        "Discharge/recovery")))

d6 <- d6 |>
  mutate(parameter_type=factor(parameter_type,
                               levels=c("Symptom onset>admission to care",
                                        "Symptom onset>severe illness",
                                        "Symptom onset>recovery/death",
                                        "Symptom onset>discharge/recovery",
                                        "Admission to care>death",
                                        "Admission to care>discharge/recovery",
                                        "Time in care (length of stay)"),
                               labels=c("Onset>admission",
                                        "Onset>severe illness",
                                        "Onset>recovery/death",
                                        "Onset>discharge/recovery",
                                        "Admission>death",
                                        "Admission>discharge/recovery",
                                        "Time in care (length of stay)")))
# *---------------------------------- Plots -----------------------------------*
lanonc_colours <- ggsci::pal_lancet("lanonc")(9)

# Plot properties
text_size <- 28

# We'll produce multiple plots to filter in/out low QA studies
qa_thresh_vec <- c("qa"=0.5)
qa_alpha_vec <- c(1)

labels <- c("")
colour_columns <- c("parameter_type",
                    "population_group",
                    "population_country",
                    "population_sample_type")

p1_incb_plots <- list("qa"=list())
p2_oa_plots <- list("qa"=list())
p3_ao_plots <- list("qa"=list())
p4_oo_plots <- list("qa"=list())
p5_si_plots <- list("qa"=list())
p6_oa_o_plots <- list("qa"=list())
p7_o_a_plots <- list("qa"=list())
p7_oo_reduced_plots <- list("qa"=list())
p8_oo_plots <- list("qa"=list())

for (i in seq_along(qa_thresh_vec)){
  label <- labels[i]
  qa_threshold <- qa_thresh_vec[i]
  qa_alpha <- qa_alpha_vec[i]
  plot_type <- names(qa_thresh_vec)[i]
  dir.create(plot_type, showWarnings = FALSE)
  for (colour_col in colour_columns){
    if (colour_col!= "parameter_type"){
      # Same colours if not param type
      # Incubation period
      colour_col_label <- paste0(substr(unlist(strsplit(colour_col, "_")), 1, 1),
                                 collapse = "")
      
      all_groups <- bind_rows(d1, d2, d3, d4) |>
        distinct(.data[[colour_col]]) |>
        arrange(.data[[colour_col]]) |>
        pull()
      
      
      custom_colours <- lanonc_colours[seq_along(all_groups)]
      custom_colours <- setNames(custom_colours, all_groups)
      
      # 65 with Chua
      p1_incb_plots[[plot_type]][[colour_col]] <- forest_plot(
        d1 |> filter(qa_score>qa_threshold), "Incubation period (days)",
        colour_col, c(0,22), text_size=text_size, segment_show.legend = NA,
        sort=TRUE, custom_colours = custom_colours, qa_alpha=qa_alpha)
      
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_incubation_",
                              colour_col_label, ".pdf")),
             plot = p1_incb_plots[[plot_type]][[colour_col]],
             width = 11, height = 9)
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_incubation_",
                              colour_col_label, ".png")),
             plot = p1_incb_plots[[plot_type]][[colour_col]],
             width = 11, height = 9)
      
      # Onset to admissions
      #Note, one of the missing Balkhays is because it's a MINUS value
      p2_oa_plots[[plot_type]][[colour_col]] <- forest_plot(
        d2 |> filter(qa_score>qa_threshold),
        'Symptom onset-to-hospitalisation delay (days)', colour_col, c(0,23),
        text_size = text_size, sort=TRUE, custom_colours = custom_colours,
        qa_alpha=qa_alpha)
      
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_onset_admis_",
                              colour_col_label, ".pdf")),
             plot = p2_oa_plots[[plot_type]][[colour_col]],
             width = 11, height = 9)
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_onset_admis_",
                              colour_col_label, ".png")),
             plot = p2_oa_plots[[plot_type]][[colour_col]],
             width = 11, height = 9)
    }
    
    if (colour_col== "parameter_type"){
      colour_col_label <- paste0(substr(unlist(strsplit(colour_col, "_")), 1, 1),
                                 collapse = "")
      
      all_groups <- d3 |>
        distinct(parameter_type) |>
        arrange(parameter_type) |>
        pull()
      
      custom_colours <- lanonc_colours[seq_along(all_groups)]
      custom_colours <- setNames(custom_colours, all_groups)
      
      p3_ao_plots[[plot_type]][[colour_col]] <- forest_plot(
        d3 |> filter(qa_score>qa_threshold), 'Hospitalisation-to-outcome (days)',
        colour_col, c(0,100), text_size = text_size, sort=TRUE,
        custom_colours = custom_colours, qa_alpha=qa_alpha)
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_admis_outcome_",
                              colour_col_label, ".pdf")),
             plot = p3_ao_plots[[plot_type]][[colour_col]],
             width = 11, height = 9)
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_admis_outcome_",
                              colour_col_label, ".png")),
             plot = p3_ao_plots[[plot_type]][[colour_col]],
             width = 11, height = 9)
    }else if(plot_type=="all"){
      p3_ao_plots[[plot_type]][[colour_col]] <- forest_plot(
        d3 |> filter(qa_score>qa_threshold), 'Hospitalisation-to-outcome (days)',
        colour_col, c(0,45), text_size = text_size, sort=TRUE,
        custom_colours = custom_colours, qa_alpha=qa_alpha) +
        ggforce::facet_col(facets = vars(parameter_type),
                           scales = "free_y",
                           space = "free")
      
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_admis_outcome_facet_",
                              colour_col_label, ".pdf")),
             plot = p7_oo_reduced_plots[[plot_type]][[colour_col]],
             width = 15, height = 11)
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_admis_outcome_facet_",
                              colour_col_label, ".png")),
             plot = p7_oo_reduced_plots[[plot_type]][[colour_col]],
             width = 15, height = 11)
      
      p7_oo_reduced_plots[[plot_type]][[colour_col]] <-
        p7_oo_reduced_plots[[plot_type]][[colour_col]] +
        guides(shape =  guide_legend(title = "Parameter type", order=1),
               color = guide_legend(title = "Outcome"),
               linetype = guide_legend(title = "Variation type"))
    }
    
    # Potential typo in paper so no upper bound for the range is recorded, set to
    # param value so that the error bar plots
    # Manually draw line
    if (colour_col== "parameter_type"){
      d4_plot <- d4
      d4_plot_label <- "outcome"
      d4_x_axis_label <- 'Symptom onset-to-outcome (days)'
      xlim <- c(-2,190)
      
      d8_plot <- d6 |> filter(parameter_type=="Time in care (length of stay)")
      d8_plot_label <- "time_in_care"
      d8_x_axis_label <- 'Time in care (days)'
      xlim_d8 <- c(0,100)
      
      all_groups <- rbind(d4_plot, d8_plot) |>
        distinct(parameter_type) |>
        arrange(parameter_type) |>
        pull()
      
      custom_colours <- lanonc_colours[seq_along(all_groups)]
      custom_colours <- setNames(custom_colours, all_groups)
    } else{
      # for other plot types remove Symptom onset>recovery/death and
      # Symptom onset>discharge/recovery. Only Symptom onset>death is included
      # (makes more sense when colouring by a variable other than paramter_type)
      d4_plot <- d4 |> filter(parameter_type=="Death")
      d4_plot_label <- "death"
      d4_x_axis_label <- 'Symptom onset-to-death (days)'
      xlim <- c(0,180)
      
      d8_plot <- d6 |> filter(parameter_type=="Time in care (length of stay)")
      d8_plot_label <- "time_in_care"
      d8_x_axis_label <- 'Time in care (days)'
      xlim_d8 <- c(0,100)
    }
    
    p4_oo <- forest_plot(
      d4_plot |> filter(qa_score>qa_threshold),
      d4_x_axis_label, colour_col, xlim,
      text_size = text_size, sort=TRUE,
      segment_show.legend = c(shape=FALSE, colour=TRUE),
      custom_colours = custom_colours,
      qa_alpha=qa_alpha) +
      geom_linerange(data=d4_plot |> filter(qa_score>qa_threshold),
                     aes(xmin = parameter_2_lower_bound,
                         xmax = parameter_value, y=refs),
                     linetype="dashed")
    
    
    # send linerange to the back
    # Keep forest plot point geom last to maintain plot order
    p4_oo$layers <- c(tail(p4_oo$layers, 1), head(p4_oo$layers, -3),
                      tail(p4_oo$layers,2)[1], tail(p4_oo$layers,3)[1])
    
    p4_oo_plots[[plot_type]][[colour_col]] <- p4_oo
    
    ggsave(file.path(plot_type,
                     paste0("figure_5", label, "_onset_", d4_plot_label, "_",
                            colour_col_label, ".pdf")),
           plot = p4_oo_plots[[plot_type]][[colour_col]] ,
           width = 15, height = 15)
    ggsave(file.path(plot_type,
                     paste0("figure_5", label, "_onset_", d4_plot_label, "_",
                            colour_col_label, ".png")),
           plot = p4_oo_plots[[plot_type]][[colour_col]] ,
           width = 15, height = 15)
    
    p8_oo <- forest_plot(
      d8_plot |> filter(qa_score>qa_threshold),
      d8_x_axis_label, colour_col, xlim_d8,
      text_size = text_size, sort=TRUE,
      segment_show.legend = c(shape=FALSE, colour=TRUE),
      custom_colours = custom_colours,
      qa_alpha=qa_alpha) +
      geom_linerange(data=d8_plot |> filter(qa_score>qa_threshold),
                     aes(xmin = parameter_2_lower_bound,
                         xmax = parameter_value, y=refs),
                     linetype="dashed")
    
    # send linerange to the back
    # Keep forest plot point geom last to maintain plot order
    p8_oo$layers <- c(tail(p8_oo$layers, 1), head(p8_oo$layers, -3),
                      tail(p8_oo$layers,2)[1], tail(p8_oo$layers,3)[1])
    
    p8_oo_plots[[plot_type]][[colour_col]] <- p8_oo
    
    ggsave(file.path(plot_type,
                     paste0("figure_8", label, "_time_in_care_", d8_plot_label, "_",
                            colour_col_label, ".pdf")),
           plot = p8_oo_plots[[plot_type]][[colour_col]] ,
           width = 15, height = 15)
    ggsave(file.path(plot_type,
                     paste0("figure_8", label, "_time_in_care_", d8_plot_label, "_",
                            colour_col_label, ".png")),
           plot = p8_oo_plots[[plot_type]][[colour_col]] ,
           width = 15, height = 15)
    
    # Serial interval
    p5_si_plots[[plot_type]][[colour_col]] <- forest_plot(
      d5, 'Serial interval (days)', colour_col, c(0,25),
      text_size = text_size, sort=TRUE, custom_colours = custom_colours,
      qa_alpha=qa_alpha)
    
    if (colour_col== "parameter_type"){
      
      xlim <- c(-2, 200)
      
      all_groups <- d6 |>
        filter(qa_score>qa_threshold) |>
        distinct(parameter_type) |>
        arrange(parameter_type) |>
        pull()
      
      custom_colours <- lanonc_colours[seq_along(all_groups)]
      custom_colours <- setNames(custom_colours, all_groups)
      
      arrow_df <- data.frame(x = rep(43.7,2), xend = rep(44.7,2), y = c(4,5), yend = c(4,5),
                             parameter_type = rep("Discharge/recovery",2)) |>
        mutate(parameter_type=factor(parameter_type,
                                     levels=c("Admission", "Severe illness",
                                              "Death", "Discharge/recovery")))
      
      p6_oa_o_plots[[plot_type]][[colour_col]] <- forest_plot(
        d6 |> filter(qa_score>qa_threshold) |>
          #REMOVE THIS TO GO BACK TO ORIGINAL
          filter(parameter_type %in% c("Onset>admission",
                                       "Onset>severe illness",
                                       "Onset>recovery/death",
                                       "Onset>discharge/recovery")),
        'Symptom onset-to-outcome (days)',
        colour_col, xlim, text_size = text_size, sort=TRUE,
        custom_colours = custom_colours, qa_alpha=qa_alpha) +
        geom_segment(
          data = arrow_df,
          aes(x = x, xend = xend, y = y, yend = yend, group=parameter_type),
          arrow = arrow(type = "open", length = unit(0.20, "cm")),
        ) +
        coord_cartesian(xlim = c(-0.5, 45))
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_onset_admis_outcome_",
                              colour_col_label, ".pdf")),
             plot = p6_oa_o_plots[[plot_type]][[colour_col]],
             width = 15, height = 17)
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_onset_admis_outcome_",
                              colour_col_label, ".png")),
             plot = p6_oa_o_plots[[plot_type]][[colour_col]],
             width = 15, height = 17)
      
      p6_oa_o_plots[[plot_type]][[colour_col]] <-
        p6_oa_o_plots[[plot_type]][[colour_col]] +
        guides(shape =  guide_none(),
               linetype = guide_none(),
               color = guide_legend(title = "Outcome"))
    }
    
    if (colour_col== "parameter_type" & plot_type=="all"){
      # Do  we want consistent colours across the SI and main plot?
      # If so, remove the filter
      d7 <- d4 |>
        filter(!(parameter_type %in% c("Death")))
      
      all_groups <- d7 |>
        filter(qa_score>qa_threshold) |>
        distinct(parameter_type) |>
        arrange(parameter_type) |>
        pull()
      
      custom_colours <- lanonc_colours[seq_along(all_groups)]
      custom_colours <- setNames(custom_colours, all_groups)
      
      p7_oo_reduced_plots[[plot_type]][[colour_col]] <- forest_plot(
        d7 |> filter(qa_score>qa_threshold),
        'Symptom onset-to-outcome (days)',
        colour_col, c(0,195), text_size = text_size, sort=TRUE,
        custom_colours = custom_colours, qa_alpha=qa_alpha) +
        scale_x_break(c(50, 125)) #+
      #facet_wrap(~parameter_type, ncol=1, scales="free_x")
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_onset_outcome_reduced_",
                              colour_col_label, ".pdf")),
             plot = p7_oo_reduced_plots[[plot_type]][[colour_col]],
             width = 15, height = 11)
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_onset_outcome_reduced_",
                              colour_col_label, ".png")),
             plot = p7_oo_reduced_plots[[plot_type]][[colour_col]],
             width = 15, height = 11)
      
      p7_oo_reduced_plots[[plot_type]][[colour_col]] <-
        p7_oo_reduced_plots[[plot_type]][[colour_col]] +
        guides(shape =  guide_legend(title = "Parameter type", order=1),
               color = guide_legend(title = "Outcome"),
               linetype = guide_legend(title = "Variation type"))
    }else if(plot_type=="all"){
      # Note:: removing two estimates
      # This does nothing for MERS
      d4_filtered <- d4 |>
        filter(!(parameter_type %in% c("Recovery/death")))
      
      # update x-lim to 85 if including the above
      p7_oo_reduced_plots[[plot_type]][[colour_col]] <- forest_plot(
        d4_filtered, 'Symptom onset-to-outcome (days)',
        colour_col, c(0,195), text_size = text_size, sort=TRUE,
        custom_colours = custom_colours, qa_alpha=qa_alpha) +
        ggforce::facet_col(facets = vars(parameter_type),
                           scales = "free_y",
                           space = "free") +
        scale_x_break(c(40, 140)) +
        theme(strip.text.y = element_text(angle=0))
      
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_onset_outcome_facet_",
                              colour_col_label, ".pdf")),
             plot = p7_oo_reduced_plots[[plot_type]][[colour_col]],
             width = 15, height = 15)
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_onset_outcome_facet_",
                              colour_col_label, ".png")),
             plot = p7_oo_reduced_plots[[plot_type]][[colour_col]],
             width = 15, height = 15)
      
      p7_oo_reduced_plots[[plot_type]][[colour_col]] <-
        p7_oo_reduced_plots[[plot_type]][[colour_col]] +
        guides(shape =  guide_legend(title = "Parameter type", order=1),
               color = guide_legend(title = "Outcome"),
               linetype = guide_legend(title = "Variation type"))
    }
  }
  
  # Update legends for final plot
  if(colour_col=="population_country"){
    p1_incb_plots[[plot_type]][["population_country"]] <-
      p1_incb_plots[[plot_type]][["population_country"]] +
      guides(shape = guide_none(),
             fill = guide_none(),
             color=guide_legend(title="Country"),
             linetype = guide_none())
    
    p4_oo_plots[[plot_type]][["population_country"]] <-
      p4_oo_plots[[plot_type]][["population_country"]] +
      scale_colour_manual(values=custom_colours,
                          limits=all_groups) +
      scale_fill_manual(values=custom_colours,
                        limits=all_groups) +
      guides(shape = guide_none(),
             linetype = guide_none(),
             color=guide_legend(title="Country"))
  }
  
  common_left_legend <- theme(
    legend.position = "right",
    legend.justification = "left",
    legend.box.just = "left",
    legend.direction = "vertical",
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.spacing.y = unit(0.08, "cm"),
    legend.title.align = 0,
    plot.margin = margin(t = 5.5, l = 5.5, b =  5.5, r = 5.5)
  )
  
  
  p4_oo_plots[[plot_type]][["population_country"]]  <-
    p4_oo_plots[[plot_type]][["population_country"]] + common_left_legend
  
  if (plot_type=="qa"){
    p6_oa_o_plots[[plot_type]][["parameter_type"]] <-
      p6_oa_o_plots[[plot_type]][["parameter_type"]] + common_left_legend
    
    # Alternative is to use guides="collect" (legends) in plot_layout
    delays_plot <-  (p1_incb_plots[[plot_type]][["population_country"]] /#+
                       p8_oo_plots[[plot_type]][["population_country"]])/(
                         #bsl_model_plot +
                         p6_oa_o_plots[[plot_type]][["parameter_type"]]) +
      plot_layout(heights = c(1, 1, 1), #, widths = c(1, 1)
                  guides = "collect") +
      plot_annotation(tag_levels = 'A')
    
    ggsave(paste0("figure_8", label,"_delays.pdf"), plot = delays_plot,
           width = 25, height = 25)
    ggsave(paste0("figure_8", label,"_delays.png"), plot = delays_plot,
           width = 25, height = 25)
  }else{
    
    p1_incb <- p1_incb_plots[[plot_type]][["population_country"]] +
      guides(shape =  guide_legend(title = "Parameter type", order=1),
             color = guide_legend(title = "Outcome"),
             linetype = guide_legend(title = "Variation type")) +
      theme(legend.position = c(0.8,0.375))
    
    p3_ao <- p3_ao_plots[[plot_type]][["population_country"]] +
      guides(shape =  guide_none(),
             linetype = guide_none(),
             color = guide_none())
    
    p5_si <- p5_si_plots[[plot_type]][["population_country"]] +
      guides(shape =  guide_none(),
             linetype = guide_none(),
             color = guide_none())
    
    p7_oo <- p7_oo_reduced_plots[[plot_type]][["population_country"]] +
      guides(shape =  guide_none(),
             linetype = guide_none(),
             color = guide_none())
    
    
    # p1_incb / p5_si / free(bsl_model_plot) | p7_oo / p3_ao
    design <- "
    A#D
    B#D
    B#D
    B#E
    C#E"
    
    delays_plot <-
      p1_incb + p5_si + p7_oo + p3_ao +
      plot_layout(
        design  = design,
        widths  = c(1, 0.05, 1),
        heights = c(21,  8.25, 5, 5.25, 2.75)   # makes D = 21+3+7 taller
      ) +   plot_annotation(
        # tag_levels="A"
        tag_levels = list(c("A", "B", "", "C", "D", "E"))
      ) & theme(plot.tag.position = c(0, 1),
                plot.tag = element_text(size = 30))
    
    
    ggsave(paste0("figure_5", label,"_delays.pdf"), plot = delays_plot,
           width = 26, height = 30)
    ggsave(paste0("figure_5", label,"_delays.png"), plot = delays_plot,
           width = 26, height = 30)
  }
}


# ==============================================================================
# *--------------------------------- Not used ---------------------------------*
# Incubation facet:
incubation_pc_facet <- forest_plot(d1,
                                   "Incubation period (days)",
                                   "population_sample_type", c(0,35),
                                   text_size=text_size,
                                   segment_show.legend = NA,
                                   sort=TRUE) +
  facet_wrap("population_country", scales="free")


ggsave(paste0("figure_5_incubation_facet.pdf"), plot = incubation_pc_facet,
       width = 15, height = 9)