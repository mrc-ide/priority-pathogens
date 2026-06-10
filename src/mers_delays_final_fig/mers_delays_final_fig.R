# *=============================== Nipah delays ===============================*
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

orderly_artefact(description="Nipah delay figures",
                 c("p1_incubation_period.pdf",
                   "p2_time_in_care.pdf",
                   "p3_outcomes.pdf",
                   "p4_outcomes.pdf",
                   "mers_delays.pdf",
                   "p1_incubation_period.png",
                   "incubation_period_df.rds",
                   "p2_time_in_care.png",
                   "p3_outcomes.png",
                   "p4_outcomes.png",
                   "mers_delays.png"))

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
parameters[parameters$access_param_id=="320_002",
           "population_group"] <- "Unspecified"

#390_002 and 390_003 are "per days", we have to change their parameter_unit to "Days" to not break forest_plot
parameters[parameters$access_param_id=="390_002",
           "parameter_unit"] <- "Days"
parameters[parameters$access_param_id=="390_003",
           "parameter_unit"] <- "Days"

#271-001 is a gamma distribution but with reported mean 6.99 (unspecified units)
#It's also low-QA. For now I'm going to manually change it to days though
parameters[parameters$access_param_id=="271_001",
           "parameter_unit"] <- "Days"

#032-001 is also "Unspecified"
#It's a decent study, but never explicity SAYS "Days"
parameters[parameters$access_param_id=="032_001",
           "parameter_unit"] <- "Days"

#255_014 lists a delay of -5.9 days. That's because this individual had symptom onset AFTER hospital admission
# Let's convert this to an "Other" human delay.
#parameters <- filter(parameters, access_param_id != "255_014")
#test <- filter(parameters, access_param_id == "255_014")

parameters[parameters$access_param_id=="255_014",
           "parameter_type"] <- "Human delay - other human delay (go to section)"
parameters[parameters$access_param_id=="255_014",
           "other_delay_start"] <- "Admission to hospital"
parameters[parameters$access_param_id=="255_014",
           "other_delay_end"] <- "symptom onset"
parameters[parameters$access_param_id=="255_014",
           "parameter_value"] <- -1*parameters[parameters$access_param_id=="255_014",
                                               "parameter_value"]

#261-001 extracts it's units as "Months", but it is also an "Other" delay, so look at this later
#(It's high QA)
#TODO: Look at this

#Remove the words "human delay" throughout
parameters <- parameters |>
  mutate(parameter_type = str_replace(parameter_type, "Human delay - ", ""),
         parameter_type = str_to_sentence(parameter_type))

all_delays <- filter(parameters, parameter_class == "Human delay")

#Filter out all low-QA
all_delays_all_qa <- all_delays
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
                     "Infectious period", #None
                     "Generation time"
)
parameters <- parameters |>
  filter(parameter_type %in% all_delay_types)
#parameters <- filter(parameters, qa_score >= 0.5)
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
# parameters <- parameters |>
#   filter(!is.na(parameter_value) |
#            !is.na(parameter_lower_bound) |
#            !is.na(parameter_upper_bound))

#Let's re-assign all the country tags
parameters <- parameters |>
  mutate(population_country=ifelse(population_country=="Algeria; Austria; Bahrain; China; Egypt; France; Germany; Greece; Iran (Islamic Republic of); Italy; Jordan; Kuwait; Lebanon; Malaysia; Netherlands; Oman; Philippines; Qatar; Republic of Korea; Saudi Arabia; Thailand; Tunisia; Türkiye; United Arab Emirates; United Kingdom of Great Britain and Northern Ireland; United States of America; Yemen",
                                   "Global", population_country)) |>
  mutate(population_country=ifelse(population_country=="Algeria; Egypt; Germany; Greece; Italy; Netherlands; Philippines; Thailand; United States of America",
                                   "Global", population_country)) |>
  mutate(population_country=ifelse(population_country=="Bahrain; Kuwait; Qatar; Saudi Arabia; United Arab Emirates; Yemen",
                                   "Other (Middle East)", population_country)) |>
  mutate(population_country=ifelse(population_country=="Democratic People's Republic of Korea; Republic of Korea; Saudi Arabia",
                                   "Other", population_country)) |>
  mutate(population_country=ifelse(population_country=="France; Iran (Islamic Republic of); Italy; Jordan; Qatar; Saudi Arabia; Tunisia; United Arab Emirates; United Kingdom of Great Britain and Northern Ireland",
                                   "Global", population_country)) |>
  mutate(population_country=ifelse(population_country=="France; Italy; Tunisia; United Kingdom of Great Britain and Northern Ireland",
                                   "Global", population_country)) |>
  mutate(population_country=ifelse(population_country=="France; Iran (Islamic Republic of); Jordan; Saudi Arabia; United Arab Emirates",
                                   "Global", population_country)) |>
  mutate(population_country=ifelse(population_country=="Oman",
                                   "Other (Middle East)", population_country)) |>
  mutate(population_country=ifelse(population_country=="Oman; Saudi Arabia",
                                   "Other (Middle East)", population_country)) |>
  mutate(population_country=ifelse(population_country=="Qatar",
                                   "Other (Middle East)", population_country)) |>
  mutate(population_country=ifelse(population_country=="Jordan",
                                   "Other (Middle East)", population_country)) |>
  mutate(population_country=ifelse(population_country=="Jordan; Kuwait; Oman; Qatar; Saudi Arabia; United Arab Emirates; Yemen",
                                   "Other (Middle East)", population_country)) |>
  mutate(population_country=ifelse(population_country=="United Arab Emirates",
                                   "Other (Middle East)", population_country)) |>
  mutate(population_country=ifelse(population_country=="Iran (Islamic Republic of); Jordan; Philippines; Republic of Korea; Saudi Arabia; United Arab Emirates",
                                   "Other", population_country)) |>
  mutate(population_country=ifelse(population_country=="Republic of Korea; Saudi Arabia",
                                   "Other", population_country)) |>
  mutate(population_country=ifelse(is.na(population_country), #I checked these, for two of the papers it's mostly Saudi, but we can't be sure of the other data
                                   "Unspecified", population_country)) |>
  mutate(population_country=ifelse(!is.na(population_location) & population_location=="Global linelist",
                                   "Global linelist", population_country))

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
#Other delays
d7 <- parameters |>
  filter(tolower(parameter_type) %in% c('other human delay (go to section)'))

# Generation time
d8 <- parameters |>
  filter(tolower(parameter_type) %in% c("generation time"))
# Infectious period
d9 <- parameters |>
  filter(tolower(parameter_type) %in% c("infectious period"))
# *---------------------------------- Plots -----------------------------------*
lanonc_colours <- ggsci::pal_lancet("lanonc")(9)

# Plot properties
text_size <- 28

      # Incubation period

# Define ONE shared country colour palette from all data upfront
all_country_groups <- bind_rows(d1, d6) |>
  distinct(population_country) |>
  arrange(population_country) |>
  pull()
#Change order:
all_country_groups2 <- c(all_country_groups[1],all_country_groups[5:6], all_country_groups[2:4], all_country_groups[7])

country_colours <- lanonc_colours[seq_along(all_country_groups2)]
country_colours <- setNames(country_colours, all_country_groups2)

d1$population_country <- factor(d1$population_country,
                     levels = all_country_groups)

# Convert population_country to factor with ALL levels in both datasets
d1 <- d1 |> mutate(population_country = factor(population_country, levels = all_country_groups))

      p1_incb <- forest_plot(
        filter(d1, qa_score >= 0.5), "Incubation period (days)",
        "population_country", c(0,22), text_size=text_size, segment_show.legend = c(shape=FALSE, colour=TRUE, fill = TRUE),
        sort=TRUE, custom_colours = country_colours)

      p1_incb <- p1_incb +
        scale_colour_manual(name = "Country", values = country_colours, drop = FALSE) +
        scale_fill_manual(name = "Country", values = country_colours, drop = FALSE) +
        guides(
          color = guide_legend(title = "Country", order = 1),
          fill  = guide_none(),
          shape = guide_legend(title = "", order = 3),
          linetype = guide_legend(title = "", order = 2)
        )

      p1_incb_all_qa <- forest_plot(
        d1, "Incubation period (days)",
        "population_country", c(0,22),
        qa_alpha = 0.3, text_size=text_size, segment_show.legend = c(shape=FALSE, colour=TRUE, fill = TRUE),
        sort=TRUE, custom_colours = country_colours)

      p1_incb_all_qa <- p1_incb_all_qa +
        scale_colour_manual(name = "Country", values = country_colours, drop = TRUE) +
        scale_fill_manual(name = "Country", values = country_colours, drop = TRUE) +
        guides(
          color = guide_legend(title = "Country", order = 1),
          fill  = guide_none(),
          shape = guide_legend(title = "", order = 3),
          linetype = guide_legend(title = "", order = 2)
        )

      ggsave("SI_delay_incb.png", plot = p1_incb_all_qa, width = 14, height = nrow(d1)*0.3)
      ggsave("SI_delay_incb.pdf", plot = p1_incb_all_qa, width = 14, height = nrow(d1)*0.3)

      saveRDS(d1, "incubation_period_df.rds")
      ggsave("p1_incubation_period.pdf",
             plot = p1_incb,
             width = 11, height = 9)
      ggsave("p1_incubation_period.png",
             plot = p1_incb,
             width = 11, height = 9)


      tic_plot <- d6 |> filter(parameter_type=="Time in care (length of stay)")
      tic_plot_label <- "time_in_care"
      tic_x_axis_label <- 'Time in care (days)'
      xlim_tic <- c(0,100)

      tic_plot <- tic_plot |> mutate(population_country = factor(population_country, levels = all_country_groups))


      # all_groups <- rbind(d4_plot, d8_plot) |>
      #   distinct(population_country) |>
      #   arrange(population_country) |>
      #   pull()
      #
      # custom_colours <- lanonc_colours[seq_along(all_groups)]
      # custom_colours <- setNames(custom_colours, all_groups)

    p2_time_in_care <- forest_plot(
      filter(tic_plot, qa_score >= 0.5),
      tic_x_axis_label, "population_country", xlim_tic,
      text_size = text_size, sort=TRUE,
      segment_show.legend = c(shape=FALSE, colour=TRUE),
      custom_colours = country_colours)

    p2_time_in_care <- p2_time_in_care +
      scale_colour_manual(name = "Country", values = country_colours, drop = FALSE) +
      guides(shape = guide_none(), linetype = guide_none(),
             color = guide_legend(title = "Country"))

    p2_time_in_care_all_qa <- forest_plot(
      tic_plot,
      tic_x_axis_label, "population_country", xlim_tic,
      qa_alpha = 0.3,
      text_size = text_size, sort=TRUE,
      segment_show.legend = c(shape=FALSE, colour=TRUE),
      custom_colours = country_colours)

    p2_time_in_care_all_qa <- p2_time_in_care_all_qa +
      scale_colour_manual(name = "Country", values = country_colours, drop = TRUE) +
      guides(
        color = guide_legend(title = "Country", order = 1),
        fill  = guide_none(),
        shape = guide_legend(title = "", order = 3),
        linetype = guide_legend(title = "", order = 2)
      )

    ggsave("SI_delay_tic.png", plot = p2_time_in_care_all_qa, width = 14, height = nrow(tic_plot)*0.3)
    ggsave("SI_delay_tic.pdf", plot = p2_time_in_care_all_qa, width = 14, height = nrow(tic_plot)*0.3)

    # send linerange to the back
    # Keep forest plot point geom last to maintain plot order
    # p2_time_in_care$layers <- c(tail(p2_time_in_care$layers, 1), head(p2_time_in_care$layers, -3),
    #                   tail(p2_time_in_care$layers,2)[1], tail(p2_time_in_care$layers,3)[1])

    ggsave("p2_time_in_care.pdf",
           plot = p2_time_in_care ,
           width = 15, height = 15)
    ggsave("p2_time_in_care.png",
           plot = p2_time_in_care ,
           width = 15, height = 15)


      xlim <- c(-2, 200)

      all_groups <- d6 |>
        #filter(qa_score>=0.5) |>
        filter(parameter_type %in% c("Onset>admission",
                                     "Onset>severe illness",
                                     "Onset>recovery/death",
                                     "Onset>discharge/recovery")) |>
        distinct(parameter_type) |>
        arrange(parameter_type) |>
        pull()

      custom_colours <- c("#CBCE91", "#d3687f") #lanonc_colours[seq_along(all_groups)]
      custom_colours <- setNames(custom_colours, all_groups)

      arrow_df <- data.frame(x = rep(43.7,2), xend = rep(44.7,2), y = c(4,5), yend = c(4,5),
                             parameter_type = rep("Discharge/recovery",2)) |>
        mutate(parameter_type=factor(parameter_type,
                                     levels=c("Admission", "Severe illness",
                                              "Death", "Discharge/recovery")))

      arrow_df_1 <- data.frame(x = rep(14.5,2), xend = rep(14.9,2), y = c(1,17), yend = c(1,17),
                             parameter_type = rep("Discharge/recovery",2)) |>
        mutate(parameter_type=factor(parameter_type,
                                     levels=c("Admission", "Severe illness",
                                              "Death", "Discharge/recovery")))

      p3_outcomes <- forest_plot(
        d4 |> filter(qa_score>= 0.5) |>
          filter(parameter_type %in% c("Admission")),
        'Symptom onset-to-hospital admission (days)',
        #"parameter_type",
        "population_country",
        xlim, text_size = text_size, sort=TRUE,
        custom_colours = country_colours) +#custom_colours) +
        geom_segment(
          data = arrow_df_1,
          aes(x = x, xend = xend, y = y, yend = yend, group=parameter_type),
          arrow = arrow(type = "open", length = unit(0.20, "cm")),
        ) +
        coord_cartesian(xlim = c(-0.5, 15))

      ggsave("p3_outcomes.pdf",
             plot = p3_outcomes,
             width = 15, height = 17)
      ggsave("p3_outcomes.png",
             plot = p3_outcomes,
             width = 15, height = 17)

      p3_outcomes <-
        p3_outcomes +
        guides(shape =  guide_none(),
               linetype = guide_none(),
               color = guide_none()) +
        theme(legend.position = c(0.85, 0.75))


      arrow_df_1 <- data.frame(x = rep(14.5,3), xend = rep(14.9,3), y = c(1,16,21), yend = c(1,16,21),
                               parameter_type = rep("Discharge/recovery",3)) |>
        mutate(parameter_type=factor(parameter_type,
                                     levels=c("Admission", "Severe illness",
                                              "Death", "Discharge/recovery")))

      p3_outcomes_all_qa <- forest_plot(
        d4 |>
          filter(parameter_type %in% c("Admission")),
        'Symptom onset-to-hospital admission (days)',
        #"parameter_type",
        "population_country",
        xlim, text_size = text_size, sort=TRUE,
        qa_alpha = 0.3,
        custom_colours = country_colours) +#custom_colours) +
        geom_segment(
          data = arrow_df_1,
          aes(x = x, xend = xend, y = y, yend = yend, group=parameter_type),
          arrow = arrow(type = "open", length = unit(0.20, "cm")),
        ) +
        coord_cartesian(xlim = c(-0.5, 15))


      ############################
      p4_outcomes <- forest_plot(
        d4 |> filter(qa_score>= 0.5) |>
          #REMOVE THIS TO GO BACK TO ORIGINAL
          filter(parameter_type %in% c("Discharge/recovery")),
        'Symptom onset-to-discharge/recovery (days)',
        "population_country", xlim, text_size = text_size, sort=TRUE,
        custom_colours = country_colours) +
        geom_segment(
          data = arrow_df,
          aes(x = x, xend = xend, y = y, yend = yend, group=parameter_type),
          arrow = arrow(type = "open", length = unit(0.20, "cm")),
        ) +
        coord_cartesian(xlim = c(-0.5, 45))

      ggsave("p4_outcomes.pdf",
             plot = p4_outcomes,
             width = 15, height = 17)
      ggsave("p4_outcomes.png",
             plot = p4_outcomes,
             width = 15, height = 17)

      p4_outcomes <-
        p4_outcomes +
        guides(shape =  guide_none(),
               linetype = guide_none(),
               color = guide_none()) +
        theme(legend.position = c(0.85, 0.75))
      ###

      p4_outcomes_all_qa <- forest_plot(
        d4 |>
          #REMOVE THIS TO GO BACK TO ORIGINAL
          filter(parameter_type %in% c("Discharge/recovery")),
        'Symptom onset-to-discharge/recovery (days)',
        "population_country", xlim, text_size = text_size, sort=TRUE,
        qa_alpha = 0.3,
        custom_colours = country_colours) +
        geom_segment(
          data = arrow_df,
          aes(x = x, xend = xend, y = y, yend = yend, group=parameter_type),
          arrow = arrow(type = "open", length = unit(0.20, "cm")),
        ) +
        coord_cartesian(xlim = c(-0.5, 45))
      #################################################################
      arrow_df <- data.frame(x = rep(33.7,2), xend = rep(34.7,2), y = c(8,10), yend = c(8,10),
                             parameter_type = rep("Discharge/recovery",2)) |>
        mutate(parameter_type=factor(parameter_type,
                                     levels=c("Admission", "Severe illness",
                                              "Death", "Discharge/recovery")))
      #Remove 037_004 because it's just that a patient "died within 2 weeks", not good enough to extract in my books.
      d4 <- filter(d4, access_param_id != "037_004")

      p4_outcomes_death <- forest_plot(
        d4 |> filter(qa_score>= 0.5) |>
          #REMOVE THIS TO GO BACK TO ORIGINAL
          filter(parameter_type %in% c("Death")),
        'Symptom onset-to-death (days)',
        "population_country", c(-2,400), text_size = text_size, sort=TRUE,
        custom_colours = country_colours) +
        geom_segment(
          data = arrow_df,
          aes(x = x, xend = xend, y = y, yend = yend, group=parameter_type),
          arrow = arrow(type = "open", length = unit(0.20, "cm")),
        ) +
        coord_cartesian(xlim = c(-0.5, 35))

      ggsave("p4_outcomes.pdf",
             plot = p4_outcomes_death,
             width = 15, height = 17)
      ggsave("p4_outcomes_death.png",
             plot = p4_outcomes,
             width = 15, height = 17)

      p4_outcomes_death <-
        p4_outcomes_death +
        guides(shape =  guide_none(),
               linetype = guide_none(),
               color = guide_none()) +
        theme(legend.position = c(0.85, 0.75))
      #
      arrow_df <- data.frame(x = rep(33.7,4), xend = rep(34.7,4), y = c(8,11,15,17), yend = c(8,11,15,17),
                             parameter_type = rep("Discharge/recovery",4)) |>
        mutate(parameter_type=factor(parameter_type,
                                     levels=c("Admission", "Severe illness",
                                              "Death", "Discharge/recovery")))

      p4_outcomes_death_all_qa <- forest_plot(
        d4 |>
          #REMOVE THIS TO GO BACK TO ORIGINAL
          filter(parameter_type %in% c("Death")),
        'Symptom onset-to-death (days)',
        "population_country", c(-2,400), text_size = text_size, sort=TRUE,
        qa_alpha = 0.3,
        custom_colours = country_colours) +
        geom_segment(
          data = arrow_df,
          aes(x = x, xend = xend, y = y, yend = yend, group=parameter_type),
          arrow = arrow(type = "open", length = unit(0.20, "cm")),
        ) +
        coord_cartesian(xlim = c(-0.5, 35))


      #################################################################

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


  # p3_outcomes <-
  #   p3_outcomes + common_left_legend

    # p1_incb <- p1_incb_plots[[plot_type]][["population_country"]] +
    #   guides(shape =  guide_legend(title = "Parameter type", order=1),
    #          color = guide_legend(title = "Outcome"),
    #          linetype = guide_legend(title = "Variation type")) +
    #   theme(legend.position = c(0.8,0.375))

    # p3_ao <- p3_ao_plots[[plot_type]][["population_country"]] +
    #   guides(shape =  guide_none(),
    #          linetype = guide_none(),
    #          color = guide_none())



# Stick all 5 together:

    delays_plot <-  (p1_incb /#+
      p3_outcomes /
        p4_outcomes /
        p2_time_in_care) +
      plot_layout(heights = c(26, 24, 6, 13, 26), #, widths = c(1, 1)
                  guides = "collect") +
      plot_annotation(tag_levels = 'A')

    ggsave("mers_delays_small.pdf", plot = delays_plot,
           width = 25, height = 27)
    ggsave("mers_delays_small.png", plot = delays_plot,
           width = 25, height = 27)

    delays_plot <-  (p1_incb /#+
                       p3_outcomes /
                       p4_outcomes /
                       p4_outcomes_death /
                       p2_time_in_care) +
      plot_layout(heights = c(26, 24, 6, 13, 26), #, widths = c(1, 1)
                  guides = "collect") +
      plot_annotation(tag_levels = 'A')

    ggsave("mers_delays.pdf", plot = delays_plot,
           width = 25, height = 35)
    ggsave("mers_delays.png", plot = delays_plot,
           width = 25, height = 35)

    design <- "AC
AC
AC
BD
BE
BE"
    patchwork_delay <- p1_incb+p2_time_in_care+p3_outcomes+p4_outcomes+p4_outcomes_death+plot_layout(design = design, guides = "collect")
    patchwork_delay <- patchwork_delay + plot_annotation(tag_levels = 'A')
    ggsave("mers_delays_alt.pdf", plot = patchwork_delay,
           width = 25, height = 20)
    ggsave("mers_delays_alt.png", plot = patchwork_delay,
           width = 25, height = 20)

    ######################
    #SI from symptom onset:
    p4_outcomes_all_qa <- p4_outcomes_all_qa +
      guides(shape =  guide_none(),
             linetype = guide_none(),
             color = guide_none()) +
      theme(legend.position = c(0.85, 0.75))

    p4_outcomes_death_all_qa <- p4_outcomes_death_all_qa +
      guides(shape =  guide_none(),
             linetype = guide_none(),
             color = guide_none()) +
      theme(legend.position = c(0.85, 0.75))

    delays_plot <-  (p3_outcomes_all_qa /
                       p4_outcomes_all_qa /
                       p4_outcomes_death_all_qa) +
      plot_layout(heights = c(34, 9, 17), #, widths = c(1, 1)
                  guides = "collect") +
      plot_annotation(tag_levels = 'A')

    ggsave("SI_delay_from_onset.pdf", plot = delays_plot,
           width = 25, height = 20)
    ggsave("SI_delay_from_onset.png", plot = delays_plot,
           width = 25, height = 20)

  #######################################
    # SI from admission
    d3 <- filter(d3, parameter_type != "Time in care")

    arrow_df_1 <- data.frame(x = rep(23.5,1), xend = rep(24.9,1), y = c(1), yend = c(1),
                             parameter_type = rep("Discharge/recovery",1)) |>
      mutate(parameter_type=factor(parameter_type,
                                   levels=c("Admission", "Severe illness",
                                            "Death", "Discharge/recovery")))

    p4_admission <- forest_plot(
      d3 |>
        filter(parameter_type %in% c("Discharge/recovery")),
      'Hospital admission-to-discharge/recovery (days)',
      #"parameter_type",
      "population_country",
      xlim, text_size = text_size, sort=TRUE,
      qa_alpha = 0.3,
      custom_colours = country_colours) +#custom_colours) +
      geom_segment(
        data = arrow_df_1,
        aes(x = x, xend = xend, y = y, yend = yend, group=parameter_type),
        arrow = arrow(type = "open", length = unit(0.20, "cm")),
      ) +
      coord_cartesian(xlim = c(-0.5, 25))

    p4_admission2 <- forest_plot(
      d3 |>
        filter(parameter_type %in% c("Death")),
      'Hospital admission-to-death (days)',
      #"parameter_type",
      "population_country",
      xlim, text_size = text_size, sort=TRUE,
      qa_alpha = 0.3,
      custom_colours = country_colours) +#custom_colours) +
      # geom_segment(
      #   data = arrow_df_1,
      #   aes(x = x, xend = xend, y = y, yend = yend, group=parameter_type),
      #   arrow = arrow(type = "open", length = unit(0.20, "cm")),
      # ) +
      coord_cartesian(xlim = c(-0.5, 75))

    p4_admission2 <-
      p4_admission2 +
      guides(shape =  guide_none(),
             linetype = guide_none(),
             color = guide_none()) +
      theme(legend.position = c(0.85, 0.75))


    delays_plot <-  (p4_admission /
                       p4_admission2) +
      plot_layout(heights = c(4, 2), #, widths = c(1, 1)
                  guides = "collect") +
      plot_annotation(tag_levels = 'A')

    ggsave("SI_delay_from_admission.pdf", plot = delays_plot,
           width = 25, height = 8)
    ggsave("SI_delay_from_admission.png", plot = delays_plot,
           width = 25, height = 8)

    #####################################################
    # Generation Time
    p5_gen <- forest_plot(
      d8,
      'Generation time (days)',
      #"parameter_type",
      "population_country",
      xlim, text_size = text_size, sort=TRUE,
      qa_alpha = 0.3,
      custom_colours = country_colours) +#custom_colours) +
      coord_cartesian(xlim = c(-0.5, 25))


    #####################################################
    # Serial Interval
    p_serial <- forest_plot(
      d5,
      'Serial Interval (days)',
      #"parameter_type",
      "population_country",
      xlim, text_size = text_size, sort=TRUE,
      qa_alpha = 0.3,
      custom_colours = country_colours) +#custom_colours) +
      coord_cartesian(xlim = c(-0.5, 25)) +
      scale_colour_manual(name = "Country", values = country_colours, drop = TRUE) +
      guides(
        color = guide_legend(title = "Country", order = 1),
        fill  = guide_none(),
        shape = guide_legend(title = "", order = 3),
        linetype = guide_legend(title = "", order = 2)
      )

    ggsave("SI_delay_serial.pdf", plot = p_serial,
           width = 25, height = 9)
    ggsave("SI_delay_serial.png", plot = p_serial,
           width = 25, height = 9)

    ########
    delays_plot <- (
      (p1_incb / p2_time_in_care) +
        plot_layout(guides = "collect")
    ) /
      wrap_elements(full = p3_outcomes + theme(legend.position = c(0.85, 0.75))) +
      plot_layout(heights = c(1, 1, 1)) +
      plot_annotation(tag_levels = 'A')
    ############################
    #Other human delays

# First, recode:
    recode_delays <- c(
      "Admission" = "Admission to care",
      "Admission to hospital" = "Admission to care",
      "Symptom Onset/Fever" = "Symptom onset",
      "Onset" = "Symptom onset",
      "Admission to ICU" = "Admission to Critical Care/ICU",
      "symptom onset" = "Symptom onset",
      "Symptom Onset" = "Symptom onset",
      "Days from symptom onset to intubation, median (Q1, Q3)" = "Symptom onset",
      "Days from the onset of symptoms to the emergency room, median (Q1, Q3)" = "Symptom onset",
      "time to the emergency room" = "Admission to Critical Care/ICU",
      "Diagnosis" = "Diagnosis/test result",
      "Disease onset" = "Symptom onset",
      "First positive RT-PCR test" = "Diagnosis/test result",
      "Hospital admission" = "Admission to care",
      "Hospitalisation" = "Admission to care",
      "Hospitalization" = "Admission to care",
      "case notification" = "Diagnosis/test result",
      "Lab confirmation" = "Diagnosis/test result",
      "Lab confirmation (WHO definition)" = "Diagnosis/test result",
      "Laboratory confirmation" = "Diagnosis/test result",
      "ICU Admission" = "Admission to Critical Care/ICU",
      "ICU admission" = "Admission to Critical Care/ICU",
      "MERS confirmation/test" = "Diagnosis/test result",
      "Onset of illness" = "Symptom onset",
      "Onset of symptoms" = "Symptom onset",
      "Onset of Symptoms" = "Symptom onset",
      "emergency room" = "Admission to Critical Care/ICU",
      "Emergency room" = "Admission to Critical Care/ICU",
      "intubation" = "Intubation",
      "confirmation" = "Diagnosis/test result",
      "Isolation unit" = "Isolation",
      "Lab confirmation" = "Diagnosis/test result",
      "Negative PCR Test (Sputum)" = "Negative test",
      "Negative PCR" = "Negative test",
      "negative swab" = "Negative test",
      "peak viral load" = "Peak viral load",
      "Virus detection" = "Diagnosis/test result",
      "Symptom Onset while in Hospital" = "Symptom onset",
      "Symtom onset" = "Symptom onset",
      "the first PCR-positive result" = "Diagnosis/test result",
      "Time intubated" = "Intubation",
      "unspecified" = "Unspecified",
      "NA" = "Unspecified",
      "initiation of mechanical ventilation" = "Start of mechanical ventilation",
      "Mechanical ventilator start" = "Start of mechanical ventilation",
      "Ventilation support" = "Start of mechanical ventilation",
      "Oxygen supplementation" = "Start of mechanical ventilation",
      "Onset of ventilation" = "Start of mechanical ventilation",
      "Time to RNA clearance" = "viral RNA clearance",
      "End mechanical ventilation" = "End of mechanical ventilation",
      "Case observation" = "Diagnosis/test result",
      "symtpom onset" = "Symptom onset",
      "time to pneumonia" = "pneumonia development",
      "mechanical ventilator end" = "End of mechanical ventilation",
      "Other human delay (go to section)" = "Unspecified",
      "Days from symptom onset to ICU admission, median (Q1, Q3)" = "Symptom onset",
      "Begin mechanical ventilation" = "Start of mechanical ventilation",
      "taking a sample which showed a negative result with RT-PCR for MERS-CoV" = "Negative test",
      "Positive conversion of immunofluorescent antibody (IFA) titre (â‰¥1:640) for MERS-CoV" = "Positive IFA",
      "Mechanical ventilator end" = "End of mechanical ventilation",
      "End of ICU stay" = "Discharge from Critical Care/ICU",
      "ICU Discharge/Death" = "Discharge from Critical Care/ICU",
      "Time from entering ICU" = "Admission to Critical Care/ICU",
      "Time to leaving ICU either by discharge or death" = "Time leaving ICU by discharge or death",
      "Illness onset" = "Symptom onset",
      "hospital admission" = "Admission to care",
      "Mechanical Ventilation" = "Start of mechanical ventilation",
      "Outcome (recovery or death)" = "Death or discharge",
      "death or discharge" = "Death or discharge",
      "Notification to WHO" = "Case notification to WHO"
    )

    d7 <- d7 %>%
      mutate(
        other_delay_start = recode(other_delay_start, !!!recode_delays),
        other_delay_end   = recode(other_delay_end, !!!recode_delays)
      )

    #First, how many unique combinations?
    d7$parameter_type <- paste(d7$other_delay_start,
                               d7$other_delay_end,
                               sep = " > ")
unique(d7$parameter_type)
table(d7$parameter_type)


library(ggalluvial)

# Count transitions
#d7 <- filter(d7, other_delay_start != "Viral RNA detected")
flows <- d7 %>%
  count(other_delay_start, other_delay_end)

test <- d7
test$other_delay_start <- as.factor(test$other_delay_start)
test$other_delay_end <- as.factor(test$other_delay_end)

levs <- union(levels(test$other_delay_start), levels(test$other_delay_end))
flows <- test %>%
  mutate(other_delay_start = factor(other_delay_start, levels = levs),
         other_delay_end = factor(other_delay_end, levels = levs)) %>%
  count(other_delay_start, other_delay_end, name = "n")

library(ggfittext)
library(grid)

#We remove some that only have 1 start point:
test2 <- filter(d7, !(other_delay_start %in% c("Infection", "Infectiousness",
                                               "Intubation", "Recovery")) )

test2$other_delay_start <- as.factor(test2$other_delay_start)
test2$other_delay_end <- as.factor(test2$other_delay_end)

levs <- union(levels(test2$other_delay_start), levels(test2$other_delay_end))
flows2 <- test2 %>%
  mutate(other_delay_start = factor(other_delay_start, levels = levs),
         other_delay_end = factor(other_delay_end, levels = levs)) %>%
  count(other_delay_start, other_delay_end, name = "n")

p <- ggplot(flows2,
            aes(axis1 = other_delay_start,
                axis2 = other_delay_end,
                y = n)) +
  geom_alluvium(aes(fill = other_delay_start),
                width = 0.6,
                alpha = 0.85,
                discern = TRUE) +
  geom_stratum(width = 0.6, fill = "grey95", colour = "grey40") +
  # geom_text(stat = "stratum",
  #           aes(label = after_stat(stratum))) +
  ggfittext::geom_fit_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 42,
    min.size = 2,
    #max.size = 20,
    grow = FALSE
  ) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(size = 42),
    legend.text = element_text(size = 26),
    axis.text.y = element_text(size = 38),
    axis.title.y = element_text(size = 30),
    legend.key.size = unit(2, "cm"),
    panel.grid = element_blank()
  ) +
  labs(fill = "Other human delay \nstart point",
       y = "Count")


ggsave(
  "other_delay_flow.png",
  plot = p,
  width = 40,
  height = 30,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  "other_delay_flow.pdf",
  plot = p,
  width = 40,
  height = 30,
  units = "in",
  dpi = 300,
  bg = "white"
)


library(knitr)
library(kableExtra)

tab <- flows |>
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Transition counts",
    longtable = FALSE
  ) |>
  kable_styling(
    latex_options = c("hold_position")
  )

save_kable(tab, "other_delays_table.tex")
