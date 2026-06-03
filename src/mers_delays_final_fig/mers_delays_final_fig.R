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
  mutate(population_country=ifelse(population_country=="Oman",
                                   "Other (Middle East)", population_country)) |>
  mutate(population_country=ifelse(population_country=="Oman; Saudi Arabia",
                                   "Other (Middle East)", population_country)) |>
  mutate(population_country=ifelse(population_country=="Qatar",
                                   "Other (Middle East)", population_country)) |>
  mutate(population_country=ifelse(population_country=="United Arab Emirates",
                                   "Other (Middle East)", population_country)) |>
  mutate(population_country=ifelse(population_country=="Republic of Korea; Saudi Arabia",
                                   "Other", population_country)) |>
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
all_country_groups2 <- c(all_country_groups[2:4], all_country_groups[1])

country_colours <- lanonc_colours[seq_along(all_country_groups2)]
country_colours <- setNames(country_colours, all_country_groups2)

d1$population_country <- factor(d1$population_country,
                     levels = all_country_groups)

# Convert population_country to factor with ALL levels in both datasets
d1 <- d1 |> mutate(population_country = factor(population_country, levels = all_country_groups))

      p1_incb <- forest_plot(
        d1, "Incubation period (days)",
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

      saveRDS(d1, "incubation_period_df.rds")
      ggsave("p1_incubation_period.pdf",
             plot = p1_incb,
             width = 11, height = 9)
      ggsave("p1_incubation_period.png",
             plot = p1_incb,
             width = 11, height = 9)


      d8_plot <- d6 |> filter(parameter_type=="Time in care (length of stay)")
      d8_plot_label <- "time_in_care"
      d8_x_axis_label <- 'Time in care (days)'
      xlim_d8 <- c(0,100)

      d8_plot <- d8_plot |> mutate(population_country = factor(population_country, levels = all_country_groups))


      # all_groups <- rbind(d4_plot, d8_plot) |>
      #   distinct(population_country) |>
      #   arrange(population_country) |>
      #   pull()
      #
      # custom_colours <- lanonc_colours[seq_along(all_groups)]
      # custom_colours <- setNames(custom_colours, all_groups)

    p2_time_in_care <- forest_plot(
      d8_plot,
      d8_x_axis_label, "population_country", xlim_d8,
      text_size = text_size, sort=TRUE,
      segment_show.legend = c(shape=FALSE, colour=TRUE),
      custom_colours = country_colours)

    p2_time_in_care <- p2_time_in_care +
      scale_colour_manual(name = "Country", values = country_colours, drop = FALSE) +
      guides(shape = guide_none(), linetype = guide_none(),
             color = guide_legend(title = "Country"))


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
        filter(qa_score>=0.5) |>
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
        d6 |> filter(qa_score>= 0.5) |>
          #REMOVE THIS TO GO BACK TO ORIGINAL
          filter(parameter_type %in% c("Onset>admission")),#,
                                       #"Onset>severe illness",
                                       #"Onset>recovery/death",
                                       #"Onset>discharge/recovery")),
        #'Symptom onset-to-outcome (days)',
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

      ###
      p4_outcomes <- forest_plot(
        d6 |> filter(qa_score>= 0.5) |>
          #REMOVE THIS TO GO BACK TO ORIGINAL
          filter(parameter_type %in% c("Onset>discharge/recovery")),
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



# Stick all 4 together:

    delays_plot <-  (p1_incb /#+
      p3_outcomes /
        p4_outcomes /
        p2_time_in_care) +
      plot_layout(heights = c(26, 24, 6, 26), #, widths = c(1, 1)
                  guides = "collect") +
      plot_annotation(tag_levels = 'A')

    ggsave("mers_delays.pdf", plot = delays_plot,
           width = 25, height = 27)
    ggsave("mers_delays.png", plot = delays_plot,
           width = 25, height = 27)

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
      "Case observation" = "Diagnosis/test result"
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
