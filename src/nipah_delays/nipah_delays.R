# *=============================== Nipah delays ===============================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(orderly2)
library(patchwork)
library(readr)
library(stringr)

# *--------------------------------- Orderly ----------------------------------*
orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "outbreaks.csv", "models.csv", "params.csv"))

orderly_shared_resource("nipah_functions.R" = "nipah_functions.R")
source("nipah_functions.R")

orderly_artefact(description="Nipah delay figures",
                 c("figure_5_delays.pdf",
                   "figure_5_delays.png",
                   "figure_5SI_allqa_delays.pdf",
                   "figure_5SI_allqa_delays.png",
                   "all/figure_5SI_allqa_admis_outcome_pt.pdf",
                   "all/figure_5SI_allqa_incubation_pc.pdf",
                   "all/figure_5SI_allqa_incubation_pg.pdf",
                   "all/figure_5SI_allqa_incubation_pst.pdf",
                   "all/figure_5SI_allqa_onset_admis_outcome_pt.pdf",
                   "all/figure_5SI_allqa_onset_admis_pc.pdf",
                   "all/figure_5SI_allqa_onset_admis_pg.pdf",
                   "all/figure_5SI_allqa_onset_admis_pst.pdf",
                   "all/figure_5SI_allqa_onset_death_pc.pdf",
                   "all/figure_5SI_allqa_onset_death_pg.pdf",
                   "all/figure_5SI_allqa_onset_death_pst.pdf",
                   "all/figure_5SI_allqa_onset_outcome_pt.pdf",
                   "qa/figure_5_admis_outcome_pt.pdf",
                   "qa/figure_5_incubation_pc.pdf",
                   "qa/figure_5_incubation_pg.pdf",
                   "qa/figure_5_incubation_pst.pdf",
                   "qa/figure_5_onset_admis_outcome_pt.pdf",
                   "qa/figure_5_onset_admis_pc.pdf",
                   "qa/figure_5_onset_admis_pg.pdf",
                   "qa/figure_5_onset_admis_pst.pdf",
                   "qa/figure_5_onset_death_pc.pdf",
                   "qa/figure_5_onset_death_pg.pdf",
                   "qa/figure_5_onset_death_pst.pdf",
                   "qa/figure_5_onset_outcome_pt.pdf"))

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")


# 2 Gamma uncertainties:
# 6289bd0b84d80771220a16da295c90a2 : Nikolay (2019) Human delay - serial interval
# 7eb2ec3c3268d44a1edfab0f7c95f9ff : Nikolay (2019) Human delay - incubation period
dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

parameters <- parameters |>
  filter(parameter_class=="Human delay")

parameters |>
  write_csv("delay_temp_check.csv")

# Symptom Onset/Fever -> Recovery/Death
parameters[parameters$access_param_id=="121_003",
           "parameter_type"] <- "Human delay - symptom onset>recovery/death"

severe_illness_ids <- c("046_001",
                        "040_002",
                        "040_004",
                        "040_005",
                        "033_007",
                        "033_008")
parameters[parameters$access_param_id%in% severe_illness_ids,
           "parameter_type"] <- "Human delay - symptom onset>severe illness"

parameters <- parameters |>
  mutate(parameter_type = str_replace(parameter_type, "Human delay - ", ""),
         parameter_type = str_to_sentence(parameter_type))

# Favour uncertainty over variability:

# *--------------------------------- Summary ----------------------------------*
num_delays <- NROW(parameters)

cat("\nIn total", num_delays, "delays were extracted.")

cat("\nThe composition is as follows:\n")
parameters |>
  group_by(parameter_type) |>
  count() |>
  arrange(desc(n)) |>
  print()

cat("\nThe other delays excluded from plotting are as follows
    (note,",NROW(severe_illness_ids), "other delays have been mapped to",
    "Symptom onset>severe illness and one has been mapped to",
    "Symptom onset>recovery/death):\n")

parameters |>
  filter(parameter_type == "Other human delay (go to section)") |>
  select(parameter_type, other_delay_start, other_delay_end) |>
  print()



# Eight rows with variability only
varb_only_rows <- parameters |>
  filter(is.na(parameter_value) &
           is.na(parameter_lower_bound) &
           is.na(parameter_upper_bound)) |>
  NROW()

cat("Number of variability only rows:", varb_only_rows)
# *------------------------------ Plot datasets -------------------------------*
# Filter varb only rows
# Remove Chua quantile only (only range and quantile no central)
# Upper bound is 62 of incb with varb due to Chua
# To only filter Chua quantile:
# parameters <- parameters |>
#   filter(parameter_data_id!="b9236a13a7879c0488778f27b20e1c3d")
parameters <- parameters |>
  filter(!is.na(parameter_value) |
           !is.na(parameter_lower_bound) |
           !is.na(parameter_upper_bound))

# In order of initial outbreak per country
parameters <- parameters |>
  mutate(population_country=factor(population_country,
                                   levels=c("Malaysia", "Bangladesh",
                                            "India", "Philippines"))
         )

# Incubation period
d1 <- parameters %>% filter(tolower(parameter_type) == 'incubation period')

# Deduplication
# Nikolay (2019) is duplicated (reference below is a subset of the first)
# 37d6d213b05ac303ee32de21186977d4 large sample 82
# CI is based on gamma dist but extracted central parameter is median
# bb5bdf26abeda50067007b8db5d4bb15 small subset 11
# d1 <- d1 |>
#   filter(parameter_data_id!="bb5bdf26abeda50067007b8db5d4bb15")

# Onset to admission
d2 <- parameters |>
  filter(tolower(parameter_type) == 'symptom onset>admission to care')

# Hospital admission to outcome
d3 <- parameters |>
  filter(tolower(parameter_type) %in% c('time in care (length of stay)',
                                        'admission to care>discharge/recovery',
                                        'admission to care>death')
  )

# Symptom-onset to outcome
# CovID: 4057, symptom onset>discharge/recovery is duration of illness
d4 <- parameters |>
  filter(tolower(parameter_type) %in% c(
    "symptom onset>admission to care",
    'symptom onset>discharge/recovery',
    'symptom onset>death',
    'symptom onset>recovery/death',
    "symptom onset>severe illness")
  )

d5 <- parameters %>% filter(tolower(parameter_type) == 'serial interval')

# Combine before updating variable names
d6 <- d3 |>
  bind_rows(d4 |>
              filter(parameter_type!="Symptom onset>death"))

# Convert to factors
d3 <- d3 |>
  mutate(parameter_type=factor(parameter_type,
                               levels=c("Admission to care>death",
                                        "Admission to care>discharge/recovery",
                                        "Time in care (length of stay)"),
                               labels=c("Death",
                                        "Discharge/recovery",
                                        "Time in care")))

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

qa_thresh_vec <- c("all"=-1, "qa"=0.5)
labels <- c("SI_allqa", "")
colour_columns <- c("parameter_type",
                    "population_group",
                    "population_country",
                    "population_sample_type")

p1_incb_plots <- list("all"=list(), "qa"=list())
p2_oa_plots <- list("all"=list(), "qa"=list())
p3_ao_plots <- list("all"=list(), "qa"=list())
p4_oo_plots <- list("all"=list(), "qa"=list())
p6_oa_o_plots <- list("all"=list(), "qa"=list())
p7_o_a_plots <- list("all"=list())
p7_oo_reduced_plots <- list("all"=list())

for (i in seq_along(qa_thresh_vec)){
  label <- labels[i]
  qa_threshold <- qa_thresh_vec[i]
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

      # # Since we plot parameter_type and country in the same plot shift colours
      # # for parameter_type by the number of countries (4)
      # if (colour_col=="parameter_type"){
      #   shift <- 4
      # }else{
      #   shift <- 0
      # }

      custom_colours <- lanonc_colours[seq_along(all_groups)]
      custom_colours <- setNames(custom_colours, all_groups)

      # 65 with Chua
      p1_incb_plots[[plot_type]][[colour_col]] <- forest_plot(
        d1 |> filter(qa_score>qa_threshold), "Incubation period (days)",
        colour_col, c(0,35), text_size=text_size, segment_show.legend = NA,
        sort=TRUE, custom_colours = custom_colours)

      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_incubation_",
                              colour_col_label, ".pdf")),
             plot = p1_incb_plots[[plot_type]][[colour_col]],
             width = 11, height = 15)

    # Onset to admissions
      p2_oa_plots[[plot_type]][[colour_col]] <- forest_plot(
        d2 |> filter(qa_score>qa_threshold),
        'Symptom onset-to-hospitalisation delay (days)', colour_col, c(0,20),
        text_size = text_size, sort=TRUE, custom_colours = custom_colours)

      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_onset_admis_",
                              colour_col_label, ".pdf")),
             plot = p2_oa_plots[[plot_type]][[colour_col]],
             width = 11, height = 15)
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
        colour_col, c(0,45), text_size = text_size, sort=TRUE,
        custom_colours = custom_colours)
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_admis_outcome_",
                              colour_col_label, ".pdf")),
             plot = p3_ao_plots[[plot_type]][[colour_col]],
             width = 11, height = 15)
    }

    # Potential typo in paper so no upper bound for the range is recorded, set to
    # param value so that the error bar plots
    # Manually draw line
    if (colour_col== "parameter_type"){
      d4_plot <- d4
      d4_plot_label <- "outcome"
      d4_x_axis_label <- 'Symptom onset-to-outcome (days)'
      xlim <- c(-2,85)

      all_groups <- d4_plot |>
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
      xlim <- c(0,50)
    }

    # Sim is duplicated
    sim_duplicated_row <- d4_plot |>
      filter(access_param_id=="171_002")

    d4_plot <- d4_plot |>
      filter(access_param_id!="171_002")

    p4_oo <- forest_plot(
      d4_plot |> filter(qa_score>qa_threshold),
      d4_x_axis_label, colour_col, xlim,
      text_size = text_size, sort=TRUE,
      segment_show.legend = c(shape=FALSE, colour=TRUE),
      custom_colours = custom_colours) +
      # Showing Sim overlaid
      geom_point(data=sim_duplicated_row,
                 aes(x = parameter_value, y = refs,
                     shape = parameter_value_type, fill = .data[[colour_col]]),
                 size = 3, stroke = 1,
                 color = "black", alpha = 1) +
      geom_linerange(data=d4_plot |> filter(qa_score>qa_threshold,
                                          covidence_id==275),
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

    if (colour_col== "parameter_type"){
      # Do  we want consistent colours across the SI and main plot?
      # If so, remove the filter
      if (plot_type=="all"){
        xlim <- c(0,85)
      }else{
        xlim <- c(0,40)
      }
      all_groups <- d6 |>
        filter(qa_score>qa_threshold) |>
        distinct(parameter_type) |>
        arrange(parameter_type) |>
        pull()

      custom_colours <- lanonc_colours[seq_along(all_groups)]
      custom_colours <- setNames(custom_colours, all_groups)

      p6_oa_o_plots[[plot_type]][[colour_col]] <- forest_plot(
        d6 |> filter(qa_score>qa_threshold),
        'Symptom onset/Hospitalisation-to-outcome (days)',
        colour_col, xlim, text_size = text_size, sort=TRUE,
        custom_colours = custom_colours)
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_onset_admis_outcome_",
                              colour_col_label, ".pdf")),
             plot = p6_oa_o_plots[[plot_type]][[colour_col]],
             width = 15, height = 15)

      p6_oa_o_plots[[plot_type]][[colour_col]] <-
        p6_oa_o_plots[[plot_type]][[colour_col]] +
        guides(shape =  guide_none(),
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
        colour_col, c(0,85), text_size = text_size, sort=TRUE,
        custom_colours = custom_colours)
      ggsave(file.path(plot_type,
                       paste0("figure_5", label, "_onset_outcome_reduced_",
                              colour_col_label, ".pdf")),
             plot = p7_oo_reduced_plots[[plot_type]][[colour_col]],
             width = 15, height = 15)

      p7_oo_reduced_plots[[plot_type]][[colour_col]] <-
        p7_oo_reduced_plots[[plot_type]][[colour_col]] +
        guides(shape =  guide_none(),
               color = guide_legend(title = "Outcome"))
    }

    # Update legends for final plot
    if(colour_col=="population_country"){
      p1_incb_plots[[plot_type]][["population_country"]] <-
        p1_incb_plots[[plot_type]][["population_country"]] +
        guides(shape = guide_none(),
               fill = guide_none(),
               color = guide_none())

      p4_oo_plots[[plot_type]][["population_country"]] <-
        p4_oo_plots[[plot_type]][["population_country"]] +
        scale_colour_manual(values=custom_colours,
                            limits=all_groups) +
        scale_fill_manual(values=custom_colours,
                          limits=all_groups) +
        guides(shape = guide_legend(title = "Parameter type", order=1),
               color=guide_legend(title="Country"))
    }
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
    delays_plot <-  (p1_incb_plots[[plot_type]][["population_country"]] +
                       p4_oo_plots[[plot_type]][["population_country"]])/(
                         p6_oa_o_plots[[plot_type]][["parameter_type"]]) +
      plot_layout(heights = c(2, 1), widths = c(2, 1)) +
      plot_annotation(tag_levels = 'A')

    ggsave(paste0("figure_5", label,"_delays.pdf"), plot = delays_plot,
           width = 25, height = 13)
    ggsave(paste0("figure_5", label,"_delays.png"), plot = delays_plot,
           width = 25, height = 13)
  }else{
    # If stacking plots in a single row with three cols, need to adjust the
    # legend to have the same size plot area?
    p3_ao_plots[[plot_type]][["parameter_type"]] <-
      p3_ao_plots[[plot_type]][["parameter_type"]] +
      guides(shape =  guide_none(),
             color = guide_legend(title = "Outcome")) + common_left_legend

    # p7_oo_reduced_plots[[plot_type]][["parameter_type"]] <-
    #   p7_oo_reduced_plots[[plot_type]][["parameter_type"]] + common_left_legend


    # delays_plot <- p1_incb_plots[[plot_type]][["population_country"]] +
    #   p4_oo_plots[[plot_type]][["population_country"]] +
    #   p6_oa_o_plots[[plot_type]][["parameter_type"]] +
    #   plot_annotation(tag_levels = 'A')

    delays_plot <- (p1_incb_plots[[plot_type]][["population_country"]] +
      p4_oo_plots[[plot_type]][["population_country"]]) / (
        p7_oo_reduced_plots[[plot_type]][["parameter_type"]] +
         p3_ao_plots[[plot_type]][["parameter_type"]]) +
      plot_layout(heights = c(1.25, 1), widths = c(1.25, 1)) +
      plot_annotation(tag_levels = 'A')


    ggsave(paste0("figure_5", label,"_delays.pdf"), plot = delays_plot,
           width = 28, height = 15)
    ggsave(paste0("figure_5", label,"_delays.png"), plot = delays_plot,
           width = 28, height = 15)
  }
}

# ==============================================================================
# Incubation facet:
incubation_pc_facet <- forest_plot(d1,
                                   "Incubation period (days)",
                                   "population_sample_type", c(0,35),
                                   text_size=text_size,
                                   segment_show.legend = NA,
                                   sort=TRUE) +
  facet_wrap("population_country", scales="free")


# Excluding: QA score too low anyway
d4 |>
  filter(covidence_id%in% c(2979, 4057)) |>
  select(parameter_type, qa_score)

# Only a single serial interval so plot not saved, but the extracted row contains
# both variability and uncertainty (derived from the gamma dist - not directly
# stated in the paper
p5_aout <- forest_plot(d5,
                       'Serial interval (days)',
                       'parameter_type',
                       c(0,20),
                       text_size = text_size)
