library(dplyr)
library(ggplot2)
library(ggsci)
library(forcats)
library(orderly2)
library(patchwork)
library(readr)
library(stringr)
library(tidyr)

# *--------------------------------- Orderly ----------------------------------*
orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "outbreaks.csv", "models.csv", "params.csv"))

orderly_shared_resource("nipah_functions.R" = "nipah_functions.R")
source("nipah_functions.R")

# orderly_artefact(description="Nipah delay figures",
#                  c("figure_SI_risk_panel.pdf",
#                    "figure_SI_risk_facet.pdf"))

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

#parameters - risk factors
parameters <- parameters |>
  filter(parameter_class=="Risk factors")

parameters |>
  NROW()

parameters$riskfactor_name <- str_replace_all(parameters$riskfactor_name,
                                              ";", ",")

risk_table <- parameters |>
  separate_longer_delim(riskfactor_name, delim = ",") |>
  mutate(population_sample_size = replace_na(as.double(population_sample_size),0),
         riskfactor_adjusted = replace_na(riskfactor_adjusted, "Unspecified"),
         riskfactor_significant = replace_na(riskfactor_significant, "Unspecified"),
         riskfactor_outcome=str_replace_all(
           riskfactor_outcome, "\\(?in general population\\)?", ""),
         riskfactor_outcome=str_replace_all(
           riskfactor_outcome,
           c("Other neurological symptoms"="Neurological symptoms")),
         riskfactor_outcome=str_replace_all(
           riskfactor_outcome, c("Spillover risk"="Spillover")),
         riskfactor_outcome=str_trim(riskfactor_outcome),
         riskfactor_name=str_trim(riskfactor_name),
         riskfactor_name=str_to_sentence(riskfactor_name)) |>
  group_by(riskfactor_outcome, riskfactor_name,
           riskfactor_significant, riskfactor_adjusted) |>
  summarise(n=n(),
            pop_size = sum(population_sample_size)) |>
  unite(significant_adjusted,riskfactor_significant:riskfactor_adjusted,
        remove = FALSE, sep = " / ") |>
  mutate(significant_adjusted=factor(
    significant_adjusted,
    levels=c('Significant / Adjusted',
             'Not significant / Adjusted',
             'Significant / Not adjusted',
             'Not significant / Not adjusted',
             'Significant / Unspecified',
             'Not significant / Unspecified',
             'Unspecified / Adjusted',
             'Unspecified / Not adjusted',
             'Unspecified / Unspecified')))

# Params
text_size <- 15
colour_palette <- ggsci::pal_lancet("lanonc")(9)
temp <- colour_palette[7]
# Or "#ff8c00"
colour_palette[7] <- "#ffa500"

temp_2 <-colour_palette[8]
colour_palette[8] <- "#f0e68c"

colour_palette <- c(colour_palette, temp, temp_2)
scales::show_col(colour_palette)

custom_colours <- c('Significant / Adjusted'= colour_palette[3],
                    'Significant / Not adjusted' = colour_palette[1],
                    'Significant / Unspecified'=colour_palette[5],
                    'Not significant / Adjusted'=colour_palette[8],
                    'Not significant / Not adjusted' = colour_palette[7],
                    'Not significant / Unspecified'= colour_palette[2],
                    'Unspecified / Adjusted'='gray30',
                    'Unspecified / Not adjusted' = 'grey50',
                    'Unspecified / Unspecified'='grey70')

# Death
risk_factor_plot <- function(risk_table, outcome, custom_colours){
  rf_plot <- risk_table |>
    filter(riskfactor_outcome==outcome) %>%
    ggplot(aes(x=n,
               y=riskfactor_name,
               col=significant_adjusted,
               fill=significant_adjusted)) +
    geom_bar(stat='identity', position = position_dodge2(preserve = "single"),
             color="black") +
    scale_color_manual(name="Significant / Adjusted",
                       values = custom_colours) +
    scale_fill_manual(name="Significant / Adjusted",
                      values = custom_colours) +
    xlab('Count') + ylab('Risk factor name') +
    scale_x_continuous(breaks = function(x)
      unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))) +
    theme_minimal() +
    theme(strip.text = element_text( color = "black"),
          panel.border = element_rect(color = "black", size = 1.25, fill = NA),
          text = element_text(size = text_size),
          strip.background =element_rect(fill="white"))

  return(rf_plot)
}

# *-------------------------------- Patch work --------------------------------*
risk_table_plt_death <- risk_factor_plot(risk_table, "Death",
                                         custom_colours) +
  ggtitle('Death') + theme(legend.position = "none")
risk_table_plt_infection <- risk_factor_plot(risk_table, "Infection",
                                             custom_colours) +
  ggtitle('Infection') + theme(legend.position = "none")
risk_table_plt_sero <-  risk_factor_plot(risk_table, "Serology",
                                         custom_colours) +
  ggtitle('Serology') + theme(legend.position = "none")
risk_table_plt_spill <-  risk_factor_plot(risk_table, "Spillover",
                                          custom_colours) +
  ggtitle('Spillover') + theme(legend.position = "none")
risk_table_plt_neuro <-  risk_factor_plot(risk_table, "Neurological symptoms",
                                          custom_colours) +
  ggtitle('Neurological symptoms') + theme(legend.position = "none")
risk_table_plt_other <-  risk_factor_plot(risk_table, "Other",
                                          custom_colours) +
  ggtitle('Other')

# Everything else
risk_table_plt <- (risk_table_plt_infection  / risk_table_plt_sero / risk_table_plt_neuro) |
  ( risk_table_plt_death/ risk_table_plt_spill / risk_table_plt_other) +
  plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect")

risk_table_plt
left_col  <- risk_table_plt_infection / risk_table_plt_sero/ risk_table_plt_neuro
right_col <- risk_table_plt_death / risk_table_plt_spill / risk_table_plt_other

(left_col | right_col) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect", widths = c(1, 1), heights = c(7/15, 1.5, 1.5))

ggsave("figure_SI_risk_panel.pdf", plot = risk_table_plt, width = 20, height = 18)

# *---------------------------------- Facets ----------------------------------*
# Stack and dodge doesn't work since 3 groups (to achieve this use multiples
# calls to geom col, could work with manual x-axis position assignment)
# *----------------------- Option 1 - with neurological -----------------------*
rf_facet_o1 <- risk_table |>
  mutate(riskfactor_outcome=factor(riskfactor_outcome,
                                   levels=c("Infection",
                                            "Serology",
                                            "Neurological symptoms",
                                            "Death",
                                            "Spillover",
                                            "Other"))) |>
  arrange(riskfactor_outcome, fct_relevel(riskfactor_name, "Other",
                                          after = Inf)) |>
  ggplot(aes(x=factor(riskfactor_name),
             y=n,
             col=significant_adjusted,
             fill=significant_adjusted,
             group=riskfactor_adjusted)) +
  geom_col(stat='identity',
            width=0.9,
            color="black", position=position_dodge2(width = 0.9,
                                                    preserve = "single")) +
    scale_x_discrete(labels = c("Contact with animal" = "Contact \nwith animal",
                                "Close contact" = "Close\ncontact")) +
  scale_y_continuous(breaks = function(x)
    unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))) +
  scale_color_manual(name="Significant / Adjusted", values = custom_colours) +
  scale_fill_manual(name="Significant / Adjusted", values = custom_colours) +
  xlab('Count') + ylab('Risk factor name') +
  theme_minimal() +
  theme(strip.text = element_text( color = "black"),
        panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        text = element_text(size = text_size),
        strip.background =element_rect(fill="white", color="white"),
        legend.position="top") +
  facet_wrap(~riskfactor_outcome, scales="free", ncol=2)

ggsave("figure_SI_risk_facet_option_1.pdf", plot = rf_facet_o1,
       width = 16, height = 10)

rt_plot <- risk_table |>
  mutate(riskfactor_outcome=ifelse(riskfactor_outcome=="Neurological symptoms",
                                   "Other", riskfactor_outcome)) |>
  group_by(riskfactor_outcome, riskfactor_name, significant_adjusted,
           riskfactor_significant, riskfactor_adjusted) |>
    summarise(n=sum(n),
              pop_size=sum(pop_size)) |>
  mutate(riskfactor_outcome=factor(riskfactor_outcome,
                                   levels=c("Infection",
                                            "Serology",
                                            "Death",
                                            "Spillover",
                                            "Other"))) |>
  arrange(riskfactor_outcome, fct_relevel(riskfactor_name, "Other",
                                          after = Inf))

# *--------------------------------- Option 2 ---------------------------------*
rf_facet_o2 <- rt_plot |>
  group_by(riskfactor_outcome, riskfactor_name) |>
  mutate(total_count = sum(n),
         riskfactor_name=case_when(
           riskfactor_name=="Contact with animal"~"Contact\nwith animal",
           riskfactor_name=="Close contact"~"Close\ncontact",
           TRUE~riskfactor_name)) |>
  ungroup() |>
  ggplot(aes(x=tidytext::reorder_within(riskfactor_name,
                                        -total_count,
                                        riskfactor_outcome),
             y=n,
             col=significant_adjusted,
             fill=significant_adjusted)) +
  geom_col(stat='identity',
           width=0.9,
           color="black", position=position_dodge2(width = 0.9,
                                                   preserve = "single")) +
  tidytext::scale_x_reordered() +
  scale_y_continuous(breaks = function(x) unique(
    floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))) +
  scale_color_manual(name="Significant / Adjusted", values = custom_colours) +
  scale_fill_manual(name="Significant / Adjusted", values = custom_colours) +
  xlab('Risk factor name') + ylab('Count') +
  theme_minimal() +
  theme(strip.text = element_text( color = "black", size=15),
        panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        text = element_text(size = 15),
        strip.background =element_rect(fill="white", color="white"),
        legend.position= c(0.76,0.14),
        axis.text.x = element_text(color="black", size=13),
        axis.text.y = element_text(color="black", size=13)) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE)) +
  facet_wrap(~riskfactor_outcome, scales="free", ncol=2)

rf_facet_o2

ggsave("figure_SI_risk_facet_option_2.pdf", plot = rf_facet_o2,
       width = 17, height = 12)

# *------------------------- Option 3 - geom_pattern --------------------------*
option_3_table <- rt_plot |>
  ungroup() |>
  group_by(riskfactor_outcome, riskfactor_name, riskfactor_significant) |>
  mutate(total_count = sum(n),
         riskfactor_name=case_when(
           riskfactor_name=="Contact with animal"~"Contact\nwith animal",
           riskfactor_name=="Close contact"~"Close\ncontact",
           TRUE~riskfactor_name),
         riskfactor_name_new=paste0(riskfactor_name, " \n(",
                                   riskfactor_significant, ")")) |>
  ungroup()

rf_facet_o3 <- option_3_table |>
  group_by(riskfactor_name, riskfactor_outcome) |>
  mutate(overall_count=sum(n),
         riskfactor_adjusted=factor(riskfactor_adjusted,
                                    levels=c("Adjusted", "Not adjusted",
                                             "Unspecified")),
         riskfactor_significant=factor(riskfactor_significant,
                                    levels=c("Significant", "Not significant",
                                             "Unspecified"))) |>
  ggplot(aes(
    x = tidytext::reorder_within(riskfactor_name,
                                 overall_count,
                                 riskfactor_outcome),
    y = n,
    fill = riskfactor_significant,
    pattern = riskfactor_adjusted,
    colour = riskfactor_significant,
    group=riskfactor_significant
  )) +
  ggpattern::geom_bar_pattern(
    stat = "identity",
    width = 0.85,
    pattern_fill = "white",
    pattern_density = 0.2,
    pattern_spacing = 0.05,
    color="black",
    position=position_dodge2(preserve="single"),
  ) +
  ggpattern::scale_pattern_manual(
    name = "Adjusted",
    values = c(
      "Adjusted" = "crosshatch",
      "Not adjusted" = "circle",
      "Unspecified" = "none"
    )
  ) +
  scale_fill_manual(
    name = "Significant",
    values = c(
      'Significant' = colour_palette[3],
      'Not significant' = colour_palette[7],
      'Unspecified' = 'gray70'
    )
  ) +
  scale_y_continuous(
    breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))
  ) +
  tidytext::scale_x_reordered() +
  xlab("Risk factor name") +
  ylab("Count") +
  theme_minimal() +
  theme(
    strip.text = element_text(color = "black", size = 15),
    panel.border = element_rect(color = "black", size = 1.25, fill = NA),
    text = element_text(size = 15),
    strip.background = element_rect(fill = "white", color = "white"),
    legend.position = c(0.7, 0.19),
    legend.title = element_text(size=15),
    legend.text = element_text(size=13),
    axis.text.x = element_text(color = "black", size = 13),
    axis.text.y = element_text(color = "black", size = 13)
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(
      pattern = c("none", "none", "none"))
    ),
    pattern = guide_legend(nrow = 1, override.aes = list(
      fill = c("white", "white", "white"),
      pattern_density = 0.3,
      pattern_spacing = 0.03,
      pattern_key_scale_factor = 0.4))) +
  facet_wrap(~riskfactor_outcome, scales = "free", ncol = 2)

ggsave("figure_SI_risk_facet_option_3.pdf", plot = rf_facet_o3,
       width = 14, height = 10)
# ------------------------------------------------------------------------------
# *--------------------------------- Option 4 ---------------------------------*
option_4_table <- rt_plot |>
  ungroup() |>
  group_by(riskfactor_outcome, riskfactor_name, riskfactor_significant) |>
  mutate(total_count = sum(n),
         riskfactor_name=case_when(
           riskfactor_name=="Contact with animal"~"Contact\nwith animal",
           riskfactor_name=="Close contact"~"Close\ncontact",
           TRUE~riskfactor_name),
         riskfactor_name_new=paste0(riskfactor_name, " \n(",
                                    riskfactor_significant, ")")) |>
  ungroup()

option_4_table <- option_4_table |>
  ungroup() |>
  group_by(riskfactor_outcome, riskfactor_name, riskfactor_significant) |>
  mutate(first=ifelse(riskfactor_adjusted=="Unspecified", n, NA),
         second=ifelse(riskfactor_adjusted=="Not adjusted",
                       sum(first[riskfactor_adjusted=="Unspecified"],
                           na.rm=T) + n, NA),
            third=ifelse(riskfactor_adjusted=="Adjusted",
                         sum(second[riskfactor_adjusted=="Not adjusted"],
                             na.rm=T) + n, NA),
            second=ifelse(second==0, NA, second),
            third=ifelse(third==0, NA, third)) |>
  group_by(riskfactor_name, riskfactor_outcome) |>
  mutate(overall_count=sum(n),
         riskfactor_adjusted=factor(riskfactor_adjusted,
                                    levels=c("Adjusted", "Not adjusted",
                                             "Unspecified")),
         riskfactor_significant=factor(riskfactor_significant,
                                       levels=c("Significant", "Not significant",
                                                "Unspecified")))

rf_facet_o4 <- option_4_table |>
  ggplot(aes(
    x = tidytext::reorder_within(riskfactor_name,
                                 overall_count,
                                 riskfactor_outcome),
    fill = riskfactor_significant,
    pattern = riskfactor_adjusted,
    colour = riskfactor_significant,
    group = riskfactor_significant)) +
  ggpattern::geom_bar_pattern(data=option_4_table,
                              aes(y=third),
    stat = "identity",
    width = 0.95,
    pattern_fill = "white",
    pattern_density = 0.3,
    pattern_spacing = 0.1,
    color="black",
    position=position_dodge(preserve="single"),
  ) +
  ggpattern::geom_bar_pattern(data=option_4_table,
                              aes(y=second),
                              stat = "identity",
                              width = 0.95,
                              pattern_fill = "white",
                              pattern_density = 0.3,
                              pattern_spacing = 0.1,
                              color="black",
                              position=position_dodge(preserve="single"),
  ) +
  ggpattern::geom_bar_pattern(data=option_4_table,
                              aes(y=first),
                              stat = "identity",
                              width = 0.95,
                              pattern_fill = "white",
                              pattern_density = 0.3,
                              pattern_spacing = 0.1,
                              color="black",
                              position=position_dodge(preserve="single"),
  ) +
  ggpattern::scale_pattern_manual(
    name = "Adjusted",
    values = c(
      "Adjusted" = "stripe",
      "Not adjusted" = "circle",
      "Unspecified" = "none"
    ),
    labels=c("Adjusted   ", "Not adjusted  ", "Unspecified")
  ) +
  scale_fill_manual(
    name = "Significant",
    values = c(
      'Significant' = colour_palette[3],
      'Not significant' = colour_palette[7],
      'Unspecified' = 'gray70'
    )
  ) +
  scale_y_continuous(
    breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))
  ) +
  tidytext::scale_x_reordered() +
  xlab("Risk factor name") +
  ylab("Count") +
  theme_minimal() +
  theme(
    strip.text = element_text(color = "black", size = 15),
    panel.border = element_rect(color = "black", size = 1.25, fill = NA),
    text = element_text(size = 15),
    strip.background = element_rect(fill = "white", color = "white"),
    legend.position = c(0.7, 0.19),
    legend.title = element_text(size=15),
    legend.text = element_text(size=14),
    legend.key.size = unit(.75, "cm"),
    axis.text.x = element_text(color = "black", size = 13),
    axis.text.y = element_text(color = "black", size = 13)
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = TRUE,
                        override.aes = list(
                          pattern = c("none", "none", "none"))
    ),
    pattern = guide_legend(nrow = 1, override.aes = list(
      fill = c("white", "white", "white"),
      pattern_density = 0.3,
      pattern_spacing = 0.03,
      pattern_key_scale_factor = 0.5))) +
  facet_wrap(~riskfactor_outcome, scales = "free", ncol = 2)

ggsave("figure_SI_risk_facet_option_4.pdf", plot = rf_facet_o4,
       width = 14, height = 10)
