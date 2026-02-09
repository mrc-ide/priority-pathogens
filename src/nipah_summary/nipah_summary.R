library(dplyr)
library(forcats)
library(ggplot2)
library(orderly2)
library(readr)
library(stringr)
library(tibble)
library(tidyr)
library(patchwork)

# *============================================================================*
# *------------------------------ Orderly config ------------------------------*
orderly_strict_mode()

orderly_parameters(pathogen = NULL)

orderly_dependency(
  "db_cleaning",
  "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "models.csv", "params.csv", "outbreaks.csv"))

orderly_shared_resource("nipah_functions.R"="nipah_functions.R")

source("nipah_functions.R")

orderly_artefact(description = "Nipah supplementary figures",
                 c("figure_S1.pdf", "figure_S2.pdf", "figure_S3.pdf"))

# *------------------------------- Read in data -------------------------------*
articles <- read_csv("articles.csv", show_col_types = FALSE)
models <- read_csv("models.csv", show_col_types = FALSE)
outbreaks <- read_csv("outbreaks.csv", show_col_types = FALSE)
parameters <- read_csv("params.csv", show_col_types = FALSE)

# Should plotting be TRUE?
dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

outbreaks  <- dfs$outbreaks

# Colours
colour_palette <- ggsci::pal_lancet("lanonc")(9)
temp <- colour_palette[7]
# Or "#ff8c00"
colour_palette[7] <- "#ffa500"

temp_2 <-colour_palette[8]
colour_palette[8] <- "#f0e68c"

colour_palette <- c(colour_palette, temp, temp_2)

## scales::show_col(colour_palette)
# *--------------------------------- Articles ---------------------------------*
# convert to a %
quality <- articles |>
  rowwise() |>
  mutate(score = 100*qa_score)

# What if an article has more parameters?
# 1 study?
quality <- quality |>
  mutate(category = case_when(
    (covidence_id %in% models$covidence_id &
      covidence_id %in% parameters$covidence_id)~"Both",
    covidence_id %in% models$covidence_id~"Modelling Studies",
    TRUE~"Non-Modelling Studies"))

quality$category <- factor(quality$category,
                           levels = c("Non-Modelling Studies",
                                      "Modelling Studies"))

answers <- quality |>
  filter(!is.na(year_publication) & !is.na(pathogen)) |> # no articles where this is true
  select(covidence_id,qa_m1,qa_m2,qa_a3,qa_a4,qa_d5,qa_d6,qa_d7) |>
  pivot_longer(-covidence_id,names_to = "Question",values_to= "Assessment") |>
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
  geom_col(stat="count", fill = colour_palette[4], color = "black", width=1) +
  scale_x_continuous(limits = c(min(articles$year_publication)-1,
                                max(articles$year_publication)+1),
                     breaks = seq(2000, 2025, by = 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,16), expand = c(0, 0)) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA)) +
  labs(x = "Year of Publication", y = "Article Count")

p2 <- ggplot() +
  geom_histogram(data=quality, aes(x = score), binwidth = 10, boundary = 0,
                 fill = colour_palette[4], color = "black") +
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(limits = c(0,25), expand = c(0, 0)) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA)) +
  labs(x = "Quality Assessment Score (%)", y = "Article Count")

p3 <- ggplot() +
  geom_point(data = quality |> filter(category!="Both"),
             aes(x=year_publication,y=score,color=category)) +
  geom_point(data = quality |> filter(category=="Both"),
             aes(x=year_publication,y=score,color=category), size=1) + # Draw second so it shows
  geom_smooth(data = subset(quality, category == "Non-Modelling Studies"),
              aes(x=year_publication,y=score), span = 2,
              color = colour_palette[3], fill = colour_palette[3]) +
  geom_smooth(data = subset(quality, category == "Modelling Studies"),
              aes(x=year_publication,y=score), span = 2,
              color = colour_palette[2], fill = colour_palette[2]) +
  scale_x_continuous(limits = c(1998,2026), breaks = seq(2000, 2025, by=5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20)) +
  xlab("Year of Publication") + ylab("Quality Assessment Score (%)") +
  scale_color_manual(values = c("Non-Modelling Studies" = colour_palette[3],
                                "Modelling Studies"=colour_palette[2],
                                "Both"=colour_palette[4]),
                     name = NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = 'bottom')

p4 <- answers |>
  group_by(Question,Assessment) |> summarize(count=n()) |> ungroup() |>
  ggplot(aes(fill=Assessment, y=count, x=Question)) +
  geom_bar(position="stack", stat="identity" , color = "black") + theme_bw() +
  scale_y_continuous(limits = c(0,120), breaks = seq(0,300,by=40), expand = c(0, 0)) +
  scale_fill_manual(values = c(colour_palette[3],
                               colour_palette[2],
                               colour_palette[11]),
                    aesthetics = "fill",name="",breaks=c('Yes', 'No','NA')) +
  xlab("") + ylab("Article Count") +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA)) +
  theme(legend.position = 'bottom')

patchwork <- (p1 + p2 + p3 + p4) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')

ggsave("figure_S1.pdf", plot = patchwork, width = 12, height = 12)

# *---------------------------------- Models ----------------------------------*
models <- dfs$models

models <- models |>
  mutate(stoch_deter = replace_na(stoch_deter,'Unspecified'),
         assumptions = replace_na(assumptions,'Unspecified'),
         compartmental_type = replace_na(compartmental_type,'Unspecified'),
         interventions_type = replace_na(interventions_type,'Unspecified'),
         # Correct?
         # interventions_type = str_replace_all(interventions_type,"Unspecified",
         #                                      "Other"),
         transmission_route = replace_na(transmission_route,'Unspecified'),
         compartmental_type = ifelse(
           compartmental_type %in% c("Other compartmental, please specify"),
           "Other compartmental", compartmental_type))

# Transmission route
models <- models |>
  mutate(transmission_route = case_when(
    transmission_route %in%
      c("Human to human (direct contact),Vector/Animal to human",
        "Human to human (direct contact),Vector/Animal to human,Sexual") ~'Both Human to human (direct contact) and Vector/Animal to human',
    transmission_route %in%
      c('Human to human (direct contact),Sexual',
        'Human to human (direct contact),Unspecified') ~ 'Human to human (direct contact)',
    transmission_route %in%
      c('Vector/Animal to human,Unspecified') ~ 'Vector/Animal to human',
    TRUE ~ transmission_route))

assumption_replacements <- c(
  "Heterogenity in transmission rates - between human and vector"="Human-to-animal heterogeneous",
  "Heterogenity in transmission rates - between human groups"="Subgroup heterogeneous",
  "Heterogenity in transmission rates - over time"="Time heterogeneous",
  "Homogeneous mixing"="Homogeneous")

models$assumptions <- str_replace_all(models$assumptions,
                                      assumption_replacements)

models |>
  count(assumptions)

# If multiple assumptions route:
# models <- models |>
#   mutate(num_assumptions = str_count(assumptions, ",")+1,
#          assumptions = case_when(
#            assumptions=="Subgroup heterogeneous,Human-to-animal heterogeneous"~"Human-to-animal & subgroup heterogeneous",
#            num_assumptions>1~"Multiple assumptions",
#            TRUE~assumptions))

models <- models |>
  mutate(assumptions = ifelse(
    assumptions=="Subgroup heterogeneous,Human-to-animal heterogeneous",
    "Human-to-animal & subgroup heterogeneous", assumptions))

p1 <- ggplot() +
  geom_bar(data = models, aes(x = model_type, fill = stoch_deter),
           color = "black") +
  scale_y_continuous(limits = c(0,40), breaks = seq(0,40,by = 10),
                     expand = c(0,0)) +
  xlab("Model Type") + ylab("Model Count") +
  scale_fill_manual(values = c("Deterministic model" = colour_palette[4],
                               "Stochastic model" = colour_palette[2]),
                    name = NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = c(0,1), legend.justification = c(0,1), legend.box.just = "left")

model_colour_pal <- c("Branching process" = colour_palette[5],
                      "Compartmental" = colour_palette[7],
                      "Other" = colour_palette[3])
p2 <- models |>
  mutate(transmission_route=factor(
    transmission_route,
    levels = c("Human to human (direct contact)",
               "Vector/Animal to human",
               "Both Human to human (direct contact) and Vector/Animal to human",
               "Unspecified"))
    ) |>
  ggplot() +
  geom_bar(data = models,
           aes(x = transmission_route, fill = model_type), color = "black") +
  scale_x_discrete(
    labels = c("Airborne or close contact;Human to human (direct contact)" = "Airborne",
               "Both Human to human (direct contact) and Vector/Animal to human" = "Human \n(direct contact) & \nVector/Animal",
               "Human to human (direct contact)" = "Human \n(direct contact)",
               "Airborne or close contact" = 'Airbone or \n close contact')) +
  scale_y_continuous(limits = c(0,23), breaks = seq(0,20,by = 5), expand = c(0,0)) +
  xlab("Transmission Route(s)") + ylab("Model Count") +
  scale_fill_manual(values=model_colour_pal, name = NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = c(0,1), legend.justification = c(0,1), legend.box.just = "left")

# What does Human-to-animal mean?
p3 <- models |>
  separate_rows(assumptions, sep=",") |>
  mutate(assumptions=factor(assumptions,
                            levels = c("Homogeneous",
                                       "Time heterogeneous",
                                       "Human-to-animal heterogeneous",
                                       "Subgroup heterogeneous",
                                       "Human-to-animal & subgroup heterogeneous",
                                       "Other",
                                       "Unspecified"))
         ) |>
  ggplot(aes(x = assumptions, fill = model_type)) +
  geom_bar(color = "black") +
  scale_x_discrete(
    labels = c("Human-to-animal heterogeneous" = "Human-to-animal\nheterogeneous",
               "Time heterogeneous"="Time\nheterogeneous",
               "Subgroup heterogeneous" = "Subgroup\nheterogeneous",
               "Human-to-animal & subgroup heterogeneous"="Human-to-animal\n& subgroup\nheterogeneous",
               "Multiple assumptions"= "Multiple\nassumptions")) +
  scale_y_continuous(limits = c(0,23), breaks = seq(0,20,by = 5), expand = c(0,0)) +
  xlab("Model assumptions") + ylab("Model Count") +
  scale_fill_manual(values=model_colour_pal, name = NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none")

p4 <- models |>
  mutate(compartmental_type=factor(compartmental_type,
                                   levels = c("SIR", "SEIR", "SEIR-SEI",
                                              "Other compartmental",
                                              "Unspecified",
                                              "Not compartmental"),
                                   labels = c("SIR", "SEIR", "SEIR-SEI",
                                              "Other", "Unspecified",
                                              "Not compartmental"))) |>
  arrange(compartmental_type) |>
  ggplot(aes(x = compartmental_type,
             fill = model_type)) +
  geom_bar(color = "black") +
  scale_y_continuous(limits = c(0,25), breaks = seq(0,25,by = 5),
                     expand = c(0,0)) +
  xlab("Compartments") + ylab("Model Count") +
  scale_fill_manual(values=model_colour_pal, name = NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none")

p5 <- models |>
  mutate(theoretical_model=factor(theoretical_model,
                                  levels = c("No", "Yes"))) |>
  ggplot(aes(x = theoretical_model, fill = model_type)) +
  geom_bar(color = "black") +
  scale_x_discrete(labels = c("No" = "Fitted to Data",
                              "Yes" = "Theoretical")) +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,35,by = 5),
                     expand = c(0,0)) +
  xlab("Model Calibration") + ylab("Model Count") +
  scale_fill_manual(values=model_colour_pal, name = NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none")

p6 <-  models |>
  separate_rows(interventions_type, sep = ",") |>
  mutate(interventions_type = ifelse(
    interventions_type %in% c("Hospitals", "Treatment centres"),
    "Hospitals, Treatment centres", interventions_type),
    interventions_type= factor(interventions_type,
                               levels = c("Vector/Animal control",
                                          "Behaviour changes",
                                          "Quarantine",
                                          "Safe burials",
                                          "Contact tracing",
                                          "Treatment",
                                          "Hospitals, Treatment centres",
                                          "Vaccination",
                                          "Other",
                                          "Unspecified"))) |>
  ggplot(aes(x = interventions_type, fill = model_type)) +
  geom_bar(color = "black") +
  scale_x_discrete(labels = c("Vector/Animal control" = "Animal\ncontrol",
                              "Hospitals, Treatment centres" = "Hospitals,\ntreatment\ncentres",
                              "Behaviour changes" = "Behaviour\nchanges",
                              "Contact tracing" = "Contact\ntracing",
                              "Other" = "Other")) +
  scale_y_continuous(limits = c(0,15), breaks = seq(0,15,by = 5), expand = c(0,0)) +
  xlab("Interventions") + ylab("Model Count") +
  scale_fill_manual(values=model_colour_pal, name = NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none")

patchwork <- (p1 + p2 + p5 + p4 + p3 + p6) + plot_layout(ncol = 2, widths = c(1,1))
patchwork <- patchwork + plot_annotation(tag_levels = 'A')

ggsave("figure_S2.pdf", plot = patchwork, width = 16, height = 16)

# *-------------------------------- Parameters --------------------------------*
parameters <- dfs$parameters |>
  left_join(qa_scores) |>
  mutate(parameter_value = coalesce(parameter_value,central)) |>
  arrange(desc(parameter_value))

parameters <- parameters |>
  mutate(parameter_class = case_when(
    parameter_class %in% c("Attack rate") ~ "Other transmission parameters",
    parameter_class == "Human delay" ~ "Delays",
    TRUE ~ parameter_class))

parameters <- parameters |>
  mutate(
    parameter_type = case_when(
      parameter_type == "Human delay - admission to care>death" ~ "Admission-to-death",
      parameter_type == "Human delay - admission to care>discharge/recovery" ~ "Admission-to-discharge/recovery",
      parameter_type == "Human delay - incubation period" ~ "Incubation period",
      parameter_type == "Human delay - other human delay (go to section)" ~ "Other delay",
      parameter_type == "Human delay - symptom onset>admission to care" ~ "Onset-to-admission",
      parameter_type == "Human delay - symptom onset>death" ~ "Onset-to-death",
      parameter_type == "Human delay - symptom onset>discharge/recovery" ~ "Onset-to-discharge/recovery",
      parameter_type == "Human delay - time in care (length of stay)" ~ "Time in care",
      parameter_type == "Human delay - serial interval"~"Serial interval",
      parameter_type == "Mutations - evolutionary rate" ~ "Evolutionary rate",
      parameter_type == "Mutations - substitution rate" ~ "Substitution rate",
      parameter_type == "Reproduction number (Basic R0)" ~ "Basic (R0)",
      parameter_type == "Seroprevalence - IFA" ~ "IFA test",
      parameter_type == "Seroprevalence - IgG" ~ "IgG test",
      parameter_type == "Seroprevalence - IgM" ~ "IgM test",
      parameter_type == "Seroprevalence - PRNT" ~ "PRNT",
      parameter_type == "Seroprevalence - Unspecified" ~ "Unspecified test",
      parameter_type == "Severity - case fatality rate (CFR)" ~ "Case fatality ratio (CFR)",
      parameter_type == "Severity - proportion of symptomatic cases" ~ "Proportion of symptomatic cases",
    TRUE ~ parameter_type)
    )

# Study year categorisation
parameters <-parameters |>
  mutate(population_study_end_year=ifelse(population_study_end_year=="xxxx",
                                          NA,
                                          population_study_end_year),
         population_study_end_year=as.numeric(population_study_end_year),
         study_midyear = case_when(
           !is.na(population_study_start_year) & !is.na(population_study_end_year)~round(
             (as.integer(population_study_start_year) + as.integer(population_study_end_year)) / 2),
           !is.na(population_study_start_year)~population_study_end_year,
           !is.na(population_study_end_year)~population_study_start_year,
           TRUE~0),
         study_midyear_cat = case_when(
           study_midyear %in% 1998:2004 ~ "1998-2004",
           study_midyear %in% 2005:2009 ~ "2005-2009",
           study_midyear %in% 2010:2014 ~ "2010-2014",
           study_midyear %in% 2015:2019 ~ "2015-2019",
           study_midyear %in% 2020:2029 ~ "2020-Present",
           TRUE ~ "Unspecified"),
         study_midyear_cat=factor(
           study_midyear_cat,
           levels = c("1998-2004", "2005-2009", "2010-2014",
                      "2015-2019", "2020-Present",'Unspecified')))

# Sample type cleaning
# Removed capitalisation
parameters <- parameters |>
  mutate(population_sample_type = replace_na(population_sample_type, "Unspecified"))

# Colour pal
param_colour_palette <- c("Delays"=colour_palette[4],
                          "Mutations"=colour_palette[2],
                          "Other transmission parameters"=colour_palette[3],
                          "Overdispersion"=colour_palette[6],
                          "Reproduction number"=colour_palette[1],
                          "Risk factors"=colour_palette[5],
                          "Seroprevalence"=colour_palette[7],
                          "Severity"=colour_palette[8])

# Sort before plotting
ordering <- parameters |>
  count(parameter_class, parameter_type, name = "type_count") |>
  group_by(parameter_class) |>
  mutate(class_count = sum(type_count)) |>
  ungroup() |>
  arrange(desc(class_count), desc(type_count)) |>
  pull(parameter_type) |>
  unique()

parameters$parameter_type <- factor(parameters$parameter_type,
                                    levels = ordering)
p1 <- ggplot(data = parameters,
             aes(x = parameter_type, fill = parameter_class)) +
  geom_bar(color = "black") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(limits = c(0,90), breaks = seq(0,90,by = 10), expand = c(0,0)) +
  xlab("Parameter Type") +
  ylab("Parameter Count") +
  scale_fill_manual(values=param_colour_palette, name=NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = 'top' ) +
  coord_flip()

# 352 rows instead of 332 - 20 introduced
# separate_rows(population_country, sep = ";")
p2 <- ggplot(data = parameters |>
               separate_rows(population_country, sep = ";") |>
               mutate(population_country = str_trim(population_country)),
             aes(x = fct_infreq(population_country), fill = parameter_class)) +
  geom_bar(color = "black") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(limits = c(0,165),
                     breaks = seq(0,160,by = 20), expand = c(0,0)) +
  xlab("Study Country") + ylab("Parameter Count") +
  scale_fill_manual(values=param_colour_palette, name=NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = 'none') +
  coord_flip()

p3 <- ggplot(data = parameters,
             aes(x = study_midyear_cat, fill = parameter_class)) +
  geom_bar(color = "black") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(limits = c(0,145), breaks = seq(0,140,by = 20),
                     expand = c(0,0)) +
  xlab("Study Year") + ylab("Parameter Count") +
  scale_fill_manual(values=param_colour_palette, name=NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none") +
  coord_flip()

p4 <- ggplot() +
  geom_bar(data = parameters, aes(x = fct_infreq(population_sample_type), fill = parameter_class), color = "black") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(limits = c(0,145), breaks = seq(0,140,by = 20), expand = c(0,0)) +
  xlab("Study Setting") + ylab("Parameter Count") +
  scale_fill_manual(values=param_colour_palette, name=NULL) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", size = 1.25, fill = NA),
        legend.position = "none") +
  coord_flip()

parameters |>
  separate_rows(population_country, sep = ";") |>
  mutate(population_country = str_trim(population_country)) |>
  summarise(pst_count = n_distinct(population_sample_type),
            s_mid_count = n_distinct(study_midyear_cat),
            pc_count=n_distinct(population_country),
            pt_count=n_distinct(parameter_type),
            total=pst_count+s_mid_count+pt_count+pc_count)

# Size based on y-axis label count from above
patchwork <- p1 / p3 / p2 / p4 + plot_layout(ncol = 1,
                                             heights = c(21,6,8,9))
  plot_annotation(tag_levels = 'A')

ggsave("figure_S3.pdf", plot = patchwork, width = 12, height = 15)
