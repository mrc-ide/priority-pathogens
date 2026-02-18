# *============================== Nipah Serology ==============================*
library(dplyr)
library(ggplot2)
library(ggsci)
library(grid)
library(orderly2)
library(patchwork)
library(readr)
library(stringr)
library(tidyr)

# *--------------------------------- Orderly ----------------------------------*
orderly_strict_mode()

orderly_parameters(pathogen = NULL)

orderly_dependency(
  "db_cleaning",
  "latest(parameter:pathogen == this:pathogen)",
  c("articles.csv", "models.csv", "params.csv", "outbreaks.csv"))

orderly_shared_resource("nipah_functions.R"="nipah_functions.R")

source("nipah_functions.R")

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles,outbreaks,models,parameters, plotting = TRUE)

articles   <- dfs$articles
articles   <- epireview::assign_qa_score(articles = articles)$articles
qa_scores  <- articles |> dplyr::select(covidence_id,qa_score)

parameters <- dfs$parameters |>
  left_join(qa_scores)

# *----------------------------- Data preparation -----------------------------*
parameters <- parameters |>
  mutate(article_label = make.unique(refs)) |>
  mutate(article_label = factor(article_label,levels=rev(unique(article_label)))) |>
  mutate(in_CSF = case_when(str_detect(parameter_notes,'CSF')~TRUE,
                            TRUE~FALSE))

sero_studies <- parameters |>
  filter(parameter_class == 'Seroprevalence')

sero_no_unit_row_count <- sero_studies |>
  filter(parameter_unit=="Unspecified" | is.na(parameter_unit)) |>
  NROW()

cat("Number of extracted serology rows with no unit: ", sero_no_unit_row_count)

# Assumption: parameter unit is always a percentage
sero_studies <- sero_studies |>
  mutate(parameter_value = coalesce(parameter_value,central),
         parameter_unit="Percentage (%)",
         population_group = factor(
           population_group,
           levels = c(sort(setdiff(unique(population_group),
                                   c("Other", "Unspecified"))),
                      "Other", "Unspecified")))

# CSF studies
sero_studies |>
  filter(in_CSF) |>
  select(article_id, covidence_id, parameter_type, parameter_value,
         qa_score, refs) |>
  print()
# *---------------------------------- Plots -----------------------------------*
p1 <- forest_plot(sero_studies, 'Serology (%)', 'parameter_type', c(-4,104),
                  qa_alpha = 0.3, text_size = 28, sort=TRUE, point_size=6) +
  ggforce::facet_col(facets = vars(population_country),
                     scales = "free_y",
                     space = "free") +
  guides(fill = guide_none(),
         linetype = guide_none(),
         color = guide_legend(title = "Assay", order =2),
         shape = guide_legend(title = "Parameter type", order =1)) +
  theme(legend.position = c(0.84, 0.94))
ggsave(paste0("sero_apx_col_assay_pop_country.pdf"),
       plot = p1, width = 19, height = 35)

p2 <- forest_plot(sero_studies, 'Serology (%)', 'parameter_type', c(-4,104),
                  qa_alpha = 0.3, text_size = 28, sort=TRUE, point_size=6) +
  ggforce::facet_col(facets = vars(population_group),
                     scales = "free_y",
                     space = "free") +
  guides(fill = guide_none(),
         linetype = guide_none(),
         color = guide_legend(title = "Assay", order =2),
         shape = guide_legend(title = "Parameter type", order =1)) +
  theme(legend.position = c(0.84, 0.4))
ggsave(paste0("sero_apx_col_assay_pop_group.pdf"),
       plot = p2, width = 19, height = 35)

p3 <- forest_plot(sero_studies, 'Serology (%)', 'population_group', c(-4,104),
                  qa_alpha = 0.3, text_size = 28, sort=TRUE, point_size=6) +
  ggforce::facet_col(facets = vars(population_country),
                     scales = "free_y",
                     space = "free") +
  guides(fill = guide_none(),
         linetype = guide_none(),
         color = guide_legend(title = "Population group", order =2),
         shape = guide_legend(title = "Parameter type", order =1)) +
  theme(legend.position = c(0.86, 0.93))
ggsave(paste0("sero_apx_col_country_pop_country.pdf"),
       plot = p3, width = 19, height = 35)

# SI figures
# Split figures to allow for larger panels and figures to split over two pages
# Figure 1 by assay type
sero_apx_1 <-  (p1+ theme(legend.position = c(0.76, 0.94)) |
                  p2  + guides(color=guide_none(), shape=guide_none())) +
  plot_annotation(tag_levels = "A")

ggsave(paste0("sero_apx_col_assay_patchwork.pdf"),
       plot = sero_apx_1, width = 32, height = 38)



sero_studies <- parameters |>
  filter(parameter_class == "Seroprevalence") |>
  mutate(population_group = factor(population_group,
    levels = c(
      "General population",
      sort(
        setdiff(
          unique(population_group),
          c("General population", "Other", "Unspecified")
        )
      ),
      "Other", "Unspecified"
    )
  )) |>
  mutate(
    parameter_unit = "Percentage (%)",
    population_group = replace_na(population_group, "Other"),
    sero_CSF = case_when(
      str_detect(parameter_notes, "CSF") ~ "CSF",
      TRUE ~ "Other"
    ),
    parameter_type = str_replace(
      parameter_type,
      "Seroprevalence - ", ""
    )
  ) |>
  mutate(parameter_value = coalesce(parameter_value, central)) |>
  arrange(population_country, central)

custom_colour_pop_groups <- ggsci::pal_lancet("lanonc")(9)


common_cntries <- c("Bangladesh", "India", "Malaysia", "Philippines", "Singapore")
extra_cntries <- setdiff(
  unique(sero_studies$population_country),
  common_cols
)
countries <- c(common_cntries, extra_cntries)

custom_colour_countries <- lanonc_colours[seq_along(countries)]
names(custom_colour_countries) <- countries

sero_forest_pop_group <- forest_plot(
  sero_studies,
  sort = TRUE, "Serology (%)", "population_country", c(-4, 104),
  text_size = 13, qa_alpha = 0.3, custom_colours = custom_colour_countries
) +
  ggforce::facet_col(
    facets = vars(population_group),
    scales = "free_y",
    space = "free"
  ) +
  guides(
    shape = guide_none(),
    linetype = guide_none(),
    color = guide_legend(title = "Country")
  )

# manual forest plot to shape type
color_column <- "population_country"
qa_alpha <- 0.3


sero_studies$plot_alpha <- 1
sero_studies$segment_alpha <- 1

sero_studies[sero_studies$qa_score <= 0.5, ]$plot_alpha <- qa_alpha
sero_studies[sero_studies$qa_score <= 0.5, ]$segment_alpha <- 0.65 * qa_alpha

# remove geom_point
sero_forest_pop_group$layers <-
  sero_forest_pop_group$layers[1:(length(sero_forest_pop_group$layers) - 1)]

sero_forest_pop_group <- sero_forest_pop_group +
  geom_point(
    data = sero_studies |>
      mutate(
        urefs = make.unique(refs),
        urefs = factor(urefs, levels = rev(unique(urefs)))
      ),
    aes(
      x = parameter_value, y = urefs,
      shape = parameter_type,
      fill = population_country
    ),
    alpha = sero_studies$plot_alpha,
    size = 3, stroke = 1, color = "black"
  ) +
  scale_shape_manual(
    name = "Assay",
    values = c(
      IgG = 21, IgM = 22,
      PRNT = 23, Unspecified = 24
    ),
    breaks = c("IgG", "IgM", "PRNT", "Unspecified")
  ) +
  guides(shape = guide_legend(title = "Assay")) +
  theme(legend.position = c(0.9, 0.49))

ggsave("sero_forest_pop_group.png",
  plot = sero_forest_pop_group,
  width = 11, height = 20
)
