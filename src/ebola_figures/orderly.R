## Manuscript tables and figures

orderly_strict_mode()

library(png)

orderly_parameters(pathogen = "EBOLA")

orderly_artefact(
  "Figure for range plots",
  c(
    "Manuscript_figures/basic_r_plot.png",
    "Manuscript_figures/cfr_plot.png",
    "Manuscript_figures/delays_meta_analysis.png",
    "Manuscript_figures/combined_range_tables.png"
  )
)

# Get figures/tables from tasks
orderly_dependency("ebola_reproduction_number",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("R_plots/basic_r_outbreak_filtered.png",
                             "R_tables/qa_filtered/range_outbreak.png"))
orderly_dependency("ebola_severity",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Severity_plots/plot_split_outbreak_filtered.png",
                             "Severity_tables/qa_filtered/range_outbreak_country.png",
                             "Severity_tables/qa_filtered/range_outbreak.png"))
orderly_dependency("ebola_delays",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Delay_tables/qa_filtered/select_ranges_table.png",
                             "Meta_plots/meta_delays_variance_QAfiltered.png"))

dir.create("Manuscript_figures")


## Figure 1: PRISMA - TO DO

## Figure 2: Basic R plot with QA filter

f2 <- png::readPNG("R_plots/basic_r_outbreak_filtered.png")
writePNG(f2, "Manuscript_figures/basic_r_plot.png")

## Figure 3: CFR plot

f3 <- png::readPNG("Severity_plots/plot_split_outbreak_filtered.png")
writePNG(f3, "Manuscript_figures/cfr_plot.png")

## Figure 4: Delays meta-analysis

f4 <- png::readPNG("Meta_plots/meta_delays_variance_QAfiltered.png")
writePNG(f4, "Manuscript_figures/delays_meta_analysis.png")

## Figure 5: Combine range tables

t1 <- png::readPNG("R_tables/qa_filtered/range_outbreak.png")
t2 <- png::readPNG("Severity_tables/qa_filtered/range_outbreak_country.png")
t3 <- png::readPNG("Delay_tables/qa_filtered/select_ranges_table.png")

# Create plots for each image
plot1 <- rasterGrob(t1, width = 0.8)
plot2 <- rasterGrob(t2, width = 0.8)
plot3 <- rasterGrob(t3, width = 0.8)

heights <- c(
  heightDetails(plot1),
  heightDetails(plot2),
  heightDetails(plot3)
)

# Normalise the heights to make them proportional
p_heights <- heights / sum(heights)

# Arrange and display the plots in a grid
rf <- grid.arrange(plot1, plot2, plot3, ncol = 1, heights = p_heights)

ggsave("Manuscript_figures/combined_range_tables.png", rf, width = 8, height = 14)

