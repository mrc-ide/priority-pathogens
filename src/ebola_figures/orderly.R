## Manuscript tables and figures

orderly_strict_mode()

library(png)
library(cowplot)
library(grid)
library(orderly2)
library(ggplot2)
library(magick)

orderly_parameters(pathogen = "EBOLA")


orderly_artefact(
  "Figure for range plots",
  c(
    "R_plots/basic_r_outbreak_filtered.pdf",
    "Severity_plots/plot_split_outbreak_filtered.pdf",
    "Meta_plots/meta_delays_main_text.pdf",
    "R_tables/qa_filtered/range_outbreak.docx",
    "Severity_tables/qa_filtered/range_outbreak_country.docx",
    "Delay_tables/qa_filtered/select_ranges_table.docx",
    "combined_range_tables.png"
  )
)

# Get figures/tables from tasks
orderly_dependency("ebola_reproduction_number",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("R_plots/basic_r_outbreak_filtered.pdf",
                             "R_tables/qa_filtered/range_outbreak.docx",
                             "R_tables/qa_filtered/range_outbreak.png"))
orderly_dependency("ebola_severity",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Severity_plots/plot_split_outbreak_filtered.pdf",
                             "Severity_tables/qa_filtered/range_outbreak_country.docx",
                             "Severity_tables/qa_filtered/range_outbreak_country.png"))
orderly_dependency("ebola_delays",
                   "latest(parameter:pathogen == this:pathogen)",
                   files = c("Delay_tables/qa_filtered/select_ranges_table.docx",
                             "Delay_tables/qa_filtered/select_ranges_table.png",
                             "Meta_plots/meta_delays_main_text.pdf"))

# Figure 5: Combine range tables

t1 <- png::readPNG("R_tables/qa_filtered/range_outbreak.png")
t2 <- png::readPNG("Delay_tables/qa_filtered/select_ranges_table.png")
t3 <- png::readPNG("Severity_tables/qa_filtered/range_outbreak_country.png")

# Create plots for each image
plot1 <- rasterGrob(t1, width = 0.85)
plot2 <- rasterGrob(t2, width = 0.85)
plot3 <- rasterGrob(t3, width = 0.85)

heights <- c(
  heightDetails(plot1),
  heightDetails(plot2),
  heightDetails(plot3)
)

# Normalise the heights to make them proportional
p_heights <- heights / sum(heights)

# Arrange and display the plots in a grid with panel labels
range_tables <- plot_grid(
  plot1, plot2, plot3,
  labels = c("A) Basic Reproduction Numbers ", "B) Epidemiological Delays", "C) Case Fatality Ratios"),
  label_size = 12,
  vjust = c(0, 0.5, 1),
  hjust = c(-0.1, -0.1, -0.1),
  ncol = 1,
  rel_heights = p_heights) +
  theme(plot.background = element_rect(color = "white", fill = "white"),
        plot.margin  = unit(c(1,0.5,0,0.5), "cm")
  )

ggsave("combined_range_tables.png", range_tables, width = 7, height = 15)

dev.off()