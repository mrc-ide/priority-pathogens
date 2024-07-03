#task to collate lassa outputs

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("sars_world_map", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_2_world_map.png","figure_2_world_map.pdf"))
orderly_dependency("sars_delays", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_5_delays.png","figure_5_delays.pdf","figure_5SI_subgroup_meta.png","figure_5SI_delays.png"))
orderly_dependency("sars_transmission", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_3_r_plot.png",
                             "figure_3_r_plot.pdf",
                             "figure_3SI_r_plot.png",
                             "figure_3SI_r_plot_cnt.png",
                             "figure_3SI_r_plot_method.png",
                             "figure_4_r_other.png",
                             "figure_4_r_other.pdf",
                             "figure_4SI_r_other.png"))
orderly_dependency("sars_summary", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_S1.png","figure_S2.png","figure_S3.png"))
orderly_dependency("sars_latex_tables", "latest(parameter:pathogen == this:pathogen)",
                   files = c("latex_models.csv",
                             "latex_transmission.csv","latex_delays.csv",
                             "latex_severity.csv","latex_seroprevalence.csv",
                             "latex_riskfactors.csv"))
orderly_artefact("sars-specific outputs",
                 c("main/figure_2_world_map.png","main/figure_2_world_map.pdf",
                   "main/figure_3_r_plot.png","main/figure_3_r_plot.pdf",
                   "main/figure_4_r_other.png","main/figure_4_r_other.pdf",
                   "main/figure_5_delays.png","main/figure_5_delays.pdf",
                   
                   "supp/figure_S1.png","supp/figure_S2.png","supp/figure_S3.png",
                   "supp/figure_3SI_r_plot.png", "supp/figure_3SI_r_plot_cnt.png",
                   "supp/figure_3SI_r_plot_method.png","supp/figure_4SI_r_other.png",
                   "supp/figure_5SI_subgroup_meta.png","supp/figure_5SI_delays.png",
                   
                   "supp/latex_models.csv", "supp/latex_seroprevalence.csv","supp/latex_riskfactors.csv",
                   "supp/latex_transmission.csv","supp/latex_delays.csv","supp/latex_severity.csv"))

###############
## MAIN TEXT ##
###############

dir.create("main")

file.copy(from = "figure_2_world_map.png", to = "main/figure_2_world_map.png")
file.copy(from = "figure_2_world_map.pdf", to = "main/figure_2_world_map.pdf")

file.copy(from = "figure_3_r_plot.png", to = "main/figure_3_r_plot.png")
file.copy(from = "figure_3_r_plot.pdf", to = "main/figure_3_r_plot.pdf")

file.copy(from = "figure_4_r_other.png", to = "main/figure_4_r_other.png")
file.copy(from = "figure_4_r_other.pdf", to = "main/figure_4_r_other.pdf")

file.copy(from = "figure_5_delays.png", to = "main/figure_5_delays.png")
file.copy(from = "figure_5_delays.pdf", to = "main/figure_5_delays.pdf")

###############################
## SUPPLEMENTARY INFORMATION ##
###############################

dir.create("supp")

file.copy(from = "figure_S1.png", to = "supp/figure_S1.png")
file.copy(from = "figure_S2.png", to = "supp/figure_S2.png")
file.copy(from = "figure_S3.png", to = "supp/figure_S3.png")

file.copy(from = "figure_3SI_r_plot.png",        to = "supp/figure_3SI_r_plot.png")
file.copy(from = "figure_3SI_r_plot_cnt.png",    to = "supp/figure_3SI_r_plot_cnt.png")
file.copy(from = "figure_3SI_r_plot_method.png", to = "supp/figure_3SI_r_plot_method.png")
file.copy(from = "figure_4SI_r_other.png",       to = "supp/figure_4SI_r_other.png")
file.copy(from = "figure_5SI_subgroup_meta.png", to = "supp/figure_5SI_subgroup_meta.png")
file.copy(from = "figure_5SI_delays.png",        to = "supp/figure_5SI_delays.png")

file.copy(from = "latex_models.csv",         to = "supp/latex_models.csv")
file.copy(from = "latex_transmission.csv",   to = "supp/latex_transmission.csv")
file.copy(from = "latex_delays.csv",         to = "supp/latex_delays.csv")
file.copy(from = "latex_severity.csv",       to = "supp/latex_severity.csv")
file.copy(from = "latex_seroprevalence.csv", to = "supp/latex_seroprevalence.csv")
file.copy(from = "latex_riskfactors.csv",    to = "supp/latex_riskfactors.csv")