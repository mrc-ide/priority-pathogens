#task to collate lassa outputs

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("lassa_serology", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_2.png"))
orderly_dependency("lassa_severity", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_3.png","figure_S5.png","figure_S6.png","figure_S7.png",
                             "figure_S8.png","figure_S9.png","figure_S10.png","figure_S11.png"))
orderly_dependency("lassa_delays", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_4.png","figure_S4.png"))
orderly_dependency("lassa_transmission", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_5.png"))
orderly_dependency("lassa_summary", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_S1.png","figure_S2.png","figure_S3.png"))
orderly_dependency("lassa_latex_tables", "latest(parameter:pathogen == this:pathogen)",
                   files = c("latex_outbreaks.csv","latex_models.csv",
                             "latex_transmission.csv","latex_delays.csv","latex_severity.csv",
                             "latex_seroprevalence.csv","latex_riskfactors.csv"))
orderly_artefact("lassa-specific outputs",
                 c("main/figure_2.png","main/figure_3.png","main/figure_4.png","main/figure_5.png",
                   "supp/figure_S1.png","supp/figure_S2.png","supp/figure_S3.png","supp/figure_S4.png",
                   "supp/figure_S5.png","supp/figure_S6.png","supp/figure_S7.png","supp/figure_S8.png",
                   "supp/figure_S9.png","supp/figure_S10.png","supp/figure_S11.png",
                   "supp/latex_outbreaks.csv","supp/latex_models.csv",
                   "supp/latex_transmission.csv","supp/latex_delays.csv","supp/latex_severity.csv",
                   "supp/latex_seroprevalence.csv","supp/latex_riskfactors.csv"))

###############
## MAIN TEXT ##
###############

dir.create("main")

file.copy(from = "figure_2.png",
          to = "main/figure_2.png")
file.copy(from = "figure_3.png",
          to = "main/figure_3.png")
file.copy(from = "figure_4.png",
          to = "main/figure_4.png")
file.copy(from = "figure_5.png",
          to = "main/figure_5.png")

###############################
## SUPPLEMENTARY INFORMATION ##
###############################

dir.create("supp")

file.copy(from = "figure_S1.png",
          to = "supp/figure_S1.png")
file.copy(from = "figure_S2.png",
          to = "supp/figure_S2.png")
file.copy(from = "figure_S3.png",
          to = "supp/figure_S3.png")
file.copy(from = "figure_S4.png",
          to = "supp/figure_S4.png")
file.copy(from = "figure_S5.png",
          to = "supp/figure_S5.png")
file.copy(from = "figure_S6.png",
          to = "supp/figure_S6.png")
file.copy(from = "figure_S7.png",
          to = "supp/figure_S7.png")
file.copy(from = "figure_S8.png",
          to = "supp/figure_S8.png")
file.copy(from = "figure_S9.png",
          to = "supp/figure_S9.png")
file.copy(from = "figure_S10.png",
          to = "supp/figure_S10.png")
file.copy(from = "figure_S11.png",
          to = "supp/figure_S11.png")

file.copy(from = "latex_outbreaks.csv",
          to = "supp/latex_outbreaks.csv")
file.copy(from = "latex_models.csv",
          to = "supp/latex_models.csv")
file.copy(from = "latex_transmission.csv",
          to = "supp/latex_transmission.csv")
file.copy(from = "latex_delays.csv",
          to = "supp/latex_delays.csv")
file.copy(from = "latex_severity.csv",
          to = "supp/latex_severity.csv")
file.copy(from = "latex_seroprevalence.csv",
          to = "supp/latex_seroprevalence.csv")
file.copy(from = "latex_riskfactors.csv",
          to = "supp/latex_riskfactors.csv")