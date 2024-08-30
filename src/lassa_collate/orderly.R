#task to collate lassa outputs

#orderly preparation 
orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency("lassa_serology", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_2.png","figure_2.pdf"))
orderly_dependency("lassa_severity", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_3.png","figure_S7.png","figure_S8.png",
                             "figure_S9.png","figure_S10.png","figure_S11.png","figure_S12.png","figure_S13.png","figure_S14.png",
                             "figure_3.pdf","figure_S7.pdf","figure_S8.pdf",
                             "figure_S9.pdf","figure_S10.pdf","figure_S11.pdf","figure_S12.pdf","figure_S13.pdf","figure_S14.pdf"))
orderly_dependency("lassa_delays", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_4.png","figure_S5.png","figure_S6.png","figure_4.pdf","figure_S5.pdf","figure_S6.pdf"))
orderly_dependency("lassa_transmission", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_5.png","figure_5.pdf"))
orderly_dependency("lassa_summary", "latest(parameter:pathogen == this:pathogen)",
                   files = c("figure_S2.png","figure_S3.png","figure_S4.png",
                             "figure_S2.pdf","figure_S3.pdf","figure_S4.pdf"))
orderly_dependency("lassa_latex_tables", "latest(parameter:pathogen == this:pathogen)",
                   files = c("latex_outbreaks.csv","latex_models.csv",
                             "latex_transmission.csv","latex_delays.csv","latex_severity.csv",
                             "latex_seroprevalence.csv","latex_riskfactors.csv"))
orderly_artefact("lassa-specific outputs",
                 c("main/figure_2.png","main/figure_3.png","main/figure_4.png","main/figure_5.png",
                   "main/figure_2.pdf","main/figure_3.pdf","main/figure_4.pdf","main/figure_5.pdf",
                   "supp/figure_S2.png","supp/figure_S3.png","supp/figure_S4.png",
                   "supp/figure_S5.png","supp/figure_S6.png","supp/figure_S7.png","supp/figure_S8.png",
                   "supp/figure_S9.png","supp/figure_S10.png","supp/figure_S11.png","supp/figure_S12.png","supp/figure_S13.png",
                   "supp/figure_S14.png",
                   "supp/figure_S2.pdf","supp/figure_S3.pdf","supp/figure_S4.pdf",
                   "supp/figure_S5.pdf","supp/figure_S6.pdf","supp/figure_S7.pdf","supp/figure_S8.pdf",
                   "supp/figure_S9.pdf","supp/figure_S10.pdf","supp/figure_S11.pdf","supp/figure_S12.pdf","supp/figure_S13.pdf",
                   "supp/figure_S14.pdf",
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

file.copy(from = "figure_2.pdf",
          to = "main/figure_2.pdf")
file.copy(from = "figure_3.pdf",
          to = "main/figure_3.pdf")
file.copy(from = "figure_4.pdf",
          to = "main/figure_4.pdf")
file.copy(from = "figure_5.pdf",
          to = "main/figure_5.pdf")

###############################
## SUPPLEMENTARY INFORMATION ##
###############################

dir.create("supp")

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
file.copy(from = "figure_S12.png",
          to = "supp/figure_S12.png")
file.copy(from = "figure_S13.png",
          to = "supp/figure_S13.png")
file.copy(from = "figure_S14.png",
          to = "supp/figure_S14.png")

file.copy(from = "figure_S2.pdf",
          to = "supp/figure_S2.pdf")
file.copy(from = "figure_S3.pdf",
          to = "supp/figure_S3.pdf")
file.copy(from = "figure_S4.pdf",
          to = "supp/figure_S4.pdf")
file.copy(from = "figure_S5.pdf",
          to = "supp/figure_S5.pdf")
file.copy(from = "figure_S6.pdf",
          to = "supp/figure_S6.pdf")
file.copy(from = "figure_S7.pdf",
          to = "supp/figure_S7.pdf")
file.copy(from = "figure_S8.pdf",
          to = "supp/figure_S8.pdf")
file.copy(from = "figure_S9.pdf",
          to = "supp/figure_S9.pdf")
file.copy(from = "figure_S10.pdf",
          to = "supp/figure_S10.pdf")
file.copy(from = "figure_S11.pdf",
          to = "supp/figure_S11.pdf")
file.copy(from = "figure_S12.pdf",
          to = "supp/figure_S12.pdf")
file.copy(from = "figure_S13.pdf",
          to = "supp/figure_S13.pdf")
file.copy(from = "figure_S14.pdf",
          to = "supp/figure_S14.pdf")

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