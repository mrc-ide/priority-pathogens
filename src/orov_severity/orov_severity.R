## OROV severity

library(tidyverse)
library(forcats)
library(orderly2)
library(ggsci)
library(tidytext)

orderly_strict_mode()
orderly::orderly_parameters(pathogen = "OROV")

orderly2::orderly_artefact(description = "inputs folder",
                           files = "inputs/")

orderly_dependency(
  name = "db_compilation_orov",
  query = "latest()",
  files = c("inputs/articles.csv"="articles.csv",
            "inputs/parameters.csv"="parameters.csv",
            "inputs/outbreaks.csv"="outbreaks.csv"))

# forest plot code
orderly_shared_resource("orov_functions.R" = "orov_functions.R")
source("orov_functions.R")

# read in data
articles <- read.csv("inputs/articles.csv")
outbreaks <- read.csv("inputs/outbreaks.csv")
parameters <- read.csv("inputs/parameters.csv")

dfs <- curation(articles, outbreaks, tibble(), parameters, plotting = TRUE)

parameters <- dfs$parameters
parameters <- parameters |> mutate(qa_score = article_qa_score / 100)
parameters$parameter_class <- parameters$parameter_type_broad

parameters <- parameters |>
  mutate(
    population_group = factor(
      population_group,
      levels = c(
        sort(setdiff(unique(population_group), c("Other", "Unspecified"))),
        "Other",
        "Unspecified"
      )
    )
  )


## plot
s1 <- parameters |> filter(parameter_class=="Severity")

s1 <- s1 |> mutate(
  parameter_label = case_when(
    parameter_type=="Severity - proportion of cases that relapse" ~ "Percentage of cases with symptom recurrence",
    parameter_type=="Severity - proportion of symptomatic cases" ~ "Percentage of symptomatic cases",
    parameter_type=="Severity - case fatality rate (CFR)" ~ "Case Fatality Ratio (CFR)",
    .default = parameter_type
  ),
  parameter_label = factor(parameter_label,
                           levels=c("Case Fatality Ratio (CFR)",
                                    "Percentage of symptomatic cases",
                                    "Percentage of cases with symptom recurrence")),
  location_label = case_when(
    population_country %in% c("Brazil","Cuba","Peru") ~ "Local transmission",
    population_country %in% c("France","United States of America") ~ "Importation"
  ),
  location_label = factor(location_label,
                          levels=c("Local transmission","Importation")),
  parameter_unit = "Percentage (%)",
  article_label = case_when(
    article_label=="Martos-BenÃ­tez 2025" ~ "Martos-Benãtez 2025",
    .default = article_label
  )
)

## wrapper for binom function
binom_ci_lower <- function(x,n){
  if(!is.na(x)){
    b <- binom::binom.exact(x=x,n=n)
  return(b$lower)
  } else{
    return(NA)
  }
}

binom_ci_upper <- function(x,n){
  if(!is.na(x)){
    b <- binom::binom.exact(x=x,n=n)
    return(b$upper)
  } else{
    return(NA)
  }
}

s1 <- s1 |> mutate(id_for_calc = seq(1,nrow(s1),1)) |>
  group_by(id_for_calc) |>
  mutate(
    lower = binom_ci_lower(cfr_ifr_numerator,cfr_ifr_denominator)*100,
    upper = binom_ci_upper(cfr_ifr_numerator,cfr_ifr_denominator)*100
) |> ungroup()



qa_threshold <- -1
qa_alpha <- 1
point_size <- 4.5
text_size <- 20

custom_colour_pop_groups <- get_colour_pop_groups(
  parameters,
  s1
)


s1 <- s1 |>
  mutate(article_label = reorder_within(article_label, -central, parameter_label)) |>
  filter(parameter_type != "Severity - case fatality rate (CFR)")


ggplot(s1,
       aes(y=article_label,col = location_label))+
  geom_point(aes(x=central),
             size = point_size)+
  geom_errorbar(aes(xmin=lower,xmax=upper,linetype="Inferred 95% CI"))+
  facet_wrap(~parameter_label,ncol=1,scale="free_y",
             labeller = label_wrap_gen(width = 40))+
  scale_x_continuous(limits=c(0,100),expand = c(0, 0))+
  labs(x="Percentage (%)",y="",col="",linetype="")+
  scale_y_reordered() +  
  theme_minimal() +
  theme(
    panel.border = element_rect(
      color = "black",
      linewidth = 1.25,
      fill = NA
    ),
    text = element_text(size = text_size))+
  scale_color_lancet(palette = "lanonc")+ guides(
    colour = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  )
ggsave("orov_severity.png",width=10,height=6)





