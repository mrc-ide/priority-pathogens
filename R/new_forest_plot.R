library(tidyverse)
library(stringr)
library(metafor)
library(meta)
library(estmeansd)
library(mixdist)
library(ggplot2)
library(ggsci)
library(sf)
library(ragg)
library(ggspatial)
library(png)
library(patchwork)

articles   <- read.csv("../archive/db_compilation/20231211-175900-afa938db/articles.csv")
parameters <- read.csv("../archive/db_compilation/20231211-175900-afa938db/parameters.csv")
parameters <- parameters %>% #account for inverse and exponents
              mutate(refs = paste(articles$first_author_first_name[match(covidence_id, articles$covidence_id)],
                     " (", articles$year[match(covidence_id, articles$covidence_id)], ")",sep = ""),
                     parameter_value = ifelse(inverse_param, 1/parameter_value, parameter_value),
                     parameter_lower_bound = ifelse(inverse_param, 1/parameter_lower_bound, parameter_lower_bound),
                     parameter_upper_bound = ifelse(inverse_param, 1/parameter_upper_bound, parameter_upper_bound),
                     parameter_uncertainty_lower_value = ifelse(inverse_param, 1/parameter_uncertainty_lower_value, parameter_uncertainty_lower_value),
                     parameter_uncertainty_upper_value = ifelse(inverse_param, 1/parameter_uncertainty_upper_value, parameter_uncertainty_upper_value),
                     across(c(parameter_value, parameter_lower_bound, parameter_upper_bound, parameter_uncertainty_lower_value, parameter_uncertainty_upper_value), ~. * 10^exponent)) %>%              
              filter(!parameter_from_figure) %>%
              mutate(no_unc = is.na(parameter_uncertainty_lower_value) & is.na(parameter_uncertainty_upper_value),
                     parameter_uncertainty_lower_value = case_when(
                     parameter_uncertainty_singe_type == "Maximum" & no_unc ~ parameter_value,
                     parameter_uncertainty_singe_type == "Standard deviation (Sd)" & no_unc ~ parameter_value-parameter_uncertainty_single_value,
                     parameter_uncertainty_singe_type == "Standard Error (SE)" & no_unc ~ parameter_value-parameter_uncertainty_single_value,
                     distribution_type == "Gamma" & no_unc ~ qgamma(0.05, shape = (distribution_par1_value/distribution_par2_value)^2, rate = distribution_par1_value/distribution_par2_value^2),      
                     TRUE ~ parameter_uncertainty_lower_value),                                                 
                     parameter_uncertainty_upper_value = case_when(
                     parameter_uncertainty_singe_type == "Maximum" & no_unc ~ parameter_uncertainty_single_value,
                     parameter_uncertainty_singe_type == "Standard deviation (Sd)" & no_unc ~ parameter_value+parameter_uncertainty_single_value,
                     parameter_uncertainty_singe_type == "Standard Error (SE)" & no_unc ~ parameter_value+parameter_uncertainty_single_value,
                     distribution_type == "Gamma" & no_unc ~ qgamma(0.95, shape = (distribution_par1_value/distribution_par2_value)^2, rate = distribution_par1_value/distribution_par2_value^2),      
                     TRUE ~ parameter_uncertainty_upper_value)) %>%
                     select(-c(no_unc))
parameters$population_country <- str_replace_all(parameters$population_country, c("Congo; Rep." = "Republic of Congo", "Congo; Dem. Rep." = "Democratic Republic of Congo"))

plot_generic <- function(df, label, color_column, lims) {
  
  stopifnot(length(unique(df$parameter_unit)) == 1)
  df   <- df %>% mutate(urefs = make.unique(refs)) %>%
                 mutate(urefs = factor(urefs, levels = rev(unique(urefs))))
  cats <- length(unique(df[[color_column]]))
  
  gg <- ggplot(df) +
        geom_segment(aes(x = parameter_lower_bound, xend = parameter_upper_bound,
                         y = urefs, yend = urefs, color = .data[[color_column]]),
                     size = 3, alpha = 0.65) +
        geom_errorbar(aes(xmin=parameter_uncertainty_lower_value, xmax=parameter_uncertainty_upper_value,
                          y = urefs),
                      width = 0.15, lwd=0.5, color = "black", alpha = 1) +
        geom_point(aes(x = parameter_value, y = urefs, 
                       shape = df$parameter_value_type, fill = .data[[color_column]]),
                   size = 3, stroke = 1,
                   color = "black", alpha = 1)
  
  if (all(df$parameter_class=="Reproduction number")) {gg <- gg + geom_vline(xintercept = 1, linetype = "dashed", colour = "dark grey")}
  
  gg <- gg + scale_fill_lancet(palette = "lanonc") + scale_color_lancet(palette = "lanonc") +
        scale_shape_manual(name = "Parameter Type",values = c(Mean = 21, Median = 22, Unspecified = 24)) +
        scale_x_continuous(limits = lims, expand = c(0, 0)) +
        scale_y_discrete(labels = setNames(df$refs, df$urefs)) +
        labs(x = label, y = NULL) +
        theme_minimal() + 
        theme(panel.border = element_rect(color = "black", size = 2, fill = NA))
        if (cats == 1) {
          gg <- gg + guides(fill = "none", color = FALSE, shape = guide_legend(title = NULL,order = 1))
        } else {
          gg <- gg + guides(fill = "none", color = guide_legend(title = NULL,order = 1), shape = guide_legend(title = NULL,order = 2))}
  
  return(gg)
}

pdf_generic <- function(dists, xbar, sdhat, label, lims) {

  df <- data.frame()
  n  <- length(dists)
  for (i in 1:n) {
    x_values  <- seq(lims[1], lims[2], by = 0.01)
    if (dists[i]=="Exponential"){
      y_values <- dexp(x_values, rate = 1/xbar)
    } else if (dists[i]=="Weibull") {
      w_params <- weibullpar(xbar, sdhat)
      y_values <- dweibull(x_values, shape = w_params$shape, scale = w_params$scale)
    } else if (dists[i]=="Gamma") {
      y_values <- dgamma(x_values, shape = (xbar/sdhat)^2, rate = xbar/(sdhat^2))
    } else if (dists[i]=="Lognormal") {
      mean_log <- log(xbar^2 / sqrt(xbar^2 + sdhat^2))
      sd_log   <- sqrt(log(1 + sdhat^2 / xbar^2))
      y_values <- dlnorm(x_values, meanlog = mean_log, sdlog = sd_log)
    } else {
      y_values <- rep(0, times = length(x_values))
    }
    dist_data <- data.frame(x = x_values, y = y_values, dist = rep(dists[i], length(x_values)))
    df        <- rbind(df, dist_data)
  }
  
  df$dist <- factor(df$dist, levels = dists)
  
  gg <- ggplot(df, aes(x = x, y = y, color = dist)) + geom_line(size = 1) +
        scale_fill_lancet(palette = "lanonc") + scale_color_lancet(palette = "lanonc") +
        scale_x_continuous(limits = lims, expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 0.125), expand = c(0, 0)) +
        labs(x = label, y = "Probability") + 
        theme_minimal() + 
        theme(panel.border = element_rect(color = "black", size = 2, fill = NA)) +
        guides(fill = "none", color = guide_legend(title = NULL,order = 1))        

  return(gg)
}

map_generic <- function(df=df, country_list=country_list, exclude=NULL,
                        range_mp=range_mp, summ_dups=summ_dups, f=f,
                        title=title, color_opt=color_opt, lims=lims) {

  outlines <- read_sf('wca_admbnda_adm0_ocha/wca_admbnda_adm0_ocha.shp') %>%
              filter(admin0Name %in% country_list)
  
  df           <- df %>% separate_longer_delim(population_country, delim = ";") #dataframe with expanded list of countries
  regional_dat <- unique(df$population_country[!is.na(df$population_location)])#countries with regional data
  regional_dat <- intersect(country_list,regional_dat)#only countries in country_list are plotted
  country_dat  <- country_list[!(country_list %in% regional_dat)]#countries with national data only
  
  shapes <- bind_rows(read_sf('wca_admbnda_adm0_ocha/wca_admbnda_adm0_ocha.shp') %>%
            filter(admin0Name %in% country_dat) %>%
            mutate(geo_code=admin0Pcod),
            read_sf('wca_admbnda_adm1_ocha/wca_admbnda_adm1_ocha.shp') %>%
            filter(admin0Name %in% regional_dat) %>%
            mutate(geo_code=admin1Pcod))
  
  outlines <- outlines %>% filter(!(admin0Pcod %in% shapes$admin0Pcod[match(exclude,shapes$admin1Pcod)]))
  shapes   <- shapes %>% filter(!(admin1Pcod %in% exclude))
  
  no_loc <- shapes %>% group_by(admin0Name) %>% summarise(all_codes = str_c(admin1Pcod, collapse = ";"))
  
  stopifnot(length(unique(df$parameter_unit[!is.na(df$parameter_unit)])) == 1)#values must have same units
  df <- df %>% mutate(parameter_value = coalesce(parameter_value, 100*cfr_ifr_numerator/cfr_ifr_denominator)) %>%
               mutate(parameter_value = if(range_mp) {
                      coalesce(parameter_value, (parameter_lower_bound + parameter_upper_bound)/2)
                      } else {parameter_value}) %>%
               mutate(geo_code = 
                      ifelse(
                      population_country %in% country_dat,
                      shapes$admin0Pcod[match(population_country,shapes$admin0Name)],
                      ifelse(
                      is.na(population_location),  
                      no_loc$all_codes[match(population_country,no_loc$admin0Name)],
                      case_when(
                      population_country == "Benin" ~ str_replace_all(population_location, c("Central Region" = "BJ04;BJ05;BJ07;BJ12")),
                      population_country == "Côte d'Ivoire" ~ str_replace_all(population_location, c("Abidjan" = "CI01", "Northeastern Region" = "CI08;CI14")),
                      population_country == "Ghana" ~ str_replace_all(population_location, c("Ankaakur" = "GH02", "Ehiawenwu" = "GH06", "Amomaso" = "GH03", "Menkwo" = "GH04", "Mangoase" = "GH04", "Jirandogo" = "GH11", "Bowena" = "GH11", "Natorduori" = "GH13", "Teanoba" = "GH09", "Doniga; Naveransa" = "GH12", "Tamale" = "GH08", "Agogo" = "GH02", "Kumasi" = "GH02")),#map in article
                      population_country == "Guinea" ~ str_replace_all(population_location, c("Gueckedou" = "GN08","Macenta" = "GN08","Pita" = "GN07","Lola" = "GN08","Yomou" = "GN08")),
                      population_country == "Mali" ~ str_replace_all(population_location, c("Bougouni District" = "ML03")),
                      population_country == "Nigeria" ~ str_replace_all(population_location, c("Edo State" = "NG12", "Southeast" = "NG01;NG04;NG11;NG14;NG17","Benue River" = "NG07","Ibadan" = "NG31")),
                      population_country == "Sierra Leone" ~ str_replace_all(population_location, c("Kenema" = "SL01", "Port Loko" = "SL05", "Tonkolili" = "SL02","Niahun" = "SL01", "Konia" = "SL01", "Palima" = "SL01", "Semewabu" = "SL01", "Knonia" = "SL01", "Tongola" = "SL01", "Njakundoma" = "SL01", "Kpandebu" = "SL01", "Neama" = "SL01", "Lowoma" = "SL01", "Landoma" = "SL01", "Bomie" = "SL01", "Yengema" = "SL03", "Kamethe" = "SL05", "Kamabunyele" = "SL02", "Kathumpe" = "SL02")),
                      population_country == "Central African Republic" ~ str_replace_all(population_location, c("Nola And Ikaumba" = "CF23;CF12", "The Pre\nForest\nGrassland Of Bozo And Bangassou" = "CF11;CF62", "The Moist\nWooded Grassland Of Bouar And Obo" = "CF22;CF63", "The Dry Wooded\nRassland Near Mbre" = "CF51", "And The Dry Grassland-Of Birao." = "CF53")),#map in article
                      population_country == "Gabon" ~ str_replace_all(population_location, c("Haut-Ogooue" = "GA02")),
                      population_country == "Liberia" ~ str_replace_all(population_location, c("Zigida" = "LR08","Lofa County" = "LR08", "Montserrado County" = "LR11")),
                      TRUE ~ population_location)))) %>%
               mutate(geo_code = str_replace_all(geo_code, " ", "")) %>%
               mutate(geo_code = sapply(str_split(geo_code, ";"), function(x) paste(unique(x), collapse = ";"))) %>% #remove duplicate regions for each value
               separate_longer_delim(geo_code, delim = ";")#broadcast multi-region values
               
  if (summ_dups=="mean") {
    df <- df %>% group_by(geo_code) %>% 
          mutate(value = mean(parameter_value)) %>% distinct(geo_code,value)
  } else if (summ_dups=="most_recent") {
    df <- df %>% group_by(geo_code) %>% 
          mutate(value = ifelse(all(is.na(population_study_start_year)), 
          first(parameter_value),
          parameter_value[which.max(!is.na(population_study_start_year))])) %>% distinct(geo_code, value)
  } else if (summ_dups=="max_sample") {
    df <- df %>% group_by(geo_code) %>% 
          mutate(value = ifelse(all(is.na(population_sample_size)), 
          first(parameter_value),
          parameter_value[which.max(!is.na(population_sample_size))])) %>% distinct(geo_code, value)
  } else {stop("Error: choose summary option for duplicate geo_codes")}
  
  shapes <- shapes %>% left_join(df,by=c('geo_code')) 
  
  gg <- ggplot() +
        geom_sf(data = shapes, lwd = 0.3, col = "grey40", aes(fill = value)) +
        geom_sf(data = outlines, lwd = 0.7, col = "black",  fill = NA) +
        scale_fill_viridis_c(option = color_opt, direction = -1, na.value = "grey80", 
                             limits = lims) +
        geom_point(data = f %>% filter(type == "Outbreak"), aes(x = longitude, y = latitude, color = type), shape = 8, size = 2.5, stroke = 1.5) +
        geom_text(data = f %>% filter(type == "Treatment Centre"), aes(x = longitude, y = latitude, color = type), label = "T", size = 5, fontface='bold') +
        scale_color_manual(values = c(Outbreak = "green1", "Treatment Centre" = "blue")) +    
        #scale_shape_manual(values = c(circle = 21, square = 22, triangle = 24)) +
        #scale_color_manual(values = c(blue = "blue", green = "green")) +
        #geom_emoji(data=f, aes(longitude,latitude), emoji = "", size=0.03) +
        theme_void() +
        guides(fill = guide_colorbar(title = NULL), color = guide_legend(title = NULL, override.aes = list(shape = c(8,NA)))) + 
        theme(legend.key.size = unit(2, "lines"), legend.position = "right", legend.text = element_text(size = 12)) + 
        annotation_scale(location = "bl", width_hint = 0.2) +
        annotation_north_arrow(location = "bl", which_north = "true", pad_y = unit(0.4, "in"), style = north_arrow_fancy_orienteering) +
        ggtitle(title)
  
  return(gg)
}

##TRANSMISSION

d1 <- parameters %>% filter(parameter_type == 'Mutations - evolutionary rate')
d2 <- parameters %>% filter(parameter_type == 'Mutations – substitution rate')
d3 <- parameters %>% filter(parameter_class == 'Relative contribution')
d4 <- parameters %>% filter(parameter_class == 'Attack rate')
d5 <- parameters %>% filter(parameter_class == 'Growth rate')
d6 <- parameters %>% filter(parameter_class == 'Reproduction number')

d1 <- d1 %>% arrange(genome_site,as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", refs)))
d2 <- d2 %>% arrange(genome_site,as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", refs)))
d3 <- d3 %>% arrange(as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", refs)))
d4 <- d4 %>% mutate(arate=c("Primary","Secondary")) %>%
             arrange(arate,as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", refs)))
d5 <- d5 %>% arrange(as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", refs)))
d6 <- d6 %>% arrange(parameter_type,as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", refs)))

d6 <- d6 %>% mutate(parameter_type = factor(parameter_type,
                                            levels = unique(parameter_type),
                                            labels = c("Basic (R0)","Effective (Re)")))

d1 <- d1 %>% mutate(across(
             c(parameter_value,
             parameter_lower_bound, parameter_upper_bound,
             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value),
             ~. * 10^4)) #multiply by 10^4
d2 <- d2 %>% mutate(across(
             c(parameter_value,
             parameter_lower_bound, parameter_upper_bound,
             parameter_uncertainty_lower_value, parameter_uncertainty_upper_value),
             ~. * 10^4)) #multiply by 10^4

p1 <- plot_generic(d1,expression(Evolutionary~Rate~(s/s/y ~10^{-4})),"genome_site",c(5,30))
p2 <- plot_generic(d2,expression(Substitution~Rate~(s/s/y ~10^{-4})),"genome_site",c(5,30))
p3 <- plot_generic(d3,'Human-Human Transmission Contribution (%)',"pathogen",c(10,35))
p4 <- plot_generic(d4,'Attack Rate (%)',"arate",c(0,6))
p5 <- plot_generic(d5,'Growth Rate (per day)',"pathogen",c(0,1.25))
p6 <- plot_generic(d6,'Reproduction Number',"parameter_type",c(0.5,2))

patchwork <- (p6 | p5) / (p4 | p3) / (p1 | p2)
patchwork <- patchwork + plot_annotation(tag_levels = 'A')# + plot_layout(guides = 'collect')
ggsave("lassa plots/transmission.png", plot = patchwork, width = 12, height = 8)

##HUMAN DELAYS

d1 <- parameters %>% filter(parameter_type == 'Human delay - incubation period')
d2 <- parameters %>% filter(parameter_type == 'Human delay - symptom onset>admission to care')
d3 <- parameters %>% filter(parameter_type == 'Human delay - time in care (length of stay)' |
                            parameter_type == 'Human delay - admission to care>discharge/recovery' |
                            parameter_type == 'Human delay - admission to care>death')
d4 <- parameters %>% filter(parameter_type == 'Human delay - symptom onset>discharge/recovery' |
                            parameter_type == 'Human delay - symptom onset>death')

d1 <- d1 %>% arrange(as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", refs)))
d2 <- d2 %>% arrange(as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", refs)))
co <- c('Human delay - admission to care>discharge/recovery',
        'Human delay - admission to care>death',
        'Human delay - time in care (length of stay)')
d3 <- d3 %>% mutate(parameter_type = factor(parameter_type, levels = co)) %>%
             arrange(parameter_type,as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", refs)))
co <- c('Human delay - symptom onset>discharge/recovery',
        'Human delay - symptom onset>death')
d4 <- d4 %>% mutate(parameter_type = factor(parameter_type, levels = co)) %>%
             arrange(parameter_type,as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", refs)))

d3 <- d3 %>% mutate(parameter_type = factor(parameter_type,
                                            levels = unique(parameter_type),
                                            labels = c("Recovery","Death","Unspecified")))
d4 <- d4 %>% mutate(parameter_type = factor(parameter_type,
                                            levels = unique(parameter_type),
                                            labels = c("Recovery","Death")))

d4 <- d4 %>% mutate_at(vars(c("parameter_value","parameter_lower_bound","parameter_upper_bound","parameter_uncertainty_lower_value","parameter_uncertainty_upper_value")),
                       list(~ ifelse(parameter_unit == "Weeks", . * 7, .))) %>%
             mutate(parameter_unit = "Days")

p1 <- plot_generic(d1,'Incubation Period (days)',"parameter_type",c(0,40))
p2 <- plot_generic(d2,'Onset-Admission Delay (days)',"parameter_type",c(-0.5,40))
p3 <- plot_generic(d3,'Admission-Outcome Delay (days)',"parameter_type",c(-1,40))
p4 <- plot_generic(d4,'Onset-Outcome Delay (days)',"parameter_type",c(0,40))

#analysis

da <- d2 %>% filter(!is.na(parameter_value) &
                      !(is.na(parameter_uncertainty_lower_value) & is.na(parameter_uncertainty_upper_value)) &
                      parameter_value_type != 'Unspecified' &
                      !(parameter_value_type == 'Mean' & !is.na(parameter_uncertainty_type) & parameter_uncertainty_type == 'Range') &
                      !(parameter_value_type == 'Mean' & !is.na(parameter_uncertainty_type) & parameter_uncertainty_type == 'Inter Quartile Range (IQR)') &
                      !is.na(population_sample_size))#can't do meta-analysis on any of these

da <- da %>% mutate(parameter_uncertainty_single_value = case_when(
                    parameter_uncertainty_singe_type == "Standard Error (SE)" ~ parameter_uncertainty_single_value*sqrt(population_sample_size),
                    TRUE ~ parameter_uncertainty_single_value),
                    parameter_uncertainty_singe_type = case_when(
                    parameter_uncertainty_singe_type == "Standard Error (SE)" ~ "Standard deviation (Sd)",
                    TRUE ~ parameter_uncertainty_singe_type)) %>%
             mutate(parameter_uncertainty_lower_value = case_when(
                    parameter_uncertainty_singe_type == "Standard deviation (Sd)" ~ parameter_value-parameter_uncertainty_single_value,
                    TRUE ~ parameter_uncertainty_lower_value),
                    parameter_uncertainty_upper_value = case_when(
                    parameter_uncertainty_singe_type == "Standard deviation (Sd)" ~ parameter_value+parameter_uncertainty_single_value,
                    TRUE ~ parameter_uncertainty_upper_value))#convert SE to SD

# da_fixed <- da
#
# #approach 1: call estmeansd directly and then call metamean
#
# set.seed(NULL)
#
# cmn1 <- c()
# rnd1 <- c()
#
# for (j in seq(1000)){
#   da <- da_fixed
#   for (i in seq(nrow(da))) {
#     if (da$parameter_value_type[i] == "Median") {
#       if (da$parameter_uncertainty_type[i] == "Range") {
#         #set.seed(1)
#         result <- mln.mean.sd(min.val = da$parameter_uncertainty_lower_value[i],
#                               med.val = da$parameter_value[i],
#                               max.val = da$parameter_uncertainty_upper_value[i],
#                               n       = da$population_sample_size[i])
#
#         da$parameter_value_type[i]               <- "Mean"
#         da$parameter_value[i]                    <- result$est.mean
#         da$parameter_uncertainty_singe_type[i]   <- "Standard deviation (Sd)"
#         da$parameter_uncertainty_single_value[i] <- result$est.sd
#         da$parameter_uncertainty_lower_value[i]  <- result$est.mean - result$est.sd
#         da$parameter_uncertainty_upper_value[i]  <- result$est.mean + result$est.sd}
#       else if (da$parameter_uncertainty_type[i] == "Inter Quartile Range (IQR)") {
#         #set.seed(1)
#         result <- mln.mean.sd(q1.val  = da$parameter_uncertainty_lower_value[i],
#                               med.val = da$parameter_value[i],
#                               q3.val  = da$parameter_uncertainty_upper_value[i],
#                               n       = da$population_sample_size[i])
#
#         da$parameter_value_type[i]               <- "Mean"
#         da$parameter_value[i]                    <- result$est.mean
#         da$parameter_uncertainty_singe_type[i]   <- "Standard deviation (Sd)"
#         da$parameter_uncertainty_single_value[i] <- result$est.sd
#         da$parameter_uncertainty_lower_value[i]  <- result$est.mean - result$est.sd
#         da$parameter_uncertainty_upper_value[i]  <- result$est.mean + result$est.sd}
#     }
#   }#estmeansd only takes scalar inputs
#
#   test <- metamean(n = population_sample_size,
#                    mean = parameter_value,
#                    sd = parameter_uncertainty_single_value,
#                    studlab = refs,
#                    data = da,
#                    sm = "MRAW",
#                    method.tau = "ML")
#
#   cmn1[j] = test$TE.common
#   rnd1[j] = test$TE.random
# }
# plot(cmn1, type = "l", lty = 1, ylim = c(min(cmn1), max(cmn1)))
# plot(rnd1, type = "l", lty = 1, ylim = c(min(rnd1), max(rnd1)))
# #forest.meta(test, layout="RevMan5")
#
# #approach 2: call metamean which uses estmeansd internally
#
# da <- da_fixed

da <- da %>% mutate(xbar   = ifelse(parameter_value_type == "Mean", parameter_value, NA),
                    median = ifelse(parameter_value_type == "Median", parameter_value, NA),
                    q1     = ifelse(parameter_uncertainty_type == "Inter Quartile Range (IQR)", parameter_uncertainty_lower_value, NA),
                    q3     = ifelse(parameter_uncertainty_type == "Inter Quartile Range (IQR)", parameter_uncertainty_upper_value, NA),
                    min    = ifelse(parameter_uncertainty_type == "Range", parameter_uncertainty_lower_value, NA),
                    max    = ifelse(parameter_uncertainty_type == "Range", parameter_uncertainty_upper_value, NA))

set.seed(42)
dama <- metamean(n = population_sample_size,
                 mean = xbar,
                 sd = parameter_uncertainty_single_value,
                 studlab = refs,
                 data = da,
                 median = median,
                 q1 = q1,
                 q3 = q3,
                 min = min,
                 max = max,
                 method.mean = "Cai",
                 method.sd = "Cai",
                 sm = "MRAW",
                 method.tau = "ML")

png(file = "lassa plots/p5.png", width = 9500, height = 4000, res = 1000)
forest.meta(dama, digits=2, digits.sd=2, digits.weight=2, layout="RevMan5",
            weight.study = "same", col.square.lines = "black", col.square = "dodgerblue3",
            col.study = "black", col.inside = "black",
            col.diamond.lines = "black", col.diamond.common = "dodgerblue3", col.diamond.random = "dodgerblue3",
            at = seq(0,14,by=2), xlim = c(0,14), xlab="Onset-Admission Delay (days)", fontsize=10)
dev.off()
p5 <- png::readPNG("lassa plots/p5.png", native = TRUE)
#p5 <- wrap_elements(plot = rasterGrob(p5, interpolate = TRUE))

# mean_c <- dama$TE.common
# mean_r <- dama$TE.random
# sdp    <- sqrt(sum((dama$n-1)*(dama$sd^2))/(sum(dama$n)-dama$k))
#
# p6 <- pdf_generic(c("Exponential","Weibull","Gamma","Lognormal"),
#                   mean_r,sdp,'Onset-Admission Delay (days)',c(-0.1,20))

patchwork <- (p1 | p2) / (p3 | p4) / (p5)
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("lassa plots/human_delays.png", plot = patchwork, width = 12, height = 12)

##SEVERITY

d1 <- parameters %>% filter(parameter_type == 'Severity - case fatality rate (CFR)')

d1 <- d1 %>% arrange(as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", refs)))

#p1 <- plot_generic(d1,'Case Fatality Rate (%)',"pathogen",c(0,100))

#analysis

da <- d1 %>% filter(!is.na(cfr_ifr_denominator)) %>%
             mutate(cfr_ifr_numerator = case_when(
                    is.na(cfr_ifr_numerator) & !is.na(parameter_value) ~ round((parameter_value/100)*cfr_ifr_denominator),
                    TRUE ~ cfr_ifr_numerator))

da <- da %>% mutate(study_midyear = ifelse(!is.na(population_study_start_year) & !is.na(population_study_end_year),
                                            round((population_study_start_year + population_study_end_year) / 2),
                                            population_study_start_year)) %>%
             mutate(study_midyear_cat = case_when(
                    study_midyear %in% 1970:1979 ~ "1970-1979",
                    study_midyear %in% 1980:1989 ~ "1980-1989",
                    study_midyear %in% 1990:1999 ~ "1990-1999",
                    study_midyear %in% 2000:2009 ~ "2000-2009",
                    study_midyear %in% 2010:2019 ~ "2010-2019",
                    study_midyear %in% 2020:2029 ~ "2020-Present",
                    TRUE ~ "Unspecified")) %>%
             mutate(cfr_denom_cat = case_when(
                    cfr_ifr_denominator %in% 1:29      ~ "N < 30",
                    cfr_ifr_denominator %in% 30:99     ~ "N = 30-99",
                    cfr_ifr_denominator %in% 100:299   ~ "N = 100-299",
                    cfr_ifr_denominator %in% 300:999   ~ "N = 300-999",
                    cfr_ifr_denominator %in% 1000:9000 ~ "N > 1000",
                    TRUE ~ "Unspecified")) %>%
             mutate(population_group = case_when(
                    population_group == "Persons under investigation" ~ "Persons Under Investigation",
                    population_group == "Pregnant women" ~ "Pregnant Women",
                    population_group == "Children" ~ population_group,
                    TRUE ~ "Mixed Groups"))

#da <- da %>% mutate_at(vars(c("population_country","study_midyear")), ~coalesce(., 'Unspecified'))

dama <- metaprop(event = cfr_ifr_numerator,
                 n = cfr_ifr_denominator,
                 studlab = refs,
                 data = da,
                 subgroup = population_country,
                 sm = "PLOGIT",
                 method="GLMM",
                 method.tau = "ML")

png(file = "lassa plots/p1.png", width = 4000, height = 4000, res = 500)
forest.meta(dama, digits=3, layout="RevMan5",
            print.subgroup.name = FALSE,sort.subgroup = TRUE,
            study.results = FALSE,pooled.events = TRUE,
            overall=TRUE,
            col.subgroup = "black",
            weight.study = "same",#col.square.lines = "green", col.square = "blue", #not working
            col.inside = "black",
            col.diamond.lines = "black",col.diamond.common = "red", col.diamond.random = "red",
            at = seq(0,1,by=0.2), xlim = c(0,1), xlab="Case Fatality Rate (proportion)", fontsize=10)
dev.off()
p1 <- png::readPNG("lassa plots/p1.png", native = TRUE)
p1 <- wrap_elements(plot = rasterGrob(p1, interpolate = TRUE))

dama <- metaprop(event = cfr_ifr_numerator,
                 n = cfr_ifr_denominator,
                 studlab = refs,
                 data = da %>% arrange(study_midyear),
                 subgroup = study_midyear_cat,
                 sm = "PLOGIT",
                 method="GLMM",
                 method.tau = "ML")

png(file = "lassa plots/p2.png", width = 4000, height = 4000, res = 500)
forest.meta(dama, digits=3, layout="RevMan5",
            print.subgroup.name = FALSE,sort.subgroup = FALSE,
            study.results = FALSE,pooled.events = TRUE,
            overall=TRUE,
            col.subgroup = "black",
            weight.study = "same",#col.square.lines = "green", col.square = "blue", #not working
            col.inside = "black",
            col.diamond.lines = "black",col.diamond.common = "red", col.diamond.random = "red",
            at = seq(0,1,by=0.2), xlim = c(0,1), xlab="Case Fatality Rate (proportion)", fontsize=10)
dev.off()
p2 <- png::readPNG("lassa plots/p2.png", native = TRUE)
p2 <- wrap_elements(plot = rasterGrob(p2, interpolate = TRUE))

dama <- metaprop(event = cfr_ifr_numerator,
                 n = cfr_ifr_denominator,
                 studlab = refs,
                 data = da %>% arrange(cfr_ifr_denominator),
                 subgroup = cfr_denom_cat,
                 sm = "PLOGIT",
                 method="GLMM",
                 method.tau = "ML")

png(file = "lassa plots/p3.png", width = 4000, height = 4000, res = 500)
forest.meta(dama, digits=3, layout="RevMan5",
            print.subgroup.name = FALSE,sort.subgroup = FALSE,
            study.results = FALSE,pooled.events = TRUE,
            overall=TRUE,
            col.subgroup = "black",
            weight.study = "same",#col.square.lines = "green", col.square = "blue", #not working
            col.inside = "black",
            col.diamond.lines = "black",col.diamond.common = "red", col.diamond.random = "red",
            at = seq(0,1,by=0.2), xlim = c(0,1), xlab="Case Fatality Rate (proportion)", fontsize=10)
dev.off()
p3 <- png::readPNG("lassa plots/p3.png", native = TRUE)
p3 <- wrap_elements(plot = rasterGrob(p3, interpolate = TRUE))

dama <- metaprop(event = cfr_ifr_numerator,
                 n = cfr_ifr_denominator,
                 studlab = refs,
                 data = da,
                 subgroup = population_group,
                 sm = "PLOGIT",
                 method="GLMM",
                 method.tau = "ML")

png(file = "lassa plots/p4.png", width = 4000, height = 4000, res = 500)
forest.meta(dama, digits=3, layout="RevMan5",
            print.subgroup.name = FALSE,sort.subgroup = TRUE,
            study.results = FALSE,pooled.events = TRUE,
            overall=TRUE,
            col.subgroup = "black",
            weight.study = "same",#col.square.lines = "green", col.square = "blue", #not working
            col.inside = "black",
            col.diamond.lines = "black",col.diamond.common = "red", col.diamond.random = "red",
            at = seq(0,1,by=0.2), xlim = c(0,1), xlab="Case Fatality Rate (proportion)", fontsize=10)
dev.off()
p4 <- png::readPNG("lassa plots/p4.png", native = TRUE)
p4 <- wrap_elements(plot = rasterGrob(p4, interpolate = TRUE))

# dama <- metaprop(event = cfr_ifr_numerator,
#                  n = cfr_ifr_denominator,
#                  studlab = refs,
#                  data = da,
#                  subgroup = cfr_ifr_method,
#                  sm = "PLOGIT",
#                  method="GLMM",
#                  method.tau = "ML")
#
# png(file = "lassa plots/p5.png", width = 4000, height = 4000, res = 500)
# forest.meta(dama, digits=3, layout="RevMan5",
#             print.subgroup.name = FALSE,sort.subgroup = TRUE,
#             study.results = FALSE,pooled.events = TRUE,
#             overall=TRUE,
#             col.subgroup = "black",
#             weight.study = "same",#col.square.lines = "green", col.square = "blue", #not working
#             col.inside = "black",
#             col.diamond.lines = "black",col.diamond.common = "red", col.diamond.random = "red",
#             at = seq(0,1,by=0.2), xlim = c(0,1), xlab="Case Fatality Rate (proportion)", fontsize=10)
# dev.off()
# p5 <- png::readPNG("lassa plots/p5.png", native = TRUE)

patchwork <- (p1 | p2) / (p3 | p4)
patchwork <- patchwork + plot_annotation(tag_levels = 'A')
ggsave("lassa plots/severity.png", plot = patchwork, width = 12, height = 12)

#SEROPREVALENCE

d1 <- parameters %>% filter(parameter_type == 'Seroprevalence - IgG' &
                           (population_sample_type == 'Population based' |
                            population_sample_type == 'Community based') &
                            population_group == 'General population')
d2 <- parameters %>% filter(parameter_class == 'Seroprevalence' & 
                            parameter_type != 'Seroprevalence - IgG' &
                            parameter_type != 'Seroprevalence - IgM' &
                           (population_sample_type == 'Population based' |
                            population_sample_type == 'Community based') &
                            population_group == 'General population')

#d1 <- d1 %>% arrange(population_country,population_location)
#d2 <- d2 %>% arrange(population_country,population_location)

#p1 <- plot_generic(d1,'Seroprevalence - IgG (%)',"pathogen",c(0,100))
#p2 <- plot_generic(d2,'Seroprevalence - Unspecified (%)',"pathogen",c(0,100))

c1 <- c("Guinea Bissau","Guinea","Sierra Leone","Liberia","Côte d'Ivoire",
        "Burkina Faso","Ghana","Togo","Benin","Nigeria","Mali","Gambia","Senegal")
e1 <- c('ML06','ML07','ML08','ML10')
f1 <- data.frame(map_point = c("Irrua Specialist Teaching Hospital","Federal Medical Centre Owo",
                               "Lagos University Teaching Hospital","University of Maiduguri Teaching Hospital",
                               "Federal Teaching Hospital Abakaliki",#Nigeria has 22 TCs
                               "Kenema Government Hospital",#Sierra Leone has one TC
                               "Monrovia Treatment Facility(?)",#Liberia has one TC (unnamed)
                               "Tchaourou Benin 2015-2016","Tanguieta Benin 2014",
                               "Suakoko Liberia 2016",
                               "Edo-Ondo Nigeria 2015-2016, 2018, 2019","Ebonyi Nigeria 2012, 2018, 2019","Bauchi-Plateau-Taraba Nigeria 2015-2016, 2018",
                               "Tonkolili Sierra Leone 2019","Kenema Sierra Leone 1997, 2010-2012","Kailahun Sierra Leone 1997"),
                 latitude  = c(6.732479,7.216375,6.517484,11.825366,6.323199,7.873899,6.308282,
                               8.888718,10.628637,6.985884,6.703927,6.054975,9.484048,8.748332,7.873899,8.277113),
                 longitude = c(6.188815,5.598108,3.354400,13.180658,8.092004,-11.184657,-10.807939,
                               2.584898,1.260810,-9.579024,5.572952,7.874588,10.530532,-11.836672,-11.184657,-10.566351),
                 type      = c("Treatment Centre","Treatment Centre","Treatment Centre","Treatment Centre",
                               "Treatment Centre","Treatment Centre","Treatment Centre",
                               "Outbreak","Outbreak","Outbreak","Outbreak","Outbreak","Outbreak","Outbreak","Outbreak","Outbreak"))
                 #shape_type = c("square", "triangle", "circle"),
                 #point_color = c("blue", "blue", "green"))
p1 <- map_generic(d1,c1,e1,range_mp=FALSE,summ_dups="mean",f1,
                  'IgG Seroprevalence (%) in General Population, West Africa 1993-2018','magma',c(0,60))
c2 <- c("Guinea Bissau","Guinea","Sierra Leone","Liberia","Côte d'Ivoire",
        "Burkina Faso","Ghana","Togo","Benin","Nigeria","Mali",
        "Cameroon","Central African Republic","Chad","Republic of Congo","Equatorial Guinea","Gabon","Gambia","Senegal","Niger")
f2 <- data.frame(map_point = c("Irrua Specialist Teaching Hospital","Federal Medical Centre Owo",
                               "Lagos University Teaching Hospital","University of Maiduguri Teaching Hospital",
                               "Federal Teaching Hospital Abakaliki",#Nigeria has 22 TCs
                               "Kenema Government Hospital",#Sierra Leone has one TC
                               "Monrovia Treatment Facility(?)",#Liberia has one TC (unnamed)
                               "Zorzor Liberia 1972, 1980-1982",
                               "Onitsha Nigeria 1974","Jos Nigeria 1969-1970",
                               "Panguma-Tongo Sierra Leone 1972"),
                 latitude  = c(6.732479,7.216375,6.517484,11.825366,6.323199,7.873899,6.308282,
                               7.775914,6.134231,9.889000,8.658131),
                 longitude = c(6.188815,5.598108,3.354400,13.180658,8.092004,-11.184657,-10.807939,
                               -9.432665,6.803424,8.860781,-11.063004),
                 type      = c("Treatment Centre","Treatment Centre","Treatment Centre","Treatment Centre",
                               "Treatment Centre","Treatment Centre","Treatment Centre",
                               "Outbreak","Outbreak","Outbreak","Outbreak"))
p2 <- map_generic(d2,c2,NULL,range_mp=TRUE,summ_dups="mean",f2,
                  'Unspecified Seroprevalence (%) in General Population, West Africa 1965-1992','magma',c(0,60))

patchwork <- p1 / p2
patchwork <- patchwork + plot_layout(heights = c(0.75, 1)) + plot_annotation(tag_levels = 'A')
ggsave("lassa plots/seroprevalence.png", plot = patchwork, width = 12, height = 15)