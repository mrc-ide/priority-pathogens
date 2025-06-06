# Data curation functions
# These used to be housed within the lassa_functions.R script but have been moved here 

# curated data should be used for plotting but not for the final epireview dataset (i.e. we have changed to inverse parameters, exponentiated, etc)

data_curation <- function(articles, outbreaks, models, parameters, plotting, switch_first_surname=FALSE) {
  
     # this is due to legacy access database issue
  if(switch_first_surname){
    print('switched')
    articles <- articles %>% rename(first_author_first_name=first_author_surname,first_author_surname=first_author_first_name)
  }
  
  articles   <- articles %>%
    mutate(refs = paste(first_author_surname," (",year_publication,")",sep="")) %>% #define references
    group_by(refs) %>% mutate(counter = row_number()) %>% ungroup() %>% #distinguish same-author-same-year references
    mutate(new_refs = ifelse(refs %in% refs[duplicated(refs)], paste0(sub("\\)$", "", refs),letters[counter],")"), refs)) %>%
    select(-counter,-refs) %>% rename(refs = new_refs)
  
  if(dim(outbreaks)[1]>0)  {
    outbreaks  <- outbreaks %>% 
      mutate(refs = articles$refs[match(covidence_id, articles$covidence_id)])
  }
  
  models     <- models %>% 
    mutate(refs = articles$refs[match(covidence_id, articles$covidence_id)])
  
  parameters <- parameters %>% 
    mutate(refs = articles$refs[match(covidence_id, articles$covidence_id)]) %>%
    filter(!(as.logical(parameter_from_figure) & is.na(parameter_value) & is.na(parameter_lower_bound))) # ensure that parameter_from_figure is logical not character
  
  if (pathogen == 'ZIKA'){
    var_select <- c("parameter_value", "parameter_lower_bound", "parameter_upper_bound", 
                    "parameter_uncertainty_lower_value", "parameter_uncertainty_upper_value",
                    "parameter_2_value", "parameter_2_lower_bound", "parameter_2_upper_bound", 
                    "parameter_2_uncertainty_lower_value", "parameter_2_uncertainty_upper_value")
  } else {
    var_select <- c("parameter_value", "parameter_lower_bound", "parameter_upper_bound", 
                    "parameter_uncertainty_lower_value", "parameter_uncertainty_upper_value")
  }
  
  param4plot <- parameters %>%
    mutate_at(vars(all_of(var_select)),
              list(~ ifelse(inverse_param, round(1/.x, 2), .x))) %>%
    # account for different units for inverse parameters 
    mutate(parameter_unit = ifelse(parameter_unit == 'Per week','Weeks', 
                                      ifelse(parameter_unit == 'Per day','Days', parameter_unit))) %>%
    mutate(parameter_type = str_replace_all(parameter_type, ' (inverse parameter)', ''))%>% # we changed all inverse params to not be inverse so shouldn't be labled that way 
    mutate_at(vars(all_of(var_select)),
              list(~ .x * 10^exponent)) %>% 
    mutate_at(vars(all_of(var_select)), #account for different units
              list(~ ifelse(parameter_unit %in% "Weeks", . * 7, .))) %>% 
    mutate(parameter_unit = ifelse(parameter_unit %in% "Weeks", "Days", parameter_unit)) %>%
    mutate(no_unc = is.na(parameter_uncertainty_lower_value) & is.na(parameter_uncertainty_upper_value), #store uncertainty in pu_lower and pu_upper
           parameter_uncertainty_lower_value = case_when(
             str_detect(str_to_lower(parameter_uncertainty_singe_type),"maximum") & no_unc ~ parameter_value,
             str_detect(str_to_lower(parameter_uncertainty_singe_type),"standard deviation") & no_unc ~ parameter_value-parameter_uncertainty_single_value,
             str_detect(str_to_lower(parameter_uncertainty_singe_type),"variance") & no_unc ~ parameter_value-sqrt(parameter_uncertainty_single_value),
             str_detect(str_to_lower(parameter_uncertainty_singe_type),"standard error") & no_unc ~ parameter_value-parameter_uncertainty_single_value,
             str_detect(str_to_lower(distribution_type),"gamma") & no_unc ~ qgamma(0.05, shape = (distribution_par1_value/distribution_par2_value)^2, rate = distribution_par1_value/distribution_par2_value^2), 
             TRUE ~ parameter_uncertainty_lower_value),                                                 
           parameter_uncertainty_upper_value = case_when(
             str_detect(str_to_lower(parameter_uncertainty_singe_type),"maximum") & no_unc ~ parameter_uncertainty_single_value,
             str_detect(str_to_lower(parameter_uncertainty_singe_type),"standard deviation") & no_unc ~ parameter_value+parameter_uncertainty_single_value,
             str_detect(str_to_lower(parameter_uncertainty_singe_type),"variance") & no_unc ~ parameter_value+sqrt(parameter_uncertainty_single_value),
             str_detect(str_to_lower(parameter_uncertainty_singe_type),"standard error") & no_unc ~ parameter_value+parameter_uncertainty_single_value,
             str_detect(str_to_lower(distribution_type),"gamma") & no_unc ~ qgamma(0.95, shape = (distribution_par1_value/distribution_par2_value)^2, rate = distribution_par1_value/distribution_par2_value^2), 
             TRUE ~ parameter_uncertainty_upper_value)) %>%
    select(-c(no_unc)) %>%
    mutate(central = coalesce(parameter_value, round(100*cfr_ifr_numerator/cfr_ifr_denominator, 3)), #central value for plotting, rmeoved this: , round(0.5*(parameter_lower_bound+parameter_upper_bound),3))
           central = ifelse(grepl('Seroprevalence', parameter_type) | grepl('Miscarriage',parameter_type) | grepl('microcephaly',parameter_type), 
                            coalesce(central, round(0.5*(parameter_lower_bound+parameter_upper_bound),3)), 
                            central))
  
  if (pathogen == 'ZIKA'){
    # Zika database has extra variables for the variability -- all of these have _2
    
    param4plot <- param4plot %>%
      mutate(no_unc = is.na(parameter_2_uncertainty_lower_value) & is.na(parameter_2_uncertainty_upper_value), #store uncertainty in pu_lower and pu_upper
             parameter_2_uncertainty_lower_value = case_when(
               str_detect(str_to_lower(parameter_2_uncertainty_single_type),"maximum") & no_unc ~ parameter_2_value,
               str_detect(str_to_lower(parameter_2_uncertainty_single_type),"standard deviation") & no_unc ~ parameter_2_value - parameter_2_uncertainty_single_value,
               str_detect(str_to_lower(parameter_2_uncertainty_single_type),"variance") & no_unc ~ parameter_2_value - sqrt(parameter_2_uncertainty_single_value),
               str_detect(str_to_lower(parameter_2_uncertainty_single_type),"standard error") & no_unc ~ parameter_2_value - parameter_2_uncertainty_single_value,
               str_detect(str_to_lower(distribution_2_type),"gamma") & no_unc ~ qgamma(0.05, shape = (distribution_2_par1_value / distribution_2_par2_value)^2, rate = distribution_2_par1_value / distribution_2_par2_value^2),
               TRUE ~ parameter_2_uncertainty_lower_value),
             parameter_2_uncertainty_upper_value = case_when(
               str_detect(str_to_lower(parameter_2_uncertainty_single_type),"maximum") & no_unc ~ parameter_2_uncertainty_single_value,
               str_detect(str_to_lower(parameter_2_uncertainty_single_type),"standard deviation") & no_unc ~ parameter_2_value+parameter_2_uncertainty_single_value,
               str_detect(str_to_lower(parameter_2_uncertainty_single_type),"variance") & no_unc ~ parameter_2_value+sqrt(parameter_2_uncertainty_single_value),
               str_detect(str_to_lower(parameter_2_uncertainty_single_type),"standard error") & no_unc ~ parameter_2_value+parameter_2_uncertainty_single_value,
               str_detect(str_to_lower(distribution_2_type),"gamma") & no_unc ~ qgamma(0.95, shape = (distribution_2_par1_value/distribution_2_par2_value)^2, rate = distribution_2_par1_value/distribution_2_par2_value^2),
               TRUE ~ parameter_2_uncertainty_upper_value))
  }
  
  if (plotting) {
    parameters <- param4plot    
  } else {
    check_param_id <- (parameters$parameter_data_id == param4plot$parameter_data_id )    # check that parameter data ids didn't get scrambled 
    if(sum(check_param_id, na.rm = TRUE)==dim(parameters)[1]) {
      parameters$central <- param4plot$central 
    } else {
      errorCondition('parameters not in right order to match')
    }  
  }
  
  if(dim(outbreaks)[1]>0)  {
    outbreaks  <- outbreaks  %>% mutate(outbreak_location  = str_replace_all(outbreak_location, "\xe9" , "é"))
  }
  
  parameters <- parameters %>% mutate(parameter_type     = str_replace_all(parameter_type, "\x96" , "–"),
                                      population_country = str_replace_all(population_country, c("昼㸴" = "ô", "�" = "ô")))
  
  return(list(articles = articles, outbreaks = outbreaks, 
              models = models, parameters = parameters))
}


curation <- function(articles, outbreaks, models, parameters, plotting) {
  #call data_curation function (which at some stage will move to epireview) but keep curation to be backward compatible
  df <- data_curation(articles,outbreaks,models,parameters,plotting)
  
  return(list(articles = df$articles, outbreaks = df$outbreaks, 
              models = df$models, parameters = df$parameters))
}
