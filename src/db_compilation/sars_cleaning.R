sars_cleaning <- function(df) {
          df <- df %>% 
        mutate(journal = str_to_title(journal)) %>%
        mutate(journal = sub("^The\\s+", "", journal)) %>%
        mutate(journal = ifelse(journal == 'Bmj','British Medical Journal',
                                ifelse(journal == 'Transactions Of The Royal Society Tropical Medicine And Hygiene','Transactions Of The Royal Society Of Tropical Medicine And Hygiene',        
                                       journal))) %>%
        #journal typos
        # mutate(journal = case_when(
        #   covidence_id == 870 ~ 'Transactions Of The Royal Society Of Tropical Medicine And Hygiene',
        #   TRUE ~ journal)) %>%
        #year typos
        mutate(year_publication = case_when(
          covidence_id == 613 ~ 2004,
          covidence_id == 3394 ~ 2018,
          covidence_id == 4233 ~ 2009,
          covidence_id == 4261 ~ 2006,
          covidence_id == 5861 ~ 2003,
          covidence_id == 5880 ~ 2008,
          covidence_id == 6144 ~ 2006,
          TRUE ~ year_publication)) %>%  #volume typos
        mutate(volume = case_when(
          covidence_id == 2043 ~ 5,
          covidence_id == 6239 ~ 68,
          covidence_id == 7256 ~ 15,
          TRUE ~ volume)) %>%
        #issue typos
        mutate(issue = case_when(
          covidence_id %in% c(558, 4217,	4676,	7256,	5650,	5484) ~ 1,
          covidence_id %in% c(3421,	7260,	8759,	10922,	4261) ~ 2,
          covidence_id %in% c(1036,	1564,	3449,	3468,	4169,	5635,	11040) ~ 3, 
          covidence_id %in% c(1046,	5880,	6145) ~ 4,
          covidence_id %in% c(1102,	2043,	3343,	4675) ~ 5,
          covidence_id %in% c(2006,	11042) ~ 6,
          covidence_id %in% c(523,	4614, 5585) ~ 9,
          covidence_id %in% c(5589,	5595) ~ 10,
          covidence_id %in% c(4184) ~ 12, 
          covidence_id %in% c(5879) ~ 14, 
          covidence_id %in% c(5069) ~ 1554, 
          covidence_id %in% c(3375) ~ 5627, 
          covidence_id %in% c(5393) ~ 7433, 								
          TRUE ~ issue)) %>%
        #page typos
        mutate(page_first = case_when(
          covidence_id %in% c(793) ~ 32,
          covidence_id %in% c(1641) ~ 195,
          covidence_id %in% c(4614) ~ 925,
          # covidence_id %in% c(1413, 2567, 2610, 2661, 3215, 3258, 3635) ~ NA,  
          TRUE ~ page_first)) %>%
        mutate(page_last = case_when(
          covidence_id == 1641 ~ 194,
          covidence_id == 4614 ~ 926,
          TRUE ~ page_last)) %>%
        #title typos
        mutate(article_title = case_when(
          covidence_id == 392 ~ 'Epidemiologic clues to SARS origin in China',
          covidence_id == 513 ~ 'Prevalence of subclinical infection by the SARS coronavirus among general practitioners in Hong Kong',
          covidence_id == 613 ~ 'Characterization of severe acute respiratory syndrome coronavirus genomes in Taiwan: molecular epidemiology and genome evolution',
          covidence_id == 718 ~ 'Clinical description of a completed outbreak of SARS in Vietnam, February-May 2003',
          covidence_id == 727 ~ 'Risk of severe acute respiratory syndrome-associated coronavirus transmission aboard commercial aircraft',
          covidence_id == 745 ~ 'Brief Report: Incubation Period Duration and Severity of Clinical Disease Following Severe Acute Respiratory Syndrome Coronavirus Infection',
          covidence_id == 1046 ~ 'The SARS outbreak in a general hospital in Tianjin, China -- the case of super-spreader',
          covidence_id == 1564 ~ 'Theoretically estimated risk of severe acute respiratory syndrome transmission through blood transfusion during an epidemic in Shenzhen, Guangdong, China in 2003',
          covidence_id == 1641 ~ 'SARS surveillance during emergency public health response, United States, March-July 2003',
          covidence_id == 1672 ~ 'The effect of global travel on the spread of SARS',
          covidence_id == 2043 ~ 'Risk factors for SARS transmission from patients requiring intubation: a multicentre investigation in Toronto, Canada',
          covidence_id == 2613 ~ 'Rapid awareness and transmission of severe acute respiratory syndrome in Hanoi French Hospital, Vietnam',
          covidence_id == 4285 ~ 'SARS in Singapore--predictors of disease severity',
          covidence_id == 4455 ~ 'Severe acute respiratory syndrome (SARS) in Singapore: clinical features of index patient and initial contacts',
          covidence_id == 4561 ~ 'Neutralizing antibody response and SARS severity',
          covidence_id == 4612 ~ 'Secondary household transmission of SARS, Singapore',
          covidence_id == 5387 ~ 'Transmission characteristics of MERS and SARS in the healthcare setting: a comparative study',
          covidence_id == 5589 ~ 'A simple approximate mathematical model to predict the number of severe acute respiratory syndrome cases and deaths',
          covidence_id == 5635 ~ 'Effectiveness of noninvasive positive pressure ventilation in the treatment of acute respiratory failure in severe acute respiratory syndrome',
          covidence_id == 5743 ~ 'The outbreak of SARS at Tan Tock Seng Hospital--relating epidemiology to control',
          covidence_id == 5878 ~ 'Alternative methods of estimating an incubation distribution: examples from severe acute respiratory syndrome',
          covidence_id == 7260 ~ 'Reconstruction of the infection curve for SARS epidemic in Beijing, China using a back-projection method',
          covidence_id == 12002 ~ 'Heterogeneous and Stochastic Agent-Based Models for Analyzing Infectious Diseases Super Spreaders',
          TRUE ~ article_title)) %>%#title consistency
        mutate(article_title = gsub(";", ",", article_title)) %>%
        mutate(article_title = gsub("\n", " ", article_title)) %>%
        mutate(article_title = gsub("\\s+", " ", article_title)) %>%
        mutate(article_title = str_to_title(article_title)) %>%
        #missing dois
        mutate(doi = sub(".*?10\\.", "10.", doi)) %>%
        mutate(doi = case_when(
          covidence_id == 1090 ~ '10.1016/s019606440300828x',
          covidence_id == 4261 ~ '10.1017/s0950268805004826',
          covidence_id == 4268 ~ '10.3201/eid1009.040155',
          covidence_id == 4387 ~ '10.1093/aje/kwh056',
          covidence_id == 5006 ~ '10.1007/s11325-004-0097-0',
          covidence_id == 5635 ~ '10.1378/chest.126.3.845',
          covidence_id == 5652 ~ '10.1001/jama.293.12.1450-c',
          covidence_id == 5682 ~ '10.1128/jcm.44.2.359-365.2006',
          covidence_id == 5879 ~ '10.1001/archinte.166.14.1505',
          covidence_id == 6239 ~ '10.1016/s1726-4901(09)70124-8',
          covidence_id == 7260 ~ '10.1080/03610910701792562',
          covidence_id == 8001 ~ '10.1360/03ww0126',
          covidence_id == 10922 ~ '10.1016/j.jmaa.2006.11.026',
          covidence_id == 10924 ~ '10.1137/040615547',
          covidence_id == 11000 ~ '10.3201/eid1309.070081',
          TRUE ~ doi)) %>%
        #paper copies
        # mutate(paper_copy_only = case_when(
        #   covidence_id %in% c(845,917) ~ FALSE,
        #   TRUE ~ paper_copy_only)) %>%
        #name typos
        # mutate(first_author_surname = case_when(
        #   covidence_id == 2648 ~ 'Shirley C.',
        #   covidence_id == 1447 ~ 'N.A.',
        #   TRUE ~ first_author_surname)) %>%
        mutate(first_author_first_name = sub(".*\\.(.*)", "\\1", first_author_first_name)) %>%
        mutate(first_author_first_name = sub("^\\s+", "", first_author_first_name)) #%>%                 
      # mutate(first_author_first_name = case_when(
      #   covidence_id == 2648 ~ 'Nimo-Paintsil',
      #   covidence_id == 2585 ~ 'Dalhat',
      #   covidence_id == 1033 ~ 'Ehichioya',
      #   covidence_id == 661 ~ 'Kerneis',
      #   TRUE ~ first_author_first_name))     
    df
}