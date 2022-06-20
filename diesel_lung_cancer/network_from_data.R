rm(list = ls())

library(igraph)
library(dplyr)
library(stringr)

hygiene_hypo_gen <- function(){
  
  c_grid <- read.csv("data/DatasetHygieneHypothesisCitationpaths.csv", na.strings = '')
  pub_list <- read.csv("data/DatasetHygieneHypothesisPublications.csv", na.strings = '')
  
  c_grid_true <- filter(c_grid, citation == 'yes')
  cited <- str_c(c_grid_true$authorcited, '-', c_grid_true$paperpublicationdate_cited) %>% stringr::str_to_lower()
  citing <- str_c(c_grid_true$AUTHORciting, '-', c_grid_true$PAPERPUBLICATIONDATE_citing) %>% stringr::str_to_lower()
  
  edge_list <- data_frame(cited,citing)
  pub_list$ID <- str_c(pub_list$FirstAuthor, '-', pub_list$PaperPublicationDate)%>% stringr::str_to_lower()
  
  setdiff(unique(c(edge_list$cited,edge_list$citing)), unique(pub_list$ID))
  # "torresborrego-mar-08" "lighterfisher-jun-12" difference in with or without hyphen
  
  pub_list$ID[pub_list$ID == "torres-borrego-mar-08"] <- 'torresborrego-mar-08'
  pub_list$ID[pub_list$ID == 'lighter-fisher-jun-12'] <- 'lighterfisher-jun-12'
  
  # process general outcome based on the supplement material: 
  outcome_vector <- c()
  for (i in (1:nrow(pub_list))){
    outcome <- unique(c
                      (pub_list$Siblings.Rhinitis[i],pub_list$Siblings.Allergy[i],pub_list$Infections.Rhinitis[i],pub_list$Infections.Allergy[i]))
    
    outcome <- outcome[outcome!='not reported']
    outcome <- outcome[!is.na(outcome)]
    
    x <- case_when(
      (length(outcome) == 1) && (outcome[1] == 'inverse relationship') ~ 'supportive',
      (length(outcome) == 1) && (outcome[1] %in% c('no relationship', 'positive relationship')) ~ 'non-supportive',
      (length(outcome) == 2) && (outcome %in% c('no relationship', 'positive relationship')) ~ 'non-supportive',
      TRUE ~ 'mixed'
    )
    
    outcome_vector <- c(outcome_vector, x)
  }
  
  pub_list$General.Outcome <- outcome_vector
  
  # create a graph
  g <- igraph::graph_from_data_frame(edge_list, directed=TRUE, vertices=pub_list)
  
  return(g)
  
}


diesel_lung_cancer <- function(){
  
  data <- read.delim("data/Diesel Exposure - dataset - citationpaths.csv",
                     na.strings = '',
                     sep = ";")
  
  # coding scheme: https://uofi.app.box.com/file/906354952928
  # study outcome: 1. supportive (positive association)
  #                2. no association
  #                3. unclear association (no conclusion can be drawn based on the included evidence)
  #                4. mixed (supportive in some subgroup(s), , no association in other subgroup(s)))
  #                5. no conclusion on DEE
  
  # exposure: 1. job title
  #           2. participants’ self-assessment
  #           3. quantified estimate based on a static job-exposure matrix 
  #               (without time component), or by an industrial hygienist
  #           4. quantified estimate based on dynamic job-exposure matrix, 
  #              (with time component), taking into account changes over time in 
  #               diesel exposure and participants’ job or working hours. 
  
  # Type of diesel exhaust exposure: 1. traditional ???
  #                                  2. mix of traditional and transitional
  
  cited <- unique(data[,c(1,3,4,8,10,12,14,16,18,20,22,
                          24,26,28,30,32,34,36,38,40,42,44,46,
                          48,50,52)])
  
  citing <- unique(data[,c(2,5,6,9,11,13,15,17,19,21,23,25,27,29,
                           31,33,35,37,39,41,43,45,47,49,51,53)])
  
  cited_colnames <- colnames(cited)
  citing_colnames <- stringr::str_to_lower(colnames(citing))
  colnames(cited) <- str_replace(cited_colnames, "_cited", "") %>% str_replace("cited","")
  colnames(citing) <- str_replace(citing_colnames, "_citing", "") %>% str_replace("citing", "")
  
  pub_list <- unique(rbind(cited,citing))
  
  # convert conclusion(coded) to Conclusion
  # study outcome: 1. supportive (positive association)
  #                2. no association --> non-supportive
  #                3. unclear association (no conclusion can be drawn based on the included evidence) --> unclear association
  #                4. mixed (supportive in some subgroup(s), , no association in other subgroup(s))) --> mixed results
  #                5. no conclusion on DEE --> not reported
  
  coded_conclusion <- pub_list$conclusion
  pub_list$Conclusion <- case_when(
    coded_conclusion == 1 ~ 'Supportive',
    coded_conclusion == 2 ~ 'Non-supportive',
    coded_conclusion == 3 ~ 'Unclear association',
    coded_conclusion == 4 ~ 'Mixed results',
    coded_conclusion == 5 ~ 'Not reported')
  
  pub_list$Conclusion <- factor(pub_list$Conclusion, 
                                levels = c('Supportive',
                                           'Non-supportive',
                                           'Unclear association',
                                           'Mixed results',
                                           'Not reported'))
  
  # convert study.design(coded) to Study.Design
  # based on the publication
  # A3: Case-control
  # A4: Cohort
  # B1: Narrative review
  # B2 + B3: Systematic review
  coded_study_design <- pub_list$study.design
  pub_list$Study.Design <- case_when(
    coded_study_design == 'A3' ~ "Case-control",
    coded_study_design == 'A4' ~ "Cohort study",
    coded_study_design == 'B1' ~ "Narrative review",
    coded_study_design %in% c('B2','B3') ~ "Systematic review"
  )
  
  # convert fundingsource (coded) to Funding.Source
  # 0: Exclusively non-profit
  # 1: Exclusively for-profit
  # 2: Both profit and non-profit
  # 3: Not reported/unclear
  
  coded_funding_source <- pub_list$fundingsource
  pub_list$Funding.Source <- case_when(
    coded_funding_source == 0 ~ 'Exclusively non-profit',
    coded_funding_source == 1 ~ 'Exclusively for-profit',
    coded_funding_source == 2 ~ 'Both profit and non-profit',
    coded_funding_source == 3 ~ 'Not reported/unclear'
  )
  
  # create edge list
  c_grid <- filter(data, citation==1) %>% select(CITINGid,citedid)
  
  # create a graph
  g <- igraph::graph_from_data_frame(c_grid, directed = TRUE, vertices = pub_list)
  
  return(g)
  
}

# generate the attribute list used for matrix conversion -- hygiene hypothesis
hygiene_hypo_publist_gen <-  function(){
  
  pub_list <- read.csv("data/DatasetHygieneHypothesisPublications.csv", na.strings = '')
  pub_list$ID <- str_c(pub_list$FirstAuthor, '-', pub_list$PaperPublicationDate)%>% stringr::str_to_lower()
  pub_list$ID[pub_list$ID == "torres-borrego-mar-08"] <- 'torresborrego-mar-08'
  pub_list$ID[pub_list$ID == 'lighter-fisher-jun-12'] <- 'lighterfisher-jun-12'
  
  return(pub_list)
  
}

# generate the attribute list used for matrix conversion -- Diesel Lung cancer dataset
diesel_lung_cancer_publist_gen <- function(){
  
  data <- read.delim("data/Diesel Exposure - dataset - citationpaths.csv",
                     na.strings = '',
                     sep = ";")
  
  # coding scheme: https://uofi.app.box.com/file/906354952928
  # study outcome: 1. supportive (positive association)
  #                2. no association
  #                3. unclear association (no conclusion can be drawn based on the included evidence)
  #                4. mixed (supportive in some subgroup(s), , no association in other subgroup(s)))
  #                5. no conclusion on DEE
  
  # exposure: 1. job title
  #           2. participants’ self-assessment
  #           3. quantified estimate based on a static job-exposure matrix 
  #               (without time component), or by an industrial hygienist
  #           4. quantified estimate based on dynamic job-exposure matrix, 
  #              (with time component), taking into account changes over time in 
  #               diesel exposure and participants’ job or working hours. 
  
  # Type of diesel exhaust exposure: 1. traditional ???
  #                                  2. mix of traditional and transitional
  
  cited <- unique(data[,c(1,3,4,8,10,12,14,16,18,20,22,
                          24,26,28,30,32,34,36,38,40,42,44,46,
                          48,50,52)])
  
  citing <- unique(data[,c(2,5,6,9,11,13,15,17,19,21,23,25,27,29,
                           31,33,35,37,39,41,43,45,47,49,51,53)])
  
  cited_colnames <- colnames(cited)
  citing_colnames <- stringr::str_to_lower(colnames(citing))
  colnames(cited) <- str_replace(cited_colnames, "_cited", "") %>% str_replace("cited","")
  colnames(citing) <- str_replace(citing_colnames, "_citing", "") %>% str_replace("citing", "")
  
  pub_list <- unique(rbind(cited,citing))
  
  # convert conclusion(coded) to Conclusion
  # study outcome: 1. supportive (positive association)
  #                2. no association --> non-supportive
  #                3. unclear association (no conclusion can be drawn based on the included evidence) --> unclear association
  #                4. mixed (supportive in some subgroup(s), , no association in other subgroup(s))) --> mixed results
  #                5. no conclusion on DEE --> not reported
  
  coded_conclusion <- pub_list$conclusion
  pub_list$Conclusion <- case_when(
    coded_conclusion == 1 ~ 'Supportive',
    coded_conclusion == 2 ~ 'Non-supportive',
    coded_conclusion == 3 ~ 'Unclear association',
    coded_conclusion == 4 ~ 'Mixed results',
    coded_conclusion == 5 ~ 'Not reported')
  
  pub_list$Conclusion <- factor(pub_list$Conclusion, 
                                levels = c('Supportive',
                                           'Non-supportive',
                                           'Unclear association',
                                           'Mixed results',
                                           'Not reported'))
  
  # convert study.design(coded) to Study.Design
  # based on the publication
  # A3: Case-control
  # A4: Cohort
  # B1: Narrative review
  # B2 + B3: Systematic review
  coded_study_design <- pub_list$study.design
  pub_list$Study.Design <- case_when(
    coded_study_design == 'A3' ~ "Case-control",
    coded_study_design == 'A4' ~ "Cohort study",
    coded_study_design == 'B1' ~ "Narrative review",
    coded_study_design %in% c('B2','B3') ~ "Systematic review"
  )
  
  # convert fundingsource (coded) to Funding.Source
  # 0: Exclusively non-profit
  # 1: Exclusively for-profit
  # 2: Both profit and non-profit
  # 3: Not reported/unclear
  
  coded_funding_source <- pub_list$fundingsource
  pub_list$Funding.Source <- case_when(
    coded_funding_source == 0 ~ 'Exclusively non-profit',
    coded_funding_source == 1 ~ 'Exclusively for-profit',
    coded_funding_source == 2 ~ 'Both profit and non-profit',
    coded_funding_source == 3 ~ 'Not reported/unclear'
  )
  
  return(pub_list)
}