library(intergraph)
library(lubridate)
library(intergraph)

create_potential_citation_list <- function(input_graph, time_gap = 365){
  
  citation = unname(igraph::degree(g, mode='in'))
  pub_date <- igraph::get.vertex.attribute(g, 'PaperPublicationDate')
  potential_citation_list <- list()
  ignore_nodes <- c()
  
  for (i in (1: length(pub_date))) {
    
    temp_potential_citation_list <- c()
    
    for (j in (1:length(pub_date))){
      
      time_difference <- my(pub_date[j]) - my(pub_date[i])
      
      if (time_difference > time_gap) {
        
        temp_potential_citation_list <- c(temp_potential_citation_list, j)
        
      }
    }
    
    potential_citation_list[[i]] <- temp_potential_citation_list
    
    if (length(temp_potential_citation_list) == 0) {
      
      print(stringr::str_c('temp_potential_citation_list ', i, ' is empty' ))
      
      ignore_nodes <- c(i, ignore_nodes)
      
    }
  }
  
  return(list("potential_citation_list" = potential_citation_list,
              "ignore_nodes" = ignore_nodes))
}



single_network_sim <- function(input_graph, result){
  
  g_sim <- igraph::delete.edges(g, edges=igraph::E(g))
  my.net.sim <- intergraph::asNetwork(g_sim)
  citation = unname(igraph::degree(g, mode='in'))
  
  potential_citation_list <- result$potential_citation_list
  ignore_nodes <- result$ignore_nodes
  
  for (i in 1:network::network.size(my.net.sim)){
    
    if (!(i %in% ignore_nodes)){
      
      citation_count <- citation[i]
      
      nodes_selected <- sample(potential_citation_list[[i]], 
                               min(citation_count, length(potential_citation_list[[i]])))
      
      n <- length(nodes_selected)
      
      e_list <- as.vector(rbind(rep(i,n), nodes_selected))
      
      network::add.edges(my.net.sim, head = rep(i, n), tail = nodes_selected)
      
    }
  }
  
  return(my.net.sim)
}