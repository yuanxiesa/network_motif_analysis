source('network_from_data.R')
source('network_sim_functions.R')

library(motifr)
library(ggplot2)

motif_name <- "2,1[II.C.5]"
number_of_sim <- 500

# use diesel lung cancer dataset
g <- diesel_lung_cancer()
pub_list <- diesel_lung_cancer_publist_gen()

my.edge.list <- igraph::as_edgelist(g)
# must switch the order, citing and cited were flipped if using intergraph.
V1 <- my.edge.list[,1]
V2 <- my.edge.list[,2]
my.edge.list <- cbind(V2,V1)

my.net <- network::network(x=my.edge.list, directed=TRUE)
network::set.vertex.attribute(my.net, attrname = 'name', value = pub_list$name)
network::set.vertex.attribute(my.net, attrname = 'Study.Design', value=pub_list$Study.Design)

article.type <- network::get.vertex.attribute(my.net, 'Study.Design')

article.type.convert <- case_when(
  article.type == 'Cohort study' ~ 0,
  article.type == 'Case-control' ~ 0,
  article.type == 'Narrative review' ~ 1,
  article.type == "Systematic review" ~ 1
)

network::set.vertex.attribute(my.net, 'lvl', article.type.convert)

#motifr::explore_motifs(net=my.net, lvl_attr = 'lvl')

obs <- motifr::count_motifs(my.net, lvl_attr = c('lvl'), 
                            motifs = motif_name,
                            directed = TRUE)

motif_stats <- c()
result <- create_potential_citation_list(g)

for (i in 1:number_of_sim) {
  
  my.net.sim <- single_network_sim(g, result)
  
  network::set.vertex.attribute(my.net.sim, 'lvl', article.type.convert)
  
  motif <- motifr::count_motifs(my.net.sim, lvl_attr = c('lvl'), 
                                motifs = motif_name,
                                directed = TRUE)
  
  motif_stats <- c(motif_stats, motif$count)
  
}

dev.off()
data <- as.data.frame(motif_stats)

ggplot(data, aes(x=motif_stats)) +  
  geom_histogram(color="black", fill="white", binwidth = 1) +
  geom_vline(xintercept = obs$count, color='blue', linetype = 2) +
  xlab(motif_name) +
  ylab('')

