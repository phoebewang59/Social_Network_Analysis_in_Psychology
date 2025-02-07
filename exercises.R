## Week 1 Exercise ##

birds <- read.csv("data/birds1.csv")

birds_sps <- read.csv("data/birds1_sps.csv")


g <- graph_from_adjacency_matrix(as.matrix(birds), mode=c("undirected"), weighted = T)

# Create new vertex attribute called 'species'
g <- set_vertex_attr(g, "species", value = birds_sps$species)
g

plot(g)


E(g)[[.inc('JRA')]] 

E(g)[[weight>=6]] 

# Plot the graph with node coloring
node_colors <- c("GCW" = "lightblue", "YTW" = "pink", "NP" = "lightgreen", "BWW" = "yellow")
vertex_color <- node_colors[vertex_attr(g, "species")]
plot(
  g, vertex.label.color = "black",
  vertex.color = vertex_color
)

#####################################
# Q6-10
library(igraph)
hb <- read.csv("data/holmes_bigram.csv")
hb_attr <- read.csv("data/holmes_bigram_attr.csv")

ghb <- graph.edgelist(as.matrix(hb[,1:2]), directed = FALSE)

ghb <- set_edge_attr(ghb, "weight", value = hb$value)

ghb

E(ghb)[[.inc('strong')]] 

plot(ghb)


V(ghb)$color <- ifelse(V(ghb)$valence == "positive", "orange", "dodgerblue")
plot(ghb, vertex.label.color = "black")

as_adjacency_matrix(ghb)

################################################################################
################################################################################
## Week 2 Exercise ##

# Question 1-4: Food Sharing Data

fs <- readRDS("data/FoodSharing_Data.RData")
gfs <- graph_from_adjacency_matrix(fs$TransferOut, mode = "directed", weighted=T)
gfs

# add attribute FoodInsecure from Individual
gfs <- set_vertex_attr(gfs, "FoodInsecure", value = fs$Individual$FoodInsecure)

# View a summary of out-degree
table(degree(gfs, mode = "out"))

# Function for K-step reach (x = igraoh object, K = number of steps)
reachK<-function(x,K){
  r=vector(length=vcount(x))
  for (i in 1:vcount(x)){
    n=neighborhood(x,K,nodes=i)
    ni=unlist(n)
    l=length(ni)
    r[i]=(l)/vcount(x)}
  r}

round(mean(reachK(gfs, 3)), 3)

# Spearman rank correlation test
cor.test(degree(gfs, mode="out"), V(gfs)$FoodInsecure, method = "spearman")

################################
# Questions 5-9: gossip_crew.RData

gossip <- readRDS("data/gossip_crew.RData")
names(gossip)

g_neg <- graph_from_adjacency_matrix(gossip$negative, mode = "directed", weighted = T)

# add gender vertex
g_neg <- set_vertex_attr(g_neg, "gender", value = gossip$attributes$Gender)
g_neg

eigen_centrality(g_neg, directed= T, weights = NA)$vector[c('M12', 'M20', 'F23', 'M14', 'F21')]

round(hub_score(g_neg, weights = NA)$vector[c("M12", "M14", "F21", "M20", "F23")], 4)

hubScore <- hub_score(g_neg, weights = NA)$vector
eigenC <- eigen_centrality(g_neg, directed= T, weights = NA)$vector

# spearman rank correlation test
cor.test(hubScore, eigenC)


# compare by group
by(hubScore, INDICES = V(g_neg)$gender, mean)
by(eigenC, INDICES = V(g_neg)$gender, mean)

################################
# Question 10: birds1.csv

birds1 <- read_csv("data/birds1.csv")

# threshold matrix
birds1[birds1 < 3] <- 0
birds1[birds1 >= 3] <- 1
birds1

g_birds <- graph_from_adjacency_matrix(as.matrix(birds1), mode = "undirected")
g_birds

round(closeness(g_birds, mode="all", weights=NA), 3)  # average shortest distance to get to all the other nodes in the network

plot(g_birds)

round(closeness(g_birds), 3)

################################################################################
################################################################################
## Week 3 Exercise ##

# Questions 1-3: Bat_Data.RData
bat <- readRDS("data/Bat_Data.RData")

gbat <- graph_from_adjacency_matrix(bat$Lick, mode = "directed", weighted = T)
gbat

# total asymmetric and mutual ties
dyad_census(gbat)  #get mutual, reciprocal, absent ties.

# reciprocity
reciprocity(gbat)

# transitivity
transitivity(gbat)

n1 <- neighbors(gbat, 'dot', mode = c('in'))
n2 <- neighbors(gbat, 'mya', mode = c('out'))
intersection(n1, n2)
##################################################
# Questions 4-7
cfos0 <- read_csv("data/cfos0.csv")
cfos2 <- read_csv("data/cfos2.csv")

# convert into matrix
cfos0.mat <- as.matrix(cfos0)
cfos2.mat <- as.matrix(cfos2)

g_cfos0 <- graph_from_edgelist(cfos0.mat, directed = F)
g_cfos2 <- graph_from_edgelist(cfos2.mat, directed = F)

diameter(g_cfos0)
diameter(g_cfos2)

# furthest vertices
farthest_vertices(g_cfos0) 

# graph density & average path length
print(c(round(edge_density(g_cfos2), 2), round(mean_distance(g_cfos2), 2)))
print(c(round(edge_density(g_cfos0), 2), round(mean_distance(g_cfos0), 2)))



##################################################
# Questions 8-10
bard <- readRDS("data/bard_nets.RData")

g_hamlet <- graph_from_adjacency_matrix(bard$Hamlet, mode = "undirected", weighted = T)
g_macbeth <- graph_from_adjacency_matrix(bard$macbeth, mode = "undirected", weighted = T)
g_merry <- graph_from_adjacency_matrix(bard$`Merry Wives of Windsor`, mode = "undirected", weighted = T)
g_twelfth <- graph_from_adjacency_matrix(bard$`Twelfth Night`, mode = "undirected", weighted = T)


round(edge_density(g_merry), 2)
round(edge_density(g_macbeth), 2)
round(edge_density(g_twelfth), 2)
round(edge_density(g_hamlet), 2)

diameter(g_merry)
diameter(g_macbeth)
diameter(g_twelfth)
diameter(g_hamlet)


t <- as.data.frame(eigen_centrality(g_hamlet)$vector)

t |> 
  arrange(desc(eigen_centrality(g_hamlet)$vector))
