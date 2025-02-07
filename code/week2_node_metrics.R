### What makes a node 'key' or 'central' to a network ? ----

library(tidyverse)
library(igraph)

# import campnet data

mat <- as.matrix(read_csv("data/campnet.csv")[,-1])
mat1 <- as.matrix(read_csv("data/Camp92_wk2.csv")[,-1])
mat2 <- as.matrix(read_csv("data/Camp92_wk3.csv")[,-1])

mat   # top 3 choices in week 3
mat1  # ranked choices week 2
mat2  # ranked choices week 3

# attributes
campattr <- readr::read_csv("data/campattr.csv")
campattr

# readr:: only reads one function from the package

# convert to igraph object
g <-  graph_from_adjacency_matrix(mat)
g


# add gender attribute
g <- set_vertex_attr(g,"gender",value=campattr$Gender)
g


# visualize
plot(g, edge.arrow.size=.1)


### Degree Centrality ----

# the number of edges that an individual node has
# can be 'all', 'outgoing', or 'incoming'
# equivalent to summing rows/columns of adjacency matrix 
# for weighted networks equivalent to 'strength'

degree(g)
degree(g, mode="all")  # sum of all ties everyone has
degree(g, mode="out")  # sum of only the out-going ties (giving)
degree(g, mode="in")   # sum of only the in-coming ties (receiving)

# gives more info (including graph level info - centralization)
centr_degree(g, mode="all", normalized=T)  # centralization measures how spread out degrees are in a network
centr_degree(g, mode="out", normalized=T)
centr_degree(g, mode="in", normalized=T)


## Degree distributions:
degree_distribution(g) # relative frequency of vertices with degree 0,1,2,3,4,5....etc.
degree_distribution(g,mode="in") 

# View a summary of degree
table(degree(g))

# View a summary of in-degree
table(degree(g, mode="in"))


# Plot based on in-degree.

# Set colors 
blues <- colorRampPalette(c("cyan", "purple"))  # get me colors between cyan and purple
col <- blues(max(degree(g,mode="in"))+1) # look at max n degree and then + 1 to get the number of colors
col <- col[degree(g, mode="in")+1]
col

plot(g, vertex.color=col, edge.arrow.size=.1, vertex.size=19,
     vertex.label.cex=.8,vertex.label.color="black")





### Comparing subgroups - e.g. degree ----

#making an undirected campnet graph
gu <- as_undirected(g) 
gu
plot(gu)

degree(gu)
degree(gu, normalized=T) # scales the degree, useful when comparing multiple networks

# compare by group
by(degree(gu), INDICES=V(gu)$gender, mean)  # compare mean degrees of gu by the gender vertices

# can test these group differences by doing a permutation based test (see later).



### Eigenvector Centrality ----

# Bonacich 1972
# higher in individuals who are well-connected and connected to others who are also well-connected
# related to sum of centralitites of neighbors


eigen_centrality(g, directed=T, weights=NA)
eigen_centrality(g, directed=T, weights=NA)$vector  # looks at just the eigen-central vector
round(eigen_centrality(g, directed=T, weights=NA)$vector,2) # rounded

# includes graph level info
centr_eigen(g, directed=T, normalized=T) 
centr_eigen(g, directed=T, normalized=T)$vector
round(centr_eigen(g, directed=T, normalized=T)$vector,1)




eigen_centrality(gu)

# note: in disconnected networks, small component nodes will get ev.cent of 0.





### Closeness centrality ----

# Freeman 1979
# the sum of geodesic distances from a node to all others
# how close to others in a network is each node
# this is essentially the inverse of a node's average geodesic distance to others in the network

closeness(g, mode="all", weights=NA)  # average shortest distance to get to all the other nodes in the network

#includes graph level metrics - be careful with directed graphs
centr_clo(g, mode="all", normalized=T)  # determine how clustered the graph is
centr_clo(g, mode="out", normalized=T) 
centr_clo(g, mode="in", normalized=T)  # comes out as NaN if there are people that didn't receive anything (no direction in) 

#note: closeness centrality is problematic in disconnected networks as no path exists between pairs of nodes
#note: not particularly useful for directed networks due to fragmentation


closeness(gu)


### K-reach Centrality ----

# Borgatti 2006
# how many  nodes can be reached in K steps from each  node

# Function modified from those by Dai Shizuka
# this is for undirected graphs - could be modified for directed graphs
# could also be modified for weighted graphs

# Function for K-step reach
reachK<-function(x,K){
  r=vector(length=vcount(x))
  for (i in 1:vcount(x)){
    n=neighborhood(x,K,nodes=i)
    ni=unlist(n)
    l=length( ni)
    r[i]=(l)/vcount(x)}
  r}

reachK(gu,3)  #proportion of nodes that can be reached in 3 steps
V(gu)$name
mean(reachK(gu,3)) #average across all nodes



### Betweenness Centrality ----

# Freeman 1979
# related to how many shortest paths each node lies on when examining all pairs of nodes
# individuals who have high betweenness are bridges connecting nodes from different parts of the network
# potential for controlling flow through a network

betweenness(g, directed=T, weights=NA) # can also do directed=F for undirected networks

#gives graph level info
centr_betw(g, directed=T, normalized=T)


betweenness(gu)
betweenness(gu, normalized=T)


## As well as edges, each edge can also be assigned a betweenness score
edge_betweenness(g, directed=T, weights=NA)






### Hub Score and Authority Score ----

# Kleinberg defined hubs and authorities as follows:
# hubs provide many links to other vertices (many outgoing ties)
# authorities receive many incoming links

hub_score(g, weights=NA)
hub_score(g, weights=NA)$vector

authority_score(g, weights=NA)
authority_score(g, weights=NA)$vector


hub_score(gu)
authority_score(gu)


### Google Page Rank ----

#  a measure of how influential a node is in directing flow through a network.

page_rank(g)
page_rank(g)$vector

page_rank(gu)


## There are several others... we can discuss in time.



### Practice Exercises ----


## 1. Undirected Network - Mouse Brain Network

# Import the cfos0.csv file and make an undirected network from this edgelist
# This network shows correlated brain activity in a mouse brain

cfos0 <- read_csv("data/cfos0.csv")

# make an igraph object from edgelist
g_cfos0 <- graph_from_edgelist(as.matrix(cfos0), directed = FALSE)


# which node has the highest degree?
degree(g_cfos0) # CA1 has the highest degree

#plot the network coloring nodes by degree
# Set colors 
greens <- colorRampPalette(c("lightseagreen", "orange"))  # get me colors between lightseagreen and orange
col <- greens(max(degree(g_cfos0,mode="in"))+1)  # look at max n degree and then + 1 to get the number of colors
col <- col[degree(g_cfos0, mode="in")+1]
col

plot(g_cfos0, vertex.color=col, edge.arrow.size=.1, vertex.size=19,
     vertex.label.cex=.8,vertex.label.color="black")

# what is the eigenvector centrality of each node?
eigen_c <- eigen_centrality(g_cfos0, directed=F)$vector
eigen_c

# what is the Spearman correlation between degree and eigenvector centrality?
cor.test(degree(g_cfos0), eigen_c, method = "spearman")

  ## degree and eigenvector centrality are NOT independent


# what are the closeness centralities?
closeness(g_cfos0)

# which node has the highest betweenness?
betweenness(g_cfos0, directed=F)


## Directed Network - Baboon Data

# Import the Baboon Data and make a 
d <- readRDS("data/Baboon_Data.Rdata")
d$Grooming   # Grooming Matrix
d$Individual # Individual Attributes

b <- graph_from_adjacency_matrix(d$Grooming, mode = "directed", weighted=T)

# add age attribute from "Individual" dataset
b <- set_vertex_attr(b, "age", value = d$Individual$Age)
b
plot(b, edge.arrow.size=.3)

# what is the out-degree of each baboon?
degree(b, mode="out")

# is out-degree associated with age? 
# run a Spearman correlation to determine
plot(V(b)$age, degree(b, mode="out"))
cor.test(degree(b, mode="out"), V(b)$age, method = "spearman")

# calculate the authority and hub score of each node
hub_score(b, weight=NA)$vector
authority_score(b, weight=NA)$vector
page_rank(b)$vector

hits_scores(b, weights = NA)$hub


# what do these metrics suggest about who is most important to the grooming network?




### How to import Shakespeare data ----

# This shows how frequently various characters of Shakespeare plays coccur in acts in each play
# The networks are weighted and undirected.
# The data are a list of named matrices.

data <- readRDS("data/bard_nets.RData")
data
names(data)
lapply(data, rownames) # gets the row names from each shakespeare play
lapply(data, function(x) x[1:4,1:5])

#e.g. Let's look at The Tempest.  It's matrix 31.
s31 <- graph_from_adjacency_matrix(data[[31]], mode="undirected", weighted=T)
s31
plot(s31)
degree(s31)
eigen_centrality(s31)

