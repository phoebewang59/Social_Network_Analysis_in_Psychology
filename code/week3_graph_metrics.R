### Characterizing whole networks ----

library(igraph)

## Example Dataset - campnet
# 18 individuals at a Summer camp nominating friendship preferences

mat <- as.matrix(readr::read_csv("data/campnet.csv")[,-1])
mat1 <- as.matrix(readr::read_csv("data/Camp92_wk2.csv")[,-1])
mat2 <- as.matrix(readr::read_csv("data/Camp92_wk3.csv")[,-1])

mat   # top 3 choices in week 3
mat1  # ranked choices week 2
mat2  # ranked choices week 3

# attributes
campattr <- readr::read_csv("data/campattr.csv")
campattr

# convert to igraph object

g <-  graph_from_adjacency_matrix(mat)
g


# add gender attribute
g <- set_vertex_attr(g,"gender",value=campattr$Gender)
g


# visualize
plot(g, edge.arrow.size=.2)


## sizes

gorder(g) # nodes
vcount(g) # also nodes

gsize(g) # edges
ecount(g) # also edges



### Measures of Cohesion ----

## All cohesion measures to some extent don't capture everything about what a network's structure looks like.



### Graph Density ----

# The probability of tie occurring between any 2 random nodes
# in valued networks, equates to average tie strength

graph.density(g)
edge_density(g, loops=F)  # almost always the default is to ignore self-loops
edge_density(g)  # a lot of igraph functions have '.' and '_' versions.

ecount(g)/(vcount(g)*(vcount(g)-1)) #equivalent to this in a directed network


# Density is obviously influenced by e.g. size, making comparisons hard between varying networks.
# in this case possible to use average degree of network.

#dhat = 2T / n  (T is number of edges)
degree(g)
mean(degree(g))
(ecount(g)*2)/vcount(g)


# Can compare density within subgroups:
# Count ties between members of same group, compute density just for those ties.
# e.g. campnet dataset males vs females.

g.female <- induced_subgraph(g, V(g)$gender==1)  #female-female
g.male <- induced_subgraph(g, V(g)$gender==2)  #male-male

g.female
g.male

graph.density(g.female)  #.36
graph.density(g.male) #.28



## Compare Density of two graphs visually

set.seed(17)

gs1 <- sample_gnp(n = 20, p = .1, directed = F)
gs2 <- sample_gnp(n = 20, p = .25, directed = F)

gs1
gs2


par(mfrow=c(1,2))
plot(gs1, main=paste0("density = ",round(graph.density(gs1),3)))
plot(gs2, main=paste0("density = ",round(graph.density(gs2),3)))
par(mfrow=c(1,1))





### What is a 'whole' network anyway... Components ----

# Components are individually connected parts of graphs
components(gs1) # number and size of components

# get components
decompose(gs1)



# Component Ratio:

# a measure of how well connected a graph is.
# CR =  c - 1 / n -1   (c is number of components, n is number of nodes in the graph)
# maximum is 1 when every node is an isolate.
# minimum of 0 when whole network is one component.
# an inverse measure of cohesion

gs3 <- sample_gnp(n = 200, p=.015, directed = F)
gs3
plot(gs3, vertex.size=2, vertex.label=NA)

cno <- components(gs3)$no #10
cno
(cno - 1) / (vcount(g) - 1) # [1] Component Ratio = 0.529


# you can also just create a network that is only the largest component
cs <- components(gs1)
cs
gs.large <- induced_subgraph(gs1, which(cs$membership == which.max(cs$csize)))
gs.large
plot(gs.large)


### Average path lengths, diameter etc.----

# Another key measure of network connectedness revolves around paths in a network
# undirected network - either direction
# directed network - can only go via direction of edge


# Example Dataset - outbreak of measles in Germany village (19th C)
measles <- read.csv("data/measles.csv")

# Get graph object
gm <- graph_from_data_frame(measles, directed = TRUE)

# Make a basic plot
plot(gm, 
     vertex.label.color = "black", 
     vertex.label.cex = .6,
     edge.color = 'gray42',
     vertex.size = 0,
     edge.arrow.size = 0.05,
     layout = layout_nicely(gm))


# diameter - standard measure of 'distance' in graph
diameter(gm, directed=T)
diameter(gm, directed=F) # if don't wish to consider edge direction

# Of all shortest paths between two nodes, what is the average length?
average.path.length(gm)
average.path.length(gm, directed = F) # if don't wish to consider edge direction

# another name for the same function
mean_distance(gm, directed = T)
mean_distance(gm, directed = FALSE)



## Checking for edges:

# Is there an edge going from vertex 184 to vertex 178?
gm['184', '178']

# Is there an edge going from vertex 184 to vertex 178?
gm['178', '184']

# Show all edges going to or from vertex 184
incident(gm, '184', mode = c("all"))

# Show all edges going out from vertex 184
incident(gm, '184', mode = c("out"))

# Identify all neighbors of vertex 12 regardless of direction
neighbors(gm, '12', mode = c('all'))

# Identify other vertices that direct edges towards vertex 12
neighbors(gm, '12', mode = c('in'))

# Identify any vertices that receive an edge from vertex 42 and direct an edge to vertex 124
n1 <- neighbors(gm, '42', mode = c('out'))
n2 <- neighbors(gm, '124', mode = c('in'))
intersection(n1, n2)

#- Which two vertices are the furthest apart in the graph ?
farthest_vertices(gm) 

#- Shows the path sequence between two furthest apart vertices.
get_diameter(gm)  

# Identify vertices that are reachable within two connections from vertex 42
ego(gm, 2, '42', mode = c('out'))

# Identify vertices that can reach vertex 42 within two connections
ego(gm, 2, '42', mode = c('in'))

# Identify vertices that are within two connections of vertex 42 either way
ego(gm, 2, '42')





### Egocentric graphs / Geodesic Distance ----

# Shortest paths between all nodes:
distances(gm) # includes weights in assessment
distances(gm, weights=NA) # ignore weights


# Make an ego graph
g184 <- make_ego_graph(gm, diameter(gm), nodes = '184', mode = c("all"))[[1]]
g184

# Get a vector of geodesic distances of all vertices from vertex 184 
dists <- distances(g184, "184")
dists

# Create a color palette of length equal to the maximal geodesic distance plus one.
colors <- c("black", "red", "orange", "blue", "dodgerblue", "cyan")

# Set color attribute to vertices of network g184.
V(g184)$color <- colors[dists+1]

# Visualize the network based on geodesic distance from vertex 184 (patient zero).
plot(g184, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = .6,
     edge.color = 'black',
     vertex.size = 7,
     edge.arrow.size = .05,
     main = "Geodesic Distances from Patient Zero"
)


### Reciprocity and Transitivity ----


# Reciprocity examines dyadic ties in directed networks
# are they mutual/symmetric/bidrectional or asymmetric/unidirectional?

plot(g, edge.arrow.size=.2) #campnet graph again
 
## reciprocity
reciprocity(g) #[1] 0.7037037  70% of ties are reciprocated.

dyad_census(g)  #get mutual, reciprocal, absent ties.

2*dyad_census(g)$mut/ecount(g) # this is how reciprocity is calculated



## Transitivity

# transitivity is a local measure of connectedness.

# in undirected graphs, it's a measure of how closed each triangle in the network is
set.seed(111)
gx <- sample_gnp(n=22, p=0.15, directed=F)
gx

#undirected graph
plot(gx, vertex.color="darkred", edge.color="mistyrose",
     edge.width=3, edge.arrow.mode=0,
     vertex.size=9,vertex.label.cex=.8,
     vertex.label.color="white")

transitivity(gx) #0.2282609   23% of triangles are 'closed' = transitive.
transitivity(gx, type="global")   # same as above

transitivity(gx, type="local")  # gives relative transitivity for each node.
# notice node 7 has one closed triangle out of  3 potential = 1/3 = .333


# In directed networks, we can learn a lot more about triangle structure. 
# there are many more potential triangle (see notes)

plot(g, edge.arrow.size=.2) # directed network

transitivity(g) #[1] 0.5046729   friendship network has 50% of triangles being 'closed' = transitive.
transitivity(g, type="global")   # same as above
transitivity(g, type="local")





### Practice Example ----


# Read in the Columbia Friendship Data:
d <- readRDS("data/Colombia_Data.RData")
d
d$Friends  # adjacency matrix

d$Individual  # node attributes



# Answer the following questions:

# using the "Friends" matrix, create a directed graph object:

g <- graph_from_adjacency_matrix(d$Friends, mode = "directed")
g

# using the "Individual" data, add a node attribute of "Ethnicity"
g <- set_vertex_attr(g, "Ethnicity", value = d$Individual$Ethnicity)
g

# Plot the network coloring nodes by Ethnicity
# Plot the graph with node coloring
node_colors <- c("MESTIZO" = "darkorange", "AFROCOLOMBIAN" = "blue", "EMBERA" = "pink")
vertex_color <- node_colors[vertex_attr(g, "Ethnicity")]
plot(
  g, vertex.label.color = "black",
  vertex.color = vertex_color
)

# Change the labeling: get rid of "ID "
V(g)$label <- gsub("ID ", "", names(V(g)))

plot(
  g,
  vertex.color = vertex_color,
  vertex.size = 12,
  vertex.label = V(g)$label,
  edge.arrow.size = .2
)


# how many components?
components(g)

# what is the size of the largest component?
table(components(g)$membership)

# what is the component ratio? 
cno <- components(g)$no # $no is number of components
cno
(cno - 1)/ (vcount(g) - 1)

# what is the density, average path length, diameter?
edge_density(g)  # density
average.path.length(g) # default is Directed = T
diameter(g, directed=T)

# what is the reciprocity and transitivity?
reciprocity(g)
transitivity(g)

# subset the graph to only include the largest component
cs <- components(g)

g.large <- induced_subgraph(g, which(cs$membership == which.max(cs$csize))) # which.max(cs$csize) == 1
g.large
plot(g.large)

vertex_color.large <- node_colors[vertex_attr(g.large, "Ethnicity")]

plot(
  g.large,
  vertex.color = vertex_color.large,
  vertex.size = 6,
  vertex.label = V(g)$label,
  edge.arrow.size = .1
)

# what is the density and average path length of this large component graph? 
# How does it differ from the whole network?
edge_density(g.large)
mean_distance(g.large)

# subset the graph to only include individuals of EMBERA ethnicity.

g.EMBERA <- induced_subgraph(g, V(g)$Ethnicity == "EMBERA")
g.EMBERA
plot(g.EMBERA)

# what is the density, reciprocity and transitivity of the Embera network?
edge_density(g.EMBERA)
transitivity(g.EMBERA)
reciprocity(g.EMBERA)



### Gaining confidence in network measures via: ----

## bootstrapping
## jackknifing
## permutation


# Quick example - 

# Let's look at using jackknifing to get confidence in transitivity of campnet data


transitivity(g)

#  We will run a for loop to:
# i) remove one name/node at a time
# ii) get the graph with this node removed
# iii) recalculate transitivity excluding that node
# iv) compare results.


# example - remove "John", node 13
V(g)$name
V(g)$name[13]

ids <- setdiff(V(g)$name, V(g)$name[13]) # all nodes but not John
ids

g #18 nodes
induced_subgraph(g, ids) #John removed, 17nodes

transitivity(induced_subgraph(g, ids)) #0.54, very slightly higher than 0.50


# Repeat for All....

# create empty list for results.
newt<-numeric(length=vcount(g))
newt

for(i in 1:vcount(g)){
  vids <- setdiff(V(g)$name, V(g)$name[i])
  newt[[i]]<-transitivity(induced_subgraph(g, vids))
}

newt # shows the transitivity of the network with each node removed 
     # (e.g., 0.5625 is the transitivity of the first node removed)

summary(newt)
quantile(newt,.05)
quantile(newt,.95)
transitivity(g)

# 0.50 [0.44, 0.56]  # could describe transitivity with these CIs




### How to import Shakespeare data ----

# This shows how frequently various characters of Shakespeare plays coccur in acts in each play
# The networks are weighted and undirected.
# The data are a list of named matrices.

data <- readRDS("data/bard_nets.RData")
data
names(data)
lapply(data, rownames)
lapply(data, function(x) x[1:4,1:5])

#e.g. Let's look at The Tempest.  It's matrix 31.

data[[31]]

s31 <- graph_from_adjacency_matrix(data[[31]], mode="undirected", weighted=T)
s31
plot(s31)
transitivity(s31)
average.path.length(s31)

