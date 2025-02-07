
#### Types of Network Data - brief introduction ----



### 1.1 Importing graphs and switching between data types ----

library(igraph) # the package we'll use


###  Edgelist to Graph Object


# Inspect the first few rows of the dataframe 'friends'
friends <- read.csv("data/friends.csv")
head(friends)

# Convert friends dataframe to a matrix
friends.mat <- as.matrix(friends)
friends.mat

# Convert friends matrix to an igraph object
g <- graph.edgelist(friends.mat, directed = FALSE)  # old version of running edgelist
g <- graph_from_edgelist(friends.mat, directed = FALSE)  # denote if directed



g  # let's look at what the graph object is


# Look at four letters at the top:
#     D / U, Directed vs undirected graph
#     N      Named graph 
#     W      Weighted graph 
#     B      Bipartite (two-mode) graph 

# first number = number of nodes
# second number = number of edges
# details about attributes of nodes and edges


class(g)

summary(g)

# Make a very basic plot of the network
plot(g)



### Can make graph object straight from dataframe

friends
g <- graph_from_data_frame(friends, directed=F)
g


## Useful thing about 'graph_from_data_frame' is you can add edge attributes e.g.weight

friends_w <- read.csv("data/friends_w.csv")
head(friends_w)

gw <- graph_from_data_frame(friends_w, directed=F)
gw
summary(gw)
plot(gw)


## Data may also be in an adjacency matrix

# binary matrix
friends_mat <- read.csv("data/friends_mat.csv")
friends_mat

gM <- graph_from_adjacency_matrix(as.matrix(friends_mat), mode=c("undirected"))
gM


# valued matrix
friends_w_mat <- read.csv("data/friends_w_mat.csv")
friends_w_mat

gWM <- graph_from_adjacency_matrix(as.matrix(friends_w_mat), mode=c("undirected"),weighted=T)
gWM





## Graph objects can also be converted to matrices, edgelists or dataframes

gw[]   # gives adjacency matrix (old way of getting the matrix)
get.adjacency(gw)
get.adjacency(gw,sparse = F)
as_adjacency_matrix(gw, attr="weight")

as_edgelist(gw, names=T)

as_data_frame(gw, what="edges")
as_data_frame(gw, what="vertices")



# Subset vertices and edges
V(g)
E(g)

names(V(g)) # to get names e.g. if need to use 'match'

# Count number of edges
gsize(g)
ecount(g)

# Count number of vertices/nodes
gorder(g)
vcount(g)



## import some attributes
friends_attr <- read.csv("data/friends_attr.csv")

# Create new vertex attribute called 'gender'
g <- set_vertex_attr(g, "gender", value = friends_attr$gender)

# Create new vertex attribute called 'age'
g <- set_vertex_attr(g, "age", value = friends_attr$age)

# View all vertex attributes in a list
vertex_attr(g)


# View edge attributes of graph object
gw

edge_attr(gw)

# Create new edge attribute called 'hours'
g <- set_edge_attr(g, "hours", value = friends_w$weight)


# View graph attributes
graph_attr(gw)


## can also import node and edge dataframes at same time with all attributes in them
gg <- graph_from_data_frame(friends_w, directed = F, vertices = friends_attr)
gg




### 1.2 subsetting graphs ----


# View attributes of first five vertices in a dataframe
V(g)[[1:5]] 


# Find all edges that include "Britt"
E(g)[[.inc('Britt')]]  


# Find all pairs that spend 4 or more hours together per week
E(g)[[hours>=4]]  


# also directly create new attributes
# Plot network and color vertices by gender
V(g)$color <- ifelse(V(g)$gender == "F", "orange", "dodgerblue")
plot(g, vertex.label.color = "black")

# Plot the graph with node coloring
node_colors <- c("F" = "darkorange", "M" = "blue")
vertex_color <- node_colors[vertex_attr(g, "gender")]
plot(
  g, vertex.label.color = "black",
  vertex.color = vertex_color
)


# attributes directly
V(g)$age
V(g)$gender
E(g)$hours


# Single brackets to get rows/cols/cells of the network matrix:
g[] # gets the whole matrix
g[1,]
g[,5]
g[5,7]


# Is the graph directed?
is_directed(g)

# Is the graph weighted?
is.weighted(g)
is.weighted(gw)

# Where does each edge originate from?
head_of(g, E(g))
table(head_of(g, E(g)))


# Extract edges
edges <- ends(gw, E(gw)) # Get node pairs for edges

# Extract weights
weights <- E(gw)$weight

# Combine into a data frame
edge_list <- data.frame(from = edges[, 1], to = edges[, 2], weight = weights)
edge_list


#### 1.3 Thresholding matrices: 


whales <- read.csv("data/whales.csv")
whales

# only keep relationships that are > 0.1 association
whales[whales<0.1]<-0
whales[whales>=0.1]<-1
whales

w <- graph_from_adjacency_matrix(as.matrix(whales), mode=c("undirected"))
w

plot(w)





#### PRACTICE QUESTIONS ----

## EXAMPLE 1.  COMMUNICATION NETWORK

#' i) Import the 'communication.csv' file that shows the number of messages
#' sent between different individuals in a business.
#' 
#' ii) Convert these data into a graph object.
#' 
#' iii) Add an edge attribute called 'messages' that relates to the number of 
#' messages sent between each pair of individuals.
#' 
#' iv) Convert the igraph object into an adjacency matrix based on the number
#' of messages
#' 
#' v) Import the 'communication_nodes.csv' file that contains information on 
#' the department that each individual belongs to.
#' 
#' vi) Add a vertex attribute called 'department' that details which department
#' each node belongs to.
#' 
#' vii) Subset the edges of 'Person 5'.  How many messages to they send to 
#' other individuals, and how many do they receive? 
#' 
#' viii) Plot the network coloring nodes by department.



## EXAMPLE 2. 


#' i) Import the dataset 'barbary.csv'. What sort of data format is this ?
#' 
#' 
#' ii) Threshold these data to assign a 0 if cells contain values less than 25,
#' and a 1 if they are 25 or greater.
#' 
#' iii) Create an igraph object from these data.  Are the data undirected and/or weighted?
#' 
#' iv) How would you inspect this graph object as an edgelist?
#' 
#' v) Import the 'barbary_nodes.csv' dataset. Use this to add node attributes: age_sex
#' to the graph object.
#' 
#' vi) Plot the graph coloring the nodes by age_sex. 
#' What do you notice about the network?
#' What might happen with different thresholds?
#' 
#' vii) Optional:  filter the adjacency matrix (directly from graph object) to
#' only include juvenile animals.
#' 
#' 





### Other ways of creating graphs in igraph: ----

rm(list = ls()) # Remove everything from environment
dev.off() # remove plots


## Manually (You almost never will need this):

graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F ) 
graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=T ) 


g1 <- graph( c("A", "B", "B", "C", "C", "D")) # named vertices
plot(g1)

g2 <- graph( c("A", "B", "B", "C", "C", "D", "D", "E"), isolates=c("F", "G", "H", "I") )  
plot(g2)

plot(graph_from_literal(a---b, b---c)) 
plot(graph_from_literal(a--+b, b+--c))
plot(graph_from_literal(a+-+b, b+-+c)) 
plot(graph_from_literal(a:b:c---c:d:e))
graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j)
plot(graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j))


## graph models ... ----

# these are specific  types of network graphs  that igraph will build:


g <- make_empty_graph(10)
g
plot(g)

g <- make_full_graph(10)
g
plot(g)

g <- make_star(10)
g
plot(g)

g <- make_tree(10, children = 3, mode = "undirected")
g
plot(g)


g <- make_ring(10)
g
plot(g)


g <- sample_gnm(n=30, m=25) #erdos-renyi
g
plot(g)

g <- sample_gnp(n=30, p=.15)
g
plot(g)

g <- sample_smallworld(dim=2, size=10, nei=1, p=0.1)
g
plot(g)

g <- sample_pa(n=40, power=1, m=1,  directed=F) #Barabasi-Albert preferential attachment model
g
plot(g)



## Built in datasets.... ----

zach <- graph("Zachary") # the Zachary carate club
plot(zach, vertex.size=10, vertex.label=NA)


library(igraphdata)
data(package = "igraphdata")
