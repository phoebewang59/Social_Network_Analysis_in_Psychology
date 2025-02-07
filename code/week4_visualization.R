
## igraph visualizations



## A very good reference for igraph plotting:
# http://kateto.net/network-visualization
?igraph.plotting # help guide



## igraph visualization

## example dataset:
library(igraph)

friends1_nodes <- read.csv("data/friends1_nodes.csv", stringsAsFactors = F)
friends1_edges <- read.csv("data/friends1_edges.csv", stringsAsFactors = F)
g <- graph_from_data_frame(d = friends1_edges, vertices = friends1_nodes, directed = FALSE)
g
V(g)$name
V(g)$gender
E(g)$hours


# Default Plot
plot(g)



### Vertices ----


# Plot network and color vertices by gender
V(g)$color <- ifelse(V(g)$gender == "F", "orange", "#aabedd")
plot(g, 
     vertex.label.color = "black") # also change vertex label color


# Change border color of vertices
plot(g, 
     vertex.label.color = "black",
     vertex.frame.color="red") 


# no border
plot(g, 
     vertex.label.color = "black",
     vertex.frame.color=NA) 


# frame color by group
V(g)$frame.color <- ifelse(V(g)$gender == "F", "red", "navy")
plot(g, 
     vertex.label.color = "black") 



# change shape of node
shapes()
# "circle", "square", "csquare", "rectangle", "crectangle", "vrectangle", "pie", "sphere", "none"
shapes <- rep("circle", gorder(g))
shapes

plot(g, 
     vertex.label.color = "black",
     vertex.shape=shapes) 

shapes <- rep("csquare", gorder(g))
shapes

plot(g, 
     vertex.label.color = "black",
     vertex.shape=shapes) 

V(g)$shape <- ifelse(V(g)$gender == "F", "csquare", "circle")
plot(g, 
     vertex.label.color = "black")




# change size of shapes


plot(g, 
     vertex.label.color = "black",
     vertex.size=25)


plot(g, 
     vertex.label.color = "black",
     vertex.size=5)

plot(g, 
     vertex.label.color = "black",
     vertex.size=15)





#### Labels... ----


# Add custom label and alter size of label
plot(g, 
     vertex.label.color = "black",
     vertex.label=LETTERS[1:gorder(g)], 
     vertex.label.cex=.6) 


# remove labels
plot(g, 
     vertex.label=NA)
     

# change font of labels
plot(g, 
     vertex.label.color = "black",
     vertex.label=LETTERS[1:gorder(g)], 
     vertex.label.cex=1.2,
     label.font=5) # try change this 1:5





#### Edges ----
dev.off() # clears all plots

# edge weights

plot(g, 
     vertex.label.color = "black",
     edge.width = E(g)$hours)


# can set linetypes too

plot(g, 
     vertex.label.color = "black",
     edge.lty = 1 )


plot(g, 
     vertex.label.color = "black",
     edge.lty = 3)   # try 0:5





## Directed edges examples ...
  
library(igraph)
measles <- read.csv("data/measles.csv")
g <- graph_from_data_frame(measles, directed=T)
g

# Is the graph directed?
is.directed(g)


# Make a basic plot
plot(g, 
     vertex.label.color = "black")

# adjust arrow size and edge color
plot(g, 
     vertex.label.color = "black", 
     edge.color = 'gray77',
     edge.arrow.size = 0.1)

# decrease node size and label size
plot(g, 
     vertex.label.color = "black", 
     edge.color = 'gray22',
     vertex.size = 0,
     edge.arrow.size = 0.1,
     vertex.label.cex=.8)



# Calculate betweenness of each vertex
g.b <- betweenness(g, directed = TRUE)
g.b

# remove labels and increase node size based on betweenness
plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     edge.arrow.size = 0.1)

# change curvature of arrows
plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     edge.arrow.size = 0.05,
     edge.curved=.2)


# alternatively, state if desire curved or not
plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     edge.arrow.size = 0.05,
     edge.curved=F)

plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     edge.arrow.size = 0.05,
     edge.curved=T)




  
#### Layouts ----

set.seed(17)
gx <- sample_gnp(40,.05) # 40 nodes, 0.5 probability
gx


## there are  many, many custom layouts...
plot(gx, vertex.label.color = "black")
plot(gx, vertex.label.color = "black", layout = layout_in_circle(gx)) # circle layout
plot(gx, vertex.label.color = "black", layout = layout_with_fr(gx)) # Fruchterman-Reingold layout 
plot(gx, vertex.label.color = "black", layout = layout_on_grid(gx)) # grid layout
plot(gx, vertex.label.color = "black", layout = layout_as_tree(gx)) # tree layout
plot(gx, vertex.label.color = "black", layout = layout_nicely(gx)) # igraph decides


# rerunning some algorithms several times leads to slightly different layouts



### Advanced - layout control....

# You may also wish to specify exact x,y coordinates. 
# e.g. if you have spatial coordinates that animals spend most time in
# you could overlay the observed network over these points.

# One very useful feature is that we can set layouts based on one graph to use with others
# if they possess  the same nodes....


# example
set.seed(17)
gf1 <- sample_forestfire(40,.1)
gf2 <- sample_forestfire(40,.1)
gf3 <- sample_forestfire(40,.1)
gf1

layoutg <- layout_nicely(gf1)

par(mfrow=c(2,3))
plot(gf1, layout = layoutg, edge.arrow.size=.02, edge.color='gray21')
plot(gf2, layout = layoutg, edge.arrow.size=.02, edge.color='tomato')
plot(gf3, layout = layoutg, edge.arrow.size=.02, edge.color='dodgerblue')
plot(gf1, edge.arrow.size=.02, edge.color='gray21')
plot(gf2, edge.arrow.size=.02, edge.color='tomato')
plot(gf3, edge.arrow.size=.02, edge.color='dodgerblue')
par(mfrow=c(1,1))




#### Bonus:  using ggraph.   A brief guide:



