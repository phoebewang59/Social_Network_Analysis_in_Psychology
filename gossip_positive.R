library(igraph)
library("viridis")

gossip <- readRDS("data/gossip_crew.RData")

# threshold matrix
gossip$positive[gossip$positive <= 2] <- 0
gossip$positive[gossip$positive > 2] <- 1
gossip$positive

g_pos <- graph_from_adjacency_matrix(gossip$positive, mode = "directed", weighted = T)
g_pos <- set_vertex_attr(g_pos, "gender", value = gossip$attributes$Gender)
g_pos

V(g_pos)$color <- ifelse(V(g_pos)$gender == "female", "lightyellow", "pink")
btwn_frame_color <- ifelse(betweenness(g_pos) > 100, "dodgerblue", NA)
btwn_frame_width <- ifelse(betweenness(g_pos) > 10, 3, 1)
# Adjust node size based on betweenness centrality
g_pos.b <- betweenness(g_pos, directed = TRUE)

plot(g_pos, 
     vertex.label.color = "black",
     vertex.frame.color = btwn_frame_color,
     vertex.frame.width = btwn_frame_width,
     vertex.size = sqrt(g_pos.b)+1,
     vertex.label.cex = 0.8,
     edge.color = "grey",
     edge.arrow.size = 0.1,
     layout = layout_nicely(g_pos))

##################
# Make an ego graph
g_M20 <- make_ego_graph(g_pos, diameter(g_pos), nodes = 'M20', mode = c("all"))[[1]]
g_M20

# Get a vector of geodesic distances of all vertices from vertex M20
dists <- distances(g_M20, "M20")
dists

# Create a color palette of length equal to the maximal geodesic distance plus one.
colors <- viridis(6, option = "H")


# Set color attribute to vertices of network M20.
V(g_M20)$color <- colors[dists+1]
  

gender_frame <- ifelse(V(g_M20)$gender == "female", "orange", "darkred")


plot(g_M20, 
     vertex.label = NA, 
     vertex.label.color = "black",
     vertex.label.cex = .6,
     vertex.frame.color = gender_frame,
     vertex.frame.width = 3,
     edge.color = 'black',
     vertex.size = 10,
     edge.arrow.size = .1,
     main = "Geodesic Distances from M20"
)

legend(x = -2.45, y = 1.1, legend = unique(V(g_M20)$gender), bty = "n", fill = unique(gender_frame), title = "Gender (vertex frame color)", cex = 0.8)
legend(x = -2.35, y = 0.1, legend = unique(sort(as.vector(dists), decreasing = F)), bty = "n", fill = colors, title = "Geodesic Distance", cex = 0.8)

