#### Data Visualization Tutorial using ggraph.   A brief guide:

# Use friends datasets again

friends1_nodes <- read.csv("data/friends1_nodes.csv", stringsAsFactors = F)
friends1_edges <- read.csv("data/friends1_edges.csv", stringsAsFactors = F)
g <- graph_from_data_frame(d = friends1_edges, vertices = friends1_nodes, directed = FALSE)
g
V(g)$name
V(g)$gender
E(g)$hours


# Load libraries
library(igraph)
library(tidygraph)
library(ggraph)
library(dplyr)

# Step 1: Convert to a tbl_graph (tidygraph object)
g_tidy <- as_tbl_graph(g)
g_tidy

# Step 2: Basic Network Plot
ggraph(g_tidy, layout = "kk") + 
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1.5) +
  ggtitle("Basic Network Plot") +
  theme_minimal()

# Step 3: Change Node Colors Based on Gender
ggraph(g_tidy, layout = "kk") + 
  geom_edge_link() + 
  geom_node_point(aes(color = gender), size = 6) + 
  geom_node_text(aes(label = name), vjust = 1.5) +
  scale_color_manual(values = c("F" = "#fde1c2", "M" = "lightblue")) +
  ggtitle("Node Colors by Gender") +
  theme_void()

# Step 4: Change Node Size Based on Degree (Number of Connections)
# also removing legend
# also changing layout to F-R
ggraph(g_tidy, layout = "fr") + 
  geom_edge_link() + 
  geom_node_point(aes(size = centrality_degree()), color = "steelblue") + 
  geom_node_text(aes(label = name), vjust = 1.5) +
  ggtitle("Node Size Proportional to Degree") +
  theme_void() +
  theme(legend.position='none')

# Step 5: Change Edge Thickness Based on Hours Spent Together
ggraph(g_tidy, layout = "fr") + 
  geom_edge_link(aes(width = hours), color = "grey40") + 
  geom_node_point(aes(color = gender), size = 6) + 
  geom_node_text(aes(label = name), vjust = 1.5) +
  scale_color_manual(values = c("F" = "#fde1c2", "M" = "lightblue")) +
  ggtitle("Edge Thickness Proportional to Hours") +
  theme_void() +
  theme(legend.position='none')

# Step 6: Change Edge Styles (Dashed, Dotted, Solid) Based on Hours
g_tidy <- g_tidy %>% 
  activate(edges) %>% 
  mutate(edge_style = case_when(
    hours > 5 ~ "solid",
    hours > 3 ~ "dashed",
    TRUE ~ "dotted"
  ))

ggraph(g_tidy, layout = "fr") + 
  geom_edge_link(aes(linetype = edge_style, width = hours), color = "grey40") + 
  geom_node_point(aes(color = gender), size = 6) + 
  geom_node_text(aes(label = name), vjust = 1.5) +
  scale_color_manual(values = c("F" = "#fde1c2", "M" = "lightblue")) +
  ggtitle("Edge Styles Based on Hours") +
  theme_void() +
  theme(legend.position='none')


# Step 7: Change Node Borders and Label Styles
ggraph(g_tidy, layout = "fr") + 
  geom_edge_link(aes(width = hours), color = "grey40") + 
  geom_node_point(aes(color = gender), size = 6, stroke = 2, shape = 21, fill = "white") + 
  geom_node_text(aes(label = name, color = gender), vjust = 1.5, fontface = "bold") +
  scale_color_manual(values = c("F" = "#fde1c2", "M" = "lightblue")) +
  ggtitle("Customized Node Borders and Labels")  +
  theme_void() +
  theme(legend.position='none')

# Step 8: Different Layouts
ggraph(g_tidy, layout = "circle") + 
  geom_edge_link(aes(width = hours), color = "grey40") + 
  geom_node_point(aes(color = gender), size = 6) + 
  geom_node_text(aes(label = name), vjust = 1.5) +
  ggtitle("Circular Layout")  +
  theme_void() +
  theme(legend.position='none')

ggraph(g_tidy, layout = "fr") + 
  geom_edge_link(aes(width = hours), color = "grey40") + 
  geom_node_point(aes(color = gender), size = 6) + 
  geom_node_text(aes(label = name), vjust = 1.5) +
  ggtitle("Fruchterman-Reingold Layout")  +
  theme_void() +
  theme(legend.position='none')

ggraph(g_tidy, layout = "kk") + 
  geom_edge_link(aes(width = hours), color = "grey40") + 
  geom_node_point(aes(color = gender), size = 6) + 
  geom_node_text(aes(label = name), vjust = 1.5) +
  ggtitle("Kamada-Kawai Layout")  +
  theme_void() +
  theme(legend.position='none')




