library(igraph)
library(viridis)

m_edgelist <- read.csv("data/hero-network.csv")

m_edgelist.mat <- as.matrix(m_edgelist)
gm <- graph_from_edgelist(m_edgelist.mat, directed = F)

# define new names
new_names <- c(
  "IRON MAN/TONY STARK " = "Iron Man",
  "CAPTAIN AMERICA" = "Captain America",
  "THOR/DR. DONALD BLAK" = "Thor",
  "HULK/DR. ROBERT BRUC" = "Hulk",
  "BLACK WIDOW/NATASHA " = "Black Widow",
  "HAWKEYE DOPPELGANGER" = "Hawkeye",
  "SPIDER-MAN/PETER PAR" = "Spider-Man",
  "DR. STRANGE/STEPHEN " = "Dr. Strange",
  "BLACK PANTHER/T'CHAL" = "Black Panther",
  "SCARLET WITCH/WANDA " = "Scarlet Witch",
  "VISION " = "Vision"
)


# only picking the main characters from Avengers

gm_avengers <- induced_subgraph(gm, V(gm)$name %in% c("IRON MAN/TONY STARK ",
                                       "CAPTAIN AMERICA",
                                       "THOR/DR. DONALD BLAK",
                                       "HULK/DR. ROBERT BRUC",
                                       "BLACK WIDOW/NATASHA ",
                                       "HAWKEYE DOPPELGANGER",
                                       "SPIDER-MAN/PETER PAR",
                                       "DR. STRANGE/STEPHEN ",
                                       "BLACK PANTHER/T'CHAL",
                                       "SCARLET WITCH/WANDA ",
                                       "VISION "))
avengers.mat <- as.matrix(gm_avengers)
gm_a <- graph_from_adjacency_matrix(avengers.mat, mode = "undirected", weighted = T)
V(gm_a)$name <- new_names[V(gm_a)$name]

# color by degree
col <- viridis(max(degree(gm_a,mode="all"))+1) 
col <- col[degree(gm_a, mode="all")+1]

# color by betweenness
btwn_frame <- ifelse(betweenness(gm_a) > 0, "darkorange", NA)
btwn_frame_width <- ifelse(betweenness(gm_a) > 0, 5, 1)

plot(gm_a, 
     vertex.color=col,
     edge.arrow.size=.1,
     vertex.size= 13 + (betweenness(gm_a)),
     vertex.frame.color = btwn_frame,
     vertex.frame.width = btwn_frame_width,
     vertex.label.cex=.7,
     vertex.label.color="black", 
     layout = layout_nicely(gm_a),
     main = "Degree Centralities of the Main Avengers Characters")
