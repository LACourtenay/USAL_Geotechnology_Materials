# Load libraries ------------------------

# install.packages("tibble") - Example of installing packages from CRAN
# install.packages(c("tibble","tidyverse")) - Example of installing multiple packages from CRAN
# remotes::install_github("Nowosad/spDataLarge") - Example of installing packages from GitHub

# Load data libraries

library(tibble)
library(plyr)
library(dplyr)
library(tidyverse)

# Load spatial libraries

library(sf) # Simple Features Package for transferable information
library(spDataLarge)
library(spData)
library(ggspatial)
library(sp)

# Load Visualisation Libraries

library(ggplot2)
library(gridExtra)
library(RColorBrewer)

# Load Network Libraries

library(igraph)

# Set enviornment

setwd("C:\\Users") # Set directory where files are saved

# Create creative colour function

add.alpha <- function(col, alpha=1){ # Function for setting the alpha of colours
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

#

# Load Data --------------------------

metadata<-read.delim("Node Metadata.csv", sep = ";", head = TRUE); metadata <- metadata %>% mutate_all(as.character) %>% drop_na()
location<-read.table("Node Geographic Locations.csv", sep = ";", head = TRUE); location <- location %>% mutate(id = as.character(id)) %>% drop_na()
connections<-read.table("Clean Links.csv", sep = ";", head = TRUE); connections <- connections %>%
  mutate_all(as.character) %>%
  drop_na() %>%
  unique() # For efficiency in demonstrations we will remove duplicate edges

# Create Base Maps ---------------------

# Note the distortion of simply plotting x and y coordinates in base R
plot(location$longitude, location$latitude)

# Libraries such as SP preserve geometry
data(world) # Load world database

# Plot all categories within world and summarise
plot(world)
summary(world)
summary(world["pop"])
plot(world["pop"])

# Plot world preserving geometry
plot(world["geom"])

# Plot specific countries and filter
africa = st_union(world[world$continent == "Africa",])
plot(world["geom"], reset = FALSE)
plot(africa, add = TRUE, col = "red")

# Customise map projections

ggplot() + geom_sf(data = st_as_sf(world["geom"][world$continent == "Africa", ]),
                   fill = "gray90") +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()

ggplot() + geom_sf(data = st_as_sf(world["geom"][world$continent == "Africa", ] %>%
                                     lwgeom::st_transform_proj(crs = "+proj=moll")),
                   fill = "gray90") +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()

ggplot() + geom_sf(data = st_as_sf(world["geom"][world$continent == "Africa", ] %>%
                                     st_transform(crs = "+proj=laea")),
                   fill = "gray90") +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()
  
# Africa ------------

# African Nodes

africa_db<-join(location, metadata, by = "id") %>%
  select(-Label, -Country) %>%
  filter(Continent == "AF") %>%
  select(-Continent) %>% filter(id != "CFF" & id !=  "ANJ" & id != "ASI" & id != "CCE" & id != "SHO")
location_list = c()
for (i in 1:nrow(africa_db)) {location_list = rbind(location_list, c(africa_db[i,3], africa_db[i,2]))}
location_geometry<-st_sfc(st_multipoint(location_list), crs = "+proj=longlat +datum=WGS84")
plot(location_geometry, pch = 16, axes = TRUE, cex = 0.5)

africa = st_union(world[world$continent == "Africa", ])
plot(africa, axes = TRUE, col = "darkgrey")
plot(location_geometry, pch = 16, add = TRUE, cex = 0.5)

# African Edges

africa_connections<-connections %>%
  mutate(source_continent = metadata$Continent[match(x = connections$Source,
                                                     table = metadata$id)]) %>%
  mutate(target_continent = metadata$Continent[match(x = connections$Target,
                                                     table = metadata$id)]) %>%
  filter(source_continent == "AF" & target_continent == "AF") %>%
  select(Source, Target)
africa_connections<-africa_connections %>%
  mutate(source_long = africa_db$longitude[match(x = africa_connections$Source,
                                                 table = africa_db$id)]) %>%
  mutate(source_lat = africa_db$latitude[match(x = africa_connections$Source,
                                               table = africa_db$id)]) %>%
  mutate(target_long = africa_db$longitude[match(x = africa_connections$Target,
                                                 table = africa_db$id)]) %>%
  mutate(target_lat = africa_db$latitude[match(x = africa_connections$Target,
                                               table = africa_db$id)])
source = c(); target = c(); edges = st_sf(geom = st_sfc(), crs = "+proj=longlat +datum=WGS84")
for(i in 1:nrow(africa_connections)) {source = rbind(source, c(africa_connections[i,3], africa_connections[i,4]))}
for(i in 1:nrow(africa_connections)) {target = rbind(target, c(africa_connections[i,5], africa_connections[i,6]))}
for(i in 1:nrow(africa_connections)) {
  edge = st_sfc(st_linestring(rbind(source[i,], target[i,])),crs = "+proj=longlat +datum=WGS84")
  edge = st_sf(geom = edge)
  edges = rbind(edges, edge)
}

# Plot African Network

plot(africa, axes = TRUE, col = "darkgrey")
plot(edges, col = "red", add = TRUE, lwd = 0.5)
plot(location_geometry, pch = 16, add = TRUE, cex = 0.5)

# General Network Plots --------------------

# ggplot

total_plot<-ggplot() + geom_sf(data = st_as_sf(world["geom"][world$continent == "Africa", ]),
                               fill = "gray90")
total_plot<-total_plot + geom_sf(data = edges, color = "red", size = 0.5) + geom_sf(data = location_geometry)
total_plot + theme_bw()

# Plot with scale bar and compass

total_plot +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()

# plot with pale blue see colour

total_plot +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = "gray60", linetype = "dashed", size = 0.25), 
        panel.background = element_rect(fill = "aliceblue"))


# customise map projection

new_edges<-st_transform(edges, crs = "+proj=laea")
new_nodes<-st_transform(location_geometry, crs = "+proj=laea")

ggplot() +
  geom_sf(data = st_as_sf(world["geom"][world$continent == "Africa", ] %>%
                            st_transform(crs = "+proj=laea")),
          fill = "gray90") +
  geom_sf(data = edges, color = "red", size = 0.5) +
  geom_sf(data = new_nodes) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()

# Network Analysis (Africa) ---------------------

# Create Graph Object

nodes<-data.frame(id = africa_db$id, x = africa_db$longitude, y = africa_db$latitude)
g<-graph_from_data_frame(vertices = nodes, d = as.matrix(africa_connections[,1:2]), directed = TRUE)

# Remove isolates if necessary

iso<-which(degree(g)==0)
g<-delete.vertices(g, iso)

# Graph Visualisation Examples

plot(g, vertex.size = 2, edge.arrow.size = 0.1, vertex.color = "red", vertex.label = NA,
     layout = layout_in_circle)
plot(g, vertex.size = 2, edge.arrow.size = 0.1, vertex.color = "red", vertex.label = NA,
     layout = layout_with_graphopt)
plot(g, vertex.size = 2, edge.arrow.size = 0.1, vertex.color = "red", vertex.label = NA,
     layout = layout_with_mds)
plot(g, vertex.size = 2, edge.arrow.size = 0.1, vertex.color = "red", vertex.label = NA,
     layout = layout_with_kk)
plot(g, vertex.size = 2, edge.arrow.size = 0.1, vertex.color = "red", vertex.label = NA,
     layout = layout_with_fr)

# Visualise Graph with geospatial information

plot(g, vertex.size = 2, edge.arrow.size = 0.1,
     vertex.color = "red", vertex.label = NA)

# Graph analysis -------------------------------

# Density - Proportion of present edges from all possible edges in the network
edge_density(g, loops = FALSE)
ecount(g)/(vcount(g)*(vcount(g)-1)) # for directed networks

# Transitivity - Ratio of traingles to connected tripples
transitivity(g, type = "global")


# Degree
deg<-degree(g, mode = "all")
plot(g, vertex.size = deg * 0.1,
     edge.arrow.size = 0.1,
     vertex.color = "red",
     edge.color = "grey",
     vertex.label = NA)

# In degree
plot(g, vertex.size = degree(g, mode = "in") * 0.25,
     edge.arrow.size = 0.1,
     vertex.color = "red",
     edge.color = "grey",
     vertex.label = NA)

# Out degree
plot(g, vertex.size = degree(g, mode = "out") * 0.25,
     edge.arrow.size = 0.1,
     vertex.color = "red",
     edge.color = "grey",
     vertex.label = NA)

plot(degree(g, mode = "in"), degree(g, mode = "out"), pch = 19, cex = 1, main = "In vs Out Degree",
     xlab = "In Degree", ylab = "Out Degree")

# Histograms
hist(deg, breaks = 1:vcount(g)-1, main = "Histogram of node degree")
hist(deg, breaks = vcount(g)/10, main = "Histogram of node degree", col = "dark grey")

# Centrality

plot(g, vertex.size = centr_degree(g, mode = "in", normalized = TRUE)$res * 0.25,
     edge.arrow.size = 0.1,
     vertex.color = "red",
     edge.color = "grey",
     vertex.label = NA)

# Closeness

plot(g, vertex.size = centr_clo(g, mode = "all", normalized = TRUE)$res * 10,
     edge.arrow.size = 0.1,
     vertex.color = "red",
     edge.color = "grey",
     vertex.label = NA)

# Eigenvector

plot(g, vertex.size = centr_eigen(g, directed = TRUE, normalized = TRUE)$vector * 10,
     edge.arrow.size = 0.1,
     vertex.color = "red",
     edge.color = "grey",
     vertex.label = NA)

sort(eigen_centrality(g)$vector, decreasing = TRUE)[1:10]
metadata %>% filter(id == "NBO")

hist(eigen_centrality(g)$vector, breaks = vcount(g)/3, main = "Histogram of Eigenvector Centrality",
     col = "dark gray")
plot(eigen_centrality(g)$vector, eigen_centrality(g)$vector, pch = 19, cex = 1)

# Betweeness

plot(g, vertex.size = centr_betw(g, directed = TRUE, normalized = TRUE)$res * 0.00075,
     edge.arrow.size = 0.1,
     vertex.color = "red",
     edge.color = "grey",
     vertex.label = NA)

sort(centr_betw(g, directed = TRUE, normalized = TRUE)$res, decreasing = TRUE)[1:10]

par(mfrow = c(1,2)); plot(g, vertex.size = centr_eigen(g,
                                                       directed = TRUE,
                                                       normalized = TRUE)$vector * 10,
                          edge.arrow.size = 0.05,
                          vertex.color = "red",
                          edge.color = "grey",
                          vertex.label = NA,
                          main = "Eigenvector"); plot(g, vertex.size = centr_betw(g,
                                                                                  directed = TRUE,
                                                                                  normalized = TRUE)$res * 0.00075,
                                                      edge.arrow.size = 0.05,
                                                      vertex.color = "red",
                                                      edge.color = "grey",
                                                      vertex.label = NA,
                                                      main = "Betweeness")

# Hubs and Authorities
par(mfrow = c(1,2))
plot(g, vertex.size = hub_score(g, weights = NA)$vector * 10,
     edge.arrow.size = 0.1,
     vertex.color = "red",
     edge.color = "grey",
     vertex.label = NA, main = "Hub")
plot(g, vertex.size = authority_score(g, weights = NA)$vector * 10,
     edge.arrow.size = 0.1,
     vertex.color = "red",
     edge.color = "grey",
     vertex.label = NA, main = "Authority")
par(mfrow = c(1,1))

plot(hub_score(g, weights = NA)$vector, authority_score(g, weights = NA)$vector, pch = 19, cex = 1,
     xlab = "Hub", ylab = "Authority")

# Diameter - longest geodesic distance (Length of the shortest path between two nodes)
diameter(g, directed = TRUE, weights = NA)
get_diameter(g, directed = TRUE) # with metadata %>% filter(id == "ABS") we can consult where these nodes are

# Plot the diameter
diam<-get_diameter(g, directed = TRUE)
vcol = rep("gray40", vcount(g)); vcol[diam]<-"gold"
ecol<-rep("gray80",ecount(g)); ecol[E(g, path = diam)]<-"orange"
esize<-rep(0.1, ecount(g));esize[E(g, path = diam)]=2.5
plot(g, vertex.size = 2.5,
     edge.width = esize,
     edge.arrow.size = 0.1,
     vertex.color = vcol,
     edge.color = ecol,
     vertex.label = NA)

# distances

mean_distance(g, directed = TRUE)
distances(g, weights = NA)

# plotting distance from a specific node

# first we can calculate the most important nodes
sort(eigen_centrality(g)$vector, decreasing = TRUE)[1:10]
sort(eigen_centrality(g)$vector, decreasing = FALSE)[1:10]

dist_from_NBO<-distances(g, v = V(g)["NBO"], to = V(g), weights = NA)
oranges<-colorRampPalette(c("dark red", "gold"))
col<-oranges(4+1)
col<-col[dist_from_NBO+1]
plot(g, vertex.size = 2.5,
     edge.arrow.size = 0.1,
     vertex.color = col,
     edge.color = "gray",
     vertex.label = NA)

# Shortest path
shortest_paths(g, from = V(g)["NBO"], to = V(g)["FMI"], output = "both")

path<-shortest_paths(g, from = V(g)["NBO"], to = V(g)["MZW"], output = "both")
ecol <- rep("gray80", ecount(g)); ecol[unlist(path$epath)] <- "orange"
ew <- rep(0.1, ecount(g)); ew[unlist(path$epath)] <- 4
vcol <- rep("gray40", vcount(g)); vcol[unlist(path$vpath)] <- "gold"

plot(g, vertex.size = 2.5,
     edge.arrow.size = 0.1,
     vertex.color = vcol,
     edge.color = ecol,
     edge.width = ew,
     vertex.label = NA)

# In and out of a specific node

conected_nodes<-incident(g, V(g)["NBO"], mode = "all")
ecol <- rep("gray80", ecount(g)); ecol[conected_nodes] <- "orange"
vcol <- rep("gray40", vcount(g)); vcol[V(g)["NBO"]] <- "gold"
neighbor_nodes<-neighbors(g, V(g)["NBO"], mode = "out")
vcol[neighbor_nodes]<-"#ff9d00"
plot(g, vertex.size = 2.5,
     edge.arrow.size = 0.1,
     vertex.color = vcol,
     edge.color = ecol,
     edge.width = 0.1,
     vertex.label = NA)

# Clustering and Community Detection

# Communities

ceb<-cluster_edge_betweenness(as.undirected(g))
dendPlot(ceb, mode = "hclust")
plot(ceb, g, vertex.size = 2.5,
     edge.arrow.size = 0.1,
     vertex.color = "black",
     edge.color = "grey",
     edge.width = 0.1,
     vertex.label = NA)

ceb<-cluster_label_prop(as.undirected(g))
plot(ceb, g, vertex.size = 2.5,
     edge.arrow.size = 0.1,
     vertex.color = "black",
     edge.color = "grey",
     edge.width = 0.1,
     vertex.label = NA)

ceb<-cluster_fast_greedy(as.undirected(g))
plot(ceb, g, vertex.size = 2.5,
     edge.arrow.size = 0.1,
     vertex.color = "black",
     edge.color = "grey",
     edge.width = 0.1,
     vertex.label = NA)

ceb<-cluster_louvain(as.undirected(g)) # This is the same modularity algorithm as Gephy
plot(ceb, g, vertex.size = 2.5,
     edge.arrow.size = 0.1,
     vertex.color = "black",
     edge.color = "grey",
     edge.width = 0.1,
     vertex.label = NA)

#

# Combine Graph Analysis Data with Original Map --------------------------------

g<-graph_from_data_frame(vertices = nodes, d = as.matrix(africa_connections[,1:2]), directed = TRUE)

eigen_data<-centr_eigen(g, directed = TRUE, normalized = TRUE)$vector
between_data<-centr_betw(g, directed = TRUE, normalized = TRUE)$res
eigen_data<-nodes %>% mutate(Eigen_Centrality = eigen_data, Between_Centrality = between_data)
eigen_spat = st_sf(id = character(),
                   eigen = numeric(), between = numeric(),
                   geom = st_sfc(),
                   crs = "+proj=longlat +datum=WGS84")
for(i in 1:nrow(eigen_data)) {
  point = st_sfc(st_point(cbind(eigen_data[i,2],eigen_data[i,3])))
  point = st_sf(id = eigen_data[i,1],
                eigen = eigen_data[i,4], between = eigen_data[i,5],
                geom = point,
                crs = "+proj=longlat +datum=WGS84")
  eigen_spat = rbind(eigen_spat, point)
}
eigen_spat<-st_transform(eigen_spat, crs = "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

ggplot() +
  geom_sf(data = st_geometry(st_as_sf(world["geom"][world$continent == "Africa", ] %>%
                                        st_transform(crs = "+proj=laea"))),
          fill = "gray90") +
  geom_sf(data = st_transform(edges,
                              crs = "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"),
          size = 0.1, color = "red") +
  geom_sf(data = eigen_spat, aes(geometry = geom, size = eigen)) +
  scale_size_continuous(range = c(0.01, 7), trans = "exp") +
  theme_bw() +
  labs(title = "Eigenvector Centrality") +
  theme(legend.position = "none", panel.grid.major = element_line(color = "gray")) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)

grid.arrange(ggplot() +
               geom_sf(data = st_geometry(st_as_sf(world["geom"][world$continent == "Africa", ] %>%
                                                     st_transform(crs = "+proj=laea"))),
                       fill = "gray90") +
               geom_sf(data = st_transform(edges,
                                           crs = "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"),
                       size = 0.1, color = "red") +
               geom_sf(data = eigen_spat, aes(geometry = geom, size = between)) +
               scale_size_continuous(range = c(0.01, 7)) +
               theme_bw() +
               labs(title = "Betweeness Centrality") +
               theme(legend.position = "none", panel.grid.major = element_line(color = "gray")) +
               annotation_scale(location = "bl") +
               annotation_north_arrow(location = "bl", which_north = "true",
                                      pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                                      style = north_arrow_fancy_orienteering),
             ggplot() +
               geom_sf(data = st_geometry(st_as_sf(world["geom"][world$continent == "Africa", ] %>%
                                                     st_transform(crs = "+proj=laea"))),
                       fill = "gray90") +
               geom_sf(data = st_transform(edges,
                                           crs = "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"),
                       size = 0.1, color = "red") +
               geom_sf(data = eigen_spat, aes(geometry = geom, size = eigen)) +
               scale_size_continuous(range = c(0.01, 7), trans = "exp") +
               theme_bw() +
               labs(title = "Eigenvector Centrality") +
               theme(legend.position = "none", panel.grid.major = element_line(color = "gray")) +
               annotation_scale(location = "bl") +
               annotation_north_arrow(location = "bl", which_north = "true",
                                      pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                                      style = north_arrow_fancy_orienteering),
             ncol = 2, nrow = 1)

ggplot() +
  geom_sf(data = st_geometry(st_as_sf(world["geom"][world$continent == "Africa", ] %>%
                                        st_transform(crs = "+proj=laea"))),
          fill = "gray90") +
  geom_sf(data = st_transform(edges,
                              crs = "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"),
          size = 0.1, color = "red") +
  geom_sf(data = eigen_spat, aes(geometry = geom, size = between)) +
  scale_size_continuous(range = c(0.01, 7)) +
  theme_bw() +
  labs(title = "Betweeness Centrality") +
  theme(legend.position = "none", panel.grid.major = element_line(color = "gray")) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)

ggplot(data = eigen_data, aes(x = x, y = y)) +
  geom_point(aes(size = Eigen_Centrality)) +
  scale_size_continuous(range = c(0.1, 5)) + theme_bw()

# Export to Gephi

write.csv(eigen_data, "Africa_Nodes.csv", row.names = FALSE)
write.csv(africa_connections[1:2], "Africa_Edges.csv", row.names = FALSE)

# Louvian Clustering Visualisation ---------------------------

# Remove isolates if necessary

iso<-which(degree(g)==0)
g2<-delete.vertices(g, iso)

louvian<-cluster_louvain(as.undirected(g2))
groups = tibble(id = character(), group = factor())
for(i in 1:length(louvian)){
  new = tibble(id = as.character(louvian[i][[1]]), group = as.factor(i))
  groups = groups %>% full_join(new)
}; groups$group = as.factor(groups$group)

modularity_data<-nodes %>% mutate(modularity = as.character(groups$group[match(x = nodes$id,
                                                                  table = groups$id)])) %>%
  mutate(id = as.character(id),
         modularity = as.factor(coalesce(modularity, "0")))


modularity_spat = st_sf(id = character(),
                        class = factor(),
                        geom = st_sfc(),
                        crs = "+proj=longlat +datum=WGS84")
for(i in 1:nrow(modularity_data)) {
  point = st_sfc(st_point(cbind(modularity_data[i,2],modularity_data[i,3])))
  point = st_sf(id = modularity_data[i,1],
                class = modularity_data[i,4],
                geom = point,
                crs = "+proj=longlat +datum=WGS84")
  modularity_spat = rbind(modularity_spat, point)
}
modularity_spat<-st_transform(modularity_spat, crs = "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

ggplot() +
  geom_sf(data = st_geometry(st_as_sf(world["geom"][world$continent == "Africa", ] %>%
                                        st_transform(crs = "+proj=laea"))),
          fill = add.alpha("gray90", alpha = 0.8)) +
  geom_sf(data = st_transform(edges,
                              crs = "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"),
          size = 0.005, color = "grey") +
  geom_sf(data = modularity_spat %>%
            filter(class == "0"),
          aes(geometry = geom), size = 1) +
  geom_sf(data = modularity_spat %>%
            filter(class != "0"),
          aes(geometry = geom, colour = as.numeric(class)), size = 4) +
  scale_color_distiller(palette = "Dark2") +
  theme_bw() +
  labs(title = "Louvain Modularity Class") +
  theme(legend.position = "none", panel.grid.major = element_line(color = "gray")) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)

# Cluster and Eigenvalues

mod_eig_spat<-modularity_spat %>% mutate(eig = as.numeric(eigen_data$Eigen_Centrality[match(x = modularity_spat$id,
                                                                                            table = eigen_data$id)]),
                                         bet = as.numeric(eigen_data$Between_Centrality[match(x = modularity_spat$id,
                                                                                            table = eigen_data$id)]))
ggplot() +
  geom_sf(data = st_geometry(st_as_sf(world["geom"][world$continent == "Africa", ] %>%
                                        st_transform(crs = "+proj=laea"))),
          fill = add.alpha("gray90", alpha = 0.8)) +
  geom_sf(data = st_transform(edges,
                              crs = "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"),
          size = 0.005, color = "grey") +
  geom_sf(data = modularity_spat %>%
            filter(class == "0"),
          aes(geometry = geom), size = 1) +
  #geom_sf(data = eigen_spat, aes(geometry = geom, size = eigen)) +
  geom_sf(data = mod_eig_spat %>%
            filter(class != "0"),
          aes(geometry = geom, colour = as.numeric(class), size = eig)) +
  scale_size_continuous(range = c(0.5, 10), trans = "exp") +
  scale_color_distiller(palette = "Dark2") +
  theme_bw() +
  labs(title = "Eigenvector Centrality and Louvian Modularity Class") +
  theme(legend.position = "none", panel.grid.major = element_line(color = "gray")) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)

grid.arrange(ggplot() +
               geom_sf(data = st_geometry(st_as_sf(world["geom"][world$continent == "Africa", ] %>%
                                                     st_transform(crs = "+proj=laea"))),
                       fill = add.alpha("gray90", alpha = 0.8)) +
               geom_sf(data = st_transform(edges,
                                           crs = "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"),
                       size = 0.005, color = "grey") +
               geom_sf(data = modularity_spat %>%
                         filter(class == "0"),
                       aes(geometry = geom), size = 1) +
               #geom_sf(data = eigen_spat, aes(geometry = geom, size = eigen)) +
               geom_sf(data = mod_eig_spat %>%
                         filter(class != "0"),
                       aes(geometry = geom, colour = as.numeric(class), size = eig)) +
               scale_size_continuous(range = c(0.5, 10), trans = "exp") +
               scale_color_distiller(palette = "Dark2") +
               theme_bw() +
               labs(title = "Eigenvector Centrality and Louvian Modularity Class") +
               theme(legend.position = "none", panel.grid.major = element_line(color = "gray")) +
               annotation_scale(location = "bl") +
               annotation_north_arrow(location = "bl", which_north = "true",
                                      pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                                      style = north_arrow_fancy_orienteering),
             ggplot() +
               geom_sf(data = st_geometry(st_as_sf(world["geom"][world$continent == "Africa", ] %>%
                                                     st_transform(crs = "+proj=laea"))),
                       fill = add.alpha("gray90", alpha = 0.8)) +
               geom_sf(data = st_transform(edges,
                                           crs = "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"),
                       size = 0.005, color = "grey") +
               geom_sf(data = modularity_spat %>%
                         filter(class == "0"),
                       aes(geometry = geom), size = 1) +
               #geom_sf(data = eigen_spat, aes(geometry = geom, size = eigen)) +
               geom_sf(data = mod_eig_spat %>%
                         filter(class != "0"),
                       aes(geometry = geom, colour = as.numeric(class), size = bet)) +
               scale_size_continuous(range = c(0.5, 10)) +
               scale_color_distiller(palette = "Dark2") +
               theme_bw() +
               labs(title = "Betweeness Centrality and Louvian Modularity Class") +
               theme(legend.position = "none", panel.grid.major = element_line(color = "gray")) +
               annotation_scale(location = "bl") +
               annotation_north_arrow(location = "bl", which_north = "true",
                                      pad_x = unit(1,"cm"), pad_y = unit(1, "cm"),
                                      style = north_arrow_fancy_orienteering),
             ncol = 2, nrow = 1)
