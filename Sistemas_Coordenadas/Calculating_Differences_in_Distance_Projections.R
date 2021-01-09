
# Libraries -------------------

# Load spatial libraries

library(sf) # Simple Features Package for transferable information
library(sp)
library(spDataLarge) # Open Source Spatial Databases
library(spData) # Open Source Spatial Databases

# Database management library

library(dplyr)

# Plot Information

library(ggplot2)

#

# Compare Systems -------------------------

point_1<-st_sfc(
  st_point(c(-122.44583346530746, 37.78189590999756)),
  crs = "+proj=longlat"
)
point_1_aeqd <- point_1 %>% st_transform(crs = "+proj=aeqd")
point_1_aea <- point_1 %>% st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=42.5")

point_2<-st_sfc(
  st_point(c(-95.993612 , 36.248181)),
  crs = "+proj=longlat"
)
point_2_aeqd <- point_2 %>% st_transform(crs = "+proj=aeqd")
point_2_aea <- point_2 %>% st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=42.5")

point_3<-st_sfc(
  st_point(c(-74.005973, 40.712775)),
  crs = "+proj=longlat"
)
point_3_aeqd <- point_3 %>% st_transform(crs = "+proj=aeqd")
point_3_aea <- point_3 %>% st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=42.5")

point_4<-st_sfc(
  st_point(c(-40.095642, 36.226027)),
  crs = "+proj=longlat"
)
point_4_aeqd <- point_4 %>% st_transform(crs = "+proj=aeqd")
point_4_aea <- point_4 %>% st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=42.5")

point_5<-st_sfc(
  st_point(c(-3.730634, 40.479888)),
  crs = "+proj=longlat"
)
point_5_aeqd <- point_5 %>% st_transform(crs = "+proj=aeqd")
point_5_aea <- point_5 %>% st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=42.5")

point_6<-st_sfc(
  st_point(c(31.227862, 30.030494)),
  crs = "+proj=longlat"
)
point_6_aeqd <- point_6 %>% st_transform(crs = "+proj=aeqd")
point_6_aea <- point_6 %>% st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=42.5")

point_7<-st_sfc(
  st_point(c(72.953937, 19.040736)),
  crs = "+proj=longlat"
)
point_7_aeqd <- point_7 %>% st_transform(crs = "+proj=aeqd")
point_7_aea <- point_7 %>% st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=42.5")

point_8<-st_sfc(
  st_point(c(106.765254 , 10.908194)),
  crs = "+proj=longlat"
)
point_8_aeqd <- point_8 %>% st_transform(crs = "+proj=aeqd")
point_8_aea <- point_8 %>% st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=42.5")

point_9<-st_sfc(
  st_point(c(151.191836 , -33.848146)),
  crs = "+proj=longlat"
)
point_9_aeqd <- point_9 %>% st_transform(crs = "+proj=aeqd")
point_9_aea <- point_9 %>% st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=42.5")

point_10<-st_sfc(
  st_point(c(174.770625 , -36.840583)),
  crs = "+proj=longlat"
)
point_10_aeqd <- point_10 %>% st_transform(crs = "+proj=aeqd")
point_10_aea <- point_10 %>% st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=42.5")

# Plot Points

plot(world["geom"], axes = TRUE, col = "light grey")
plot(point_1, add = TRUE, pch = 19, cex = 2)
plot(point_2, add = TRUE, pch = 19, cex = 2)
plot(point_3, add = TRUE, pch = 19, cex = 2)
plot(point_4, add = TRUE, pch = 19, cex = 2)
plot(point_5, add = TRUE, pch = 19, cex = 2)
plot(point_6, add = TRUE, pch = 19, cex = 2)
plot(point_7, add = TRUE, pch = 19, cex = 2)
plot(point_8, add = TRUE, pch = 19, cex = 2)
plot(point_9, add = TRUE, pch = 19, cex = 2)
plot(point_10, add = TRUE, pch = 19, cex = 2)

par(mfrow = c(1,2))

plot(world["geom"] %>% st_transform(crs = "+proj=aeqd"), axes = TRUE)
plot(point_1_aeqd, add = TRUE, pch = 19, cex = 2)
plot(point_2_aeqd, add = TRUE, pch = 19, cex = 2)
plot(point_3_aeqd, add = TRUE, pch = 19, cex = 2)
plot(point_4_aeqd, add = TRUE, pch = 19, cex = 2)
plot(point_5_aeqd, add = TRUE, pch = 19, cex = 2)
plot(point_6_aeqd, add = TRUE, pch = 19, cex = 2)
plot(point_7_aeqd, add = TRUE, pch = 19, cex = 2)
plot(point_8_aeqd, add = TRUE, pch = 19, cex = 2)
plot(point_9_aeqd, add = TRUE, pch = 19, cex = 2)
plot(point_10_aeqd, add = TRUE, pch = 19, cex = 2)

plot(world["geom"]  %>% st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=42.5"),
     axes = TRUE)
plot(point_1_aea, add = TRUE, pch = 19, cex = 2)
plot(point_2_aea, add = TRUE, pch = 19, cex = 2)
plot(point_3_aea, add = TRUE, pch = 19, cex = 2)
plot(point_4_aea, add = TRUE, pch = 19, cex = 2)
plot(point_5_aea, add = TRUE, pch = 19, cex = 2)
plot(point_6_aea, add = TRUE, pch = 19, cex = 2)
plot(point_7_aea, add = TRUE, pch = 19, cex = 2)
plot(point_8_aea, add = TRUE, pch = 19, cex = 2)
plot(point_9_aea, add = TRUE, pch = 19, cex = 2)
plot(point_10_aea, add = TRUE, pch = 19, cex = 2)

# Calculate Distances

data <- tibble(Point_N = numeric(), Proj_1 = numeric(), Proj_2 = numeric())
data <- data %>% add_row(
  Point_N = 10,
  Proj_1 = as.numeric(st_distance(point_1_aeqd, point_10_aeqd)[1,1] / 1000),
  Proj_2 = as.numeric(st_distance(point_1_aea, point_10_aea)[1,1] / 1000)
)
data <- data %>% mutate(difference = abs(Proj_1 - Proj_2))

grid.arrange(ggplot(data = data) +
               geom_line(aes(x = Point_N, y = Proj_1),
                         size = 1) +
               geom_point(aes(x = Point_N, y = Proj_1),
                          size = 2) +
               geom_line(aes(x = Point_N, y = Proj_2),
                         size = 1, color = "red",
                         linetype = "dashed") +
               geom_point(aes(x = Point_N, y = Proj_2),
                          size = 2, color = "red") +
               theme_bw() +
               scale_x_discrete(limits = as.character(seq(1:10))) +
               ggtitle("Distance from San Francisco in Km") +
               ylab("Distance (Km)") +
               theme(axis.title.x = element_blank(),
                     plot.margin = margin(25,25,25,25),
                     axis.title.y = element_text(face = "bold", margin = margin(r = 15),
                                                 size = 15),
                     axis.text = element_text(size = 15),
                     plot.title = element_text(face = "bold")),
             ggplot(data = data, aes(x = Point_N, y = difference)) +
               geom_line(size = 1) +
               geom_point(size = 2) +
               theme_bw() +
               scale_x_discrete(limits = as.character(seq(1:10))) +
               ggtitle("Difference between Measurements") +
               ylab("Difference (Km)") +
               theme(axis.title.x = element_blank(),
                     plot.margin = margin(25,25,25,25),
                     axis.title.y = element_text(face = "bold", margin = margin(r = 15),
                                                 size = 15),
                     axis.text = element_text(size = 15),
                     plot.title = element_text(face = "bold")),
             ncol = 2, nrow = 1)

plot(world["geom"]  %>% st_transform(crs = "+proj=aea +lon_0=-122.445 +lat_2=37.78"),
     axes = TRUE)
plot(point_1_aea %>% st_transform(crs = "+proj=aea +lon_0=-122.445 +lat_2=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_2_aea %>% st_transform(crs = "+proj=aea +lon_0=-122.445 +lat_2=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_3_aea %>% st_transform(crs = "+proj=aea +lon_0=-122.445 +lat_2=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_4_aea %>% st_transform(crs = "+proj=aea +lon_0=-122.445 +lat_2=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_5_aea %>% st_transform(crs = "+proj=aea +lon_0=-122.445 +lat_2=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_6_aea %>% st_transform(crs = "+proj=aea +lon_0=-122.445 +lat_2=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_7_aea %>% st_transform(crs = "+proj=aea +lon_0=-122.445 +lat_2=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_8_aea %>% st_transform(crs = "+proj=aea +lon_0=-122.445 +lat_2=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_9_aea %>% st_transform(crs = "+proj=aea +lon_0=-122.445 +lat_2=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_10_aea %>% st_transform(crs = "+proj=aea +lon_0=-122.445 +lat_2=37.78"),
     add = TRUE, pch = 19, cex = 2)

plot(world["geom"]  %>% st_transform(crs = "+proj=aeqd +lon_0=-122.445 +lat_0=37.78"),
     axes = TRUE)
plot(point_1_aeqd %>% st_transform(crs = "+proj=aeqd +lon_0=-122.445 +lat_0=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_2_aeqd %>% st_transform(crs = "+proj=aeqd +lon_0=-122.445 +lat_0=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_3_aeqd %>% st_transform(crs = "+proj=aeqd +lon_0=-122.445 +lat_0=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_4_aeqd %>% st_transform(crs = "+proj=aeqd +lon_0=-122.445 +lat_0=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_5_aeqd %>% st_transform(crs = "+proj=aeqd +lon_0=-122.445 +lat_0=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_6_aeqd %>% st_transform(crs = "+proj=aeqd +lon_0=-122.445 +lat_0=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_7_aeqd %>% st_transform(crs = "+proj=aeqd +lon_0=-122.445 +lat_0=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_8_aeqd %>% st_transform(crs = "+proj=aeqd +lon_0=-122.445 +lat_0=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_9_aeqd %>% st_transform(crs = "+proj=aeqd +lon_0=-122.445 +lat_0=37.78"),
     add = TRUE, pch = 19, cex = 2)
plot(point_10_aeqd %>% st_transform(crs = "+proj=aeqd +lon_0=-122.445 +lat_0=37.78"),
     add = TRUE, pch = 19, cex = 2)

centred_data <- tibble(Point_N = numeric(), Proj_1 = numeric(), Proj_2 = numeric())
centred_proj_aeqd = "+proj=aeqd +lon_0=-122.445 +lat_0=37.78"
centred_proj_aea = "+proj=aea +lon_0=-122.445 +lat_2=37.78"
centred_data <- centred_data %>% add_row(
  Point_N = 10,
  Proj_1 = as.numeric(st_distance(point_1_aeqd %>%
                                    st_transform(crs = centred_proj_aeqd),
                                  point_10_aeqd %>%
                                    st_transform(crs = centred_proj_aeqd))[1,1] / 1000),
  Proj_2 = as.numeric(st_distance(point_1_aea %>%
                                    st_transform(crs = centred_proj_aea),
                                  point_10_aea %>%
                                    st_transform(crs = centred_proj_aea))[1,1] / 1000)
)
centred_data <- centred_data %>% mutate(difference = abs(Proj_1 - Proj_2))

grid.arrange(ggplot(data = centred_data) +
               geom_line(aes(x = Point_N, y = Proj_1),
                         size = 1) +
               geom_point(aes(x = Point_N, y = Proj_1),
                          size = 2) +
               geom_line(aes(x = Point_N, y = Proj_2),
                         size = 1, color = "red",
                         linetype = "dashed") +
               geom_point(aes(x = Point_N, y = Proj_2),
                          size = 2, color = "red") +
               theme_bw() +
               scale_x_discrete(limits = as.character(seq(1:10))) +
               ggtitle("Distance from San Francisco in Km") +
               ylab("Distance (Km)") +
               theme(axis.title.x = element_blank(),
                     plot.margin = margin(25,25,25,25),
                     axis.title.y = element_text(face = "bold", margin = margin(r = 15),
                                                 size = 15),
                     axis.text = element_text(size = 15),
                     plot.title = element_text(face = "bold")),
             ggplot(data = centred_data, aes(x = Point_N, y = difference)) +
               geom_line(size = 1) +
               geom_point(size = 2) +
               theme_bw() +
               scale_x_discrete(limits = as.character(seq(1:10))) +
               ggtitle("Difference between Measurements") +
               ylab("Difference (Km)") +
               theme(axis.title.x = element_blank(),
                     plot.margin = margin(25,25,25,25),
                     axis.title.y = element_text(face = "bold", margin = margin(r = 15),
                                                 size = 15),
                     axis.text = element_text(size = 15),
                     plot.title = element_text(face = "bold")),
             ncol = 2, nrow = 1)

differences_non_centred <- tibble(Point_N = data$Point_N,
                                  Difference = data$difference,
                                  Sample = factor("Non Centred"))
differences_centred <- tibble(Point_N = centred_data$Point_N,
                              Difference = centred_data$difference,
                              Sample = factor("Centred"))
differences <- rbind(differences_non_centred, differences_centred)

add.alpha <- function(col, alpha=1){ # Function for setting the alpha of colours
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

ggplot(data = differences, aes(x = Point_N, y = Difference, colour = Sample)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  theme_bw() +
  scale_x_discrete(limits = as.character(seq(1:10))) +
  ggtitle("Difference between Measurements") +
  ylab("Difference (Km)") +
  theme(axis.title.x = element_blank(),
        plot.margin = margin(25,25,25,25),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15),
                                    size = 15),
        axis.text = element_text(size = 15),
        plot.title = element_text(face = "bold"),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 18, face = "bold"),
        legend.background = element_rect(fill = add.alpha("#CCCCCC", alpha = 0.2)),
        legend.box.background = element_rect(colour = "black"),
        legend.position = "bottom")

#