
# Libraries -------------------

# Load spatial libraries

library(sf) # Simple Features Package for transferable information
library(sp)
library(spDataLarge) # Open Source Spatial Databases
library(spData) # Open Source Spatial Databases

# Database management library

library(dplyr)

#

# Define Coordinate Points ------------------

london <- st_sfc(
  st_point(c(51.47, 0.453)),
  crs = "+proj=longlat"
)

paris <- st_sfc(
  st_point(c(48.856614, 2.352222)),
  crs = "+proj=longlat"
)

new_york <- st_sfc(
  st_point(c(40.712775, -74.005973)),
  crs = "+proj=longlat"
)

#

# Consult Coordinate Reference System --------------------------

st_crs(london) #or paris or new_york

# Are two CRS the same?

st_crs(london) == st_crs(paris)
st_crs(paris) == st_crs(new_york)

#

# Calculate Distance ---------------------------

st_distance(london, paris)
st_distance(paris, new_york)

#

# Reproject Data -----------------------------

# Try:
# +proj=aea +lat_1=29.5 +lat_2=42.5
# +proj=stere
# +proj=aeqd
# +proj=tmerc

new_proj <- "+proj=tmerc"

london_transformed <- st_transform(london, crs = new_proj)
paris_transformed <- st_transform(paris, crs = new_proj)
new_york_transformed <- st_transform(new_york, crs = new_proj)

st_distance(london, paris)
st_distance(london_transformed, paris_transformed)
st_distance(new_york, paris)
st_distance(new_york_transformed, paris_transformed)

#

# Calculate the Area of Greenland -------------------------

data(world) # Load world database
greenland = st_union(world[world$name_long == "Greenland",])

# Consult the already existing CRS

st_crs(greenland)$proj4string

# Calculate Area

st_area(greenland)

# Change CRS

# Try:
# +proj=aea +lat_1=29.5 +lat_2=42.5
# +proj=stere
# +proj=aeqd
# +proj=tmerc

new_proj <- "+proj=tmerc"
greenland_transformed <- st_transform(greenland, crs = new_proj)
st_area(greenland)
st_area(greenland_transformed)

# Observed differences in plot

X11(); par(mfrow = c(2,2)); for (i in c("+proj=aea +lat_1=29.5 +lat_2=42.5",
                                        "+proj=stere",
                                        "+proj=aeqd",
                                        "+proj=tmerc")) {
  transformed_data <- st_transform(greenland, crs = i)
  plot(transformed_data, axes = TRUE,
       main = i,
       col = "dark grey")
  }; par(mfrow = c(1,1))

#
