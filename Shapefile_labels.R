



library(sf)
library(dplyr)
library(purrr)
library(sfheaders)

setwd("C:/Users/PaintRock/Documents/Data processing/Hyperspectral")
# Load the shapefile
shapefile_path <- "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_Endmembers_32619.shp"
polygons <- st_read(shapefile_path)

# Function to split a polygon
split_polygon <- function(polygon) {
  coords <- st_coordinates(polygon)
  
  # Determine the bounding box
  bbox <- st_bbox(polygon)
  
  # Check orientation
  horizontal_split <- (bbox[4] - bbox[2]) < (bbox[3] - bbox[1])
  
  # Create a split line
  if (horizontal_split) {
    split_line <- st_line_string(matrix(c(bbox[1], mean(c(bbox[2], bbox[4])),
                                          bbox[3], mean(c(bbox[2], bbox[4]))), 
                                        ncol = 2, byrow = TRUE))
  } else {
    split_line <- st_line_string(matrix(c(mean(c(bbox[1], bbox[3])), bbox[2],
                                          mean(c(bbox[1], bbox[3])), bbox[4]), 
                                        ncol = 2, byrow = TRUE))
  }
  
  # Create two new polygons by splitting the original polygon
  new_polygons <- st_split(polygon, split_line)
  
  # Add buffer to create separation
  new_polygons <- st_buffer(new_polygons, dist = 0.01) # Adjust distance as needed
  
  return(new_polygons)
}

# Split all polygons
new_polygons <- polygons %>%
  rowwise() %>%
  mutate(split_geom = list(split_polygon(geometry)),
         new_label_1 = paste0(label, "_1"),
         new_label_2 = paste0(label, "_2")) %>%
  unnest(split_geom) %>%
  select(new_label = ifelse(row_number() %% 2 == 1, new_label_1, new_label_2), geometry)

# Convert to sf object
new_polygons_sf <- st_as_sf(new_polygons)

# Save to new shapefile
st_write(new_polygons_sf, "path/to/your/new_shapefile.shp")
