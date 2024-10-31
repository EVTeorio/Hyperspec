library(sf)
library(xml2)

# Load the XML file path
xml_file <- "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Labeling/Labeled_Endmembers_32619.xml"

# Function to convert XML to a list of polygons
xmlToPolygons <- function(xml_file) {
  xml_data <- read_xml(xml_file)
  regions <- xml_find_all(xml_data, "//Region")
  
  if (length(regions) == 0) {
    stop("No regions found in the XML file. Please check your XML structure.")
  }
  
  polygon_list <- lapply(regions, function(region) {
    coords <- xml_text(xml_find_first(region, ".//Coordinates"))
    coords <- strsplit(trimws(coords), "\\s+")[[1]]
    
    x_coords <- as.numeric(coords[seq(1, length(coords), by = 2)])
    y_coords <- as.numeric(coords[seq(2, length(coords), by = 2)])
    
    # Create polygon
    polygon <- st_polygon(list(cbind(x_coords, y_coords)))
    return(polygon)
  })
  
  polygons_sf <- st_sfc(polygon_list, crs = 4326)
  roi_polygons_cleaned <- st_make_valid(polygons_sf)
  
  valid_indices <- st_is_valid(roi_polygons_cleaned)
  roi_polygons_valid <- roi_polygons_cleaned[valid_indices]
  
  if (length(roi_polygons_valid) == 0) {
    stop("All polygons were invalid after repair. Please check the input data.")
  }
  
  return(list(polygons = roi_polygons_valid, regions = regions))
}

# Define spatial resolution and buffer parameters
spatial_resolution <- 1  # Adjust this value as needed (in meters)
buffer_pixels <- 2  # Number of pixels for the buffer
buffer_distance <- buffer_pixels * spatial_resolution  # Convert to meters

# Function to split ROI polygons while retaining original shape
split_roi <- function(roi) {
  coords <- st_coordinates(roi)
  
  # Calculate the centroid to use as the splitting point
  centroid <- colMeans(coords)
  
  # Create a vertical split line through the centroid
  split_line <- st_sfc(st_linestring(rbind(c(centroid[1], min(coords[, 2])),
                                           c(centroid[1], max(coords[, 2])))), crs = st_crs(roi))
  
  # Use st_difference to retain the original shape while creating new polygons
  left_part <- st_difference(roi, st_sfc(st_polygon(list(rbind(c(min(coords[,1]), min(coords[,2])),
                                                               c(min(coords[,1]), max(coords[,2])),
                                                               c(centroid[1], max(coords[,2])),
                                                               c(centroid[1], min(coords[,2])),
                                                               c(min(coords[,1]), min(coords[,2]))))), crs = st_crs(roi)))
  
  right_part <- st_difference(roi, st_sfc(st_polygon(list(rbind(c(centroid[1], min(coords[,2])),
                                                                c(max(coords[,1]), min(coords[,2])),
                                                                c(max(coords[,1]), max(coords[,2])),
                                                                c(centroid[1], max(coords[,2])),
                                                                c(centroid[1], min(coords[,2]))))), crs = st_crs(roi)))
  
  return(list(left_part, right_part))
}

# Load ROI data as polygons
roi_data <- xmlToPolygons(xml_file)
roi_polygons <- roi_data$polygons
regions <- roi_data$regions

# Split ROIs and collect new ROIs with updated names
new_rois <- list()
coord_sys_str <- "GEOGCS[\"GCS_WGS_1984\",DATUM[\"D_WGS_1984\",SPHEROID[\"WGS_1984\",6378137.0,298.257223563]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]]"

for (i in seq_along(roi_polygons)) {
  split_rois <- split_roi(roi_polygons[[i]])  # Split while retaining shape
  
  # Get the original name of the current ROI
  original_name <- xml_attr(regions[[i]], "name")
  
  # Ensure split_rois has valid parts
  if (length(split_rois) == 0) {
    warning(paste("No valid split for ROI:", original_name))
    next
  }
  
  for (j in seq_along(split_rois)) {
    # Label the split ROIs with the original name and prefixes 1_ and 2_
    region_name <- paste0(j, "_", original_name)
    new_rois[[length(new_rois) + 1]] <- list(polygon = split_rois[[j]], name = region_name)
  }
}

# Check if new_rois has any entries
if (length(new_rois) == 0) {
  stop("No new ROIs were created. Please check the splitting logic.")
}

# Create a new XML document for output
new_doc <- xml_new_root("RegionsOfInterest", version="1.1")
for (i in seq_along(new_rois)) {
  roi <- new_rois[[i]]$polygon
  coords <- st_coordinates(roi)
  coords <- coords[,1:2]
  
  region_name <- new_rois[[i]]$name
  
  region_node <- xml_add_child(new_doc, "Region")
  xml_attr(region_node, "name") <- region_name
  xml_attr(region_node, "color") <- "0,255,0"  # Example color
  
  geometry_node <- xml_add_child(region_node, "GeometryDef")
  xml_add_child(geometry_node, "CoordSysStr", coord_sys_str)
  
  polygon_node <- xml_add_child(geometry_node, "Polygon")
  exterior_node <- xml_add_child(polygon_node, "Exterior")
  linear_ring_node <- xml_add_child(exterior_node, "LinearRing")
  
  coords_string <- paste(apply(coords, 1, paste, collapse = " "), collapse = " ")
  xml_add_child(linear_ring_node, "Coordinates", coords_string)
}

# Save the new XML document to a file
write_xml(new_doc, "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Split_Endmembers_32619.xml", overwrite = TRUE)













coords <- coords[,1:2]







