

library(sf)
library(xml2)

# Read the XML file
xml_file <- "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Labeled_Endmembers_32619.xml"


# Function to convert XML to a list of polygons
xmlToPolygons <- function(xml_file) {
  xml_data <- read_xml(xml_file)
  regions <- xml_find_all(xml_data, "//Region")  # Store regions for later use
  
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
  invalid_polygons <- roi_polygons_cleaned[!st_is_valid(roi_polygons_cleaned)]
  
  if (length(invalid_polygons) > 0) {
    message("Invalid polygons found after cleaning. Attempting to repair them.")
    roi_polygons_cleaned <- st_make_valid(roi_polygons_cleaned)
  }
  
  valid_indices <- st_is_valid(roi_polygons_cleaned)
  roi_polygons_valid <- roi_polygons_cleaned[valid_indices]
  
  if (length(roi_polygons_valid) == 0) {
    stop("All polygons were invalid after repair. Please check the input data.")
  }
  
  return(list(polygons = roi_polygons_valid, regions = regions))  # Return both polygons and regions
}

# Load ROI data as polygons
xml_file <- "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Labeled_Endmembers_32619.xml"
roi_data <- xmlToPolygons(xml_file)
roi_polygons <- roi_data$polygons
regions <- roi_data$regions  # Extract regions for later use

# Function to split ROI polygons with a gap
split_roi <- function(roi, roi_name, gap_size = 0.1) {
  coords <- st_coordinates(roi)
  x1 <- min(coords[, 1])
  y1 <- min(coords[, 2])
  x2 <- max(coords[, 1])
  y2 <- max(coords[, 2])
  
  width <- x2 - x1
  height <- y2 - y1
  
  if (height > width) {
    # Split horizontally and create a gap
    mid_y <- (y1 + y2) / 2
    gap_y1 <- mid_y - gap_size / 2
    gap_y2 <- mid_y + gap_size / 2
    
    roi1 <- st_polygon(list(rbind(c(x1, y1), c(x2, y1), c(x2, gap_y1), c(x1, gap_y1), c(x1, y1))))
    roi2 <- st_polygon(list(rbind(c(x1, gap_y2), c(x2, gap_y2), c(x2, y2), c(x1, y2), c(x1, gap_y2))))
    
    return(list(st_sfc(roi1, crs = st_crs(roi)), st_sfc(roi2, crs = st_crs(roi))))
    
  } else {
    # Split vertically and create a gap
    mid_x <- (x1 + x2) / 2
    gap_x1 <- mid_x - gap_size / 2
    gap_x2 <- mid_x + gap_size / 2
    
    roi1 <- st_polygon(list(rbind(c(x1, y1), c(gap_x1, y1), c(gap_x1, y2), c(x1, y2), c(x1, y1))))
    roi2 <- st_polygon(list(rbind(c(gap_x2, y1), c(x2, y1), c(x2, y2), c(gap_x2, y2), c(gap_x2, y1))))
    
    return(list(st_sfc(roi1, crs = st_crs(roi)), st_sfc(roi2, crs = st_crs(roi))))
  }
}

# Split ROIs and collect new ROIs with updated names
new_rois <- list()
coord_sys_str <- "GEOGCS[\"GCS_WGS_1984\",DATUM[\"D_WGS_1984\",SPHEROID[\"WGS_1984\",6378137.0,298.257223563]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]]"
for (i in seq_along(roi_polygons)) {
  split_rois <- split_roi(roi_polygons[[i]], xml_attr(regions[[i]], "name"))
  new_rois[[length(new_rois) + 1]] <- split_rois[[1]]
  new_rois[[length(new_rois) + 1]] <- split_rois[[2]]
}

# Create a new XML document for output
new_doc <- xml_new_root("RegionsOfInterest", version="1.1")
for (i in seq_along(new_rois)) {
  roi <- new_rois[[i]]
  coords <- st_coordinates(roi)
  
  # Prefix names with "1_" or "2_"
  prefix <- ifelse(i %% 2 == 1, "1_", "2_")
  original_name <- xml_attr(regions[[((i + 1) %/% 2)]], "name")
region_name <- paste0(prefix, original_name)

region_node <- xml_add_child(new_doc, "Region")
xml_attr(region_node, "name") <- region_name
xml_attr(region_node, "color") <- "0,255,0"  # Example color; you can set based on your needs

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





