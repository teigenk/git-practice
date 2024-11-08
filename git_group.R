# clear work space of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# Load pacman
library(pacman)

# Load necessary packages
pacman::p_load(dplyr, sf, ggplot2, readr, tidyr, tmap, stringr, rnaturalearth, rnaturalearthdata) 

# Load rdata file
load("data/all_disasters_select_vars.rdata")

# View the dataframe
glimpse(all_disaster_perimeters_ics_and_news_buffers_conus_select_variables)

# Read the shapefile from the .zip file
shapefile_data <- st_read("data/ca_shapefile/data/ZCTA2010.shp")

# View the dataframe
glimpse(shapefile_data)

# Check the CRS (Coordinate Reference System)
st_crs(shapefile_data)

# Plot total population 
ggplot(data = shapefile_data) +
  geom_sf(aes(fill = TOT_POP)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  labs(title = "Total Population in California", fill = "Population") +
  theme_minimal()

# Filter data for California and for disasters from 2009 onward
ca_disasters <- all_disaster_perimeters_ics_and_news_buffers_conus_select_variables %>%
  filter(disaster_list_states == "CA", aggregate_year >= 2009)

# Check the subset
glimpse(ca_disasters)

# Get USA state boundaries
states <- ne_states(country = "united states of america", returnclass = "sf")

# Filter for California
california_border <- states[states$name == "California", ]

# Plot wildfire perimeters with California border
ggplot() +
  geom_sf(data = ca_disasters, aes(fill = area_sq_m)) + 
  geom_sf(data = california_border, color = "black", fill = NA, size = 0.8) + 
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") + 
  labs(title = "California Wildfire Perimeters 2009-2019", fill = "Area (sq m)") +
  theme_minimal()

# Check CRS for both datasets
st_crs(shapefile_data)
st_crs(ca_disasters)

# Ensure shapefile_data is already in EPSG: 3310
shapefile_data <- st_transform(shapefile_data, crs = 3310)

# Transform ca_disasters to EPSG: 3310
ca_disasters <- st_transform(ca_disasters, crs = 3310)

# Overlay ZCTAs and wildfire perimeters
ggplot() +
  geom_sf(data = shapefile_data, fill = NA, color = "gray") +  
  geom_sf(data = ca_disasters, aes(fill = area_sq_m), alpha = 0.5) +  
  geom_sf(data = california_border, color = "black", fill = NA, size = 0.8) +  
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +  
  labs(title = "CA ZCTAs and Wildfire Perimeters 2009-2019", fill = "Area (sq m)") +
  theme_minimal()

# Create a 10 km buffer around each disaster perimeter
ca_disasters_buffered <- ca_disasters %>%
  st_buffer(dist = 10000)

# Check the buffered disaster perimeters
glimpse(ca_disasters_buffered)

# Plot the 10 km buffered disaster perimeters in California
ggplot(data = ca_disasters_buffered) +
  geom_sf(aes(fill = area_sq_m)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  labs(title = "10 km Buffered Disaster Perimeters in California", fill = "Area (sq m)") +
  theme_minimal()

# Overlay ZCTAs and buffered disaster perimeters
ggplot() +
  geom_sf(data = shapefile_data, fill = NA, color = "gray") +  
  geom_sf(data = ca_disasters_buffered, aes(fill = area_sq_m), alpha = 0.5) +  
  geom_sf(data = california_border, color = "black", fill = NA, size = 0.8) +  
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +  
  labs(title = "CA ZCTAs and WF Perimeters with 10km Buffers 2009-2019", fill = "Area (sq m)") +
  theme_minimal()

# Read in the kpsc zcta data
kpsc_zcta_data <- read.csv("data/kpsc_zcta_counts.csv")

# View data
glimpse(kpsc_zcta_data)

# Convert zcta to character in kpsc_zcta_data
kpsc_zcta_data <- kpsc_zcta_data %>%
  mutate(zcta = as.character(zcta))

# Select only the necessary columns from kpsc_zcta_data
kpsc_zcta_data_selected <- kpsc_zcta_data %>%
  select(zcta, kpsc_pop_age_60p, classification)

# Perform the join to retain spatial data and the selected columns
kpsc_zcta_spatial <- shapefile_data %>%
  filter(ZCTA %in% kpsc_zcta_data$zcta) %>%
  left_join(kpsc_zcta_data_selected, by = c("ZCTA" = "zcta"))

# View data
glimpse(kpsc_zcta_spatial)

# Ensure CRS compatability
st_crs(kpsc_zcta_spatial)


# consider different function
# Perform spatial intersection to check overlaps
zcta_disaster_overlap <- st_intersects(kpsc_zcta_spatial, ca_disasters_buffered, sparse = FALSE)

# Add a logical column indicating overlap
kpsc_zcta_spatial$overlap_with_disaster <- apply(zcta_disaster_overlap, 1, any)

length(unique(kpsc_zcta_spatial$ZCTA))

nrow(kpsc_zcta_spatial)

# Filter to see only overlapping ZCTAs
overlapping_zctas <- kpsc_zcta_spatial %>%
  filter(overlap_with_disaster)

# Remove geometry from 'overlapping_zctas' to make it a regular data frame for CSV export
overlapping_zctas_df <- st_drop_geometry(overlapping_zctas)

# Export the resulting data frame to CSV
write.csv(overlapping_zctas_df, "~/Documents/repos/kpsc/overlapping_zctas.csv", row.names = FALSE)

# Plot all ZCTAs with highlighted overlapping ZCTAs
ggplot() +
  geom_sf(data = shapefile_data, fill = "lightgray", color = "white", size = 0.1) +
  geom_sf(data = overlapping_zctas, fill = "blue", color = "black", size = 0.3) +
  geom_sf(data = california_border, color = "black", fill = NA, size = 0.8) + 
  labs(title = "KPSC ZCTAs Overlapping with Buffered Wildfire Perimeters",
       fill = "Overlap Status") +
  theme_minimal()


# Identify the wildfire perimeters that overlap with the ZCTAs
wildfire_overlap_indices <- st_intersects(ca_disasters_buffered, overlapping_zctas, sparse = FALSE) %>%
  apply(1, any)

glimpse(ca_disasters_buffered)

# Filter ca_disasters_buffered to include only the overlapping wildfire perimeters
overlapping_fires <- ca_disasters_buffered[wildfire_overlap_indices, ]

# Plot overlapping ZCTAs and their respective wildfire perimeters
ggplot() +
  geom_sf(data = shapefile_data, fill = "lightgray", color = "white", size = 0.1) +
  geom_sf(data = overlapping_fires, fill = NA, color = "red", size = 0.5) +
  geom_sf(data = california_border, color = "black", fill = NA, size = 0.8) + 
  labs(title = "Buffered WF Perimeters Overlapping with KPSC ZCTAs",
       fill = "Overlap Status") +
  theme_minimal()

# Plot overlapping ZCTAs and their respective wildfire perimeters
ggplot() +
  geom_sf(data = shapefile_data, fill = "lightgray", color = "white", size = 0.1) +
  geom_sf(data = overlapping_zctas, fill = "blue", color = "black", size = 0.3) +
  geom_sf(data = overlapping_fires, fill = NA, color = "red", size = 0.5) +
  geom_sf(data = california_border, color = "black", fill = NA, size = 0.8) + 
  labs(title = "Buffered WF Perimeters with KPSC ZCTA Overlaps",
       fill = "Overlap Status") +
  theme_minimal()

# Remove geometry from 'overlapping_fires' to make it a regular data frame
overlapping_fires_df <- st_drop_geometry(overlapping_fires)

# Export the resulting data frame to CSV
write.csv(overlapping_fires_df, "~/Documents/repos/kpsc/overlapping_fires.csv", row.names = FALSE)


# Perform a spatial join to associate each ZCTA with overlapping wildfire perimeters
overlap_summary <- st_join(kpsc_zcta_spatial, ca_disasters_buffered, join = st_intersects) %>%
  filter(!is.na(disaster_id_clean)) %>%
  group_by(disaster_id_clean) %>%
  summarize(
    overlapping_zctas = paste(unique(ZCTA), collapse = ", "),
    total_population_age_60p = sum(kpsc_pop_age_60p, na.rm = TRUE)
  )

# View the summarized data
glimpse(overlap_summary)

# Remove geometry to export as a CSV
overlap_summary_df <- st_drop_geometry(overlap_summary)

# Export data frame to CSV
write.csv(overlap_summary_df, "~/Documents/repos/kpsc/zcta_wildfire_overlap_summary.csv", row.names = FALSE)
