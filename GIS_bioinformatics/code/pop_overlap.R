library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)         # For handling shapefiles
library(ggplot2)    # For plotting
library(dplyr)      # For data manipulation
library(rnaturalearth)  # For world map data
library(rnaturalearthdata)
library(raster)
#install.packages("ggpattern")
library(ggpattern)

sf_use_s2(FALSE)
library(terra)


mammals_data <- st_read("../data/assessment/MAMMALS(1)/MAMMALS.shp")

population_raster<- raster("../data/assessment/gpw_v4_population_density_rev11_2020_15_min.tif")
values_vector <- values(population_raster)

quantile_value <- quantile(values_vector, 0.95, na.rm = TRUE)
cat("95th percentile value", quantile_value)


top_5_percent_raster <- population_raster
top_5_percent_raster[top_5_percent_raster < quantile_value] <- NA

num_valid_values <- sum (!is.na(values(top_5_percent_raster)))
cat("Number of valid values left after reclassification", num_valid_values, "\n")

if(num_valid_values > 0 ) {
    max_value <- max(values(top_5_percent_raster), na.rm = TRUE)
    breaks <- c(quantile_value, max_value)

    colors <- hcl.colors(length(breaks) -1, "YlOrRd", rev = TRUE)
    world_map <- ne_countries(scale = "medium", returnclass = "sf")
    
    raster_df <- as.data.frame(top_5_percent_raster, xy = TRUE)
 
}
mammals_data <- st_read("../data/assessment/MAMMALS(1)/MAMMALS.shp")


CDV_animals <- c("Procyon lotor", "Musterla putorius", "Pusa caspica", 
                 "Mustela lutreola", "Civettictis civetta", 
                 "Panthera pardus", "Panthera leo", 
                 "Lupulella mesomelas", "Otocyon megalotis", 
                 "Pusa sibirica", "Vulpes vulpes")


CDV_subset <- mammals_data %>%
  filter(sci_name %in% CDV_animals)


bbox <- bbox(CDV_subset)
map <- ne_countries(scale = "medium", returnclass = "sf") #load world map



mammals_data <- st_read("../data/assessment/MAMMALS(1)/MAMMALS.shp")


CDV_animals <- list("Procyon lotor", "Musterla putorius", "Pusa caspica", "Mustela lutreola", "Civettictis civetta", 
"Panthera pardus", "Panthera leo", "Lupulella mesomelas", "Otocyon megalotis", "Pusa sibirica")

CDV_subset <- mammals_data %>%
  filter(sci_name %in% CDV_animals)

print(CDV_subset)

focal <- mammals_data %>% 
  filter(sci_name == "Vulpes vulpes")

print(focal)
    
###Fox vs human popu
plot2 <- ggplot() +
  geom_sf(data = map, fill = "gray90", color = "white") +  # World map
  #geom_sf(data = focal, aes(fill = sci_name), alpha = 0.5, color = NA ) +  # Other species
  #geom_sf(data = filter(CDV_subset, sci_name == "Vulpes vulpes"), fill = "red", alpha = 0.5, color = "black", size = 0.3) +  # Highlight fox
  geom_sf(data = CDV_subset, aes(fill = sci_name))+
  geom_sf(data = pop_df, aes(fill = population_density_bin), alpha = 0.3, color = NULL)+
  theme_minimal() +
  theme(legend.position = "bottom" ,
    legend.text = element_text(face = "italic"), 
    legend.title = element_text(face = "bold")) +
  labs(
    title = "Global Distribution of Vulpes vulpes",
    subtitle = "Overlapping distributions of Vulpes vulpes with Human Population",
    caption = "Data source: IUCN"
  )
plot2

breaks <- c(0,1,5,10,50,100,500,1000,5000)

# Extract values from the raster (excluding NA values)

####################################
# Load required libraries
library(ggplot2)
library(sf)
library(raster)
library(dplyr)
library(ggnewscale)
#install.packages("ggnewscale")
library(rnaturalearth)

# Load world map
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Load mammal shapefile
mammals_data <- st_read("../data/assessment/MAMMALS(1)/MAMMALS.shp")

# Load population density raster
population_raster <- raster("../data/assessment/gpw_v4_population_density_rev11_2020_15_min.tif")

# Calculate the top 5% population density threshold
quantile_value <- quantile(values(population_raster), 0.95, na.rm = TRUE)
top_5_percent_raster <- population_raster
top_5_percent_raster[top_5_percent_raster < quantile_value] <- NA

# Convert raster to data frame for ggplot
raster_df <- as.data.frame(rasterToPoints(top_5_percent_raster), xy = TRUE)
colnames(raster_df) <- c("x", "y", "population_density")
glimpse(raster_df)
# Filter CDV species
CDV_animals <- c("Procyon lotor", "Musterla putorius", "Pusa caspica", 
                 "Mustela lutreola", "Civettictis civetta", 
                 "Panthera pardus", "Panthera leo", 
                 "Lupulella mesomelas", "Otocyon megalotis", 
                 "Pusa sibirica", "Vulpes vulpes")
CDV_subset <- mammals_data %>% filter(sci_name %in% CDV_animals)

# Filter focal species (Vulpes vulpes)
focal <- mammals_data %>% filter(sci_name == "Vulpes vulpes")

###THIS MAP WORKS
plot2 <- ggplot() +
  # Base map
  geom_sf(data = world_map, fill = "gray90", color = "white") +
  
  # CDV species ranges
  geom_sf(data = CDV_subset, aes(fill = sci_name), color = "black", size = 0.3,  alpha = 0.5) +
  scale_fill_manual(
    values = c(
      "Procyon lotor" = "blue",
      "Musterla putorius" = "green",
      "Pusa caspica" = "purple",
      "Mustela lutreola" = "yellow",
      "Civettictis civetta" = "orange",
      "Panthera pardus" = "red",
      "Panthera leo" = "brown",
      "Lupulella mesomelas" = "pink",
      "Otocyon megalotis" = "cyan",
      "Pusa sibirica" = "violet",
      "Vulpes vulpes" = "magenta"
    ),
    name = "CDV Species"
  ) +
  
  # Add new scale for population density
  new_scale_fill() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = population_density), alpha = 0.2) +
  scale_fill_viridis_c(
    name = "Population Density",
    option = "plasma"
  ) +
  
  # Styling
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(face = "italic"),
    legend.title = element_text(face = "bold")
  ) +
  labs(
    title = "Global Distribution of CDV Species and Human Population Density Hotspots",
    subtitle = "Overlapping distributions of CDV species with human population hotspots",
    caption = "Data Source: IUCN and GPW v4"
  )

# Display the plot
print(plot2)


plot2 <- ggplot() +
  # Base map
  geom_sf(data = world_map, fill = "gray90", color = "white") +
  
  # CDV species ranges with borders
  geom_sf(data = CDV_subset, aes(fill = sci_name), alpha = 0.5, color = "black") +
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(n = 11, name = "Set3"),
    name = "CDV Species"
  ) +
  
  # Add new scale for population density with increased transparency
  new_scale_fill() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = population_density), alpha = 0.6) +
  scale_fill_viridis_c(
    name = "Population Density",
    option = "plasma"
  ) +
  
  # Styling and gridlines
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    panel.grid.major = element_line(color = "gray80", size = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(face = "bold")
  ) +
  
  # Add titles and annotations
  labs(
    title = "Global Distribution of CDV Species and Human Population Density Hotspots",
    subtitle = "Overlapping distributions of CDV species with human population hotspots",
    caption = "Data Source: IUCN and GPW v4",
    x = "Longitude",
    y = "Latitude"
  )

# Display the plot
print(plot2)

ggsave("../results/popu_mammal_overlap.jpg", plot = plot2, bg = "white", width = 12, height = 5)



##############################

library(ggplot2)
library(sf)
library(viridis)
library(ggplot2)
library(sf)
library(viridis)

plot2 <- ggplot() +
  # Base world map
  geom_sf(data = world_map, fill = "gray90", color = "white") +
  
  # Species range with discrete fill
  geom_sf(data = CDV_subset, aes(fill = sci_name), alpha = 0.5) +
  
  # Population density with continuous fill
  geom_tile(data = pop_df, aes(x = x, y = y, fill = population_density), alpha = 0.7) +
  
  # Scale for species (discrete)
  scale_fill_manual(
    values = scales::hue_pal()(length(unique(CDV_subset$sci_name))),
    name = "Species"
  ) +
  
  # Scale for population density (continuous)
  scale_fill_viridis_c(
    option = "plasma",
    name = "Population Density\n(Top 5%)"
  ) +
  
  # Minimal theme
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(face = "italic"),
    legend.title = element_text(face = "bold")
  ) +
  
  # Labels
  labs(
    title = "Global Distribution of Mammal Ranges and Population Density Hotspots",
    subtitle = "Overlap of Vulpes vulpes and CDV Species with Human Population Hotspots",
    caption = "Data Source: IUCN and GPW v4"
  )

# Display the map
print(plot2)


library(ggplot2)
library(sf)
library(viridis)

plot2 <- ggplot() +
  # Base world map
  geom_sf(data = world_map, fill = "gray90", color = "white") +
  
  # Species range with discrete fill
  geom_sf(data = CDV_subset, aes(fill = sci_name), alpha = 0.5) +
  
  # Population density with continuous fill
  geom_tile(data = pop_df, aes(x = x, y = y, fill = population_density), alpha = 0.7, inherit.aes = FALSE) +
  
  # Scale for species (discrete)
  scale_fill_manual(),
    name = "Species"
  ) +
  
  # Separate scale for population density (continuous)
  
  scale_fill_viridis_c(
    aesthetics = "fill_population_density",
    option = "plasma",
    name = "Population Density\n(Top 5%)"
  ) +
  
  # Minimal theme
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(face = "italic"),
    legend.title = element_text(face = "bold")
  ) +
  
  # Labels
  labs(
    title = "Global Distribution of Mammal Ranges and Population Density Hotspots",
    subtitle = "Overlap of Vulpes vulpes and CDV Species with Human Population Hotspots",
    caption = "Data Source: IUCN and GPW v4"
  )

# Display the map
print(plot2)

dev.off()


ls()
CDV_animals <- list("Procyon lotor", "Musterla putorius", "Pusa caspica", "Mustela lutreola", "Civettictis civetta", 
"Panthera pardus", "Panthera leo", "Lupulella mesomelas", "Otocyon megalotis", "Pusa sibirica")

CDV_subset <- mammals_data %>%
  filter(sci_name %in% CDV_animals)

print(CDV_subset)

focal <- mammals_data %>% 
  filter(sci_name == "Vulpes vulpes")

print(focal)

focal_density <- mask(pop, focal)
CDV_density <- mask(pop, CDV_subset)

focal_values <- extract(focal_density, focal)#, fun = median, na.rm = TRUE)
CDV_values <- extract(CDV_density, CDV_subset)#, fun = median, na.rm = TRUE)


# Load required libraries
library(sf)               # For handling spatial data
library(raster)           # For working with raster data
library(dplyr)            # For data manipulation
library(ggplot2)          # For plotting
library(rnaturalearth)    # For world map data
library(rnaturalearthdata) 
library(terra)            # For raster and vector data
library(exactextractr)    # For raster value extraction to polygons

# Set options and disable S2 geometry for sf
sf_use_s2(FALSE)

# Step 1: Load Data
# Load mammal range data
mammals_data <- st_read("../data/assessment/MAMMALS(1)/MAMMALS.shp") %>%
  st_transform(crs = 4326) # Ensure CRS is WGS84

# Load human population density raster
population_raster <- raster("../data/assessment/gpw_v4_population_density_rev11_2020_15_min.tif")

# Step 2: Identify Top 5% Population Density
# Calculate the 95th percentile of population density
quantile_value <- quantile(values(population_raster), 0.95, na.rm = TRUE)

# Create a raster showing only the top 5% population density
top_5_percent_raster <- population_raster
top_5_percent_raster[top_5_percent_raster < quantile_value] <- NA

# Convert raster to data frame for ggplot visualization
pop_df <- as.data.frame(rasterToPoints(top_5_percent_raster), xy = TRUE)# %>%
 # rename(population_density = layer)

# Step 3: Define Focal and Concerned Mammal Species
# List of concerned species
CDV_animals <- c("Procyon lotor", "Musterla putorius", "Pusa caspica", 
                 "Mustela lutreola", "Civettictis civetta", "Panthera pardus", 
                 "Panthera leo", "Lupulella mesomelas", "Otocyon megalotis", 
                 "Pusa sibirica", "Vulpes vulpes")

# Filter mammal data for focal and CDV species
focal_species <- mammals_data %>% filter(sci_name == "Vulpes vulpes")
CDV_subset <- mammals_data %>% filter(sci_name %in% CDV_animals)

# Step 4: Calculate Population Statistics for Each Mammal Range
# Function to extract statistics for a given species range
calculate_population_stats <- function(species_range, raster_data) {
  exactextractr::exact_extract(raster_data, species_range, 
                               c("mean", "median", "min", "max"))
}

# Example: Statistics for focal species
focal_stats <- calculate_population_stats(focal_species, population_raster)

# Step 5: Create the Map
# Load a world map for context
world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_map <- st_transform(world_map, crs = st_crs(focal_species))
CDV_subset <- st_transform(CDV_subset, crs = st_crs(focal_species))

if (!"x" %in% colnames(pop_df) | !"y" %in% colnames(pop_df)) {
    pop_df <- as.data.frame(rasterToPoints(top_5_percent_raster), xy = TRUE) %>%
              rename(population_density = layer)
}

head(pop_df)#

pop_df <- as.data.frame(rasterToPoints(top_5_percent_raster), xy = TRUE) %>%
  rename(population_density = layer)

  pop_df <- as.data.frame(rasterToPoints(top_5_percent_raster), xy = TRUE)
head(pop_df)
colnames(pop_df)
colnames(pop_df)[3] <- "population_density"
names(top_5_percent_raster)

colnames(pop_df)


CDV_subset <- mammals_data %>%
  filter(sci_name %in% CDV_animals)

print(CDV_subset)

focal <- mammals_data %>% 
  filter(sci_name == "Vulpes vulpes")

print(focal)


st_crs(focal)  # Check CRS of the focal species
st_crs(CDV_subset) 

focal_projected <- st_transform(focal, crs = 3857)  # or crs = 3395
CDV_subset_projected <- st_transform(CDV_subset, crs = 3857)  # or crs = 3395


overlap <- st_intersection(focal_projected, CDV_subset_projected)

overlapping_CDV_subset <- CDV_subset %>%
  filter(sci_name %in% overlap$sci_name)  # Only take species that are in the intersection

head(pop_df)




focal_projected <- st_transform(focal, crs = 3857)  # or crs = 3395
CDV_subset_projected <- st_transform(CDV_subset, crs = 3857)  # or crs = 3395


st_crs(pop_df) <- st_crs(world_map)

# Plot mammal range and population density hotspots
plot2 <- ggplot() +
    geom_sf(data = world_map, fill = "gray90", color = "white") + # World map
    # geom_sf(data = focal_projected, fill = "yellow", alpha = 0.5, label = "fox" )+
    # geom_sf(data = focal_species, fill = "blue", alpha = 0.5) + # Focal species range
    geom_sf(data = CDV_subset, aes(fill = sci_name), alpha = 0.5) + # CDV species range
    geom_raster(data = pop_df, aes(x = x, y = y), alpha = 0.1) + # Population hotspots
    scale_fill_viridis_c(
        name = "Population Density",
        option = "plasma"
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.text = element_text(face = "italic"),
        legend.title = element_text(face = "bold")
    ) +
    labs(
        title = "Global Distribution of Mammal Ranges and Population Density Hotspots",
        subtitle = "Overlap of Vulpes vulpes and CDV Species with Human Population Hotspots",
        caption = "Data Source: IUCN and GPW v4"
    )
# Display the map
print(plot2)

summary(pop_df$population_density)
# Load necessary libraries
library(ggplot2)
library(sf)
library(raster)
library(viridis)
# Load ggnewscale for multiple fill scales
library(ggnewscale)
download.packages("ggnewscale")

# Plot mammal ranges and population density hotspots
plot2 <- ggplot() +
    # Base world map
    geom_sf(data = world_map, fill = "lightgray", color = "black") +
    
    # Add individual species ranges with specific colors
    geom_sf(data = CDV_subset, aes(fill = sci_name), alpha = 0.5) +
    scale_fill_manual(
        values = c(
            "Civettictis civetta" = "green",
            "Mustela lutreola" = "yellow",
            "Panthera leo" = "orange",
            "Panthera pardus" = "blue",
            "Pusa caspica" = "purple",
            "Pusa sibirica" = "#D50060", 
            "Vulpes vulpes" = "pink"
        ),
        name = "CDV Species"
    ) +
    
    # Add a new fill scale for population density
    new_scale_fill() +
    geom_tile(data = pop_df, aes(x = x, y = y), alpha = 0.3) +
    scale_fill_viridis_c(
        name = "Population Density",
        option = "plasma",
        limits = c(quantile(pop_df$population_density, 0.05), max(pop_df$population_density))
    ) +

    # Add labels for axes and title
    labs(
        title = "Global Distribution of Mammal Ranges and Population Density Hotspots",
        subtitle = "Overlap of Vulpes vulpes and CDV Susceptible Species with Human Population Hotspots",
        caption = "Data Source: IUCN and GPW v4",
        x = "Longitude",
        y = "Latitude"
    ) +
    
    # Minimal theme for clean visualization
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.text = element_text(face = "italic"),
        legend.title = element_text(face = "bold")
    )

# Display the plot
print(plot2)


# Load necessary libraries
library(ggplot2)
library(viridis)


# Plot mammal range and population density hotspots
plot2 <- ggplot() +
    # World map
    geom_sf(data = world_map, fill = "gray90", color = "white") +
    
    # CDV species range with color aesthetic
    geom_sf(data = CDV_subset, aes(color = sci_name), alpha = 0.5) +
    
    # Population density hotspots with fill aesthetic
    geom_raster(data = pop_df, aes(x = x, y = y, fill = population_density), alpha = 0.1) +
    
    # Scales
    scale_color_manual(
        values = scales::hue_pal()(length(unique(CDV_subset$sci_name))),
        name = "CDV Species"
    ) +
    scale_fill_viridis_c(
        option = "plasma",
        name = "Population Density"
    ) +
    
    # Minimal theme
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.text = element_text(face = "italic"),
        legend.title = element_text(face = "bold")
    ) +
    
    # Add labels
    labs(
        title = "Global Distribution of Mammal Ranges and Population Density Hotspots",
        subtitle = "Overlap of Vulpes vulpes and CDV Species with Human Population Hotspots",
        caption = "Data Source: IUCN and GPW v4"
    )

# Display the map
print(plot2)




ggplot() +
  geom_sf(data = world_map, fill = "gray", color = "white")


ggplot() +
  geom_sf(data = focal_species, fill = "blue", alpha = 0.5)
dev.off()

ggplot() +
  geom_sf(data = CDV_subset, aes(fill = sci_name), alpha = 0.5)
  
ggplot() +
  geom_raster(data = pop_df, aes(x = x, y = y), alpha = 0.3)

# Step 6: Save Outputs
# Save the map
ggsave("hotspots_map.png", plot = plot2, width = 10, height = 8, dpi = 300)

# Save population statistics
write.csv(focal_stats, "focal_population_stats.csv")
# Plot mammal range and population density hotspots

plot2 <- ggplot() +
  geom_sf(data = world_map, fill = "gray90", color = "white") + # World map
  geom_sf(data = CDV_subset, aes(fill = sci_name), alpha = 0.5) + # CDV species range
  geom_tile(data = pop_df, aes(x = x, y = y, fill = population_density), alpha = 0.7) + # Population hotspots
  scale_fill_manual(values = scales::hue_pal()(length(unique(CDV_subset$sci_name))), 
                    name = "Species") + # Discrete legend for species
  scale_fill_viridis_c(aesthetics = "fill_population_density", option = "plasma", 
                       name = "Population Density\n(Top 5%)") + # Continuous legend for population density
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(face = "italic"),
    legend.title = element_text(face = "bold")
  ) +
  labs(
    title = "Global Distribution of Mammal Ranges and Population Density Hotspots",
    subtitle = "Overlap of Vulpes vulpes and CDV Species with Human Population Hotspots",
    caption = "Data Source: IUCN and GPW v4"
  )

# Display the map
print(plot2)


# Ensure CRS matches between mammals_data and population_raster
mammals_data <- st_transform(mammals_data, crs = crs(population_raster))

# Define the CDV species of interest
CDV_subset_species <- c("Procyon lotor", "Mustela putorius", "Pusa caspica",
                        "Mustela lutreola", "Civettictis civetta", 
                        "Panthera pardus", "Panthera leo", "Lupulella mesomelas", 
                        "Otocyon megalotis", "Pusa sibirica")

# Filter mammals_data to include only CDV species
CDV_subset <- mammals_data %>% filter(sci_name %in% CDV_subset_species)

# Initialize an empty list to store results
population_density_stats <- list()

# Loop through each CDV species' range
for (species in unique(CDV_subset$sci_name)) {
  # Subset the data for the current species
  species_range <- CDV_subset %>% filter(sci_name == species)
  
  # Extract population density statistics for the species' range
  stats <- exact_extract(population_raster, species_range, c("mean", "median", "min", "max"))
  
  # Combine stats with species name
  species_stats <- data.frame(
    sci_name = species,
    mean_density = stats$mean,
    median_density = stats$median,
    min_density = stats$min,
    max_density = stats$max
  )
  
  # Append to the list
  population_density_stats[[species]] <- species_stats
}

# Combine all results into a single data frame
final_stats <- bind_rows(population_density_stats)

# View the final table
print(final_stats)

# Save the table as a CSV file
write.csv(final_stats, "cdv_population_density_stats.csv", row.names = FALSE)

# Ensure CRS matches between mammals_data and population_raster
mammals_data <- st_transform(mammals_data, crs = crs(population_raster))

# Define the CDV species of interest
CDV_subset_species <- c("Procyon lotor", "Mustela putorius", "Pusa caspica",
                        "Mustela lutreola", "Civettictis civetta", 
                        "Panthera pardus", "Panthera leo", "Lupulella mesomelas", 
                        "Otocyon megalotis", "Pusa sibirica")

# Filter mammals_data to include only CDV species
CDV_subset <- mammals_data %>% filter(sci_name %in% CDV_subset_species)

# Initialize an empty list to store results
population_density_stats <- list()

# Loop through each unique species in the CDV subset
for (species in unique(CDV_subset$sci_name)) {
  # Subset the data for the current species
  species_range <- CDV_subset %>% filter(sci_name == species)
  
  # Extract population density statistics for the species' range
  stats <- exact_extract(population_raster, species_range, c("mean", "median", "min", "max"))
  
  # Aggregate statistics for the species
  aggregated_stats <- data.frame(
    sci_name = species,
    mean_density = mean(stats$mean, na.rm = TRUE),     # Average of mean densities
    median_density = median(stats$median, na.rm = TRUE), # Median of medians
    min_density = min(stats$min, na.rm = TRUE),         # Minimum of mins
    max_density = max(stats$max, na.rm = TRUE)          # Maximum of maxs
  )
  
  # Append to the list
  population_density_stats[[species]] <- aggregated_stats
}

# Combine all results into a single data frame
final_stats <- bind_rows(population_density_stats)

# View the final table
print(final_stats)

# Save the table as a CSV file
write.csv(final_stats, "cdv_population_density_stats.csv", row.names = FALSE)

library(ggplot2)
library(sf)
library(viridis)

plot2 <- ggplot() +
  # Base world map
  geom_sf(data = world_map, fill = "gray90", color = "white") +

  geom_sf(data = focal_projected, aes(fill = "yellow"), alpha = 0.5)+
  
  # Species range with discrete fill
  geom_sf(data = CDV_subset, aes(fill = sci_name), alpha = 0.5) +
  
  # Population density with continuous fill
  geom_tile(data = pop_df, aes(x = x, y = y, fill = population_density), alpha = 0.7, inherit.aes = FALSE) +
  
  # Scale for species (discrete)
  scale_fill_manual(
    values = scales::hue_pal()(length(unique(CDV_subset$sci_name))),
    name = "Species"
  ) +
  
  # Separate scale for population density (continuous)
  scale_fill_viridis_c(
    aesthetics = "fill",
    option = "plasma",
    name = "Population Density (Top 5%)"
  ) + guides(
    fill = guide_legend(order = 1),  # For species
    fill_density = guide_colorbar(order = 2)  # For population dens
  )+
  # Minimal theme
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(face = "italic"),
    legend.title = element_text(face = "bold")
  ) +
  
  # Labels
  labs(
    title = "Global Distribution of Mammal Ranges and Population Density Hotspots",
    subtitle = "Overlap of Vulpes vulpes and CDV Species with Human Population Hotspots",
    caption = "Data Source: IUCN and GPW v4"
  )

# Display the map
print(plot2)

dev.off()

library(ggplot2)
library(sf)
library(viridis)
library(ggplot2)
library(sf)
library(viridis)
library(ggnewscale)
download.packages("ggnewscale")

plot2 <- ggplot() +
  # Base world map
  geom_sf(data = world_map, fill = "gray90", color = "white") +
  
  # Species range with discrete fill
  geom_sf(data = CDV_subset, aes(fill = sci_name), alpha = 0.5) +
  
  # Population density with continuous fill
  geom_tile(data = pop_df, aes(x = x, y = y, density_fill = population_density), alpha = 0.7, inherit.aes = FALSE) +
  
  # Scale for species (discrete)
  scale_fill_manual(
    values = scales::hue_pal()(length(unique(CDV_subset$sci_name))),
    name = "Species"
  ) +
  #scale_fill_viridis_c(
   # option = "plasma",
    #name = "Population Density"
  #) +
  
  # Minimal theme
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(face = "italic"),
    legend.title = element_text(face = "bold")
  ) +
  
  # Labels
   labs(
    title = "Global Distribution of Mammal Ranges and Population Density Hotspots",
    subtitle = "Overlap of Vulpes vulpes and CDV Species with Human Population Hotspots",
    caption = "Data Source: IUCN and GPW v4"
  )

# Display the map
print(plot2)

ggsave("../results/hotspots_map_final.png", plot = plot2, width = 10, height = 8, dpi = 300, bg = "white")








 
pop <- rast("../data/assessment/gpw_v4_population_density_rev11_2020_15_min.tif")
 
print(pop)

mammals_data <- st_read("../data/assessment/MAMMALS(1)/MAMMALS.shp")


CDV_animals <- list("Procyon lotor", "Musterla putorius", "Pusa caspica", "Mustela lutreola", "Civettictis civetta", 
"Panthera pardus", "Panthera leo", "Lupulella mesomelas", "Otocyon megalotis", "Pusa sibirica", "Vulpes vulpoes")

CDV_subset <- mammals_data %>%
  filter(sci_name %in% CDV_animals)

print(CDV_subset)

focal <- mammals_data %>% 
  filter(sci_name == "Vulpes vulpes")

print(focal)


  # Check CRS of the focal species
st_crs(CDV_subset) 

focal_projected <- st_transform(focal, crs = 3857)  # or crs = 3395
CDV_subset_projected <- st_transform(CDV_subset, crs = 3857)  # or crs = 3395

 

ss_pop <- crop(pop, CDV_subset)

head(ss_pop)
glimpse(ss_pop)
 
ss_pop<- project(ss_pop, "ESRI:102022")
 
print(ss_pop)
 
# initial visual
breaks <- quantile(values(ss_pop, na.rm = TRUE), probs = seq(0, 1, by = 0.1), na.rm = TRUE)
 
breaks <- c(0,20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 8000)
 
breaks <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 8000)
 
breaks <- c(0, 100, 200, 300, 400, 8000)
 
plot(ss_pop, breaks = breaks)
 
## extract popdens data
 
# median
mammals_popu$medianoverlap <- extract(ss_pop, CDV_subset, fun = median, ID =FALSE)
 
glimpse(mammals_popu)
 
glimpse(sub_sah_mammals_rob$medianoverlap[[1]])
 
# range
# Extract both min and max values
 
# define function
range <- function(values) {
  min = round(min(values))
  max = round(max(values))
  return(paste0(min, "-", max))
}
 
 
sub_sah_mammals_rob$humanoverlap_range <- extract(ss_pop, sub_sah_mammals_rob, fun = range, ID = FALSE)
 
glimpse(sub_sah_mammals_rob)
glimpse(sub_sah_mammals_rob$humanoverlap_range[[1]])
 
 
# top 5% of human density
 
boxplot(values(ss_pop))
 
top5density <- quantile(values(ss_pop), probs = 0.95, na.rm = TRUE)
 
print(top5density) #245
 
high_pops <- ifel(ss_pop >= 245, ss_pop, NA)
 
range(values(high_pops))
str(values(high_pops))
 
summary(values(ss_pop >= 245))
summary(values(high_pops))
 
str(ss_pop_df)
 
plot(high_pops)
 
highpops_vector <- as.polygons(high_pops, dissolve = TRUE)
 
# Convert to an sf object
highpops_sf <- st_as_sf(highpops_vector)
 
# Dissolve all polygons into one
dissolved_highpops <- st_union(highpops_sf)
 
# Plot the dissolved polygon
plot(dissolved_highpops, col = "red", border = "black")
 
