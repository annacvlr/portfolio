##Script to plot the polygon ranges of mammals, with a special interest in the Felidae,

# Load required libraries
#install.packages("rnaturalearth") install the packages 
#install.packages("rnaturalearthdata")
library(sf)         # For handling shapefiles
library(ggplot2)    # For plotting
library(dplyr)      # For data manipulation
library(rnaturalearth)  # For world map data
library(rnaturalearthdata)
library(raster)
#install.packages("ggpattern")
library(ggpattern)


mammals_data<- st_read("../data/assessment/MAMMALS(1)/MAMMALS.shp")
print(mammals_data)
mammals_names <- mammals_data$sci_name 
print(mammals_names)

#felidae_data<- c("Neofelis nebulosa", "Caracal aurata", "Leopardus guttulus", "Lynx pardinus", "Panthera tigris", "Panthera uncia",
                #"Catopuma badia", "Neofelis diardi", "Panthera pardus", "Leopardus jacobita", "Leopardus tigrinus", "Prionailurus viverrinus", "Felis nigripes", "Acinonyx jubatus",
                #"Panthera leo", "Leopardus guigna", "Felis bieti", "Prionailurus planiceps", "Felis")
#felidae_subset <- felidae_data %>%
                #filter(sci_name %in% felidae_spp) #filter subset of felidae that are of interest (IUCN categories: VU, EN, CR or DD)


CDV_animals <- c("Procyon lotor", "Musterla putorius", "Pusa caspica", "Vulpes vulpes", "Mustela lutreola", "Civettictis civetta", 
"Panthera pardus", "Panthera leo", "Lupulella mesomelas", "Otocyon megalotis", "Pusa sibirica")

CDV_subset <- CDV_animals[CDV_animals %in% mammals_names]
print(CDV_subset)
print(mammals_data)

CDV_animals_df <- data.frame(sci_name = CDV_animals)

# Filter based on the scientific names in mammals_data
CDV_subset <- CDV_animals_df %>%
                filter(sci_name %in% mammals_data$sci_name)

# Print the result
print(CDV_subset)


class(CDV_subset) 


CDV_subset <- mammals_data %>%
  filter(sci_name %in% CDV_animals)


map <- ne_countries(scale = "medium", returnclass = "sf") #load world map

ggplot() +
  geom_sf(data = map, fill = "gray90", color = "white") +  # World map
  geom_sf(data = CDV_subset, aes(fill = sci_name), color = NA, alpha = 0.6) +  # Felidae shapefile
  scale_fill_viridis_d(name = "CDV susceptible species") +  # Color scale
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Global Distribution of CDV susceptible species",
       subtitle = "Overlapping distributions with fox (Vulpes vulpes)",
       caption = "Data source: IUCN")


#####final overlap distributiion of species
plot <- ggplot() +
  geom_sf(data = map, fill = "gray90", color = "white") +  # World map
  geom_sf(data = filter(CDV_subset, sci_name != "Vulpes vulpes"), aes(fill = sci_name), alpha = 0.5, color = NA ) +  # Other species
  #geom_sf(data = filter(CDV_subset, sci_name == "Vulpes vulpes"), fill = "red", alpha = 0.5, color = "black", size = 0.3) +  # Highlight fox
  geom_sf_pattern(data = filter(CDV_subset, sci_name == "Vulpes vulpes"), 
                  aes(pattern = "stripes", fill = "Vulpes vulpes"),  # Use pattern
                  pattern_density = 0.01,  # Control density of the pattern (smaller = sparser)
                  pattern_angle = 90,  # Angle of the pattern lines (you can adjust this)
                  alpha = 0.2,  # Transparency of the pattern
                  color = "black",  # Border color for pattern
                  size = 0.1, 
                  fill = NA) +  # Border size for the pattern
  scale_fill_viridis_d(name = "Species", option = "C") +
  guides(fill = guide_legend(title = "CDV Susceptible Species"), 
        pattern = guide_legend(title = "Vulpes vulpes")) + 
  theme_minimal() +
  theme(legend.position = "bottom" ,
    legend.text = element_text(face = "italic"), 
    legend.title = element_text(face = "bold")) +
  labs(
    title = "Global Distribution of Susceptible Species to CDV",
    subtitle = expression("Overlapping distributions of Susceptible Species with " * italic ("Vulpes vulpes")),
    caption = "Data source: IUCN"
  )
plot
ggsave("../results/CDV_species_dsitribution.png", plot = plot, dpi=300, width = 12, height = 6, bg= "white")

dev.off() 

focal <- mammals_data %>% 
  filter(sci_name == "Vulpes vulpes")
class(focal)

class(CDV_subset)


CDV_subset <- mammals_data %>%
  filter(sci_name %in% CDV_animals & sci_name != "Vulpes vulpes")



tabulated_overlap <- overlap %>% 
  group_by(sci_name) %>% 
  summarise(
    total_overlapping_area = sum(area, na.rm = TRUE),
    num_overlapping_polygons = n()
  )

library(sf)
library(dplyr)

# Filter for focal species and CDV animals
focal_species <- mammals_data %>% filter(sci_name == "Vulpes vulpes")
cdv_species <- mammals_data %>% filter(sci_name %in% CDV_animals)

# Initialize a results dataframe
results <- data.frame(
  Species = character(),
  Total_Range_Area = numeric(),
  Overlap_Area = numeric(),
  Prop_Focal_Overlap = numeric(),
  Prop_Concern_Overlap = numeric()
)

# Loop through each species in CDV_animals to compute overlap
for (species in unique(cdv_species$sci_name)) {
  # Extract species data
  species_data <- cdv_species %>% filter(sci_name == species)
  
  # Calculate intersection
  intersection <- st_intersection(focal_species$geometry, species_data$geometry)
  
  # Calculate areas
  focal_area <- st_area(focal_species$geometry)
  species_area <- st_area(species_data$geometry)
  overlap_area <- st_area(intersection)
  
  # Compute proportions
  prop_focal_overlap <- as.numeric(overlap_area / focal_area)
  prop_species_overlap <- as.numeric(overlap_area / species_area)
  
  # Add to results
  results <- rbind(results, data.frame(
    Species = species,
    Total_Range_Area = as.numeric(species_area),
    Overlap_Area = as.numeric(overlap_area),
    Prop_Focal_Overlap = prop_focal_overlap,
    Prop_Concern_Overlap = prop_species_overlap
  ))
}

# View results
print(results)





species_list <- list("Procyon lotor", "Musterla putorius", "Pusa caspica", "Mustela lutreola", "Civettictis civetta", 
"Panthera pardus", "Panthera leo", "Lupulella mesomelas", "Otocyon megalotis", "Pusa sibirica")

species_subset <- mammals_data %>% filter(sci_name %in% species_list)

class(CDV_animals)


# Function to calculate overlap proportion
calculate_overlap <- function(species_range, focal_range) {
  intersection <- st_intersection(focal_range, species_range)
  area_overlap <- st_area(intersection)
  area_focal <- st_area(focal_range)
  return(as.numeric(area_overlap / area_focal))
}

# Apply function to each species


focal_species <- mammals_data %>% filter(sci_name == "Vulpes vulpes")

overlap_results <- sapply(species_list, calculate_overlap, focal_range = focal_species)
overlap_results

proportion_overlap <- overlap_results[1, ]
total_overlap_area <- overlap_results[2, ]

overlap_table <- data.frame(
  Species = colnames(overlap_results),
  ProportionOverlap = proportion_overlap,
  TotalOverlapArea = total_overlap_area
)

print(overlap_table)










#population of humans map
pop <- raster("../data/assessment/gpw_v4_population_density_rev11_2020_15_min.tif")
pop_df <- as.data.frame(pop, xy = TRUE, na.rm = TRUE)
colnames(pop_df) <- c("longitude", "latitude", "population_density")
breaks <- c(0, 1, 10, 50, 100, 500, 1000, 5000)# max(pop_df$population_density, na.rm = TRUE))
breaks_rescaled <- scales::rescale(breaks)
print(pop)
print(df_pop)

#plot popu w plot 
#not amazing
plot(pop,
     col = hcl.colors(length(breaks) - 1, "YlOrRd", rev = TRUE),
     breaks = breaks,  # Use custom breaks
     main = "Population Density (2020)")
dev.off()


# Plot population density using ggplot 
##not the best plot
ggplot(pop_df)+
  geom_raster(aes(x = longitude, y = latitude, fill = population_density)) +
  
  # Custom color scale from light blue to dark blue
  scale_fill_gradientn(
        colours = c("lightblue", "blue", "darkblue"), #low = "lightblue", high = "darkblue", 
        values = breaks_rescaled,
        name = "Population Density", 
        breaks = breaks,
        labels = breaks, 
        limits = c(0,5000)) +
  
  # Set the title and other map aesthetics
  theme_minimal() +
  labs(
    title = "Population Density (2020)",
    subtitle = "Data source: GPWv4",
    x = "Longitude",
    y = "Latitude"
  ) +
  guides(fill = guide_colorbar(barheight = 30, barwidth = 3))


dev.off()



# Define breaks for discrete categories of population density
breaks <- c(0, 1, 5, 10, 50, 100, 500, 1000, 5000)





#population of humans map
pop <- raster("../data/assessment/gpw_v4_population_density_rev11_2020_15_min.tif")
pop_df <- as.data.frame(pop, xy = TRUE, na.rm = TRUE)
colnames(pop_df) <- c("longitude", "latitude", "population_density")
breaks <- c(0, 1, 10, 50, 100, 500, 1000, 5000)# max(pop_df$population_density, na.rm = TRUE))
breaks_rescaled <- scales::rescale(breaks)
print(pop)
print(df_pop)


###HUMAN POPULATION MAP
# Cut population_density into discrete bins
# Create population_density_bin by cutting population_density into bins
pop_df$population_density_bin <- cut(pop_df$population_density,
                                      breaks = c(0, 1, 5, 10, 50, 100, 500, 1000, 5000),
                                      labels = c("0-1", "1-5", "5-10", "10-50", "50-100", "100-500", "500-1000", "1000-5000"),
                                      right = FALSE)  # Adjust boundaries as needed

# Verify the new column
head(pop_df)



# Plot population density using ggplot 
ggplot(pop_df) +
  geom_raster(aes(x = longitude, y = latitude, fill = population_density_bin)) +
  
  # Custom color scale for discrete bins
  scale_fill_manual(
    values = c("lightblue", "skyblue", "dodgerblue", "#176be9e1",  "royalblue",  "blue","darkblue", "navy"),  # Custom colors for each category
    name = "Human Population Density",
    labels = c("0-1", "1-5", "5-10", "10-50", "50-100", "100-500", "500-1000", "1000-5000")  # Labels for legend
  ) +
  
  # Adjust the legend size
  guides(fill = guide_legend(title = "Population Density", keyheight = 1.5, keywidth = 1.5)) +

  # Set the title and other map aesthetics
  theme_minimal() +
  labs(
    title = "Population Density (2020)",
    subtitle = "Data source: GPWv4",
    x = "Longitude",
    y = "Latitude"
  )
  
 
dev.off()



focal <- mammals_data %>% 
  filter(sci_name == "Vulpes vulpes")




###Fox vs human popu
plot2 <- ggplot() +
  geom_sf(data = map, fill = "gray90", color = "white") +  # World map
  geom_sf(data = focal, aes(fill = sci_name), alpha = 0.5, color = NA ) +  # Other species
  #geom_sf(data = filter(CDV_subset, sci_name == "Vulpes vulpes"), fill = "red", alpha = 0.5, color = "black", size = 0.3) +  # Highlight fox
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




glimpse(pop_df)
print(pop)


library(exactextractr)

pop_density <- pop_df$population_density
focal_density <- exact_extract(pop_density, focal, "mean")  # Extract population density for focal species

# For each species in the species subset
CDV_density <- lapply(CDV_subset$sci_name, function(species_name) {
  species_data <- CDV_subset[CDV_subset$sci_name == species_name, ]
  exact_extract(pop_density, species_data, "mean")
})

# Create a summary table for the species ranges and median population density
density_summary <- data.frame(
  species = CDV_subset$sci_name,
  median_density = sapply(CDV_density, function(x) median(x, na.rm = TRUE))
)

# Find the top 5% of population density
top_5_percent_threshold <- quantile(pop_density[], probs = 0.95, na.rm = TRUE)

# Mask the raster to only show areas with population density in the top 5%
hotspots <- pop_density
hotspots[hotspots < top_5_percent_threshold] <- NA 



ggplot() +
  geom_sf(data = map, fill = "gray90", color = "white") +  # World map
  geom_sf(data = filter(CDV_subset, sci_name != "Vulpes vulpes"), aes(fill = sci_name), alpha = 0.5, color = NA ) +  # Other species
  geom_raster(data = hotspots, aes(x = longitude, y = latitude, fill = layer)) +  # Hotspots
  scale_fill_viridis_c(name = "Human Population Density") +
  theme_minimal() +
  labs(
    title = "Hotspots of Spillover Risk",
    subtitle = "Top 5% Population Density within Species Ranges",
    x = "Longitude",
    y = "Latitude"
  )