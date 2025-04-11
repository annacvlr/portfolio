# CDV species overlap and spillover risk Mapping

## Description
This project investigates the spatial overlap between the red fox (Vulpes vulpes) and various carnivorous mammal species susceptible to Canine Distemper Virus (CDV). It further integrates global human population density data to highlight potential zoonotic spillover risk zones.
Using GIS methods in R, species range shapefiles are compared, intersected, and analysed to quantify overlap. Human population density hotspots are then layered to identify areas of high spillover potential.

## Objectives 
- Identify species susceptible to CDV that spatially overlap with Vulpes vulpes
- Quantify overlap areas and compute relative proportions
- Overlay CDV overlap zones with top 5% human population density hotspots
- Produce publication-ready maps and tables for visualisation and analysis

## Languages
R (v4.3.3) 
Bash 

    
## Dependencies
- Spatial packages: sf, raster, terra, ggplot2, ggpattern, rnaturalearth, exactextractr, viridis, ggnewscale
- Data manipulation: dplyr, tidyr, kableExtra

## Project Structure and Usage
- `code/`: Scripts for cleaning, analysis, and visualisation.
- `data/`: Cleaned datasets and metadata (raw data not uploaded due to size/privacy).
- `results/`: Plots and summary tables.
- `sandbox/`: Scratch code and trial notebooks.

## Data Sources
- Species ranges: IUCN Red List shapefiles (MAMMALS.shp)
- Population density: GPW v4 (15-min resolution raster)

## Code
**code/overlap_tabulate.R**: Filters CDV species and Vulpes vulpes, projects their geometries, calculates intersecting areas, and summarises overlap metrics into tables and plots.

**code/plot_felidae.R**: Plots the global distribution of CDV-susceptible species (including Vulpes vulpes), highlighting overlaps with population hotspots and generating thematic maps.

**code/pop_overlap.R**: Integrates CDV species ranges and population density data into layered visualisations, calculates top 5% hotspots, computes population statistics per species, and generates the final mapping outputs.

## Results
Tables:
- cdv_population_density_stats.csv: Mean/median population density for each CDV species range
- overlap_summary.csv: Proportional and absolute overlap values between focal and CDV species

Figures:
- hotspots_map_final.png: Global distribution of CDV species and population hotspots
- Vulpes_vulpes_overlap_table.pdf: Table showing overlap metrics with red fox

Report:
- CDV_spillover.pdf: final report of the project

## Author
   Anna Cavalieri Canosa
    	acavaliericanosa@gmail.com

