# portfolio
Data science &amp; ecology projects 

# Structure
Each folder represents a standalone data science project, organised with subfolders as follows:
- `code/`: Scripts for cleaning, analysis, and visualisation.
- `data/`: Cleaned datasets and metadata (raw data not uploaded due to size/privacy).
- `results/`: Plots and summary tables.
- `sandbox/`: Scratch code and trial notebooks.

# Languages
R (v4.3.3), Python (v3.12.9), Bash, LaTeX


### `bacterial_population_growth_modelling`
This project focused on fitting multiple models to bacterial population growth data and evaluating which model best explains the observed dynamics. I tested a **Quadratic**, **Logistic**, and **Gompertz** model on both raw and log-transformed population data.

**Languages used**:  
R (v4.3.3), Python (v3.12.9), Bash, LaTeX

**Key dependencies**:
- `ipdb`: debugging in Python
- `Jupyter`: exploratory analysis and reporting
- `ggplot2`: data visualisation
- `minpack.lm`: non-linear model fitting
- `dplyr`, `data.table`: data wrangling
- `gridExtra`: multi-panel plots

**Code**:
- `1_wrangle_data.py`: Prepares dataset for modelling
- `2_model_fitting_plot.R`: Fits models, saves plots and stats
- `3_model_analysis.R`: Analyses model results to find best fits (e.g., R² > 0.5)
- `miniproject.tex`: LaTeX report of methods, results, and discussion
- `run_latex.sh`, `run_MiniProject.sh`: Automate report generation and full workflow
- `WorkFlowMiniProj.ipynb`: Jupyter notebook outlining workflow logic

**Results**:
- Multi-panel model fits
- Summary statistics for each model and dataset
- Final PDF report with figures and references

### `GIS_bioinformatics`
This project investigated the spatial overlap between the red fox (Vulpes vulpes) and various carnivorous mammal species susceptible to Canine Distemper Virus (CDV). It further integrates global human population density data to highlight potential zoonotic spillover risk zones.
Using GIS methods in R, species range shapefiles are compared, intersected, and analysed to quantify overlap. Human population density hotspots are then layered to identify areas of high spillover potential.

**Objectives**
- Identify species susceptible to CDV that spatially overlap with Vulpes vulpes
- Quantify overlap areas and compute relative proportions
- Overlay CDV overlap zones with top 5% human population density hotspots
- Produce publication-ready maps and tables for visualisation and analysis

**Languages used** 
R (v4.3.3), Bash 

    
**Dependencies**
- Spatial packages: sf, raster, terra, ggplot2, ggpattern, rnaturalearth, exactextractr, viridis, ggnewscale
- Data manipulation: dplyr, tidyr, kableExtra

**Code**
- `overlap_tabulate.R` Filters CDV species and Vulpes vulpes, projects their geometries, calculates intersecting areas, and summarises overlap metrics into tables and plots.

- `plot_felidae.R`: Plots the global distribution of CDV-susceptible species (including Vulpes vulpes), highlighting overlaps with population hotspots and generating thematic maps.

- `pop_overlap.R`: Integrates CDV species ranges and population density data into layered visualisations, calculates top 5% hotspots, computes population statistics per species, and generates the final mapping outputs.


**Results**:
Tables:
- cdv_population_density_stats.csv: Mean/median population density for each CDV species range
- overlap_summary.csv: Proportional and absolute overlap values between focal and CDV species

Figures:
- hotspots_map_final.png: Global distribution of CDV species and population hotspots
- Vulpes_vulpes_overlap_table.pdf: Table showing overlap metrics with red fox

Report:
- CDV_spillover.pdf: final report of the project

**Author**: Anna Cavalieri Canosa  
📧 acavaliericanosa@gmail.com

