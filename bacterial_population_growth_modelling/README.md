# CMEE MiniProject

## Description
MiniProject carried out in Term 2 of MSc Computational Methods in Ecology and Evolution. Fitting different models to bacterial population growth data and evaluate which model is the best. I fit a Quadratic, Logistic and Gompertz model to both non-transformed and log-transformed population data. 

## Languages
R (v4.3.3) 
Python (v3.12.9) 
Bash 
LaTex 

    
## Dependencies
- **ipdb** : for enhanced debugging
- **Jupyter** : to create online files with code and writing
- **ggplot2**: Data visualization
- **minpack.lm**: Fitting nonlinear curves (Levenberg-Marquardt algorithm).
- **dyplr**:Data manipulation (filtering, grouping, summarizing)
- **data.table**:Handling large datasets efficiently
- **gridExtra**:Creating multi-panel plots in ggplot2

## Installation
pip install jupyter
pip install pandas 

    

## Project Structure and Usage
- `code/`: Scripts for cleaning, analysis, and visualisation.
- `data/`: Cleaned datasets and metadata (raw data not uploaded due to size/privacy).
- `results/`: Plots and summary tables.
- `sandbox/`: Scratch code and trial notebooks.



## Code
- **1_wrangle_data.py**: Script to wrangle dataset and create a new csv with columns of interest.
- **2_model_fitting_plot.R**: Fitting models to dataset with a for loop. Fitting the models, plotting them, saving both statistic metrics and plotted graphs. 
- **3_model_analysis.R**: Analyse statistical metrics to determine "winning" models. Group and filter data to have models that fit (R2 > 0.5).
- **miniproject.tex**:Mini Project report LaTeX file to be compiled into a pdf. 
- **ReferencesMiniProj.bib**: All the references used in main.tex.
- **run_latex.sh**: Bash script to run the LaTeX file into pdf.
- **run_MiniProject.sh**: Bash script that runs all of the scripts required for the miniproject.
- **WorkFlowMiniProj.ipynb**:Jupyter Notebook initially used to view the data, workflow and decision making.

## Author
   Anna Cavalieri Canosa
    	acavaliericanosa@gmail.com

