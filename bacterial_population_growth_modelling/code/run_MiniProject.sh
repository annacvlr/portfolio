#!/bin/bash
# Author: anna.cavalieri-canosa24@imperial.ac.uk
#Script: run_MiniProject.sh
#Date: March 2025

# Runs the CMEE MiniProject 

#data wrangling
echo "run 1_wrangle_data.py"
python3 1_wrangle_data.py

#model fitting and plotting
echo "run 2_model_fitting_plot.R"
Rscript 2_model_fitting_plot.R


#statistical analysis
echo "running 3_model_analysis.R"
Rscript 3_model_analysis.R


#LaTeX 
echo "running latex.sh"
bash run_latex.sh main

rm miniproject.blg
rm miniproject.run.xml
rm miniproject.bbl
rm miniproject.bcf





