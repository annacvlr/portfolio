#!/bin/bash
# Author: anna.cavalieri-canosa24@imperial.ac.uk
#Script: 
#Date: March 2025

# Runs the CMEE MiniProject Latex file



pdflatex miniproject.tex
pdflatex miniproject.tex
biber miniproject
pdflatex miniproject.tex
pdflatex miniproject.tex
evince miniproject.pdf &

## Cleanup
rm *~
rm *.aux
rm *.dvi
rm *.log
rm *.nav
rm *.out
rm *.snm
rm *.toc

