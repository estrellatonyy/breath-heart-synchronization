# Breath-Heart Synchronization: RSA Analysis 

This repository contains the source code for analyzing and visualizing Respiratory Sinus Arrhythmia (RSA) developed in R/Shiny and Python. It also contains artifact detection in cardiorespiratory synchronization signals during experimental procedure. 

This project is part of a doctoral thesis, and the associated datasets are described in Zenodo and CORA.

Estrella, T., Ramos Castro, J., Losilla, J.-M., & Capdevila, L. (2026). A dataset of heart rate variability and respiratory activity during paced and free breathing (Version 1.0) [Dataset]. Zenodo. https://doi.org/10.5281/ZENODO.19115192


---

## Repository Structure

The project is organized as follows to ensure data transparency and full reproducibility:

- ArtifactDetection/: Scripts and algorithms developed for artifact detection and filtering in heart rate variability signals. Python.
- convergent_validity_ML/: Source code corresponding to the convergent validity analysis using Machine Learning approaches.
- BreathHeart_App/: Main folder hosting the Shiny web application code (`app.R`) for interactive cardiorespiratory synchronization analysis.
- HRV_indexes/: Scripts dedicated to the calculation and extraction of Heart Rate Variability (HRV) metrics.
- RR_Segmentation/: Script designed for the segmentation of RR intervals according to the optimal resonance frequency protocol.
- Respiration_Cleaning/: Dedicated pipeline for filtering, cleaning, and processing raw respiratory signals.

---

## Requirements & Installation

To run the application locally, you need to have **R** (version 4.0 or higher) and **RStudio** installed on your machine.
