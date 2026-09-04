# master-thesis-auditory-cognitive-training-erp
This repository contains the data processing and statistical analysis scripts for my Master's thesis at the University of Lübeck.

## Data Availability & Reproducibility Note
The scripts in this repository are provided for full transparency and documentation purposes. Please note that they cannot be executed without the original raw dataset, which is withheld due to participant data privacy.

## File Structure
ERP_extraction.m
The MatLab file reads the BrainVision export files and extracts the ERP latencies for P50, N100, and P200 component, as well es the amplitude values for the Cz Elektrode. Further statistical analysis is conducted in the R skripts.
ERP_Komponenten_final.R
Reads the previously generated .csv files and conducts all statistical analyses regarding the single ERP components.
ERP_N1P2_final.R
This skript evaluates ROI selection, and conducts all N1-P2 amplitude analysis, including subgroup and cluster analyses. 
baseline_comparision.R 
This script was primarily used to plot the baseline comparisons. Although it also contains the statistical analysis necessary for that approach, all other baseline comparisons and statistical baseline checks required for any analytic approach conducted in the aforementioned scripts were already performed within them.
Audiogramme.R
Prepares hearing thresholds for the combined audiogram plot.
