# JAPSDC2026
R and Stata syntax supporting "Does Self-Directed Care Reduce Unmet Needs of Older Adults: Quasi-Experimental Evidence from China", accepted for publication in the Journal of Aging & Social Policy.

## Files and running order

1. Run `Create dataset.R` in R to prepare the dataset.
2. Run `Analysis.do` in Stata to conduct the analyses.

## Data availability

The study uses publicly available data from the China Health and Retirement Longitudinal Study (CHARLS), including Harmonized CHARLS. The author does not have permission to redistribute these data. Original and generated datasets are therefore excluded from this repository.

Users must obtain the required data directly from the providers, subject to their access requirements:

CHARLS: https://charls.pku.edu.cn/en/

Gateway to Global Aging Data: https://g2aging.org/

## Instructions

Download the required data and update the data directory and output directory in `Create dataset.R`. Install the R packages listed at the beginning of the script, then run it to create `an.dta`.

Update the working directory in `Analysis.do` to the folder containing `an.dta`. Install the required Stata packages, then run the script.


## Citation

Please cite the associated publication when using this syntax:

Jinbao Zhang (2026), Does Self-Directed Care Reduce Unmet Needs of Older Adults: Quasi-Experimental Evidence from China, Journal of Aging & Social Policy.
