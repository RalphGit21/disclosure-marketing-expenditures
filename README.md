# The Influence of Voluntarily Disclosing Marketing Expenditures on Firm Value

## About
This GitHub repository is part of the Master Thesis conducted and written by [Ralph Delsing](https://www.github.com/RalphGit21) in 2022. The project is supervised by Dr. Leon Gim Lim and Dr. Lachlan Deer (co-reader). For this study, data on the 10-K Reports of firms based in the USA is analyzed and extracted using the Compustat database from [WRDS](https://wrds-www.wharton.upenn.edu). This database may only be used by researchers and students of which the accounts have been verified and, as a result, may not simply be disclosed. Because the datasets used may not be disclosed, the study cannot be fully replicated. Instead, I created this because I believe in the **value of open science** and I want to stimulate **reproducability** as much as possible. Therefore, all the exact steps taken in the Master's thesis from beginning till the end are shown.
  
## Motivation
To sell products and services to customers and to increase market share and valuation, companies make use of marketing expenditures such as advertising expenses, promotional spending and market research (Chauvin & Hirschey, 1993; M. Conchar et al., 2005). Previously, it was mandatory for companies to disclose the amount they spent on such advertising expenses, until the SEC made it voluntary to publish these numbers in 1994, leading to a decrease in the number of companies publishing advertising expenses. This study investigates the how making voluntarily disclosures on such marketing expenditures affects firm value.

## Repository Overview
```txt
├── Data <-- Holds all the datasets that are used and called in the scripts (these are not on Github for the reasons described above) 
├── Gen 
│   ├── Output <-- Contains the final datasets on which the analyses are  conducted (and additional metadata and/or PDFs if relevant)
│   └── Temp <-- Contains temporary files that are created during the cleaning phase, from which the output files are created
└── Src
    └── Data-Preparation 
        ├── 1_clean.R <-- Clean the data
        ├── 2_join.R <-- Join the relevant datasets
        ├── 3_dummies.R <-- Create dummies and control variables
        ├── 4_analysis.R <-- Conduct analyses
        └── 5_sensitivity.R <-- Conduct sensitivity analyses
```
## Method and Results


## Packages used
The following packages have been installed and called within this repository:
```
library(haven)
library(dplyr)
library(rstatix)
library(tidyr)
library(car)
```
