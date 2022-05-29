# The Influence of Voluntarily Disclosing Marketing Expenditures on Firm Value

## About
This GitHub repository is part of the Master Thesis conducted and written by [Ralph Delsing](https://www.github.com/RalphGit21) in 2022. The project is supervised by Dr. Leon Gim Lim and Dr. Lachlan Deer (co-reader). For this study, data on the 10-K Reports of firms based in the USA is analyzed and extracted using the Compustat database from [WRDS](https://wrds-www.wharton.upenn.edu). Access to this database is only granted to researchers and students of associated universities and accounts are first verified before receiving access. As a result, the datasets used in this study may not be shared with third parties, such that the study cannot be fully replicated. The final dataset includes 194 firm-year observations across 15 different firms.
  
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
The conceptual model that follows from the literature review is as follows:
![Conceptual model](https://user-images.githubusercontent.com/90320730/170889666-0d71512b-1463-4214-a545-dc59f7fb0c27.png)

Our results do not support H1 and H2, such that we do not find a significant impact of disclosures of marketing expenditures on firm value or a moderating effect of the type of business (B2B vs B2C). We do find support in favor of H3, where an increase in disclosure quality leads to a stronger effect of the disclosure of marketing expenditures on firm value for disclosing firms, highlighting the benefits of higher-quality disclosures (i.e., a disaggregated overview of the expenses on each component).

**Final model:**
| **Hypothesis & Expected Sign**  | **Coefficients (SE)** |
| ------------- | ------------- |
| **H1: + Voluntary Disclosure**  | -.708 (.508)  |
| **H2: + Voluntary Disclosure x Nature of the Business**  | .248 (.398)  |
| **H3: + Voluntary Disclosure x Disclosure Quality** | .447 (.198)** |
**Control Variables**:
| **Firm size** | -.056 (.065) |
| **Financial leverage** | -.450 (.614) |
| **Revenue growth** | .016 (.003)*** |
| **Lagged firm value** | .552 (.046)*** |
| **Constant** | 2.056 (.659)*** |
| **Year Dummies** | Included |
| **Adjusted R2** | .758 |
| **F-Statistic** | 21.89*** |

*** p-value <.01   ** p-value <.05   * p-value <.10




## Packages used
The following packages have been installed and used within this repository:
```
library(haven)
library(dplyr)
library(rstatix)
library(tidyr)
library(car)
```
