# The Fragility of the Urban Advantage: Replication Code

R code to reproduce the results in:

> Urago, G. G. *The Fragility of the Urban Advantage: Vulnerability to Food Insecurity under Systemic Shocks.* Scientific Reports (under review).

The analysis uses the Food Insecurity Experience Scale (FIES) for Ethiopia (2014–2020, 2022, 2023) to examine how the urban–rural gap in food insecurity changed during systemic shocks.

## Contents

```
fies-ethiopia-urban-advantage/
|-- fies-ethiopia-urban-advantage.Rproj
|-- README.md
|-- sessionInfo.txt                   R and package versions used for the published results
|-- 1_scripts/
|   `-- fies_ethiopia_analysis.R      full analysis, from raw data to tables and figures
|-- 2_data_raw/                       FIES microdata (not included, see below)
`-- 3_data_clean/
    |-- population.rds                World Bank population by age, 2014–2023
    `-- macro.rds                     World Bank GDP growth and CPI inflation, 2014–2023
```

## Data

The FIES microdata are not redistributed here. Download the Ethiopia datasets from the FAO Microdata Catalogue (https://microdata.fao.org) and place each `.RData` file in a `microdata` subfolder named after its dataset ID:

```
2_data_raw/ETH_2014_FIES_v01_EN_M_v01_A_OCS/microdata/ETH_2014_FIES_v01_EN_M_v01_A_OCS.RData
2_data_raw/ETH_2015_FIES_v01_EN_M_v01_A_OCS/microdata/ETH_2015_FIES_v01_EN_M_v01_A_OCS.RData
...
2_data_raw/ETH_2023_FIES_v01_EN_M_v01_A_ESS/microdata/ETH_2023_FIES_v01_EN_M_v01_A_ESS.RData
```

Datasets: 2014–2020 and 2022 (`..._OCS`), and 2023 (`..._ESS`). No FIES round is available for 2021.

`population.rds` and `macro.rds` are the World Bank (World Development Indicators) data used in the paper. They are included because these series are revised over time; if deleted, the script downloads the current versions, and the figures may then differ slightly.

## How to run

1. Open `fies-ethiopia-urban-advantage.Rproj` in RStudio (or set the working directory to this folder in a fresh R session).
2. Run `1_scripts/fies_ethiopia_analysis.R` from top to bottom.

Missing packages are installed automatically through `pacman`. The results were produced with R 4.6.1 on Windows 11; exact package versions are listed in `sessionInfo.txt`. The script creates `3_data_clean/data.rds` (cleaned pooled data) and writes all results to `4_outputs/`.

## Outputs

| File in `4_outputs/` | Paper |
|---|---|
| `1_rasch_models.xlsx` | Table 1; Supplementary Table 1 |
| `2_food_insecurity_by_groups.xlsx` | Supplementary Table 2 (sheet `insecured`) |
| `3_food_insecurity_prevalence.png` | Figure 1 |
| `4_macro_economic_trends.png` | Figure 2 |
| `5_regression_models.xlsx` | Table 2 (`insecured`); Supplementary Table 3 (`vif`); Supplementary Table 4 (`or_insecured`); 2020 robustness checks (`secondary_check`, `composition`) |
| `6_predicted_probability.png` | Predicted probabilities by area and year (not in the paper) |

## Notes

- Figures use the Segoe UI font. On systems without it, a default font is used; the estimates are unaffected.
- Missing income quintiles are imputed with `mice` (seed 123) for Model 3 only.

## Licence

Code released under the MIT License.

## Contact

Gutama Girja Urago, Laterite, Addis Ababa, Ethiopia (gurago@laterite.com)
