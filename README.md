# "Wrangling ESM data"

This vignette shows how you can download, wrangle, and process ESM data to calculate climate metrics. The idea is that the functions are independent of each other and can still perform their purpose without having to follow the entire workflow detailed below, from start to finish.

This code works for both monthly (`mon`) and daily (`day`) climate data.

I would recommend creating a `data` folder, with the following basic structure:

- `data/raw/`: raw, untouched data
  - `data/raw/wget`: downloaded `wget` scripts
  - `data/raw/tos`: raw ESM data (e.g., tos - sea surface temperature)
- `data/proc/`: processed ESM data
  - `data/proc/merged`: merged files per model - 1 model per model/variable/scenario/frequency combination
  - `data/proc/sliced`: sliced files depending on the required time period
  - `data/proc/yearly`: yearly data (after converting monthly frequency to yearly frequency)
  - `data/proc/regridded`: regridded data, standardizing grids of data
  - `data/proc/ensemble`: ensemble mean/median for each variable/scenario/frequency combination
  
We will follow this folder structure throughout the vignette. It will still work with a different folder structure, just make sure you're inputting the right paths.

Vignette is found in `R/analysis/ESM_vignette.Rmd` and will go through the following steps:
1. Downloading ESM outputs
2. Merging files
3. Adjust and reframe time periods
4. Fix calendar periods (if needed)
5. Changing frequency of climate data
6. Regridding
7. Create ensemble
