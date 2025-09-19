# Dataset for *Monitoring the Effects of Ovariectomy on Seasonal Movement Behavior in Suburban Female White-Tailed Deer Using Internet of Things Enabled Devices*

[Access this dataset on Dryad](doi.org/10.5061/dryad.h70rxwdxb)

The data and code found here were used for the analyses in:

> DeNicola, V., Mezzini, S., Cagnacci, F. Monitoring the Effects of Ovariectomy on Seasonal Movement Behavior in Suburban Female White-Tailed Deer Using Internet of Things Enabled Devices. Wildlife Biology. https://doi.org/10.1002/wlb3.01512

The file `DeNicola-2025-south-euclid-deer.zip` contains:

* A `data` folder with the original (uncleaned) deer telemetry data (`Odocoileus virginianus DeNicola South Euclid.csv`), the telemetry metadata (`Odocoileus virginianus DeNicola South Euclid-reference-data.csv`), the cleaned telemetry data (`cleaned-telemetry-data.rds`), and the number of daily fixes for each deer (`daily-fixes.rds`).
* All `R` code used for the analyses in the `analysis` folder, except for custom functions, which are in the `functions` folder.

## Description of the data and file structure

All missing values in the two csv files are indicated as `NA`.

## `Odocoileus virginianus DeNicola South Euclid.csv`

This file contains the telemetry data before data cleaning. Also see the metadata dataset and the cleaned telemetry data below. It contains the following columns:

- `event-id`: numeric; the fix ID (each value is unique).
- `visible`: logical; all values are `TRUE`.
- `timestamp`: character; date and time of the fixes with format `yyyy-mm-ddTHH:MM:SS.000000Z`.
- `location-long`: numeric; longitude of the fix, rounded to two decimals as per Dryad's requirements.
- `location-lat`: numeric; latitude of the fix, rounded to two decimals as per Dryad's requirements.
- `behavioural-classification`: character; behavioral classification as one of `STOP`, `START`, `ON_THE_MOVE`, `PERIODIC`, or `NA`.
- `geolocator-fix-type`: character; indicates whether the geolocator fix was obtained with GPS (`GPS`) or WiFi positioning (`wifi`); is `NA` if the fix is `sensolus` (see `sensolus:fix-type` below).
- `gps:dop`: numeric; dilution of precision.
- `gps:hdop`: numeric; horizontal dilution of precision.
- `gps-time-to-fix`: numeric; number of seconds to GPS fix.
- `sensolus:fix-type`: character; indicates whether the Sensolus fix was obtained with GPS (`GPS`), WiFi positioning (`wifi`), or the Sigfox network (`network`); is `NA` if the fix is with a `geolocator` (see `geolocator-fix-type` above).
- `sensor-type`: character; indicates sensor type (always `gps`).
- `individual-taxon-canonical-name`: character; species name (always `Odocoileus virginianus`).
- `tag-local-identifier`: character; animal tag identifier (see `individual-local-identifier` below for animal ID).
- `individual-local-identifier`: character; animal ID identifier with `C` indicating control deer and `T` indicating treated (ovariectomized) deer (see `tag-local-identifier` above for tag ID).
- `study-name`: character; all values are `Odocoileus virginianus DeNicola South Euclid`.

## `Odocoileus virginianus DeNicola South Euclid-reference-data.csv`

This file contains the metadata related to the telemetry data detailed above. It contains the following columns:

- `tag-id`: character; tag IDs containing all values from the telemetry data plus tag `YZX346` for deer C_103, which had no telemetry data.
- `animal-id`: character; animal ID identifier with `C` indicating control deer and `T` indicating treated deer (see `tag-id` above for tag ID).
- `animal-taxon`: character; species (all values are `Odocoileus virginianus`).
- `deploy-on-date`: date; dates on which collars were deployed.
- `deploy-off-date`: date; dates on which collars were removed, if available (`NA` otherwise).
- `animal-comments`: character; only non-`NA` value is `Lost collar - out of area` for deer C_103.
- `animal-death-comments`: character; only non-`NA` value `3/20/23\r\nMortality 5/30/23` for deer T_169.
- `animal-mortality-date`: date; mortality date if available.
- `animal-sex`: character; all values `f` to indicate female deer.
- `deployment-comments`: character; only non-`NA` value is `Photo of doe without collar from 2/2023` for deer C_103.
- `tag-manufacturer-name`: character; tag manufacturer (all values are `Sigfox`).
- `tag-serial-no`: character; all values are `NA` except for `LGFZWH` (same as `tag-id`) for deer C_101.

## `cleaned-telemetry-data.rds`

This RDS file contains the cleaned telemetry data as created by the `analysis/01-telemetry-data-cleaning.R` script. Deer C_101 was removed from the telemetry data because she only had 16 fixes. Deer C_103 did not have any fixes. The dateset includes the following columns:

- `tag_local_identifier`: character; tag ID corresponding to `tag-local-identifier` in the original telemetry dataset.
- `animal`: character; animal ID corresponding to `individual-local-identifier` in the original telemetry dataset.
- `group`: character; either `Control` for control group or `Ovariectomy` for treatment (ovariectomized) group corresponding to `C` and `T` in the animal ID.
- `tel`: list; tibbles of the telemetry data for each animal after removing problematic fixes through data cleaning.

Each tibble in the `tel` column contains the following columns:

- `event_id`: numeric; the fix ID (each value is unique).
- `visible`: logical; all values are `TRUE`.
- `timestamp`: datetime; date and time of the fixes.
- `long`: numeric; longitude of the fix, rounded to two decimals as per Dryad's requirements.
- `lat`: numeric; latitude of the fix, rounded to two decimals as per Dryad's requirements.
- `behavioural_classification`: character; behavioral classification as one of `STOP`, `START`, `ON_THE_MOVE`, `PERIODIC`, or `NA`.
- `geolocator_fix_type`: character; indicates whether the geolocator fix was obtained with GPS (`GPS`) or WiFi positioning (`wifi`); is `NA` if the fix is `sensolus` (see `sensolus:fix-type` below).
- `hdop`: numeric; horizontal dilution of precision.
- `gps_time_to_fix`: numeric; number of seconds to GPS fix.
- `fix_type`: character; indicates whether the Sensolus fix was obtained with GPS (`GPS`), WiFi positioning (`wifi`), or the Sigfox network (`network`); is `NA` if the fix is with a `geolocator` (see `geolocator-fix-type` above).
- `sensor_type`: character; indicates sensor type (always `gps`).
- `species`: character; species name (always `Odocoileus virginianus`).
- `individual_local_identifier`: character; animal ID identifier with `C` indicating control deer and `T` indicating treated (ovariectomized) deer (see `tag-local-identifier` above for tag ID).
- `study_name`: character; all values are `Odocoileus virginianus DeNicola South Euclid`.
- `outlier`: numeric; either `0` (to keep) or `1` (to remove before model fitting).

## `daily-fixes.rds`

This RDS file contains the number of fixes for each deer on each day between the start and end of the tracking periods. It contains the following columns:

- `group`: factor; either `Control` for control group or `Ovariectomy` for treatment (ovariectomized) group corresponding to `C` and `T` in the animal ID.
- `animal`: character; character; animal ID corresponding to `individual-local-identifier` in the original telemetry dataset.
- `deploy-on-date`: date; dates on which collar was deployed.
- `date`: date; date of reference.
- `daily_fixes`: numeric; number of fixes on the date of reference (`date`).
- `doy`: numeric; day of year of the date of reference (between 1 and 366).
- `doy_cr`: numeric; same as `doy` above, used for non-cyclical cubic regression splines in the hierarchical generalized additive model (see `analysis/03-fixes-per-day-hgam.R`).
- `animal_year`: factor; the animal ID (`animal`) along with the year (starting in September) to account for differences across years (2023 and 2024).

## Sharing/Access information

The data and code, along with all related figures and models, are also available on GitHub at [https://github.com/StefanoMezzini/south-euclid-deer-ovariectomy](https://github.com/StefanoMezzini/south-euclid-deer-ovariectomy).

## Code/Software

All analyses were run in `R` 4.4.0 along with the creation of all figures. The code used is available in the `analysis` folder, with the exception of custom functions, which are in the `functions` folder.

The `analysis` folder includes the following files:

- `00-data-overview.R`: gives an overview of the two csv files and performs some data exploration; creates figure S2.
- `01-telemetry-data-cleaning.R`: cleans the telemetry data and saves the output as `data/cleaned-telemetry-data.rds`.
- `02-fit-movement-models.R`: fits the `ctmm` movement models and AKDEs; creates figure 3.
- `03-fixes-per-day-hgam.R`: fits a hierarchical generalized additive model that estimates the difference in number of daily fixes across groups.
- `04-hgams-without-T_169.R`: fits hierarchical generalized additive models for the movement behavior of the deer, excluding deer T_169.
- `04-hgams.R`: fits hierarchical generalized additive models for the movement behavior of the deer, including deer T_169; creates figures S4 and S5.
- `figures/akde-contours-excursivity.R`: creates figure 2.
- `figures/check-data-gaps.R`: checks gaps in data; creates figure S1 and a figure of T_169's excursions.
- `figures/default-ggplot-theme.R`: creates the default ggplot theme used for the figures.
- `figures/hgam-figures.R`: creates figures 4 and S3.
- `figures/hgams-with-and-without-T_169.R`: compares the models with and without deer T_169 and creates figures S6 and S7.
- `figures/study-site-map.R`: creates figure 1.
- `README.md`: a simple README for the folder.

The files in the `analysis` folder are to be run sequentially following the file name order, while the files in the `analysis/figures` folder are to be run after scripts `00` to `04`. The two `04` scripts can be run in either order.

Movement models were fit using `ctmm` v. 1.2.0, while generalized additive models were fit using `mgcv` v. 1.9-1.

The `functions` contains all custom functions used in this project, namely:

- `check_animal.R`: creates a telemetry for animal `id` and calls `outlier_plots()` (see below)
- `find_angle.R`: given two vector of coordinates `x` and `y`, finds the turning angle between each group of three temporally consecutive points
- `flag_outlier.R`: flags one or more points as outliers based on maximum allowed speed (`max_speed`), maximum allowed median displacement (`max_distance`), maximum allowed angle (`max_angle`) = 0, and maximum allowed sampling interval (`max_dt`). Outliers are indicated by a `value` > 0.
- `get_cis.R`: given model `m`, extracts estimated relative differences between the treatment and control groups, along with 95% confidence intervals and P-values.
- `get_legend.R`: a modified version of `cowplot::get_legend()` to extract legends from `ggplot` plot objects.
- `get_preds.R`: predicts from the model for a given parameter (`parameter`) among home range size (`hr`), diffusion (`diff`), excursivity (`exc`), and daily fixes (`fixes`).
- `outlier_plots.R`: creates the diagnostic plots used to clean the telemetry data.
- `plot_adj.R`: plots `n_adj` before and after fixes flagged based on minimum or maximum: speed, distance from median location (`distance`), angle, and sampling interval (`dt`).
- `remove_outlier_flags.R`: removes all outlier flags by changing all values of the `outlier` back to `0`.
- `window_ctmm.R`: fits movement `ctmm` movement models to telemetry `.tel` using a moving window approach with a window `window` days and a slide of `dt` days.
