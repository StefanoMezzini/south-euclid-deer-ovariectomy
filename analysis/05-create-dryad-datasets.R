library('readr')
library('dplyr')
library('purrr')

# no need to change the reference data
read_csv('data/Odocoileus virginianus DeNicola South Euclid-reference-data.csv') %>%
  write_csv('data/dryad-data/Odocoileus virginianus DeNicola South Euclid-reference-data.csv')

# round coordinates to 2 decimals and drop the column with address
read_csv('data/Odocoileus virginianus DeNicola South Euclid.csv') %>%
  mutate(`location-long` = round(`location-long`, 2),
         `location-lat` = round(`location-lat`, 2)) %>%
  select(! comments) %>%
  write_csv(file = 'data/dryad-data/Odocoileus virginianus DeNicola South Euclid.csv')

# round coordinates to 2 decimals and drop the column with address
read_rds('data/cleaned-telemetry-data.rds') %>%
  mutate(tel = map(tel, \(.t) {
    .t %>%
      mutate(`long` = round(`long`, 2), `lat` = round(`lat`, 2)) %>%
      select(! comments)
  })) %>%
  saveRDS('data/dryad-data/cleaned-telemetry-data.rds')

# no need to change data with counts of daily fixes
readRDS('data/daily-fixes.rds') %>%
  saveRDS('data/dryad-data/daily-fixes.rds')
