library('readr')   # for importing csv files in a tidy way
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('purrr')   # for functional programming
library('ggplot2') # for fancy figures
library('ctmm')    # for movement modeling
source('analysis/figures/default-ggplot-theme.R')
source('functions/outlier_plots.R') # to plot outlier diagnostic plots
source('functions/check_animal.R') # to run diagnostic plots
source('functions/plot_adj.R') # to plot 20 adjacent locations
source('functions/flag_outlier.R') # to mark outliers
source('functions/remove_outlier_flags.R') # to start over with an animal

# telemetry data
d <-
  read_csv('data/Odocoileus virginianus DeNicola South Euclid.csv',
                   show_col_types = FALSE) %>%
  rename_with(\(.names) gsub('-', '_', .names)) %>%
  rename_with(\(.names) gsub('location_', '', .names)) %>%
  rename(species = individual_taxon_canonical_name,
         dop = `gps:dop`,
         hdop = `gps:hdop`,
         fix_type = `sensolus:fix_type`) %>%
  mutate(animal = individual_local_identifier,
         species = 'Odocoileus virginianus',
         group = if_else(grepl('T_', animal),
                         'ovariectomy', 'control'),
         outlier = 0)

#' no cases where both `hdop` and `dop` are `NA` or not `NA`
filter(d, ! is.na(hdop) & ! is.na(dop))
filter(d, is.na(hdop) & is.na(dop))
unique(is.na(d$hdop) + is.na(d$dop)) # 0 = both NA, 2 = both non-NA

#' sub `dop` for `hdop` when `hdop` is `NA`
d <- d %>%
  mutate(hdop = if_else(is.na(hdop), dop, hdop)) %>%
  select(! dop)
sum(is.na(d$hdop))

# create nested version by animal
d <- d %>%
  group_by(animal) %>%
  nest(tel = ! c(tag_local_identifier, animal, group))
d

# print outlier diagnostic plots
if(FALSE) { # initial diagnostic plots
  N <- nrow(d)
  
  for(i in 1:N) {
    cat('Running deer ', i, ' of ', N, '.\n', sep = '')
    png(filename = paste0('figures/outlier-diagnostics/', d$group[i],
                          '-', d$animal[i], '.png'),
        width = 8, height = 12, units = 'in', res = 300)
    check_animal(id = d$animal[[i]], return_out = FALSE)
    dev.off()
  }
}

DROP <- character(length = 0)
CHECK <- character(length = 0)

# C_100 ----
# ok
ID <- 'C_100'
out <- check_animal(ID)
plot_adj(ID, max_speed = 0.4)

# C_101 ----
# not worth keeping
ID <- 'C_101'
nrow(d$tel[[which(d$animal == ID)]])
diff(range(d$tel[[which(d$animal == ID)]]$timestamp))
DROP <- c(DROP, ID)

# C_102 ----
# ok
ID <- 'C_102'
out <- check_animal(ID)
plot_adj(ID, max_speed = 0.5, n_adj = 10) # ok

# C_106 ----
# check
ID <- 'C_106'
out <- check_animal(ID)
plot_adj(ID, max_speed = 1.2, n_adj = 10) # ok given the uncertainty
which(out$speed > 1)
nrow(out) # not beginning or end of the telemetry
CHECK <- c(CHECK, ID)

# C_107 ----
# check
ID <- 'C_107'
out <- check_animal(ID)
plot_adj(ID, max_speed = 2, n_adj = 10) # ok
plot_adj(ID, max_speed = 2, n_adj = 5) # ok given the error and the distance
which(out$speed > 1); nrow(out) # not beginning or end of the telemetry
filter(data.frame(out), speed > 2) # maybe escaped something
flag_outlier(ID, max_speed = 2, value = 2) # just to check other values
out <- check_animal(ID) # ok
remove_outlier_flags(ID) # remove outlier flag
out <- check_animal(ID) # ok
CHECK <- c(CHECK, ID)

# C_108 ----
# ok
ID <- 'C_108'
out <- check_animal(ID)
plot_adj(ID, n_adj = 10, max_speed = 0.3) # excursion; data are ok

# C_161 ----
# ok
ID <- 'C_161'
out <- check_animal(ID)
plot_adj(ID, n_adj = 10, max_speed = 2) # ok given the range and the error

# C_200 ----
# check: removing the first 26 locations with outlier behavior
ID <- 'C_200'
out <- check_animal(ID)
plot_adj(ID, n_adj = 5, max_speed = 5)
filter(data.frame(out), speed > 5)
ggplot(d$tel[[which(d$animal == ID)]], aes(long, lat)) +
  geom_path(aes(color = lubridate::decimal_date(timestamp))) +
  geom_point(shape = c(rep(19, 26), rep(NA, nrow(d$tel[[which(d$animal == ID)]]) - 26)),
             color = 'red', size = 3) +
  khroma::scale_color_smoothrainbow(name = 'Date', reverse = TRUE)
d$tel[[which(d$animal == ID)]]$outlier[1:26] <- 1
out <- check_animal(ID)
CHECK <- c(CHECK, ID)

# T_151 ----
# one clear outlier
ID <- 'T_151'
out <- check_animal(ID)
plot_adj(ID, n_adj = 10, max_speed = 0.5)
flag_outlier(ID, max_speed = 0.5, value = 1) # clear outlier
out <- check_animal(ID) # ok

# T_152 ----
# ok
ID <- 'T_152'
out <- check_animal(ID)

# T_153 ----
# ok
ID <- 'T_153'
out <- check_animal(ID)

# T_155 ----
# one clear outlier
ID <- 'T_155'
out <- check_animal(ID)
flag_outlier(id = ID, max_speed = 1e3, value = 1)
out <- check_animal(ID)
plot_adj(ID, max_speed = 1.5)
filter(data.frame(out), speed > 1.5)
plot_adj(ID, max_speed = 0.5, max_angle = 160) # ok

# T_158 ----
# ok, some excursions
ID <- 'T_158'
out <- check_animal(ID) # outlier behavior at the end
i <- which(d$tel[[which(d$animal == ID)]]$lat > 41.545)
d %>%
  filter(animal == ID) %>%
  pull(tel) %>%
  first() %>%
  ggplot(aes(long, lat)) +
  geom_path(aes(color = lubridate::decimal_date(timestamp))) +
  geom_point(aes(color = lubridate::decimal_date(timestamp))) +
  geom_point(data = d$tel[[which(d$animal == ID)]][i, ], color = 'red',
             size = 3) +
  scale_shape_manual(values = c(NA, 19)) +
  khroma::scale_color_smoothrainbow(name = 'Date', reverse = TRUE)
d$tel[[which(d$animal == ID)]]$outlier[i] <- 1
out <- check_animal(ID) # ok

# T_159 ----
# ok
ID <- 'T_159'
out <- check_animal(ID)

# T_160 ----
# ok
ID <- 'T_160'
out <- check_animal(ID)

# T_163 ----
# ok
ID <- 'T_163'
out <- check_animal(ID)
plot_adj(ID, max_speed = 0.3)

# T_166 ----
# one clear outlier
ID <- 'T_166'
out <- check_animal(ID)
plot_adj(ID, max_speed = 10)
flag_outlier(ID, max_speed = 10, value = 1)
out <- check_animal(ID)
plot_adj(ID, max_speed = 0.6) # ok

# T_169 ----
ID <- 'T_169'
out <- check_animal(ID)
plot(lat ~ long, d$tel[[which(d$animal == ID)]])
d %>%
  filter(animal == ID) %>%
  pull(tel) %>%
  first() %>%
  ggplot(aes(long, lat)) +
  geom_path(aes(color = lubridate::decimal_date(timestamp))) +
  geom_point(aes(shape = long < -81.55), color = 'red', size = 3,
             na.rm = TRUE) +
  scale_shape_manual(values = c(NA, 19)) +
  khroma::scale_color_smoothrainbow(name = 'Date', reverse = TRUE)
i <- which(d$tel[[which(d$animal == ID)]]$long < - 81.55)
layout(1:2)
hist(out[i, 'speed'], xlab = 'Minimum speed (SLD; m/s)',
     main = NULL)
hist('hours' %#% out$dt[i], xlab = 'Interval between locations (hours)',
     main = NULL)
layout(1)
d$tel[[which(d$animal == ID)]]$outlier[i] <- 1
out <- check_animal(ID)
plot(lat ~ long, d$tel[[which(d$animal == ID)]][- i, ])

# drop deer that are not worth keeping ----
DROP
d <- filter(d, ! animal %in% DROP)

d %>%
  arrange(animal) %>%
  saveRDS('data/cleaned-telemetry-data.rds')

# check which deer need to be checked
CHECK

# check range of dates
layout(1:2)
d %>%
  unnest(tel) %>%
  pull(timestamp) %>%
  hist(breaks = 'months')
hist(read_csv('data/Odocoileus virginianus DeNicola South Euclid.csv',
         show_col_types = FALSE)$timestamp, breaks = 'months')
layout(1)

# count number of rows dropped
nrow(read_csv('data/Odocoileus virginianus DeNicola South Euclid.csv',
              show_col_types = FALSE)) -
  nrow(filter(unnest(d, tel), ! outlier))
