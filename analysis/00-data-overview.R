library('readr')   # for importing csv files in a tidy way
library('dplyr')   # for data wrangling
library('ggplot2') # for fancy figures
library('ctmm')    # for movement modeling
source('analysis/figures/default-ggplot-theme.R')

# telemetry data
d <- read_csv('data/Odocoileus virginianus DeNicola South Euclid.csv',
              show_col_types = FALSE) %>%
  rename_with(\(.names) gsub('-', '_', .names)) %>%
  rename_with(\(.names) gsub('location_', '', .names)) %>%
  rename(animal_id = individual_local_identifier,
         dop = `gps:dop`,
         hdop = `gps:hdop`,
         fix_type = `sensolus:fix_type`) %>%
  mutate(group = if_else(grepl('T_', animal_id),
                         'ovariectomy', 'control'))

glimpse(d)

unique(d$behavioural_classification)
unique(d$fix_type)
unique(d$geolocator_fix_type)
unique(d$group)
head(unique(d$comments)) # nearest addresses

# reference data
d_ref <- read_csv('data/Odocoileus virginianus DeNicola South Euclid-reference-data.csv',
                  show_col_types = FALSE, col_types = c(`animal-sex` = 'c')) %>%
  rename_with(\(.names) gsub('-', '_', .names))
d_ref

#' `d` has 2 less rows than the reference data
n_distinct(d$animal_id)
nrow(d_ref)

table(d$animal_id)

# missing two control deer in the dataset
d_ref$animal_id[which(! d_ref$animal_id %in% unique(d$animal_id))]

d_ref[which(! d_ref$animal_id %in% unique(d$animal_id)), ]

# no deer tracked after the end date (when available; 16/20 missing)
# C_201 and C_103 have start & end dates but are not in telemetry dataset
# C_101 barely has any data
ggplot() +
  geom_hline(aes(yintercept = animal_id), d_ref, color = 'grey') +
  geom_point(aes(as.Date(timestamp), animal_id, color = group), d,
             pch = 20, alpha = 0.1) +
  geom_point(aes(deploy_on_date, animal_id, shape = 'start'), d_ref,
             size = 5) +
  geom_point(aes(deploy_off_date, animal_id, shape = 'end'), d_ref,
             size = 5, na.rm = TRUE) +
  geom_point(aes(animal_mortality_date, animal_id, shape = 'mortality'),
             d_ref, size = 7, na.rm = TRUE) +
  scale_color_manual('Group', values = PAL) +
  scale_shape_manual('Event type', breaks = c('start', 'end', 'mortality'),
                     values = c('[', ']', '\U00d7')) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = NULL, y = 'Animal ID') +
  theme(legend.position = 'top')

ggsave('figures/time-series-dotplot.png', width = 10, height = 8,
       units = 'in', dpi = 600, bg = 'white')

ggplot() +
  facet_wrap(~ animal_id) +
  geom_histogram(aes(timestamp, fill = group), d) +
  scale_fill_manual('Group', values = PAL) +
  # labs(x = NULL, y = 'Animal ID') +
  theme(legend.position = 'top')

# check comments (nothing problematic)
table(d_ref$animal_comments)
table(d_ref$animal_death_comments)
table(d_ref$deployment_comments)

# plot histograms of time to fix (not sure what the units are)
d %>%
  ggplot() +
  facet_wrap(~ animal_id, scales = 'free_y', ncol = 4) +
  geom_histogram(aes(gps_time_to_fix, fill = group), na.rm = TRUE, bins = 10) +
  scale_fill_manual(NULL, values = PAL) +
  labs(x = 'GPS time to fix (units?)', y = 'Count') +
  theme(legend.position = 'inside', legend.position.inside = c(0.75, 0.09))

# plot histograms of sampling intervals
d %>%
  group_by(animal_id) %>%
  mutate(dt = c(NA, diff(timestamp, units = 'hours'))) %>%
  ggplot() +
  facet_wrap(~ animal_id, scales = 'free_y', ncol = 4) +
  geom_histogram(aes(log10(dt), fill = group), na.rm = TRUE) +
  scale_fill_manual(NULL, values = PAL) +
  scale_x_continuous(expression(bold(log[10](paste(Delta, t))~(hours))),
                     breaks = c(-2, 0, 2), labels = 10^c(-2, 0, 2)) +
  ylab('Count') +
  theme(legend.position = 'inside', legend.position.inside = c(0.75, 0.09))

ggsave('figures/sampling-interval-histograms.png', width = 12, height = 8,
       units = 'in', dpi = 600, bg = 'white')

# look at median sampling intervals and overall duration
d %>%
  group_by(animal_id) %>%
  summarize(median_dt = round(median(diff(timestamp, units = 'hours')), 2),
            duration = round(diff(range(timestamp))))

# check some of the spotty telemetries
tels <- as.telemetry(d)

plot(tels$T_163, xlim = c(-3e3, 3e3), col = '#00000060')

# will need to do some outlier cleaning
plot(tels$T_155, col = '#00000060')
out <- outlie(tels$T_155)
rm(out)

layout(matrix(1:20, ncol = 5))
for(i in 1:length(tels)) {
  plot(tels[[i]], col = '#00000060', error = FALSE)
  plot(tels[[i]], col = '#FF000060', error = TRUE, add = TRUE)
  title(names(tels)[i])
}

# see C_200, T_151, T_155, T_166, 
layout(matrix(1:20, ncol = 5))
out <- outlie(tels)
layout(1)
