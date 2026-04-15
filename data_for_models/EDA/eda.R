library(tidyverse)
library(sf)
library(tigris)
library(viridis)

load("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/f_data.Rdata")

dim(forMod_F |> filter(year == 2023 & bounded_age == 5))

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

# ---- 1) Build county-level dep_rate for 2023 ----
# If you have pos/total columns, use them for an exact county PSR. If not, fallback shown below.

dep_2023 <- forMod_F %>%
  filter(year == 2023 & bounded_age == 5) %>%
  # Make sure FIPS are 5-digit character to match shapefile GEOID
  mutate(fips_chr = ifelse(!is.na(fips),
                           str_pad(as.character(fips), 5, pad = "0"),
                           as.character(county)))

# If you *don’t* trust/ have pos & tot, replace the summarise block above with this fallback:
# summarise(dep_rate_county = mean(dep_rate, na.rm = TRUE)) %>% ungroup()

# ---- 2) Get county geometries and filter to contiguous U.S. ----
cnty <- tigris::counties(year = 2023, class = "sf", cb = TRUE) %>%
  # Drop AK, HI, and territories (keep only the 48 states + DC)
  filter(!STATEFP %in% c("02","15","60","66","69","72","78"))

# ---- 3) Join data to shapes ----
map_dat <- cnty %>%
  left_join(dep_2023, by = c("GEOID" = "fips_chr"))

# ---- 4) Plot ----
p <- ggplot(map_dat) +
  geom_sf(aes(fill = dep_rate), linewidth = 0.05, color = "white") +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    name = "Severe depression\npositive screen rate",
    direction = -1 # reverse the color scale
  ) +
  labs(
    title = "County-Level Severe Depression Positive Screening Rate (PSR), 2023",
    subtitle = "Mental Health America screening data aggregated to county-year",
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 6))
  )

# Print
p

# ---- 5) (Optional) Save to file ----
ggsave("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/EDA/f_dep_rate_map_2023.png", p, width = 10, height = 6, dpi = 300, bg = "white")

# ---- 4) Plot ----
p <- ggplot(map_dat) +
  geom_sf(aes(fill = si_rate), linewidth = 0.05, color = "white") +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    name = "Frequent suicide ideation\npositive screen rate"
  ) +
  labs(
    title = "County-Level Frequent Suicide Ideation Positive Screening Rate (PSR), 2023",
    subtitle = "Mental Health America screening data aggregated to county-year",
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 6))
  )

# Print
p

# ---- 5) (Optional) Save to file ----
ggsave("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/EDA/f_si_rate_map_2023.png", p, width = 10, height = 6, dpi = 300, bg = "white")

# ---- 4) Plot ----
p <- ggplot(map_dat) +
  geom_sf(aes(fill = psych_rate), linewidth = 0.05, color = "white") +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    name = "High psychosis risk\npositive screen rate"
  ) +
  labs(
    title = "County-Level High Psychosis Risk Positive Screening Rate (PSR), 2023",
    subtitle = "Mental Health America screening data aggregated to county-year",
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 6))
  )

# Print
p

# ---- 5) (Optional) Save to file ----
ggsave("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/EDA/psych_rate_map_2023.png", p, width = 10, height = 6, dpi = 300, bg = "white")

# ---- 4) Plot ----
p <- ggplot(map_dat) +
  geom_sf(aes(fill = ptsd_rate), linewidth = 0.05, color = "white") +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    name = "Post Traumatic Stress Disorder\npositive screen rate"
  ) +
  labs(
    title = "County-Level Post Traumatic Stress Disorder Positive Screening Rate (PSR), 2023",
    subtitle = "Mental Health America screening data aggregated to county-year",
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 6))
  )

# Print
p

# ---- 5) (Optional) Save to file ----
ggsave("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/EDA/ptsd_rate_map_2023.png", p, width = 10, height = 6, dpi = 300, bg = "white")

rm(list = ls())

# Males
load("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/m_data.Rdata")

dim(forMod_M |> filter(year == 2023 & bounded_age == 5))

# ---- 1) Build county-level dep_rate for 2023 ----
# If you have pos/total columns, use them for an exact county PSR. If not, fallback shown below.

dep_2023 <- forMod_M %>%
  filter(year == 2023 & bounded_age == 5) %>%
  # Make sure FIPS are 5-digit character to match shapefile GEOID
  mutate(fips_chr = ifelse(!is.na(fips),
                           str_pad(as.character(fips), 5, pad = "0"),
                           as.character(county)))

# If you *don’t* trust/ have pos & tot, replace the summarise block above with this fallback:
# summarise(dep_rate_county = mean(dep_rate, na.rm = TRUE)) %>% ungroup()

# ---- 2) Get county geometries and filter to contiguous U.S. ----
cnty <- tigris::counties(year = 2023, class = "sf", cb = TRUE) %>%
  # Drop AK, HI, and territories (keep only the 48 states + DC)
  filter(!STATEFP %in% c("02","15","60","66","69","72","78"))

# ---- 3) Join data to shapes ----
map_dat <- cnty %>%
  left_join(dep_2023, by = c("GEOID" = "fips_chr"))

# ---- 4) Plot ----
p <- ggplot(map_dat) +
  geom_sf(aes(fill = dep_rate), linewidth = 0.05, color = "white") +
  scale_fill_viridis_c(
    option = "mako",
    na.value = "grey90",
    name = "Severe depression\npositive screen rate",
    direction = -1 # reverse the color scale
  ) +
  labs(
    title = "County-Level Severe Depression Positive Screening Rate (PSR), 2023",
    subtitle = "Mental Health America screening data aggregated to county-year"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 6)),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.margin = margin(t = 10, r = 20, b = 10, l = 10)
  )

p <- ggplot(map_dat) +
  geom_sf(aes(fill = dep_rate), linewidth = 0.05, color = "white") +
  scale_fill_viridis_c(
    option = "mako",
    na.value = "grey90",
    name = "Severe depression positive screen rate",
    direction = -1 # reverse the color scale
  ) +
  labs(
    title = "County-Level Severe Depression Positive Screening Rate (PSR), 2023",
    subtitle = "Mental Health America screening data aggregated to county-year"
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 30,
    barheight = 1
)) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 6)),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.margin = margin(t = 10, r = 20, b = 10, l = 10)
  )

# Print
p

# ---- 5) (Optional) Save to file ----
###### BRIANNE WORK HERE ######
ggsave("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/EDA/m_dep_rate_map_2023.png", p, width = 10, height = 6, dpi = 300, bg = "white")

p <- ggplot(map_dat) +
  geom_sf(aes(fill = si_rate), linewidth = 0.05, color = "white") +
  scale_fill_viridis_c(
    option = "mako",
    na.value = "grey90",
    name = "Frequent suicide ideation positive screen rate",
    direction = -1 # reverse the color scale
  ) +
  labs(
    title = "County-Level Frequent Suicide Ideation Positive Screening Rate (PSR), 2023",
    subtitle = "Mental Health America screening data aggregated to county-year"
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 30,
    barheight = 1
)) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 6)),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.margin = margin(t = 10, r = 20, b = 10, l = 10)
  )

p

# ---- 5) (Optional) Save to file ----
#### BRIANNE ALSO WORK HERE #######
ggsave("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/EDA/m_si_rate_map_2023.png", p, width = 10, height = 6, dpi = 300, bg = "white")

p <- ggplot(map_dat) +
  geom_sf(aes(fill = ptsd_rate), linewidth = 0.05, color = "white") +
  scale_fill_viridis_c(
    option = "mako",
    na.value = "grey90",
    name = "Post-Traumatic Stress Disorder\npositive screen rate",
    direction = -1
  ) +
  labs(
    title = "County-Level Post-Traumatic Stress Disorder Positive Screening Rate (PSR), 2023",
    subtitle = "Mental Health America screening data aggregated to county-year",
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 6))
  )

p

# ---- 5) (Optional) Save to file ----
ggsave("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/EDA/m_ptsd_rate_map_2023.png", p, width = 10, height = 6, dpi = 300, bg = "white")

p <- ggplot(map_dat) +
  geom_sf(aes(fill = psych_rate), linewidth = 0.05, color = "white") +
  scale_fill_viridis_c(
    option = "mako",
    na.value = "grey90",
    name = "High psychosis risk\npositive screen rate",
    direction = -1
  ) +
  labs(
    title = "County-Level High Psychosis Risk Positive Screening Rate (PSR), 2023",
    subtitle = "Mental Health America screening data aggregated to county-year",
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 6))
  )

p

# ---- 5) (Optional) Save to file ----
ggsave("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/EDA/m_psych_rate_map_2023.png", p, width = 10, height = 6, dpi = 300, bg = "white")

load("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/BRFFS_F.Rdata")

dim(forMod_F |> filter(year == 2023 & bounded_age == 5))

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

# ---- 1) Build county-level dep_rate for 2023 ----
# If you have pos/total columns, use them for an exact county PSR. If not, fallback shown below.

dep_2023 <- forMod_F %>%
  filter(year == 2023 & bounded_age == 5) %>%
  # Make sure FIPS are 5-digit character to match shapefile GEOID
  mutate(fips_chr = ifelse(!is.na(fips),
                           str_pad(as.character(fips), 5, pad = "0"),
                           as.character(county)))

# If you *don’t* trust/ have pos & tot, replace the summarise block above with this fallback:
# summarise(dep_rate_county = mean(dep_rate, na.rm = TRUE)) %>% ungroup()

# ---- 2) Get county geometries and filter to contiguous U.S. ----
cnty <- tigris::counties(year = 2023, class = "sf", cb = TRUE) %>%
  # Drop AK, HI, and territories (keep only the 48 states + DC)
  filter(!STATEFP %in% c("02","15","60","66","69","72","78"))

# ---- 3) Join data to shapes ----
map_dat <- cnty %>%
  left_join(dep_2023, by = c("GEOID" = "fips_chr"))

# ---- 4) Plot ----
p <- ggplot(map_dat) +
  geom_sf(aes(fill = MentalHealthDays), linewidth = 0.05, color = "white") +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    name = "Average Mental Health Days",
    direction = -1
  ) +
  labs(
    title = "County-Level Mental Health Days, 2023",
    subtitle = "BRFSS data aggregated to county-year",
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 6))
  )

# Print
p

# ---- 5) (Optional) Save to file ----
ggsave("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/EDA/f_brfss_rate_map_2023.png", p, width = 10, height = 6, dpi = 300, bg = "white")


load("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/BRFFS_M.Rdata")

dim(forMod_M |> filter(year == 2023 & bounded_age == 5))

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

# ---- 1) Build county-level dep_rate for 2023 ----
# If you have pos/total columns, use them for an exact county PSR. If not, fallback shown below.

dep_2023 <- forMod_M %>%
  filter(year == 2023 & bounded_age == 5) %>%
  # Make sure FIPS are 5-digit character to match shapefile GEOID
  mutate(fips_chr = ifelse(!is.na(fips),
                           str_pad(as.character(fips), 5, pad = "0"),
                           as.character(county)))

# If you *don’t* trust/ have pos & tot, replace the summarise block above with this fallback:
# summarise(dep_rate_county = mean(dep_rate, na.rm = TRUE)) %>% ungroup()

# ---- 2) Get county geometries and filter to contiguous U.S. ----
cnty <- tigris::counties(year = 2023, class = "sf", cb = TRUE) %>%
  # Drop AK, HI, and territories (keep only the 48 states + DC)
  filter(!STATEFP %in% c("02","15","60","66","69","72","78"))

# ---- 3) Join data to shapes ----
map_dat <- cnty %>%
  left_join(dep_2023, by = c("GEOID" = "fips_chr"))

### BRIANNE ALSO ALSO WORK HERE ###
# ---- 4) Plot ----
p <- ggplot(map_dat) +
  geom_sf(aes(fill = MentalHealthDays), linewidth = 0.05, color = "white") +
  scale_fill_viridis_c(
    option = "mako",
    na.value = "grey90",
    name = "% Reporting ≥14 Poor Mental Health Days (Past 30 Days)",
    direction = -1
  ) +
  labs(
    title = "County-Level BRFSS Data, 2023",
    subtitle = "BRFSS data aggregated to county-year",
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 30,
    barheight = 1
)) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 6)),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.margin = margin(t = 10, r = 20, b = 10, l = 10)
  )

# Print
p

# ---- 5) (Optional) Save to file ----
ggsave("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/EDA/m_brfss_rate_map_2023.png", p, width = 10, height = 6, dpi = 300, bg = "white")
