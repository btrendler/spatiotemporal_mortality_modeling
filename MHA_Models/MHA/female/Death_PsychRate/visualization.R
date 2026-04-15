library(CARBayesST)
library(tidyverse)
library(sf)
library(tigris)
library(gridExtra)
library(grid)
library(jpeg)

### Potential problem: the 2012 map isn't loading propperly... but the rest do.

setwd("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/models/7_22/MHA/female/Death_PsychRate")

# Load the data
load("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/f_data.Rdata")
all_deltas <- readRDS("Diagnostics/all_deltas.rds")
all_phis <- readRDS("Diagnostics/all_phis.rds")

min_delta <- min(all_deltas$value)
max_delta <- max(all_deltas$value)

min_phi <- min(all_phis$value)
max_phi <- max(all_phis$value)

beta_cred_int_list <- readRDS("Diagnostics/beta_cred_int.rds")
beta_cred_int <- do.call(rbind, beta_cred_int_list)

m_beta_cred_int_list <- readRDS("../../male/Death_PsychRate/Diagnostics/beta_cred_int.rds")
m_beta_cred_int <- do.call(rbind, m_beta_cred_int_list)

ymax <- max(max(beta_cred_int$Q97.5), max(m_beta_cred_int$Q97.5))
ymin <- min(min(beta_cred_int$Q2.5), min(m_beta_cred_int$Q2.5))

# Load the county data (for plotting maps)
us_counties_2012 <- counties(cb = TRUE, resolution = "20m", year = 2012)
us_counties_2013 <- counties(cb = TRUE, resolution = "20m", year = 2013)
us_counties_2014 <- counties(cb = TRUE, resolution = "20m", year = 2014)
us_counties_2015 <- counties(cb = TRUE, resolution = "20m", year = 2015)

us_counties_2016 <- counties(cb = TRUE, resolution = "20m", year = 2016)
us_counties_2017 <- counties(cb = TRUE, resolution = "20m", year = 2017)
us_counties_2018 <- counties(cb = TRUE, resolution = "20m", year = 2018)
us_counties_2019 <- counties(cb = TRUE, resolution = "20m", year = 2019)

us_counties_2020 <- counties(cb = TRUE, resolution = "20m", year = 2020)
us_counties_2021 <- counties(cb = TRUE, resolution = "20m", year = 2021)
us_counties_2022 <- counties(cb = TRUE, resolution = "20m", year = 2022)
us_counties_2023 <- counties(cb = TRUE, resolution = "20m", year = 2023)

# Add a year column
us_counties_2012$year <- 2012
us_counties_2013$year <- 2013
us_counties_2014$year <- 2014
us_counties_2015$year <- 2015

us_counties_2016$year <- 2016
us_counties_2017$year <- 2017
us_counties_2018$year <- 2018
us_counties_2019$year <- 2019

us_counties_2020$year <- 2020
us_counties_2021$year <- 2021
us_counties_2022$year <- 2022
us_counties_2023$year <- 2023

# Combine into one data set
us_counties_combined_su <- bind_rows(us_counties_2013, us_counties_2014, us_counties_2015,
                                     us_counties_2016, us_counties_2017, us_counties_2018, us_counties_2019,
                                     us_counties_2020, us_counties_2021, us_counties_2022, us_counties_2023)

for (i in 1:18) {
  MH_SF <- forMod_F %>%
    filter(bounded_age == i) %>%
    left_join(us_counties_combined_su, by = c("county" = "GEOID", 
                                          "year" = "year", 
                                          "state" = "STATEFP", 
                                          "state_abbr" = "STUSPS", 
                                          "county_name" = "NAMELSAD"))

    deltas.df <- all_deltas |> filter(ageGroup == i)
    phis.df <- all_phis |> filter(ageGroup == i)

  # Merge with spatial data
  deltas_map <- left_join(MH_SF, deltas.df, by = "fips")
  deltas_map <- st_as_sf(deltas_map)

  # Save map
  jpeg(paste("Diagnostics/delta_by_county_F", i, ".jpeg", sep = ""))
  print(
    ggplot(deltas_map) +
      geom_sf(aes(fill = value), color = NA) +
      scale_fill_viridis_c(option = "C", na.value = "grey90", limits=c(min_delta, max_delta)) +
      labs(title = "Mean of Delta by County", fill = "Delta") +
      theme_minimal()
  )
  dev.off()

  # Merge with spatial data
  phis_map <- left_join(MH_SF, phis.df, by = "fips")
  phis_map <- st_as_sf(phis_map)

  # Save map
  jpeg(paste("Diagnostics/phi_by_county_F", i, ".jpeg", sep = ""))
  print(
    ggplot(phis_map) +
      geom_sf(aes(fill = value), color = NA) +
      scale_fill_viridis_c(option = "D", na.value = "grey90", limits=c(min_phi, max_phi)) +
      labs(title = "Mean of Phi by County", fill = "Phi") +
      theme_minimal()
  )
  dev.off()
}

beta_summary_list <- readRDS("Diagnostics/beta_summary_list.rds")
beta_summary <- dplyr::bind_rows(beta_summary_list)

jpeg("Diagnostics/_summary_table.jpg", width = 1000, height = 600, quality = 100)
grid.table(beta_summary)
dev.off()

ggplot(beta_cred_int, aes(x = AgeGroup, y = Mean)) +
  geom_line(color = "darkred") +
  geom_point(color = "darkred") +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.3, color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Effect of PsychRate on total_deaths for females by Age Group (Posterior Mean ± 95% CI)",
    x = "Age Group",
    y = "Posterior Mean"
  ) +
  ylim(ymin, ymax) +
  theme_minimal()
ggsave("Diagnostics/_cred_int.png", bg = "white")
