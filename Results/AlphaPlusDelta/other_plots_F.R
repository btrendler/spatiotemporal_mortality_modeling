library(tidyverse)
library(sf)
library(tigris)

### New code ###

# Prep mapping data

load("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/f_data.Rdata")
setwd("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/models/6_5/female/Death_BRFFS")

# Load the county data (for plotting maps)
us_counties_2000 <- counties(cb = TRUE, resolution = "20m", year = 2000)

# currently the tigris package does not have 2001-2009 and 2011-2012 data
us_counties_2001 <- us_counties_2000

us_counties_2010 <- counties(cb = TRUE, resolution = "20m", year = 2010)

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

  # Make sure correct data type
us_counties_2000 <- st_as_sf(us_counties_2000)

us_counties_2010 <- st_as_sf(us_counties_2010)

# make sure that it has the right columns

us_counties_2000 <- us_counties_2000 |> mutate(county = paste0(STATE, COUNTY))

us_counties_2010 <- us_counties_2010 |> mutate(county = paste0(STATE, COUNTY))

# Add year column to each sf object
us_counties_2013 <- us_counties_2013 %>% st_as_sf() %>% mutate(year = 2013)
us_counties_2014 <- us_counties_2014 %>% st_as_sf() %>% mutate(year = 2014)
us_counties_2015 <- us_counties_2015 %>% st_as_sf() %>% mutate(year = 2015)
us_counties_2016 <- us_counties_2016 %>% st_as_sf() %>% mutate(year = 2016)
us_counties_2017 <- us_counties_2017 %>% st_as_sf() %>% mutate(year = 2017)
us_counties_2018 <- us_counties_2018 %>% st_as_sf() %>% mutate(year = 2018)
us_counties_2019 <- us_counties_2019 %>% st_as_sf() %>% mutate(year = 2019)

# Combine all into one sf object
counties1319 <- bind_rows(
  us_counties_2013,
  us_counties_2014,
  us_counties_2015,
  us_counties_2016,
  us_counties_2017,
  us_counties_2018,
  us_counties_2019)

# Add year column and convert to sf
us_counties_2020 <- us_counties_2020 %>% st_as_sf() %>% mutate(year = 2020)
us_counties_2021 <- us_counties_2021 %>% st_as_sf() %>% mutate(year = 2021)
us_counties_2022 <- us_counties_2022 %>% st_as_sf() %>% mutate(year = 2022)
us_counties_2023 <- us_counties_2023 %>% st_as_sf() %>% mutate(year = 2023)

# Combine all into one sf object
counties2023 <- bind_rows(
  us_counties_2020,
  us_counties_2021,
  us_counties_2022,
  us_counties_2023
)

age_labels <- c(
  "2" = "5–9", "3" = "10–14", "4" = "15–19", "5" = "20–24", "6" = "25–29",
  "7" = "30–34", "8" = "35–39", "9" = "40–44", "10" = "45–49",
  "11" = "50–54", "12" = "55–59", "13" = "60–64", "14" = "65–69",
  "15" = "70–74", "16" = "75–79", "17" = "80–84", "18" = "85+"
)

all_values <- c()

# for (i in 3:18) {
#   model_name <- paste0("modF", i, "lincovall.Rdata")
#   load(model_name)

#   alpha <- mean(modFLincov$samples$alpha)
#   rep_alpha <- rep(alpha, ncol(modFLincov$samples$delta))
#   time <- rep_alpha + base::apply(modFLincov$samples$delta, 2, mean)

#   all_values <- c(all_values, time)
# }

# global_min <- min(all_values, na.rm = TRUE)
# global_max <- max(all_values, na.rm = TRUE)


# Create plots of alpha + delta for each age group
### New version of the code ###

for(i in 3:18) {
  cat("Creating plot", i, "\n")

# Save each plot as a PNG file
  png_filename <- paste0("_Final_Plots/AlphaPlusDeltaF_", i, ".png")
  png(filename = png_filename, width = 2000, height = 1400, res = 300)

  # Select age group i
  forMod <- forMod_F |> 
    dplyr::filter(bounded_age == i) |>
    arrange(year, county)

  model_name <- paste0("modF", i, "lincovall.Rdata")
  load(paste0(model_name))

  # Create time-period specific datasets
  data0009 <- forMod |> filter(year < 2010) |> left_join(us_counties_2000, by = c("county" = "county", "state" = "STATEFP")) |> select(sex:ptsd_rate, NAME, COUNTYFP, geometry)
  data1012 <- forMod |> filter(year > 2009 & year < 2013) |> left_join(us_counties_2010, by = c("county" = "county", "state" = "STATEFP")) |> select(sex:ptsd_rate, NAME, COUNTYFP, geometry)
  data1319 <- forMod |> filter(year > 2012 & year < 2020) |> left_join(counties1319, by = c("county" = "GEOID", "state" = "STATEFP", "year" = "year")) |> select(sex:ptsd_rate, NAME, COUNTYFP, geometry)
  data2023 <- forMod |> filter(year > 2019) |> left_join(counties2023, by = c("county" = "GEOID", "state" = "STATEFP", "year" = "year")) |> select(sex:ptsd_rate, NAME, COUNTYFP, geometry)

  MH_SF <- rbind(data0009, data1012, data1319, data2023)
  MH_SF <- st_as_sf(MH_SF, sf_column_name = "geometry")

  alpha <- mean(modFLincov$samples$alpha)
  rep_alpha <- rep(alpha, ncol(modFLincov$samples$delta))
  time <- rep_alpha + base::apply(modFLincov$samples$delta, 2, mean)

  regions <- unique(forMod$county)
  deltas.df <- data.frame(county = regions, value = time)

  deltas_map <- left_join(MH_SF, deltas.df, by = "county")
  deltas_map <- sf::st_zm(deltas_map, drop = TRUE, what = "ZM")

  p <- ggplot(deltas_map) +
        geom_sf(aes(fill = value), color = NA) +
        scale_fill_viridis_c(
        option = "C",
        # limits = c(global_min, global_max),  # fixes color mapping
        na.value = "grey90"
        ) +
        labs(title = bquote(alpha~"+"~delta[k]~"for Females, Ages "~.(age_labels[as.character(i)])), fill = NULL) +
        theme_minimal()

  print(p)
  dev.off()  # Close JPEG device
}


dev.off()

for(i in 1:18) {

    model_name <- paste0("modF", i, "lincovall.Rdata")
  load(paste0(model_name))

    alpha <- mean(modFLincov$samples$alpha)
rep_alpha <- rep(alpha, ncol(modFLincov$samples$delta))
time <- rep_alpha + base::apply(modFLincov$samples$delta, 2, mean)

print(paste("For age group", i, ":"))
print(max(time))
print(min(time))
}


# fitted vs actual values
# make sure to run age_labels first

setwd("/yunity/weaverb1/spatmort/brianne_weaver/Mental_Health/Models/sui_all_cov_increased_sample/Female")

for (i in 2:18) {
  model_file <- paste0("modF", i, "lincovall.Rdata")
  if (!file.exists(model_file)) next

  load(model_file)
  model <- get("modFLincov")

  df_i <- forMod_F %>% filter(bounded_age == i)
  y_actual <- df_i$Y_suicide
  y_fitted <- model$fitted.values  # * df_i$pop

  age_label <- age_labels[i-1]

  # Fitted vs Actual plot
  p1 <- ggplot(data.frame(y_actual, y_fitted), aes(y = y_actual, x = y_fitted)) +
    geom_point(color = "steelblue", alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = paste("Fitted vs. Actual (Age", age_label, ")"),
      y = "Actual Values",
      x = "Fitted Values"
    ) +
    theme_minimal(base_size = 14)

  ggsave(paste0("fittedvactual_", i, ".jpg"), plot = p1, width = 6, height = 6, dpi = 300)

  # Residuals
  residuals <- y_actual - y_fitted
  p2 <- ggplot(data.frame(y_fitted, residuals), aes(x = y_fitted, y = residuals)) +
    geom_point(color = "darkorange", alpha = 0.6) +
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
    labs(
      title = paste("Residuals vs. Fitted (Age", age_label, ")"),
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal(base_size = 14)

  ggsave(paste0("fittedvresidual_", i, ".jpg"), plot = p2, width = 6, height = 6, dpi = 300)

  rm(modFLincov)
}
