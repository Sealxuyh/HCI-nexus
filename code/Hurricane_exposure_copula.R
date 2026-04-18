# install.packages("hurricaneexposure")
# install required data package
install.packages("drat")
library(drat)
addRepo("geanders")
install.packages("hurricaneexposuredata")

setwd("/Users/yuhan/Library/CloudStorage/OneDrive-GeorgiaInstituteofTechnology/06_Research_PRJ_2025_HCI/HCI-nexus/code")
# load packages
library(hurricaneexposure)
library(hurricaneexposuredata)
library(dplyr)
library(readr)
library(sf)
library(tigris)
library(tidyr)
library(stringr)
library(ggplot2)

# =============================================================================
# Data Preparation
# =============================================================================
# check that the functions are available
exists("map_counties")
exists("county_rain")

# example
hurricaneexposure::map_counties(storm = "Sandy-2012", metric = "wind")

options(tigris_use_cache = TRUE)

# 1. Get all Florida counties and their 5-digit FIPS codes
fl_counties <- counties(state = "FL", cb = TRUE, year = 2020) |>
  st_drop_geometry() |>
  transmute(
    fips = GEOID,
    county = NAME
  ) |>
  arrange(fips)

fl_fips <- fl_counties$fips

# check
print(fl_counties)
length(fl_fips)

# 2. Load package datasets
data("closest_dist", package = "hurricaneexposuredata")
data("storm_winds", package = "hurricaneexposuredata")
data("rain", package = "hurricaneexposuredata")
data("storm_events", package = "hurricaneexposuredata")

# 3. Subset all Florida records
fl_dist <- closest_dist |>
  dplyr::filter(fips %in% fl_fips) |>
  dplyr::left_join(fl_counties, by = "fips")

fl_wind <- storm_winds |>
  dplyr::filter(fips %in% fl_fips) |>
  dplyr::left_join(fl_counties, by = "fips")

fl_rain <- rain |>
  dplyr::filter(fips %in% fl_fips) |>
  dplyr::left_join(fl_counties, by = "fips")

fl_flood <- hurricaneexposure::county_events(
  counties = fl_fips,
  start_year = 1996,
  end_year = 2022,
  event_type = "flood"
) |>
  dplyr::left_join(fl_counties, by = "fips")

# 5. Save raw Florida extracts
readr::write_csv(fl_dist, r"(../data/hurricaneexposuredata/florida_distance_raw.csv)")
readr::write_csv(fl_wind, r"(../data/hurricaneexposuredata/florida_wind_raw.csv)")
readr::write_csv(fl_rain, r"(../data/hurricaneexposuredata/florida_rain_raw.csv)")
readr::write_csv(fl_flood, r"(../data/hurricaneexposuredata/florida_flood_raw.csv)")

# Rain exposure by county-storm
fl_rain_evt <- hurricaneexposure::county_rain(
  counties = fl_fips,
  start_year = 1988,
  end_year = 2022,
  rain_limit = 0,
  dist_limit = 1000,
  days_included = c(-1, 0, 1)
) |>
  dplyr::left_join(fl_counties, by = "fips") |>
  dplyr::arrange(storm_id, fips)

# Wind exposure by county-storm
fl_wind_evt <- hurricaneexposure::county_wind(
  counties = fl_fips,
  start_year = 1988,
  end_year = 2022,
  wind_limit = 0,
  wind_var = "vmax_sust",
  wind_source = "modeled"
) |>
  dplyr::left_join(fl_counties, by = "fips") |>
  dplyr::arrange(storm_id, fips)

# Merge
fl_evt <- fl_rain_evt |>
  dplyr::full_join(
    fl_wind_evt,
    by = c("fips", "county", "storm_id", "closest_date", "local_time", "closest_time_utc")
  ) |>
  dplyr::arrange(storm_id, fips)

readr::write_csv(fl_evt, "../data/hurricaneexposuredata/florida_rain_wind_eventlevel.csv")



# =============================================================================
# Data cleaning and EDA
# =============================================================================
# read compiled dataset
hazard_df <- readr::read_csv(
  "../data/hurricaneexposuredata/florida_rain_wind_eventlevel.csv",
  show_col_types = FALSE
)

# clean
hazard_df <- hazard_df %>%
  mutate(
    year = as.integer(sub(".*-", "", storm_id)),
    storm_dist = coalesce(storm_dist.x, storm_dist.y)
  ) %>%
  select(
    storm_id, year, fips, county,
    tot_precip, vmax_sust, vmax_gust,
    storm_dist, everything(),
    -storm_dist.x, -storm_dist.y
  )

glimpse(hazard_df)

# check the structure
hazard_df %>%
  summarise(
    n = n(),
    n_years = n_distinct(year),
    n_storms = n_distinct(storm_id),
    n_counties = n_distinct(fips),
    missing_rain = sum(is.na(tot_precip)),
    missing_wind = sum(is.na(vmax_sust))
  )

# scatterplot of rain versus wind
ggplot(
  hazard_df %>% filter(!is.na(tot_precip), !is.na(vmax_sust)),
  aes(x = vmax_sust, y = tot_precip)
) +
  geom_point(alpha = 0.35) +
  labs(
    x = "Maximum sustained wind",
    y = "Total precipitation",
    title = "County-storm relationship between wind and rain in Florida"
  ) +
  theme_minimal()

# annual dependence metrics
yearly_corr <- hazard_df %>%
  filter(!is.na(tot_precip), !is.na(vmax_sust)) %>%
  group_by(year) %>%
  summarise(
    n_obs = n(),
    pearson_cor = cor(tot_precip, vmax_sust, method = "pearson"),
    spearman_cor = cor(tot_precip, vmax_sust, method = "spearman"),
    mean_rain = mean(tot_precip, na.rm = TRUE),
    mean_wind = mean(vmax_sust, na.rm = TRUE),
    max_rain = max(tot_precip, na.rm = TRUE),
    max_wind = max(vmax_sust, na.rm = TRUE),
    .groups = "drop"
  )

print(yearly_corr)
ggplot(yearly_corr, aes(x = year, y = spearman_cor)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Spearman correlation",
    title = "Annual rain-wind dependence across county-storm observations"
  ) +
  theme_minimal()

yearly_summary <- hazard_df %>%
  filter(!is.na(tot_precip), !is.na(vmax_sust)) %>%
  group_by(year) %>%
  summarise(
    n_obs = n(),
    avg_rain = mean(tot_precip, na.rm = TRUE),
    median_rain = median(tot_precip, na.rm = TRUE),
    max_rain = max(tot_precip, na.rm = TRUE),
    avg_wind = mean(vmax_sust, na.rm = TRUE),
    median_wind = median(vmax_sust, na.rm = TRUE),
    max_wind = max(vmax_sust, na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# Lee County
# =============================================================================
lee_df <- hazard_df %>%
  filter(county == "Lee") %>%
  mutate(
    year = as.integer(sub(".*-", "", storm_id))
  ) %>%
  select(
    storm_id, year, fips, county,
    closest_date, local_time, closest_time_utc,
    tot_precip, vmax_sust, vmax_gust,
    sust_dur, gust_dur,
    storm_dist, usa_atcf_id
  ) %>%
  filter(
    !is.na(tot_precip),
    !is.na(vmax_sust)
  ) %>%
  arrange(year, storm_id)

lee_df %>%
  summarise(
    n_rows = n(),
    n_years = n_distinct(year),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE)
  )

# annual maximum rain
lee_amax_rain <- lee_df %>%
  group_by(year) %>%
  slice_max(order_by = tot_precip, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(year)

lee_copula_rainmax <- lee_amax_rain %>%
  transmute(
    year,
    storm_id,
    rain = tot_precip,
    wind = vmax_sust,
    distance_km = storm_dist
  )

# annual maximum wind
lee_amax_wind <- lee_df %>%
  group_by(year) %>%
  slice_max(order_by = vmax_sust, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(year)

lee_copula_windmax <- lee_amax_wind %>%
  transmute(
    year,
    storm_id,
    rain = tot_precip,
    wind = vmax_sust,
    distance_km = storm_dist
  )

lee_compare <- lee_copula_rainmax %>%
  rename(
    storm_id_rainmax = storm_id,
    rain_rainmax = rain,
    wind_at_rainmax = wind,
    dist_rainmax = distance_km
  ) %>%
  left_join(
    lee_copula_windmax %>%
      rename(
        storm_id_windmax = storm_id,
        rain_at_windmax = rain,
        wind_windmax = wind,
        dist_windmax = distance_km
      ),
    by = "year"
  )

print(lee_compare, n = Inf)

# use the concomitant wind of the annual rain maximum
lee_rainmax <- hazard_df %>%
  filter(county == "Lee") %>%
  mutate(
    year = as.integer(sub(".*-", "", storm_id))
  ) %>%
  filter(
    !is.na(tot_precip),
    !is.na(vmax_sust)
  ) %>%
  group_by(year) %>%
  slice_max(order_by = tot_precip, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(year)

lee_rainmax <- lee_rainmax %>%
  transmute(
    year,
    storm_id,
    fips,
    county,
    closest_date,
    rain = tot_precip,
    wind = vmax_sust,
    wind_gust = vmax_gust,
    distance_km = storm_dist
  )

print(lee_rainmax, n = Inf)

lee_rainmax %>%
  summarise(
    n_years = n(),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    min_rain = min(rain, na.rm = TRUE),
    max_rain = max(rain, na.rm = TRUE),
    min_wind = min(wind, na.rm = TRUE),
    max_wind = max(wind, na.rm = TRUE)
  )

readr::write_csv(
  lee_rainmax,
  "../data/hurricaneexposuredata/lee_annual_rainmax_with_wind.csv"
)

ggplot(lee_rainmax, aes(x = wind, y = rain)) +
  geom_point() +
  geom_text(aes(label = year), vjust = -0.4, size = 3) +
  labs(
    x = "Wind speed during annual rain-maximum event (m/s)",
    y = "Maximum rainfall (mm)",
    title = "Hurricane-induced rain maxima with concurrent wind"
  ) +
  theme_minimal()

# compare maximum wind
lee_windmax <- hazard_df %>%
  filter(county == "Lee") %>%
  mutate(
    year = as.integer(sub(".*-", "", storm_id))
  ) %>%
  filter(
    !is.na(tot_precip),
    !is.na(vmax_sust)
  ) %>%
  group_by(year) %>%
  slice_max(order_by = vmax_sust, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    year,
    storm_id,
    fips,
    county,
    closest_date,
    rain = tot_precip,
    wind = vmax_sust,
    wind_gust = vmax_gust,
    distance_km = storm_dist
  )

lee_compare <- lee_rainmax %>%
  rename(
    storm_id_rainmax = storm_id,
    date_rainmax = closest_date,
    rain_rainmax = rain,
    wind_at_rainmax = wind,
    gust_at_rainmax = wind_gust,
    dist_rainmax = distance_km
  ) %>%
  left_join(
    lee_windmax %>%
      rename(
        storm_id_windmax = storm_id,
        date_windmax = closest_date,
        rain_at_windmax = rain,
        wind_windmax = wind,
        gust_windmax = wind_gust,
        dist_windmax = distance_km
      ),
    by = c("year", "fips", "county")
  ) %>%
  mutate(
    same_event = storm_id_rainmax == storm_id_windmax
  ) %>%
  arrange(year)

# In many years, the annual rainfall maximum and annual wind maximum in Lee County
# occurred in different storm events, indicating that univariate annual maxima 
# are often not concurrent and may be unsuitable for representing compound hurricane hazard.

lee_events <- hazard_df %>%
  filter(county == "Lee") %>%
  mutate(
    year = as.integer(sub(".*-", "", storm_id))
  ) %>%
  transmute(
    year,
    storm_id,
    fips,
    county,
    closest_date,
    rain = tot_precip,
    wind = vmax_sust,
    wind_gust = vmax_gust,
    distance_km = storm_dist
  ) %>%
  filter(
    !is.na(rain),
    !is.na(wind)
  ) %>%
  distinct() %>%
  arrange(year, closest_date, storm_id)

lee_events %>%
  summarise(
    n_events = n(),
    n_years = n_distinct(year),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    min_rain = min(rain, na.rm = TRUE),
    max_rain = max(rain, na.rm = TRUE),
    min_wind = min(wind, na.rm = TRUE),
    max_wind = max(wind, na.rm = TRUE)
  )

readr::write_csv(
  lee_events,
  "../data/hurricaneexposuredata/lee_county_all_events.csv"
)

# Load required libraries
library("copula")   # for copula modeling (dependence)
library("nsRFA")    # for extreme value analysis (MLE fitting)
library("lmomco")   # for Gumbel distribution functions
library("extRemes")


# ============================================================
# Joint extreme analysis for annual Lee County rain-wind data
# Variables:
#   rain      = annual maximum rainfall (or annual rain-max event rainfall)
#   wind_gust = wind gust recorded for the same annual event
# ============================================================
# ============================================================
# 0. Extract and clean data
# ============================================================
# ---- Read input file & assign data pairs ----
# Read CSV file containing flood data
dat <- read.csv(
  "../data/hurricaneexposuredata/lee_annual_rainmax_with_wind.csv"
)

rain <- dat$rain
wind <- dat$wind_gust

idx <- complete.cases(rain, wind)
rain <- rain[idx]
wind <- wind[idx]

n <- length(rain)

cat("Number of annual observations:", n, "\n")

# ============================================================
# 1. Fit GEV marginals by maximum likelihood
# ============================================================

# ---- Rain ----
gev_fit_rain <- fevd(rain, type = "GEV")

location_rain <- gev_fit_rain$results$par["location"]
scale_rain    <- gev_fit_rain$results$par["scale"]
shape_rain    <- gev_fit_rain$results$par["shape"]

# ---- Wind gust ----
gev_fit_wind <- fevd(wind, type = "GEV")

location_wind <- gev_fit_wind$results$par["location"]
scale_wind    <- gev_fit_wind$results$par["scale"]
shape_wind    <- gev_fit_wind$results$par["shape"]

# ---- Print parameter estimates ----
cat("\nGEV parameters for rain:\n")
cat("Location:", location_rain, "\n")
cat("Scale   :", scale_rain, "\n")
cat("Shape   :", shape_rain, "\n")

cat("\nGEV parameters for wind gust:\n")
cat("Location:", location_wind, "\n")
cat("Scale   :", scale_wind, "\n")
cat("Shape   :", shape_wind, "\n")

# ============================================================
# 2. Histogram + fitted GEV density
# ============================================================
par(mfrow = c(1, 2))

# ---- Rain ----
hist(rain,
     breaks = 12,
     freq = FALSE,
     main = "GEV fit to hurricane-induced maximum rainfall",
     xlab = "Rainfall (mm)",
     ylab = "Density",
     col = "lightblue",
     border = "black")

x_rain <- seq(min(rain) - 0.5, max(rain) + 0.5, length.out = 300)

dens_rain <- devd(x_rain,
                  loc = location_rain,
                  scale = scale_rain,
                  shape = shape_rain,
                  type = "GEV")

lines(x_rain, dens_rain, col = "red", lwd = 2.5)

# ---- Wind ----
hist(wind,
     breaks = 12,
     freq = FALSE,
     main = "GEV fit to hurricane-induced wind gust",
     xlab = "Wind gust (m/s)",
     ylab = "Density",
     col = "lightblue",
     border = "black")

x_wind <- seq(min(wind) - 0.5, max(wind) + 0.5, length.out = 300)

dens_wind <- devd(x_wind,
                  loc = location_wind,
                  scale = scale_wind,
                  shape = shape_wind,
                  type = "GEV")

lines(x_wind, dens_wind, col = "red", lwd = 2.5)

# Optional diagnostic plots
par(mfrow = c(2, 2))
plot(gev_fit_rain)
par(mfrow = c(2, 2))
plot(gev_fit_wind)

# ============================================================
# 3. Transform to probability space using fitted GEV CDF
# ============================================================
F_rain <- pevd(rain,
               loc = location_rain,
               scale = scale_rain,
               shape = shape_rain,
               type = "GEV")

F_wind <- pevd(wind,
               loc = location_wind,
               scale = scale_wind,
               shape = shape_wind,
               type = "GEV")

U_obs <- cbind(F_rain, F_wind)

# Marginal exceedance probabilities and return periods
P_rain <- 1 - F_rain
P_wind <- 1 - F_wind

T_rain <- 1 / P_rain
T_wind <- 1 / P_wind

# ============================================================
# 4. Fit a Gumbel copula
# ============================================================
tau_hat <- cor(rain, wind, method = "kendall", use = "complete.obs")

gumbel_base <- gumbelCopula(dim = 2)
fit_gumbel  <- fitCopula(gumbel_base, U_obs, method = "itau")

theta_hat <- as.numeric(coef(fit_gumbel))
gumbel_fit <- gumbelCopula(theta_hat, dim = 2)

cat("\nDependence summary:\n")
cat("Kendall's tau:", tau_hat, "\n")
cat("Gumbel theta :", theta_hat, "\n")

lambda_U <- 2 - 2^(1 / theta_hat)
cat("Upper tail dependence (lambda_U):", lambda_U, "\n")

# Scatter plot in copula space
par(mfrow = c(1, 1))
plot(F_rain, F_wind,
     xlab = "GEV CDF of rainfall",
     ylab = "GEV CDF of wind gust",
     main = "Transformed observations in copula space",
     pch = 19,
     col = "steelblue")

# ============================================================
# 5. Copula CDF and PDF visualization
# ============================================================
n_grid <- 60
u <- seq(0.001, 0.999, length.out = n_grid)
v <- seq(0.001, 0.999, length.out = n_grid)

grid_uv <- expand.grid(u, v)
U_grid  <- as.matrix(grid_uv)

# Copula CDF
C_vals <- pCopula(U_grid, gumbel_fit)
C_mat  <- matrix(C_vals, n_grid, n_grid)

# Copula density
c_vals <- dCopula(U_grid, gumbel_fit)
c_mat  <- matrix(c_vals, n_grid, n_grid)

# Cap extreme density spikes for plotting
c_mat_plot <- c_mat
cap_val <- quantile(c_mat, 0.98, na.rm = TRUE)
c_mat_plot[c_mat_plot > cap_val] <- cap_val

# Color palettes
plasma <- colorRampPalette(
  c("#0d0887", "#5c01a6", "#9c179e", "#cc4778", "#ed7953", "#fdb32f", "#f0f921")
)(100)

inferno <- colorRampPalette(
  c("#000004", "#420a68", "#932667", "#dd513a", "#fca50a", "#fcffa4")
)(100)

zcol <- function(mat, pal) {
  zlim <- range(mat, na.rm = TRUE)
  idx <- round((mat - zlim[1]) / diff(zlim) * (length(pal) - 1)) + 1
  idx <- pmax(1, pmin(length(pal), idx))
  matrix(pal[idx], nrow = nrow(mat))
}

# 3D surfaces
par(mfrow = c(1, 2), mar = c(3, 2, 3, 2), bg = "white")

persp(u, v, C_mat,
      col = zcol(C_mat, plasma)[-1, -1],
      theta = 35, phi = 25,
      expand = 0.6, shade = 0.3,
      ltheta = 120, border = NA,
      xlab = "u (rainfall probability)",
      ylab = "v (wind gust probability)",
      zlab = "C(u,v)",
      main = "Gumbel copula CDF",
      ticktype = "detailed", cex.axis = 0.7)

persp(u, v, c_mat_plot,
      col = zcol(c_mat_plot, inferno)[-1, -1],
      theta = 35, phi = 25,
      expand = 0.6, shade = 0.25,
      ltheta = 120, border = NA,
      xlab = "u (rainfall probability)",
      ylab = "v (wind gust probability)",
      zlab = "c(u,v)",
      main = "Gumbel copula density",
      ticktype = "detailed", cex.axis = 0.7)

# Filled contour plots
filled.contour(
  u, v, C_mat,
  nlevels = 20,
  color.palette = function(n) plasma,
  plot.title = title(
    main = "CDF contours",
    xlab = "u (rainfall probability)",
    ylab = "v (wind gust probability)"
  ),
  key.title = title(main = "C(u,v)"),
  plot.axes = {
    axis(1)
    axis(2)
    contour(u, v, C_mat,
            levels = seq(0.1, 0.9, by = 0.1),
            add = TRUE, col = "white",
            lwd = 0.8, labcex = 0.6, drawlabels = TRUE)
  }
)

filled.contour(
  u, v, c_mat_plot,
  nlevels = 20,
  color.palette = function(n) inferno,
  plot.title = title(
    main = "Density contours",
    xlab = "u (rainfall probability)",
    ylab = "v (wind gust probability)"
  ),
  key.title = title(main = "c(u,v)"),
  plot.axes = {
    axis(1)
    axis(2)
    contour(u, v, c_mat_plot,
            add = TRUE, col = "white",
            lwd = 0.8, drawlabels = FALSE, nlevels = 8)
  }
)

# Simulated samples in copula space
par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
sim_uv <- rCopula(2000, gumbel_fit)

plot(sim_uv[, 1], sim_uv[, 2],
     col = adjustcolor("steelblue", alpha.f = 0.35),
     pch = 16, cex = 0.6,
     xlim = c(0, 1), ylim = c(0, 1),
     xlab = "u (rainfall probability)",
     ylab = "v (wind gust probability)",
     main = "Simulated samples from fitted Gumbel copula")

abline(a = 0, b = 1, lty = 2, col = "grey55", lwd = 1)

contour(u, v, c_mat_plot,
        add = TRUE, col = "#e63946", lwd = 1.4,
        labcex = 0.6, drawlabels = TRUE, nlevels = 12)

legend("topleft",
       legend = c("Simulated", "Density contours", "1:1 line"),
       col = c(adjustcolor("steelblue", 0.7), "#e63946", "grey55"),
       pch = c(16, NA, NA),
       lty = c(NA, 1, 2),
       lwd = c(NA, 1.4, 1),
       pt.cex = 0.9, bty = "n", cex = 0.8)

# ============================================================
# 6. Joint return periods for observed data
# ============================================================
# For annual data:
#   OR exceedance probability  = 1 - C(u,v)
#   AND exceedance probability = 1 - u - v + C(u,v)

C_obs <- pCopula(U_obs, gumbel_fit)

P_or_obs  <- 1 - C_obs
T_or_obs  <- 1 / P_or_obs

P_and_obs <- 1 - F_rain - F_wind + C_obs
T_and_obs <- 1 / P_and_obs

joint_obs <- data.frame(
  rain = rain,
  wind = wind,
  F_rain = F_rain,
  F_wind = F_wind,
  C_uv = C_obs,
  P_or = P_or_obs,
  T_or = T_or_obs,
  P_and = P_and_obs,
  T_and = T_and_obs
)

print(head(joint_obs))

# ============================================================
# 7. Simulate from fitted model and draw return-period contours
#    revised plotting range
# ============================================================

# Simulate dependent samples in copula space
sim_uv_phys <- rCopula(5000, gumbel_fit)

sim_rain <- qevd(sim_uv_phys[, 1],
                 loc = location_rain,
                 scale = scale_rain,
                 shape = shape_rain,
                 type = "GEV")

sim_wind <- qevd(sim_uv_phys[, 2],
                 loc = location_wind,
                 scale = scale_wind,
                 shape = shape_wind,
                 type = "GEV")

# Probability grid
n_sim <- 500
f_rain <- seq(0.05, 0.999, length.out = n_sim)
f_wind <- seq(0.05, 0.999, length.out = n_sim)

rain_grid <- qevd(f_rain,
                  loc = location_rain,
                  scale = scale_rain,
                  shape = shape_rain,
                  type = "GEV")

wind_grid <- qevd(f_wind,
                  loc = location_wind,
                  scale = scale_wind,
                  shape = shape_wind,
                  type = "GEV")

pair_grid <- cbind(
  expand.grid(f_rain, f_wind)$Var1,
  expand.grid(f_rain, f_wind)$Var2
)

C_grid <- pCopula(pair_grid, gumbel_fit)

or_prob_mat  <- matrix(1 - C_grid, n_sim, n_sim)
and_prob_mat <- matrix(1 - pair_grid[, 1] - pair_grid[, 2] + C_grid, n_sim, n_sim)

return_levels <- c(2, 5, 10, 25, 50, 100)
prob_levels <- 1 / return_levels

# ------------------------------------------------------------
# Use trimmed plotting limits
# based on observed data
# ------------------------------------------------------------
x_lim <- c(min(rain, na.rm = TRUE), quantile(rain, 0.995, na.rm = TRUE))
y_lim <- c(min(wind, na.rm = TRUE), quantile(wind, 0.995, na.rm = TRUE))

x_lim[2] <- x_lim[2] * 2
y_lim[2] <- y_lim[2] * 3

# Keep only the part of the grid inside plotting limits
rain_keep <- rain_grid <= x_lim[2]
wind_keep <- wind_grid <= y_lim[2]

rain_grid_plot <- rain_grid[rain_keep]
wind_grid_plot <- wind_grid[wind_keep]

or_prob_mat_plot  <- or_prob_mat[rain_keep, wind_keep, drop = FALSE]
and_prob_mat_plot <- and_prob_mat[rain_keep, wind_keep, drop = FALSE]

# Also trim simulated points for display only
sim_keep <- sim_rain <= x_lim[2] & sim_wind <= y_lim[2]

par(mfrow = c(1, 2))

# OR return period contour
plot(sim_rain[sim_keep], sim_wind[sim_keep],
     col = "lightgrey", cex = 0.5,
     xlab = "Hurricane-induced maximum rainfall (mm)",
     ylab = "Maximum wind gust (m/s)",
     xlim = x_lim,
     ylim = y_lim,
     main = "OR return period contour")

points(rain, wind, col = "red", pch = 16, cex = 0.7)

contour(rain_grid_plot, wind_grid_plot, or_prob_mat_plot,
        levels = prob_levels,
        labels = paste(return_levels, "yr"),
        drawlabels = TRUE,
        labcex = 0.8,
        col = "black",
        lwd = 1.5,
        add = TRUE,
        method = "flattest")

# AND return period contour
plot(sim_rain[sim_keep], sim_wind[sim_keep],
     col = "lightgrey", cex = 0.5,
     xlab = "Hurricane-induced maximum rainfall (mm)",
     ylab = "Maximum wind gust (m/s)",
     xlim = x_lim,
     ylim = y_lim,
     main = "AND return period contour")

points(rain, wind, col = "red", pch = 16, cex = 0.7)

contour(rain_grid_plot, wind_grid_plot, and_prob_mat_plot,
        levels = prob_levels,
        labels = paste(return_levels, "yr"),
        drawlabels = TRUE,
        labcex = 0.8,
        col = "black",
        lwd = 1.5,
        add = TRUE,
        method = "flattest")

# ============================================================
# 8. Table of joint return periods for selected marginal return levels
# ============================================================
return_levels <- c(10, 25, 50, 100)

# For annual maxima, marginal non-exceedance probability
q <- 1 - 1 / return_levels

uv_dep <- cbind(q, q)
C_dep  <- pCopula(uv_dep, gumbel_fit)

T_or_dep  <- 1 / (1 - C_dep)
T_and_dep <- 1 / (1 - 2 * q + C_dep)

# Independence benchmark
C_ind <- q * q
T_or_ind  <- 1 / (1 - C_ind)
T_and_ind <- 1 / (1 - 2 * q + C_ind)

joint_return_periods <- data.frame(
  Marginal_T = return_levels,
  q = q,
  C_dep = C_dep,
  T_OR_dep = T_or_dep,
  T_AND_dep = T_and_dep,
  C_ind = C_ind,
  T_OR_ind = T_or_ind,
  T_AND_ind = T_and_ind
)

print(joint_return_periods)

write.csv(joint_return_periods,
          "joint_return_periods.csv",
          row.names = FALSE)

write.csv(joint_obs,
          "joint_return_periods_observed.csv",
          row.names = FALSE)

