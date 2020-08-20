## Exposures

##' Computes the average PM2.5 exposure for each id over the years available.
##'
##' Both APACHE and ELAPSE estimates are given.
##' @title Get average annual PM2.5 exposure.
##' @return Tibble with id, model PM2.5 annual average.
##' @author Sergio
readAnnualExposure <- function() {
  load(here("data", "stroke_v1_3_exposures.RData"))
  exposure_annual_avg <- sto_uxue %>%
    select(id, starts_with("pm25_300_"), starts_with("ap_pm25_")) %>%
    pivot_longer(
      cols = -id,
      names_to = c("model", "year"),
      names_pattern = "(.+_.+)_([0-9]+)",
      values_to = "pm25"
    ) %>%
    group_by(id, model) %>%
    summarize(pm25_annual_avg = mean(pm25)) %>%
    ungroup() %>%
    mutate(model = ifelse(model == "ap_pm25", "apache", "elapse"))

  exposure_annual_avg
}

##' @title Compute the variance for all individual average exposure.
##' @param data Data frame produced by \code{readAnnualExposure()}
##' @param model Character specifying which model estimates to use from data
##' @return Scalar
##' @author Sergio
getAnnualVariance <- function(data, model = c("apache", "elapse")) {
  model <- match.arg(model)
  var(data$pm25_annual_avg[data$model == model])
}

##' Daily PM2.5 measurements from Vall d'Heron statation for years 2008-2018.
##'
##' Observations for 2008 and 2012 are missing.
##' @title Read and clean daily PM2.5 exposure data.
##' @param data Data frame produced by \code{readAnnualExposure}
##' @param model Character specifying which model estimates to use from data
##' @return Tibble with date, year and pm25
##' @author Sergio
readDailyExposure <- function() {
  read_csv(here("data", "vall_hebron_pm25_2008_2018.csv")) %>%
    mutate(month = month(date)) %>%
    select(date, year, month, pm25)
}

##' Annual variance for any given year varies a lot, so the average of these annual variances is taken.
##'
##' @title Compute average variance in each month across years 2014-2018.
##' @param data Data frame as produced by \code{readDailyExposure}
##' @return Scalar
##' @author Sergio
getDailyVariance <- function(data) {
  var_by_year <- data %>%
    filter(year >= 2014) %>%
    group_by(year, month) %>%
    summarize(var_pm25 = var(pm25, na.rm = TRUE)) %>%
    ungroup()

  mean(var_by_year$var_pm25, na.rm = TRUE)
}

## Power

##' Calculate power
##'
##' Calculate power using powerEpiCountDefault()
##'
##' @title Calculate power for Cox PH model
##' @param n Sample size (scalar)
##' @param events Number of events (scalar)
##' @param sigma2 Main exposure variance (scalar)
##' @param hr.min Minimum hazard ratio (scalar)
##' @param hr.max Maximum hazard ratio (scalar)
##' @return Tibble with calculated power for every HR and rho^2 combination
##' @author Sergio
calcPowerDefault <- function(n, events, sigma2, hr.min, hr.max) {
  events_prop <- events / n
  hr_range <- seq(hr.min, hr.max, by = 0.001)
  rho2_range <- seq(0, 1, by = 0.001)
  par_grid <- expand.grid(hr = hr_range, rho2 = rho2_range)
  power <- map2_dbl(
    par_grid$hr, par_grid$rho2,
    function(hr, rho2) {
      powerEpiCont.default(
        n = n, theta = hr, sigma2 = sigma2,
        psi = events_prop, rho2, alpha = 0.05)
    }
  )

  bind_cols(par_grid, power = power) %>%
    as_tibble()
}

plotPowerDefault <- function(power.grid) {
  ggplot(power.grid, aes(hr, rho2, z = power)) +
    ## For some reason break at 1 shows empty area
    geom_contour_filled(breaks = c(0, 0.4, 0.6, 0.8, 0.9, 1.0001)) +
    scale_x_continuous(expand = expansion(mult = c(0.03, 0))) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0))) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 11)
    ) +
    labs(x = "HR", fill = "Power")
}

