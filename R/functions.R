cleanExposureData <- function() {
  load(here("data", "stroke_v1_3_exposures.RData"))
  pm25_long <- sto_uxue %>%
    select(id, deprivation, starts_with("pm25_300_")) %>%
    pivot_longer(
      cols = starts_with("pm25"),
      names_to = "year",
      names_pattern = "pm25_300_(.+)",
      values_to = "pm25"
    )
  no2_long <- sto_uxue %>%
    select(id, deprivation, starts_with("no2_300_")) %>%
    pivot_longer(
      cols = starts_with("no2"),
      names_to = "year",
      names_pattern = "no2_300_(.+)",
      values_to = "no2"
    )
  exposure_long <- pm25_long %>%
    left_join(no2_long, by = c("id", "deprivation", "year"))

  exposure_long
}

getAnnualVariance <- function(.data) {
  variance_annual <- .data %>%
    group_by(year) %>%
    summarize(var_pm25 = var(pm25)) %>%
    ungroup()

  mean(variance_annual$var_pm25)
}

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
    geom_contour_filled(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0001)) +
    scale_x_continuous(expand = expansion(mult = c(0.03, 0))) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0))) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 11)
    ) +
    labs(x = "HR", fill = "Power")
}
